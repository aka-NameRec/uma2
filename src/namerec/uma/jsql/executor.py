"""JSQL executor - executes parsed JSQL queries."""

from collections.abc import Mapping
from typing import Any

try:
    import sqlparse
except ImportError:
    sqlparse = None  # type: ignore[assignment]

try:
    import structlog
except ImportError:
    structlog = None  # type: ignore[assignment]

from sqlalchemy import Engine
from sqlalchemy import text
from sqlalchemy.engine import Compiled
from sqlalchemy.sql.expression import Select

from namerec.uma.core.access import check_access
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.types import Operation
from namerec.uma.jsql.cache.keys import CachedQuery
from namerec.uma.jsql.cache.keys import make_cache_key
from namerec.uma.jsql.cache.protocol import CacheBackend
from namerec.uma.jsql.entity_extractor import extract_select_entities_from_ast
from namerec.uma.jsql.exceptions import JSQLExecutionError
from namerec.uma.jsql.parser import JSQLParser
from namerec.uma.jsql.result import JSQLResultBuilder
from namerec.uma.jsql.types import QueryResult


class JSQLExecutor:
    """JSQL query executor - stateless namespace holder."""

    @classmethod
    async def execute(
        cls,
        jsql: dict,
        namespace_configs: Mapping[str, NamespaceConfig],
        user_context: Any = None,
        cache_backend: CacheBackend | None = None,
        params: dict | None = None,
    ) -> QueryResult:
        """
        Execute JSQL with full access control.

        Args:
            jsql: JSQL query dictionary
            namespace_configs: Mapping of namespace names to configurations
            user_context: User context for access control
            cache_backend: Optional cache backend (None = no caching)
            params: Query parameters

        Returns:
            QueryResult with metadata and data

        Raises:
            UMAAccessDeniedError: If access denied
            ValueError: If JSQL invalid or namespace not found
            JSQLSyntaxError: If JSQL syntax is invalid
            JSQLExecutionError: If query execution fails
        """
        # Validate FROM field early (before parsing)
        if 'from' not in jsql:
            raise ValueError("JSQL must contain 'from' field")

        # Parse query - returns AST, config, and parameter mapping (namespace already validated)
        parser = JSQLParser(namespace_configs)
        sql_query, config, param_mapping = await parser.parse(jsql, params)

        # Try cache (namespace already in jsql, so cache key is unique)
        # Cache key depends on JSQL structure and user context, NOT on parameter values
        # This allows caching SQL with placeholders and reusing it with different parameter values
        cache_key = None
        if cache_backend:
            # Don't include params in cache key - same JSQL structure should use same cached SQL
            # Parameter values are passed at execution time
            cache_key = make_cache_key(jsql, user_context, params=None)
            cached = cache_backend.get(cache_key)

            if cached:
                # Check access to SELECT entities (even for cached queries!)
                cls._check_select_access(
                    entities=cached.entities,
                    metadata_provider=config.metadata_provider,
                    user_context=user_context,
                )
                # Execute cached SQL
                # Use cached debug_sql if available, otherwise format on demand
                debug_sql = cached.debug_sql
                if jsql.get('debug', False) and not debug_sql and sqlparse:
                    # Format cached SQL for debug if not already cached
                    debug_sql = sqlparse.format(cached.sql, reindent=True, keyword_case='upper')
                # Cached SQL may have parameter placeholders (for queries with params)
                # or embedded literal values (for queries without params)
                # Map JSQL parameter names to SQL parameter names and execute
                # params_mapping maps JSQL param names to SQL param names (bindparam keys)
                if cached.params_mapping and params:
                    # Build SQL parameters dict using cached mapping
                    # Since we use key=param_name in bindparam(), SQL param name matches JSQL param name
                    sql_params = {}
                    for jsql_param_name, sql_param_name in cached.params_mapping.items():
                        if jsql_param_name in params:
                            sql_params[sql_param_name] = params[jsql_param_name]
                    result = await cls._execute_cached_query(cached.sql, sql_params, config.engine, debug_sql)
                else:
                    # No parameters - execute with empty params
                    result = await cls._execute_cached_query(cached.sql, {}, config.engine, debug_sql)
                return result

        # Cache miss - extract entities from AST
        from_entity = jsql.get('from', '')
        select_entities = extract_select_entities_from_ast(sql_query, from_entity)

        # Check access to all SELECT entities
        cls._check_select_access(
            entities=select_entities,
            metadata_provider=config.metadata_provider,
            user_context=user_context,
        )

        # Execute
        debug_sql = None
        if jsql.get('debug', False):
            debug_sql = cls._compile_query_to_sql(sql_query, config.engine)

        # Execute query with parameterized queries for security (handled in _execute_query)
        # Parameters use bindparam() and are passed separately, preventing SQL injection
        result = await cls._execute_query(sql_query, params, config.engine, debug_sql)

        # Cache result - compile query for caching
        # SQL with parameter placeholders can be cached and reused with different parameter values
        if cache_backend and cache_key:
            try:
                # Compile query WITHOUT literal_binds to get SQL with parameter placeholders
                # This allows caching queries with parameters - SQL is cached, values are passed at execution
                compiled = JSQLExecutor._compile_query(sql_query, config.engine, literal_binds=False)
                
                # Use parameter mapping from parser (JSQL param name -> SQL param name)
                # Since we use key=param_name in bindparam(), the mapping should preserve names
                # param_mapping is already in the format: {jsql_name: sql_name}
                
                # Cache SQL with parameter placeholders
                # If query has no parameters, SQL will have embedded literal values
                # If query has parameters, SQL will have placeholders (e.g., $1, $2 or :param_name)
                cached_query = CachedQuery(
                    sql=str(compiled),
                    params_mapping=param_mapping,  # Map JSQL param names to SQL param names (bindparam keys)
                    dialect=config.engine.dialect.name,
                    entities=select_entities,
                    debug_sql=debug_sql,  # Cache debug SQL to avoid recompilation
                )
                cache_backend.set(cache_key, cached_query)
            except Exception as e:
                # Log compilation errors but don't fail the query
                if structlog:
                    logger = structlog.get_logger()
                    logger.warning(
                        'Failed to cache query',
                        cache_key=cache_key,
                        error=str(e),
                        error_type=type(e).__name__,
                    )

        return result

    @staticmethod
    def _check_select_access(
        entities: list[str],
        metadata_provider: Any,
        user_context: Any,
    ) -> None:
        """
        Check access to SELECT entities.

        Args:
            entities: List of entity names to check
            metadata_provider: Metadata provider for access control
            user_context: User context for permission checks

        Raises:
            UMAAccessDeniedError: If access denied to any entity
        """
        for select_entity in entities:
            check_access(
                metadata_provider=metadata_provider,
                entity_name=select_entity,
                operation=Operation.SELECT,
                user_context=user_context,
            )

    @staticmethod
    async def _execute_query(sql_query: Select, params: dict | None, engine: Engine, debug_sql: str | None = None) -> QueryResult:
        """
        Execute SQL query and return result.
        
        Uses parameterized queries for security (SQL injection protection).
        Parameters from bindparam() are passed separately, not embedded in SQL.
        This prevents SQL injection attacks by ensuring user input is treated as data, not code.
        """
        conn = await engine.connect()
        try:
            # Execute query directly with parameter binding
            # This uses SQLAlchemy's parameterized queries for security
            # Parameters from bindparam() are passed separately, preventing SQL injection
            # SQLAlchemy automatically extracts bindparam() names and uses values from params dict
            # For async engines, use the connection's execute method directly
            # Pass params dict with parameter values (keys match bindparam() names)
            result = await conn.execute(sql_query, params or {})
            return JSQLResultBuilder.build_result(result, sql_query, debug_sql)
        finally:
            await conn.close()

    @staticmethod
    async def _execute_cached_query(sql: str, params: dict | None, engine: Engine, debug_sql: str | None = None) -> QueryResult:
        """
        Execute cached SQL query and return result.
        
        Args:
            sql: Cached SQL string (may contain parameter placeholders)
            params: Parameter values to bind (keys match SQL parameter names)
            engine: Database engine
            debug_sql: Optional debug SQL string
            
        Returns:
            QueryResult with data (metadata may be limited for cached queries)
        """
        conn = await engine.connect()
        try:
            # Execute cached SQL with parameter binding
            # If SQL has placeholders (e.g., $1, $2), params will be bound to them
            # If SQL has embedded literals (no placeholders), params will be empty
            result = await conn.execute(text(sql), params or {})
            return JSQLExecutor._format_cached_result(result, debug_sql)
        finally:
            await conn.close()

    @staticmethod
    def _format_cached_result(result: Any, debug_sql: str | None = None) -> QueryResult:
        """
        Format cached query result.
        
        Args:
            result: SQLAlchemy result object
            debug_sql: Optional debug SQL string
            
        Returns:
            QueryResult with data (metadata may be limited for cached queries)
        """
        # For cached queries, metadata is not available (we don't have the original query AST)
        data = [list(row) for row in result]
        return QueryResult(meta=[], data=data, debug=debug_sql)

    @staticmethod
    def _compile_query(
        sql_query: Select,
        engine: Engine,
        literal_binds: bool = False,
    ) -> Compiled:
        """
        Compile SQLAlchemy query to compiled SQL.

        Args:
            sql_query: SQLAlchemy Select statement
            engine: Database engine (async or sync)
            literal_binds: Whether to embed literal values in SQL (for debug output)

        Returns:
            Compiled SQL object
        """
        compile_kwargs = {'literal_binds': literal_binds} if literal_binds else {}
        
        if hasattr(engine, 'sync_engine'):
            # Async engine - use sync_engine for compilation
            return sql_query.compile(bind=engine.sync_engine, compile_kwargs=compile_kwargs)
        else:
            # Sync engine - use dialect
            return sql_query.compile(dialect=engine.dialect, compile_kwargs=compile_kwargs)

    @staticmethod
    def _compile_query_to_sql(query: Select, engine: Engine | None = None) -> str:
        """
        Compile SQLAlchemy query to SQL string for debug output.

        Args:
            query: SQLAlchemy Select statement
            engine: Optional engine for compilation context
        """
        try:
            # Try to compile with literal binds first (substitute parameters)
            try:
                if engine:
                    compiled = JSQLExecutor._compile_query(query, engine, literal_binds=True)
                else:
                    # Fallback: try to compile without engine context
                    compiled = query.compile(compile_kwargs={'literal_binds': True})
                sql_str = str(compiled)
            except (TypeError, ValueError, AttributeError):
                # Fallback: compile without literal binds if it fails
                # (some dialects or parameter types don't support literal_binds)
                if engine:
                    compiled = JSQLExecutor._compile_query(query, engine, literal_binds=False)
                else:
                    compiled = query.compile()
                sql_str = str(compiled)

            # Format SQL for readability
            if sqlparse:
                return sqlparse.format(sql_str, reindent=True, keyword_case='upper')
            return sql_str
        except (AttributeError, TypeError) as e:
            # Debug mode should never break query execution
            # Catch formatting errors (sqlparse import issues, formatting errors)
            return f'-- Failed to generate debug SQL: {e!s}'
