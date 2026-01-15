"""JSQL executor - executes parsed JSQL queries."""

from collections.abc import Mapping
from typing import Any

from sqlalchemy import Engine
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

        # Parse query - returns AST and config (namespace already validated)
        parser = JSQLParser(namespace_configs)
        sql_query, config = await parser.parse(jsql, params)

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
                for select_entity in cached.entities:
                    check_access(
                        metadata_provider=config.metadata_provider,
                        entity_name=select_entity,
                        operation=Operation.SELECT,
                        user_context=user_context,
                    )
                # Execute cached SQL
                debug_sql = None
                if jsql.get('debug', False):
                    # For cached queries, format the cached SQL
                    import sqlparse
                    debug_sql = sqlparse.format(cached.sql, reindent=True, keyword_case='upper')
                # Cached SQL may have parameter placeholders (for queries with params)
                # or embedded literal values (for queries without params)
                # Map JSQL parameter names to SQL parameter names and execute
                # If params_mapping is empty, query has no parameters (all literals)
                if cached.params_mapping and params:
                    # Map JSQL param names to SQL param names
                    # params_mapping maps JSQL param names to SQL param names (from bindparam)
                    # Since we use bindparam(param_name), SQL param name should match JSQL param name
                    sql_params = {}
                    for jsql_param_name, sql_param_name in cached.params_mapping.items():
                        if jsql_param_name in params:
                            # Use SQL param name (may differ from JSQL name if SQLAlchemy renamed it)
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
        for select_entity in select_entities:
            check_access(
                metadata_provider=config.metadata_provider,
                entity_name=select_entity,
                operation=Operation.SELECT,
                user_context=user_context,
            )

        # Execute
        debug_sql = None
        if jsql.get('debug', False):
            debug_sql = cls._compile_query_to_sql(sql_query)

        # Execute query with parameterized queries for security (handled in _execute_query)
        # Parameters use bindparam() and are passed separately, preventing SQL injection
        result = await cls._execute_query(sql_query, params, config.engine, debug_sql)

        # Cache result - compile query for caching
        # SQL with parameter placeholders can be cached and reused with different parameter values
        if cache_backend and cache_key:
            try:
                # Compile query WITHOUT literal_binds to get SQL with parameter placeholders
                # This allows caching queries with parameters - SQL is cached, values are passed at execution
                if hasattr(config.engine, 'sync_engine'):
                    compiled = sql_query.compile(bind=config.engine.sync_engine)
                else:
                    compiled = sql_query.compile(dialect=config.engine.dialect)
                
                # Extract parameter mapping: JSQL param names -> SQL param names
                # compiled.params contains SQLAlchemy-generated parameter names (e.g., 'param_1', 'param_2')
                # We need to map them back to JSQL parameter names
                params_mapping = {}
                if compiled.params:
                    # Extract bindparam names from query and map to SQL parameter names
                    # SQLAlchemy uses sequential names like 'param_1', 'param_2' for bindparam()
                    # We need to preserve the original JSQL parameter names
                    # For now, we'll use the compiled.params keys directly
                    # TODO: Improve mapping to preserve original JSQL param names
                    params_mapping = {name: name for name in compiled.params.keys()}
                
                # Cache SQL with parameter placeholders
                # If query has no parameters, SQL will have embedded literal values
                # If query has parameters, SQL will have placeholders (e.g., $1, $2 or :param_1)
                cached_query = CachedQuery(
                    sql=str(compiled),
                    params_mapping=params_mapping,  # Map JSQL param names to SQL param names
                    dialect=config.engine.dialect.name,
                    entities=select_entities,
                )
                cache_backend.set(cache_key, cached_query)
            except Exception:
                # If compilation fails, skip caching
                pass

        return result

    @staticmethod
    async def _execute_query(sql_query: Select, params: dict | None, engine: Engine, debug_sql: str | None = None) -> QueryResult:
        """
        Execute SQL query and return result.
        
        Uses parameterized queries for security (SQL injection protection).
        Parameters from bindparam() are passed separately, not embedded in SQL.
        This prevents SQL injection attacks by ensuring user input is treated as data, not code.
        """
        from namerec.uma.jsql.result import JSQLResultBuilder

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
        from sqlalchemy import text

        conn = await engine.connect()
        try:
            # Execute cached SQL with parameter binding
            # If SQL has placeholders (e.g., $1, $2), params will be bound to them
            # If SQL has embedded literals (no placeholders), params will be empty
            result = await conn.execute(text(sql), params or {})
            # For cached queries, metadata is not available (we don't have the original query AST)
            data = [list(row) for row in result]
            return QueryResult(meta=[], data=data, debug=debug_sql)
        finally:
            await conn.close()

    @staticmethod
    def _compile_query_to_sql(query: Select) -> str:
        """Compile SQLAlchemy query to SQL string for debug output."""
        import sqlparse

        try:
            # Try to compile with literal binds first (substitute parameters)
            # For async engines, use sync_engine if available
            try:
                # Try to get engine from query context if available
                # Otherwise use default compilation
                compiled = query.compile(compile_kwargs={'literal_binds': True})
                sql_str = str(compiled)
            except (TypeError, ValueError, AttributeError):
                # Fallback: compile without literal binds if it fails
                # (some dialects or parameter types don't support literal_binds)
                compiled = query.compile()
                sql_str = str(compiled)

            # Format SQL for readability
            return sqlparse.format(sql_str, reindent=True, keyword_case='upper')
        except (ImportError, AttributeError, TypeError) as e:
            # Debug mode should never break query execution
            # Catch formatting errors (sqlparse import issues, formatting errors)
            return f'-- Failed to generate debug SQL: {e!s}'
