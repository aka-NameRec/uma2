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
        cache_key = None
        if cache_backend:
            cache_key = make_cache_key(jsql, user_context)
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
                result = await cls._execute_cached_query(cached.sql, params, config.engine, debug_sql)
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

        result = await cls._execute_query(sql_query, params, config.engine, debug_sql)

        # Cache result
        if cache_backend and cache_key:
            # Compile query to SQL string with parameter placeholders
            compiled = sql_query.compile()
            cached_query = CachedQuery(
                sql=str(compiled),
                params_mapping={name: name for name in compiled.params.keys()},
                dialect=config.engine.dialect.name,
                entities=select_entities,
            )
            cache_backend.set(cache_key, cached_query)

        return result

    @staticmethod
    async def _execute_query(sql_query: Select, params: dict | None, engine: Engine, debug_sql: str | None = None) -> QueryResult:
        """Execute SQL query and return result."""
        from namerec.uma.jsql.result import JSQLResultBuilder

        conn = await engine.connect()
        try:
            result = await conn.execute(sql_query, params or {})
            return JSQLResultBuilder.build_result(result, sql_query, debug_sql)
        finally:
            await conn.close()

    @staticmethod
    async def _execute_cached_query(sql: str, params: dict | None, engine: Engine, debug_sql: str | None = None) -> QueryResult:
        """Execute cached SQL query and return result."""
        from sqlalchemy import text

        conn = await engine.connect()
        try:
            result = await conn.execute(text(sql), params or {})
            # For cached queries, metadata is not available
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
            try:
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
