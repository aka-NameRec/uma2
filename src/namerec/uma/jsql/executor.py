"""JSQL executor - executes parsed JSQL queries."""

from typing import Any

import sqlparse
from sqlalchemy import text
from sqlalchemy.engine import Engine

from namerec.uma.core.context import UMAContext
from namerec.uma.jsql.cache import CachedQuery
from namerec.uma.jsql.cache import CacheBackend
from namerec.uma.jsql.cache import make_cache_key
from namerec.uma.jsql.exceptions import JSQLExecutionError
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.parser import JSQLParser
from namerec.uma.jsql.result import JSQLResultBuilder
from namerec.uma.jsql.types import JSQLQuery
from namerec.uma.jsql.types import QueryResult


class JSQLExecutor:
    """
    Executor for JSQL queries.
    Orchestrates parsing and execution of JSQL queries with caching.
    """

    def __init__(
        self,
        context: UMAContext,
        cache_backend: CacheBackend | None = None,
        cache_enabled: bool = True,
    ) -> None:
        """
        Initialize JSQL executor.

        Args:
            context: UMA execution context
            cache_backend: Optional cache backend (query caching)
            cache_enabled: Enable/disable caching (default: True)
        """
        self.context = context
        self.parser = JSQLParser(context)
        self.cache_backend = cache_backend
        self.cache_enabled = cache_enabled and cache_backend is not None

    async def execute(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> QueryResult:
        """
        Execute JSQL query with caching.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            QueryResult with metadata and data

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
            JSQLExecutionError: If query execution fails
        """
        try:
            # Try cache first (if enabled)
            cached_sql = None
            if self.cache_enabled and self.cache_backend:
                cache_key = make_cache_key(
                    jsql,
                    user_context=self.context.user_context,
                    namespace=self.context.namespace,
                )
                cached_query = self.cache_backend.get(cache_key)
                if cached_query:
                    # Use cached SQL
                    cached_sql = cached_query.sql
                    # Map JSQL params to SQL params
                    sql_params = self._map_params(params or {}, cached_query.params_mapping)
                    query = text(cached_sql)
                else:
                    # Parse and cache
                    query = await self.parser.parse(jsql, params)
                    compiled_sql = self._compile_query_to_cacheable_sql(query)
                    cached_query = CachedQuery(
                        sql=compiled_sql,
                        params_mapping=self._extract_params_mapping(query),
                        dialect=self._get_dialect_name(self.context.engine),
                    )
                    self.cache_backend.set(cache_key, cached_query)
                    sql_params = params or {}
            else:
                # No cache - parse normally
                query = await self.parser.parse(jsql, params)
                sql_params = params or {}

            # Generate debug SQL if requested
            debug_sql: str | None = None
            if jsql.get('debug', False):
                if cached_sql:
                    debug_sql = self._format_sql(cached_sql)
                else:
                    debug_sql = self._compile_query_to_sql(query)

            # Execute query
            async with self.context.engine.connect() as conn:
                if cached_sql:
                    result = await conn.execute(text(cached_sql), sql_params)
                    # Build simplified result for cached queries
                    rows = [list(row) for row in result]
                    return QueryResult(
                        meta=[],  # Metadata not available from text() queries
                        data=rows,
                        debug=debug_sql,
                    )

                result = await conn.execute(query, sql_params)

                # Build result with metadata
                query_result = JSQLResultBuilder.build_result(result, query, debug_sql)

                return query_result

        except (JSQLExecutionError, JSQLSyntaxError):
            # Re-raise our own exceptions as-is
            raise
        except Exception as e:
            # Wrap other exceptions
            raise JSQLExecutionError(
                message=f'Failed to execute JSQL query: {e!s}',
                query=jsql,
                original_error=e,
            ) from e

    def _compile_query_to_sql(self, query: Any) -> str:
        """
        Compile SQLAlchemy query to SQL string for debug output.

        Args:
            query: SQLAlchemy Select statement

        Returns:
            Formatted SQL string, or error message if compilation fails
        """
        try:
            # Try to compile with literal binds first (substitute parameters)
            try:
                compiled = query.compile(
                    compile_kwargs={'literal_binds': True},
                )
                sql_str = str(compiled)
            except Exception:
                # Fallback: compile without literal binds if it fails
                # (e.g., for complex types like lists in IN operator)
                compiled = query.compile()
                sql_str = str(compiled)

            # Format SQL for readability
            formatted_sql = sqlparse.format(
                sql_str,
                reindent=True,
                keyword_case='upper',
            )

            return formatted_sql

        except Exception as e:
            # Debug mode should never break query execution
            # Return error message instead
            return f'-- Failed to generate debug SQL: {e!s}'

    def _compile_query_to_cacheable_sql(self, query: Any) -> str:
        """
        Compile SQLAlchemy query to cacheable SQL (with parameter placeholders).

        Args:
            query: SQLAlchemy Select statement

        Returns:
            SQL string with parameter placeholders
        """
        compiled = query.compile()
        return str(compiled)

    def _extract_params_mapping(self, query: Any) -> dict[str, str]:
        """
        Extract parameter mapping from compiled query.

        Args:
            query: SQLAlchemy Select statement

        Returns:
            Dictionary mapping JSQL param names to SQL param names
        """
        compiled = query.compile()
        # SQLAlchemy params are already named correctly
        # Just return identity mapping for all params
        return {name: name for name in compiled.params.keys()}

    def _get_dialect_name(self, engine: Engine) -> str:
        """
        Get SQL dialect name from engine.

        Args:
            engine: SQLAlchemy engine

        Returns:
            Dialect name (e.g., 'postgresql', 'mysql')
        """
        return engine.dialect.name

    def _map_params(self, jsql_params: dict[str, Any], mapping: dict[str, str]) -> dict[str, Any]:
        """
        Map JSQL parameter names to SQL parameter names.

        Args:
            jsql_params: Parameters from JSQL query
            mapping: Name mapping (JSQL -> SQL)

        Returns:
            Parameters with SQL names
        """
        return {mapping.get(k, k): v for k, v in jsql_params.items()}

    def _format_sql(self, sql: str) -> str:
        """
        Format SQL for debug output.

        Args:
            sql: SQL string

        Returns:
            Formatted SQL
        """
        try:
            return sqlparse.format(
                sql,
                reindent=True,
                keyword_case='upper',
            )
        except Exception:
            return sql
