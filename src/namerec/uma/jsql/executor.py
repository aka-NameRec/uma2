"""JSQL executor - executes parsed JSQL queries."""

from typing import Any

import sqlparse

from namerec.uma.core.context import UMAContext
from namerec.uma.jsql.exceptions import JSQLExecutionError
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.parser import JSQLParser
from namerec.uma.jsql.result import JSQLResultBuilder
from namerec.uma.jsql.types import JSQLQuery
from namerec.uma.jsql.types import QueryResult


class JSQLExecutor:
    """
    Executor for JSQL queries.
    Orchestrates parsing and execution of JSQL queries.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL executor.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.parser = JSQLParser(context)

    async def execute(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> QueryResult:
        """
        Execute JSQL query.

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
            # Parse JSQL to SQLAlchemy query
            query = await self.parser.parse(jsql, params)

            # Generate debug SQL if requested
            debug_sql: str | None = None
            if jsql.get('debug', False):
                debug_sql = self._compile_query_to_sql(query)

            # Execute query
            async with self.context.engine.connect() as conn:
                result = await conn.execute(query)

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

    def _compile_query_to_sql(self, query: Any) -> str:  # noqa: ANN401
        """
        Compile SQLAlchemy query to SQL string.

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
