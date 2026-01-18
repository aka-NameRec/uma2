"""JSQL to SQL conversion."""

import logging
from typing import Any

import sqlparse

from namerec.uma.jsql.converter.jsql_to_sqlglot import jsql_from_to_sqlglot as _jsql_from_to_sqlglot
from namerec.uma.jsql.converter.jsql_to_sqlglot import jsql_query_to_sqlglot as _jsql_query_to_sqlglot
from namerec.uma.jsql.converter.jsql_to_sqlglot import jsql_select_to_sqlglot as _jsql_select_to_sqlglot
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLQuery

logger = logging.getLogger(__name__)


def jsql_to_sql(jsql: JSQLQuery, dialect: str = 'generic') -> str:
    """
    Convert JSQL query to SQL string.

    This function directly converts JSQL to SQL using sqlglot expressions,
    without requiring database metadata or connection.
    """
    logger.info(f'Converting JSQL to SQL, dialect={dialect}')
    logger.debug(f'Input JSQL: {jsql}')

    try:
        query = _jsql_query_to_sqlglot(jsql)

        sql_str = query.sql(dialect=dialect if dialect != 'generic' else None, pretty=True)

        formatted_sql = sqlparse.format(
            sql_str,
            reindent=True,
            keyword_case='upper',
        )

        logger.info(f'Successfully converted JSQL to SQL ({dialect})')
        logger.debug(f'Generated SQL:\n{formatted_sql}')
        return formatted_sql

    except JSQLSyntaxError as e:
        logger.error(f'Failed to convert JSQL: {e}', exc_info=True)
        raise
    except Exception as e:
        logger.error(f'Unexpected error during JSQL conversion: {e}', exc_info=True)
        raise JSQLSyntaxError(
            message=f'Failed to convert JSQL to SQL: {e!s}',
            path='',
        ) from e


def jsql_query_to_sqlglot(jsql: dict[str, Any]):
    """Compatibility wrapper for direct SQLGlot conversion."""
    return _jsql_query_to_sqlglot(jsql)


def jsql_select_to_sqlglot(select_spec: list[dict[str, Any]] | None):
    """Compatibility wrapper for SELECT conversion."""
    return _jsql_select_to_sqlglot(select_spec)


def jsql_from_to_sqlglot(from_spec: str | dict[str, Any] | None):
    """Compatibility wrapper for FROM conversion."""
    return _jsql_from_to_sqlglot(from_spec)
