"""SQL to JSQL conversion."""

import logging
from typing import Any

import sqlglot
import sqlglot.expressions as exp

from namerec.uma.jsql.converter.sqlglot_to_jsql import convert_order_to_jsql as _convert_order_to_jsql
from namerec.uma.jsql.converter.sqlglot_to_jsql import convert_sqlglot_select_to_jsql as _convert_sqlglot_select_to_jsql
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLQuery

logger = logging.getLogger(__name__)


def sql_to_jsql(sql: str, dialect: str = 'generic') -> JSQLQuery:
    """
    Convert SQL query string to JSQL query dictionary.

    Args:
        sql: SQL query string
        dialect: SQL dialect (default: 'generic')

    Returns:
        JSQL query dictionary

    Raises:
        JSQLSyntaxError: If SQL parsing fails or conversion fails

    Example:
        >>> jsql = sql_to_jsql("SELECT id, name FROM users WHERE active = TRUE")
        >>> # Returns: {
        ... #     "from": "users",
        ... #     "select": [{"field": "id"}, {"field": "name"}],
        ... #     "where": {"op": "=", "left": {"field": "active"}, "right": {"value": True}}
        ... # }
    """
    logger.info(f'Converting SQL to JSQL, dialect={dialect}')
    logger.debug(f'Input SQL:\n{sql}')

    try:
        # Parse SQL using sqlglot
        parsed = sqlglot.parse_one(sql, read=dialect if dialect != 'generic' else None)
        if not isinstance(parsed, exp.Select):
            raise JSQLSyntaxError(
                message=f'Only SELECT queries are supported, got {type(parsed).__name__}',
                path='',
            )

        # Convert to JSQL
        jsql = _convert_sqlglot_select_to_jsql(parsed)

        logger.info('Successfully converted SQL to JSQL')
        logger.debug(f'Generated JSQL: {jsql}')
        return jsql

    except sqlglot.ParseError as e:
        logger.error(f'Failed to parse SQL: {e}', exc_info=True)
        raise JSQLSyntaxError(
            message=f'Failed to parse SQL: {e!s}',
            path='',
        ) from e
    except JSQLSyntaxError:
        raise
    except Exception as e:
        logger.error(f'Unexpected error during SQL conversion: {e}', exc_info=True)
        raise JSQLSyntaxError(
            message=f'Failed to convert SQL to JSQL: {e!s}',
            path='',
        ) from e


def convert_sqlglot_select_to_jsql(parsed: exp.Select) -> dict[str, Any]:
    """
    Convert sqlglot Select expression to JSQL query dictionary.

    Args:
        parsed: SQLGlot Select expression

    Returns:
        JSQL query dictionary
    """
    return _convert_sqlglot_select_to_jsql(parsed)


def convert_order_to_jsql(order_expr: exp.Ordered) -> dict[str, Any]:
    """
    Convert sqlglot ORDER BY expression to JSQL order.
    """
    return _convert_order_to_jsql(order_expr)
