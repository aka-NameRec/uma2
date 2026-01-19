"""SQL to JSQL conversion."""

import logging
from time import perf_counter
from typing import Any
from typing import cast

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
    started_at = perf_counter()
    parse_ms: float | None = None
    convert_ms: float | None = None

    try:
        # Parse SQL using sqlglot
        parse_start = perf_counter()
        parsed = sqlglot.parse_one(sql, read=dialect if dialect != 'generic' else None)
        parse_ms = (perf_counter() - parse_start) * 1000
    except sqlglot.ParseError as e:
        logger.exception('Failed to parse SQL', exc_info=False)
        raise JSQLSyntaxError(
            message=f'Failed to parse SQL: {e!s}',
            path='',
        ) from e
    except JSQLSyntaxError:
        raise
    except Exception as e:
        logger.exception('Unexpected error during SQL conversion')
        raise JSQLSyntaxError(
            message=f'Failed to convert SQL to JSQL: {e!s}',
            path='',
        ) from e
    else:
        if not isinstance(parsed, exp.Select):
            raise JSQLSyntaxError(
                message=f'Only SELECT queries are supported, got {type(parsed).__name__}',
                path='',
            )

        # Convert to JSQL
        convert_start = perf_counter()
        jsql = _convert_sqlglot_select_to_jsql(parsed)
        convert_ms = (perf_counter() - convert_start) * 1000

        logger.info('Successfully converted SQL to JSQL')
        logger.debug(f'Generated JSQL: {jsql}')
        return jsql
    finally:
        total_ms = (perf_counter() - started_at) * 1000
        parse_label = f'{parse_ms:.2f}ms' if parse_ms is not None else 'n/a'
        convert_label = f'{convert_ms:.2f}ms' if convert_ms is not None else 'n/a'
        logger.info(
            'SQL to JSQL timing: total=%0.2fms parse=%s convert=%s',
            total_ms,
            parse_label,
            convert_label,
        )


def convert_sqlglot_select_to_jsql(parsed: exp.Select) -> dict[str, Any]:
    """
    Convert sqlglot Select expression to JSQL query dictionary.

    Args:
        parsed: SQLGlot Select expression

    Returns:
        JSQL query dictionary
    """
    return cast('dict[str, Any]', _convert_sqlglot_select_to_jsql(parsed))


def convert_order_to_jsql(order_expr: exp.Ordered) -> dict[str, Any]:
    """
    Convert sqlglot ORDER BY expression to JSQL order.
    """
    return cast('dict[str, Any]', _convert_order_to_jsql(order_expr))
