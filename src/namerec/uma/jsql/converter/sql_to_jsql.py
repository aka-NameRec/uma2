"""SQL to JSQL conversion."""

import logging
from typing import Any

import sqlglot
import sqlglot.expressions as exp

from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.converter.conditions.to_jsql import convert_condition_to_jsql
from namerec.uma.jsql.converter.expressions import convert_expression_to_jsql
from namerec.uma.jsql.converter.joins import convert_join_to_jsql
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
        jsql = convert_sqlglot_select_to_jsql(parsed)

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
    result: dict[str, Any] = {}

    # WITH clause (CTEs) - sqlglot uses 'with_' key
    if parsed.args.get('with_'):
        from namerec.uma.jsql.converter.subqueries import convert_sqlglot_select_to_jsql
        
        ctes = []
        for cte in parsed.args['with_'].expressions:
            if isinstance(cte, exp.CTE):
                cte_name = cte.alias
                cte_query = convert_sqlglot_select_to_jsql(cte.this)
                ctes.append({'name': cte_name, 'query': cte_query})
        if ctes:
            result['with'] = ctes

    # FROM clause
    if from_expr := parsed.args.get('from_'):
        if isinstance(from_expr, exp.From):
            table = from_expr.this
            if isinstance(table, exp.Table):
                if table.alias:
                    result['from'] = {
                        'entity': table.name,
                        'alias': table.alias,
                    }
                else:
                    result['from'] = table.name

    # SELECT clause
    if parsed.expressions:
        select_fields = []
        for expr in parsed.expressions:
            field_dict = convert_expression_to_jsql(expr)
            select_fields.append(field_dict)
        if select_fields:
            result['select'] = select_fields

    # WHERE clause
    if parsed.args.get('where'):
        where_expr = parsed.args['where'].this
        where_jsql = convert_condition_to_jsql(where_expr)
        result['where'] = where_jsql

    # JOIN clauses
    if parsed.args.get('joins'):
        joins = []
        for join in parsed.args['joins']:
            join_dict = convert_join_to_jsql(join)
            joins.append(join_dict)
        if joins:
            result['joins'] = joins

    # ORDER BY clause
    if parsed.args.get('order'):
        order_by = []
        for order_expr in parsed.args['order'].expressions:
            order_dict = convert_order_to_jsql(order_expr)
            order_by.append(order_dict)
        if order_by:
            result['order_by'] = order_by

    # LIMIT clause
    if parsed.args.get('limit'):
        limit_expr = parsed.args['limit'].expression
        if isinstance(limit_expr, exp.Literal):
            result['limit'] = int(limit_expr.this)

    # OFFSET clause
    if parsed.args.get('offset'):
        offset_expr = parsed.args['offset'].expression
        if isinstance(offset_expr, exp.Literal):
            result['offset'] = int(offset_expr.this)

    # GROUP BY clause
    if parsed.args.get('group'):
        group_by = []
        for group_expr in parsed.args['group'].expressions:
            field_dict = convert_expression_to_jsql(group_expr)
            group_by.append(field_dict)
        if group_by:
            result['group_by'] = group_by

    # HAVING clause
    if parsed.args.get('having'):
        having_expr = parsed.args['having'].this
        having_jsql = convert_condition_to_jsql(having_expr)
        result['having'] = having_jsql

    return result


def convert_order_to_jsql(order_expr: exp.Ordered) -> dict[str, Any]:
    """
    Convert sqlglot ORDER BY expression to JSQL order.

    Raises:
        InvalidExpressionError: If ORDER BY structure is invalid
    """
    if not isinstance(order_expr, exp.Ordered):
        raise InvalidExpressionError(
            message=f'Expected exp.Ordered, got {type(order_expr).__name__}',
            path='order_by',
            expression={'type': type(order_expr).__name__}
        )

    field_expr = convert_expression_to_jsql(order_expr.this)
    result = field_expr.copy()
    # Use OrderDirection constant and uppercase for consistency
    direction = OrderDirection.DESC if order_expr.args.get('desc') else OrderDirection.ASC
    result['direction'] = direction.value.upper()

    return result
