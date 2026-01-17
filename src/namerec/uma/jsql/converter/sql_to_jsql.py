"""SQL to JSQL conversion."""

import logging
from typing import Any

import sqlglot
import sqlglot.expressions as exp

from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.converter.conditions import convert_condition_to_jsql
from namerec.uma.jsql.converter.expressions import convert_expression_to_jsql
from namerec.uma.jsql.converter.joins import convert_join_to_jsql
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLQuery

logger = logging.getLogger(__name__)


def sql_to_jsql(sql: str, dialect: str = 'generic') -> JSQLQuery:
    """
    Convert SQL query to JSQL dictionary.

    This function parses SQL using sqlglot and converts it to JSQL format.
    Currently supports SELECT queries with WHERE, JOIN, ORDER BY, LIMIT, OFFSET.

    Args:
        sql: SQL query string
        dialect: SQL dialect to parse (generic, postgresql, mysql, sqlite, etc.)

    Returns:
        JSQL query dictionary

    Raises:
        JSQLSyntaxError: If SQL cannot be parsed or converted

    Example:
        >>> sql = "SELECT id, name FROM users WHERE active = 1 ORDER BY name LIMIT 10"
        >>> jsql = sql_to_jsql(sql)
        >>> print(jsql)
        {
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'where': {'field': 'active', 'op': '=', 'value': 1},
            'order_by': [{'field': 'name', 'direction': 'ASC'}],
            'limit': 10
        }
    """
    logger.info(f'Converting SQL to JSQL, dialect={dialect}')
    logger.debug(f'Input SQL: {sql}')

    try:
        # Parse SQL with sqlglot
        # Use None for generic dialect
        read_dialect = None if dialect == 'generic' else dialect
        logger.debug(f"Parsing SQL with dialect: {read_dialect or 'generic'}")
        parsed = sqlglot.parse_one(sql, read=read_dialect)

        logger.debug(f'Parsed expression type: {type(parsed).__name__}')

        if not isinstance(parsed, exp.Select):
            logger.warning(f'Unsupported query type: {type(parsed).__name__}')
            raise JSQLSyntaxError(
                message='Only SELECT queries are supported',
                path='',
            )

        # Convert to JSQL
        jsql: JSQLQuery = {}

        # WITH clause (CTEs) - sqlglot uses 'with_' key
        if parsed.args.get('with_'):
            from namerec.uma.jsql.converter.conditions import _convert_sqlglot_select_to_jsql
            ctes = []
            for cte in parsed.args['with_'].expressions:
                if isinstance(cte, exp.CTE):
                    cte_name = cte.alias
                    cte_query = _convert_sqlglot_select_to_jsql(cte.this)
                    ctes.append({
                        'name': cte_name,
                        'query': cte_query,
                    })
            if ctes:
                jsql['with'] = ctes

        # FROM clause
        if from_expr := parsed.args.get('from_'):
            if isinstance(from_expr, exp.From):
                table = from_expr.this
                if isinstance(table, exp.Table):
                    if table.alias:
                        jsql['from'] = {
                            'entity': table.name,
                            'alias': table.alias,
                        }
                    else:
                        jsql['from'] = table.name

        # SELECT clause
        if parsed.expressions:
            select_fields = []
            for expr in parsed.expressions:
                field_dict = convert_expression_to_jsql(expr)
                select_fields.append(field_dict)
            if select_fields:
                jsql['select'] = select_fields

        # WHERE clause
        if parsed.args.get('where'):
            where_expr = parsed.args['where'].this
            where_jsql = convert_condition_to_jsql(where_expr)
            jsql['where'] = where_jsql

        # JOIN clauses
        if parsed.args.get('joins'):
            joins = []
            for join in parsed.args['joins']:
                join_dict = convert_join_to_jsql(join)
                joins.append(join_dict)
            if joins:
                jsql['joins'] = joins

        # ORDER BY clause
        if parsed.args.get('order'):
            order_by = []
            for order_expr in parsed.args['order'].expressions:
                order_dict = convert_order_to_jsql(order_expr)
                order_by.append(order_dict)
            if order_by:
                jsql['order_by'] = order_by

        # LIMIT clause
        if parsed.args.get('limit'):
            limit_expr = parsed.args['limit'].expression
            if isinstance(limit_expr, exp.Literal):
                jsql['limit'] = int(limit_expr.this)

        # OFFSET clause
        if parsed.args.get('offset'):
            offset_expr = parsed.args['offset'].expression
            if isinstance(offset_expr, exp.Literal):
                jsql['offset'] = int(offset_expr.this)

        # GROUP BY clause
        if parsed.args.get('group'):
            group_by = []
            for group_expr in parsed.args['group'].expressions:
                field_dict = convert_expression_to_jsql(group_expr)
                group_by.append(field_dict)
            if group_by:
                jsql['group_by'] = group_by

        # HAVING clause
        if parsed.args.get('having'):
            having_expr = parsed.args['having'].this
            having_jsql = convert_condition_to_jsql(having_expr)
            jsql['having'] = having_jsql

        logger.info('Successfully converted SQL to JSQL')
        logger.debug(f'Generated JSQL: {jsql}')
        return jsql

    except JSQLSyntaxError as e:
        logger.error(f'Failed to parse SQL: {e}', exc_info=True)
        raise
    except Exception as e:
        logger.error(f'Unexpected error during SQL parsing: {e}', exc_info=True)
        raise JSQLSyntaxError(
            message=f'Failed to parse SQL: {e!s}',
            path='',
        ) from e


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
