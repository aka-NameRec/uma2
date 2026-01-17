"""JSQL to SQL conversion."""

import logging
from typing import Any

import sqlglot
import sqlglot.expressions as exp
import sqlparse

from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.converter.conditions import jsql_condition_to_sqlglot
from namerec.uma.jsql.converter.expressions import jsql_expression_to_sqlglot
from namerec.uma.jsql.converter.joins import jsql_join_to_sqlglot
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLQuery

logger = logging.getLogger(__name__)


def jsql_to_sql(jsql: JSQLQuery, dialect: str = 'generic') -> str:
    """
    Convert JSQL query to SQL string.

    This function directly converts JSQL to SQL using sqlglot expressions,
    without requiring database metadata or connection.

    Args:
        jsql: JSQL query dictionary
        dialect: SQL dialect to generate (generic, postgresql, mysql, sqlite, etc.)

    Returns:
        Formatted SQL string

    Raises:
        JSQLSyntaxError: If JSQL syntax is invalid

    Example:
        >>> jsql = {
        ...     "from": "users",
        ...     "select": [{"field": "id"}, {"field": "name"}],
        ...     "where": {"field": "active", "op": "=", "value": True}
        ... }
        >>> sql = jsql_to_sql(jsql)
        >>> print(sql)
        SELECT users.id,
               users.name
        FROM users
        WHERE users.active = TRUE
    """
    logger.info(f'Converting JSQL to SQL, dialect={dialect}')
    logger.debug(f'Input JSQL: {jsql}')

    try:
        query = jsql_query_to_sqlglot(jsql)

        # Generate SQL
        sql_str = query.sql(dialect=dialect if dialect != 'generic' else None, pretty=True)

        # Format for readability
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


def jsql_query_to_sqlglot(jsql: dict[str, Any]) -> exp.Select:
    """
    Convert JSQL query dictionary to sqlglot Select expression.
    
    This is the core conversion function that builds the SQL query structure.
    """
    # Handle CTE (WITH clause) if present
    ctes = []
    if with_clause := jsql.get('with'):
        for cte_spec in with_clause:
            if 'name' not in cte_spec:
                raise MissingFieldError('name', path='with[].name', context='CTE must have name')
            if 'query' not in cte_spec:
                raise MissingFieldError('query', path='with[].query', context='CTE must have query')
            
            cte_query = jsql_query_to_sqlglot(cte_spec['query'])
            cte_alias = exp.TableAlias(this=exp.Identifier(this=cte_spec['name']))
            cte = exp.CTE(this=cte_query, alias=cte_alias)
            ctes.append(cte)
    
    # Build SELECT expression
    select_exprs = jsql_select_to_sqlglot(jsql.get('select', []))

    # Build FROM expression (will raise exception if invalid/missing)
    from_expr = jsql_from_to_sqlglot(jsql.get('from'))

    # Start building query
    if ctes:
        with_clause = exp.With(expressions=ctes)
        query = exp.Select(expressions=select_exprs, with_=with_clause)
    else:
        query = exp.Select(expressions=select_exprs)
    query = query.from_(from_expr)

    # Add WHERE clause
    if where_spec := jsql.get('where'):
        where_expr = jsql_condition_to_sqlglot(where_spec)
        query = query.where(where_expr)

    # Add JOINs
    if joins := jsql.get('joins'):
        for join_spec in joins:
            join_expr = jsql_join_to_sqlglot(join_spec)
            query = query.join(
                join_expr['table'],
                on=join_expr.get('on'),
                join_type=join_expr.get('type', JoinType.INNER.value),
            )

    # Add GROUP BY
    if group_by := jsql.get('group_by'):
        for group_expr_spec in group_by:
            group_expr = jsql_expression_to_sqlglot(group_expr_spec)
            if group_expr:
                query = query.group_by(group_expr)

    # Add HAVING
    if having_spec := jsql.get('having'):
        having_expr = jsql_condition_to_sqlglot(having_spec)
        query.args['having'] = exp.Having(this=having_expr)

    # Add ORDER BY
    if order_by := jsql.get('order_by'):
        order_exprs = []
        for order_spec in order_by:
            # Handle both string and dict formats
            if isinstance(order_spec, dict):
                order_expr = jsql_expression_to_sqlglot(order_spec.get('field') or order_spec)
                direction = order_spec.get('direction', OrderDirection.ASC.value).upper()
            else:
                # If order_spec is a string, use it directly
                order_expr = jsql_expression_to_sqlglot(order_spec)
                direction = OrderDirection.ASC.value.upper()

            if order_expr:
                desc = direction == OrderDirection.DESC.value.upper()
                # Create Ordered expression directly to preserve DESC
                ordered_expr = exp.Ordered(this=order_expr, desc=desc)
                order_exprs.append(ordered_expr)
        
        if order_exprs:
            query = query.order_by(*order_exprs)

    # Add LIMIT
    if limit := jsql.get('limit'):
        query.args['limit'] = exp.Limit(expression=exp.Literal.number(limit))

    # Add OFFSET
    if offset := jsql.get('offset'):
        query.args['offset'] = exp.Offset(expression=exp.Literal.number(offset))

    return query


def jsql_select_to_sqlglot(select_spec: list[dict[str, Any]] | None) -> list[exp.Expression]:
    """Convert JSQL SELECT to sqlglot expressions."""
    if not select_spec:
        return [exp.Star()]

    result = []
    for field_spec in select_spec:
        # Handle window functions (OVER clause) - check before converting expression
        if isinstance(field_spec, dict) and 'over' in field_spec:
            # For window functions, convert the function/field first
            # Remove 'over' temporarily to convert base expression
            over_spec = field_spec['over']
            field_spec_no_over = {k: v for k, v in field_spec.items() if k != 'over'}
            field_expr = jsql_expression_to_sqlglot(field_spec_no_over)
            
            partition_by = []
            order_by_exprs = []
            
            if 'partition_by' in over_spec:
                for part_spec in over_spec['partition_by']:
                    part_expr = jsql_expression_to_sqlglot(part_spec)
                    if part_expr:
                        partition_by.append(part_expr)
            
            if 'order_by' in over_spec:
                for order_spec in over_spec['order_by']:
                    if isinstance(order_spec, dict):
                        order_expr = jsql_expression_to_sqlglot(order_spec.get('field') or order_spec)
                        desc = order_spec.get('direction', 'ASC').upper() == 'DESC'
                    else:
                        order_expr = jsql_expression_to_sqlglot(order_spec)
                        desc = False
                    if order_expr:
                        order_by_exprs.append(exp.Ordered(this=order_expr, desc=desc))
            
            # Create window expression
            # sqlglot Window uses partition_by (not partition) and order
            partition = None
            order = None
            if partition_by:
                partition = exp.Partition(expressions=partition_by)
            if order_by_exprs:
                order = exp.Order(expressions=order_by_exprs)
            
            # Wrap expression with Window
            field_expr = exp.Window(
                this=field_expr,
                partition_by=partition,
                order=order,
            )
        else:
            # Normal expression (no window function)
            field_expr = jsql_expression_to_sqlglot(field_spec)
        
        if field_expr:
            # Check if field_spec is a dict before trying to get 'alias'
            alias = field_spec.get('alias') if isinstance(field_spec, dict) else None
            if alias:
                field_expr = exp.alias_(field_expr, alias)
            result.append(field_expr)

    return result if result else [exp.Star()]


def jsql_from_to_sqlglot(from_spec: str | dict[str, Any] | None) -> exp.Table:
    """
    Convert JSQL FROM to sqlglot table.

    Raises:
        MissingFieldError: If FROM clause is missing or empty
        InvalidExpressionError: If FROM structure is invalid
    """
    if not from_spec:
        raise MissingFieldError('from', path='from', context='SELECT query requires FROM clause')

    if isinstance(from_spec, str):
        return exp.table_(from_spec)

    if isinstance(from_spec, dict):
        if 'entity' not in from_spec:
            raise MissingFieldError('entity', path='from.entity', context='FROM with alias')

        entity = from_spec['entity']
        alias = from_spec.get('alias')
        table = exp.table_(entity)
        if alias:
            table = exp.alias_(table, alias, table=True)
        return table

    raise InvalidExpressionError(
        message=f'FROM must be string or dict, got {type(from_spec).__name__}',
        path='from',
        expression={'type': type(from_spec).__name__, 'value': repr(from_spec)}
    )
