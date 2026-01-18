"""Subquery conversion between SQLGlot and JSQL formats.

This module handles bidirectional conversion of subqueries between SQLGlot
expression trees and JSQL query dictionaries. It provides functions for
converting subqueries in contexts like EXISTS, NOT EXISTS, IN, and SELECT clauses.

Functions in this module depend only on lower-level converter modules
(conditions, expressions, joins), avoiding circular dependencies.
"""

import logging
from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.converter.conditions.to_jsql import convert_condition_to_jsql
from namerec.uma.jsql.converter.conditions.to_sql import jsql_condition_to_sqlglot
from namerec.uma.jsql.converter.expressions import convert_expression_to_jsql
from namerec.uma.jsql.converter.expressions import jsql_expression_to_sqlglot
from namerec.uma.jsql.converter.joins import convert_join_to_jsql
from namerec.uma.jsql.converter.joins import jsql_join_to_sqlglot
from namerec.uma.jsql.converter.sql_to_jsql import convert_order_to_jsql

logger = logging.getLogger(__name__)


def convert_sqlglot_select_to_jsql(select_expr: exp.Select) -> dict[str, Any]:
    """
    Convert SQLGlot Select expression to JSQL query dictionary.
    
    This is a helper function primarily used for converting subqueries
    in EXISTS and NOT EXISTS conditions, CTEs, and SELECT subqueries.
    It converts a complete SELECT statement to its JSQL representation.
    
    Args:
        select_expr: SQLGlot Select expression object.
    
    Returns:
        JSQL query dictionary containing:
            - 'from': Table name or specification
            - 'select': List of selected fields (optional)
            - 'where': Condition specification (optional)
            - 'joins': List of join specifications (optional)
            - 'order_by': List of order specifications (optional)
            - 'limit': Limit value (optional)
            - 'offset': Offset value (optional)
            - 'group_by': List of group by fields (optional)
            - 'having': Having condition (optional)
    
    Example:
        >>> from sqlglot import parse_one
        >>> sql = "SELECT id FROM users WHERE age > 18"
        >>> select_expr = parse_one(sql)
        >>> jsql = convert_sqlglot_select_to_jsql(select_expr)
        >>> # Returns: {
        ... #     "from": "users",
        ... #     "select": [{"field": "id"}],
        ... #     "where": {"op": ">", "left": {"field": "age"}, "right": {"value": 18}}
        ... # }
    """
    jsql: dict[str, Any] = {}
    
    # FROM clause
    if from_expr := select_expr.args.get('from_'):
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
    if select_expr.expressions:
        select_fields = []
        for expr in select_expr.expressions:
            field_dict = convert_expression_to_jsql(expr)
            select_fields.append(field_dict)
        if select_fields:
            jsql['select'] = select_fields
    
    # WHERE clause
    if select_expr.args.get('where'):
        where_expr = select_expr.args['where'].this
        where_jsql = convert_condition_to_jsql(where_expr)
        jsql['where'] = where_jsql
    
    # JOIN clauses
    if select_expr.args.get('joins'):
        joins = []
        for join in select_expr.args['joins']:
            join_dict = convert_join_to_jsql(join)
            joins.append(join_dict)
        if joins:
            jsql['joins'] = joins
    
    # ORDER BY clause
    if select_expr.args.get('order'):
        order_by = []
        for order_expr in select_expr.args['order'].expressions:
            order_dict = convert_order_to_jsql(order_expr)
            order_by.append(order_dict)
        if order_by:
            jsql['order_by'] = order_by
    
    # LIMIT clause
    if select_expr.args.get('limit'):
        limit_expr = select_expr.args['limit'].expression
        if isinstance(limit_expr, exp.Literal):
            jsql['limit'] = int(limit_expr.this)
    
    # OFFSET clause
    if select_expr.args.get('offset'):
        offset_expr = select_expr.args['offset'].expression
        if isinstance(offset_expr, exp.Literal):
            jsql['offset'] = int(offset_expr.this)
    
    # GROUP BY clause
    if select_expr.args.get('group'):
        group_by = []
        for group_expr in select_expr.args['group'].expressions:
            field_dict = convert_expression_to_jsql(group_expr)
            group_by.append(field_dict)
        if group_by:
            jsql['group_by'] = group_by
    
    # HAVING clause
    if select_expr.args.get('having'):
        having_expr = select_expr.args['having'].this
        having_jsql = convert_condition_to_jsql(having_expr)
        jsql['having'] = having_jsql
    
    return jsql


def jsql_query_to_sqlglot_select(query_spec: dict[str, Any]) -> exp.Select:
    """
    Convert JSQL query to sqlglot Select expression.
    
    Helper function for EXISTS and NOT EXISTS subqueries.
    This is a simplified version that doesn't handle CTEs.
    
    Args:
        query_spec: JSQL query dictionary (subquery specification)
    
    Returns:
        SQLGlot Select expression
    
    Example:
        >>> jsql = {
        ...     "from": "users",
        ...     "select": [{"field": "id"}],
        ...     "where": {"op": ">", "left": {"field": "age"}, "right": {"value": 18}}
        ... }
        >>> select_expr = jsql_query_to_sqlglot_select(jsql)
        >>> # Returns: exp.Select representing the query
    """
    
    # Build SELECT expressions
    select_exprs = [
        jsql_expression_to_sqlglot(field_spec)
        for field_spec in query_spec.get('select', [])
    ]
    
    # Build FROM expression
    from_spec = query_spec.get('from')
    if not from_spec:
        raise MissingFieldError('from', path='from', context='SELECT query requires FROM clause')
    
    if isinstance(from_spec, str):
        from_expr = exp.table_(from_spec)
    elif isinstance(from_spec, dict):
        if 'entity' not in from_spec:
            raise MissingFieldError('entity', path='from.entity', context='FROM with alias')
        entity = from_spec['entity']
        alias = from_spec.get('alias')
        from_expr = exp.table_(entity)
        if alias:
            from_expr = exp.alias_(from_expr, alias, table=True)
    else:
        raise InvalidExpressionError(
            message=f'FROM must be string or dict, got {type(from_spec).__name__}',
            path='from',
            expression={'type': type(from_spec).__name__, 'value': repr(from_spec)}
        )
    
    # Start building query
    query = exp.Select(expressions=select_exprs)
    query = query.from_(from_expr)
    
    # Add WHERE clause
    if where_spec := query_spec.get('where'):
        where_expr = jsql_condition_to_sqlglot(where_spec)
        query = query.where(where_expr)
    
    # Add JOINs
    if joins := query_spec.get('joins'):
        for join_spec in joins:
            join_expr = jsql_join_to_sqlglot(join_spec)
            query = query.join(
                join_expr['table'],
                on=join_expr.get('on'),
                join_type=join_expr.get('type', JoinType.INNER.value),
            )
    
    # Add GROUP BY
    if group_by := query_spec.get('group_by'):
        for group_expr_spec in group_by:
            group_expr = jsql_expression_to_sqlglot(group_expr_spec)
            if group_expr:
                query = query.group_by(group_expr)
    
    # Add HAVING
    if having_spec := query_spec.get('having'):
        having_expr = jsql_condition_to_sqlglot(having_spec)
        query.args['having'] = exp.Having(this=having_expr)
    
    # Add ORDER BY
    if order_by := query_spec.get('order_by'):
        for order_spec in order_by:
            if isinstance(order_spec, dict):
                order_expr = jsql_expression_to_sqlglot(order_spec.get('field') or order_spec)
                direction = order_spec.get('direction', OrderDirection.ASC.value).upper()
            else:
                order_expr = jsql_expression_to_sqlglot(order_spec)
                direction = OrderDirection.ASC.value.upper()
            
            if order_expr:
                desc = direction == OrderDirection.DESC.value.upper()
                query = query.order_by(order_expr, desc=desc)
    
    # Add LIMIT
    if limit := query_spec.get('limit'):
        query.args['limit'] = exp.Limit(expression=exp.Literal.number(limit))
    
    # Add OFFSET
    if offset := query_spec.get('offset'):
        query.args['offset'] = exp.Offset(expression=exp.Literal.number(offset))
    
    return query
