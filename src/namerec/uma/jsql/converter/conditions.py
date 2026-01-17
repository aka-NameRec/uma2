"""Condition handling for JSQL <-> SQL conversion."""

import logging
from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import LOGICAL_OPERATORS
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.conversion_exceptions import UnsupportedOperationError
from namerec.uma.jsql.converter.expressions import convert_expression_to_jsql
from namerec.uma.jsql.converter.expressions import jsql_expression_to_sqlglot
from namerec.uma.jsql.converter.operators import COMPARISON_OP_TO_SQLGLOT
from namerec.uma.jsql.converter.operators import LOGICAL_OP_TO_SQLGLOT
from namerec.uma.jsql.converter.operators import SQLGLOT_TO_COMPARISON

logger = logging.getLogger(__name__)


def _extract_left_expression(cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Extract left expression from condition spec.
    
    Supports both old format ('field') and new format ('left').
    
    Args:
        cond_spec: Condition specification
        
    Returns:
        SQLGlot expression for left side
        
    Raises:
        MissingFieldError: If neither 'field' nor 'left' is present
    """
    if 'left' in cond_spec:
        return jsql_expression_to_sqlglot(cond_spec['left'])
    elif 'field' in cond_spec:
        return jsql_expression_to_sqlglot(cond_spec['field'])
    else:
        raise MissingFieldError('left or field', path='left/field', context='condition')


def _extract_pattern(cond_spec: dict[str, Any]) -> str | None:
    """
    Extract pattern from condition spec for string operators.
    
    Supports both 'pattern' field and 'right' expression formats.
    If 'right' is a string literal, extracts the value.
    If 'right' is an expression, returns None (caller should use expression directly).
    
    Args:
        cond_spec: Condition specification
        
    Returns:
        Pattern string if found, None if should use expression directly
    """
    if 'pattern' in cond_spec:
        return cond_spec['pattern']
    
    if 'right' in cond_spec:
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
        if isinstance(right_expr, exp.Literal) and right_expr.is_string:
            return right_expr.this
    
    return None


def convert_is_null(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert IS NULL operator to sqlglot expression."""
    left_expr = _extract_left_expression(cond_spec)
    return exp.Is(this=left_expr, expression=exp.Null())


def convert_is_not_null(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert IS NOT NULL operator to sqlglot expression."""
    left_expr = _extract_left_expression(cond_spec)
    return exp.Not(this=exp.Is(this=left_expr, expression=exp.Null()))


def convert_between(cond_spec: dict[str, Any], negate: bool = False) -> exp.Expression:
    """
    Convert BETWEEN operator to sqlglot expression.
    
    Args:
        cond_spec: Condition specification
        negate: Whether to negate (for NOT BETWEEN)
        
    Returns:
        SQLGlot expression
    """
    if 'expr' not in cond_spec:
        raise MissingFieldError('expr', path='expr', context='BETWEEN operator')
    if 'low' not in cond_spec:
        raise MissingFieldError('low', path='low', context='BETWEEN operator')
    if 'high' not in cond_spec:
        raise MissingFieldError('high', path='high', context='BETWEEN operator')
    
    expr = jsql_expression_to_sqlglot(cond_spec['expr'])
    low_expr = jsql_expression_to_sqlglot(cond_spec['low'])
    high_expr = jsql_expression_to_sqlglot(cond_spec['high'])
    
    logger.debug(f'Processing {"NOT " if negate else ""}BETWEEN operator')
    between_expr = exp.Between(this=expr, low=low_expr, high=high_expr)
    return exp.Not(this=between_expr) if negate else between_expr


def convert_in_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert IN or NOT IN operator to sqlglot expression.
    
    Args:
        op: Operator ('IN' or 'NOT IN')
        cond_spec: Condition specification
        
    Returns:
        SQLGlot expression
    """
    left_expr = _extract_left_expression(cond_spec)
    negate = op == JSQLOperator.NOT_IN.value
    
    # Extract values or subquery
    if 'values' in cond_spec:
        values = cond_spec['values']
    elif 'right' in cond_spec:
        right_spec = cond_spec['right']
        if isinstance(right_spec, dict) and 'values' in right_spec:
            values = right_spec['values']
        elif isinstance(right_spec, list):
            values = right_spec
        else:
            # Subquery or single expression
            right_expr = jsql_expression_to_sqlglot(right_spec)
            in_expr = exp.In(this=left_expr, expressions=[right_expr])
            return exp.NotIn(this=left_expr, expressions=[right_expr]) if negate else in_expr
    else:
        raise MissingFieldError('values or right', path='values/right', context=f'{op} operator')
    
    logger.debug(f'Processing {op} operator with {len(values)} values')
    val_exprs = [jsql_expression_to_sqlglot({'value': v}) for v in values]
    in_expr = exp.In(this=left_expr, expressions=val_exprs)
    return exp.Not(this=in_expr) if negate else in_expr


def convert_string_operator(
    op: str,
    cond_spec: dict[str, Any],
    left_expr: exp.Expression
) -> exp.Expression:
    """
    Convert string matching operator (LIKE, ILIKE, SIMILAR_TO, REGEXP, RLIKE) to sqlglot expression.
    
    Args:
        op: String operator
        cond_spec: Condition specification
        left_expr: Left expression (already extracted)
        
    Returns:
        SQLGlot expression
    """
    pattern = _extract_pattern(cond_spec)
    
    # If pattern is None, use right expression directly (might be column reference or function)
    if pattern is None:
        if 'right' not in cond_spec:
            raise MissingFieldError('pattern or right', path='pattern/right', context=f'{op} operator')
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
        
        # Map operator to SQLGlot class for expression-based matching
        if op == JSQLOperator.LIKE.value:
            return exp.Like(this=left_expr, expression=right_expr)
        elif op == JSQLOperator.NOT_LIKE.value:
            return exp.Not(this=exp.Like(this=left_expr, expression=right_expr))
        elif op == JSQLOperator.ILIKE.value:
            return exp.ILike(this=left_expr, expression=right_expr)
        elif op == JSQLOperator.NOT_ILIKE.value:
            return exp.NotILike(this=left_expr, expression=right_expr)
        elif op == JSQLOperator.SIMILAR_TO.value:
            return exp.SimilarTo(this=left_expr, expression=right_expr)
        elif op == JSQLOperator.REGEXP.value:
            return exp.RegexpLike(this=left_expr, expression=right_expr)
        elif op == JSQLOperator.RLIKE.value:
            return exp.RegexpLike(this=left_expr, expression=right_expr)
        else:
            raise UnknownOperatorError(operator=op, path='op', supported=[
                JSQLOperator.LIKE.value, JSQLOperator.NOT_LIKE.value,
                JSQLOperator.ILIKE.value, JSQLOperator.NOT_ILIKE.value,
                JSQLOperator.SIMILAR_TO.value, JSQLOperator.REGEXP.value, JSQLOperator.RLIKE.value
            ])
    
    # Use pattern as string literal
    pattern_literal = exp.Literal.string(pattern)
    logger.debug(f'Processing {op} operator with pattern: {pattern}')
    
    # Map operator to SQLGlot class for pattern-based matching
    if op == JSQLOperator.LIKE.value:
        return exp.Like(this=left_expr, expression=pattern_literal)
    elif op == JSQLOperator.NOT_LIKE.value:
        return exp.Not(this=exp.Like(this=left_expr, expression=pattern_literal))
    elif op == JSQLOperator.ILIKE.value:
        return exp.ILike(this=left_expr, expression=pattern_literal)
    elif op == JSQLOperator.NOT_ILIKE.value:
        return exp.NotILike(this=left_expr, expression=pattern_literal)
    elif op == JSQLOperator.SIMILAR_TO.value:
        return exp.SimilarTo(this=left_expr, expression=pattern_literal)
    elif op == JSQLOperator.REGEXP.value:
        return exp.RegexpLike(this=left_expr, expression=pattern_literal)
    elif op == JSQLOperator.RLIKE.value:
        return exp.RegexpLike(this=left_expr, expression=pattern_literal)
    else:
        raise UnknownOperatorError(operator=op, path='op', supported=[
            JSQLOperator.LIKE.value, JSQLOperator.NOT_LIKE.value,
            JSQLOperator.ILIKE.value, JSQLOperator.NOT_ILIKE.value,
            JSQLOperator.SIMILAR_TO.value, JSQLOperator.REGEXP.value, JSQLOperator.RLIKE.value
        ])


def convert_standard_comparison(
    op: str,
    cond_spec: dict[str, Any],
    left_expr: exp.Expression,
    right_expr: exp.Expression
) -> exp.Expression:
    """
    Convert standard comparison operator to sqlglot expression.
    
    Args:
        op: Comparison operator
        cond_spec: Condition specification
        left_expr: Left expression
        right_expr: Right expression
        
    Returns:
        SQLGlot expression
    """
    operator_class = COMPARISON_OP_TO_SQLGLOT.get(op)
    if not operator_class:
        raise UnknownOperatorError(
            operator=op,
            path='op',
            supported=list(COMPARISON_OP_TO_SQLGLOT.keys())
        )
    return operator_class(this=left_expr, expression=right_expr)


def jsql_condition_to_sqlglot(cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert JSQL condition to sqlglot expression.

    Delegates to specialized functions based on operator type.

    Raises:
        InvalidExpressionError: If condition structure is invalid
        MissingFieldError: If required operator field is missing
    """
    if not isinstance(cond_spec, dict):
        raise InvalidExpressionError(
            message=f'Condition must be a dict, got {type(cond_spec).__name__}',
            path='',
            expression={'type': type(cond_spec).__name__, 'value': repr(cond_spec)}
        )

    if 'op' not in cond_spec:
        raise MissingFieldError('op', path='op', context='condition')

    op = cond_spec['op']

    # Handle EXISTS and NOT EXISTS separately (don't follow comparison pattern)
    if op == JSQLOperator.EXISTS.value:
        return convert_exists_operator(cond_spec)
    if op == JSQLOperator.NOT_EXISTS.value:
        return convert_not_exists_operator(cond_spec)

    # Try logical operators first (use constant set)
    if op in LOGICAL_OPERATORS:
        return convert_logical_operator(op, cond_spec)

    # Then try comparison operators
    return convert_comparison_operator(op, cond_spec)


def convert_logical_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert logical operator (AND/OR/NOT) to sqlglot expression.

    Raises:
        MissingFieldError: If required field is missing
        UnknownOperatorError: If operator is not supported
        InvalidExpressionError: If conditions list is empty
    """
    logger.debug(f'Converting logical operator: {op}')

    # Handle AND/OR operators
    if op in (JSQLOperator.AND.value, JSQLOperator.OR.value):
        if 'conditions' not in cond_spec:
            raise MissingFieldError('conditions', path='conditions', context=f'{op} operator')

        conditions = cond_spec['conditions']
        logger.debug(f'Processing {len(conditions)} conditions for {op}')

        if not conditions:
            raise InvalidExpressionError(
                message=f'{op} operator requires at least one condition',
                path='conditions'
            )

        result = jsql_condition_to_sqlglot(conditions[0])
        operator_class = LOGICAL_OP_TO_SQLGLOT.get(op)
        if not operator_class:
            raise UnknownOperatorError(
                operator=op,
                path='op',
                supported=list(LOGICAL_OP_TO_SQLGLOT.keys())
            )

        for cond in conditions[1:]:
            next_cond = jsql_condition_to_sqlglot(cond)
            result = operator_class(this=result, expression=next_cond)

        logger.debug(f'Built {op} expression with {len(conditions)} conditions')
        return result

    # Handle NOT operator
    if op == JSQLOperator.NOT.value:
        logger.debug('Processing NOT condition')
        if 'condition' not in cond_spec:
            raise MissingFieldError('condition', path='condition', context='NOT operator')

        condition = jsql_condition_to_sqlglot(cond_spec['condition'])
        return exp.Not(this=condition)

    # Unknown logical operator
    logger.warning(f'Unknown logical operator: {op}')
    raise UnknownOperatorError(
        operator=op,
        path='op',
        supported=[JSQLOperator.AND.value, JSQLOperator.OR.value, JSQLOperator.NOT.value]
    )


def _jsql_query_to_sqlglot_select(query_spec: dict[str, Any]) -> exp.Select:
    """
    Convert JSQL query to sqlglot Select expression.
    
    Helper function for EXISTS and NOT EXISTS subqueries.
    """
    # Lazy import to avoid circular dependency
    from namerec.uma.jsql.converter.jsql_to_sql import jsql_from_to_sqlglot
    from namerec.uma.jsql.converter.jsql_to_sql import jsql_join_to_sqlglot
    from namerec.uma.jsql.converter.jsql_to_sql import jsql_select_to_sqlglot
    from namerec.uma.jsql.constants import JSQLField
    from namerec.uma.jsql.constants import JoinType
    from namerec.uma.jsql.constants import OrderDirection
    
    # Build SELECT expressions
    select_exprs = jsql_select_to_sqlglot(query_spec.get('select', []))
    
    # Build FROM expression
    from_expr = jsql_from_to_sqlglot(query_spec.get('from'))
    
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


def convert_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert EXISTS operator to sqlglot expression."""
    if 'subquery' not in cond_spec:
        raise MissingFieldError('subquery', path='subquery', context='EXISTS operator')
    subquery_spec = cond_spec['subquery']
    subquery = _jsql_query_to_sqlglot_select(subquery_spec)
    return exp.Exists(expression=subquery)


def convert_not_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert NOT EXISTS operator to sqlglot expression."""
    if 'subquery' not in cond_spec:
        raise MissingFieldError('subquery', path='subquery', context='NOT EXISTS operator')
    subquery_spec = cond_spec['subquery']
    subquery = _jsql_query_to_sqlglot_select(subquery_spec)
    return exp.NotExists(expression=subquery)


def convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert comparison operator to sqlglot expression.

    Supports both old and new JSQL formats:
    - Old: {"op": ">=", "field": "created", "value": "2025-12-18"}
    - New: {"op": ">=", "left": {"field": "created"}, "right": {"value": "2025-12-18"}}
    - BETWEEN: {"op": "BETWEEN", "expr": {...}, "low": {...}, "high": {...}}

    Raises:
        MissingFieldError: If required field is missing
        UnknownOperatorError: If operator is not supported
    """
    logger.debug(f'Converting comparison operator: {op}')

    # Handle IS NULL and IS NOT NULL first
    if op == JSQLOperator.IS_NULL.value:
        return convert_is_null(cond_spec)
    
    if op == JSQLOperator.IS_NOT_NULL.value:
        return convert_is_not_null(cond_spec)

    # Handle BETWEEN and NOT BETWEEN
    if op == JSQLOperator.BETWEEN.value:
        return convert_between(cond_spec, negate=False)
    
    if op == JSQLOperator.NOT_BETWEEN.value:
        return convert_between(cond_spec, negate=True)

    # Handle IN and NOT IN
    if op in (JSQLOperator.IN.value, JSQLOperator.NOT_IN.value):
        return convert_in_operator(op, cond_spec)

    # Handle string matching operators (before extracting right_expr)
    string_operators = {
        JSQLOperator.LIKE.value,
        JSQLOperator.NOT_LIKE.value,
        JSQLOperator.ILIKE.value,
        JSQLOperator.NOT_ILIKE.value,
        JSQLOperator.SIMILAR_TO.value,
        JSQLOperator.REGEXP.value,
        JSQLOperator.RLIKE.value,
    }
    if op in string_operators:
        left_expr = _extract_left_expression(cond_spec)
        return convert_string_operator(op, cond_spec, left_expr)

    # Extract left and right expressions for standard comparison operators
    # Check for new format (left/right) first
    if 'left' in cond_spec and 'right' in cond_spec:
        left_expr = jsql_expression_to_sqlglot(cond_spec['left'])
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
    # Fall back to old format (field/value)
    elif 'field' in cond_spec:
        left_expr = jsql_expression_to_sqlglot(cond_spec['field'])
        # Get right side (value or right_field)
        if 'value' in cond_spec:
            right_expr = jsql_expression_to_sqlglot({'value': cond_spec['value']})
        elif 'right_field' in cond_spec:
            right_expr = jsql_expression_to_sqlglot(cond_spec['right_field'])
        else:
            raise MissingFieldError(
                'value or right_field',
                path='',
                context='comparison requires right operand'
            )
    else:
        raise MissingFieldError(
            'left/right or field',
            path='',
            context='comparison operation requires left and right operands'
        )

    # Handle standard comparison operators
    operator_class = COMPARISON_OP_TO_SQLGLOT.get(op)
    if operator_class:
        return convert_standard_comparison(op, cond_spec, left_expr, right_expr)

    # Unknown operator
    supported = list(COMPARISON_OP_TO_SQLGLOT.keys()) + [
        JSQLOperator.IN.value,
        JSQLOperator.NOT_IN.value,
        JSQLOperator.BETWEEN.value,
        JSQLOperator.NOT_BETWEEN.value,
        JSQLOperator.LIKE.value,
        JSQLOperator.NOT_LIKE.value,
        JSQLOperator.ILIKE.value,
        JSQLOperator.NOT_ILIKE.value,
        JSQLOperator.SIMILAR_TO.value,
        JSQLOperator.REGEXP.value,
        JSQLOperator.RLIKE.value,
        JSQLOperator.IS_NULL.value,
        JSQLOperator.IS_NOT_NULL.value,
    ]
    logger.warning(f'Unknown comparison operator: {op}')
    raise UnknownOperatorError(operator=op, path='op', supported=supported)


def _convert_sqlglot_select_to_jsql(select_expr: exp.Select) -> dict[str, Any]:
    """
    Convert sqlglot Select expression to JSQL query dictionary.
    
    Helper function for EXISTS and NOT EXISTS subqueries.
    """
    from namerec.uma.jsql.converter.joins import convert_join_to_jsql
    from namerec.uma.jsql.converter.sql_to_jsql import convert_order_to_jsql
    from namerec.uma.jsql.constants import JSQLField
    
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


def convert_condition_to_jsql(expr: exp.Expression) -> dict[str, Any]:
    """
    Convert sqlglot condition to JSQL condition.

    Raises:
        InvalidExpressionError: If condition structure is invalid
        UnknownOperatorError: If comparison operator is not supported
        UnsupportedOperationError: If condition type is not supported
    """
    # Handle AND/OR
    if isinstance(expr, exp.And):
        return {
            'op': JSQLOperator.AND.value,
            'conditions': [
                convert_condition_to_jsql(expr.left),
                convert_condition_to_jsql(expr.right),
            ],
        }

    if isinstance(expr, exp.Or):
        return {
            'op': JSQLOperator.OR.value,
            'conditions': [
                convert_condition_to_jsql(expr.left),
                convert_condition_to_jsql(expr.right),
            ],
        }

    # Handle ILIKE (case-insensitive LIKE) - this is a real sqlglot class
    if isinstance(expr, exp.ILike):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='ILIKE operator left side must be a field reference',
                path='left',
                expression=left
            )

        if 'value' not in right:
            raise InvalidExpressionError(
                message='ILIKE operator right side must be a string literal',
                path='right',
                expression=right
            )

        return {
            'field': left['field'],
            'op': JSQLOperator.ILIKE.value,
            'pattern': right['value'],
        }

    # Note: NOT ILIKE is handled below as exp.Not(exp.ILike(...))
    # Similar to NOT IN, NOT LIKE, etc.

    if isinstance(expr, exp.SimilarTo):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='SIMILAR TO operator left side must be a field reference',
                path='left',
                expression=left
            )

        if 'value' not in right:
            raise InvalidExpressionError(
                message='SIMILAR TO operator right side must be a string literal',
                path='right',
                expression=right
            )

        return {
            'field': left['field'],
            'op': JSQLOperator.SIMILAR_TO.value,
            'pattern': right['value'],
        }

    if isinstance(expr, exp.RegexpLike):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='REGEXP/RLIKE operator left side must be a field reference',
                path='left',
                expression=left
            )

        if 'value' not in right:
            raise InvalidExpressionError(
                message='REGEXP/RLIKE operator right side must be a string literal',
                path='right',
                expression=right
            )

        # Use REGEXP as default (RLIKE is MySQL alias)
        return {
            'field': left['field'],
            'op': JSQLOperator.REGEXP.value,
            'pattern': right['value'],
        }

    # Note: NOT BETWEEN is handled below as exp.Not(exp.Between(...))
    # Similar to NOT IN, NOT LIKE, etc.

    # Handle EXISTS (explicit, not wrapped)
    # Note: NOT EXISTS is handled below as exp.Not(exp.Exists(...))
    if isinstance(expr, exp.Exists):
        subquery_expr = expr.this
        return {
            'op': JSQLOperator.EXISTS.value,
            'subquery': _convert_sqlglot_select_to_jsql(subquery_expr),
        }

    # Handle NOT - check for wrapped patterns
    if isinstance(expr, exp.Not):
        inner = expr.this
        
        # Handle NOT(IN(...)) pattern - convert to NOT IN
        if isinstance(inner, exp.In):
            left = convert_expression_to_jsql(inner.this)
            if 'field' not in left:
                raise InvalidExpressionError(
                    message='NOT IN operator left side must be a field reference',
                    path='left',
                    expression=left
                )
            
            values = []
            if isinstance(inner.expressions, list):
                for val_expr in inner.expressions:
                    val = convert_expression_to_jsql(val_expr)
                    if 'value' not in val:
                        raise InvalidExpressionError(
                            message='NOT IN operator values must be literals',
                            path='values',
                            expression=val
                        )
                    values.append(val['value'])
            
            return {
                'field': left['field'],
                'op': JSQLOperator.NOT_IN.value,
                'values': values,
            }
        
        # Handle NOT(LIKE(...)) pattern - convert to NOT LIKE
        if isinstance(inner, exp.Like):
            left = convert_expression_to_jsql(inner.this)
            right = convert_expression_to_jsql(inner.expression)
            
            if 'field' not in left:
                raise InvalidExpressionError(
                    message='NOT LIKE operator left side must be a field reference',
                    path='left',
                    expression=left
                )
            
            if 'value' not in right:
                raise InvalidExpressionError(
                    message='NOT LIKE operator right side must be a string literal',
                    path='right',
                    expression=right
                )
            
            return {
                'field': left['field'],
                'op': JSQLOperator.NOT_LIKE.value,
                'pattern': right['value'],
            }
        
        # Handle NOT(ILIKE(...)) pattern - convert to NOT ILIKE
        if isinstance(inner, exp.ILike):
            left = convert_expression_to_jsql(inner.this)
            right = convert_expression_to_jsql(inner.expression)
            
            if 'field' not in left:
                raise InvalidExpressionError(
                    message='NOT ILIKE operator left side must be a field reference',
                    path='left',
                    expression=left
                )
            
            if 'value' not in right:
                raise InvalidExpressionError(
                    message='NOT ILIKE operator right side must be a string literal',
                    path='right',
                    expression=right
                )
            
            return {
                'field': left['field'],
                'op': JSQLOperator.NOT_ILIKE.value,
                'pattern': right['value'],
            }
        
            # Handle NOT(BETWEEN(...)) pattern - convert to NOT BETWEEN
            if isinstance(inner, exp.Between):
                expr_jsql = convert_expression_to_jsql(inner.this)
                low_jsql = convert_expression_to_jsql(inner.args.get('low'))
                high_jsql = convert_expression_to_jsql(inner.args.get('high'))
            
            result = {
                'op': JSQLOperator.NOT_BETWEEN.value,
            }

            # Use 'expr' field (can be field or expression)
            if 'field' in expr_jsql:
                result['expr'] = {'field': expr_jsql['field']}
            elif 'value' in expr_jsql:
                result['expr'] = {'value': expr_jsql['value']}
            else:
                result['expr'] = expr_jsql
            
            if 'value' in low_jsql:
                result['low'] = {'value': low_jsql['value']}
            elif 'field' in low_jsql:
                result['low'] = {'field': low_jsql['field']}
            else:
                result['low'] = low_jsql
            
            if 'value' in high_jsql:
                result['high'] = {'value': high_jsql['value']}
            elif 'field' in high_jsql:
                result['high'] = {'field': high_jsql['field']}
            else:
                result['high'] = high_jsql
            
            return result
        
        # Handle NOT(EXISTS(...)) pattern - convert to NOT EXISTS
        if isinstance(inner, exp.Exists):
            # Extract subquery from EXISTS
            # Note: sqlglot's Exists has 'this' containing the subquery Select
            subquery_expr = inner.this
            # Convert sqlglot Select back to JSQL - this is complex, 
            # but we'll handle it similar to how we handle regular queries
            # For now, return NOT EXISTS with nested structure
            return {
                'op': JSQLOperator.NOT_EXISTS.value,
                'subquery': _convert_sqlglot_select_to_jsql(subquery_expr),
            }
        
        # Generic NOT - fall back to NOT wrapper
        return {
            'op': JSQLOperator.NOT.value,
            'condition': convert_condition_to_jsql(expr.this),
        }

    # Handle comparison operators (use reverse mapping)
    if isinstance(expr, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE)):
        op = SQLGLOT_TO_COMPARISON.get(type(expr))
        if not op:
            raise UnknownOperatorError(
                operator=type(expr).__name__,
                path='',
                supported=list(SQLGLOT_TO_COMPARISON.values())
            )

        left = convert_expression_to_jsql(expr.left)
        right = convert_expression_to_jsql(expr.right)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='Comparison operator left side must be a field reference',
                path='left',
                expression=left
            )

        result: dict[str, Any] = {
            'field': left['field'],
            'op': op,
        }

        if 'value' in right:
            result['value'] = right['value']
        elif 'field' in right:
            result['right_field'] = right['field']
        elif 'from' in right and 'select' in right:
            # Right side is a subquery
            result['right'] = right
        else:
            raise InvalidExpressionError(
                message='Comparison operator right side must be a value, field reference, or subquery',
                path='right',
                expression=right
            )

        return result

    # Handle IN
    if isinstance(expr, exp.In):
        left = convert_expression_to_jsql(expr.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message='IN operator left side must be a field reference',
                path='left',
                expression=left
            )

        # Check if IN has a subquery (stored in args['query'])
        if expr.args.get('query'):
            query_expr = expr.args['query']
            if isinstance(query_expr, exp.Subquery):
                # IN with subquery
                from namerec.uma.jsql.converter.conditions import _convert_sqlglot_select_to_jsql
                # query_expr.this might be another Subquery (nested) or Select
                select_expr = query_expr.this
                if isinstance(select_expr, exp.Subquery):
                    # Unwrap nested subquery
                    select_expr = select_expr.this
                if isinstance(select_expr, exp.Select):
                    subquery_jsql = _convert_sqlglot_select_to_jsql(select_expr)
                    return {
                        'field': left['field'],
                        'op': JSQLOperator.IN.value,
                        'right': subquery_jsql,
                    }
        
        # Also check expressions list for subquery (fallback)
        if isinstance(expr.expressions, list) and len(expr.expressions) == 1:
            first_expr = expr.expressions[0]
            if isinstance(first_expr, exp.Subquery):
                # IN with subquery
                from namerec.uma.jsql.converter.conditions import _convert_sqlglot_select_to_jsql
                subquery_jsql = _convert_sqlglot_select_to_jsql(first_expr.this)
                return {
                    'field': left['field'],
                    'op': JSQLOperator.IN.value,
                    'right': subquery_jsql,
                }

        # IN with value list
        values = []
        if isinstance(expr.expressions, list):
            for val_expr in expr.expressions:
                val = convert_expression_to_jsql(val_expr)
                if 'value' not in val:
                    raise InvalidExpressionError(
                        message='IN operator values must be literals',
                        path='values',
                        expression=val
                    )
                values.append(val['value'])

        return {
            'field': left['field'],
            'op': JSQLOperator.IN.value,
            'values': values,
        }

    # Handle BETWEEN
    if isinstance(expr, exp.Between):
        expr_jsql = convert_expression_to_jsql(expr.this)
        low_jsql = convert_expression_to_jsql(expr.args.get('low'))
        high_jsql = convert_expression_to_jsql(expr.args.get('high'))

        result = {
            'op': JSQLOperator.BETWEEN.value,
        }

        # Use 'expr' field (can be field or expression)
        if 'field' in expr_jsql:
            result['expr'] = {'field': expr_jsql['field']}
        elif 'value' in expr_jsql:
            result['expr'] = {'value': expr_jsql['value']}
        else:
            result['expr'] = expr_jsql

        if 'value' in low_jsql:
            result['low'] = {'value': low_jsql['value']}
        elif 'field' in low_jsql:
            result['low'] = {'field': low_jsql['field']}
        else:
            result['low'] = low_jsql

        if 'value' in high_jsql:
            result['high'] = {'value': high_jsql['value']}
        elif 'field' in high_jsql:
            result['high'] = {'field': high_jsql['field']}
        else:
            result['high'] = high_jsql

        return result

    # Handle LIKE
    if isinstance(expr, exp.Like):
        left = convert_expression_to_jsql(expr.this)
        right = convert_expression_to_jsql(expr.expression)

        if 'field' not in left:
            raise InvalidExpressionError(
                message='LIKE operator left side must be a field reference',
                path='left',
                expression=left
            )

        if 'value' not in right:
            raise InvalidExpressionError(
                message='LIKE operator right side must be a string literal',
                path='right',
                expression=right
            )

        return {
            'field': left['field'],
            'op': JSQLOperator.LIKE.value,
            'pattern': right['value'],
        }

    # Handle IS NULL / IS NOT NULL
    if isinstance(expr, exp.Is):
        left = convert_expression_to_jsql(expr.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message='IS NULL operator left side must be a field reference',
                path='left',
                expression=left
            )

        if isinstance(expr.expression, exp.Null):
            return {
                'field': left['field'],
                'op': JSQLOperator.IS_NULL.value,
            }

        # IS NOT NULL is handled as Not(Is(...)) in sqlglot
        raise InvalidExpressionError(
            message='IS expression must be IS NULL',
            path='expression',
            expression={'type': type(expr.expression).__name__}
        )

    # Default: unsupported condition type
    raise UnsupportedOperationError(
        operation=f'Condition type: {type(expr).__name__}',
        reason='This condition type is not supported in JSQL conversion',
        path=''
    )
