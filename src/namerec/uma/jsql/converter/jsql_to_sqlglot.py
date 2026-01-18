"""JSQL to SQLGlot conversion helpers and entry points."""

import logging
from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.converter.operators import ARITHMETIC_OP_TO_SQLGLOT

logger = logging.getLogger(__name__)


def convert_field_reference(field: str) -> exp.Expression:
    """Convert field reference to sqlglot column expression."""
    if field == '*':
        return exp.Star()
    if '.' in field:
        table, column = field.split('.', 1)
        return exp.column(column, table=table)
    return exp.column(field)


def convert_literal_value(value: Any) -> exp.Expression:
    """Convert literal value to sqlglot expression."""
    match value:
        case bool():
            return exp.Boolean(this=value)
        case int() | float():
            return exp.Literal.number(value)
        case _:
            return exp.Literal.string(str(value))


def convert_function_call(expr_spec: dict[str, Any]) -> exp.Expression:
    """Convert function call to sqlglot expression."""
    func = expr_spec['func']
    logger.debug(f'Converting function call: {func}')

    args = [
        converted
        for arg in expr_spec.get('args', [])
        if (converted := jsql_expression_to_sqlglot(arg)) is not None
    ]
    logger.debug(f'Function {func} has {len(args)} arguments')

    distinct = expr_spec.get('distinct', False)
    if distinct and args:
        args[0] = exp.Distinct(expressions=[args[0]])

    func_class = getattr(exp, func.upper(), None)
    if func_class and issubclass(func_class, exp.Func):
        logger.debug(f'Using specific sqlglot function: {func_class.__name__}')
        if distinct and args:
            return func_class(this=args[0], expressions=args[1:])
        return func_class(*args)

    logger.debug(f'Using Anonymous function for: {func}')
    return exp.Anonymous(this=func, expressions=args)


def convert_arithmetic_op(expr_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert arithmetic operation to sqlglot expression.
    
    Raises:
        MissingFieldError: If left or right operand is missing
        UnknownOperatorError: If operator is not supported
    """
    op = expr_spec.get('op')
    if not op:
        raise MissingFieldError('op', path='op', context='arithmetic operation')

    if 'left' not in expr_spec:
        raise MissingFieldError('left', path='left', context='arithmetic operation')
    if 'right' not in expr_spec:
        raise MissingFieldError('right', path='right', context='arithmetic operation')

    left = jsql_expression_to_sqlglot(expr_spec['left'])
    right = jsql_expression_to_sqlglot(expr_spec['right'])

    if op == JSQLOperator.CONCAT.value:
        return exp.Concat(expressions=[left, right])

    operator_class = ARITHMETIC_OP_TO_SQLGLOT.get(op)
    if not operator_class:
        raise UnknownOperatorError(
            operator=op,
            path='op',
            supported=list(ARITHMETIC_OP_TO_SQLGLOT.keys()) + [JSQLOperator.CONCAT.value],
        )

    return operator_class(this=left, expression=right)


def jsql_expression_to_sqlglot(expr_spec: dict[str, Any] | str) -> exp.Expression:
    """
    Convert JSQL expression to sqlglot expression.
    
    Delegates to specialized functions based on expression type.
    
    Raises:
        InvalidExpressionError: If expression structure is invalid or unrecognized
    """
    if isinstance(expr_spec, str):
        return convert_field_reference(expr_spec)

    if isinstance(expr_spec, dict):
        if 'field' in expr_spec:
            return convert_field_reference(expr_spec['field'])
        if 'value' in expr_spec:
            return convert_literal_value(expr_spec['value'])
        if 'func' in expr_spec:
            return convert_function_call(expr_spec)
        if 'op' in expr_spec:
            return convert_arithmetic_op(expr_spec)
        if 'from' in expr_spec and 'select' in expr_spec:
            subquery_select = jsql_query_to_sqlglot_select(expr_spec)
            return exp.Subquery(this=subquery_select)

        raise InvalidExpressionError(
            message="Expression must contain one of: 'field', 'value', 'func', 'op', or subquery ('from')",
            path='',
            expression=expr_spec,
        )

    raise InvalidExpressionError(
        message=f'Expression must be string or dict, got {type(expr_spec).__name__}',
        path='',
        expression={'type': type(expr_spec).__name__, 'value': repr(expr_spec)},
    )


def extract_left_expression(cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Extract left expression from condition spec.
    
    Supports both old format ('field') and new format ('left').
    """
    if 'left' in cond_spec:
        return jsql_expression_to_sqlglot(cond_spec['left'])
    if 'expr' in cond_spec:
        return jsql_expression_to_sqlglot(cond_spec['expr'])
    if 'field' in cond_spec:
        return jsql_expression_to_sqlglot(cond_spec['field'])
    raise MissingFieldError('left or field', path='left/field', context='condition')


def extract_pattern(cond_spec: dict[str, Any]) -> str | None:
    """
    Extract pattern from condition spec for string operators.
    """
    if 'pattern' in cond_spec:
        return cond_spec['pattern']
    
    if 'right' in cond_spec:
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
        if isinstance(right_expr, exp.Literal) and right_expr.is_string:
            return right_expr.this
    
    return None


def jsql_join_to_sqlglot(join_spec: dict[str, Any]) -> dict[str, Any]:
    """
    Convert JSQL JOIN to sqlglot join components.
    
    Raises:
        InvalidExpressionError: If JOIN structure is invalid
        MissingFieldError: If required entity field is missing
    """
    if not isinstance(join_spec, dict):
        logger.debug('Invalid join_spec type, expected dict')
        raise InvalidExpressionError(
            message=f'JOIN spec must be dict, got {type(join_spec).__name__}',
            path='joins',
            expression={'type': type(join_spec).__name__, 'value': repr(join_spec)},
        )

    if 'entity' not in join_spec:
        logger.debug("Missing 'entity' in join_spec")
        raise MissingFieldError('entity', path='joins.entity', context='JOIN')

    entity = join_spec['entity']
    join_type = join_spec.get('type', JoinType.INNER.value).upper()
    logger.debug(f'Processing {join_type} JOIN to {entity}')

    table = exp.table_(entity)
    if alias := join_spec.get('alias'):
        table = exp.alias_(table, alias, table=True)

    on_cond = None
    if on_spec := join_spec.get('on'):
        on_cond = jsql_condition_to_sqlglot(on_spec)

    logger.debug(f'Built JOIN: {join_type} {entity}')
    return {
        'table': table,
        'on': on_cond,
        'type': join_type,
    }


LOGICAL_OP_TO_SQLGLOT = {
    JSQLOperator.AND.value: exp.And,
    JSQLOperator.OR.value: exp.Or,
}

STRING_OP_TO_SQLGLOT: dict[str, tuple[type[exp.Expression], bool]] = {
    JSQLOperator.LIKE.value: (exp.Like, False),
    JSQLOperator.NOT_LIKE.value: (exp.Like, True),
    JSQLOperator.ILIKE.value: (exp.ILike, False),
    JSQLOperator.NOT_ILIKE.value: (exp.ILike, True),
    JSQLOperator.SIMILAR_TO.value: (exp.SimilarTo, False),
    JSQLOperator.REGEXP.value: (exp.RegexpLike, False),
    JSQLOperator.RLIKE.value: (exp.RegexpLike, False),
}

_COMPARISON_OPERATOR_HANDLERS: dict[str, Any] = {}


def convert_between(cond_spec: dict[str, Any], negate: bool = False) -> exp.Expression:
    """Convert BETWEEN/NOT BETWEEN operator to sqlglot expression."""
    if 'expr' not in cond_spec and 'left' not in cond_spec and 'field' not in cond_spec:
        raise MissingFieldError('expr', path='expr', context='BETWEEN operator')
    if 'low' not in cond_spec:
        raise MissingFieldError('low', path='low', context='BETWEEN operator')
    if 'high' not in cond_spec:
        raise MissingFieldError('high', path='high', context='BETWEEN operator')

    expr = extract_left_expression(cond_spec)
    low = jsql_expression_to_sqlglot(cond_spec['low'])
    high = jsql_expression_to_sqlglot(cond_spec['high'])

    between_expr = exp.Between(this=expr, low=low, high=high)
    return exp.Not(this=between_expr) if negate else between_expr


def convert_in_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert IN/NOT IN operator to sqlglot expression."""
    left_expr = extract_left_expression(cond_spec)
    negate = op == JSQLOperator.NOT_IN.value

    if 'values' in cond_spec:
        values = cond_spec['values']
    elif 'right' in cond_spec:
        right_spec = cond_spec['right']
        if isinstance(right_spec, dict) and 'values' in right_spec:
            values = right_spec['values']
        elif isinstance(right_spec, list):
            values = right_spec
        else:
            right_expr = jsql_expression_to_sqlglot(right_spec)
            in_expr = exp.In(this=left_expr, expressions=[right_expr])
            return exp.NotIn(this=left_expr, expressions=[right_expr]) if negate else in_expr
    else:
        raise MissingFieldError('values or right', path='values/right', context=f'{op} operator')

    val_exprs = [jsql_expression_to_sqlglot({'value': v}) for v in values]
    in_expr = exp.In(this=left_expr, expressions=val_exprs)
    return exp.Not(this=in_expr) if negate else in_expr


def convert_is_null(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert IS NULL operator to sqlglot expression."""
    left = extract_left_expression(cond_spec)
    return exp.Is(this=left, expression=exp.Null())


def convert_is_not_null(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert IS NOT NULL operator to sqlglot expression."""
    left = extract_left_expression(cond_spec)
    return exp.Not(this=exp.Is(this=left, expression=exp.Null()))


def convert_string_operator(
    op: str,
    cond_spec: dict[str, Any],
    left_expr: exp.Expression,
) -> exp.Expression:
    """Convert string operators (LIKE, ILIKE, SIMILAR TO, REGEXP) to sqlglot."""
    op_class, negate = STRING_OP_TO_SQLGLOT[op]

    pattern = extract_pattern(cond_spec)
    if pattern is not None:
        right_expr = exp.Literal.string(pattern)
    else:
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])

    expr = op_class(this=left_expr, expression=right_expr)
    if negate:
        return exp.Not(this=expr)
    return expr


def convert_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert EXISTS operator to sqlglot expression."""
    if 'subquery' not in cond_spec:
        raise MissingFieldError('subquery', path='subquery', context='EXISTS operator')
    subquery_spec = cond_spec['subquery']
    subquery = jsql_query_to_sqlglot_select(subquery_spec)
    return exp.Exists(this=subquery)


def convert_not_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert NOT EXISTS operator to sqlglot expression."""
    if 'subquery' not in cond_spec:
        raise MissingFieldError('subquery', path='subquery', context='NOT EXISTS operator')
    subquery_spec = cond_spec['subquery']
    subquery = jsql_query_to_sqlglot_select(subquery_spec)
    return exp.NotExists(this=subquery)


def _extract_right_expression(cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Extract right expression from condition spec, supporting multiple formats.
    """
    if 'right' in cond_spec:
        return jsql_expression_to_sqlglot(cond_spec['right'])
    if 'value' in cond_spec:
        return jsql_expression_to_sqlglot({'value': cond_spec['value']})
    if 'right_field' in cond_spec:
        return jsql_expression_to_sqlglot({'field': cond_spec['right_field']})
    
    raise MissingFieldError(
        'value, right_field, or right',
        path='right',
        context='comparison requires right operand',
    )


def _make_string_handler(op: str):
    """Create a handler function for a string operator."""
    def handler(spec: dict[str, Any]) -> exp.Expression:
        return convert_string_operator(op, spec, extract_left_expression(spec))
    return handler


def _init_comparison_handlers() -> None:
    """Initialize comparison operator handler registry."""
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.IS_NULL.value] = convert_is_null
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.IS_NOT_NULL.value] = convert_is_not_null
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.BETWEEN.value] = lambda spec: convert_between(spec, negate=False)
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.NOT_BETWEEN.value] = lambda spec: convert_between(spec, negate=True)
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.IN.value] = lambda spec: convert_in_operator(JSQLOperator.IN.value, spec)
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.NOT_IN.value] = lambda spec: convert_in_operator(JSQLOperator.NOT_IN.value, spec)
    
    string_operators = {
        JSQLOperator.LIKE.value,
        JSQLOperator.NOT_LIKE.value,
        JSQLOperator.ILIKE.value,
        JSQLOperator.NOT_ILIKE.value,
        JSQLOperator.SIMILAR_TO.value,
        JSQLOperator.REGEXP.value,
        JSQLOperator.RLIKE.value,
    }
    
    for op in string_operators:
        _COMPARISON_OPERATOR_HANDLERS[op] = _make_string_handler(op)


_init_comparison_handlers()


def convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert comparison operator to sqlglot expression."""
    handler = _COMPARISON_OPERATOR_HANDLERS.get(op)
    if handler:
        return handler(cond_spec)

    left = extract_left_expression(cond_spec)
    right = _extract_right_expression(cond_spec)

    if op == JSQLOperator.EQ.value:
        return exp.EQ(this=left, expression=right)
    if op in (JSQLOperator.NE.value, JSQLOperator.NEQ_ISO.value):
        return exp.NEQ(this=left, expression=right)
    if op == JSQLOperator.GT.value:
        return exp.GT(this=left, expression=right)
    if op == JSQLOperator.GE.value:
        return exp.GTE(this=left, expression=right)
    if op == JSQLOperator.LT.value:
        return exp.LT(this=left, expression=right)
    if op == JSQLOperator.LE.value:
        return exp.LTE(this=left, expression=right)

    raise UnknownOperatorError(
        operator=op,
        path='op',
        supported=[
            JSQLOperator.EQ.value,
            JSQLOperator.NE.value,
            JSQLOperator.NEQ_ISO.value,
            JSQLOperator.GT.value,
            JSQLOperator.GE.value,
            JSQLOperator.LT.value,
            JSQLOperator.LE.value,
        ],
    )


def convert_logical_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert logical operator (AND/OR/NOT) to sqlglot expression."""
    if op in (JSQLOperator.AND.value, JSQLOperator.OR.value):
        conditions = cond_spec['conditions']
        logger.debug(f'Processing {len(conditions)} conditions for {op}')

        if not conditions:
            raise InvalidExpressionError(
                message=f'{op} operator requires at least one condition',
                path='conditions',
            )

        result = jsql_condition_to_sqlglot(conditions[0])
        operator_class = LOGICAL_OP_TO_SQLGLOT.get(op)
        if not operator_class:
            raise UnknownOperatorError(
                operator=op,
                path='op',
                supported=list(LOGICAL_OP_TO_SQLGLOT.keys()),
            )

        for cond in conditions[1:]:
            next_cond = jsql_condition_to_sqlglot(cond)
            result = operator_class(this=result, expression=next_cond)

        logger.debug(f'Built {op} expression with {len(conditions)} conditions')
        return result

    if op == JSQLOperator.NOT.value:
        logger.debug('Processing NOT condition')
        if 'condition' not in cond_spec:
            raise MissingFieldError('condition', path='condition', context='NOT operator')

        condition = jsql_condition_to_sqlglot(cond_spec['condition'])
        return exp.Not(this=condition)

    logger.warning(f'Unknown logical operator: {op}')
    raise UnknownOperatorError(
        operator=op,
        path='op',
        supported=[JSQLOperator.AND.value, JSQLOperator.OR.value, JSQLOperator.NOT.value],
    )


def jsql_condition_to_sqlglot(cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert JSQL condition to SQLGlot expression.
    
    Raises:
        InvalidExpressionError: If condition structure is invalid
        MissingFieldError: If required fields are missing
        UnknownOperatorError: If operator is not supported
    """
    if 'op' not in cond_spec:
        raise MissingFieldError('op', path='op', context='condition')

    op = cond_spec['op'].upper()

    if op == JSQLOperator.EXISTS.value:
        return convert_exists_operator(cond_spec)
    if op == JSQLOperator.NOT_EXISTS.value:
        return convert_not_exists_operator(cond_spec)

    if op in (JSQLOperator.AND.value, JSQLOperator.OR.value, JSQLOperator.NOT.value):
        return convert_logical_operator(op, cond_spec)

    return convert_comparison_operator(op, cond_spec)


def jsql_select_to_sqlglot(select_spec: list[dict[str, Any]] | None) -> list[exp.Expression]:
    """Convert JSQL SELECT to sqlglot expressions."""
    if not select_spec:
        return [exp.Star()]

    result = []
    for field_spec in select_spec:
        if isinstance(field_spec, dict) and 'over' in field_spec:
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
            
            partition = None
            order = None
            if partition_by:
                partition = exp.Partition(expressions=partition_by)
            if order_by_exprs:
                order = exp.Order(expressions=order_by_exprs)
            
            field_expr = exp.Window(
                this=field_expr,
                partition_by=partition,
                order=order,
            )
        else:
            field_expr = jsql_expression_to_sqlglot(field_spec)
        
        if field_expr:
            alias = field_spec.get('alias') if isinstance(field_spec, dict) else None
            if alias:
                field_expr = exp.alias_(field_expr, alias)
            result.append(field_expr)
    
    return result if result else [exp.Star()]


def jsql_from_to_sqlglot(from_spec: str | dict[str, Any] | None) -> exp.Table:
    """Convert JSQL FROM to sqlglot table."""
    if not from_spec:
        raise MissingFieldError('from', path='from', context='SELECT query requires FROM clause')
    
    if isinstance(from_spec, str):
        return exp.table_(from_spec)
    
    if isinstance(from_spec, dict):
        if 'entity' not in from_spec:
            raise MissingFieldError('entity', path='from.entity', context='FROM with alias')
        entity = from_spec['entity']
        alias = from_spec.get('alias')
        table_expr = exp.table_(entity)
        if alias:
            table_expr = exp.alias_(table_expr, alias, table=True)
        return table_expr
    
    raise InvalidExpressionError(
        message=f'FROM must be string or dict, got {type(from_spec).__name__}',
        path='from',
        expression={'type': type(from_spec).__name__, 'value': repr(from_spec)},
    )


def jsql_query_to_sqlglot(jsql: dict[str, Any]) -> exp.Select:
    """
    Convert JSQL query dictionary to sqlglot Select expression.
    
    This is the core conversion function that builds the SQL query structure.
    """
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
    
    select_exprs = jsql_select_to_sqlglot(jsql.get('select', []))

    from_expr = jsql_from_to_sqlglot(jsql.get('from'))

    if ctes:
        with_clause = exp.With(expressions=ctes)
        query = exp.Select(expressions=select_exprs, with_=with_clause)
    else:
        query = exp.Select(expressions=select_exprs)
    query = query.from_(from_expr)

    if where_spec := jsql.get('where'):
        where_expr = jsql_condition_to_sqlglot(where_spec)
        query = query.where(where_expr)

    if joins := jsql.get('joins'):
        for join_spec in joins:
            join_expr = jsql_join_to_sqlglot(join_spec)
            query = query.join(
                join_expr['table'],
                on=join_expr.get('on'),
                join_type=join_expr.get('type', JoinType.INNER.value),
            )

    if group_by := jsql.get('group_by'):
        for group_expr_spec in group_by:
            group_expr = jsql_expression_to_sqlglot(group_expr_spec)
            if group_expr:
                query = query.group_by(group_expr)

    if having_spec := jsql.get('having'):
        having_expr = jsql_condition_to_sqlglot(having_spec)
        query.args['having'] = exp.Having(this=having_expr)

    if order_by := jsql.get('order_by'):
        order_exprs = []
        for order_spec in order_by:
            if isinstance(order_spec, dict):
                order_expr = jsql_expression_to_sqlglot(order_spec.get('field') or order_spec)
                direction = order_spec.get('direction', OrderDirection.ASC.value).upper()
            else:
                order_expr = jsql_expression_to_sqlglot(order_spec)
                direction = OrderDirection.ASC.value.upper()

            if order_expr:
                desc = direction == OrderDirection.DESC.value.upper()
                if desc:
                    ordered_expr = exp.Ordered(this=order_expr, desc=True)
                    order_exprs.append(ordered_expr)
                else:
                    order_exprs.append(order_expr)
        
        if order_exprs:
            query = query.order_by(*order_exprs)

    if limit := jsql.get('limit'):
        query.args['limit'] = exp.Limit(expression=exp.Literal.number(limit))

    if offset := jsql.get('offset'):
        query.args['offset'] = exp.Offset(expression=exp.Literal.number(offset))

    return query


def jsql_query_to_sqlglot_select(query_spec: dict[str, Any]) -> exp.Select:
    """
    Convert JSQL query to sqlglot Select expression.
    
    Helper function for EXISTS and NOT EXISTS subqueries.
    """
    return jsql_query_to_sqlglot(query_spec)
