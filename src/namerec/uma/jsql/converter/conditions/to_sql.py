"""Convert JSQL conditions to SQLGlot expressions."""

from collections.abc import Callable
from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.converter.conditions.helpers import extract_left_expression
from namerec.uma.jsql.converter.conditions.helpers import extract_pattern

LOGICAL_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
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

_COMPARISON_OPERATOR_HANDLERS: dict[str, Callable[[dict[str, Any]], exp.Expression]] = {}


def _convert_expression(expr_spec: dict[str, Any] | str) -> exp.Expression:
    """Resolve circular dependency lazily."""
    from namerec.uma.jsql.converter.jsql_to_sqlglot import jsql_expression_to_sqlglot  # noqa: PLC0415

    return jsql_expression_to_sqlglot(expr_spec)


def _convert_subquery(subquery_spec: dict[str, Any]) -> exp.Select:
    """Resolve circular dependency lazily."""
    from namerec.uma.jsql.converter.jsql_to_sqlglot import jsql_query_to_sqlglot_select  # noqa: PLC0415

    return jsql_query_to_sqlglot_select(subquery_spec)


def _extract_right_expression(cond_spec: dict[str, Any]) -> exp.Expression:
    """Extract right expression from condition spec in supported formats."""
    if 'right' in cond_spec:
        return _convert_expression(cond_spec['right'])
    if 'value' in cond_spec:
        return _convert_expression({'value': cond_spec['value']})
    if 'right_field' in cond_spec:
        return _convert_expression({'field': cond_spec['right_field']})
    raise MissingFieldError(  # noqa: TRY003
        'value, right_field, or right',
        path='right',
        context='comparison requires right operand',
    )


def _convert_between(cond_spec: dict[str, Any], negate: bool = False) -> exp.Expression:
    if 'expr' not in cond_spec and 'left' not in cond_spec and 'field' not in cond_spec:
        raise MissingFieldError('expr', path='expr', context='BETWEEN operator')
    if 'low' not in cond_spec:
        raise MissingFieldError('low', path='low', context='BETWEEN operator')
    if 'high' not in cond_spec:
        raise MissingFieldError('high', path='high', context='BETWEEN operator')

    expr_value = extract_left_expression(cond_spec, _convert_expression)
    low = _convert_expression(cond_spec['low'])
    high = _convert_expression(cond_spec['high'])
    between_expr = exp.Between(this=expr_value, low=low, high=high)
    return exp.Not(this=between_expr) if negate else between_expr


def _convert_in_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    left_expr = extract_left_expression(cond_spec, _convert_expression)
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
            right_expr = _convert_expression(right_spec)
            in_expr = exp.In(this=left_expr, expressions=[right_expr])
            return exp.NotIn(this=left_expr, expressions=[right_expr]) if negate else in_expr
    else:
        raise MissingFieldError('values or right', path='values/right', context=f'{op} operator')  # noqa: TRY003

    val_exprs = [_convert_expression({'value': value}) for value in values]
    in_expr = exp.In(this=left_expr, expressions=val_exprs)
    return exp.Not(this=in_expr) if negate else in_expr


def _convert_is_null(cond_spec: dict[str, Any]) -> exp.Expression:
    left = extract_left_expression(cond_spec, _convert_expression)
    return exp.Is(this=left, expression=exp.Null())


def _convert_is_not_null(cond_spec: dict[str, Any]) -> exp.Expression:
    left = extract_left_expression(cond_spec, _convert_expression)
    return exp.Not(this=exp.Is(this=left, expression=exp.Null()))


def _convert_string_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    op_class, negate = STRING_OP_TO_SQLGLOT[op]
    left_expr = extract_left_expression(cond_spec, _convert_expression)
    pattern = extract_pattern(cond_spec, _convert_expression)
    right_expr = exp.Literal.string(pattern) if pattern is not None else _convert_expression(cond_spec['right'])
    expr = op_class(this=left_expr, expression=right_expr)
    return exp.Not(this=expr) if negate else expr


def _convert_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    if 'subquery' not in cond_spec:
        raise MissingFieldError('subquery', path='subquery', context='EXISTS operator')
    return exp.Exists(this=_convert_subquery(cond_spec['subquery']))


def _convert_not_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    if 'subquery' not in cond_spec:
        raise MissingFieldError('subquery', path='subquery', context='NOT EXISTS operator')
    return exp.NotExists(this=_convert_subquery(cond_spec['subquery']))


def _make_string_handler(op: str) -> Callable[[dict[str, Any]], exp.Expression]:
    def handler(spec: dict[str, Any]) -> exp.Expression:
        return _convert_string_operator(op, spec)

    return handler


def _init_comparison_handlers() -> None:
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.IS_NULL.value] = _convert_is_null
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.IS_NOT_NULL.value] = _convert_is_not_null
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.BETWEEN.value] = lambda spec: _convert_between(spec, negate=False)
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.NOT_BETWEEN.value] = lambda spec: _convert_between(spec, negate=True)
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.IN.value] = lambda spec: _convert_in_operator(
        JSQLOperator.IN.value,
        spec,
    )
    _COMPARISON_OPERATOR_HANDLERS[JSQLOperator.NOT_IN.value] = lambda spec: _convert_in_operator(
        JSQLOperator.NOT_IN.value,
        spec,
    )

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
    """Convert comparison operator condition."""
    handler = _COMPARISON_OPERATOR_HANDLERS.get(op)
    if handler:
        return handler(cond_spec)

    left = extract_left_expression(cond_spec, _convert_expression)
    right = _extract_right_expression(cond_spec)

    comparison_builders: dict[str, type[exp.Expression]] = {
        JSQLOperator.EQ.value: exp.EQ,
        JSQLOperator.NE.value: exp.NEQ,
        JSQLOperator.NEQ_ISO.value: exp.NEQ,
        JSQLOperator.GT.value: exp.GT,
        JSQLOperator.GE.value: exp.GTE,
        JSQLOperator.LT.value: exp.LT,
        JSQLOperator.LE.value: exp.LTE,
    }
    operator_class = comparison_builders.get(op)
    if operator_class is not None:
        return operator_class(this=left, expression=right)

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
    """Convert logical operator (AND/OR/NOT)."""
    if op in (JSQLOperator.AND.value, JSQLOperator.OR.value):
        conditions = cond_spec['conditions']
        if not conditions:
            raise InvalidExpressionError(
                message=f'{op} operator requires at least one condition',
                path='conditions',
            )
        result = jsql_condition_to_sqlglot(conditions[0])
        operator_class = LOGICAL_OP_TO_SQLGLOT.get(op)
        if operator_class is None:
            raise UnknownOperatorError(
                operator=op,
                path='op',
                supported=list(LOGICAL_OP_TO_SQLGLOT.keys()),
            )
        for condition in conditions[1:]:
            result = operator_class(this=result, expression=jsql_condition_to_sqlglot(condition))
        return result

    if op == JSQLOperator.NOT.value:
        if 'condition' not in cond_spec:
            raise MissingFieldError('condition', path='condition', context='NOT operator')
        return exp.Not(this=jsql_condition_to_sqlglot(cond_spec['condition']))

    raise UnknownOperatorError(
        operator=op,
        path='op',
        supported=[JSQLOperator.AND.value, JSQLOperator.OR.value, JSQLOperator.NOT.value],
    )


def jsql_condition_to_sqlglot(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert JSQL condition to SQLGlot expression."""
    if 'op' not in cond_spec:
        raise MissingFieldError('op', path='op', context='condition')

    op = cond_spec['op'].upper()
    if op == JSQLOperator.EXISTS.value:
        return _convert_exists_operator(cond_spec)
    if op == JSQLOperator.NOT_EXISTS.value:
        return _convert_not_exists_operator(cond_spec)
    if op in (JSQLOperator.AND.value, JSQLOperator.OR.value, JSQLOperator.NOT.value):
        return convert_logical_operator(op, cond_spec)
    return convert_comparison_operator(op, cond_spec)


__all__ = [
    '_COMPARISON_OPERATOR_HANDLERS',
    'convert_comparison_operator',
    'convert_exists_operator',
    'convert_logical_operator',
    'convert_not_exists_operator',
    'jsql_condition_to_sqlglot',
]


def convert_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    """Public wrapper for EXISTS conversion."""
    return _convert_exists_operator(cond_spec)


def convert_not_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    """Public wrapper for NOT EXISTS conversion."""
    return _convert_not_exists_operator(cond_spec)
