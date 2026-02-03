"""Convert SQLGlot expressions to JSQL conditions."""

from collections.abc import Callable
from typing import Any
from typing import cast

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.conversion_exceptions import UnsupportedOperationError
from namerec.uma.jsql.converter.conditions.helpers import extract_in_operator_values
from namerec.uma.jsql.converter.conditions.helpers import extract_lower_like_pattern
from namerec.uma.jsql.converter.conditions.helpers import normalize_between_jsql_fields


def _convert_expression(expression: exp.Expression) -> dict[str, Any]:
    """Resolve circular dependency lazily."""
    from namerec.uma.jsql.converter.sqlglot_to_jsql import convert_expression_to_jsql  # noqa: PLC0415

    return cast('dict[str, Any]', convert_expression_to_jsql(expression))


def _convert_select(select_expr: exp.Select) -> dict[str, Any]:
    """Resolve circular dependency lazily."""
    from namerec.uma.jsql.converter.sqlglot_to_jsql import convert_sqlglot_select_to_jsql  # noqa: PLC0415

    return cast('dict[str, Any]', convert_sqlglot_select_to_jsql(select_expr))


def _convert_and(expr: exp.And) -> dict[str, Any]:
    return {
        'op': JSQLOperator.AND.value,
        'conditions': [
            convert_condition_to_jsql(expr.left),
            convert_condition_to_jsql(expr.right),
        ],
    }


def _convert_or(expr: exp.Or) -> dict[str, Any]:
    return {
        'op': JSQLOperator.OR.value,
        'conditions': [
            convert_condition_to_jsql(expr.left),
            convert_condition_to_jsql(expr.right),
        ],
    }


def _convert_not_in(inner: exp.In) -> dict[str, Any]:
    left = _convert_expression(inner.this)

    if inner.args.get('query'):
        query_expr = inner.args['query']
        if isinstance(query_expr, exp.Subquery):
            select_expr = query_expr.this
            if isinstance(select_expr, exp.Subquery):
                select_expr = select_expr.this
            if isinstance(select_expr, exp.Select):
                return {
                    'op': JSQLOperator.NOT_IN.value,
                    'left': left,
                    'right': _convert_select(select_expr),
                }

    values = extract_in_operator_values(inner.expressions, 'NOT IN', _convert_expression)
    return {
        'op': JSQLOperator.NOT_IN.value,
        'left': left,
        'right': {'values': values},
    }


def _convert_not_like(inner: exp.Like) -> dict[str, Any]:
    lower_result = extract_lower_like_pattern(inner, _convert_expression)
    if lower_result is not None:
        left, right = lower_result
        return {
            'op': JSQLOperator.NOT_ILIKE.value,
            'left': left,
            'right': right,
        }

    return {
        'op': JSQLOperator.NOT_LIKE.value,
        'left': _convert_expression(inner.this),
        'right': _convert_expression(inner.expression),
    }


def _convert_not_ilike(inner: exp.ILike) -> dict[str, Any]:
    return {
        'op': JSQLOperator.NOT_ILIKE.value,
        'left': _convert_expression(inner.this),
        'right': _convert_expression(inner.expression),
    }


def _convert_not_between(inner: exp.Between) -> dict[str, Any]:
    result = cast(
        'dict[str, Any]',
        normalize_between_jsql_fields(
        _convert_expression(inner.this),
        _convert_expression(inner.args.get('low')),
        _convert_expression(inner.args.get('high')),
        ),
    )
    result['op'] = JSQLOperator.NOT_BETWEEN.value
    return result


def _convert_not_exists(inner: exp.Exists) -> dict[str, Any]:
    return {
        'op': JSQLOperator.NOT_EXISTS.value,
        'subquery': _convert_select(inner.this),
    }


_NOT_OPERATOR_HANDLERS: dict[type[exp.Expression], Callable[[exp.Expression], dict[str, Any]]] = {
    exp.In: _convert_not_in,
    exp.Like: _convert_not_like,
    exp.ILike: _convert_not_ilike,
    exp.Between: _convert_not_between,
    exp.Exists: _convert_not_exists,
}


def _convert_not(expr: exp.Not) -> dict[str, Any]:
    inner = expr.this
    not_handler = _NOT_OPERATOR_HANDLERS.get(type(inner))
    if not_handler is not None:
        return not_handler(inner)
    return {
        'op': JSQLOperator.NOT.value,
        'condition': convert_condition_to_jsql(inner),
    }


def _convert_comparison(expr: exp.EQ | exp.NEQ | exp.GT | exp.GTE | exp.LT | exp.LTE) -> dict[str, Any]:
    op = {
        exp.EQ: JSQLOperator.EQ.value,
        exp.NEQ: JSQLOperator.NE.value,
        exp.GT: JSQLOperator.GT.value,
        exp.GTE: JSQLOperator.GE.value,
        exp.LT: JSQLOperator.LT.value,
        exp.LTE: JSQLOperator.LE.value,
    }.get(type(expr))
    if op is None:
        raise UnknownOperatorError(
            operator=type(expr).__name__,
            path='',
            supported=[
                JSQLOperator.EQ.value,
                JSQLOperator.NE.value,
                JSQLOperator.GT.value,
                JSQLOperator.GE.value,
                JSQLOperator.LT.value,
                JSQLOperator.LE.value,
            ],
        )

    left = _convert_expression(expr.left)
    right = _convert_expression(expr.right)
    return {
        'op': op,
        'left': left,
        'right': right,
    }


def _convert_ilike(expr: exp.ILike) -> dict[str, Any]:
    return {
        'op': JSQLOperator.ILIKE.value,
        'left': _convert_expression(expr.this),
        'right': _convert_expression(expr.expression),
    }


def _convert_similar_to(expr: exp.SimilarTo) -> dict[str, Any]:
    return {
        'op': JSQLOperator.SIMILAR_TO.value,
        'left': _convert_expression(expr.this),
        'right': _convert_expression(expr.expression),
    }


def _convert_regexp_like(expr: exp.RegexpLike) -> dict[str, Any]:
    return {
        'op': JSQLOperator.REGEXP.value,
        'left': _convert_expression(expr.this),
        'right': _convert_expression(expr.expression),
    }


def _convert_exists(expr: exp.Exists) -> dict[str, Any]:
    return {
        'op': JSQLOperator.EXISTS.value,
        'subquery': _convert_select(expr.this),
    }


def _convert_in(expr: exp.In) -> dict[str, Any]:
    left = _convert_expression(expr.this)

    if expr.args.get('query'):
        query_expr = expr.args['query']
        if isinstance(query_expr, exp.Subquery):
            select_expr = query_expr.this
            if isinstance(select_expr, exp.Subquery):
                select_expr = select_expr.this
            if isinstance(select_expr, exp.Select):
                return {
                    'op': JSQLOperator.IN.value,
                    'left': left,
                    'right': _convert_select(select_expr),
                }

    if expr.expressions:
        first_expr = expr.expressions[0]
        if isinstance(first_expr, exp.Subquery):
            return {
                'op': JSQLOperator.IN.value,
                'left': left,
                'right': _convert_select(first_expr.this),
            }

    values = extract_in_operator_values(expr.expressions, 'IN', _convert_expression)
    return {
        'op': JSQLOperator.IN.value,
        'left': left,
        'right': {'values': values},
    }


def _convert_between(expr: exp.Between) -> dict[str, Any]:
    result = cast(
        'dict[str, Any]',
        normalize_between_jsql_fields(
        _convert_expression(expr.this),
        _convert_expression(expr.args.get('low')),
        _convert_expression(expr.args.get('high')),
        ),
    )
    result['op'] = JSQLOperator.BETWEEN.value
    return result


def _convert_like(expr: exp.Like) -> dict[str, Any]:
    lower_result = extract_lower_like_pattern(expr, _convert_expression)
    if lower_result is not None:
        left, right = lower_result
        return {
            'op': JSQLOperator.ILIKE.value,
            'left': left,
            'right': right,
        }
    return {
        'op': JSQLOperator.LIKE.value,
        'left': _convert_expression(expr.this),
        'right': _convert_expression(expr.expression),
    }


def _convert_is(expr: exp.Is) -> dict[str, Any]:
    left = _convert_expression(expr.this)
    if expr.args.get('expression') and isinstance(expr.args['expression'], exp.Null):
        return {'op': JSQLOperator.IS_NULL.value, 'left': left, 'right': {'value': None}}
    if isinstance(expr.args.get('this'), exp.Null):
        return {'op': JSQLOperator.IS_NULL.value, 'left': left, 'right': {'value': None}}
    raise InvalidExpressionError(
        message='IS operator must compare with NULL',
        path='',
        expression={'type': 'Is', 'expression': str(expr)},
    )


_CONDITION_HANDLERS: dict[type[exp.Expression], Callable[[exp.Expression], dict[str, Any]]] = {
    exp.And: _convert_and,
    exp.Or: _convert_or,
    exp.Not: _convert_not,
    exp.EQ: _convert_comparison,
    exp.NEQ: _convert_comparison,
    exp.GT: _convert_comparison,
    exp.GTE: _convert_comparison,
    exp.LT: _convert_comparison,
    exp.LTE: _convert_comparison,
    exp.In: _convert_in,
    exp.Between: _convert_between,
    exp.Like: _convert_like,
    exp.ILike: _convert_ilike,
    exp.SimilarTo: _convert_similar_to,
    exp.RegexpLike: _convert_regexp_like,
    exp.Exists: _convert_exists,
    exp.Is: _convert_is,
}


def convert_condition_to_jsql(expr: exp.Expression) -> dict[str, Any]:
    """Convert SQLGlot condition expression to JSQL condition spec."""
    handler = _CONDITION_HANDLERS.get(type(expr))
    if handler is not None:
        return handler(expr)
    raise UnsupportedOperationError(
        operation=f'Condition type: {type(expr).__name__}',
        reason='This condition type is not supported in JSQL conversion',
        path='',
    )


__all__ = ['convert_condition_to_jsql']
