"""Helper functions for condition conversion."""

from collections.abc import Callable
from typing import Any

import sqlglot.expressions as exp
from sqlalchemy.sql.elements import ClauseElement

from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError

__all__ = [
    'extract_in_operator_values',
    'extract_left_expression',
    'extract_lower_like_pattern',
    'extract_pattern',
    'normalize_between_jsql_fields',
    'validate_field_reference',
    'validate_string_operator_operands',
]


def extract_left_expression(
    cond_spec: dict[str, Any],
    expression_converter: Callable[[dict[str, Any] | str], ClauseElement],
) -> ClauseElement:
    """
    Extract left-side expression from condition specification.

    Supports both old format ('field') and new format ('left').
    """
    if 'left' in cond_spec:
        return expression_converter(cond_spec['left'])
    if 'expr' in cond_spec:
        return expression_converter(cond_spec['expr'])
    if 'field' in cond_spec:
        return expression_converter(cond_spec['field'])
    raise MissingFieldError('left or field', path='left/field', context='condition')  # noqa: TRY003


def extract_pattern(
    cond_spec: dict[str, Any],
    expression_converter: Callable[[dict[str, Any] | str], ClauseElement],
) -> str | None:
    """Extract string pattern from condition specification."""
    if 'pattern' in cond_spec:
        return str(cond_spec['pattern'])

    if 'right' in cond_spec:
        right_expr = expression_converter(cond_spec['right'])
        if isinstance(right_expr, exp.Literal) and right_expr.is_string:
            return str(right_expr.this)

    return None


def extract_in_operator_values(
    expressions: list[exp.Expression],
    operator: str,
    expression_converter: Callable[[exp.Expression], dict[str, Any]],
) -> list[Any]:
    """Extract literal values from IN/NOT IN expression list."""
    values: list[Any] = []
    for value_expr in expressions:
        converted = expression_converter(value_expr)
        if 'value' not in converted:
            raise InvalidExpressionError(
                message=f'{operator} operator values must be literals',
                path='values',
                expression=converted,
            )
        values.append(converted['value'])
    return values


def extract_lower_like_pattern(
    like_expr: exp.Like,
    expression_converter: Callable[[exp.Expression], dict[str, Any]],
) -> tuple[dict[str, Any], dict[str, Any]] | None:
    """Extract field/pattern from LOWER(field) LIKE LOWER(pattern) form."""
    left_is_lower = isinstance(like_expr.this, exp.Lower)
    right_is_lower = isinstance(like_expr.expression, exp.Lower)
    if not (left_is_lower and right_is_lower):
        return None

    left_inner = like_expr.this.this
    right_inner = like_expr.expression.this
    return expression_converter(left_inner), expression_converter(right_inner)


def normalize_between_jsql_fields(
    expr_jsql: dict[str, Any],
    low_jsql: dict[str, Any],
    high_jsql: dict[str, Any],
) -> dict[str, Any]:
    """Normalize BETWEEN fields to consistent JSQL structure."""
    result: dict[str, Any] = {}

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


def validate_string_operator_operands(
    left: dict[str, Any],
    right: dict[str, Any],
    operator: str,
    context: str = '',
) -> tuple[str, str]:
    """Validate string operator operands and return (field, pattern)."""
    if 'field' not in left:
        message = f'{operator} operator'
        if context:
            message += f' ({context})'
        message += ' left side must be a field reference'
        raise InvalidExpressionError(
            message=message,
            path='left',
            expression=left,
        )

    pattern_value = right.get('value') or right.get('pattern')
    if pattern_value is None:
        message = f'{operator} operator'
        if context:
            message += f' ({context})'
        message += ' right side must be a string literal'
        raise InvalidExpressionError(
            message=message,
            path='right',
            expression=right,
        )

    return left['field'], str(pattern_value)


def validate_field_reference(
    expr_jsql: dict[str, Any],
    operator: str,
    side: str = 'left',
) -> str:
    """Validate that expression contains a field reference."""
    if 'field' not in expr_jsql:
        raise InvalidExpressionError(
            message=f'{operator} operator {side} side must be a field reference',
            path=side,
            expression=expr_jsql,
        )
    return str(expr_jsql['field'])
