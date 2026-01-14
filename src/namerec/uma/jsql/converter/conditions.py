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

    # Handle BETWEEN first, as it has different structure
    if op == JSQLOperator.BETWEEN.value:
        if 'expr' not in cond_spec:
            raise MissingFieldError('expr', path='expr', context='BETWEEN operator')
        if 'low' not in cond_spec:
            raise MissingFieldError('low', path='low', context='BETWEEN operator')
        if 'high' not in cond_spec:
            raise MissingFieldError('high', path='high', context='BETWEEN operator')

        expr = jsql_expression_to_sqlglot(cond_spec['expr'])
        low_expr = jsql_expression_to_sqlglot(cond_spec['low'])
        high_expr = jsql_expression_to_sqlglot(cond_spec['high'])

        logger.debug('Processing BETWEEN operator')
        return exp.Between(this=expr, low=low_expr, high=high_expr)

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

    # Handle standard comparison operators using mapping
    operator_class = COMPARISON_OP_TO_SQLGLOT.get(op)
    if operator_class:
        return operator_class(this=left_expr, expression=right_expr)

    # Handle special operators
    if op == JSQLOperator.IN.value:
        if 'values' not in cond_spec:
            raise MissingFieldError('values', path='values', context='IN operator')
        values = cond_spec['values']
        logger.debug(f'Processing IN operator with {len(values)} values')
        val_exprs = [jsql_expression_to_sqlglot({'value': v}) for v in values]
        return exp.In(this=left_expr, expressions=val_exprs)

    if op == JSQLOperator.LIKE.value:
        if 'pattern' not in cond_spec:
            raise MissingFieldError('pattern', path='pattern', context='LIKE operator')
        pattern = cond_spec['pattern']
        logger.debug(f'Processing LIKE operator with pattern: {pattern}')
        return exp.Like(this=left_expr, expression=exp.Literal.string(pattern))

    if op == JSQLOperator.IS_NULL.value:
        return exp.Is(this=left_expr, expression=exp.Null())

    if op == JSQLOperator.IS_NOT_NULL.value:
        return exp.Not(this=exp.Is(this=left_expr, expression=exp.Null()))

    # Unknown operator
    supported = list(COMPARISON_OP_TO_SQLGLOT.keys()) + [
        JSQLOperator.IN.value,
        JSQLOperator.BETWEEN.value,
        JSQLOperator.LIKE.value,
        JSQLOperator.IS_NULL.value,
        JSQLOperator.IS_NOT_NULL.value,
    ]
    logger.warning(f'Unknown comparison operator: {op}')
    raise UnknownOperatorError(operator=op, path='op', supported=supported)


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

    # Handle NOT
    if isinstance(expr, exp.Not):
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
        else:
            raise InvalidExpressionError(
                message='Comparison operator right side must be a value or field reference',
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
