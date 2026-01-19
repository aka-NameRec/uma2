"""SQLGlot to JSQL conversion helpers and entry points."""

import logging
from collections.abc import Callable
from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.conversion_exceptions import UnsupportedOperationError
from namerec.uma.jsql.converter.operators import SQLGLOT_JOIN_SIDE_TO_TYPE
from namerec.uma.jsql.converter.operators import SQLGLOT_TO_ARITHMETIC
from namerec.uma.jsql.converter.operators import SQLGLOT_TO_COMPARISON
from namerec.uma.jsql.types import JSQLExpression

logger = logging.getLogger(__name__)

CONCAT_BINARY_ARITY = 2


def extract_in_operator_values(
    expressions: list[exp.Expression],
    operator: str,
) -> list[Any]:
    """
    Extract and validate literal values from IN/NOT IN operator expressions.

    This helper function centralizes the logic for extracting values from IN/NOT IN
    operator expressions, ensuring all values are literals (not field references or
    subqueries).

    Args:
        expressions: List of SQLGlot expressions (from exp.In.expressions)
        operator: Operator name for error messages ('IN' or 'NOT IN')

    Returns:
        List of literal values

    Raises:
        InvalidExpressionError: If any expression is not a literal value
    """
    values: list[Any] = []
    if isinstance(expressions, list):
        for val_expr in expressions:
            val = convert_expression_to_jsql(val_expr)
            if 'value' not in val:
                raise InvalidExpressionError(
                    message=f'{operator} operator values must be literals',
                    path='values',
                    expression=val,
                )
            values.append(val['value'])

    return values


def extract_lower_like_pattern(
    like_expr: exp.Like,
) -> tuple[dict[str, Any], dict[str, Any]] | None:
    """
    Extract field and pattern from LOWER(...) LIKE LOWER(...) pattern.

    SQLite doesn't support ILIKE, so SQLGlot converts ILIKE to LOWER(...) LIKE LOWER(...).
    This function detects this pattern and extracts the inner expressions for conversion
    back to ILIKE.
    """
    left_is_lower = isinstance(like_expr.this, exp.Lower)
    right_is_lower = isinstance(like_expr.expression, exp.Lower)

    if not (left_is_lower and right_is_lower):
        return None

    # Extract inner expressions from LOWER()
    left_inner = like_expr.this.this
    right_inner = like_expr.expression.this

    left = convert_expression_to_jsql(left_inner)
    right = convert_expression_to_jsql(right_inner)

    return left, right


def normalize_between_jsql_fields(
    expr_jsql: dict[str, Any],
    low_jsql: dict[str, Any],
    high_jsql: dict[str, Any],
) -> dict[str, Any]:
    """
    Normalize BETWEEN expression components to JSQL format.

    This helper function normalizes the expr, low, and high fields for BETWEEN
    and NOT BETWEEN operators. It handles both field references and value literals,
    ensuring consistent JSQL structure.
    """
    result: dict[str, Any] = {}

    # Normalize expr field
    if 'field' in expr_jsql:
        result['expr'] = {'field': expr_jsql['field']}
    elif 'value' in expr_jsql:
        result['expr'] = {'value': expr_jsql['value']}
    else:
        result['expr'] = expr_jsql

    # Normalize low field
    if 'value' in low_jsql:
        result['low'] = {'value': low_jsql['value']}
    elif 'field' in low_jsql:
        result['low'] = {'field': low_jsql['field']}
    else:
        result['low'] = low_jsql

    # Normalize high field
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
    """
    Validate operands for string operators (LIKE, ILIKE, SIMILAR_TO, REGEXP, etc.).
    """
    if 'field' not in left:
        msg = f'{operator} operator'
        if context:
            msg += f' ({context})'
        msg += ' left side must be a field reference'
        raise InvalidExpressionError(
            message=msg,
            path='left',
            expression=left,
        )

    # Accept both 'value' and 'pattern' fields (they're semantically equivalent)
    pattern_value = right.get('value') or right.get('pattern')
    if pattern_value is None:
        msg = f'{operator} operator'
        if context:
            msg += f' ({context})'
        msg += ' right side must be a string literal'
        raise InvalidExpressionError(
            message=msg,
            path='right',
            expression=right,
        )

    return left['field'], pattern_value


def validate_field_reference(
    expr_jsql: dict[str, Any],
    operator: str,
    side: str = 'left',
) -> str:
    """Validate that expression is a field reference."""
    if 'field' not in expr_jsql:
        raise InvalidExpressionError(
            message=f'{operator} operator {side} side must be a field reference',
            path=side,
            expression=expr_jsql,
        )
    return expr_jsql['field']


def _extract_distinct_expression(expr: exp.Distinct) -> tuple[JSQLExpression, bool]:
    """
    Extract expression from DISTINCT wrapper and return (jsql, is_distinct).

    This function centralizes DISTINCT handling logic. It handles both 'expressions'
    list and 'this' attributes of exp.Distinct.
    """
    if expr.expressions and len(expr.expressions) > 0:
        inner_expr = expr.expressions[0]
    else:
        inner_expr = expr.this
        if inner_expr is None:
            raise InvalidExpressionError(
                message='DISTINCT must wrap an expression',
                path='',
                expression={'type': 'Distinct', 'expressions': expr.expressions, 'this': None},
            )

    inner_jsql = convert_expression_to_jsql(inner_expr)

    if isinstance(inner_jsql, dict):
        inner_jsql['_distinct_marker'] = True

    return inner_jsql, True


def convert_expression_to_jsql(expr: exp.Expression) -> JSQLExpression:
    """
    Convert sqlglot expression to JSQL expression.

    Raises:
        InvalidExpressionError: If expression structure is invalid
        UnknownOperatorError: If arithmetic operator is not supported
        UnsupportedOperationError: If expression type is not supported
    """
    if isinstance(expr, exp.Alias):
        base_expr = convert_expression_to_jsql(expr.this)
        base_expr['alias'] = expr.alias
        return base_expr

    if isinstance(expr, exp.Column):
        result: JSQLExpression = {'field': expr.name}
        if expr.table:
            result['field'] = f'{expr.table}.{expr.name}'
        return result

    if isinstance(expr, exp.Literal):
        value = expr.this
        if expr.is_int:
            value = int(value)
        elif expr.is_number:
            value = float(value)
        return {'value': value}

    if isinstance(expr, exp.Boolean):
        return {'value': expr.this}

    if isinstance(expr, exp.Star):
        return {'field': '*'}

    if isinstance(expr, exp.Subquery):
        return convert_sqlglot_select_to_jsql(expr.this)

    if isinstance(expr, exp.Window):
        func_expr = expr.this
        if isinstance(func_expr, exp.Func):
            func_name = func_expr.sql_name()
            args = [
                converted
                for arg in func_expr.args.values()
                if isinstance(arg, exp.Expression) and (converted := convert_expression_to_jsql(arg)) is not None
            ]

            result: dict[str, Any] = {
                'func': func_name,
            }
            if args:
                result['args'] = args

            over_spec: dict[str, Any] = {}

            if expr.args.get('partition_by'):
                partition_by = []
                part_by_arg = expr.args['partition_by']
                if isinstance(part_by_arg, list):
                    for part_expr in part_by_arg:
                        part_jsql = convert_expression_to_jsql(part_expr)
                        partition_by.append(part_jsql)
                elif hasattr(part_by_arg, 'expressions'):
                    for part_expr in part_by_arg.expressions:
                        part_jsql = convert_expression_to_jsql(part_expr)
                        partition_by.append(part_jsql)
                if partition_by:
                    over_spec['partition_by'] = partition_by

            if expr.args.get('order'):
                order_by = []
                order_arg = expr.args['order']
                if isinstance(order_arg, list):
                    for order_expr in order_arg:
                        order_jsql = convert_order_to_jsql(order_expr)
                        order_by.append(order_jsql)
                elif hasattr(order_arg, 'expressions'):
                    for order_expr in order_arg.expressions:
                        order_jsql = convert_order_to_jsql(order_expr)
                        order_by.append(order_jsql)
                if order_by:
                    over_spec['order_by'] = order_by

            if over_spec:
                result['over'] = over_spec

            return result

    if isinstance(expr, exp.Distinct):
        inner_jsql, _ = _extract_distinct_expression(expr)
        return inner_jsql

    if isinstance(expr, exp.Func):
        func_name = expr.sql_name()
        args = []
        first_arg_is_distinct = False

        for idx, arg in enumerate(expr.args.values()):
            if isinstance(arg, exp.Expression):
                if isinstance(arg, exp.Distinct):
                    converted, _ = _extract_distinct_expression(arg)
                    args.append(converted)
                    if idx == 0:
                        first_arg_is_distinct = True
                else:
                    converted = convert_expression_to_jsql(arg)
                    if isinstance(converted, dict) and converted.get('_distinct_marker'):
                        del converted['_distinct_marker']
                        args.append(converted)
                        if idx == 0:
                            first_arg_is_distinct = True
                    else:
                        args.append(converted)

        result = {
            'func': func_name,
            'args': args,
        }

        if first_arg_is_distinct:
            result['distinct'] = True

        return result

    if isinstance(expr, exp.Concat):
        if hasattr(expr, 'expressions') and expr.expressions:
            expressions = expr.expressions
        elif hasattr(expr, 'left') and hasattr(expr, 'right'):
            expressions = [expr.left, expr.right]
        else:
            expressions = getattr(expr, 'expressions', None) or expr.args.get('expressions', [])

        if len(expressions) == CONCAT_BINARY_ARITY:
            return {
                'op': JSQLOperator.CONCAT.value,
                'left': convert_expression_to_jsql(expressions[0]),
                'right': convert_expression_to_jsql(expressions[1]),
            }
        if len(expressions) > CONCAT_BINARY_ARITY:
            result = convert_expression_to_jsql(expressions[-1])
            for expr_item in reversed(expressions[:-1]):
                result = {
                    'op': JSQLOperator.CONCAT.value,
                    'left': convert_expression_to_jsql(expr_item),
                    'right': result,
                }
            return result

    if isinstance(expr, (exp.Add, exp.Sub, exp.Mul, exp.Div)):
        op = SQLGLOT_TO_ARITHMETIC.get(type(expr))
        if not op:
            raise UnknownOperatorError(
                operator=type(expr).__name__,
                path='',
                supported=list(SQLGLOT_TO_ARITHMETIC.values()),
            )

        return {
            'op': op,
            'left': convert_expression_to_jsql(expr.left),
            'right': convert_expression_to_jsql(expr.right),
        }

    raise UnsupportedOperationError(
        operation=f'Expression type: {type(expr).__name__}',
        reason='This expression type is not supported in JSQL conversion',
        path='',
    )


def _convert_and(expr: exp.And) -> dict[str, Any]:
    """Convert AND operator to JSQL."""
    return {
        'op': JSQLOperator.AND.value,
        'conditions': [
            convert_condition_to_jsql(expr.left),
            convert_condition_to_jsql(expr.right),
        ],
    }


def _convert_or(expr: exp.Or) -> dict[str, Any]:
    """Convert OR operator to JSQL."""
    return {
        'op': JSQLOperator.OR.value,
        'conditions': [
            convert_condition_to_jsql(expr.left),
            convert_condition_to_jsql(expr.right),
        ],
    }


def _convert_not(expr: exp.Not) -> dict[str, Any]:
    """Convert NOT operator to JSQL."""
    inner = expr.this
    not_handler = _NOT_OPERATOR_HANDLERS.get(type(inner))
    if not_handler:
        return not_handler(inner)

    return {
        'op': JSQLOperator.NOT.value,
        'condition': convert_condition_to_jsql(expr.this),
    }


def _convert_not_in(inner: exp.In) -> dict[str, Any]:
    """Convert NOT(IN(...)) to NOT IN operator."""
    left = convert_expression_to_jsql(inner.this)

    if inner.args.get('query'):
        query_expr = inner.args['query']
        if isinstance(query_expr, exp.Subquery):
            select_expr = query_expr.this
            if isinstance(select_expr, exp.Subquery):
                select_expr = select_expr.this
            if isinstance(select_expr, exp.Select):
                subquery_jsql = convert_sqlglot_select_to_jsql(select_expr)
                return {
                    'op': JSQLOperator.NOT_IN.value,
                    'left': left,
                    'right': subquery_jsql,
                }

    values = extract_in_operator_values(inner.expressions, 'NOT IN')
    return {
        'op': JSQLOperator.NOT_IN.value,
        'left': left,
        'right': {'values': values},
    }


def _convert_not_like(inner: exp.Like) -> dict[str, Any]:
    """Convert NOT(LIKE(...)) to NOT LIKE or NOT ILIKE operator."""
    lower_result = extract_lower_like_pattern(inner)

    if lower_result:
        left, right = lower_result
        return {
            'op': JSQLOperator.NOT_ILIKE.value,
            'left': left,
            'right': right,
        }

    left = convert_expression_to_jsql(inner.this)
    right = convert_expression_to_jsql(inner.expression)

    return {
        'op': JSQLOperator.NOT_LIKE.value,
        'left': left,
        'right': right,
    }


def _convert_not_ilike(inner: exp.ILike) -> dict[str, Any]:
    """Convert NOT(ILIKE(...)) to NOT ILIKE operator."""
    left = convert_expression_to_jsql(inner.this)
    right = convert_expression_to_jsql(inner.expression)

    return {
        'op': JSQLOperator.NOT_ILIKE.value,
        'left': left,
        'right': right,
    }


def _convert_not_between(inner: exp.Between) -> dict[str, Any]:
    """Convert NOT(BETWEEN(...)) to NOT BETWEEN operator."""
    expr_jsql = convert_expression_to_jsql(inner.this)
    low_jsql = convert_expression_to_jsql(inner.args.get('low'))
    high_jsql = convert_expression_to_jsql(inner.args.get('high'))

    result = normalize_between_jsql_fields(expr_jsql, low_jsql, high_jsql)
    result['op'] = JSQLOperator.NOT_BETWEEN.value
    return result


def _convert_not_exists(inner: exp.Exists) -> dict[str, Any]:
    """Convert NOT(EXISTS(...)) to NOT EXISTS operator."""
    subquery_expr = inner.this
    return {
        'op': JSQLOperator.NOT_EXISTS.value,
        'subquery': convert_sqlglot_select_to_jsql(subquery_expr),
    }


_NOT_OPERATOR_HANDLERS: dict[type[exp.Expression], Callable[[exp.Expression], dict[str, Any]]] = {
    exp.In: _convert_not_in,
    exp.Like: _convert_not_like,
    exp.ILike: _convert_not_ilike,
    exp.Between: _convert_not_between,
    exp.Exists: _convert_not_exists,
}


def _convert_comparison(expr: exp.EQ | exp.NEQ | exp.GT | exp.GTE | exp.LT | exp.LTE) -> dict[str, Any]:
    """Convert comparison operator to JSQL."""
    op = SQLGLOT_TO_COMPARISON.get(type(expr))
    if not op:
        raise UnknownOperatorError(
            operator=type(expr).__name__,
            path='',
            supported=list(SQLGLOT_TO_COMPARISON.values()),
        )

    left = convert_expression_to_jsql(expr.left)
    right = convert_expression_to_jsql(expr.right)

    if 'field' not in left and 'func' not in left and 'op' not in left and 'from' not in left:
        raise InvalidExpressionError(
            message='Comparison operator left side must be a field reference, function call, expression, or subquery',
            path='left',
            expression=left,
        )

    if (
        'value' not in right
        and 'field' not in right
        and 'func' not in right
        and 'from' not in right
        and 'op' not in right
    ):
        raise InvalidExpressionError(
            message=(
                'Comparison operator right side must be a value, field reference, '
                'function call, expression, or subquery'
            ),
            path='right',
            expression=right,
        )

    return {
        'op': op,
        'left': left,
        'right': right,
    }


def _convert_ilike(expr: exp.ILike) -> dict[str, Any]:
    """Convert ILIKE operator to JSQL."""
    left = convert_expression_to_jsql(expr.this)
    right = convert_expression_to_jsql(expr.expression)

    return {
        'op': JSQLOperator.ILIKE.value,
        'left': left,
        'right': right,
    }


def _convert_similar_to(expr: exp.SimilarTo) -> dict[str, Any]:
    """Convert SIMILAR TO operator to JSQL."""
    left = convert_expression_to_jsql(expr.this)
    right = convert_expression_to_jsql(expr.expression)

    return {
        'op': JSQLOperator.SIMILAR_TO.value,
        'left': left,
        'right': right,
    }


def _convert_regexp_like(expr: exp.RegexpLike) -> dict[str, Any]:
    """Convert REGEXP operator to JSQL."""
    left = convert_expression_to_jsql(expr.this)
    right = convert_expression_to_jsql(expr.expression)

    return {
        'op': JSQLOperator.REGEXP.value,
        'left': left,
        'right': right,
    }


def _convert_exists(expr: exp.Exists) -> dict[str, Any]:
    """Convert EXISTS operator to JSQL."""
    subquery_expr = expr.this
    return {
        'op': JSQLOperator.EXISTS.value,
        'subquery': convert_sqlglot_select_to_jsql(subquery_expr),
    }


def _convert_in(expr: exp.In) -> dict[str, Any]:
    """Convert IN operator to JSQL."""
    left = convert_expression_to_jsql(expr.this)

    if expr.args.get('query'):
        query_expr = expr.args['query']
        if isinstance(query_expr, exp.Subquery):
            select_expr = query_expr.this
            if isinstance(select_expr, exp.Subquery):
                select_expr = select_expr.this
            if isinstance(select_expr, exp.Select):
                subquery_jsql = convert_sqlglot_select_to_jsql(select_expr)
                return {
                    'op': JSQLOperator.IN.value,
                    'left': left,
                    'right': subquery_jsql,
                }

    if expr.expressions:
        first_expr = expr.expressions[0]
        if isinstance(first_expr, exp.Subquery):
            subquery_jsql = convert_sqlglot_select_to_jsql(first_expr.this)
            return {
                'op': JSQLOperator.IN.value,
                'left': left,
                'right': subquery_jsql,
            }

    values = extract_in_operator_values(expr.expressions, 'IN')
    return {
        'op': JSQLOperator.IN.value,
        'left': left,
        'right': {'values': values},
    }


def _convert_between(expr: exp.Between) -> dict[str, Any]:
    """Convert BETWEEN operator to JSQL."""
    expr_jsql = convert_expression_to_jsql(expr.this)
    low_jsql = convert_expression_to_jsql(expr.args.get('low'))
    high_jsql = convert_expression_to_jsql(expr.args.get('high'))

    result = normalize_between_jsql_fields(expr_jsql, low_jsql, high_jsql)
    result['op'] = JSQLOperator.BETWEEN.value
    return result


def _convert_like(expr: exp.Like) -> dict[str, Any]:
    """Convert LIKE operator to JSQL."""
    lower_result = extract_lower_like_pattern(expr)

    if lower_result:
        left, right = lower_result
        return {
            'op': JSQLOperator.ILIKE.value,
            'left': left,
            'right': right,
        }

    left = convert_expression_to_jsql(expr.this)
    right = convert_expression_to_jsql(expr.expression)

    return {
        'op': JSQLOperator.LIKE.value,
        'left': left,
        'right': right,
    }


def _convert_is(expr: exp.Is) -> dict[str, Any]:
    """Convert IS (NULL/NOT NULL) operator to JSQL."""
    left = convert_expression_to_jsql(expr.this)

    if expr.args.get('expression') and isinstance(expr.args['expression'], exp.Null):
        return {
            'op': JSQLOperator.IS_NULL.value,
            'left': left,
            'right': {'value': None},
        }

    if isinstance(expr.args.get('this'), exp.Null):
        return {
            'op': JSQLOperator.IS_NULL.value,
            'left': left,
            'right': {'value': None},
        }

    raise InvalidExpressionError(
        message='IS operator must compare with NULL',
        path='',
        expression={'type': 'Is', 'expression': str(expr)},
    )


def convert_condition_to_jsql(expr: exp.Expression) -> dict[str, Any]:
    """
    Convert SQLGlot expression to JSQL condition specification.

    Raises:
        UnsupportedOperationError: If expression type is not supported
    """
    if isinstance(expr, exp.And):
        return _convert_and(expr)
    if isinstance(expr, exp.Or):
        return _convert_or(expr)
    if isinstance(expr, exp.Not):
        return _convert_not(expr)

    if isinstance(expr, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE)):
        return _convert_comparison(expr)

    if isinstance(expr, exp.In):
        return _convert_in(expr)

    if isinstance(expr, exp.Between):
        return _convert_between(expr)

    if isinstance(expr, exp.Like):
        return _convert_like(expr)

    if isinstance(expr, exp.ILike):
        return _convert_ilike(expr)

    if isinstance(expr, exp.SimilarTo):
        return _convert_similar_to(expr)

    if isinstance(expr, exp.RegexpLike):
        return _convert_regexp_like(expr)

    if isinstance(expr, exp.Exists):
        return _convert_exists(expr)

    if isinstance(expr, exp.Is):
        return _convert_is(expr)

    raise UnsupportedOperationError(
        operation=f'Condition type: {type(expr).__name__}',
        reason='This condition type is not supported in JSQL conversion',
        path='',
    )


def convert_join_to_jsql(join: exp.Join) -> dict[str, Any]:
    """
    Convert sqlglot JOIN to JSQL join.

    Raises:
        InvalidExpressionError: If JOIN structure is invalid
    """
    if not isinstance(join, exp.Join):
        raise InvalidExpressionError(
            message=f'Expected exp.Join, got {type(join).__name__}',
            path='joins',
            expression={'type': type(join).__name__},
        )

    if join.kind and join.kind.upper() == 'CROSS':
        join_type = JoinType.CROSS.value
    elif join.side:
        join_type = SQLGLOT_JOIN_SIDE_TO_TYPE.get(join.side, JoinType.INNER.value)
    else:
        join_type = JoinType.INNER.value

    table = join.this
    if not isinstance(table, exp.Table):
        raise InvalidExpressionError(
            message=f'JOIN table must be exp.Table, got {type(table).__name__}',
            path='joins.this',
            expression={'type': type(table).__name__},
        )

    result: dict[str, Any] = {
        'type': join_type,
        'entity': table.name,
    }

    if table.alias:
        result['alias'] = table.alias

    if join.args.get('on'):
        on_arg = join.args['on']

        if isinstance(
            on_arg,
            (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE, exp.And, exp.Or, exp.Not, exp.In, exp.Like, exp.Is),
        ):
            on_expr = on_arg
        elif hasattr(on_arg, 'this') and on_arg.this is not None:
            on_expr = on_arg.this
        else:
            on_expr = on_arg

        logger.debug(f'ON condition type: {type(on_expr).__name__}, value: {on_expr}')

        on_jsql = convert_condition_to_jsql(on_expr)
        result['on'] = on_jsql

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
            expression={'type': type(order_expr).__name__},
        )

    field_expr = convert_expression_to_jsql(order_expr.this)
    result = field_expr.copy()
    direction = OrderDirection.DESC if order_expr.args.get('desc') else OrderDirection.ASC
    result['direction'] = direction.value.upper()

    return result


def convert_sqlglot_select_to_jsql(select_expr: exp.Select) -> dict[str, Any]:
    """
    Convert SQLGlot Select expression to JSQL query dictionary.

    This helper converts a complete SELECT statement to its JSQL representation,
    including optional WITH, WHERE, JOIN, ORDER BY, LIMIT/OFFSET, GROUP BY, HAVING.
    """
    result: dict[str, Any] = {}

    if select_expr.args.get('with_'):
        ctes = []
        for cte in select_expr.args['with_'].expressions:
            if isinstance(cte, exp.CTE):
                cte_name = cte.alias
                cte_query = convert_sqlglot_select_to_jsql(cte.this)
                ctes.append({'name': cte_name, 'query': cte_query})
        if ctes:
            result['with'] = ctes

    if (from_expr := select_expr.args.get('from_')) and isinstance(from_expr, exp.From):
        table = from_expr.this
        if isinstance(table, exp.Table):
            if table.alias:
                result['from'] = {
                    'entity': table.name,
                    'alias': table.alias,
                }
            else:
                result['from'] = table.name

    if select_expr.expressions:
        select_fields = []
        for expr in select_expr.expressions:
            field_dict = convert_expression_to_jsql(expr)
            select_fields.append(field_dict)
        if select_fields:
            result['select'] = select_fields

    if select_expr.args.get('where'):
        where_expr = select_expr.args['where'].this
        where_jsql = convert_condition_to_jsql(where_expr)
        result['where'] = where_jsql

    if select_expr.args.get('joins'):
        joins = []
        for join in select_expr.args['joins']:
            join_dict = convert_join_to_jsql(join)
            joins.append(join_dict)
        if joins:
            result['joins'] = joins

    if select_expr.args.get('order'):
        order_by = []
        for order_expr in select_expr.args['order'].expressions:
            order_dict = convert_order_to_jsql(order_expr)
            order_by.append(order_dict)
        if order_by:
            result['order_by'] = order_by

    if select_expr.args.get('limit'):
        limit_expr = select_expr.args['limit'].expression
        if isinstance(limit_expr, exp.Literal):
            result['limit'] = int(limit_expr.this)

    if select_expr.args.get('offset'):
        offset_expr = select_expr.args['offset'].expression
        if isinstance(offset_expr, exp.Literal):
            result['offset'] = int(offset_expr.this)

    if select_expr.args.get('group'):
        group_by = []
        for group_expr in select_expr.args['group'].expressions:
            field_dict = convert_expression_to_jsql(group_expr)
            group_by.append(field_dict)
        if group_by:
            result['group_by'] = group_by

    if select_expr.args.get('having'):
        having_expr = select_expr.args['having'].this
        having_jsql = convert_condition_to_jsql(having_expr)
        result['having'] = having_jsql

    return result
