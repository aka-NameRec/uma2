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
from namerec.uma.jsql.converter.conditions.to_jsql import convert_condition_to_jsql
from namerec.uma.jsql.converter.operators import SQLGLOT_JOIN_SIDE_TO_TYPE
from namerec.uma.jsql.converter.operators import SQLGLOT_TO_ARITHMETIC
from namerec.uma.jsql.types import JSQLExpression

logger = logging.getLogger(__name__)

CONCAT_BINARY_ARITY = 2


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


def _convert_alias_expression(expr: exp.Expression) -> JSQLExpression:
    alias_expr = expr
    base_expr = convert_expression_to_jsql(alias_expr.this)
    base_expr['alias'] = alias_expr.alias
    return base_expr


def _convert_column_expression(expr: exp.Expression) -> JSQLExpression:
    column_expr = expr
    field_name = f'{column_expr.table}.{column_expr.name}' if column_expr.table else column_expr.name
    return {'field': field_name}


def _convert_literal_expression(expr: exp.Expression) -> JSQLExpression:
    literal_expr = expr
    value: Any = literal_expr.this
    if literal_expr.is_int:
        value = int(value)
    elif literal_expr.is_number:
        value = float(value)
    return {'value': value}


def _convert_boolean_expression(expr: exp.Expression) -> JSQLExpression:
    bool_expr = expr
    return {'value': bool_expr.this}


def _convert_star_expression(_: exp.Expression) -> JSQLExpression:
    return {'field': '*'}


def _convert_subquery_expression(expr: exp.Expression) -> JSQLExpression:
    subquery_expr = expr
    return convert_sqlglot_select_to_jsql(subquery_expr.this)


def _extract_over_partition(expr: exp.Window) -> list[JSQLExpression]:
    part_by_arg = expr.args.get('partition_by')
    if part_by_arg is None:
        return []
    if isinstance(part_by_arg, list):
        expressions = part_by_arg
    elif isinstance(part_by_arg, exp.Partition):
        expressions = part_by_arg.expressions
    else:
        return []
    return [convert_expression_to_jsql(part_expr) for part_expr in expressions if isinstance(part_expr, exp.Expression)]


def _extract_over_order(expr: exp.Window) -> list[dict[str, Any]]:
    order_arg = expr.args.get('order')
    if order_arg is None:
        return []
    if isinstance(order_arg, list):
        expressions = order_arg
    elif isinstance(order_arg, exp.Order):
        expressions = order_arg.expressions
    else:
        return []
    return [convert_order_to_jsql(order_expr) for order_expr in expressions if isinstance(order_expr, exp.Ordered)]


def _convert_window_expression(expr: exp.Expression) -> JSQLExpression:
    window_expr = expr
    func_expr = window_expr.this
    if not isinstance(func_expr, exp.Func):
        raise UnsupportedOperationError(
            operation=f'Window expression type: {type(func_expr).__name__}',
            reason='Only function-based window expressions are supported',
            path='',
        )

    args = [
        convert_expression_to_jsql(arg)
        for arg in func_expr.args.values()
        if isinstance(arg, exp.Expression)
    ]
    result: JSQLExpression = {'func': func_expr.sql_name()}
    if args:
        result['args'] = args

    over_spec: dict[str, Any] = {}
    partition_by = _extract_over_partition(window_expr)
    order_by = _extract_over_order(window_expr)
    if partition_by:
        over_spec['partition_by'] = partition_by
    if order_by:
        over_spec['order_by'] = order_by
    if over_spec:
        result['over'] = over_spec
    return result


def _convert_distinct_expression(expr: exp.Expression) -> JSQLExpression:
    distinct_expr = expr
    inner_jsql, _ = _extract_distinct_expression(distinct_expr)
    return inner_jsql


def _convert_function_expression(expr: exp.Expression) -> JSQLExpression:
    func_expr = expr
    args: list[JSQLExpression] = []
    first_arg_is_distinct = False
    for idx, arg in enumerate(func_expr.args.values()):
        if not isinstance(arg, exp.Expression):
            continue
        if isinstance(arg, exp.Distinct):
            converted, _ = _extract_distinct_expression(arg)
            args.append(converted)
            if idx == 0:
                first_arg_is_distinct = True
            continue

        converted = convert_expression_to_jsql(arg)
        if isinstance(converted, dict) and converted.get('_distinct_marker'):
            del converted['_distinct_marker']
            if idx == 0:
                first_arg_is_distinct = True
        args.append(converted)

    result: JSQLExpression = {'func': func_expr.sql_name(), 'args': args}
    if first_arg_is_distinct:
        result['distinct'] = True
    return result


def _convert_concat_expression(expr: exp.Expression) -> JSQLExpression:
    concat_expr = expr
    if hasattr(concat_expr, 'expressions') and concat_expr.expressions:
        expressions = concat_expr.expressions
    elif hasattr(concat_expr, 'left') and hasattr(concat_expr, 'right'):
        expressions = [concat_expr.left, concat_expr.right]
    else:
        expressions = getattr(concat_expr, 'expressions', None) or concat_expr.args.get('expressions', [])

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
    raise InvalidExpressionError(
        message='CONCAT expression must have at least two operands',
        path='',
        expression={'type': 'Concat', 'expressions': expressions},
    )


def _convert_arithmetic_expression(expr: exp.Expression) -> JSQLExpression:
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


_EXPRESSION_HANDLERS: list[tuple[type[exp.Expression], Callable[[exp.Expression], JSQLExpression]]] = [
    (exp.Alias, _convert_alias_expression),
    (exp.Column, _convert_column_expression),
    (exp.Literal, _convert_literal_expression),
    (exp.Boolean, _convert_boolean_expression),
    (exp.Star, _convert_star_expression),
    (exp.Subquery, _convert_subquery_expression),
    (exp.Window, _convert_window_expression),
    (exp.Distinct, _convert_distinct_expression),
    (exp.Func, _convert_function_expression),
    (exp.Concat, _convert_concat_expression),
]


def convert_expression_to_jsql(expr: exp.Expression) -> JSQLExpression:
    """Convert SQLGlot expression to JSQL expression."""
    for expression_type, handler in _EXPRESSION_HANDLERS:
        if isinstance(expr, expression_type):
            return handler(expr)

    if isinstance(expr, (exp.Add, exp.Sub, exp.Mul, exp.Div)):
        return _convert_arithmetic_expression(expr)

    raise UnsupportedOperationError(
        operation=f'Expression type: {type(expr).__name__}',
        reason='This expression type is not supported in JSQL conversion',
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
    result = dict(field_expr)
    direction = OrderDirection.DESC if order_expr.args.get('desc') else OrderDirection.ASC
    result['direction'] = direction.value.upper()

    return result


def _convert_with_clause(select_expr: exp.Select, result: dict[str, Any]) -> None:
    with_expr = select_expr.args.get('with_')
    if not with_expr:
        return
    ctes: list[dict[str, Any]] = []
    for cte in with_expr.expressions:
        if isinstance(cte, exp.CTE):
            ctes.append({'name': cte.alias, 'query': convert_sqlglot_select_to_jsql(cte.this)})
    if ctes:
        result['with'] = ctes


def _convert_from_clause(select_expr: exp.Select, result: dict[str, Any]) -> None:
    from_expr = select_expr.args.get('from_')
    if not isinstance(from_expr, exp.From):
        return
    table = from_expr.this
    if not isinstance(table, exp.Table):
        return
    if table.alias:
        result['from'] = {'entity': table.name, 'alias': table.alias}
    else:
        result['from'] = table.name


def _convert_select_fields(select_expr: exp.Select, result: dict[str, Any]) -> None:
    if not select_expr.expressions:
        return
    fields = [convert_expression_to_jsql(expr_item) for expr_item in select_expr.expressions]
    if fields:
        result['select'] = fields


def _convert_where_clause(select_expr: exp.Select, result: dict[str, Any]) -> None:
    where_expr = select_expr.args.get('where')
    if where_expr:
        result['where'] = convert_condition_to_jsql(where_expr.this)


def _convert_join_clause(select_expr: exp.Select, result: dict[str, Any]) -> None:
    joins = select_expr.args.get('joins')
    if joins:
        result['joins'] = [convert_join_to_jsql(join) for join in joins]


def _convert_order_clause(select_expr: exp.Select, result: dict[str, Any]) -> None:
    order_expr = select_expr.args.get('order')
    if order_expr:
        result['order_by'] = [convert_order_to_jsql(item) for item in order_expr.expressions]


def _convert_limit_offset(select_expr: exp.Select, result: dict[str, Any]) -> None:
    limit_expr = select_expr.args.get('limit')
    if limit_expr and isinstance(limit_expr.expression, exp.Literal):
        result['limit'] = int(limit_expr.expression.this)

    offset_expr = select_expr.args.get('offset')
    if offset_expr and isinstance(offset_expr.expression, exp.Literal):
        result['offset'] = int(offset_expr.expression.this)


def _convert_group_having(select_expr: exp.Select, result: dict[str, Any]) -> None:
    group_expr = select_expr.args.get('group')
    if group_expr:
        result['group_by'] = [convert_expression_to_jsql(item) for item in group_expr.expressions]

    having_expr = select_expr.args.get('having')
    if having_expr:
        result['having'] = convert_condition_to_jsql(having_expr.this)


def convert_sqlglot_select_to_jsql(select_expr: exp.Select) -> dict[str, Any]:
    """
    Convert SQLGlot Select expression to JSQL query dictionary.

    This helper converts a complete SELECT statement to its JSQL representation,
    including optional WITH, WHERE, JOIN, ORDER BY, LIMIT/OFFSET, GROUP BY, HAVING.
    """
    result: dict[str, Any] = {}
    _convert_with_clause(select_expr, result)
    _convert_from_clause(select_expr, result)
    _convert_select_fields(select_expr, result)
    _convert_where_clause(select_expr, result)
    _convert_join_clause(select_expr, result)
    _convert_order_clause(select_expr, result)
    _convert_limit_offset(select_expr, result)
    _convert_group_having(select_expr, result)
    return result
