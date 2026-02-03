"""JSQL to SQLGlot conversion helpers and entry points."""

import logging
from typing import Any
from typing import cast

import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.converter.conditions.to_sql import jsql_condition_to_sqlglot
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

    func_class_obj = getattr(exp, func.upper(), None)
    if isinstance(func_class_obj, type) and issubclass(func_class_obj, exp.Func):
        func_class = cast('type[exp.Func]', func_class_obj)
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
            supported=[*ARITHMETIC_OP_TO_SQLGLOT.keys(), JSQLOperator.CONCAT.value],
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


def jsql_join_to_sqlglot(join_spec: Any) -> dict[str, Any]:
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


def _build_ordered_expression(order_spec: dict[str, Any] | str) -> exp.Ordered:
    """Build ORDER BY expression for window function."""
    if isinstance(order_spec, dict):
        order_expr = jsql_expression_to_sqlglot(order_spec.get('field') or order_spec)
        desc = order_spec.get('direction', 'ASC').upper() == 'DESC'
    else:
        order_expr = jsql_expression_to_sqlglot(order_spec)
        desc = False
    return exp.Ordered(this=order_expr, desc=desc)


def _build_window_expression(field_spec: dict[str, Any]) -> exp.Window:
    """Build window expression from SELECT field spec."""
    over_spec = field_spec['over']
    field_spec_no_over = {k: v for k, v in field_spec.items() if k != 'over'}
    field_expr = jsql_expression_to_sqlglot(field_spec_no_over)

    partition_by = [jsql_expression_to_sqlglot(spec) for spec in over_spec.get('partition_by', [])]
    order_by_exprs = [_build_ordered_expression(spec) for spec in over_spec.get('order_by', [])]

    partition = exp.Partition(expressions=partition_by) if partition_by else None
    order = exp.Order(expressions=order_by_exprs) if order_by_exprs else None
    return exp.Window(this=field_expr, partition_by=partition, order=order)


def _apply_field_alias(field_expr: exp.Expression, field_spec: dict[str, Any] | str) -> exp.Expression:
    """Apply alias from field specification if provided."""
    if isinstance(field_spec, dict) and field_spec.get('alias'):
        return exp.alias_(field_expr, field_spec['alias'])
    return field_expr


def jsql_select_to_sqlglot(select_spec: list[dict[str, Any]] | None) -> list[exp.Expression]:
    """Convert JSQL SELECT to sqlglot expressions."""
    if not select_spec:
        return [exp.Star()]

    converted_fields: list[exp.Expression] = []
    for field_spec in select_spec:
        if 'over' in field_spec:
            field_expr = _build_window_expression(field_spec)
        else:
            field_expr = jsql_expression_to_sqlglot(field_spec)
        converted_fields.append(_apply_field_alias(field_expr, field_spec))

    return converted_fields if converted_fields else [exp.Star()]


def jsql_from_to_sqlglot(from_spec: str | dict[str, Any] | None) -> exp.Table:
    """Convert JSQL FROM to sqlglot table."""
    if not from_spec:
        raise MissingFieldError('from', path='from', context='SELECT query requires FROM clause')

    if isinstance(from_spec, str):
        return exp.table_(from_spec)
    from_dict = from_spec
    if 'entity' not in from_dict:
        raise MissingFieldError('entity', path='from.entity', context='FROM with alias')
    entity = from_dict['entity']
    alias = from_dict.get('alias')
    table_expr = exp.table_(entity)
    if alias:
        table_expr = exp.alias_(table_expr, alias, table=True)
    return table_expr


def _build_ctes(with_clause: list[dict[str, Any]]) -> list[exp.CTE]:
    """Build CTE expressions from WITH clause."""
    ctes: list[exp.CTE] = []
    for cte_spec in with_clause:
        if 'name' not in cte_spec:
            raise MissingFieldError('name', path='with[].name', context='CTE must have name')
        if 'query' not in cte_spec:
            raise MissingFieldError('query', path='with[].query', context='CTE must have query')
        cte_query = jsql_query_to_sqlglot(cte_spec['query'])
        cte_alias = exp.TableAlias(this=exp.Identifier(this=cte_spec['name']))
        ctes.append(exp.CTE(this=cte_query, alias=cte_alias))
    return ctes


def _build_base_select(jsql: dict[str, Any]) -> exp.Select:
    """Build base SELECT with WITH/FROM parts."""
    with_spec = jsql.get('with')
    select_exprs = jsql_select_to_sqlglot(jsql.get('select', []))
    from_expr = jsql_from_to_sqlglot(jsql.get('from'))

    ctes = _build_ctes(with_spec) if with_spec else []
    query = exp.Select(expressions=select_exprs, with_=exp.With(expressions=ctes)) if ctes else exp.Select(
        expressions=select_exprs
    )
    return query.from_(from_expr)


def _apply_where_and_joins(query: exp.Select, jsql: dict[str, Any]) -> exp.Select:
    """Apply WHERE and JOIN sections."""
    if where_spec := jsql.get('where'):
        query = query.where(jsql_condition_to_sqlglot(where_spec))

    if joins := jsql.get('joins'):
        for join_spec in joins:
            join_expr = jsql_join_to_sqlglot(join_spec)
            query = query.join(
                join_expr['table'],
                on=join_expr.get('on'),
                join_type=join_expr.get('type', JoinType.INNER.value),
            )
    return query


def _apply_group_having(query: exp.Select, jsql: dict[str, Any]) -> exp.Select:
    """Apply GROUP BY and HAVING sections."""
    if group_by := jsql.get('group_by'):
        for group_expr_spec in group_by:
            group_expr = jsql_expression_to_sqlglot(group_expr_spec)
            if group_expr:
                query = query.group_by(group_expr)

    if having_spec := jsql.get('having'):
        query.args['having'] = exp.Having(this=jsql_condition_to_sqlglot(having_spec))

    return query


def _apply_order_limit_offset(query: exp.Select, jsql: dict[str, Any]) -> exp.Select:
    """Apply ORDER BY, LIMIT and OFFSET sections."""
    if order_by := jsql.get('order_by'):
        order_exprs: list[exp.Expression] = []
        for order_spec in order_by:
            if isinstance(order_spec, dict):
                order_expr = jsql_expression_to_sqlglot(order_spec.get('field') or order_spec)
                direction = order_spec.get('direction', OrderDirection.ASC.value).upper()
            else:
                order_expr = jsql_expression_to_sqlglot(order_spec)
                direction = OrderDirection.ASC.value.upper()

            if order_expr:
                if direction == OrderDirection.DESC.value.upper():
                    order_exprs.append(exp.Ordered(this=order_expr, desc=True))
                else:
                    order_exprs.append(order_expr)

        if order_exprs:
            query = query.order_by(*order_exprs)

    if limit := jsql.get('limit'):
        query.args['limit'] = exp.Limit(expression=exp.Literal.number(limit))

    if offset := jsql.get('offset'):
        query.args['offset'] = exp.Offset(expression=exp.Literal.number(offset))

    return query


def jsql_query_to_sqlglot(jsql: dict[str, Any]) -> exp.Select:
    """
    Convert JSQL query dictionary to sqlglot Select expression.

    This is the core conversion function that builds the SQL query structure.
    """
    query = _build_base_select(jsql)
    query = _apply_where_and_joins(query, jsql)
    query = _apply_group_having(query, jsql)
    return _apply_order_limit_offset(query, jsql)


def jsql_query_to_sqlglot_select(query_spec: dict[str, Any]) -> exp.Select:
    """
    Convert JSQL query to sqlglot Select expression.

    Helper function for EXISTS and NOT EXISTS subqueries.
    """
    return jsql_query_to_sqlglot(query_spec)
