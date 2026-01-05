"""JSQL to SQL and SQL to JSQL converters."""

from typing import Any

import sqlglot
import sqlglot.expressions as exp
import sqlparse

from namerec.uma.jsql.constants import LOGICAL_OPERATORS
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery

__all__ = ['jsql_to_sql', 'sql_to_jsql']


# Operator mappings for JSQL -> SQL conversion
ARITHMETIC_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.ADD.value: exp.Add,
    JSQLOperator.SUB.value: exp.Sub,
    JSQLOperator.MUL.value: exp.Mul,
    JSQLOperator.DIV.value: exp.Div,
}

COMPARISON_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.EQ.value: exp.EQ,
    JSQLOperator.NE.value: exp.NEQ,
    JSQLOperator.GT.value: exp.GT,
    JSQLOperator.GE.value: exp.GTE,
    JSQLOperator.LT.value: exp.LT,
    JSQLOperator.LE.value: exp.LTE,
}

LOGICAL_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.AND.value: exp.And,
    JSQLOperator.OR.value: exp.Or,
}

# Reverse mappings for SQL -> JSQL conversion
SQLGLOT_TO_ARITHMETIC: dict[type[exp.Expression], str] = {
    exp.Add: JSQLOperator.ADD.value,
    exp.Sub: JSQLOperator.SUB.value,
    exp.Mul: JSQLOperator.MUL.value,
    exp.Div: JSQLOperator.DIV.value,
}

SQLGLOT_TO_COMPARISON: dict[type[exp.Expression], str] = {
    exp.EQ: JSQLOperator.EQ.value,
    exp.NEQ: JSQLOperator.NE.value,
    exp.GT: JSQLOperator.GT.value,
    exp.GTE: JSQLOperator.GE.value,
    exp.LT: JSQLOperator.LT.value,
    exp.LTE: JSQLOperator.LE.value,
}

# Join type mapping for SQL -> JSQL
SQLGLOT_JOIN_SIDE_TO_TYPE: dict[str, str] = {
    'LEFT': JoinType.LEFT.value,
    'RIGHT': JoinType.RIGHT.value,
    'FULL': JoinType.FULL.value,
}


def jsql_to_sql(jsql: JSQLQuery, dialect: str = 'generic') -> str:
    """
    Convert JSQL query to SQL string.

    This function directly converts JSQL to SQL using sqlglot expressions,
    without requiring database metadata or connection.

    Args:
        jsql: JSQL query dictionary
        dialect: SQL dialect to generate (generic, postgresql, mysql, sqlite, etc.)

    Returns:
        Formatted SQL string

    Raises:
        JSQLSyntaxError: If JSQL syntax is invalid

    Example:
        >>> jsql = {
        ...     "from": "users",
        ...     "select": [{"field": "id"}, {"field": "name"}],
        ...     "where": {"field": "active", "op": "=", "value": True}
        ... }
        >>> sql = jsql_to_sql(jsql)
        >>> print(sql)
        SELECT users.id,
               users.name
        FROM users
        WHERE users.active = TRUE
    """
    try:
        # Build SELECT expression
        select_exprs = _jsql_select_to_sqlglot(jsql.get('select', []))

        # Build FROM expression
        from_expr = _jsql_from_to_sqlglot(jsql.get('from'))
        if not from_expr:
            raise JSQLSyntaxError('Missing FROM clause', path='from')

        # Start building query
        query = exp.Select(expressions=select_exprs)
        query = query.from_(from_expr)

        # Add WHERE clause
        if where_spec := jsql.get('where'):
            where_expr = _jsql_condition_to_sqlglot(where_spec)
            if where_expr:
                query = query.where(where_expr)

        # Add JOINs
        if joins := jsql.get('joins'):
            for join_spec in joins:
                join_expr = _jsql_join_to_sqlglot(join_spec)
                if join_expr:
                    query = query.join(
                        join_expr['table'],
                        on=join_expr.get('on'),
                        join_type=join_expr.get('type', JoinType.INNER.value),
                    )

        # Add GROUP BY
        if group_by := jsql.get('group_by'):
            for group_expr_spec in group_by:
                group_expr = _jsql_expression_to_sqlglot(group_expr_spec)
                if group_expr:
                    query = query.group_by(group_expr)

        # Add HAVING
        if having_spec := jsql.get('having'):
            having_expr = _jsql_condition_to_sqlglot(having_spec)
            if having_expr:
                query.args['having'] = exp.Having(this=having_expr)

        # Add ORDER BY
        if order_by := jsql.get('order_by'):
            for order_spec in order_by:
                order_expr = _jsql_expression_to_sqlglot(order_spec.get('field') or order_spec)
                if order_expr:
                    direction = order_spec.get('direction', OrderDirection.ASC.value).upper()
                    desc = direction == OrderDirection.DESC.value.upper()
                    query = query.order_by(order_expr, desc=desc)

        # Add LIMIT
        if limit := jsql.get('limit'):
            query.args['limit'] = exp.Limit(expression=exp.Literal.number(limit))

        # Add OFFSET
        if offset := jsql.get('offset'):
            query.args['offset'] = exp.Offset(expression=exp.Literal.number(offset))

        # Generate SQL
        sql_str = query.sql(dialect=dialect if dialect != 'generic' else None, pretty=True)

        # Format for readability
        formatted_sql = sqlparse.format(
            sql_str,
            reindent=True,
            keyword_case='upper',
        )

        return formatted_sql

    except JSQLSyntaxError:
        raise
    except Exception as e:
        raise JSQLSyntaxError(
            message=f'Failed to convert JSQL to SQL: {e!s}',
            path='',
        ) from e


def _jsql_select_to_sqlglot(select_spec: list[dict[str, Any]] | None) -> list[exp.Expression]:
    """Convert JSQL SELECT to sqlglot expressions."""
    if not select_spec:
        return [exp.Star()]

    result = []
    for field_spec in select_spec:
        field_expr = _jsql_expression_to_sqlglot(field_spec)
        if field_expr:
            alias = field_spec.get('alias')
            if alias:
                field_expr = exp.alias_(field_expr, alias)
            result.append(field_expr)

    return result if result else [exp.Star()]


def _jsql_from_to_sqlglot(from_spec: str | dict[str, Any] | None) -> exp.Table | None:
    """Convert JSQL FROM to sqlglot table."""
    if not from_spec:
        return None

    if isinstance(from_spec, str):
        return exp.table_(from_spec)

    if isinstance(from_spec, dict):
        entity = from_spec.get('entity')
        alias = from_spec.get('alias')
        if entity:
            table = exp.table_(entity)
            if alias:
                table = exp.alias_(table, alias, table=True)
            return table

    return None


def _jsql_expression_to_sqlglot(expr_spec: dict[str, Any] | str) -> exp.Expression | None:
    """
    Convert JSQL expression to sqlglot expression.
    
    Delegates to specialized functions based on expression type.
    """
    if isinstance(expr_spec, str):
        return _convert_field_reference(expr_spec)

    if isinstance(expr_spec, dict):
        # Try each expression type in order
        if 'field' in expr_spec:
            return _convert_field_reference(expr_spec['field'])
        if 'value' in expr_spec:
            return _convert_literal_value(expr_spec['value'])
        if 'func' in expr_spec:
            return _convert_function_call(expr_spec)
        if 'op' in expr_spec:
            return _convert_arithmetic_op(expr_spec)

    return None


def _convert_field_reference(field: str) -> exp.Expression:
    """Convert field reference to sqlglot column expression."""
    if field == '*':
        return exp.Star()
    if '.' in field:
        table, column = field.split('.', 1)
        return exp.column(column, table=table)
    return exp.column(field)


def _convert_literal_value(value: Any) -> exp.Expression:  # noqa: ANN401
    """Convert literal value to sqlglot expression."""
    match value:
        case bool():
            return exp.Boolean(this=value)
        case int() | float():
            return exp.Literal.number(value)
        case _:
            return exp.Literal.string(str(value))


def _convert_function_call(expr_spec: dict[str, Any]) -> exp.Expression:
    """Convert function call to sqlglot expression."""
    func = expr_spec['func']
    # Single pass: convert and filter None values in one comprehension
    args = [
        converted
        for arg in expr_spec.get('args', [])
        if (converted := _jsql_expression_to_sqlglot(arg)) is not None
    ]
    
    # Try to find specific sqlglot function class
    func_class = getattr(exp, func.upper(), None)
    if func_class and issubclass(func_class, exp.Func):
        return func_class(*args)
    
    # Fallback to generic Anonymous function
    return exp.Anonymous(this=func, expressions=args)


def _convert_arithmetic_op(expr_spec: dict[str, Any]) -> exp.Expression | None:
    """Convert arithmetic operation to sqlglot expression."""
    op = expr_spec['op']
    left = _jsql_expression_to_sqlglot(expr_spec.get('left'))
    right = _jsql_expression_to_sqlglot(expr_spec.get('right'))
    
    if not left or not right:
        return None
    
    # Use mapping instead of match statement
    operator_class = ARITHMETIC_OP_TO_SQLGLOT.get(op)
    if operator_class:
        return operator_class(this=left, expression=right)
    
    return None


def _jsql_condition_to_sqlglot(cond_spec: dict[str, Any]) -> exp.Expression | None:
    """
    Convert JSQL condition to sqlglot expression.
    
    Delegates to specialized functions based on operator type.
    """
    if not isinstance(cond_spec, dict):
        return None

    op = cond_spec.get('op')
    if not op:
        return None

    # Try logical operators first (use constant set)
    if op in LOGICAL_OPERATORS:
        return _convert_logical_operator(op, cond_spec)

    # Then try comparison operators
    return _convert_comparison_operator(op, cond_spec)


def _convert_logical_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression | None:
    """Convert logical operator (AND/OR/NOT) to sqlglot expression."""
    # Handle AND/OR operators
    if op in (JSQLOperator.AND.value, JSQLOperator.OR.value):
        conditions = cond_spec.get('conditions', [])
        if not conditions:
            return None
        
        result = _jsql_condition_to_sqlglot(conditions[0])
        operator_class = LOGICAL_OP_TO_SQLGLOT.get(op)
        if not operator_class:
            return None
        
        for cond in conditions[1:]:
            next_cond = _jsql_condition_to_sqlglot(cond)
            if result and next_cond:
                result = operator_class(this=result, expression=next_cond)
        return result
    
    # Handle NOT operator
    if op == JSQLOperator.NOT.value:
        condition = _jsql_condition_to_sqlglot(cond_spec.get('condition', {}))
        return exp.Not(this=condition) if condition else None
    
    return None


def _convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression | None:
    """Convert comparison operator to sqlglot expression."""
    # Get left side (field)
    field_expr = _jsql_expression_to_sqlglot(cond_spec.get('field', ''))
    if not field_expr:
        return None

    # Get right side (value or field)
    right_expr = None
    if 'value' in cond_spec:
        right_expr = _jsql_expression_to_sqlglot({'value': cond_spec['value']})
    elif 'right_field' in cond_spec:
        right_expr = _jsql_expression_to_sqlglot(cond_spec['right_field'])

    # Handle standard comparison operators using mapping
    operator_class = COMPARISON_OP_TO_SQLGLOT.get(op)
    if operator_class and right_expr:
        return operator_class(this=field_expr, expression=right_expr)
    
    # Handle special operators
    if op == JSQLOperator.IN.value:
        values = cond_spec.get('values', [])
        val_exprs = [_jsql_expression_to_sqlglot({'value': v}) for v in values]
        return exp.In(this=field_expr, expressions=val_exprs)
    
    if op == JSQLOperator.LIKE.value:
        pattern = cond_spec.get('pattern', '')
        return exp.Like(this=field_expr, expression=exp.Literal.string(pattern))
    
    if op == JSQLOperator.IS_NULL.value:
        return exp.Is(this=field_expr, expression=exp.Null())
    
    if op == JSQLOperator.IS_NOT_NULL.value:
        return exp.Not(this=exp.Is(this=field_expr, expression=exp.Null()))
    
    return None


def _jsql_join_to_sqlglot(join_spec: dict[str, Any]) -> dict[str, Any] | None:
    """Convert JSQL JOIN to sqlglot join components."""
    if not isinstance(join_spec, dict):
        return None

    entity = join_spec.get('entity')
    if not entity:
        return None

    table = exp.table_(entity)
    if alias := join_spec.get('alias'):
        table = exp.alias_(table, alias, table=True)

    on_cond = None
    if on_spec := join_spec.get('on'):
        on_cond = _jsql_condition_to_sqlglot(on_spec)

    join_type = join_spec.get('type', JoinType.INNER.value).upper()

    return {
        'table': table,
        'on': on_cond,
        'type': join_type,
    }


def sql_to_jsql(sql: str, dialect: str = 'generic') -> JSQLQuery:
    """
    Convert SQL query to JSQL dictionary.

    This function parses SQL using sqlglot and converts it to JSQL format.
    Currently supports SELECT queries with WHERE, JOIN, ORDER BY, LIMIT, OFFSET.

    Args:
        sql: SQL query string
        dialect: SQL dialect to parse (generic, postgresql, mysql, sqlite, etc.)

    Returns:
        JSQL query dictionary

    Raises:
        JSQLSyntaxError: If SQL cannot be parsed or converted

    Example:
        >>> sql = "SELECT id, name FROM users WHERE active = 1 ORDER BY name LIMIT 10"
        >>> jsql = sql_to_jsql(sql)
        >>> print(jsql)
        {
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'where': {'field': 'active', 'op': '=', 'value': 1},
            'order_by': [{'field': 'name', 'direction': 'ASC'}],
            'limit': 10
        }
    """
    try:
        # Parse SQL with sqlglot
        # Use None for generic dialect
        read_dialect = None if dialect == 'generic' else dialect
        parsed = sqlglot.parse_one(sql, read=read_dialect)

        if not isinstance(parsed, exp.Select):
            raise JSQLSyntaxError(
                message='Only SELECT queries are supported',
                path='',
            )

        # Convert to JSQL
        jsql: JSQLQuery = {}

        # FROM clause
        if from_expr := parsed.args.get('from_'):
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
        if parsed.expressions:
            select_fields = []
            for expr in parsed.expressions:
                field_dict = _convert_expression_to_jsql(expr)
                if field_dict:
                    select_fields.append(field_dict)
            if select_fields:
                jsql['select'] = select_fields

        # WHERE clause
        if parsed.args.get('where'):
            where_expr = parsed.args['where'].this
            where_jsql = _convert_condition_to_jsql(where_expr)
            if where_jsql:
                jsql['where'] = where_jsql

        # JOIN clauses
        if parsed.args.get('joins'):
            joins = []
            for join in parsed.args['joins']:
                join_dict = _convert_join_to_jsql(join)
                if join_dict:
                    joins.append(join_dict)
            if joins:
                jsql['joins'] = joins

        # ORDER BY clause
        if parsed.args.get('order'):
            order_by = []
            for order_expr in parsed.args['order'].expressions:
                order_dict = _convert_order_to_jsql(order_expr)
                if order_dict:
                    order_by.append(order_dict)
            if order_by:
                jsql['order_by'] = order_by

        # LIMIT clause
        if parsed.args.get('limit'):
            limit_expr = parsed.args['limit'].expression
            if isinstance(limit_expr, exp.Literal):
                jsql['limit'] = int(limit_expr.this)

        # OFFSET clause
        if parsed.args.get('offset'):
            offset_expr = parsed.args['offset'].expression
            if isinstance(offset_expr, exp.Literal):
                jsql['offset'] = int(offset_expr.this)

        # GROUP BY clause
        if parsed.args.get('group'):
            group_by = []
            for group_expr in parsed.args['group'].expressions:
                field_dict = _convert_expression_to_jsql(group_expr)
                if field_dict:
                    group_by.append(field_dict)
            if group_by:
                jsql['group_by'] = group_by

        # HAVING clause
        if parsed.args.get('having'):
            having_expr = parsed.args['having'].this
            having_jsql = _convert_condition_to_jsql(having_expr)
            if having_jsql:
                jsql['having'] = having_jsql

        return jsql

    except JSQLSyntaxError:
        raise
    except Exception as e:
        raise JSQLSyntaxError(
            message=f'Failed to parse SQL: {e!s}',
            path='',
        ) from e


def _convert_expression_to_jsql(expr: exp.Expression) -> JSQLExpression | None:
    """Convert sqlglot expression to JSQL expression."""
    # Handle aliased expressions
    if isinstance(expr, exp.Alias):
        base_expr = _convert_expression_to_jsql(expr.this)
        if base_expr:
            base_expr['alias'] = expr.alias
            return base_expr
        return None

    # Handle column references
    if isinstance(expr, exp.Column):
        result: JSQLExpression = {'field': expr.name}
        if expr.table:
            result['field'] = f'{expr.table}.{expr.name}'
        return result

    # Handle literals
    if isinstance(expr, exp.Literal):
        return {'value': expr.this}

    # Handle star (SELECT *)
    if isinstance(expr, exp.Star):
        return {'field': '*'}

    # Handle functions
    if isinstance(expr, exp.Func):
        func_name = expr.sql_name()
        # Single pass: type check, convert, and filter in one comprehension
        args = [
            converted
            for arg in expr.args.values()
            if isinstance(arg, exp.Expression) and (converted := _convert_expression_to_jsql(arg)) is not None
        ]

        return {
            'func': func_name,
            'args': args,
        }

    # Handle binary operations (use reverse mapping)
    if isinstance(expr, (exp.Add, exp.Sub, exp.Mul, exp.Div)):
        op = SQLGLOT_TO_ARITHMETIC.get(type(expr))
        if op:
            return {
                'op': op,
                'left': _convert_expression_to_jsql(expr.left),
                'right': _convert_expression_to_jsql(expr.right),
            }

    # Default: return None for unsupported expressions
    return None


def _convert_condition_to_jsql(expr: exp.Expression) -> dict[str, Any] | None:
    """Convert sqlglot condition to JSQL condition."""
    # Handle AND/OR
    if isinstance(expr, exp.And):
        return {
            'op': JSQLOperator.AND.value,
            'conditions': [
                _convert_condition_to_jsql(expr.left),
                _convert_condition_to_jsql(expr.right),
            ],
        }

    if isinstance(expr, exp.Or):
        return {
            'op': JSQLOperator.OR.value,
            'conditions': [
                _convert_condition_to_jsql(expr.left),
                _convert_condition_to_jsql(expr.right),
            ],
        }

    # Handle NOT
    if isinstance(expr, exp.Not):
        return {
            'op': JSQLOperator.NOT.value,
            'condition': _convert_condition_to_jsql(expr.this),
        }

    # Handle comparison operators (use reverse mapping)
    if isinstance(expr, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE)):
        op = SQLGLOT_TO_COMPARISON.get(type(expr))
        if not op:
            return None
        
        left = _convert_expression_to_jsql(expr.left)
        right = _convert_expression_to_jsql(expr.right)

        if left and 'field' in left and right:
            result: dict[str, Any] = {
                'field': left['field'],
                'op': op,
            }
            if 'value' in right:
                result['value'] = right['value']
            elif 'field' in right:
                result['right_field'] = right['field']
            return result

    # Handle IN
    if isinstance(expr, exp.In):
        left = _convert_expression_to_jsql(expr.this)
        if left and 'field' in left:
            values = []
            if isinstance(expr.expressions, list):
                for val_expr in expr.expressions:
                    val = _convert_expression_to_jsql(val_expr)
                    if val and 'value' in val:
                        values.append(val['value'])

            return {
                'field': left['field'],
                'op': JSQLOperator.IN.value,
                'values': values,
            }

    # Handle LIKE
    if isinstance(expr, exp.Like):
        left = _convert_expression_to_jsql(expr.this)
        right = _convert_expression_to_jsql(expr.expression)
        if left and 'field' in left and right and 'value' in right:
            return {
                'field': left['field'],
                'op': JSQLOperator.LIKE.value,
                'pattern': right['value'],
            }

    # Handle IS NULL / IS NOT NULL
    if isinstance(expr, exp.Is):
        left = _convert_expression_to_jsql(expr.this)
        if left and 'field' in left:
            if isinstance(expr.expression, exp.Null):
                return {
                    'field': left['field'],
                    'op': JSQLOperator.IS_NULL.value,
                }

    # Default: return None
    return None


def _convert_join_to_jsql(join: exp.Join) -> dict[str, Any] | None:
    """Convert sqlglot JOIN to JSQL join."""
    if not isinstance(join, exp.Join):
        return None

    # Get join type (use constant mapping)
    join_type = JoinType.INNER.value
    if join.side:
        join_type = SQLGLOT_JOIN_SIDE_TO_TYPE.get(join.side, JoinType.INNER.value)

    # Get joined table
    table = join.this
    if not isinstance(table, exp.Table):
        return None

    result: dict[str, Any] = {
        'type': join_type,
        'entity': table.name,
    }

    if table.alias:
        result['alias'] = table.alias

    # Get ON condition
    if join.args.get('on'):
        on_expr = join.args['on'].this
        on_jsql = _convert_condition_to_jsql(on_expr)
        if on_jsql:
            result['on'] = on_jsql

    return result


def _convert_order_to_jsql(order_expr: exp.Ordered) -> dict[str, Any] | None:
    """Convert sqlglot ORDER BY expression to JSQL order."""
    if not isinstance(order_expr, exp.Ordered):
        return None

    field_expr = _convert_expression_to_jsql(order_expr.this)
    if not field_expr:
        return None

    result = field_expr.copy()
    # Use OrderDirection constant and uppercase for consistency
    direction = OrderDirection.DESC if order_expr.args.get('desc') else OrderDirection.ASC
    result['direction'] = direction.value.upper()

    return result
