"""JSQL to SQL and SQL to JSQL converters."""

import logging
from typing import Any

import sqlglot
import sqlglot.expressions as exp
import sqlparse

from namerec.uma.jsql.constants import LOGICAL_OPERATORS
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.conversion_exceptions import UnsupportedOperationError
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery

__all__ = ['jsql_to_sql', 'sql_to_jsql']

logger = logging.getLogger(__name__)


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
    logger.info(f"Converting JSQL to SQL, dialect={dialect}")
    logger.debug(f"Input JSQL: {jsql}")
    
    try:
        # Build SELECT expression
        select_exprs = _jsql_select_to_sqlglot(jsql.get('select', []))

        # Build FROM expression (will raise exception if invalid/missing)
        from_expr = _jsql_from_to_sqlglot(jsql.get('from'))

        # Start building query
        query = exp.Select(expressions=select_exprs)
        query = query.from_(from_expr)

        # Add WHERE clause
        if where_spec := jsql.get('where'):
            where_expr = _jsql_condition_to_sqlglot(where_spec)
            query = query.where(where_expr)

        # Add JOINs
        if joins := jsql.get('joins'):
            for join_spec in joins:
                join_expr = _jsql_join_to_sqlglot(join_spec)
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

        logger.info(f"Successfully converted JSQL to SQL ({dialect})")
        logger.debug(f"Generated SQL:\n{formatted_sql}")
        return formatted_sql

    except JSQLSyntaxError as e:
        logger.error(f"Failed to convert JSQL: {e}", exc_info=True)
        raise
    except Exception as e:
        logger.error(f"Unexpected error during JSQL conversion: {e}", exc_info=True)
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


def _jsql_from_to_sqlglot(from_spec: str | dict[str, Any] | None) -> exp.Table:
    """
    Convert JSQL FROM to sqlglot table.
    
    Raises:
        MissingFieldError: If FROM clause is missing or empty
        InvalidExpressionError: If FROM structure is invalid
    """
    if not from_spec:
        raise MissingFieldError('from', path='from', context='SELECT query requires FROM clause')

    if isinstance(from_spec, str):
        return exp.table_(from_spec)

    if isinstance(from_spec, dict):
        if 'entity' not in from_spec:
            raise MissingFieldError('entity', path='from.entity', context='FROM with alias')
        
        entity = from_spec['entity']
        alias = from_spec.get('alias')
        table = exp.table_(entity)
        if alias:
            table = exp.alias_(table, alias, table=True)
        return table

    raise InvalidExpressionError(
        message=f"FROM must be string or dict, got {type(from_spec).__name__}",
        path='from',
        expression={'type': type(from_spec).__name__, 'value': repr(from_spec)}
    )


def _jsql_expression_to_sqlglot(expr_spec: dict[str, Any] | str) -> exp.Expression:
    """
    Convert JSQL expression to sqlglot expression.
    
    Delegates to specialized functions based on expression type.
    
    Raises:
        InvalidExpressionError: If expression structure is invalid or unrecognized
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
        
        # If we get here, the dict doesn't match any known pattern
        raise InvalidExpressionError(
            message="Expression must contain one of: 'field', 'value', 'func', or 'op'",
            path='',
            expression=expr_spec
        )

    # If it's neither string nor dict
    raise InvalidExpressionError(
        message=f"Expression must be string or dict, got {type(expr_spec).__name__}",
        path='',
        expression={'type': type(expr_spec).__name__, 'value': repr(expr_spec)}
    )


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
    logger.debug(f"Converting function call: {func}")
    
    # Single pass: convert and filter None values in one comprehension
    args = [
        converted
        for arg in expr_spec.get('args', [])
        if (converted := _jsql_expression_to_sqlglot(arg)) is not None
    ]
    logger.debug(f"Function {func} has {len(args)} arguments")
    
    # Try to find specific sqlglot function class
    func_class = getattr(exp, func.upper(), None)
    if func_class and issubclass(func_class, exp.Func):
        logger.debug(f"Using specific sqlglot function: {func_class.__name__}")
        return func_class(*args)
    
    # Fallback to generic Anonymous function
    logger.debug(f"Using Anonymous function for: {func}")
    return exp.Anonymous(this=func, expressions=args)


def _convert_arithmetic_op(expr_spec: dict[str, Any]) -> exp.Expression:
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
    
    left = _jsql_expression_to_sqlglot(expr_spec['left'])
    right = _jsql_expression_to_sqlglot(expr_spec['right'])
    
    # Use mapping instead of match statement
    operator_class = ARITHMETIC_OP_TO_SQLGLOT.get(op)
    if not operator_class:
        raise UnknownOperatorError(
            operator=op,
            path='op',
            supported=list(ARITHMETIC_OP_TO_SQLGLOT.keys())
        )
    
    return operator_class(this=left, expression=right)


def _jsql_condition_to_sqlglot(cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert JSQL condition to sqlglot expression.
    
    Delegates to specialized functions based on operator type.
    
    Raises:
        InvalidExpressionError: If condition structure is invalid
        MissingFieldError: If required operator field is missing
    """
    if not isinstance(cond_spec, dict):
        raise InvalidExpressionError(
            message=f"Condition must be a dict, got {type(cond_spec).__name__}",
            path='',
            expression={'type': type(cond_spec).__name__, 'value': repr(cond_spec)}
        )

    if 'op' not in cond_spec:
        raise MissingFieldError('op', path='op', context='condition')
    
    op = cond_spec['op']

    # Try logical operators first (use constant set)
    if op in LOGICAL_OPERATORS:
        return _convert_logical_operator(op, cond_spec)

    # Then try comparison operators
    return _convert_comparison_operator(op, cond_spec)


def _convert_logical_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert logical operator (AND/OR/NOT) to sqlglot expression.
    
    Raises:
        MissingFieldError: If required field is missing
        UnknownOperatorError: If operator is not supported
        InvalidExpressionError: If conditions list is empty
    """
    logger.debug(f"Converting logical operator: {op}")
    
    # Handle AND/OR operators
    if op in (JSQLOperator.AND.value, JSQLOperator.OR.value):
        if 'conditions' not in cond_spec:
            raise MissingFieldError('conditions', path='conditions', context=f'{op} operator')
        
        conditions = cond_spec['conditions']
        logger.debug(f"Processing {len(conditions)} conditions for {op}")
        
        if not conditions:
            raise InvalidExpressionError(
                message=f"{op} operator requires at least one condition",
                path='conditions'
            )
        
        result = _jsql_condition_to_sqlglot(conditions[0])
        operator_class = LOGICAL_OP_TO_SQLGLOT.get(op)
        if not operator_class:
            raise UnknownOperatorError(
                operator=op,
                path='op',
                supported=list(LOGICAL_OP_TO_SQLGLOT.keys())
            )
        
        for cond in conditions[1:]:
            next_cond = _jsql_condition_to_sqlglot(cond)
            result = operator_class(this=result, expression=next_cond)
        
        logger.debug(f"Built {op} expression with {len(conditions)} conditions")
        return result
    
    # Handle NOT operator
    if op == JSQLOperator.NOT.value:
        logger.debug("Processing NOT condition")
        if 'condition' not in cond_spec:
            raise MissingFieldError('condition', path='condition', context='NOT operator')
        
        condition = _jsql_condition_to_sqlglot(cond_spec['condition'])
        return exp.Not(this=condition)
    
    # Unknown logical operator
    logger.warning(f"Unknown logical operator: {op}")
    raise UnknownOperatorError(
        operator=op,
        path='op',
        supported=[JSQLOperator.AND.value, JSQLOperator.OR.value, JSQLOperator.NOT.value]
    )


def _convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert comparison operator to sqlglot expression.
    
    Raises:
        MissingFieldError: If required field is missing
        UnknownOperatorError: If operator is not supported
    """
    logger.debug(f"Converting comparison operator: {op}")
    
    # Get left side (field)
    if 'field' not in cond_spec:
        raise MissingFieldError('field', path='field', context='comparison operation')
    
    field_expr = _jsql_expression_to_sqlglot(cond_spec['field'])

    # Handle standard comparison operators using mapping
    operator_class = COMPARISON_OP_TO_SQLGLOT.get(op)
    if operator_class:
        # Get right side (value or field)
        right_expr = None
        if 'value' in cond_spec:
            right_expr = _jsql_expression_to_sqlglot({'value': cond_spec['value']})
        elif 'right_field' in cond_spec:
            right_expr = _jsql_expression_to_sqlglot(cond_spec['right_field'])
        else:
            raise MissingFieldError(
                'value or right_field',
                path='',
                context='comparison requires right operand'
            )
        
        return operator_class(this=field_expr, expression=right_expr)
    
    # Handle special operators
    if op == JSQLOperator.IN.value:
        if 'values' not in cond_spec:
            raise MissingFieldError('values', path='values', context='IN operator')
        values = cond_spec['values']
        logger.debug(f"Processing IN operator with {len(values)} values")
        val_exprs = [_jsql_expression_to_sqlglot({'value': v}) for v in values]
        return exp.In(this=field_expr, expressions=val_exprs)
    
    if op == JSQLOperator.LIKE.value:
        if 'pattern' not in cond_spec:
            raise MissingFieldError('pattern', path='pattern', context='LIKE operator')
        pattern = cond_spec['pattern']
        logger.debug(f"Processing LIKE operator with pattern: {pattern}")
        return exp.Like(this=field_expr, expression=exp.Literal.string(pattern))
    
    if op == JSQLOperator.IS_NULL.value:
        return exp.Is(this=field_expr, expression=exp.Null())
    
    if op == JSQLOperator.IS_NOT_NULL.value:
        return exp.Not(this=exp.Is(this=field_expr, expression=exp.Null()))
    
    # Unknown operator
    supported = list(COMPARISON_OP_TO_SQLGLOT.keys()) + [
        JSQLOperator.IN.value,
        JSQLOperator.LIKE.value,
        JSQLOperator.IS_NULL.value,
        JSQLOperator.IS_NOT_NULL.value,
    ]
    logger.warning(f"Unknown comparison operator: {op}")
    raise UnknownOperatorError(operator=op, path='op', supported=supported)


def _jsql_join_to_sqlglot(join_spec: dict[str, Any]) -> dict[str, Any]:
    """
    Convert JSQL JOIN to sqlglot join components.
    
    Raises:
        InvalidExpressionError: If JOIN structure is invalid
        MissingFieldError: If required entity field is missing
    """
    if not isinstance(join_spec, dict):
        logger.debug("Invalid join_spec type, expected dict")
        raise InvalidExpressionError(
            message=f"JOIN spec must be dict, got {type(join_spec).__name__}",
            path='joins',
            expression={'type': type(join_spec).__name__, 'value': repr(join_spec)}
        )

    if 'entity' not in join_spec:
        logger.debug("Missing 'entity' in join_spec")
        raise MissingFieldError('entity', path='joins.entity', context='JOIN')
    
    entity = join_spec['entity']
    join_type = join_spec.get('type', JoinType.INNER.value).upper()
    logger.debug(f"Processing {join_type} JOIN to {entity}")

    table = exp.table_(entity)
    if alias := join_spec.get('alias'):
        table = exp.alias_(table, alias, table=True)

    on_cond = None
    if on_spec := join_spec.get('on'):
        on_cond = _jsql_condition_to_sqlglot(on_spec)

    logger.debug(f"Built JOIN: {join_type} {entity}")
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
    logger.info(f"Converting SQL to JSQL, dialect={dialect}")
    logger.debug(f"Input SQL: {sql}")
    
    try:
        # Parse SQL with sqlglot
        # Use None for generic dialect
        read_dialect = None if dialect == 'generic' else dialect
        logger.debug(f"Parsing SQL with dialect: {read_dialect or 'generic'}")
        parsed = sqlglot.parse_one(sql, read=read_dialect)
        
        logger.debug(f"Parsed expression type: {type(parsed).__name__}")

        if not isinstance(parsed, exp.Select):
            logger.warning(f"Unsupported query type: {type(parsed).__name__}")
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
                select_fields.append(field_dict)
            if select_fields:
                jsql['select'] = select_fields

        # WHERE clause
        if parsed.args.get('where'):
            where_expr = parsed.args['where'].this
            where_jsql = _convert_condition_to_jsql(where_expr)
            jsql['where'] = where_jsql

        # JOIN clauses
        if parsed.args.get('joins'):
            joins = []
            for join in parsed.args['joins']:
                join_dict = _convert_join_to_jsql(join)
                joins.append(join_dict)
            if joins:
                jsql['joins'] = joins

        # ORDER BY clause
        if parsed.args.get('order'):
            order_by = []
            for order_expr in parsed.args['order'].expressions:
                order_dict = _convert_order_to_jsql(order_expr)
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
                group_by.append(field_dict)
            if group_by:
                jsql['group_by'] = group_by

        # HAVING clause
        if parsed.args.get('having'):
            having_expr = parsed.args['having'].this
            having_jsql = _convert_condition_to_jsql(having_expr)
            jsql['having'] = having_jsql

        logger.info("Successfully converted SQL to JSQL")
        logger.debug(f"Generated JSQL: {jsql}")
        return jsql

    except JSQLSyntaxError as e:
        logger.error(f"Failed to parse SQL: {e}", exc_info=True)
        raise
    except Exception as e:
        logger.error(f"Unexpected error during SQL parsing: {e}", exc_info=True)
        raise JSQLSyntaxError(
            message=f'Failed to parse SQL: {e!s}',
            path='',
        ) from e


def _convert_expression_to_jsql(expr: exp.Expression) -> JSQLExpression:
    """
    Convert sqlglot expression to JSQL expression.
    
    Raises:
        InvalidExpressionError: If expression structure is invalid
        UnknownOperatorError: If arithmetic operator is not supported
        UnsupportedOperationError: If expression type is not supported
    """
    # Handle aliased expressions
    if isinstance(expr, exp.Alias):
        base_expr = _convert_expression_to_jsql(expr.this)
        base_expr['alias'] = expr.alias
        return base_expr

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
        # After error handling fix, None won't be returned, but we still filter
        # for cases where conversion might fail (shouldn't happen, but defensive)
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
        if not op:
            raise UnknownOperatorError(
                operator=type(expr).__name__,
                path='',
                supported=list(SQLGLOT_TO_ARITHMETIC.values())
            )
        
        return {
            'op': op,
            'left': _convert_expression_to_jsql(expr.left),
            'right': _convert_expression_to_jsql(expr.right),
        }

    # Default: unsupported expression type
    raise UnsupportedOperationError(
        operation=f"Expression type: {type(expr).__name__}",
        reason="This expression type is not supported in JSQL conversion",
        path=''
    )


def _convert_condition_to_jsql(expr: exp.Expression) -> dict[str, Any]:
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
            raise UnknownOperatorError(
                operator=type(expr).__name__,
                path='',
                supported=list(SQLGLOT_TO_COMPARISON.values())
            )
        
        left = _convert_expression_to_jsql(expr.left)
        right = _convert_expression_to_jsql(expr.right)

        if 'field' not in left:
            raise InvalidExpressionError(
                message="Comparison operator left side must be a field reference",
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
                message="Comparison operator right side must be a value or field reference",
                path='right',
                expression=right
            )
        
        return result

    # Handle IN
    if isinstance(expr, exp.In):
        left = _convert_expression_to_jsql(expr.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message="IN operator left side must be a field reference",
                path='left',
                expression=left
            )
        
        values = []
        if isinstance(expr.expressions, list):
            for val_expr in expr.expressions:
                val = _convert_expression_to_jsql(val_expr)
                if 'value' not in val:
                    raise InvalidExpressionError(
                        message="IN operator values must be literals",
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
        left = _convert_expression_to_jsql(expr.this)
        right = _convert_expression_to_jsql(expr.expression)
        
        if 'field' not in left:
            raise InvalidExpressionError(
                message="LIKE operator left side must be a field reference",
                path='left',
                expression=left
            )
        
        if 'value' not in right:
            raise InvalidExpressionError(
                message="LIKE operator right side must be a string literal",
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
        left = _convert_expression_to_jsql(expr.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message="IS NULL operator left side must be a field reference",
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
            message="IS expression must be IS NULL",
            path='expression',
            expression={'type': type(expr.expression).__name__}
        )

    # Default: unsupported condition type
    raise UnsupportedOperationError(
        operation=f"Condition type: {type(expr).__name__}",
        reason="This condition type is not supported in JSQL conversion",
        path=''
    )


def _convert_join_to_jsql(join: exp.Join) -> dict[str, Any]:
    """
    Convert sqlglot JOIN to JSQL join.
    
    Raises:
        InvalidExpressionError: If JOIN structure is invalid
    """
    if not isinstance(join, exp.Join):
        raise InvalidExpressionError(
            message=f"Expected exp.Join, got {type(join).__name__}",
            path='joins',
            expression={'type': type(join).__name__}
        )

    # Get join type (use constant mapping)
    join_type = JoinType.INNER.value
    if join.side:
        join_type = SQLGLOT_JOIN_SIDE_TO_TYPE.get(join.side, JoinType.INNER.value)

    # Get joined table
    table = join.this
    if not isinstance(table, exp.Table):
        raise InvalidExpressionError(
            message=f"JOIN table must be exp.Table, got {type(table).__name__}",
            path='joins.this',
            expression={'type': type(table).__name__}
        )

    result: dict[str, Any] = {
        'type': join_type,
        'entity': table.name,
    }

    if table.alias:
        result['alias'] = table.alias

    # Get ON condition (optional, but if present must be valid)
    # Note: sqlglot stores ON condition in join.args['on']
    # The structure: join.args['on'] is typically a wrapper, and the actual condition is in .this
    if join.args.get('on'):
        on_arg = join.args['on']
        
        # Check if on_arg itself is already a condition (EQ, AND, etc.) or a wrapper
        # If it's a condition type we can handle, use it directly
        if isinstance(on_arg, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE, exp.And, exp.Or, exp.Not, exp.In, exp.Like, exp.Is)):
            on_expr = on_arg
        elif hasattr(on_arg, 'this') and on_arg.this is not None:
            on_expr = on_arg.this
        else:
            on_expr = on_arg
        
        logger.debug(f"ON condition type: {type(on_expr).__name__}, value: {on_expr}")
        
        # Convert ON condition - will raise exception if invalid
        # ON condition must be a boolean expression (EQ, AND, OR, etc.), not a simple Column
        on_jsql = _convert_condition_to_jsql(on_expr)
        result['on'] = on_jsql

    return result


def _convert_order_to_jsql(order_expr: exp.Ordered) -> dict[str, Any]:
    """
    Convert sqlglot ORDER BY expression to JSQL order.
    
    Raises:
        InvalidExpressionError: If ORDER BY structure is invalid
    """
    if not isinstance(order_expr, exp.Ordered):
        raise InvalidExpressionError(
            message=f"Expected exp.Ordered, got {type(order_expr).__name__}",
            path='order_by',
            expression={'type': type(order_expr).__name__}
        )

    field_expr = _convert_expression_to_jsql(order_expr.this)
    result = field_expr.copy()
    # Use OrderDirection constant and uppercase for consistency
    direction = OrderDirection.DESC if order_expr.args.get('desc') else OrderDirection.ASC
    result['direction'] = direction.value.upper()

    return result
