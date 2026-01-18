"""Expression handling for JSQL <-> SQL conversion."""

import logging
from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.conversion_exceptions import UnknownOperatorError
from namerec.uma.jsql.conversion_exceptions import UnsupportedOperationError
from namerec.uma.jsql.converter.operators import ARITHMETIC_OP_TO_SQLGLOT
from namerec.uma.jsql.converter.operators import SQLGLOT_TO_ARITHMETIC
from namerec.uma.jsql.types import JSQLExpression

logger = logging.getLogger(__name__)


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
        # Try each expression type in order
        if 'field' in expr_spec:
            return convert_field_reference(expr_spec['field'])
        if 'value' in expr_spec:
            return convert_literal_value(expr_spec['value'])
        if 'func' in expr_spec:
            return convert_function_call(expr_spec)
        if 'op' in expr_spec:
            return convert_arithmetic_op(expr_spec)
        # Check for subquery (full query dictionary)
        if 'from' in expr_spec and 'select' in expr_spec:
            # This looks like a subquery - convert it and wrap in Subquery
            from namerec.uma.jsql.converter.subqueries import jsql_query_to_sqlglot_select
            subquery_select = jsql_query_to_sqlglot_select(expr_spec)
            return exp.Subquery(this=subquery_select)

        # If we get here, the dict doesn't match any known pattern
        raise InvalidExpressionError(
            message="Expression must contain one of: 'field', 'value', 'func', 'op', or subquery ('from')",
            path='',
            expression=expr_spec
        )

    # If it's neither string nor dict
    raise InvalidExpressionError(
        message=f'Expression must be string or dict, got {type(expr_spec).__name__}',
        path='',
        expression={'type': type(expr_spec).__name__, 'value': repr(expr_spec)}
    )


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

    # Single pass: convert and filter None values in one comprehension
    args = [
        converted
        for arg in expr_spec.get('args', [])
        if (converted := jsql_expression_to_sqlglot(arg)) is not None
    ]
    logger.debug(f'Function {func} has {len(args)} arguments')

    # Handle DISTINCT flag (for COUNT(DISTINCT ...), SUM(DISTINCT ...), etc.)
    # In sqlglot, DISTINCT is represented as Distinct wrapping the first argument
    distinct = expr_spec.get('distinct', False)
    if distinct and args:
        # Wrap the first argument in Distinct
        # In sqlglot, Distinct uses 'expressions' list
        args[0] = exp.Distinct(expressions=[args[0]])

    # Try to find specific sqlglot function class
    func_class = getattr(exp, func.upper(), None)
    if func_class and issubclass(func_class, exp.Func):
        logger.debug(f'Using specific sqlglot function: {func_class.__name__}')
        # For functions with DISTINCT, pass the Distinct-wrapped argument as 'this'
        if distinct and args:
            # In sqlglot, Count(DISTINCT x) is Count(this=Distinct(...))
            return func_class(this=args[0], expressions=args[1:])
        return func_class(*args)

    # Fallback to generic Anonymous function
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

    # Handle string concatenation operator ||
    from namerec.uma.jsql.constants import JSQLOperator
    if op == JSQLOperator.CONCAT.value:
        # Use Concat function for string concatenation (works across dialects)
        return exp.Concat(expressions=[left, right])

    # Use mapping instead of match statement
    operator_class = ARITHMETIC_OP_TO_SQLGLOT.get(op)
    if not operator_class:
        raise UnknownOperatorError(
            operator=op,
            path='op',
            supported=list(ARITHMETIC_OP_TO_SQLGLOT.keys()) + [JSQLOperator.CONCAT.value]
        )

    return operator_class(this=left, expression=right)


def _extract_distinct_expression(expr: exp.Distinct) -> tuple[JSQLExpression, bool]:
    """
    Extract expression from DISTINCT wrapper and return (jsql, is_distinct).
    
    This function centralizes DISTINCT handling logic, which was previously
    duplicated in multiple places. It handles both 'expressions' list and 'this'
    attributes of exp.Distinct.
    
    Args:
        expr: SQLGlot Distinct expression
        
    Returns:
        Tuple of (jsql_expression, is_distinct_flag)
        
    Raises:
        InvalidExpressionError: If DISTINCT doesn't wrap an expression
    """
    # In sqlglot, Distinct uses 'expressions' list, not 'this'
    # Get the first expression (typically there's only one in COUNT(DISTINCT ...))
    if expr.expressions and len(expr.expressions) > 0:
        inner_expr = expr.expressions[0]
    else:
        # Fallback: check 'this' (might be used in some cases)
        inner_expr = expr.this
        if inner_expr is None:
            raise InvalidExpressionError(
                message='DISTINCT must wrap an expression',
                path='',
                expression={'type': 'Distinct', 'expressions': expr.expressions, 'this': None}
            )
    
    inner_jsql = convert_expression_to_jsql(inner_expr)
    
    # Mark as distinct if it's a dict (will be handled by caller)
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
    # Handle aliased expressions
    if isinstance(expr, exp.Alias):
        base_expr = convert_expression_to_jsql(expr.this)
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
        value = expr.this
        # Preserve numeric types - sqlglot stores numbers as strings in expr.this
        if expr.is_int:
            value = int(value)
        elif expr.is_number:
            value = float(value)
        return {'value': value}

    # Handle boolean values (TRUE/FALSE)
    if isinstance(expr, exp.Boolean):
        return {'value': expr.this}

    # Handle star (SELECT *)
    if isinstance(expr, exp.Star):
        return {'field': '*'}

    # Handle subqueries
    if isinstance(expr, exp.Subquery):
        from namerec.uma.jsql.converter.subqueries import convert_sqlglot_select_to_jsql
        subquery_jsql = convert_sqlglot_select_to_jsql(expr.this)
        return subquery_jsql

    # Handle window functions (functions with OVER clause)
    if isinstance(expr, exp.Window):
        # Get the function inside the window
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
            
            # Handle OVER clause
            over_spec: dict[str, Any] = {}
            
            # PARTITION BY - can be a list directly or an object with expressions
            if expr.args.get('partition_by'):
                partition_by = []
                part_by_arg = expr.args['partition_by']
                if isinstance(part_by_arg, list):
                    # Already a list
                    for part_expr in part_by_arg:
                        part_jsql = convert_expression_to_jsql(part_expr)
                        partition_by.append(part_jsql)
                elif hasattr(part_by_arg, 'expressions'):
                    # Object with expressions attribute
                    for part_expr in part_by_arg.expressions:
                        part_jsql = convert_expression_to_jsql(part_expr)
                        partition_by.append(part_jsql)
                if partition_by:
                    over_spec['partition_by'] = partition_by
            
            # ORDER BY in OVER clause
            if expr.args.get('order'):
                order_by = []
                order_arg = expr.args['order']
                if isinstance(order_arg, list):
                    # Already a list
                    for order_expr in order_arg:
                        from namerec.uma.jsql.converter.sql_to_jsql import convert_order_to_jsql
                        order_jsql = convert_order_to_jsql(order_expr)
                        order_by.append(order_jsql)
                elif hasattr(order_arg, 'expressions'):
                    # Object with expressions attribute
                    for order_expr in order_arg.expressions:
                        from namerec.uma.jsql.converter.sql_to_jsql import convert_order_to_jsql
                        order_jsql = convert_order_to_jsql(order_expr)
                        order_by.append(order_jsql)
                if order_by:
                    over_spec['order_by'] = order_by
            
            if over_spec:
                result['over'] = over_spec
            
            return result

    # Handle DISTINCT (typically wrapping aggregate function arguments)
    # In sqlglot, COUNT(DISTINCT x) is represented as Count(this=Distinct(expressions=[Column(...)]))
    # Distinct uses 'expressions' list, not 'this'
    if isinstance(expr, exp.Distinct):
        # Use unified DISTINCT extraction function
        inner_jsql, _ = _extract_distinct_expression(expr)
        return inner_jsql

    # Handle functions
    if isinstance(expr, exp.Func):
        func_name = expr.sql_name()
        args = []
        first_arg_is_distinct = False
        
        # Process function arguments
        for idx, arg in enumerate(expr.args.values()):
            if isinstance(arg, exp.Expression):
                # Check if this argument is wrapped in Distinct
                if isinstance(arg, exp.Distinct):
                    # Use unified DISTINCT extraction function
                    converted, _ = _extract_distinct_expression(arg)
                    args.append(converted)
                    # Mark first argument as distinct if it's the first one
                    # (This covers the common case of COUNT(DISTINCT column))
                    if idx == 0:
                        first_arg_is_distinct = True
                else:
                    converted = convert_expression_to_jsql(arg)
                    # Check if conversion returned a dict with distinct marker
                    if isinstance(converted, dict) and converted.get('_distinct_marker'):
                        # Remove the marker and mark this arg as distinct
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
        
        # Set distinct flag if first argument is distinct
        if first_arg_is_distinct:
            result['distinct'] = True
        
        return result

    # Handle Concat (string concatenation ||)
    if isinstance(expr, exp.Concat):
        # Concat can have multiple expressions in 'expressions' attribute
        # or use left/right for binary operations
        from namerec.uma.jsql.constants import JSQLOperator
        
        if hasattr(expr, 'expressions') and expr.expressions:
            expressions = expr.expressions
        elif hasattr(expr, 'left') and hasattr(expr, 'right'):
            expressions = [expr.left, expr.right]
        else:
            # Fallback: try to get expressions from args
            expressions = getattr(expr, 'expressions', None) or expr.args.get('expressions', [])
        
        if len(expressions) == 2:
            return {
                'op': JSQLOperator.CONCAT.value,
                'left': convert_expression_to_jsql(expressions[0]),
                'right': convert_expression_to_jsql(expressions[1]),
            }
        # For multiple concatenations, convert to nested format
        # This is rare but possible (e.g., 'a' || 'b' || 'c')
        if len(expressions) > 2:
            result = convert_expression_to_jsql(expressions[-1])
            for expr_item in reversed(expressions[:-1]):
                result = {
                    'op': JSQLOperator.CONCAT.value,
                    'left': convert_expression_to_jsql(expr_item),
                    'right': result,
                }
            return result

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
            'left': convert_expression_to_jsql(expr.left),
            'right': convert_expression_to_jsql(expr.right),
        }
    
    # Handle string concatenation via || operator (PostgreSQL-style)
    # sqlglot may represent || as Concat, which we handle above
    # But some dialects may use a binary operator directly
    # Check if this is a binary || operator (less common in sqlglot, but possible)

    # Default: unsupported expression type
    raise UnsupportedOperationError(
        operation=f'Expression type: {type(expr).__name__}',
        reason='This expression type is not supported in JSQL conversion',
        path=''
    )
