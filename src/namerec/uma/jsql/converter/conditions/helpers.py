"""Helper functions for condition conversion.

This module provides utility functions for extracting and processing
expressions and patterns from JSQL condition specifications.
"""

from typing import Any

import sqlglot.expressions as exp

from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
from namerec.uma.jsql.conversion_exceptions import MissingFieldError
from namerec.uma.jsql.converter.expressions import jsql_expression_to_sqlglot


def extract_left_expression(cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Extract left expression from condition spec.
    
    Supports both old format ('field') and new format ('left').
    The old format is maintained for backward compatibility.
    
    Args:
        cond_spec: Condition specification dictionary. Must contain either:
            - 'left': Expression specification (new format)
            - 'field': Field name or expression specification (old format)
        
    Returns:
        SQLGlot expression for the left side of the condition.
        
    Raises:
        MissingFieldError: If neither 'field' nor 'left' is present in cond_spec.
        
    Example:
        >>> spec = {"op": ">=", "field": "age", "value": 18}
        >>> left_expr = extract_left_expression(spec)
        >>> # Returns SQLGlot expression for "age"
        
        >>> spec = {"op": ">=", "left": {"field": "age"}, "right": {"value": 18}}
        >>> left_expr = extract_left_expression(spec)
        >>> # Returns SQLGlot expression for "age"
    """
    if 'left' in cond_spec:
        return jsql_expression_to_sqlglot(cond_spec['left'])
    elif 'field' in cond_spec:
        return jsql_expression_to_sqlglot(cond_spec['field'])
    else:
        raise MissingFieldError('left or field', path='left/field', context='condition')


def extract_pattern(cond_spec: dict[str, Any]) -> str | None:
    """
    Extract pattern from condition spec for string operators.
    
    Supports both 'pattern' field and 'right' expression formats.
    If 'right' is a string literal, extracts the value.
    If 'right' is an expression (e.g., column reference), returns None,
    indicating that the caller should use the expression directly.
    
    Args:
        cond_spec: Condition specification dictionary. May contain:
            - 'pattern': Direct pattern string
            - 'right': Expression specification (will be checked if it's a string literal)
        
    Returns:
        Pattern string if found, None if should use expression directly.
        
    Example:
        >>> spec = {"op": "LIKE", "field": "name", "pattern": "%test%"}
        >>> pattern = extract_pattern(spec)
        >>> # Returns "%test%"
        
        >>> spec = {"op": "LIKE", "field": "name", "right": {"value": "%test%"}}
        >>> pattern = extract_pattern(spec)
        >>> # Returns "%test%"
        
        >>> spec = {"op": "LIKE", "field": "name", "right": {"field": "pattern_col"}}
        >>> pattern = extract_pattern(spec)
        >>> # Returns None (right is not a string literal)
    """
    if 'pattern' in cond_spec:
        return cond_spec['pattern']
    
    if 'right' in cond_spec:
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
        if isinstance(right_expr, exp.Literal) and right_expr.is_string:
            return right_expr.this
    
    return None


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
    
    Args:
        expr_jsql: Expression JSQL (from inner.this or expr.this)
        low_jsql: Low bound JSQL
        high_jsql: High bound JSQL
        
    Returns:
        Dictionary with normalized 'expr', 'low', and 'high' fields.
        
    Example:
        >>> expr = {'field': 'age'}
        >>> low = {'value': 18}
        >>> high = {'value': 65}
        >>> result = normalize_between_jsql_fields(expr, low, high)
        >>> # Returns: {
        ... #     'expr': {'field': 'age'},
        ... #     'low': {'value': 18},
        ... #     'high': {'value': 65}
        ... # }
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
    context: str = ""
) -> tuple[str, str]:
    """
    Validate operands for string operators (LIKE, ILIKE, SIMILAR_TO, REGEXP, etc.).
    
    This helper function centralizes validation logic for string operators,
    eliminating code duplication across multiple operator handlers.
    
    Args:
        left: Left operand JSQL (should contain 'field')
        right: Right operand JSQL (should contain 'value' or 'pattern')
        operator: Operator name for error messages (e.g., 'ILIKE', 'LIKE')
        context: Additional context for error messages (e.g., 'LOWER pattern')
        
    Returns:
        Tuple of (field_name, pattern_value)
        
    Raises:
        InvalidExpressionError: If validation fails
        
    Example:
        >>> left = {'field': 'name'}
        >>> right = {'value': '%test%'}
        >>> field, pattern = validate_string_operator_operands(left, right, 'ILIKE')
        >>> # Returns: ('name', '%test%')
    """
    if 'field' not in left:
        msg = f'{operator} operator'
        if context:
            msg += f' ({context})'
        msg += ' left side must be a field reference'
        raise InvalidExpressionError(
            message=msg,
            path='left',
            expression=left
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
            expression=right
        )
    
    return left['field'], pattern_value


def validate_field_reference(
    expr_jsql: dict[str, Any],
    operator: str,
    side: str = "left"
) -> str:
    """
    Validate that expression is a field reference.
    
    Args:
        expr_jsql: Expression JSQL
        operator: Operator name for error messages
        side: Which side ('left' or 'right') for error messages
        
    Returns:
        Field name
        
    Raises:
        InvalidExpressionError: If expression is not a field reference
    """
    if 'field' not in expr_jsql:
        raise InvalidExpressionError(
            message=f'{operator} operator {side} side must be a field reference',
            path=side,
            expression=expr_jsql
        )
    return expr_jsql['field']


def extract_in_operator_values(
    expressions: list[exp.Expression],
    operator: str
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
        
    Example:
        >>> from sqlglot import parse_one
        >>> sql = "SELECT * FROM users WHERE id IN (1, 2, 3)"
        >>> parsed = parse_one(sql)
        >>> in_expr = parsed.args['where'].this
        >>> values = extract_in_operator_values(in_expr.expressions, 'IN')
        >>> # Returns: [1, 2, 3]
    """
    from namerec.uma.jsql.converter.expressions import convert_expression_to_jsql
    
    values: list[Any] = []
    if isinstance(expressions, list):
        for val_expr in expressions:
            val = convert_expression_to_jsql(val_expr)
            if 'value' not in val:
                raise InvalidExpressionError(
                    message=f'{operator} operator values must be literals',
                    path='values',
                    expression=val
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
    
    Args:
        like_expr: SQLGlot Like expression
        
    Returns:
        Tuple of (left_jsql, right_jsql) if LOWER pattern detected, None otherwise
        
    Example:
        >>> from sqlglot import parse_one
        >>> sql = "SELECT * FROM users WHERE LOWER(name) LIKE LOWER('%test%')"
        >>> expr = parse_one(sql).args['where'].this
        >>> result = extract_lower_like_pattern(expr)
        >>> # Returns: ({'field': 'name'}, {'value': '%test%'})
    """
    from namerec.uma.jsql.converter.expressions import convert_expression_to_jsql
    
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
