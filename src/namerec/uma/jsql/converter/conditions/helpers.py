"""Helper functions for condition conversion.

This module provides utility functions for extracting and processing
expressions and patterns from JSQL condition specifications.
"""

from typing import Any

import sqlglot.expressions as exp

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
