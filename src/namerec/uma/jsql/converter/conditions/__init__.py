"""Condition handling for JSQL <-> SQL conversion.

This package provides bidirectional conversion between JSQL condition specifications
and SQLGlot expression objects. It is organized into the following modules:

- `helpers`: Utility functions for extracting expressions and patterns from condition specs
- `to_sql`: Conversion from JSQL conditions to SQLGlot expressions
- `to_jsql`: Conversion from SQLGlot expressions to JSQL conditions

The main entry points are:
- `jsql_condition_to_sqlglot()`: Convert JSQL condition dict to SQLGlot expression
- `convert_condition_to_jsql()`: Convert SQLGlot expression to JSQL condition dict
- `convert_sqlglot_select_to_jsql()`: Convert SQLGlot Select to JSQL query dict (for subqueries)

Example:
    >>> from namerec.uma.jsql.converter.conditions import jsql_condition_to_sqlglot
    >>> cond = {"op": ">=", "field": "age", "value": 18}
    >>> expr = jsql_condition_to_sqlglot(cond)
    >>> # expr is now a SQLGlot expression representing "age >= 18"
"""

from namerec.uma.jsql.converter.conditions.helpers import extract_in_operator_values
from namerec.uma.jsql.converter.conditions.helpers import extract_left_expression
from namerec.uma.jsql.converter.conditions.helpers import extract_lower_like_pattern
from namerec.uma.jsql.converter.conditions.helpers import extract_pattern
from namerec.uma.jsql.converter.conditions.helpers import normalize_between_jsql_fields
from namerec.uma.jsql.converter.conditions.helpers import validate_field_reference
from namerec.uma.jsql.converter.conditions.helpers import validate_string_operator_operands
from namerec.uma.jsql.converter.conditions.to_jsql import convert_condition_to_jsql
from namerec.uma.jsql.converter.conditions.to_sql import jsql_condition_to_sqlglot
from namerec.uma.jsql.converter.sqlglot_to_jsql import convert_sqlglot_select_to_jsql

__all__ = [
    'jsql_condition_to_sqlglot',
    'convert_condition_to_jsql',
    'convert_sqlglot_select_to_jsql',
    'extract_in_operator_values',
    'extract_left_expression',
    'extract_lower_like_pattern',
    'extract_pattern',
    'normalize_between_jsql_fields',
    'validate_field_reference',
    'validate_string_operator_operands',
]
