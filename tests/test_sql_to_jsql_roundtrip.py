"""Tests for SQL to JSQL roundtrip conversion.

These tests validate that JSQL queries can be converted to SQL and back to JSQL
while preserving semantic equivalence. This ensures bidirectional conversion works correctly.

Test flow:
1. Start with JSQL (from test case)
2. Convert JSQL → SQL using jsql_to_sql()
3. Convert SQL → JSQL using sql_to_jsql()
4. Compare original JSQL with roundtrip JSQL (semantic equivalence)
"""

import json
from typing import Any

import pytest

from namerec.uma.jsql.converter import jsql_to_sql, sql_to_jsql
from tests.data.jsql_to_sql_cases import JSQL_TO_SQL_TEST_CASES


def jsql_equivalent(jsql1: dict[str, Any], jsql2: dict[str, Any]) -> tuple[bool, str]:
    """
    Check if two JSQL queries are semantically equivalent.
    
    This function compares JSQL dictionaries by meaning, not by exact structure.
    It handles:
    - Different operator representations (!= vs <>)
    - Order of elements in lists (select, order_by, etc.)
    - Different representations of the same concept
    
    Args:
        jsql1: First JSQL query dictionary
        jsql2: Second JSQL query dictionary
        
    Returns:
        Tuple of (is_equivalent, reason_if_not)
    """
    # Normalize both JSQL queries for comparison
    norm1 = _normalize_jsql(jsql1)
    norm2 = _normalize_jsql(jsql2)
    
    # Compare normalized structures
    if norm1 == norm2:
        return True, ""
    
    # If not equal, provide detailed comparison
    differences = _compare_jsql_detailed(norm1, norm2)
    return False, differences


def _normalize_jsql(jsql: dict[str, Any]) -> dict[str, Any]:
    """
    Normalize JSQL for semantic comparison.
    
    This function:
    - Sorts lists where order doesn't matter semantically
    - Normalizes operator representations
    - Removes None values
    - Normalizes field representations
    """
    normalized: dict[str, Any] = {}
    
    # FROM clause - normalize to string or dict
    if 'from' in jsql:
        from_clause = jsql['from']
        if isinstance(from_clause, dict):
            normalized['from'] = from_clause
        else:
            normalized['from'] = from_clause
    
    # SELECT clause - sort by field representation for comparison
    if 'select' in jsql:
        select_list = jsql['select']
        normalized['select'] = sorted(
            [_normalize_field(field) for field in select_list],
            key=lambda x: json.dumps(x, sort_keys=True)
        )
    
    # WHERE clause - normalize operators
    if 'where' in jsql:
        where_normalized = _normalize_condition(jsql['where'])
        # Normalize pattern/value in LIKE/NOT LIKE operators for comparison
        # Normalize pattern field to value field for string operators
        if isinstance(where_normalized, dict):
            string_ops = ('LIKE', 'NOT LIKE', 'ILIKE', 'NOT ILIKE', 'SIMILAR TO', 'REGEXP', 'RLIKE')
            if where_normalized.get('op') in string_ops:
                if 'pattern' in where_normalized and 'value' not in where_normalized:
                    where_normalized['value'] = where_normalized.pop('pattern')
        normalized['where'] = where_normalized
    
    # HAVING clause - normalize operators
    if 'having' in jsql:
        normalized['having'] = _normalize_condition(jsql['having'])
    
    # JOIN clauses - sort by entity name
    if 'joins' in jsql:
        normalized['joins'] = sorted(
            [_normalize_join(join) for join in jsql['joins']],
            key=lambda x: json.dumps(x, sort_keys=True)
        )
    
    # ORDER BY - sort by field representation
    if 'order_by' in jsql:
        normalized['order_by'] = sorted(
            [_normalize_order_by(order) for order in jsql['order_by']],
            key=lambda x: json.dumps(x, sort_keys=True)
        )
    
    # GROUP BY - sort by field representation
    if 'group_by' in jsql:
        normalized['group_by'] = sorted(
            [_normalize_field(field) for field in jsql['group_by']],
            key=lambda x: json.dumps(x, sort_keys=True)
        )
    
    # LIMIT and OFFSET - direct copy
    if 'limit' in jsql:
        normalized['limit'] = jsql['limit']
    if 'offset' in jsql:
        normalized['offset'] = jsql['offset']
    
    # WITH clause (CTEs) - sort by name
    if 'with' in jsql:
        normalized['with'] = sorted(
            [_normalize_cte(cte) for cte in jsql['with']],
            key=lambda x: x.get('name', '')
        )
    
    return normalized


def _normalize_field(field: dict[str, Any] | str) -> dict[str, Any]:
    """Normalize field representation."""
    if isinstance(field, str):
        return {'field': field}
    return field.copy()


def _normalize_condition(condition: dict[str, Any]) -> dict[str, Any]:
    """Normalize condition, handling operator equivalences and different formats."""
    if not isinstance(condition, dict):
        return condition
    
    normalized = condition.copy()
    
    # Normalize != to <> for comparison (they're equivalent)
    if normalized.get('op') == '!=':
        normalized['op'] = '<>'
    
    # Normalize RLIKE to REGEXP (they're semantically equivalent)
    # RLIKE is MySQL alias for REGEXP, and SQLite converts RLIKE to REGEXP
    if normalized.get('op') == 'RLIKE':
        normalized['op'] = 'REGEXP'
    
    op = normalized.get('op')
    
    # Handle IS NOT NULL -> NOT(IS NULL) normalization
    if op == 'IS NOT NULL':
        return {
            'op': 'NOT',
            'condition': {
                'field': normalized['field'],
                'op': 'IS NULL',
            }
        }
    
    # Handle NOT(BETWEEN(...)) -> NOT BETWEEN normalization
    if op == 'NOT' and 'condition' in normalized:
        condition = normalized['condition']
        if isinstance(condition, dict) and condition.get('op') == 'BETWEEN':
            return {
                'op': 'NOT BETWEEN',
                'expr': condition.get('expr'),
                'low': condition.get('low'),
                'high': condition.get('high'),
            }
    
    # Normalize comparison operators from left/right format to field/op/value format
    # This handles the difference between JSQL input format and SQL->JSQL output format
    if op and 'left' in normalized and 'right' in normalized:
        left = _normalize_expression(normalized['left'])
        right = _normalize_expression(normalized['right'])
        
        # Handle LIKE operator: left/right -> field/pattern
        if op == 'LIKE' and 'field' in left and 'value' in right:
            return {
                'field': left['field'],
                'op': 'LIKE',
                'pattern': right['value'],
            }
        
        # Handle NOT LIKE operator: left/right -> field/pattern
        if op == 'NOT LIKE' and 'field' in left and 'value' in right:
            return {
                'field': left['field'],
                'op': 'NOT LIKE',
                'pattern': right['value'],
            }
        
        # Handle ILIKE operator: left/right -> field/pattern
        if op == 'ILIKE' and 'field' in left and 'value' in right:
            return {
                'field': left['field'],
                'op': 'ILIKE',
                'pattern': right['value'],
            }
        
        # Handle NOT ILIKE operator: left/right -> field/pattern
        if op == 'NOT ILIKE' and 'field' in left and 'value' in right:
            return {
                'field': left['field'],
                'op': 'NOT ILIKE',
                'pattern': right['value'],
            }
        
        # Handle SIMILAR TO operator: left/right -> field/pattern
        if op == 'SIMILAR TO' and 'field' in left and 'value' in right:
            return {
                'field': left['field'],
                'op': 'SIMILAR TO',
                'pattern': right['value'],
            }
        
        # Handle REGEXP operator: left/right -> field/pattern
        if op == 'REGEXP' and 'field' in left and 'value' in right:
            return {
                'field': left['field'],
                'op': 'REGEXP',
                'pattern': right['value'],
            }
        
        # Handle RLIKE operator: left/right -> field/pattern
        if op == 'RLIKE' and 'field' in left and 'value' in right:
            return {
                'field': left['field'],
                'op': 'RLIKE',
                'pattern': right['value'],
            }
        
        # Handle IN operator: left/right/values -> field/values or field/subquery
        if op == 'IN':
            # Check if right side is a subquery (has 'from' and 'select')
            if 'from' in right and 'select' in right:
                # Right side is a subquery - normalize it recursively
                if 'field' in left:
                    return {
                        'field': left['field'],
                        'op': 'IN',
                        'right': _normalize_jsql(right),  # Recursively normalize subquery
                    }
            # Check if values are in right.values (from original format) or already in normalized
            values = None
            if 'values' in right:
                values = right['values']
            elif 'values' in normalized:
                values = normalized['values']
            if 'field' in left and values is not None:
                # Don't sort - preserve original order for comparison
                return {
                    'field': left['field'],
                    'op': 'IN',
                    'values': values,
                }
        
        # Handle NOT IN operator
        if op == 'NOT IN':
            values = None
            if 'values' in right:
                values = right['values']
            elif 'values' in normalized:
                values = normalized['values']
            if 'field' in left and values is not None:
                # Don't sort - preserve original order for comparison
                return {
                    'field': left['field'],
                    'op': 'NOT IN',
                    'values': values,
                }
        
        # Handle simple comparison operators: left/right -> field/op/value
        if 'field' in left:
            new_condition = {
                'field': left['field'],
                'op': op,
            }
            if 'value' in right:
                new_condition['value'] = right['value']
            elif 'field' in right:
                new_condition['right_field'] = right['field']
            elif 'from' in right and 'select' in right:
                # Right side is a subquery - keep it as 'right' for comparison
                new_condition['right'] = right
            else:
                # Keep original format if right side is complex
                normalized['left'] = left
                normalized['right'] = right
                return normalized
            return new_condition
    
    # Recursively normalize nested conditions
    if 'conditions' in normalized:
        normalized['conditions'] = [
            _normalize_condition(c) for c in normalized['conditions']
        ]
    if 'condition' in normalized:
        normalized['condition'] = _normalize_condition(normalized['condition'])
    if 'left' in normalized:
        normalized['left'] = _normalize_expression(normalized['left'])
    if 'right' in normalized:
        normalized['right'] = _normalize_expression(normalized['right'])
    
    return normalized


def _normalize_expression(expr: dict[str, Any] | Any) -> dict[str, Any] | Any:
    """Normalize expression (field, value, subquery, etc.)."""
    if isinstance(expr, dict):
        if 'field' in expr:
            return _normalize_field(expr)
        # Recursively normalize nested structures
        return {k: _normalize_expression(v) for k, v in expr.items()}
    return expr


def _normalize_join(join: dict[str, Any]) -> dict[str, Any]:
    """Normalize join representation."""
    normalized = join.copy()
    if 'on' in normalized:
        normalized['on'] = _normalize_condition(normalized['on'])
    return normalized


def _normalize_order_by(order: dict[str, Any]) -> dict[str, Any]:
    """Normalize ORDER BY clause."""
    normalized = order.copy()
    # Normalize direction to uppercase
    if 'direction' in normalized:
        normalized['direction'] = normalized['direction'].upper()
    return normalized


def _normalize_cte(cte: dict[str, Any]) -> dict[str, Any]:
    """Normalize CTE representation."""
    normalized = cte.copy()
    if 'query' in normalized:
        normalized['query'] = _normalize_jsql(normalized['query'])
    return normalized


def _compare_jsql_detailed(jsql1: dict[str, Any], jsql2: dict[str, Any]) -> str:
    """Provide detailed comparison of two JSQL queries."""
    differences = []
    
    keys1 = set(jsql1.keys())
    keys2 = set(jsql2.keys())
    
    # Check for missing keys
    missing_in_2 = keys1 - keys2
    if missing_in_2:
        differences.append(f"Keys missing in second JSQL: {missing_in_2}")
    
    extra_in_2 = keys2 - keys1
    if extra_in_2:
        differences.append(f"Extra keys in second JSQL: {extra_in_2}")
    
    # Compare common keys
    common_keys = keys1 & keys2
    for key in common_keys:
        if jsql1[key] != jsql2[key]:
            differences.append(
                f"Key '{key}' differs:\n"
                f"  First:  {json.dumps(jsql1[key], indent=2)}\n"
                f"  Second: {json.dumps(jsql2[key], indent=2)}"
            )
    
    return "\n".join(differences) if differences else "Structures are identical but comparison failed"


# Known limitations:
# - None currently - all tests should pass with proper normalization
# Note: RLIKE gets converted to REGEXP in SQLite, but this is handled by normalization
# in _normalize_condition() which converts RLIKE to REGEXP for comparison
KNOWN_LIMITATIONS: set[str] = set()


@pytest.mark.parametrize('test_case', JSQL_TO_SQL_TEST_CASES, ids=lambda tc: tc['name'])
def test_jsql_sql_jsql_roundtrip(test_case: dict) -> None:
    """
    Test roundtrip conversion: JSQL → SQL → JSQL.
    
    For each test case:
    1. Start with original JSQL from test case
    2. Convert JSQL → SQL using jsql_to_sql()
    3. Convert SQL → JSQL using sql_to_jsql()
    4. Compare original JSQL with roundtrip JSQL for semantic equivalence
    
    This ensures that any SQL generated from JSQL can be converted back to
    semantically equivalent JSQL.
    
    Args:
        test_case: Test case dictionary containing:
            - name: Test case name
            - jsql: Original JSQL query dictionary
            - expected_sql: Expected SQL string (used for dialect)
            - dialect: SQL dialect (default: 'sqlite')
            - description: Optional description
    """
    original_jsql = test_case['jsql']
    dialect = test_case.get('dialect', 'sqlite')
    test_name = test_case['name']
    description = test_case.get('description', 'N/A')
    
    # Skip known limitations
    if test_name in KNOWN_LIMITATIONS:
        pytest.skip(f"Known limitation: {description}")
    
    # Step 1: Convert JSQL → SQL
    try:
        generated_sql = jsql_to_sql(original_jsql, dialect=dialect)
    except Exception as e:
        pytest.fail(
            f"Failed to convert JSQL to SQL for test case '{test_name}': {e}\n"
            f"Description: {description}\n"
            f"JSQL: {json.dumps(original_jsql, indent=2)}"
        )
    
    # Step 2: Convert SQL → JSQL
    try:
        roundtrip_jsql = sql_to_jsql(generated_sql, dialect=dialect)
    except Exception as e:
        pytest.fail(
            f"Failed to convert SQL to JSQL for test case '{test_name}': {e}\n"
            f"Description: {description}\n"
            f"Original JSQL: {json.dumps(original_jsql, indent=2)}\n"
            f"Generated SQL: {generated_sql}"
        )
    
    # Step 3: Compare original and roundtrip JSQL
    is_equivalent, reason = jsql_equivalent(original_jsql, roundtrip_jsql)
    
    assert is_equivalent, (
        f"JSQL roundtrip failed for test case '{test_name}'\n"
        f"Description: {description}\n"
        f"Original JSQL:\n{json.dumps(original_jsql, indent=2)}\n"
        f"Generated SQL:\n{generated_sql}\n"
        f"Roundtrip JSQL:\n{json.dumps(roundtrip_jsql, indent=2)}\n"
        f"Differences:\n{reason}"
    )
