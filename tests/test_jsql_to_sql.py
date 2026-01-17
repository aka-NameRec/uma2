"""Tests for JSQL to SQL conversion.

These tests validate that JSQL queries are correctly converted to SQL strings
by comparing generated SQL against expected SQL strings.
"""

import re

import pytest

from namerec.uma.jsql.converter import jsql_to_sql
from tests.data.jsql_to_sql_cases import JSQL_TO_SQL_TEST_CASES


def normalize_sql(sql: str) -> str:
    """
    Normalize SQL for comparison.
    
    - Remove extra whitespace
    - Normalize keyword casing (to lowercase)
    - Collapse multiple whitespace to single space
    - Remove leading/trailing whitespace
    
    Args:
        sql: SQL string to normalize
        
    Returns:
        Normalized SQL string
    """
    # Convert to lowercase
    sql = sql.lower()
    
    # Remove leading/trailing whitespace
    sql = sql.strip()
    
    # Collapse multiple whitespace (including newlines, tabs) to single space
    sql = re.sub(r'\s+', ' ', sql)
    
    # Remove spaces around parentheses (optional, for cleaner comparison)
    # sql = re.sub(r'\s*\(\s*', '(', sql)
    # sql = re.sub(r'\s*\)\s*', ')', sql)
    
    return sql


@pytest.mark.parametrize('test_case', JSQL_TO_SQL_TEST_CASES, ids=lambda tc: tc['name'])
def test_jsql_to_sql_conversion(test_case: dict) -> None:
    """
    Test JSQL to SQL conversion matches expected SQL.
    
    For each test case:
    1. Convert JSQL to SQL using jsql_to_sql()
    2. Normalize both generated and expected SQL
    3. Compare normalized SQL strings
    4. Provide detailed error message on mismatch
    
    Args:
        test_case: Test case dictionary containing:
            - name: Test case name
            - jsql: JSQL query dictionary (input)
            - expected_sql: Expected SQL string (output)
            - dialect: SQL dialect (default: 'sqlite')
            - description: Optional description
    """
    jsql = test_case['jsql']
    expected_sql = test_case['expected_sql']
    dialect = test_case.get('dialect', 'sqlite')
    
    # Convert JSQL to SQL
    generated_sql = jsql_to_sql(jsql, dialect=dialect)
    
    # Normalize for comparison
    normalized_generated = normalize_sql(generated_sql)
    normalized_expected = normalize_sql(expected_sql)
    
    # Compare
    assert normalized_generated == normalized_expected, (
        f"SQL mismatch for test case '{test_case['name']}'\n"
        f"Description: {test_case.get('description', 'N/A')}\n"
        f"JSQL: {jsql}\n"
        f"Expected SQL:\n{expected_sql}\n"
        f"Generated SQL:\n{generated_sql}\n"
        f"Normalized expected:\n{normalized_expected}\n"
        f"Normalized generated:\n{normalized_generated}"
    )
