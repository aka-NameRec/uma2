"""Tests for complex SQL roundtrip conversion (SQL -> JSQL -> SQL).

These tests validate that complex SQL queries from real-world scenarios
can be converted to JSQL and back to SQL while preserving semantic equivalence.
This ensures bidirectional conversion works correctly for complex queries including:
- COUNT(DISTINCT ...) in aggregations
- Scalar subqueries in SELECT
- EXISTS with correlated subqueries
- CTE with window functions
- Complex JOINs with aggregations
"""

import re
from dataclasses import dataclass
from typing import Any

import pytest

from namerec.uma.jsql.converter import jsql_to_sql
from namerec.uma.jsql.converter import sql_to_jsql


@dataclass
class ComplexQueryTestCase:
    """Test case for complex SQL roundtrip conversion."""

    name: str
    sql: str
    description: str


def normalize_sql(sql: str) -> str:
    """Normalize SQL for comparison by removing extra whitespace and cosmetic differences.

    This function normalizes SQL queries to enable semantic comparison by:
    - Removing AS keywords before table/column aliases (cosmetic difference)
    - Normalizing function calls without arguments (CURRENT_DATE() -> CURRENT_DATE)
    - Removing extra whitespace
    - Normalizing operator spacing

    Args:
        sql: SQL string to normalize

    Returns:
        Normalized SQL string in uppercase
    """
    # Normalize function calls without arguments first (before other processing)
    # This handles differences like CURRENT_DATE vs CURRENT_DATE()
    sql = re.sub(
        r'\b(CURRENT_DATE|CURRENT_TIME|CURRENT_TIMESTAMP|NOW|USER|DATABASE)\(\)\b',
        r'\1',
        sql,
        flags=re.IGNORECASE,
    )
    # Convert to uppercase
    sql = sql.upper()
    # Remove multiple spaces and newlines, normalize to single space
    sql = re.sub(r'\s+', ' ', sql)
    # Remove spaces around operators and punctuation
    sql = re.sub(r'\s*([(),;=<>!]+)\s*', r'\1', sql)
    # Remove AS keywords before table/column aliases (cosmetic difference)
    # Pattern: table_name AS alias_name (where alias is a single identifier)
    sql = re.sub(r'\b([A-Z_][A-Z0-9_]*)\s+AS\s+([A-Z_][A-Z0-9_]*)\b', r'\1 \2', sql)
    # Also remove CURRENT_DATE() if it still exists after uppercase (fallback)
    sql = re.sub(r'CURRENT_DATE\(\)', 'CURRENT_DATE', sql)
    sql = re.sub(r'CURRENT_TIME\(\)', 'CURRENT_TIME', sql)
    sql = re.sub(r'CURRENT_TIMESTAMP\(\)', 'CURRENT_TIMESTAMP', sql)
    return sql.strip()


# Complex SQL queries from Google Share document
# Note: Recursive CTE query is excluded as per requirements
COMPLEX_QUERY_TEST_CASES = [
    ComplexQueryTestCase(
        name='complex_join_with_aggregations',
        sql="""
        SELECT 
            c.customer_id,
            c.customer_name,
            COUNT(DISTINCT o.order_id) as total_orders,
            SUM(oi.quantity * oi.unit_price) as total_spent,
            AVG(o.order_date) as avg_order_date
        FROM customers c
        LEFT JOIN orders o ON c.customer_id = o.customer_id
        LEFT JOIN order_items oi ON o.order_id = oi.order_id
        WHERE c.active = 1
            AND o.order_date >= '2024-01-01'
            AND o.order_date <= '2024-12-31'
        GROUP BY c.customer_id, c.customer_name
        HAVING COUNT(DISTINCT o.order_id) > 5
        ORDER BY total_spent DESC
        LIMIT 10
        """,
        description='Complex JOIN with aggregations, DISTINCT, and HAVING clause',
    ),
    ComplexQueryTestCase(
        name='cte_with_window_functions',
        sql="""
        WITH ranked_products AS (
            SELECT 
                p.product_id,
                p.product_name,
                p.category_id,
                p.price,
                ROW_NUMBER() OVER (PARTITION BY p.category_id ORDER BY p.price DESC) as price_rank,
                AVG(p.price) OVER (PARTITION BY p.category_id) as avg_category_price
            FROM products p
            WHERE p.active = 1
        )
        SELECT 
            rp.product_id,
            rp.product_name,
            rp.price,
            rp.price_rank,
            rp.avg_category_price,
            c.category_name
        FROM ranked_products rp
        INNER JOIN categories c ON rp.category_id = c.category_id
        WHERE rp.price_rank <= 3
        ORDER BY c.category_name, rp.price_rank
        """,
        description='CTE with window functions (ROW_NUMBER, AVG OVER)',
    ),
    ComplexQueryTestCase(
        name='complex_subqueries_with_exists',
        sql="""
        SELECT 
            e.employee_id,
            e.employee_name,
            e.department_id,
            d.department_name,
            (
                SELECT COUNT(*)
                FROM projects p
                WHERE p.manager_id = e.employee_id
                    AND p.status = 'active'
            ) as active_projects_count,
            (
                SELECT SUM(p.budget)
                FROM projects p
                WHERE p.manager_id = e.employee_id
                    AND p.completed = 1
            ) as total_completed_budget
        FROM employees e
        INNER JOIN departments d ON e.department_id = d.department_id
        WHERE EXISTS (
            SELECT 1
            FROM projects p
            WHERE p.manager_id = e.employee_id
                AND p.deadline >= CURRENT_DATE
        )
        ORDER BY e.department_id, active_projects_count DESC
        """,
        description='Complex subqueries in SELECT and EXISTS with CURRENT_DATE function',
    ),
]


@pytest.mark.parametrize(
    'test_case',
    COMPLEX_QUERY_TEST_CASES,
    ids=lambda tc: tc.name,
)
def test_complex_sql_roundtrip(test_case: ComplexQueryTestCase) -> None:
    """Test roundtrip conversion: SQL -> JSQL -> SQL.

    For each complex query:
    1. Convert SQL -> JSQL using sql_to_jsql()
    2. Convert JSQL -> SQL using jsql_to_sql()
    3. Compare original SQL with roundtrip SQL (after normalization)

    This ensures that complex SQL queries can be converted to JSQL and back
    while preserving semantic equivalence.

    Args:
        test_case: Test case containing SQL query and metadata
    """
    original_sql = test_case.sql.strip()
    dialect = 'generic'

    # Step 1: Convert SQL -> JSQL
    try:
        jsql = sql_to_jsql(original_sql, dialect=dialect)
    except Exception as e:
        pytest.fail(
            f"Failed to convert SQL to JSQL for test case '{test_case.name}': {e}\n"
            f"Description: {test_case.description}\n"
            f"SQL: {original_sql}"
        )

    # Step 2: Convert JSQL -> SQL
    try:
        converted_sql = jsql_to_sql(jsql, dialect=dialect)
    except Exception as e:
        pytest.fail(
            f"Failed to convert JSQL to SQL for test case '{test_case.name}': {e}\n"
            f"Description: {test_case.description}\n"
            f"Original SQL: {original_sql}\n"
            f"JSQL: {jsql}"
        )

    # Step 3: Compare SQL queries (normalized)
    original_normalized = normalize_sql(original_sql)
    converted_normalized = normalize_sql(converted_sql)

    assert original_normalized == converted_normalized, (
        f"SQL roundtrip failed for test case '{test_case.name}'\n"
        f"Description: {test_case.description}\n"
        f"Original SQL:\n{original_sql}\n"
        f"Converted SQL:\n{converted_sql}\n"
        f"Original (normalized):\n{original_normalized}\n"
        f"Converted (normalized):\n{converted_normalized}"
    )
