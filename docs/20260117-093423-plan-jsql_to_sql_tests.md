# Plan: Comprehensive JSQL→SQL Conversion Tests

**Document ID**: 20260117-093423  
**Status**: Proposal  
**Version**: 1.0

## Goal

Create comprehensive test suite that validates JSQL to SQL conversion by comparing generated SQL against expected SQL strings. This ensures all JSQL capabilities are correctly converted to SQL for database execution.

## Current State

### Existing Tests
- `tests/test_converter.py`: Contains basic JSQL↔SQL conversion tests
  - Uses simple assertions checking for keyword presence (`assert 'SELECT' in sql`)
  - Does not validate exact SQL structure or formatting
  - Limited coverage of operators and complex expressions

### Conversion Function
- `namerec.uma.jsql.converter.jsql_to_sql()`: Converts JSQL dict to SQL string
- Uses `sqlglot` for SQL generation from JSQL AST

## Proposed Approach

### Test Structure

1. **Test Data Module** (`tests/data/jsql_to_sql_cases.py`):
   - Contains test case constants as dictionaries
   - Each test case has:
     - `name`: Descriptive test name
     - `jsql`: JSQL query dictionary (input)
     - `expected_sql`: Expected SQL string (output)
     - `dialect`: Optional SQL dialect (default: 'sqlite' for portability)
     - `description`: Optional description of what is being tested

2. **Test Execution Module** (`tests/test_jsql_to_sql.py`):
   - Parametrized tests using pytest
   - For each test case:
     - Convert JSQL to SQL using `jsql_to_sql()`
     - Normalize both generated and expected SQL
     - Compare normalized SQL strings
   - Provide detailed error messages on mismatch

### Test Case Categories

#### 1. Basic Query Structure
- Simple SELECT with single/multiple fields
- SELECT with wildcard (`*`)
- SELECT with aliases
- FROM clause variations
- LIMIT and OFFSET
- ORDER BY (single/multiple, ASC/DESC)
- DISTINCT

#### 2. Comparison Operators
- `=` (equals)
- `!=` (not equals)
- `<>` (ISO not equals)
- `<` (less than)
- `<=` (less than or equal)
- `>` (greater than)
- `>=` (greater than or equal)

**Note**: One representative test case per operator is sufficient (as requested).

#### 3. Logical Operators
- `AND` (multiple conditions)
- `OR` (multiple conditions)
- `NOT` (negation)
- Nested combinations (AND with OR, NOT with AND, etc.)

#### 4. Pattern Matching Operators
- `LIKE` with wildcards (`%`, `_`)
- `NOT LIKE`
- `ILIKE` (case-insensitive LIKE)
- `NOT ILIKE`
- `SIMILAR TO` (PostgreSQL regex)
- `REGEXP` / `RLIKE` (regex pattern matching)

**Note**: One test case per operator.

#### 5. Membership and Existence Operators
- `IN` with value list
- `NOT IN` with value list
- `IN` with subquery
- `NOT IN` with subquery
- `EXISTS` with subquery
- `NOT EXISTS` with subquery

#### 6. Range Operators
- `BETWEEN` (with values)
- `NOT BETWEEN` (with values)
- `BETWEEN` (with field references)
- `NOT BETWEEN` (with field references)

#### 7. Null Checks
- `IS NULL`
- `IS NOT NULL`

#### 8. Arithmetic Operators
- `+` (addition)
- `-` (subtraction)
- `*` (multiplication)
- `/` (division)
- `%` (modulo)
- `||` (string concatenation)

**Note**: One test case per operator in SELECT expressions and WHERE conditions.

#### 9. Functions
- Aggregation: `COUNT`, `SUM`, `AVG`, `MIN`, `MAX`
- `COUNT(DISTINCT ...)`
- String: `CONCAT`, `UPPER`, `LOWER`, `SUBSTRING`
- Math: `ROUND`, `ABS`, `CEIL`, `FLOOR`
- Date: `DATE`, `YEAR`, `MONTH`, `DAY`

#### 10. JOINs
- INNER JOIN
- LEFT JOIN
- RIGHT JOIN
- FULL OUTER JOIN
- CROSS JOIN
- Multiple JOINs
- JOIN with complex ON conditions

#### 11. GROUP BY and HAVING
- Simple GROUP BY
- GROUP BY with multiple columns
- HAVING with aggregation functions
- HAVING with complex conditions

#### 12. Subqueries
- Subquery in WHERE (comparison)
- Subquery in WHERE (IN/NOT IN)
- Subquery in WHERE (EXISTS/NOT EXISTS)
- Subquery in SELECT (scalar)
- Correlated subqueries

#### 13. CTE (Common Table Expressions)
- Single CTE
- Multiple CTEs
- Recursive CTEs (if supported)

#### 14. Window Functions
- `ROW_NUMBER()`
- `RANK()`, `DENSE_RANK()`
- `LAG()`, `LEAD()`
- `FIRST_VALUE()`, `LAST_VALUE()`
- Aggregation functions as window functions
- PARTITION BY
- ORDER BY in window functions

#### 15. Complex Expressions
- Nested arithmetic expressions
- Expressions with functions
- Expressions with subqueries
- CASE WHEN expressions
- COALESCE, NULLIF

#### 16. Parameters
- Parameterized queries (placeholders)
- Multiple parameters

#### 17. Type Handling
- String literals
- Numeric literals (integers, floats)
- Boolean literals
- NULL values
- Date/time literals

## Implementation Details

### Test Case Format

```python
# tests/data/jsql_to_sql_cases.py

JSQL_TO_SQL_TEST_CASES = [
    {
        'name': 'simple_select_single_field',
        'jsql': {
            'from': 'users',
            'select': [{'field': 'id'}],
        },
        'expected_sql': 'SELECT users.id FROM users',
        'description': 'Simple SELECT with single field',
    },
    {
        'name': 'comparison_operator_equal',
        'jsql': {
            'from': 'users',
            'select': [{'field': 'id'}],
            'where': {
                'op': '=',
                'left': {'field': 'active'},
                'right': {'value': True},
            },
        },
        'expected_sql': 'SELECT users.id FROM users WHERE users.active = TRUE',
        'description': 'Comparison operator: equals (=)',
    },
    {
        'name': 'like_operator',
        'jsql': {
            'from': 'users',
            'select': [{'field': 'name'}],
            'where': {
                'op': 'LIKE',
                'left': {'field': 'email'},
                'right': {'value': '%@example.com'},
            },
        },
        'expected_sql': "SELECT users.name FROM users WHERE users.email LIKE '%@example.com'",
        'description': 'Pattern matching: LIKE operator',
    },
    # ... more test cases
]
```

### Test Execution

```python
# tests/test_jsql_to_sql.py

import pytest
from namerec.uma.jsql.converter import jsql_to_sql
from tests.data.jsql_to_sql_cases import JSQL_TO_SQL_TEST_CASES


def normalize_sql(sql: str) -> str:
    """
    Normalize SQL for comparison.
    
    - Remove extra whitespace
    - Normalize keyword casing
    - Normalize quote styles (if needed)
    - Remove database-specific formatting differences
    """
    import re
    # Basic normalization: lowercase, strip, collapse whitespace
    sql = sql.lower().strip()
    sql = re.sub(r'\s+', ' ', sql)
    return sql


@pytest.mark.parametrize('test_case', JSQL_TO_SQL_TEST_CASES, ids=lambda tc: tc['name'])
def test_jsql_to_sql_conversion(test_case):
    """
    Test JSQL to SQL conversion matches expected SQL.
    
    For each test case:
    1. Convert JSQL to SQL
    2. Normalize both generated and expected SQL
    3. Compare normalized SQL
    4. Provide detailed error message on mismatch
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
        f"SQL mismatch for test case '{test_case['name']}':\n"
        f"Expected: {expected_sql}\n"
        f"Generated: {generated_sql}\n"
        f"Normalized expected: {normalized_expected}\n"
        f"Normalized generated: {normalized_generated}"
    )
```

### SQL Normalization Strategy

SQL normalization is critical because:
- Different SQL generators may format SQL differently
- Whitespace differences should not cause test failures
- Keyword casing may vary
- Parameter placeholders may differ

**Normalization Steps**:
1. Convert to lowercase (or uppercase) consistently
2. Remove leading/trailing whitespace
3. Collapse multiple whitespace to single space
4. Normalize line breaks (if multi-line SQL expected)
5. Handle parameter placeholder differences (if applicable)
6. Normalize quote styles (single vs double quotes for identifiers)

**Note**: For initial implementation, basic normalization (lowercase, strip, collapse whitespace) should be sufficient. More sophisticated normalization can be added if needed.

### Error Reporting

On test failure, provide:
1. Test case name
2. Original JSQL input
3. Expected SQL
4. Generated SQL
5. Normalized versions of both
6. Diff between normalized SQLs (if possible)

### Test Organization

```
tests/
├── data/
│   └── jsql_to_sql_cases.py          # Test case constants
├── test_jsql_to_sql.py                # Main test file
├── test_converter.py                  # Existing converter tests (keep)
└── ...
```

### Dialect Considerations

- Default dialect: `sqlite` (most portable)
- Some operators may be dialect-specific (e.g., `ILIKE`, `SIMILAR TO`)
- For dialect-specific operators, create separate test cases per dialect
- Use pytest markers to categorize dialect-specific tests

### Phased Implementation

#### Phase 1: Basic Structure
1. Create `tests/data/jsql_to_sql_cases.py` with test case structure
2. Implement basic test execution in `tests/test_jsql_to_sql.py`
3. Add 5-10 basic test cases (SELECT, WHERE with simple operators)
4. Implement basic SQL normalization

#### Phase 2: Operators
1. Add test cases for all comparison operators
2. Add test cases for logical operators
3. Add test cases for pattern matching operators
4. Add test cases for membership/existence operators
5. Add test cases for arithmetic operators

#### Phase 3: Advanced Features
1. Add test cases for JOINs
2. Add test cases for GROUP BY / HAVING
3. Add test cases for subqueries
4. Add test cases for CTEs
5. Add test cases for window functions

#### Phase 4: Edge Cases
1. Add test cases for complex nested expressions
2. Add test cases for type handling
3. Add test cases for parameters
4. Add test cases for edge cases and error conditions

## Benefits

1. **Comprehensive Coverage**: All JSQL features are tested with exact SQL validation
2. **Regression Prevention**: New changes won't break existing conversions
3. **Documentation**: Test cases serve as examples of JSQL usage
4. **Maintainability**: Easy to add new test cases as features are added
5. **Debugging**: Clear error messages help identify conversion issues quickly

## Potential Challenges

1. **SQL Formatting Variations**: Different SQL generators may format SQL differently
   - **Solution**: Robust normalization function

2. **Dialect Differences**: Some operators are dialect-specific
   - **Solution**: Separate test cases per dialect, use pytest markers

3. **Parameter Placeholders**: Different databases use different placeholder styles
   - **Solution**: Normalize placeholders or use database-agnostic placeholders

4. **Quote Styles**: Some databases use different quote styles for identifiers
   - **Solution**: Normalize quote styles in normalization function

5. **Test Case Volume**: Many test cases may be needed
   - **Solution**: Organize by category, use parametrized tests for efficiency

## Success Criteria

1. All supported JSQL operators have at least one test case
2. All basic query structures (SELECT, FROM, WHERE, etc.) are tested
3. Test failures provide clear, actionable error messages
4. Tests run quickly (< 1 second total for all tests)
5. Test cases are well-organized and easy to maintain

## Open Questions

1. Should we test with multiple SQL dialects in the same test suite?
   - **Recommendation**: Start with SQLite (portable), add dialect-specific tests as needed

2. How strict should SQL normalization be?
   - **Recommendation**: Start with basic normalization, refine based on actual test failures

3. Should we test error cases (invalid JSQL)?
   - **Recommendation**: Keep error tests separate (in `test_converter.py`), focus on valid JSQL→SQL conversion here

4. Should we use actual database execution to validate SQL?
   - **Recommendation**: No, focus on SQL string validation. Database execution is tested in `test_jsql.py`

## Next Steps

1. Review and approve this plan
2. Create test case structure (`tests/data/jsql_to_sql_cases.py`)
3. Implement basic test framework (`tests/test_jsql_to_sql.py`)
4. Add initial test cases (Phase 1)
5. Gradually expand test coverage (Phases 2-4)
