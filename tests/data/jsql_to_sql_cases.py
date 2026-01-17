"""Test cases for JSQL to SQL conversion.

Each test case contains:
- name: Unique test case identifier
- jsql: JSQL query dictionary (input)
- expected_sql: Expected SQL string (output)
- dialect: SQL dialect (default: 'sqlite')
- description: Optional description of what is being tested
"""

from typing import Any


def create_test_case(
    name: str,
    jsql: dict[str, Any],
    expected_sql: str,
    dialect: str = 'sqlite',
    description: str | None = None,
) -> dict[str, Any]:
    """Helper function to create test case dictionary."""
    return {
        'name': name,
        'jsql': jsql,
        'expected_sql': expected_sql,
        'dialect': dialect,
        'description': description,
    }


# Basic Query Structure
BASIC_QUERIES = [
    create_test_case(
        name='simple_select_single_field',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
        },
        expected_sql='SELECT id FROM users',
        description='Simple SELECT with single field',
    ),
    create_test_case(
        name='simple_select_multiple_fields',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}, {'field': 'email'}],
        },
        expected_sql='SELECT id, name, email FROM users',
        description='Simple SELECT with multiple fields',
    ),
    create_test_case(
        name='select_with_wildcard',
        jsql={
            'from': 'users',
            'select': [{'field': '*'}],
        },
        expected_sql='SELECT * FROM users',
        description='SELECT with wildcard (*)',
    ),
    create_test_case(
        name='select_with_alias',
        jsql={
            'from': 'users',
            'select': [{'field': 'name', 'alias': 'user_name'}, {'field': 'email'}],
        },
        expected_sql='SELECT name AS user_name, email FROM users',
        description='SELECT with field alias',
    ),
    create_test_case(
        name='select_with_limit',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'limit': 10,
        },
        expected_sql='SELECT id FROM users LIMIT 10',
        description='SELECT with LIMIT',
    ),
    create_test_case(
        name='select_with_offset',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'offset': 20,
        },
        expected_sql='SELECT id FROM users OFFSET 20',
        description='SELECT with OFFSET',
    ),
    create_test_case(
        name='select_with_limit_and_offset',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'limit': 10,
            'offset': 20,
        },
        expected_sql='SELECT id FROM users LIMIT 10 OFFSET 20',
        description='SELECT with LIMIT and OFFSET',
    ),
    create_test_case(
        name='select_with_order_by_asc',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'order_by': [{'field': 'name', 'direction': 'ASC'}],
        },
        expected_sql='SELECT id, name FROM users ORDER BY name',
        description='SELECT with ORDER BY ASC',
    ),
    create_test_case(
        name='select_with_order_by_desc',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'order_by': [{'field': 'name', 'direction': 'DESC'}],
        },
        expected_sql='SELECT id, name FROM users ORDER BY name DESC',
        description='SELECT with ORDER BY DESC',
    ),
    create_test_case(
        name='select_with_multiple_order_by',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}, {'field': 'department'}],
            'order_by': [
                {'field': 'department', 'direction': 'ASC'},
                {'field': 'name', 'direction': 'ASC'},
            ],
        },
        expected_sql='SELECT id, name, department FROM users ORDER BY department, name',
        description='SELECT with multiple ORDER BY columns',
    ),
]

# Comparison Operators
COMPARISON_OPERATORS = [
    create_test_case(
        name='comparison_operator_equal',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'where': {
                'op': '=',
                'left': {'field': 'active'},
                'right': {'value': True},
            },
        },
        expected_sql='SELECT id FROM users WHERE active = TRUE',
        description='Comparison operator: equals (=)',
    ),
    create_test_case(
        name='comparison_operator_not_equal',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'where': {
                'op': '!=',
                'left': {'field': 'active'},
                'right': {'value': False},
            },
        },
        expected_sql='SELECT id FROM users WHERE active <> FALSE',
        description='Comparison operator: not equals (!=)',
    ),
    create_test_case(
        name='comparison_operator_not_equal_iso',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'where': {
                'op': '<>',
                'left': {'field': 'active'},
                'right': {'value': False},
            },
        },
        expected_sql='SELECT id FROM users WHERE active <> FALSE',
        description='Comparison operator: ISO not equals (<>)',
    ),
    create_test_case(
        name='comparison_operator_less_than',
        jsql={
            'from': 'orders',
            'select': [{'field': 'id'}],
            'where': {
                'op': '<',
                'left': {'field': 'amount'},
                'right': {'value': 100},
            },
        },
        expected_sql='SELECT id FROM orders WHERE amount < 100',
        description='Comparison operator: less than (<)',
    ),
    create_test_case(
        name='comparison_operator_less_than_or_equal',
        jsql={
            'from': 'orders',
            'select': [{'field': 'id'}],
            'where': {
                'op': '<=',
                'left': {'field': 'amount'},
                'right': {'value': 100},
            },
        },
        expected_sql='SELECT id FROM orders WHERE amount <= 100',
        description='Comparison operator: less than or equal (<=)',
    ),
    create_test_case(
        name='comparison_operator_greater_than',
        jsql={
            'from': 'orders',
            'select': [{'field': 'id'}],
            'where': {
                'op': '>',
                'left': {'field': 'amount'},
                'right': {'value': 100},
            },
        },
        expected_sql='SELECT id FROM orders WHERE amount > 100',
        description='Comparison operator: greater than (>)',
    ),
    create_test_case(
        name='comparison_operator_greater_than_or_equal',
        jsql={
            'from': 'orders',
            'select': [{'field': 'id'}],
            'where': {
                'op': '>=',
                'left': {'field': 'amount'},
                'right': {'value': 100},
            },
        },
        expected_sql='SELECT id FROM orders WHERE amount >= 100',
        description='Comparison operator: greater than or equal (>=)',
    ),
]

# Logical Operators
LOGICAL_OPERATORS = [
    create_test_case(
        name='logical_operator_and',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'where': {
                'op': 'AND',
                'conditions': [
                    {'op': '=', 'left': {'field': 'active'}, 'right': {'value': True}},
                    {'op': '=', 'left': {'field': 'department'}, 'right': {'value': 'Engineering'}},
                ],
            },
        },
        expected_sql="SELECT id FROM users WHERE active = TRUE AND department = 'Engineering'",
        description='Logical operator: AND',
    ),
    create_test_case(
        name='logical_operator_or',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'where': {
                'op': 'OR',
                'conditions': [
                    {'op': '=', 'left': {'field': 'department'}, 'right': {'value': 'Engineering'}},
                    {'op': '=', 'left': {'field': 'department'}, 'right': {'value': 'Sales'}},
                ],
            },
        },
        expected_sql="SELECT id FROM users WHERE department = 'Engineering' OR department = 'Sales'",
        description='Logical operator: OR',
    ),
    create_test_case(
        name='logical_operator_not',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}],
            'where': {
                'op': 'NOT',
                'condition': {
                    'op': '=',
                    'left': {'field': 'active'},
                    'right': {'value': True},
                },
            },
        },
        expected_sql='SELECT id FROM users WHERE NOT active = TRUE',
        description='Logical operator: NOT',
    ),
]

# Pattern Matching Operators
PATTERN_MATCHING_OPERATORS = [
    create_test_case(
        name='pattern_matching_like',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'email'}],
            'where': {
                'op': 'LIKE',
                'left': {'field': 'email'},
                'right': {'value': '%@example.com'},
            },
        },
        expected_sql="SELECT id, email FROM users WHERE email LIKE '%@example.com'",
        description='Pattern matching: LIKE operator',
    ),
    create_test_case(
        name='pattern_matching_not_like',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'email'}],
            'where': {
                'op': 'NOT LIKE',
                'left': {'field': 'email'},
                'right': {'value': '%@example.com'},
            },
        },
        expected_sql="SELECT id, email FROM users WHERE NOT email LIKE '%@example.com'",
        description='Pattern matching: NOT LIKE operator',
    ),
    create_test_case(
        name='pattern_matching_ilike',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'where': {
                'op': 'ILIKE',
                'left': {'field': 'name'},
                'right': {'value': '%john%'},
            },
        },
        # SQLite doesn't support ILIKE, so SQLGlot converts it to LOWER(...) LIKE LOWER(...)
        expected_sql="SELECT id, name FROM users WHERE LOWER(name) LIKE LOWER('%john%')",
        description='Pattern matching: ILIKE operator (case-insensitive LIKE)',
    ),
    create_test_case(
        name='pattern_matching_not_ilike',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'where': {
                'op': 'NOT ILIKE',
                'left': {'field': 'name'},
                'right': {'value': '%john%'},
            },
        },
        # SQLite doesn't support ILIKE, so SQLGlot converts it to NOT LOWER(...) LIKE LOWER(...)
        expected_sql="SELECT id, name FROM users WHERE NOT LOWER(name) LIKE LOWER('%john%')",
        description='Pattern matching: NOT ILIKE operator (case-insensitive NOT LIKE)',
    ),
    create_test_case(
        name='pattern_matching_similar_to',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'email'}],
            'where': {
                'op': 'SIMILAR TO',
                'left': {'field': 'email'},
                'right': {'value': '%@example\\.(com|org)'},
            },
        },
        expected_sql="SELECT id, email FROM users WHERE email SIMILAR TO '%@example\\.(com|org)'",
        description='Pattern matching: SIMILAR TO operator (SQL regex)',
    ),
    create_test_case(
        name='pattern_matching_regexp',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'email'}],
            'where': {
                'op': 'REGEXP',
                'left': {'field': 'email'},
                'right': {'value': '^[a-z]+@example\\.com$'},
            },
        },
        # SQLite converts REGEXP to REGEXP_LIKE function
        expected_sql="SELECT id, email FROM users WHERE REGEXP_LIKE(email, '^[a-z]+@example\\.com$')",
        description='Pattern matching: REGEXP operator (regex matching)',
    ),
    create_test_case(
        name='pattern_matching_rlike',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'email'}],
            'where': {
                'op': 'RLIKE',
                'left': {'field': 'email'},
                'right': {'value': '^[a-z]+@example\\.com$'},
            },
        },
        # SQLite converts RLIKE to REGEXP_LIKE function
        expected_sql="SELECT id, email FROM users WHERE REGEXP_LIKE(email, '^[a-z]+@example\\.com$')",
        description='Pattern matching: RLIKE operator (MySQL alias for REGEXP)',
    ),
]

# Membership and Existence Operators
MEMBERSHIP_OPERATORS = [
    create_test_case(
        name='membership_operator_in',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'where': {
                'op': 'IN',
                'left': {'field': 'department'},
                'right': {'values': ['Engineering', 'Sales', 'Marketing']},
            },
        },
        expected_sql="SELECT id, name FROM users WHERE department IN ('Engineering', 'Sales', 'Marketing')",
        description='Membership operator: IN with value list',
    ),
    create_test_case(
        name='membership_operator_not_in',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'name'}],
            'where': {
                'op': 'NOT IN',
                'left': {'field': 'department'},
                'right': {'values': ['Engineering', 'Sales']},
            },
        },
        expected_sql="SELECT id, name FROM users WHERE NOT department IN ('Engineering', 'Sales')",
        description='Membership operator: NOT IN with value list',
    ),
]

# Range Operators
RANGE_OPERATORS = [
    create_test_case(
        name='range_operator_between',
        jsql={
            'from': 'orders',
            'select': [{'field': 'id'}, {'field': 'amount'}],
            'where': {
                'op': 'BETWEEN',
                'expr': {'field': 'amount'},
                'low': {'value': 100},
                'high': {'value': 1000},
            },
        },
        expected_sql='SELECT id, amount FROM orders WHERE amount BETWEEN 100 AND 1000',
        description='Range operator: BETWEEN',
    ),
    create_test_case(
        name='range_operator_not_between',
        jsql={
            'from': 'orders',
            'select': [{'field': 'id'}, {'field': 'amount'}],
            'where': {
                'op': 'NOT BETWEEN',
                'expr': {'field': 'amount'},
                'low': {'value': 100},
                'high': {'value': 1000},
            },
        },
        expected_sql='SELECT id, amount FROM orders WHERE NOT amount BETWEEN 100 AND 1000',
        description='Range operator: NOT BETWEEN',
    ),
]

# Null Checks
NULL_CHECKS = [
    create_test_case(
        name='null_check_is_null',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'email'}],
            'where': {
                'op': 'IS NULL',
                'field': 'email',
            },
        },
        expected_sql='SELECT id, email FROM users WHERE email IS NULL',
        description='Null check: IS NULL',
    ),
    create_test_case(
        name='null_check_is_not_null',
        jsql={
            'from': 'users',
            'select': [{'field': 'id'}, {'field': 'email'}],
            'where': {
                'op': 'IS NOT NULL',
                'field': 'email',
            },
        },
        expected_sql='SELECT id, email FROM users WHERE NOT email IS NULL',
        description='Null check: IS NOT NULL',
    ),
]

# JOINs
JOINS = [
    create_test_case(
        name='join_inner',
        jsql={
            'from': 'users',
            'select': [{'field': 'users.id'}, {'field': 'users.name'}, {'field': 'orders.total'}],
            'joins': [
                {
                    'type': 'INNER',
                    'entity': 'orders',
                    'on': {
                        'op': '=',
                        'left': {'field': 'users.id'},
                        'right': {'field': 'orders.user_id'},
                    },
                },
            ],
        },
        expected_sql='SELECT users.id, users.name, orders.total FROM users INNER JOIN orders ON users.id = orders.user_id',
        description='INNER JOIN',
    ),
    create_test_case(
        name='join_left',
        jsql={
            'from': 'users',
            'select': [{'field': 'users.id'}, {'field': 'users.name'}, {'field': 'orders.total'}],
            'joins': [
                {
                    'type': 'LEFT',
                    'entity': 'orders',
                    'on': {
                        'op': '=',
                        'left': {'field': 'users.id'},
                        'right': {'field': 'orders.user_id'},
                    },
                },
            ],
        },
        expected_sql='SELECT users.id, users.name, orders.total FROM users LEFT JOIN orders ON users.id = orders.user_id',
        description='LEFT JOIN',
    ),
    create_test_case(
        name='join_right',
        jsql={
            'from': 'users',
            'select': [{'field': 'users.id'}, {'field': 'users.name'}, {'field': 'orders.total'}],
            'joins': [
                {
                    'type': 'RIGHT',
                    'entity': 'orders',
                    'on': {
                        'op': '=',
                        'left': {'field': 'users.id'},
                        'right': {'field': 'orders.user_id'},
                    },
                },
            ],
        },
        expected_sql='SELECT users.id, users.name, orders.total FROM users RIGHT JOIN orders ON users.id = orders.user_id',
        description='RIGHT JOIN',
    ),
    create_test_case(
        name='join_full',
        jsql={
            'from': 'users',
            'select': [{'field': 'users.id'}, {'field': 'users.name'}, {'field': 'orders.total'}],
            'joins': [
                {
                    'type': 'FULL',
                    'entity': 'orders',
                    'on': {
                        'op': '=',
                        'left': {'field': 'users.id'},
                        'right': {'field': 'orders.user_id'},
                    },
                },
            ],
        },
        expected_sql='SELECT users.id, users.name, orders.total FROM users FULL JOIN orders ON users.id = orders.user_id',
        description='FULL OUTER JOIN',
    ),
    create_test_case(
        name='join_with_alias',
        jsql={
            'from': 'users',
            'select': [{'field': 'u.id'}, {'field': 'u.name'}, {'field': 'o.total'}],
            'joins': [
                {
                    'type': 'LEFT',
                    'entity': 'orders',
                    'alias': 'o',
                    'on': {
                        'op': '=',
                        'left': {'field': 'users.id'},
                        'right': {'field': 'o.user_id'},
                    },
                },
            ],
        },
        expected_sql='SELECT u.id, u.name, o.total FROM users LEFT JOIN orders AS o ON users.id = o.user_id',
        description='JOIN with alias',
    ),
    create_test_case(
        name='join_cross',
        jsql={
            'from': 'users',
            'select': [{'field': 'users.id'}, {'field': 'users.name'}, {'field': 'orders.id'}],
            'joins': [
                {
                    'type': 'CROSS',
                    'entity': 'orders',
                },
            ],
        },
        expected_sql='SELECT users.id, users.name, orders.id FROM users CROSS JOIN orders',
        description='CROSS JOIN',
    ),
]

# Subqueries
SUBQUERIES = [
    create_test_case(
        name='subquery_in_where_comparison',
        jsql={
            'from': 'orders',
            'select': [{'field': 'id'}, {'field': 'amount'}],
            'where': {
                'op': '>',
                'left': {'field': 'amount'},
                'right': {'from': 'orders', 'select': [{'field': 'amount'}], 'limit': 1},
            },
        },
        expected_sql='SELECT id, amount FROM orders WHERE amount > (SELECT amount FROM orders LIMIT 1)',
        description='Subquery in WHERE clause (comparison)',
    ),
    create_test_case(
        name='subquery_in_where_in',
        jsql={
            'from': 'orders',
            'select': [{'field': 'id'}, {'field': 'amount'}],
            'where': {
                'op': 'IN',
                'left': {'field': 'user_id'},
                'right': {'from': 'users', 'select': [{'field': 'id'}], 'where': {'op': '=', 'left': {'field': 'active'}, 'right': {'value': True}}},
            },
        },
        expected_sql='SELECT id, amount FROM orders WHERE user_id IN ( (SELECT id FROM users WHERE active = TRUE))',
        description='Subquery in WHERE clause (IN)',
    ),
]

# CTE (Common Table Expressions)
CTES = [
    create_test_case(
        name='cte_single',
        jsql={
            'with': [
                {
                    'name': 'active_users',
                    'query': {
                        'from': 'users',
                        'select': [{'field': 'id'}, {'field': 'name'}],
                        'where': {'op': '=', 'left': {'field': 'active'}, 'right': {'value': True}},
                    },
                },
            ],
            'from': 'active_users',
            'select': [{'field': 'id'}, {'field': 'name'}],
        },
        expected_sql='WITH active_users AS (SELECT id, name FROM users WHERE active = TRUE) SELECT id, name FROM active_users',
        description='Single CTE',
    ),
    create_test_case(
        name='cte_multiple',
        jsql={
            'with': [
                {
                    'name': 'active_users',
                    'query': {
                        'from': 'users',
                        'select': [{'field': 'id'}],
                        'where': {'op': '=', 'left': {'field': 'active'}, 'right': {'value': True}},
                    },
                },
                {
                    'name': 'user_orders',
                    'query': {
                        'from': 'orders',
                        'select': [{'field': 'user_id'}, {'field': 'amount'}],
                        'where': {'op': '>', 'left': {'field': 'amount'}, 'right': {'value': 100}},
                    },
                },
            ],
            'from': 'active_users',
            'select': [{'field': 'id'}],
            'joins': [
                {
                    'type': 'LEFT',
                    'entity': 'user_orders',
                    'on': {
                        'op': '=',
                        'left': {'field': 'active_users.id'},
                        'right': {'field': 'user_orders.user_id'},
                    },
                },
            ],
        },
        expected_sql='WITH active_users AS (SELECT id FROM users WHERE active = TRUE), user_orders AS (SELECT user_id, amount FROM orders WHERE amount > 100) SELECT id FROM active_users LEFT JOIN user_orders ON active_users.id = user_orders.user_id',
        description='Multiple CTEs',
    ),
]

# Window Functions
WINDOW_FUNCTIONS = [
    create_test_case(
        name='window_function_row_number',
        jsql={
            'from': 'orders',
            'select': [
                {'field': 'id'},
                {'field': 'user_id'},
                {'field': 'amount'},
                {
                    'func': 'ROW_NUMBER',
                    'over': {
                        'partition_by': [{'field': 'user_id'}],
                        'order_by': [{'field': 'amount', 'direction': 'DESC'}],
                    },
                },
            ],
        },
        expected_sql='SELECT id, user_id, amount, ROW_NUMBER() OVER (PARTITION BY user_id ORDER BY amount DESC) FROM orders',
        description='Window function: ROW_NUMBER() with PARTITION BY and ORDER BY',
    ),
    create_test_case(
        name='window_function_partition_by_only',
        jsql={
            'from': 'orders',
            'select': [
                {'field': 'id'},
                {'field': 'user_id'},
                {'field': 'amount'},
                {
                    'func': 'COUNT',
                    'args': [{'field': 'id'}],
                    'over': {
                        'partition_by': [{'field': 'user_id'}],
                    },
                },
            ],
        },
        expected_sql='SELECT id, user_id, amount, COUNT(id) OVER (PARTITION BY user_id) FROM orders',
        description='Window function: COUNT() with PARTITION BY only',
    ),
]

# Combine all test cases
JSQL_TO_SQL_TEST_CASES = (
    BASIC_QUERIES
    + COMPARISON_OPERATORS
    + LOGICAL_OPERATORS
    + PATTERN_MATCHING_OPERATORS
    + MEMBERSHIP_OPERATORS
    + RANGE_OPERATORS
    + NULL_CHECKS
    + JOINS
    + SUBQUERIES
    + CTES
    + WINDOW_FUNCTIONS
)
