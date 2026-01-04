"""Tests for JSQL functionality."""

from datetime import date

import pytest
import pytest_asyncio
from sqlalchemy import Column
from sqlalchemy import Date
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import Numeric
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import JSQLExecutionError
from namerec.uma import JSQLSyntaxError
from namerec.uma import initialize_uma
from namerec.uma import uma_select


@pytest_asyncio.fixture
async def test_db():
    """Create test database with sample data."""
    # Create async engine
    engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)

    # Create metadata
    metadata = MetaData()

    # Define tables
    customers = Table(
        'customers',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(255), nullable=False),
        Column('email', String(255)),
        Column('department', String(100)),
        Column('active', Integer, default=1),  # SQLite doesn't have BOOLEAN
    )

    orders = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('customer_id', Integer, ForeignKey('customers.id')),
        Column('order_date', Date, nullable=False),
        Column('amount', Numeric(10, 2), nullable=False),
        Column('quantity', Integer, nullable=False),
    )

    # Create tables and insert data using async engine
    async with engine.begin() as conn:
        # Create all tables
        await conn.run_sync(metadata.create_all)

        # Insert test data
        await conn.execute(
            customers.insert(),
            [
                {'id': 1, 'name': 'John Doe', 'email': 'john@example.com', 'department': 'Engineering', 'active': 1},
                {'id': 2, 'name': 'Jane Smith', 'email': 'jane@example.com', 'department': 'Sales', 'active': 1},
                {'id': 3, 'name': 'Bob Johnson', 'email': 'bob@example.com', 'department': 'Engineering', 'active': 0},
            ],
        )
        await conn.execute(
            orders.insert(),
            [
                {'id': 1, 'customer_id': 1, 'order_date': date(2024, 1, 15), 'amount': 100.00, 'quantity': 2},
                {'id': 2, 'customer_id': 1, 'order_date': date(2024, 2, 20), 'amount': 250.00, 'quantity': 5},
                {'id': 3, 'customer_id': 2, 'order_date': date(2024, 1, 10), 'amount': 75.50, 'quantity': 1},
            ],
        )

    # Initialize UMA
    initialize_uma(engine, metadata)

    yield engine, metadata

    # Cleanup
    await engine.dispose()


@pytest.mark.asyncio
async def test_simple_select(test_db):
    """Test simple SELECT query."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
    }

    result = await uma_select(jsql)

    assert 'meta' in result
    assert 'data' in result
    assert len(result['meta']) == 3
    assert len(result['data']) == 3
    assert result['meta'][0]['name'] == 'id'
    assert result['meta'][1]['name'] == 'name'
    assert result['meta'][2]['name'] == 'email'


@pytest.mark.asyncio
async def test_select_with_where(test_db):
    """Test SELECT with WHERE clause."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': '=',
            'left': {'field': 'department'},
            'right': {'value': 'Engineering'},
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # John Doe and Bob Johnson


@pytest.mark.asyncio
async def test_select_with_and_condition(test_db):
    """Test SELECT with AND condition."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': 'AND',
            'conditions': [
                {
                    'op': '=',
                    'left': {'field': 'department'},
                    'right': {'value': 'Engineering'},
                },
                {
                    'op': '=',
                    'left': {'field': 'active'},
                    'right': {'value': 1},
                },
            ],
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 1  # Only John Doe is active in Engineering


@pytest.mark.asyncio
async def test_select_with_alias(test_db):
    """Test SELECT with column aliases."""
    jsql = {
        'from': 'customers',
        'select': [
            {'field': 'id'},
            {'field': 'name', 'alias': 'customer_name'},
            {'field': 'email', 'alias': 'contact_email'},
        ],
        'limit': 1,
    }

    result = await uma_select(jsql)

    assert result['meta'][1]['name'] == 'customer_name'
    assert result['meta'][2]['name'] == 'contact_email'


@pytest.mark.asyncio
async def test_aggregation(test_db):
    """Test aggregation functions."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'COUNT', 'args': [{'field': 'id'}], 'alias': 'order_count'},
            {'func': 'SUM', 'args': [{'field': 'amount'}], 'alias': 'total'},
        ],
        'group_by': [{'field': 'customer_id'}],
    }

    result = await uma_select(jsql)

    assert len(result['meta']) == 3
    assert result['meta'][1]['name'] == 'order_count'
    assert result['meta'][2]['name'] == 'total'
    assert len(result['data']) == 2  # Two customers with orders


@pytest.mark.asyncio
async def test_join(test_db):
    """Test JOIN query."""
    jsql = {
        'from': 'orders',
        'joins': [
            {
                'type': 'LEFT',
                'table': 'customers',
                'alias': 'c',
                'on': {
                    'op': '=',
                    'left': {'field': 'orders.customer_id'},
                    'right': {'field': 'c.id'},
                },
            },
        ],
        'select': [
            {'field': 'orders.id'},
            {'field': 'c.name', 'alias': 'customer_name'},
            {'field': 'orders.amount'},
        ],
    }

    result = await uma_select(jsql)

    assert len(result['meta']) == 3
    assert result['meta'][1]['name'] == 'customer_name'
    assert len(result['data']) == 3  # Three orders


@pytest.mark.asyncio
async def test_order_by(test_db):
    """Test ORDER BY clause."""
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'order_by': [{'field': 'amount', 'direction': 'desc'}],
    }

    result = await uma_select(jsql)

    # Check that amounts are in descending order
    amounts = [row[1] for row in result['data']]
    assert amounts == sorted(amounts, reverse=True)


@pytest.mark.asyncio
async def test_limit_offset(test_db):
    """Test LIMIT and OFFSET."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'order_by': [{'field': 'id', 'direction': 'asc'}],
        'limit': 2,
        'offset': 1,
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2
    assert result['data'][0][0] == 2  # Second customer


@pytest.mark.asyncio
async def test_expression(test_db):
    """Test expression in SELECT."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'amount'},
            {'field': 'quantity'},
            {
                'expr': {
                    'op': '*',
                    'left': {'field': 'amount'},
                    'right': {'value': 1.1},
                },
                'alias': 'amount_with_tax',
            },
        ],
        'limit': 1,
    }

    result = await uma_select(jsql)

    assert result['meta'][3]['name'] == 'amount_with_tax'
    # Check that calculation is correct
    amount = float(result['data'][0][1])
    amount_with_tax = float(result['data'][0][3])
    assert abs(amount_with_tax - amount * 1.1) < 0.01


@pytest.mark.asyncio
async def test_cte(test_db):
    """Test CTE (Common Table Expression)."""
    jsql = {
        'with': [
            {
                'name': 'active_customers',
                'query': {
                    'from': 'customers',
                    'select': ['id', 'name'],
                    'where': {
                        'op': '=',
                        'left': {'field': 'active'},
                        'right': {'value': 1},
                    },
                },
            },
        ],
        'from': 'active_customers',
        'select': ['*'],
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # Two active customers


@pytest.mark.asyncio
async def test_subquery_in(test_db):
    """Test subquery with IN operator."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': 'IN',
            'left': {'field': 'id'},
            'right': {
                'subquery': {
                    'from': 'orders',
                    'select': [{'field': 'customer_id'}],
                },
            },
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # Customers who have orders


@pytest.mark.asyncio
async def test_query_parameters(test_db):
    """Test query with parameters."""
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }

    params = {'min_amount': 100}
    result = await uma_select(jsql, params)

    # Should get orders with amount >= 100
    assert len(result['data']) == 2


@pytest.mark.asyncio
async def test_invalid_jsql_missing_from(test_db):
    """Test error handling for missing 'from' field."""
    jsql = {
        'select': ['id', 'name'],
    }

    with pytest.raises(ValueError, match='must contain'):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_invalid_jsql_unknown_table(test_db):
    """Test error handling for unknown table."""
    jsql = {
        'from': 'nonexistent_table',
        'select': ['id'],
    }

    with pytest.raises((JSQLSyntaxError, JSQLExecutionError)):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_invalid_jsql_unknown_column(test_db):
    """Test error handling for unknown column."""
    jsql = {
        'from': 'customers',
        'select': ['nonexistent_column'],
    }

    with pytest.raises((JSQLSyntaxError, JSQLExecutionError)):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_result_metadata_types(test_db):
    """Test that result metadata includes type information."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
        'limit': 1,
    }

    result = await uma_select(jsql)

    # Check metadata structure
    for col_meta in result['meta']:
        assert 'name' in col_meta
        assert 'type' in col_meta
        assert 'nullable' in col_meta
        assert 'qualified_name' in col_meta
        assert 'source_entity' in col_meta
        assert 'source_field' in col_meta


@pytest.mark.asyncio
async def test_having_clause(test_db):
    """Test HAVING clause with aggregation."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'COUNT', 'args': [{'field': 'id'}], 'alias': 'order_count'},
        ],
        'group_by': [{'field': 'customer_id'}],
        'having': {
            'op': '>',
            'left': {'func': 'COUNT', 'args': [{'field': 'id'}]},
            'right': {'value': 1},
        },
    }

    result = await uma_select(jsql)

    # Only customer with more than 1 order
    assert len(result['data']) == 1
    assert result['data'][0][1] == 2  # Customer 1 has 2 orders


@pytest.mark.asyncio
async def test_window_function_row_number(test_db):
    """Test window function ROW_NUMBER."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'customer_id'},
            {'field': 'amount'},
            {
                'window_func': {
                    'func': 'ROW_NUMBER',
                    'partition_by': [{'field': 'customer_id'}],
                    'order_by': [{'field': 'amount', 'direction': 'desc'}],
                },
                'alias': 'row_num',
            },
        ],
    }

    result = await uma_select(jsql)

    # Check that we have row numbers
    assert len(result['meta']) == 4
    assert result['meta'][3]['name'] == 'row_num'
    assert len(result['data']) == 3

    # Check row numbers are correct (should restart for each customer)
    # Customer 1 has 2 orders: amounts 250.00 (rank 1), 100.00 (rank 2)
    customer_1_orders = [row for row in result['data'] if row[1] == 1]
    assert len(customer_1_orders) == 2
    # The order with amount 250.00 should have row_num = 1
    assert any(row[2] == 250.00 and row[3] == 1 for row in customer_1_orders)


@pytest.mark.asyncio
async def test_window_function_avg(test_db):
    """Test window function AVG."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'customer_id'},
            {'field': 'amount'},
            {
                'window_func': {
                    'func': 'AVG',
                    'args': [{'field': 'amount'}],
                    'partition_by': [{'field': 'customer_id'}],
                },
                'alias': 'avg_amount',
            },
        ],
    }

    result = await uma_select(jsql)

    # Check metadata
    assert result['meta'][3]['name'] == 'avg_amount'

    # Customer 1 has 2 orders: 100.00 and 250.00, avg = 175.00
    customer_1_orders = [row for row in result['data'] if row[1] == 1]
    assert len(customer_1_orders) == 2
    # Both rows should have the same average
    for row in customer_1_orders:
        assert abs(float(row[3]) - 175.00) < 0.01


@pytest.mark.asyncio
async def test_order_by_alias(test_db):
    """Test ORDER BY with column alias from aggregation."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'SUM', 'args': [{'field': 'amount'}], 'alias': 'total'},
        ],
        'group_by': [{'field': 'customer_id'}],
        'order_by': [{'field': 'total', 'direction': 'desc'}],
    }

    result = await uma_select(jsql)

    # Check that results are ordered by total descending
    totals = [float(row[1]) for row in result['data']]
    assert totals == sorted(totals, reverse=True)
    # Customer 1 should be first (has 2 orders: 100 + 250 = 350)
    assert result['data'][0][0] == 1
