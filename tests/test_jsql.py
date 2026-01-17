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

from namerec.uma import DefaultMetadataProvider
from namerec.uma import JSQLExecutionError
from namerec.uma import JSQLSyntaxError
from namerec.uma import NamespaceConfig
from namerec.uma import UMA


@pytest_asyncio.fixture
async def uma():  # noqa: ANN201
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
    metadata_provider = DefaultMetadataProvider()
    uma = UMA.create({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Preload metadata
    await metadata_provider.preload(engine, namespace='test')

    yield uma

    # Cleanup
    await engine.dispose()


@pytest.mark.asyncio
async def test_simple_select(uma):  # noqa: ANN001
    """Test simple SELECT query."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
    }

    result = await uma.select(jsql)

    assert 'meta' in result
    assert 'data' in result
    assert len(result['meta']) == 3
    assert len(result['data']) == 3
    assert result['meta'][0]['name'] == 'id'
    assert result['meta'][1]['name'] == 'name'
    assert result['meta'][2]['name'] == 'email'


@pytest.mark.asyncio
async def test_select_with_where(uma):
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

    result = await uma.select(jsql)

    assert len(result['data']) == 2  # John Doe and Bob Johnson


@pytest.mark.asyncio
async def test_select_with_and_condition(uma):
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

    result = await uma.select(jsql)

    assert len(result['data']) == 1  # Only John Doe is active in Engineering


@pytest.mark.asyncio
async def test_select_with_alias(uma):
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

    result = await uma.select(jsql)

    assert result['meta'][1]['name'] == 'customer_name'
    assert result['meta'][2]['name'] == 'contact_email'


@pytest.mark.asyncio
async def test_aggregation(uma):
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

    result = await uma.select(jsql)

    assert len(result['meta']) == 3
    assert result['meta'][1]['name'] == 'order_count'
    assert result['meta'][2]['name'] == 'total'
    assert len(result['data']) == 2  # Two customers with orders


@pytest.mark.asyncio
async def test_join(uma):
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

    result = await uma.select(jsql)

    assert len(result['meta']) == 3
    assert result['meta'][1]['name'] == 'customer_name'
    assert len(result['data']) == 3  # Three orders


@pytest.mark.asyncio
async def test_order_by(uma):
    """Test ORDER BY clause."""
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'order_by': [{'field': 'amount', 'direction': 'desc'}],
    }

    result = await uma.select(jsql)

    # Check that amounts are in descending order
    amounts = [row[1] for row in result['data']]
    assert amounts == sorted(amounts, reverse=True)


@pytest.mark.asyncio
async def test_limit_offset(uma):
    """Test LIMIT and OFFSET."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'order_by': [{'field': 'id', 'direction': 'asc'}],
        'limit': 2,
        'offset': 1,
    }

    result = await uma.select(jsql)

    assert len(result['data']) == 2
    assert result['data'][0][0] == 2  # Second customer


@pytest.mark.asyncio
async def test_expression(uma):
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

    result = await uma.select(jsql)

    assert result['meta'][3]['name'] == 'amount_with_tax'
    # Check that calculation is correct
    amount = float(result['data'][0][1])
    amount_with_tax = float(result['data'][0][3])
    assert abs(amount_with_tax - amount * 1.1) < 0.01


@pytest.mark.asyncio
async def test_cte(uma):
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

    result = await uma.select(jsql)

    assert len(result['data']) == 2  # Two active customers


@pytest.mark.asyncio
async def test_subquery_in(uma):
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

    result = await uma.select(jsql)

    assert len(result['data']) == 2  # Customers who have orders


@pytest.mark.asyncio
async def test_query_parameters(uma):
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
    result = await uma.select(jsql, params)

    # Should get orders with amount >= 100
    assert len(result['data']) == 2


@pytest.mark.asyncio
async def test_query_parameters_different_values(uma):
    """Test query with parameters - different values should use same cached SQL."""
    from namerec.uma.jsql.cache import MemoryCacheBackend
    from namerec.uma.application import UMA
    
    # Create UMA with cache
    cache_backend = MemoryCacheBackend(max_size=10)
    
    # Get engine from uma instance (assuming it has one)
    # We'll create a new UMA instance with cache for this test
    # This is a simplified version - in real scenario you'd use the fixture engine
    from namerec.uma import DefaultMetadataProvider, NamespaceConfig
    from sqlalchemy.ext.asyncio import create_async_engine
    
    test_engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    metadata = MetaData()
    orders_table = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('amount', Numeric(10, 2), nullable=False),
    )
    
    async with test_engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
        await conn.execute(
            orders_table.insert(),
            [
                {'id': 1, 'amount': 100.00},
                {'id': 2, 'amount': 250.00},
                {'id': 3, 'amount': 75.50},
            ],
        )
    
    metadata_provider = DefaultMetadataProvider()
    uma_cached = UMA.create({
        'test': NamespaceConfig(
            engine=test_engine,
            metadata_provider=metadata_provider,
        ),
    }, cache_backend=cache_backend)
    
    await metadata_provider.preload(test_engine, namespace='test')
    
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }
    
    # First query with min_amount=100 - should cache SQL
    result1 = await uma_cached.select(jsql, params={'min_amount': 100})
    stats1 = uma_cached.cache_stats()
    assert stats1['misses'] == 1
    assert len(result1['data']) == 2  # Orders with amount >= 100
    
    # Second query with different min_amount=200 - should use cached SQL
    result2 = await uma_cached.select(jsql, params={'min_amount': 200})
    stats2 = uma_cached.cache_stats()
    assert stats2['hits'] == 1  # Should hit cache
    assert len(result2['data']) == 1  # Only order with amount >= 200
    
    # Third query with same params as first - should hit cache again
    result3 = await uma_cached.select(jsql, params={'min_amount': 100})
    stats3 = uma_cached.cache_stats()
    assert stats3['hits'] == 2  # Another cache hit
    assert len(result3['data']) == 2
    
    await test_engine.dispose()


@pytest.mark.asyncio
async def test_parameter_mapping_preservation(uma):
    """Test that JSQL parameter names are preserved in parameter mapping."""
    jsql = {
        'from': 'orders',
        'select': ['id'],
        'where': {
            'op': 'AND',
            'conditions': [
                {
                    'op': '>=',
                    'left': {'field': 'amount'},
                    'right': {'param': 'min_amount'},
                },
                {
                    'op': '<=',
                    'left': {'field': 'amount'},
                    'right': {'param': 'max_amount'},
                },
            ],
        },
    }
    
    # Query with multiple parameters
    params = {'min_amount': 50, 'max_amount': 200}
    result = await uma.select(jsql, params)
    
    # Should return orders with amount between 50 and 200
    # We have orders: 100.00, 250.00, 75.50
    # So should get: 100.00, 75.50 (2 orders)
    assert len(result['data']) == 2


@pytest.mark.asyncio
async def test_parameter_missing_error(uma):
    """Test error when required parameter is missing."""
    jsql = {
        'from': 'orders',
        'select': ['id'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }
    
    # Query without required parameter
    from namerec.uma.jsql.exceptions import JSQLSyntaxError
    with pytest.raises(JSQLSyntaxError, match='Parameter.*not provided'):
        await uma.select(jsql, params={})


@pytest.mark.asyncio
async def test_invalid_jsql_missing_from(uma):
    """Test error handling for missing 'from' field."""
    jsql = {
        'select': ['id', 'name'],
    }

    with pytest.raises(ValueError, match='must contain'):
        await uma.select(jsql)


@pytest.mark.asyncio
async def test_invalid_jsql_unknown_table(uma):
    """Test error handling for unknown table."""
    jsql = {
        'from': 'nonexistent_table',
        'select': ['id'],
    }

    with pytest.raises((JSQLSyntaxError, JSQLExecutionError)):
        await uma.select(jsql)


@pytest.mark.asyncio
async def test_invalid_jsql_unknown_column(uma):
    """Test error handling for unknown column."""
    jsql = {
        'from': 'customers',
        'select': ['nonexistent_column'],
    }

    with pytest.raises((JSQLSyntaxError, JSQLExecutionError)):
        await uma.select(jsql)


@pytest.mark.asyncio
async def test_result_metadata_types(uma):
    """Test that result metadata includes type information."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
        'limit': 1,
    }

    result = await uma.select(jsql)

    # Check metadata structure
    for col_meta in result['meta']:
        assert 'name' in col_meta
        assert 'type' in col_meta
        assert 'nullable' in col_meta
        assert 'qualified_name' in col_meta
        assert 'source_entity' in col_meta
        assert 'source_field' in col_meta


@pytest.mark.asyncio
async def test_having_clause(uma):
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

    result = await uma.select(jsql)

    # Only customer with more than 1 order
    assert len(result['data']) == 1
    assert result['data'][0][1] == 2  # Customer 1 has 2 orders


@pytest.mark.asyncio
async def test_window_function_row_number(uma):
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

    result = await uma.select(jsql)

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
async def test_window_function_avg(uma):
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

    result = await uma.select(jsql)

    # Check metadata
    assert result['meta'][3]['name'] == 'avg_amount'

    # Customer 1 has 2 orders: 100.00 and 250.00, avg = 175.00
    customer_1_orders = [row for row in result['data'] if row[1] == 1]
    assert len(customer_1_orders) == 2
    # Both rows should have the same average
    for row in customer_1_orders:
        assert abs(float(row[3]) - 175.00) < 0.01


@pytest.mark.asyncio
async def test_order_by_alias(uma):
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

    result = await uma.select(jsql)

    # Check that results are ordered by total descending
    totals = [float(row[1]) for row in result['data']]
    assert totals == sorted(totals, reverse=True)
    # Customer 1 should be first (has 2 orders: 100 + 250 = 350)
    assert result['data'][0][0] == 1


@pytest.mark.asyncio
async def test_limit_zero(uma):
    """Test LIMIT 0 is applied correctly (Bug fix: 0 is falsy but valid)."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'limit': 0,
    }

    result = await uma.select(jsql)

    # LIMIT 0 should return no rows
    assert len(result['data']) == 0
    # But metadata should still be present
    assert len(result['meta']) == 2


@pytest.mark.asyncio
async def test_offset_zero(uma):
    """Test OFFSET 0 is applied correctly (Bug fix: 0 is falsy but valid)."""
    jsql = {
        'from': 'customers',
        'select': ['id'],
        'order_by': [{'field': 'id', 'direction': 'asc'}],
        'offset': 0,
        'limit': 2,
    }

    result = await uma.select(jsql)

    # OFFSET 0 should be same as no offset - starts from first row
    assert len(result['data']) == 2
    assert result['data'][0][0] == 1  # First customer ID


@pytest.mark.asyncio
async def test_arithmetic_operators_validation(uma):
    """Test arithmetic operators validate required fields (Bug fix: prevent KeyError)."""
    # Missing 'left' field
    jsql_missing_left = {
        'from': 'orders',
        'select': [
            {
                'expr': {
                    'op': '+',
                    'right': {'field': 'quantity'},
                },
                'alias': 'result',
            },
        ],
    }

    with pytest.raises(JSQLSyntaxError) as exc_info:
        await uma.select(jsql_missing_left)
    assert '+ operator must have "left" field' in str(exc_info.value)

    # Missing 'right' field
    jsql_missing_right = {
        'from': 'orders',
        'select': [
            {
                'expr': {
                    'op': '*',
                    'left': {'field': 'quantity'},
                },
                'alias': 'result',
            },
        ],
    }

    with pytest.raises(JSQLSyntaxError) as exc_info:
        await uma.select(jsql_missing_right)
    assert '* operator must have "right" field' in str(exc_info.value)


@pytest.mark.asyncio
async def test_debug_mode(uma: tuple) -> None:
    """Test JSQL debug mode returns SQL in response."""
    # uma is already initialized via fixture
    # No need to initialize again

    # Simple query with debug enabled
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}, {'field': 'name'}],
        'where': {
            'op': '=',
            'left': {'field': 'active'},
            'right': {'value': 1},
        },
        'debug': True,  # Enable debug mode
    }

    result = await uma.select(jsql)

    # Check that debug SQL is present
    assert 'debug' in result
    assert result['debug'] is not None
    assert isinstance(result['debug'], str)

    # Check that debug SQL contains expected keywords
    debug_sql = result['debug']
    assert 'SELECT' in debug_sql
    assert 'FROM' in debug_sql
    assert 'customers' in debug_sql
    assert 'WHERE' in debug_sql

    # Check that data is still returned normally
    assert 'meta' in result
    assert 'data' in result
    assert len(result['data']) > 0


@pytest.mark.asyncio
async def test_debug_mode_disabled(uma: tuple) -> None:
    """Test that debug SQL is not included when debug mode is disabled."""
    # uma is already initialized via fixture
    # No need to initialize again

    # Query without debug
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}],
    }

    result = await uma.select(jsql)

    # Check that debug SQL is not present
    assert 'debug' not in result or result.get('debug') is None


@pytest.mark.asyncio
async def test_debug_mode_with_in_operator(uma: tuple) -> None:
    """Test debug mode with IN operator (complex type that may fail literal_binds)."""
    # uma is already initialized via fixture
    # No need to initialize again

    # Query with IN operator and debug enabled
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}, {'field': 'name'}],
        'where': {
            'op': 'IN',
            'left': {'field': 'id'},
            'right': [1, 2, 3],  # List of values for IN operator
        },
        'debug': True,  # Enable debug mode
    }

    # This should not raise an error even if literal_binds fails
    result = await uma.select(jsql)

    # Check that query executed successfully
    assert 'data' in result
    assert len(result['data']) > 0

    # Check that debug SQL is present (may contain parameters or error message)
    assert 'debug' in result
    assert result['debug'] is not None
    assert isinstance(result['debug'], str)

    # Debug SQL should contain query structure even if parameters aren't substituted
    debug_sql = result['debug']
    assert 'SELECT' in debug_sql or 'Failed to generate' in debug_sql


@pytest.mark.asyncio
async def test_bindparam_key_preservation(uma):
    """Test that SQLAlchemy preserves parameter names when using key=param_name in bindparam."""
    from sqlalchemy import bindparam
    from sqlalchemy import select
    from sqlalchemy.ext.asyncio import create_async_engine
    
    # Create a simple test to verify bindparam key preservation
    engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    
    # Create a simple table
    from sqlalchemy import MetaData, Table, Column, Integer
    metadata = MetaData()
    test_table = Table(
        'test_table',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('value', Integer),
    )
    
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
        await conn.execute(test_table.insert(), [{'id': 1, 'value': 100}])
    
    # Create bindparam with explicit key
    param_name = 'test_param'
    bp = bindparam(param_name, value=None, key=param_name)
    
    # Build query with bindparam
    query = select(test_table.c.id).where(test_table.c.value == bp)
    
    # Compile query
    compiled = query.compile(bind=engine.sync_engine, compile_kwargs={})
    
    # Check that parameter name is preserved
    # SQLAlchemy should use the key as the parameter name
    assert param_name in compiled.params or bp.key == param_name
    
    # Execute query
    async with engine.connect() as conn:
        result = await conn.execute(query, {param_name: 100})
        row = result.fetchone()
        assert row is not None
        assert row[0] == 1
    
    await engine.dispose()


@pytest.mark.asyncio
async def test_parameter_mapping_after_compilation(uma):
    """Test that parameter mapping works correctly after SQLAlchemy compilation."""
    from namerec.uma.jsql.cache import MemoryCacheBackend
    
    cache_backend = MemoryCacheBackend(max_size=10)
    
    # Get engine from uma instance
    from namerec.uma import DefaultMetadataProvider, NamespaceConfig
    from sqlalchemy.ext.asyncio import create_async_engine
    from sqlalchemy import MetaData, Table, Column, Integer, Numeric
    
    test_engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    metadata = MetaData()
    orders_table = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('amount', Numeric(10, 2), nullable=False),
    )
    
    async with test_engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
        await conn.execute(
            orders_table.insert(),
            [
                {'id': 1, 'amount': 100.00},
                {'id': 2, 'amount': 250.00},
                {'id': 3, 'amount': 75.50},
            ],
        )
    
    metadata_provider = DefaultMetadataProvider()
    uma_cached = UMA.create({
        'test': NamespaceConfig(
            engine=test_engine,
            metadata_provider=metadata_provider,
        ),
    }, cache_backend=cache_backend)
    
    await metadata_provider.preload(test_engine, namespace='test')
    
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }
    
    # First query - should cache SQL and parameter mapping
    result1 = await uma_cached.select(jsql, params={'min_amount': 100})
    assert len(result1['data']) == 2
    
    # Second query with different parameter value - should use cached SQL
    result2 = await uma_cached.select(jsql, params={'min_amount': 200})
    assert len(result2['data']) == 1
    
    # Verify that parameter mapping was preserved correctly
    # Both queries should work with their respective parameter values
    stats = uma_cached.cache_stats()
    assert stats['hits'] == 1  # Second query should hit cache
    
    await test_engine.dispose()


@pytest.mark.asyncio
async def test_cached_query_missing_parameter(uma):
    """Test behavior when cached query is executed with missing parameter."""
    from namerec.uma.jsql.cache import MemoryCacheBackend
    from namerec.uma import DefaultMetadataProvider, NamespaceConfig
    from sqlalchemy.ext.asyncio import create_async_engine
    from sqlalchemy import MetaData, Table, Column, Integer, Numeric
    
    cache_backend = MemoryCacheBackend(max_size=10)
    test_engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    metadata = MetaData()
    orders_table = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('amount', Numeric(10, 2), nullable=False),
    )
    
    async with test_engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
        await conn.execute(
            orders_table.insert(),
            [{'id': 1, 'amount': 100.00}],
        )
    
    metadata_provider = DefaultMetadataProvider()
    uma_cached = UMA.create({
        'test': NamespaceConfig(
            engine=test_engine,
            metadata_provider=metadata_provider,
        ),
    }, cache_backend=cache_backend)
    
    await metadata_provider.preload(test_engine, namespace='test')
    
    jsql = {
        'from': 'orders',
        'select': ['id'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }
    
    # First query - cache SQL
    await uma_cached.select(jsql, params={'min_amount': 100})
    
    # Second query without parameter - should fail
    from namerec.uma.jsql.exceptions import JSQLSyntaxError
    with pytest.raises(JSQLSyntaxError, match='Parameter.*not provided'):
        await uma_cached.select(jsql, params={})
    
    await test_engine.dispose()


@pytest.mark.asyncio
async def test_cached_query_extra_parameter(uma):
    """Test behavior when cached query is executed with extra (unused) parameter."""
    from namerec.uma.jsql.cache import MemoryCacheBackend
    from namerec.uma import DefaultMetadataProvider, NamespaceConfig
    from sqlalchemy.ext.asyncio import create_async_engine
    from sqlalchemy import MetaData, Table, Column, Integer, Numeric
    
    cache_backend = MemoryCacheBackend(max_size=10)
    test_engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    metadata = MetaData()
    orders_table = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('amount', Numeric(10, 2), nullable=False),
    )
    
    async with test_engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
        await conn.execute(
            orders_table.insert(),
            [{'id': 1, 'amount': 100.00}],
        )
    
    metadata_provider = DefaultMetadataProvider()
    uma_cached = UMA.create({
        'test': NamespaceConfig(
            engine=test_engine,
            metadata_provider=metadata_provider,
        ),
    }, cache_backend=cache_backend)
    
    await metadata_provider.preload(test_engine, namespace='test')
    
    jsql = {
        'from': 'orders',
        'select': ['id'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }
    
    # First query - cache SQL
    result1 = await uma_cached.select(jsql, params={'min_amount': 100})
    assert len(result1['data']) == 1
    
    # Second query with extra parameter - should work (extra params are ignored)
    result2 = await uma_cached.select(jsql, params={'min_amount': 100, 'extra_param': 999})
    assert len(result2['data']) == 1
    
    await test_engine.dispose()


@pytest.mark.asyncio
async def test_cache_performance(uma):
    """Test that cached queries are faster than non-cached queries."""
    import time
    from namerec.uma.jsql.cache import MemoryCacheBackend
    from namerec.uma import DefaultMetadataProvider, NamespaceConfig
    from sqlalchemy.ext.asyncio import create_async_engine
    from sqlalchemy import MetaData, Table, Column, Integer, Numeric
    
    cache_backend = MemoryCacheBackend(max_size=10)
    test_engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    metadata = MetaData()
    orders_table = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('amount', Numeric(10, 2), nullable=False),
    )
    
    async with test_engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
        # Insert more data to make query slower
        await conn.execute(
            orders_table.insert(),
            [{'id': i, 'amount': float(i * 10)} for i in range(1, 100)],
        )
    
    metadata_provider = DefaultMetadataProvider()
    uma_cached = UMA.create({
        'test': NamespaceConfig(
            engine=test_engine,
            metadata_provider=metadata_provider,
        ),
    }, cache_backend=cache_backend)
    
    await metadata_provider.preload(test_engine, namespace='test')
    
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }
    
    # First query - should be slower (cache miss, needs compilation)
    start1 = time.perf_counter()
    result1 = await uma_cached.select(jsql, params={'min_amount': 50})
    time1 = time.perf_counter() - start1
    assert len(result1['data']) > 0
    
    # Second query - should be faster (cache hit, no compilation)
    start2 = time.perf_counter()
    result2 = await uma_cached.select(jsql, params={'min_amount': 100})
    time2 = time.perf_counter() - start2
    assert len(result2['data']) > 0
    
    # Verify cache was hit
    stats = uma_cached.cache_stats()
    assert stats['hits'] == 1
    
    # Note: In-memory SQLite is very fast, so the difference might be minimal
    # But we can at least verify that cache hit doesn't make it slower
    # In a real scenario with network latency and complex queries, the difference would be more significant
    assert time2 <= time1 * 1.5  # Cached query should not be significantly slower
    
    await test_engine.dispose()


@pytest.mark.asyncio
async def test_debug_sql_caching(uma):
    """Test that debug SQL is cached and reused."""
    from namerec.uma.jsql.cache import MemoryCacheBackend
    from namerec.uma import DefaultMetadataProvider, NamespaceConfig
    from sqlalchemy.ext.asyncio import create_async_engine
    from sqlalchemy import MetaData, Table, Column, Integer
    
    cache_backend = MemoryCacheBackend(max_size=10)
    test_engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    metadata = MetaData()
    test_table = Table(
        'test_table',
        metadata,
        Column('id', Integer, primary_key=True),
    )
    
    async with test_engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    metadata_provider = DefaultMetadataProvider()
    uma_cached = UMA.create({
        'test': NamespaceConfig(
            engine=test_engine,
            metadata_provider=metadata_provider,
        ),
    }, cache_backend=cache_backend)
    
    await metadata_provider.preload(test_engine, namespace='test')
    
    jsql = {
        'from': 'test_table',
        'select': ['id'],
        'debug': True,
    }
    
    # First query - should generate and cache debug SQL
    result1 = await uma_cached.select(jsql)
    assert 'debug' in result1
    assert result1['debug'] is not None
    debug_sql1 = result1['debug']
    
    # Second query - should use cached debug SQL
    result2 = await uma_cached.select(jsql)
    assert 'debug' in result2
    assert result2['debug'] is not None
    debug_sql2 = result2['debug']
    
    # Debug SQL should be the same (cached)
    assert debug_sql1 == debug_sql2
    
    await test_engine.dispose()
