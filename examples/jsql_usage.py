"""Example: JSQL (JSON-SQL) query usage."""

import asyncio
from datetime import date
from datetime import datetime

from sqlalchemy import Column
from sqlalchemy import Date
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import Numeric
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import initialize_uma
from namerec.uma import uma_select


async def main() -> None:
    """Run JSQL examples."""
    # Create async engine and metadata
    engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    metadata = MetaData()

    # Define tables
    customers = Table(
        'customers',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(255), nullable=False),
        Column('email', String(255)),
        Column('department', String(100)),
    )

    orders = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('customer_id', Integer, ForeignKey('customers.id')),
        Column('order_date', Date, nullable=False),
        Column('amount', Numeric(10, 2), nullable=False),
        Column('quantity', Integer, nullable=False),
        Column('price', Numeric(10, 2), nullable=False),
    )

    # Create tables and insert data using async engine
    async with engine.begin() as conn:
        # Create all tables
        await conn.run_sync(metadata.create_all)

        # Insert test data
        await conn.execute(
            customers.insert(),
            [
                {'id': 1, 'name': 'John Doe', 'email': 'john@example.com', 'department': 'Engineering'},
                {'id': 2, 'name': 'Jane Smith', 'email': 'jane@example.com', 'department': 'Sales'},
                {'id': 3, 'name': 'Bob Johnson', 'email': 'bob@example.com', 'department': 'Engineering'},
                {'id': 4, 'name': 'Alice Brown', 'email': 'alice@example.com', 'department': 'Sales'},
                {'id': 5, 'name': 'Charlie Davis', 'email': 'charlie@example.com', 'department': 'HR'},
            ],
        )
        await conn.execute(
            orders.insert(),
            [
                {'id': 1, 'customer_id': 1, 'order_date': date(2024, 1, 15), 'amount': 76.50, 'quantity': 3, 'price': 25.50},
                {'id': 2, 'customer_id': 1, 'order_date': date(2024, 2, 20), 'amount': 199.99, 'quantity': 1, 'price': 199.99},
                {'id': 3, 'customer_id': 2, 'order_date': date(2024, 1, 10), 'amount': 50.00, 'quantity': 5, 'price': 10.00},
                {'id': 4, 'customer_id': 2, 'order_date': date(2024, 3, 5), 'amount': 150.00, 'quantity': 2, 'price': 75.00},
                {'id': 5, 'customer_id': 3, 'order_date': date(2024, 2, 1), 'amount': 300.00, 'quantity': 4, 'price': 75.00},
            ],
        )

    # Initialize UMA
    initialize_uma(engine, metadata)

    print('=' * 80)
    print('JSQL Query Examples')
    print('=' * 80)

    # Example 1: Simple SELECT with WHERE
    print('\n1. Simple SELECT with WHERE:')
    print('-' * 80)
    jsql1 = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
        'where': {
            'op': '=',
            'left': {'field': 'department'},
            'right': {'value': 'Engineering'},
        },
    }
    result1 = await uma_select(jsql1)
    print('JSQL:', jsql1)
    print('\nResult metadata:')
    for col in result1['meta']:
        print(f"  - {col['name']}: {col['type']} (qualified: {col['qualified_name']})")
    print('\nResult data:')
    for row in result1['data']:
        print(f'  {row}')

    # Example 2: Aggregation with GROUP BY
    print('\n2. Aggregation with GROUP BY:')
    print('-' * 80)
    jsql2 = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id', 'alias': 'customer'},
            {'func': 'COUNT', 'args': [{'field': 'id'}], 'alias': 'order_count'},
            {'func': 'SUM', 'args': [{'field': 'amount'}], 'alias': 'total_spent'},
        ],
        'group_by': [{'field': 'customer_id'}],
        'order_by': [{'field': 'total_spent', 'direction': 'desc'}],
    }
    result2 = await uma_select(jsql2)
    print('JSQL:', jsql2)
    print('\nResult metadata:')
    for col in result2['meta']:
        print(f"  - {col['name']}: {col['type']} (source: {col['source_entity']}.{col['source_field']})")
    print('\nResult data:')
    for row in result2['data']:
        print(f'  Customer {row[0]}: {row[1]} orders, total ${row[2]}')

    # Example 3: JOIN with expression
    print('\n3. JOIN with computed expression:')
    print('-' * 80)
    jsql3 = {
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
            {'field': 'orders.quantity'},
            {'field': 'orders.price'},
            {
                'expr': {
                    'op': '*',
                    'left': {'field': 'orders.quantity'},
                    'right': {'field': 'orders.price'},
                },
                'alias': 'line_total',
            },
        ],
        'limit': 3,
    }
    result3 = await uma_select(jsql3)
    print('JSQL:', jsql3)
    print('\nResult metadata:')
    for col in result3['meta']:
        print(f"  - {col['name']}: {col['type']}")
    print('\nResult data:')
    for row in result3['data']:
        print(f'  Order {row[0]}: {row[1]}, qty={row[2]}, price=${row[3]}, total=${row[4]}')

    # Example 4: CTE (Common Table Expression)
    print('\n4. CTE (Common Table Expression):')
    print('-' * 80)
    jsql4 = {
        'with': [
            {
                'name': 'engineering_customers',
                'query': {
                    'from': 'customers',
                    'select': ['id', 'name'],
                    'where': {
                        'op': '=',
                        'left': {'field': 'department'},
                        'right': {'value': 'Engineering'},
                    },
                },
            },
        ],
        'from': 'orders',
        'joins': [
            {
                'type': 'INNER',
                'table': 'engineering_customers',
                'alias': 'ec',
                'on': {
                    'op': '=',
                    'left': {'field': 'orders.customer_id'},
                    'right': {'field': 'ec.id'},
                },
            },
        ],
        'select': [
            {'field': 'ec.name', 'alias': 'customer_name'},
            {'func': 'SUM', 'args': [{'field': 'orders.amount'}], 'alias': 'total'},
        ],
        'group_by': [{'field': 'ec.name'}],
    }
    result4 = await uma_select(jsql4)
    print('JSQL:', jsql4)
    print('\nResult data:')
    for row in result4['data']:
        print(f'  {row[0]}: total ${row[1]}')

    # Example 5: Subquery with IN
    print('\n5. Subquery with IN:')
    print('-' * 80)
    jsql5 = {
        'from': 'customers',
        'select': ['id', 'name', 'department'],
        'where': {
            'op': 'IN',
            'left': {'field': 'id'},
            'right': {
                'subquery': {
                    'from': 'orders',
                    'select': [{'field': 'customer_id'}],
                    'where': {
                        'op': '>',
                        'left': {'field': 'amount'},
                        'right': {'value': 100},
                    },
                },
            },
        },
    }
    result5 = await uma_select(jsql5)
    print('JSQL:', jsql5)
    print('\nResult data:')
    for row in result5['data']:
        print(f'  {row[1]} (ID: {row[0]}, Dept: {row[2]})')

    # Example 6: Query with parameters
    print('\n6. Query with parameters:')
    print('-' * 80)
    jsql6 = {
        'from': 'orders',
        'select': ['id', 'customer_id', 'amount'],
        'where': {
            'op': 'AND',
            'conditions': [
                {
                    'op': '>=',
                    'left': {'field': 'amount'},
                    'right': {'param': 'min_amount'},
                },
                {
                    'op': '=',
                    'left': {'field': 'customer_id'},
                    'right': {'param': 'customer_id'},
                },
            ],
        },
    }
    params6 = {
        'min_amount': 100,
        'customer_id': 1,
    }
    result6 = await uma_select(jsql6, params6)
    print('JSQL:', jsql6)
    print('Params:', params6)
    print('\nResult data:')
    for row in result6['data']:
        print(f'  Order {row[0]}: Customer {row[1]}, Amount ${row[2]}')

    # Example 7: Window Functions (ROW_NUMBER, AVG)
    print('\n7. Window Functions (ranking and aggregation):')
    print('-' * 80)
    jsql7 = {
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
            {
                'window_func': {
                    'func': 'ROW_NUMBER',
                    'partition_by': [{'field': 'orders.customer_id'}],
                    'order_by': [{'field': 'orders.amount', 'direction': 'desc'}],
                },
                'alias': 'order_rank',
            },
            {
                'window_func': {
                    'func': 'AVG',
                    'args': [{'field': 'orders.amount'}],
                    'partition_by': [{'field': 'orders.customer_id'}],
                },
                'alias': 'customer_avg_amount',
            },
        ],
    }
    result7 = await uma_select(jsql7)
    print('JSQL:', jsql7)
    print('\nResult data (order_id, customer, amount, rank, avg):')
    for row in result7['data']:
        print(f'  Order {row[0]}: {row[1]}, ${row[2]:.2f}, rank={row[3]}, avg=${row[4]:.2f}')

    print('\n' + '=' * 80)
    print('All examples completed successfully!')
    print('=' * 80)

    # Cleanup
    await engine.dispose()


if __name__ == '__main__':
    asyncio.run(main())
