"""Example usage of SQL ↔ JSQL conversion features."""

import asyncio

from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import initialize_uma
from namerec.uma import uma_select
from namerec.uma.jsql.converter import jsql_to_sql
from namerec.uma.jsql.converter import sql_to_jsql


async def example_debug_mode() -> None:
    """Example: Using debug mode in JSQL queries."""
    print('=== Example 1: Debug Mode ===\n')

    # Create in-memory database
    engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)
    metadata = MetaData()

    # Define table
    users = Table(
        'users',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(50)),
        Column('email', String(100)),
        Column('active', Integer, default=1),
    )

    # Create tables
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

        # Insert sample data
        await conn.execute(
            users.insert(),
            [
                {'id': 1, 'name': 'Alice', 'email': 'alice@example.com', 'active': 1},
                {'id': 2, 'name': 'Bob', 'email': 'bob@example.com', 'active': 1},
                {'id': 3, 'name': 'Charlie', 'email': 'charlie@example.com', 'active': 0},
            ],
        )

    # Initialize UMA
    initialize_uma(engine=engine, metadata=metadata)

    # Execute query with debug mode
    jsql = {
        'from': 'users',
        'select': [{'field': 'id'}, {'field': 'name'}, {'field': 'email'}],
        'where': {'op': '=', 'left': {'field': 'active'}, 'right': {'value': 1}},
        'order_by': [{'field': 'name', 'direction': 'ASC'}],
        'debug': True,  # Enable debug mode
    }

    result = await uma_select(jsql)
    result_dict = result.to_dict()

    print('Generated SQL:')
    print(result_dict['debug'])
    print('\nQuery Results:')
    for row in result_dict['data']:
        print(f'  ID: {row[0]}, Name: {row[1]}, Email: {row[2]}')
    print()


def example_jsql_to_sql() -> None:
    """Example: Converting JSQL to SQL."""
    print('=== Example 2: JSQL to SQL Conversion ===\n')

    # Simple query
    jsql = {
        'from': 'users',
        'select': [{'field': 'id'}, {'field': 'name'}],
        'where': {'op': '=', 'left': {'field': 'active'}, 'right': {'value': True}},
        'limit': 10,
    }

    sql = jsql_to_sql(jsql)
    print('JSQL:', jsql)
    print('\nGenerated SQL:')
    print(sql)
    print()


def example_jsql_to_sql_with_join() -> None:
    """Example: Converting JSQL with JOIN to SQL."""
    print('=== Example 3: JSQL to SQL with JOIN ===\n')

    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'orders.id', 'alias': 'order_id'},
            {'field': 'customers.name', 'alias': 'customer_name'},
            {'field': 'orders.total'},
        ],
        'joins': [
            {
                'type': 'INNER',
                'entity': 'customers',
                'on': {
                    'op': '=',
                    'left': {'field': 'orders.customer_id'},
                    'right': {'field': 'customers.id'},
                },
            },
        ],
        'where': {'op': '>', 'left': {'field': 'orders.total'}, 'right': {'value': 100}},
        'order_by': [{'field': 'orders.total', 'direction': 'DESC'}],
    }

    sql = jsql_to_sql(jsql)
    print('JSQL with JOIN:')
    print(jsql)
    print('\nGenerated SQL:')
    print(sql)
    print()


def example_sql_to_jsql() -> None:
    """Example: Converting SQL to JSQL."""
    print('=== Example 4: SQL to JSQL Conversion ===\n')

    # Simple query
    sql = 'SELECT id, name FROM users WHERE active = 1 ORDER BY name LIMIT 10'

    jsql = sql_to_jsql(sql)
    print('SQL:', sql)
    print('\nGenerated JSQL:')
    import json

    print(json.dumps(jsql, indent=2))
    print()


def example_sql_to_jsql_complex() -> None:
    """Example: Converting complex SQL to JSQL."""
    print('=== Example 5: Complex SQL to JSQL ===\n')

    sql = '''
        SELECT
            c.id,
            c.name,
            COUNT(o.id) as order_count,
            SUM(o.amount) as total_spent
        FROM customers c
        LEFT JOIN orders o ON c.id = o.customer_id
        WHERE c.active = 1
        GROUP BY c.id, c.name
        HAVING SUM(o.amount) > 1000
        ORDER BY total_spent DESC
        LIMIT 5
    '''

    jsql = sql_to_jsql(sql)
    print('Complex SQL:')
    print(sql)
    print('\nGenerated JSQL:')
    import json

    print(json.dumps(jsql, indent=2))
    print()


def example_dialect_conversion() -> None:
    """Example: Converting to different SQL dialects."""
    print('=== Example 6: Dialect Conversion ===\n')

    jsql = {
        'from': 'users',
        'select': [{'field': '*'}],
        'limit': 5,
        'offset': 10,
    }

    print('JSQL:', jsql)
    print()

    # Generate SQL for different databases
    for dialect in ['sqlite', 'postgresql', 'mysql']:
        sql = jsql_to_sql(jsql, dialect=dialect)
        print(f'{dialect.upper()} SQL:')
        print(sql)
        print()


def example_round_trip() -> None:
    """Example: Round-trip conversion (JSQL -> SQL -> JSQL)."""
    print('=== Example 7: Round-trip Conversion ===\n')

    # Original JSQL
    original_jsql = {
        'from': 'products',
        'select': [{'field': 'id'}, {'field': 'name'}, {'field': 'price'}],
        'where': {
            'op': 'AND',
            'conditions': [
                {'op': '>', 'left': {'field': 'price'}, 'right': {'value': 10}},
                {'op': '=', 'left': {'field': 'active'}, 'right': {'value': 1}},
            ],
        },
        'order_by': [{'field': 'price', 'direction': 'DESC'}],
        'limit': 20,
    }

    print('Original JSQL:')
    import json

    print(json.dumps(original_jsql, indent=2))

    # Convert to SQL
    sql = jsql_to_sql(original_jsql)
    print('\nGenerated SQL:')
    print(sql)

    # Convert back to JSQL
    converted_jsql = sql_to_jsql(sql)
    print('\nConverted back to JSQL:')
    print(json.dumps(converted_jsql, indent=2))
    print()


async def main() -> None:
    """Run all examples."""
    # Debug mode example (async)
    await example_debug_mode()

    # Conversion examples (sync)
    example_jsql_to_sql()
    example_jsql_to_sql_with_join()
    example_sql_to_jsql()
    example_sql_to_jsql_complex()
    example_dialect_conversion()
    example_round_trip()

    print('=== All Examples Complete ===')


if __name__ == '__main__':
    asyncio.run(main())
