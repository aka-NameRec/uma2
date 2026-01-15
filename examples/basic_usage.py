"""
Basic usage example for UMA.

This example demonstrates:
1. Initializing UMA with SQLAlchemy
2. Registering a virtual view
3. Using entity handlers
"""

import asyncio

from sqlalchemy import Column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import Numeric
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy import create_engine
from sqlalchemy import func
from sqlalchemy import select
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMA
from namerec.uma import VirtualViewHandler
from namerec.uma import copy_field_meta
from namerec.uma import parse_entity_name


# Define schema
metadata = MetaData()

users_table = Table(
    'users',
    metadata,
    Column('id', Integer, primary_key=True),
    Column('name', String(100), nullable=False),
    Column('email', String(100), nullable=False),
)

orders_table = Table(
    'orders',
    metadata,
    Column('id', Integer, primary_key=True),
    Column('user_id', Integer, ForeignKey('users.id'), nullable=False),
    Column('total', Numeric(10, 2), nullable=False),
)


# Define virtual view
class UserSummaryView(VirtualViewHandler):
    """Virtual view: user orders summary."""

    description = 'Summary of user orders'

    @classmethod
    async def select(cls, entity_name, params, context):  # noqa: ANN001, ANN201
        """Build SELECT for user summary."""
        users = context.metadata.tables['users']
        orders = context.metadata.tables['orders']

        query = (
            select(
                users.c.id.label('user_id'),
                users.c.name,
                func.count(orders.c.id).label('order_count'),
                func.coalesce(func.sum(orders.c.total), 0).label('total_spent'),
            )
            .select_from(users.outerjoin(orders, users.c.id == orders.c.user_id))
            .group_by(users.c.id, users.c.name)
        )

        # Apply filters from params
        if 'user_id' in params:
            query = query.where(users.c.id == params['user_id'])

        return query

    @classmethod
    async def meta(cls, entity_name, context, registry):  # noqa: ANN001, ANN201
        """Return metadata for virtual view."""
        columns_metadata = [
            await copy_field_meta('users', 'id', context, registry, {'name': 'user_id'}),
            await copy_field_meta('users', 'name', context, registry),
            {'name': 'order_count', 'type': 'integer', 'description': 'Number of orders'},
            {'name': 'total_spent', 'type': 'numeric', 'description': 'Total order amount'},
        ]

        return {
            'name': str(entity_name),
            'description': cls.description,
            'is_virtual_view': True,
            'columns': columns_metadata,
        }


async def main() -> None:
    """Run example."""
    # Create async engine
    engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=True)

    # Create tables
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

    # Create UMA instance
    metadata_provider = DefaultMetadataProvider()
    uma = UMA.create({
        'main': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })

    # Register virtual view
    uma.registry.register('user_summary', UserSummaryView)

    # Create context
    context = uma._create_context('main')

    # Example 1: Work with regular table
    print('\n=== Example 1: Regular table ===')
    entity_name = parse_entity_name('users')
    handler = await uma.registry.get_handler(entity_name, context)

    # Get metadata
    user_meta = await handler.meta(entity_name, context, uma.registry)
    print(f"Users table columns: {[col['name'] for col in user_meta['columns']]}")

    # Example 2: Work with virtual view
    print('\n=== Example 2: Virtual view ===')
    view_name = parse_entity_name('user_summary')
    view_handler = await uma.registry.get_handler(view_name, context)

    # Get metadata
    view_meta = await view_handler.meta(view_name, context, uma.registry)
    print(f"User summary view columns: {[col['name'] for col in view_meta['columns']]}")
    print(f"Is virtual view: {view_meta.get('is_virtual_view', False)}")

    # Example 3: List all entities
    print('\n=== Example 3: List all entities ===')
    all_entities = await uma.registry.list_entities(context)
    print(f'Available entities: {all_entities}')

    await engine.dispose()


if __name__ == '__main__':
    asyncio.run(main())
