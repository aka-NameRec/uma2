"""
API usage example for UMA.

This example demonstrates high-level API functions:
1. initialize_uma - Initialize UMA
2. uma_list_entities - List all entities
3. uma_meta - Get entity metadata
4. uma_read - Read record
5. uma_save - Save record
6. uma_delete - Delete record
"""

import asyncio

from sqlalchemy import Column
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import Numeric
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy import func
from sqlalchemy import select
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import Operation
from namerec.uma import UMAAccessDeniedError
from namerec.uma import UMAContext
from namerec.uma import VirtualViewHandler
from namerec.uma import copy_field_meta
from namerec.uma import get_registry
from namerec.uma import initialize_uma
from namerec.uma import uma_delete
from namerec.uma import uma_entity_details
from namerec.uma import uma_entity_list
from namerec.uma import uma_read
from namerec.uma import uma_save


# ========== Custom Metadata Provider with Access Control ==========


class AccessControlMetadataProvider(DefaultMetadataProvider):
    """Metadata provider with access control."""

    def can(
        self,
        entity_name: str,
        operation: Operation | str,
        context: UMAContext,
    ) -> bool:
        """
        Check access based on user role.

        Args:
            entity_name: Entity name (empty for list operation)
            operation: Operation to check
            context: Execution context with user_context

        Returns:
            True if access allowed
        """
        user = context.user_context

        # Special case: list entities
        if entity_name == '' and (operation == Operation.META or operation == 'meta'):
            return user is not None  # Only authenticated users

        # Check if user authenticated
        if not user:
            return False

        user_role = getattr(user, 'role', 'guest')

        # Admin can do everything
        if user_role == 'admin':
            return True

        # Regular user: read/list/meta only
        if user_role == 'user':
            return operation in (Operation.READ, Operation.LIST, Operation.META, 'read', 'list', 'meta')

        # Guest: no access
        return False


# ========== Define Schema ==========

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


# ========== Virtual View ==========


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
    async def meta(cls, entity_name, context):  # noqa: ANN001, ANN201
        """Return metadata for virtual view."""
        columns_metadata = [
            await copy_field_meta('users', 'id', context, {'name': 'user_id'}),
            await copy_field_meta('users', 'name', context),
            {'name': 'order_count', 'type': 'integer', 'description': 'Number of orders'},
            {'name': 'total_spent', 'type': 'numeric', 'description': 'Total order amount'},
        ]

        return {
            'name': str(entity_name),
            'description': cls.description,
            'is_virtual_view': True,
            'columns': columns_metadata,
        }


# ========== User Context Classes ==========


class User:
    """User for access control."""

    def __init__(self, user_id: int, role: str) -> None:
        """Initialize user."""
        self.id = user_id
        self.role = role


# ========== Main ==========


async def main() -> None:
    """Run API example."""
    # Create async engine
    engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)

    # Create tables
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

    # ========== Initialize UMA ==========
    print('\n=== Initializing UMA ===')

    metadata_provider = AccessControlMetadataProvider()
    registry = initialize_uma(
        engine=engine,
        metadata=metadata,
        metadata_provider=metadata_provider,
    )

    # Register virtual view
    registry.register('user_summary', UserSummaryView)

    print('✓ UMA initialized')
    print('✓ Virtual view registered')

    # ========== Example 1: List entities as admin ==========
    print('\n=== Example 1: List entities (admin) ===')

    admin_user = User(user_id=1, role='admin')

    try:
        entities = await uma_entity_list(user_context=admin_user)
        print(f'✓ Available entities: {entities}')
    except UMAAccessDeniedError as e:
        print(f'✗ Access denied: {e}')

    # ========== Example 2: Get metadata ==========
    print('\n=== Example 2: Get metadata (admin) ===')

    try:
        user_meta = await uma_entity_details('users', user_context=admin_user)
        print(f"✓ Users table columns: {[col['name'] for col in user_meta['columns']]}")
    except UMAAccessDeniedError as e:
        print(f'✗ Access denied: {e}')

    # ========== Example 3: Create records ==========
    print('\n=== Example 3: Create records (admin) ===')

    try:
        # Create user
        user_id = await uma_save(
            'users',
            {'name': 'John Doe', 'email': 'john@example.com'},
            user_context=admin_user,
        )
        print(f'✓ Created user with id={user_id}')

        # Create another user
        user_id2 = await uma_save(
            'users',
            {'name': 'Jane Smith', 'email': 'jane@example.com'},
            user_context=admin_user,
        )
        print(f'✓ Created user with id={user_id2}')

        # Create order
        order_id = await uma_save(
            'orders',
            {'user_id': user_id, 'total': 100.50},
            user_context=admin_user,
        )
        print(f'✓ Created order with id={order_id}')

    except UMAAccessDeniedError as e:
        print(f'✗ Access denied: {e}')

    # ========== Example 4: Read record ==========
    print('\n=== Example 4: Read record (admin) ===')

    try:
        user_data = await uma_read('users', user_id, user_context=admin_user)
        print(f'✓ User data: {user_data}')
    except UMAAccessDeniedError as e:
        print(f'✗ Access denied: {e}')

    # ========== Example 5: Update record ==========
    print('\n=== Example 5: Update record (admin) ===')

    try:
        await uma_save(
            'users',
            {'id': user_id, 'name': 'John Smith', 'email': 'john.smith@example.com'},
            user_context=admin_user,
        )
        print('✓ User updated')

        # Verify update
        updated_user = await uma_read('users', user_id, user_context=admin_user)
        print(f"✓ Updated name: {updated_user['name']}")
    except UMAAccessDeniedError as e:
        print(f'✗ Access denied: {e}')

    # ========== Example 6: Regular user - read allowed ==========
    print('\n=== Example 6: Regular user - read allowed ===')

    regular_user = User(user_id=2, role='user')

    try:
        user_data = await uma_read('users', user_id, user_context=regular_user)
        print(f'✓ User can read: {user_data["name"]}')
    except UMAAccessDeniedError as e:
        print(f'✗ Access denied: {e}')

    # ========== Example 7: Regular user - write denied ==========
    print('\n=== Example 7: Regular user - write denied ===')

    try:
        await uma_save(
            'users',
            {'id': user_id, 'name': 'Hacker', 'email': 'hacker@example.com'},
            user_context=regular_user,
        )
        print('✗ User should not be able to write!')
    except UMAAccessDeniedError:
        print('✓ Access correctly denied for write operation')

    # ========== Example 8: Regular user - delete denied ==========
    print('\n=== Example 8: Regular user - delete denied ===')

    try:
        await uma_delete('users', user_id, user_context=regular_user)
        print('✗ User should not be able to delete!')
    except UMAAccessDeniedError:
        print('✓ Access correctly denied for delete operation')

    # ========== Example 9: Guest - all denied ==========
    print('\n=== Example 9: Guest - all denied ===')

    guest_user = User(user_id=0, role='guest')

    try:
        await uma_read('users', user_id, user_context=guest_user)
        print('✗ Guest should not have access!')
    except UMAAccessDeniedError:
        print('✓ Access correctly denied for guest')

    # ========== Example 10: Delete record (admin) ==========
    print('\n=== Example 10: Delete record (admin) ===')

    try:
        deleted = await uma_delete('users', user_id2, user_context=admin_user)
        print(f'✓ User deleted: {deleted}')
    except UMAAccessDeniedError as e:
        print(f'✗ Access denied: {e}')

    # ========== Cleanup ==========
    await engine.dispose()
    print('\n=== Example completed ===')


if __name__ == '__main__':
    asyncio.run(main())
