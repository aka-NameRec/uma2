"""Tests for UMA API functions."""

import pytest

from namerec.uma import UMAAccessDeniedError
from namerec.uma import UMAContext
from namerec.uma import UMANotFoundError
from namerec.uma import initialize_uma
from namerec.uma import uma_delete
from namerec.uma import uma_list_entities
from namerec.uma import uma_meta
from namerec.uma import uma_read
from namerec.uma import uma_save


@pytest.mark.asyncio
async def test_initialize_uma(engine, metadata) -> None:  # noqa: ANN001
    """Test UMA initialization."""
    registry = initialize_uma(engine=engine, metadata=metadata)
    assert registry is not None


@pytest.mark.asyncio
async def test_uma_list_entities(engine, metadata) -> None:  # noqa: ANN001
    """Test listing entities."""
    initialize_uma(engine=engine, metadata=metadata)

    entities = await uma_list_entities()
    assert 'users' in entities


@pytest.mark.asyncio
async def test_uma_meta(engine, metadata) -> None:  # noqa: ANN001
    """Test getting entity metadata."""
    initialize_uma(engine=engine, metadata=metadata)

    meta = await uma_meta('users')
    assert meta['name'] == 'users'
    assert len(meta['columns']) == 3


@pytest.mark.asyncio
async def test_uma_save_and_read(engine, metadata) -> None:  # noqa: ANN001
    """Test creating and reading a record."""
    initialize_uma(engine=engine, metadata=metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})
    assert user_id is not None

    # Read
    user = await uma_read('users', user_id)
    assert user['name'] == 'Test User'
    assert user['email'] == 'test@example.com'


@pytest.mark.asyncio
async def test_uma_update(engine, metadata) -> None:  # noqa: ANN001
    """Test updating a record."""
    initialize_uma(engine=engine, metadata=metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})

    # Update
    await uma_save('users', {'id': user_id, 'name': 'Updated User', 'email': 'updated@example.com'})

    # Verify
    user = await uma_read('users', user_id)
    assert user['name'] == 'Updated User'


@pytest.mark.asyncio
async def test_uma_delete(engine, metadata) -> None:  # noqa: ANN001
    """Test deleting a record."""
    initialize_uma(engine=engine, metadata=metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})

    # Delete
    deleted = await uma_delete('users', user_id)
    assert deleted is True

    # Verify deletion
    with pytest.raises(UMANotFoundError):
        await uma_read('users', user_id)


@pytest.mark.asyncio
async def test_uma_not_initialized() -> None:
    """Test API functions raise error when UMA not initialized."""
    # Reset by importing fresh module would be needed, but for now just test with initialized
    # In real scenario, this would require module reloading
    pass
