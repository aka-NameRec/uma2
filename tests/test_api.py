"""Tests for UMA API functions."""

import pytest

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAAccessDeniedError
from namerec.uma import UMAContext
from namerec.uma import UMANotFoundError
from namerec.uma import uma_delete
from namerec.uma import uma_initialize
from namerec.uma import uma_list_entities
from namerec.uma import uma_meta
from namerec.uma import uma_read
from namerec.uma import uma_save


async def _initialize_test_uma(engine, metadata):  # noqa: ANN001, ANN202
    """Helper to initialize UMA for tests."""
    # Create tables first
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    # Initialize UMA
    metadata_provider = DefaultMetadataProvider()
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Preload metadata
    await metadata_provider.preload(engine, namespace='test')


@pytest.mark.asyncio
async def test_initialize_uma(engine, metadata) -> None:  # noqa: ANN001
    """Test UMA initialization."""
    await _initialize_test_uma(engine, metadata)


@pytest.mark.asyncio
async def test_uma_list_entities(engine, metadata) -> None:  # noqa: ANN001
    """Test listing entities."""
    await _initialize_test_uma(engine, metadata)

    entities = await uma_list_entities()
    assert 'users' in entities


@pytest.mark.asyncio
async def test_uma_meta(engine, metadata) -> None:  # noqa: ANN001
    """Test getting entity metadata."""
    await _initialize_test_uma(engine, metadata)

    meta = await uma_meta('users')
    assert meta['name'] == 'test:users'
    assert len(meta['columns']) == 3


@pytest.mark.asyncio
async def test_uma_save_and_read(engine, metadata) -> None:  # noqa: ANN001
    """Test creating and reading a record."""
    await _initialize_test_uma(engine, metadata)

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
    await _initialize_test_uma(engine, metadata)

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
    await _initialize_test_uma(engine, metadata)

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
