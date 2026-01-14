"""Tests for UMA API functions."""

import pytest

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMA
from namerec.uma import UMANotFoundError


async def _create_test_uma(engine, metadata):  # noqa: ANN001, ANN201
    """Helper to create UMA instance for tests."""
    # Create tables first
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

    # Create UMA instance
    metadata_provider = DefaultMetadataProvider()
    uma = UMA.create({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })

    # Preload metadata
    await metadata_provider.preload(engine, namespace='test')

    return uma


@pytest.mark.asyncio
async def test_initialize_uma(engine, metadata) -> None:  # noqa: ANN001
    """Test UMA initialization."""
    uma = await _create_test_uma(engine, metadata)
    assert uma is not None
    assert uma.default_namespace == 'test'


@pytest.mark.asyncio
async def test_uma_entity_list(engine, metadata) -> None:  # noqa: ANN001
    """Test listing entities."""
    uma = await _create_test_uma(engine, metadata)

    entities = await uma.entity_list()
    assert 'users' in entities


@pytest.mark.asyncio
async def test_uma_entity_details(engine, metadata) -> None:  # noqa: ANN001
    """Test getting entity metadata."""
    uma = await _create_test_uma(engine, metadata)

    meta = await uma.entity_details('users')
    assert meta['name'] == 'test:users'
    assert len(meta['columns']) == 3


@pytest.mark.asyncio
async def test_uma_save_and_read(engine, metadata) -> None:  # noqa: ANN001
    """Test creating and reading a record."""
    uma = await _create_test_uma(engine, metadata)

    # Create
    user_id = await uma.save('users', {'name': 'Test User', 'email': 'test@example.com'})
    assert user_id is not None

    # Read
    user = await uma.read('users', user_id)
    assert user['name'] == 'Test User'
    assert user['email'] == 'test@example.com'


@pytest.mark.asyncio
async def test_uma_update(engine, metadata) -> None:  # noqa: ANN001
    """Test updating a record."""
    uma = await _create_test_uma(engine, metadata)

    # Create
    user_id = await uma.save('users', {'name': 'Test User', 'email': 'test@example.com'})

    # Update
    await uma.save('users', {'id': user_id, 'name': 'Updated User', 'email': 'updated@example.com'})

    # Verify
    user = await uma.read('users', user_id)
    assert user['name'] == 'Updated User'


@pytest.mark.asyncio
async def test_uma_delete(engine, metadata) -> None:  # noqa: ANN001
    """Test deleting a record."""
    uma = await _create_test_uma(engine, metadata)

    # Create
    user_id = await uma.save('users', {'name': 'Test User', 'email': 'test@example.com'})

    # Delete
    deleted = await uma.delete('users', user_id)
    assert deleted is True

    # Verify deletion
    with pytest.raises(UMANotFoundError):
        await uma.read('users', user_id)


@pytest.mark.asyncio
async def test_uma_not_initialized() -> None:
    """Test that UMA.create validates namespace_configs."""
    with pytest.raises(ValueError, match='At least one namespace configuration required'):
        UMA.create({})
