"""Basic tests for UMA."""

import pytest

from namerec.uma import UMAContext
from namerec.uma import get_global_registry
from namerec.uma import parse_entity_name


@pytest.mark.asyncio
async def test_parse_entity_name() -> None:
    """Test entity name parsing."""
    # Simple name
    entity = parse_entity_name('users')
    assert entity.entity == 'users'
    assert entity.namespace is None
    assert str(entity) == 'users'

    # With namespace
    entity = parse_entity_name('virtual:user_summary')
    assert entity.entity == 'user_summary'
    assert entity.namespace == 'virtual'
    assert str(entity) == 'virtual:user_summary'


@pytest.mark.asyncio
async def test_get_handler_for_table(context: UMAContext) -> None:
    """Test getting handler for regular table."""
    registry = get_global_registry()
    entity_name = parse_entity_name('users')

    handler = registry.get_handler(entity_name, context)
    assert handler is not None


@pytest.mark.asyncio
async def test_get_metadata_for_table(context: UMAContext) -> None:
    """Test getting metadata for regular table."""
    registry = get_global_registry()
    entity_name = parse_entity_name('users')

    handler = registry.get_handler(entity_name, context)
    metadata = await handler.meta(entity_name, context)

    assert metadata['name'] == 'users'
    assert len(metadata['columns']) == 3
    assert {col['name'] for col in metadata['columns']} == {'id', 'name', 'email'}


@pytest.mark.asyncio
async def test_list_entities(context: UMAContext) -> None:
    """Test listing all entities."""
    registry = get_global_registry()
    entities = registry.list_entities(context)

    assert 'users' in entities
