"""Utility functions for UMA."""

from typing import Any

from sqlalchemy import Table

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName


def parse_entity_name(name: str) -> EntityName:
    """
    Parse entity name string into EntityName object.

    Supports namespace prefixes (e.g., 'db:users' -> EntityName(entity='users', namespace='db')).

    Args:
        name: Entity name string

    Returns:
        EntityName object

    Examples:
        >>> parse_entity_name('users')
        EntityName(entity='users')
        >>> parse_entity_name('virtual:user_summary')
        EntityName(entity='user_summary', namespace='virtual')
    """
    if ':' in name:
        namespace, entity = name.split(':', 1)
        return EntityName(entity=entity, namespace=namespace)
    return EntityName(entity=name)


def form_entity_name(entity: str, namespace: str | None = None) -> EntityName:
    """
    Create EntityName object from components.

    Args:
        entity: Entity name
        namespace: Optional namespace

    Returns:
        EntityName object
    """
    return EntityName(entity=entity, namespace=namespace)


async def copy_field_meta(
    source_entity: str | EntityName,
    field_name: str,
    context: UMAContext,
    overrides: dict[str, Any] | None = None,
) -> dict:
    """
    Copy field metadata from source entity with optional overrides.

    Useful for virtual views that reuse fields from base tables.

    Args:
        source_entity: Source entity name
        field_name: Field name to copy metadata from
        context: Execution context
        overrides: Optional dictionary with fields to override

    Returns:
        Field metadata dictionary

    Raises:
        ValueError: If field not found in source entity
    """
    if isinstance(source_entity, str):
        source_entity = parse_entity_name(source_entity)

    # Import here to avoid circular dependency
    from namerec.uma.api import get_registry

    registry = get_registry()
    handler = await registry.get_handler(source_entity, context)

    # Get metadata
    source_meta = await handler.meta(source_entity, context)

    # Find field
    for col in source_meta.get('columns', []):
        if col['name'] == field_name:
            result = col.copy()
            if overrides:
                result.update(overrides)
            return result

    msg = f'Field {field_name} not found in {source_entity}'
    raise ValueError(msg)


def is_virtual_view(metadata: dict) -> bool:
    """
    Check if entity is a virtual view.

    Args:
        metadata: Entity metadata dictionary

    Returns:
        True if is_virtual_view is present and True
    """
    return metadata.get('is_virtual_view', False) is True


async def get_table(
    context: UMAContext,
    entity_name: EntityName | str,
) -> Table:
    """
    Helper to get SQLAlchemy Table from context.

    Provides shorter syntax than context.metadata_provider.get_table(...).

    Args:
        context: UMA context
        entity_name: Entity name (string or EntityName)

    Returns:
        SQLAlchemy Table object

    Raises:
        UMANotFoundError: If table not found
        RuntimeError: If reflection fails
    """
    if isinstance(entity_name, str):
        entity_name = parse_entity_name(entity_name)

    return await context.metadata_provider.get_table(
        entity_name,
        context.engine,
        context,
    )
