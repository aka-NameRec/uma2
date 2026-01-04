"""High-level API functions for UMA."""

from typing import Any

from sqlalchemy import Engine
from sqlalchemy import MetaData

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import MetadataProvider
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.metadata import DefaultMetadataProvider
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None
_context_template: UMAContext | None = None


def initialize_uma(
    engine: Engine,
    metadata: MetaData,
    metadata_provider: MetadataProvider | None = None,
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA.

    Args:
        engine: SQLAlchemy Engine
        metadata: SQLAlchemy MetaData (reflected)
        metadata_provider: Metadata provider (if None - creates DefaultMetadataProvider)
        default_handler: Default handler (if None - uses DefaultEntityHandler)

    Returns:
        Initialized EntityRegistry
    """
    global _registry, _context_template  # noqa: PLW0603

    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    if metadata_provider is None:
        metadata_provider = DefaultMetadataProvider()

    _context_template = UMAContext(
        engine=engine,
        metadata=metadata,
        metadata_provider=metadata_provider,
    )

    return _registry


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(user_context: Any = None, **extra: Any) -> UMAContext:  # noqa: ANN401
    """
    Create context for operation.

    Args:
        user_context: User context for access control
        **extra: Additional context fields

    Returns:
        UMAContext instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _context_template is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)

    return UMAContext(
        engine=_context_template.engine,
        metadata=_context_template.metadata,
        metadata_provider=_context_template.metadata_provider,
        user_context=user_context,
        cache=_context_template.cache,
        extra=extra,
    )


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
        operation: Operation name
        context: Execution context

    Raises:
        UMAAccessDeniedError: If access denied
    """
    if isinstance(operation, str):
        operation = Operation(operation)

    # Check if metadata provider has can() method
    if hasattr(context.metadata_provider, 'can'):
        if not context.metadata_provider.can(entity_name, operation, context):  # type: ignore[union-attr]
            raise UMAAccessDeniedError(entity_name, operation.value)


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> dict:
    """
    Read a record by id.

    Args:
        entity_name: Entity name (can include namespace, e.g., 'virtual:users')
        id_value: Record ID
        user_context: User context for access control
        namespace: Optional namespace (if not in entity_name)

    Returns:
        Record data as dictionary

    Raises:
        UMAAccessDeniedError: If access denied
        UMANotFoundError: If record not found
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.READ, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
    """
    Save a record (create if id=None, otherwise update).

    Args:
        entity_name: Entity name (can include namespace, e.g., 'virtual:users')
        data: Record data
        user_context: User context for access control
        namespace: Optional namespace (if not in entity_name)

    Returns:
        ID of saved record

    Raises:
        UMAAccessDeniedError: If access denied
        UMANotFoundError: If record not found (for update)
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(entity_name, operation, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> bool:
    """
    Delete a record by id.

    Args:
        entity_name: Entity name (can include namespace, e.g., 'virtual:users')
        id_value: Record ID
        user_context: User context for access control
        namespace: Optional namespace (if not in entity_name)

    Returns:
        True if deleted

    Raises:
        UMAAccessDeniedError: If access denied
        UMANotFoundError: If record not found
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.DELETE, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> dict:
    """
    Get entity metadata.

    Args:
        entity_name: Entity name (can include namespace, e.g., 'virtual:users')
        user_context: User context for access control
        namespace: Optional namespace (if not in entity_name)

    Returns:
        Metadata dictionary

    Raises:
        UMAAccessDeniedError: If access denied
        UMANotFoundError: If entity not found
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.META, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter

    Returns:
        List of entity names

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from registry
    registry = get_registry()
    all_entities = registry.list_entities(context)

    # Filter by namespace if specified
    if namespace:
        prefix = f'{namespace}:'
        return [e for e in all_entities if e.startswith(prefix)]

    return all_entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> dict:
    """
    Execute JSQL query.

    Note: This is a placeholder implementation. Full JSQL parser will be implemented later.

    Args:
        jsql: JSQL query dictionary (must contain 'from' field)
        params: Query parameters
        user_context: User context for access control
        namespace: Optional namespace

    Returns:
        Dictionary with 'metadata' and 'data' keys

    Raises:
        UMAAccessDeniedError: If access denied
        ValueError: If JSQL invalid
        RuntimeError: If UMA not initialized
        NotImplementedError: JSQL parser not yet implemented
    """
    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # TODO: Implement full JSQL parser
    # For now, raise NotImplementedError
    msg = (
        'JSQL parser not yet implemented. '
        'This is a placeholder for future implementation. '
        'Use uma_read, uma_save, uma_delete, uma_meta for now.'
    )
    raise NotImplementedError(msg)
