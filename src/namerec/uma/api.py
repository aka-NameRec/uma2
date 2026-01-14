"""High-level API functions for UMA."""

from collections.abc import Mapping
from dataclasses import dataclass
from typing import Any

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


@dataclass
class EntityOperationParams:
    """Parameters prepared for entity operation."""

    entity: EntityName
    context: UMAContext
    handler: EntityHandler


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


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


def _create_context(
    namespace: str | None = None,
    user_context: Any = None,
    **extra: Any,
) -> UMAContext:
    """
    Create context for operation.

    Args:
        namespace: Namespace name (None = use default)
        user_context: User context for access control
        **extra: Additional context fields

    Returns:
        UMAContext instance

    Raises:
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
    if actual_namespace is None:
        msg = 'Cannot determine namespace'
        raise RuntimeError(msg)

    return UMAContext(
        engine=config.engine,
        metadata_provider=config.metadata_provider,
        namespace=actual_namespace,
        user_context=user_context,
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


async def _prepare_entity_operation(
    entity_name: str,
    operation: Operation,
    namespace: str | None = None,
    user_context: Any = None,
) -> EntityOperationParams:
    """
    Prepare common parameters for entity operation.

    Args:
        entity_name: Entity name (can include namespace)
        operation: Operation type for access control
        namespace: Optional namespace override
        user_context: User context for access control

    Returns:
        EntityOperationParams with entity, context, and handler

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler
    handler = await get_registry().get_handler(entity, context)

    return EntityOperationParams(entity, context, handler)


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,
    user_context: Any = None,
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
    params = await _prepare_entity_operation(
        entity_name, Operation.READ, namespace, user_context
    )
    return await params.handler.read(params.entity, id_value, params.context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,
    namespace: str | None = None,
) -> Any:
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
    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    params = await _prepare_entity_operation(
        entity_name, operation, namespace, user_context
    )
    return await params.handler.save(params.entity, data, params.context)


async def uma_delete(
    entity_name: str,
    id_value: Any,
    user_context: Any = None,
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
    params = await _prepare_entity_operation(
        entity_name, Operation.DELETE, namespace, user_context
    )
    return await params.handler.delete(params.entity, id_value, params.context)


async def uma_entity_details(
    entity_name: str,
    user_context: Any = None,
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
    params = await _prepare_entity_operation(
        entity_name, Operation.META, namespace, user_context
    )
    return await params.handler.meta(params.entity, params.context)


async def uma_entity_list(
    user_context: Any = None,
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter (None = use default)

    Returns:
        List of entity names (with namespace prefix if not default)

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found
    """
    # Create context for namespace
    context = _create_context(namespace, user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )

    # Format with namespace prefix (if not default)
    default_ns = get_default_namespace()
    if context.namespace != default_ns:
        return [f'{context.namespace}:{e}' for e in entities]

    return entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,
    namespace: str | None = None,
) -> dict:
    """
    Execute JSQL query.

    Args:
        jsql: JSQL query dictionary (must contain 'from' field)
        params: Query parameters
        user_context: User context for access control
        namespace: Optional namespace

    Returns:
        Dictionary with 'meta' and 'data' keys

    Raises:
        UMAAccessDeniedError: If access denied
        ValueError: If JSQL invalid
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Parse entity name to extract namespace
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()
