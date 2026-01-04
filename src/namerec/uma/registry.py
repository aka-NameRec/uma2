"""Entity registry for UMA."""

from typing import Any
from typing import ClassVar

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName


class EntityRegistry:
    """
    Registry for entity handlers.
    Maps entity names to handler classes.
    """

    def __init__(self, default_handler: type[EntityHandler] | None = None) -> None:
        """
        Initialize registry.

        Args:
            default_handler: Default handler class for unregistered entities
        """
        self._handlers: dict[str, type[EntityHandler]] = {}
        self._default_handler = default_handler
        if default_handler is None:
            # Import here to avoid circular dependency
            from namerec.uma.handlers.base import DefaultEntityHandler

            self._default_handler = DefaultEntityHandler

    def register(
        self,
        entity_name: str | EntityName,
        handler: type[EntityHandler],
    ) -> None:
        """
        Register a handler for an entity.

        Args:
            entity_name: Entity name
            handler: Handler class
        """
        key = str(entity_name) if isinstance(entity_name, EntityName) else entity_name
        self._handlers[key] = handler

    def unregister(self, entity_name: str | EntityName) -> None:
        """
        Unregister a handler.

        Args:
            entity_name: Entity name
        """
        key = str(entity_name) if isinstance(entity_name, EntityName) else entity_name
        self._handlers.pop(key, None)

    def get_handler(
        self,
        entity_name: EntityName,
        context: UMAContext,
    ) -> type[EntityHandler]:
        """
        Get handler for an entity.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Handler class

        Raises:
            UMANotFoundError: If entity not registered and not in database
        """
        # 1. Check explicit registrations
        key = str(entity_name)
        if key in self._handlers:
            return self._handlers[key]

        # 2. Check database tables
        if entity_name.entity in context.metadata.tables:
            return self._default_handler  # type: ignore[return-value]

        # 3. Not found
        raise UMANotFoundError(str(entity_name), f'Entity {entity_name} not found')

    def set_default_handler(self, handler: type[EntityHandler]) -> None:
        """
        Set default handler for unregistered entities.

        Args:
            handler: Handler class
        """
        self._default_handler = handler

    def list_entities(self, context: UMAContext) -> list[str]:
        """
        List all registered entities and database tables.

        Args:
            context: Execution context

        Returns:
            List of entity names
        """
        # Registered entities
        registered = set(self._handlers.keys())

        # Database tables
        db_tables = set(context.metadata.tables.keys())

        # Combine and sort
        all_entities = registered | db_tables
        return sorted(all_entities)


# Global registry singleton
_global_registry: EntityRegistry | None = None


def init_global_registry(
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize global registry.

    Args:
        default_handler: Optional default handler

    Returns:
        Initialized registry
    """
    global _global_registry  # noqa: PLW0603
    _global_registry = EntityRegistry(default_handler=default_handler)
    return _global_registry


def get_global_registry() -> EntityRegistry:
    """
    Get global registry instance.

    Returns:
        Global registry

    Raises:
        RuntimeError: If registry not initialized
    """
    if _global_registry is None:
        # Auto-initialize with default settings
        return init_global_registry()
    return _global_registry
