"""Entity registry for UMA."""

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.handlers.base import DefaultEntityHandler


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
        self._default_handler = default_handler or DefaultEntityHandler

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

    async def get_handler(
        self,
        entity_name: EntityName,
        context: UMAContext,
    ) -> type[EntityHandler]:
        """
        Get handler for an entity.

        Logic:
        1. Check explicit registration (custom handler)
        2. Check if entity exists via MetadataProvider
        3. If exists → return default_handler
        4. If not → raise UMANotFoundError

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Handler class

        Raises:
            UMANotFoundError: If entity not registered and not found in database
        """
        # 1. Check explicit registrations (custom handlers)
        key = str(entity_name)
        if key in self._handlers:
            return self._handlers[key]

        # 2. Check if entity exists via MetadataProvider
        if await context.metadata_provider.entity_exists(entity_name, context):
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

    async def list_entities(self, context: UMAContext) -> list[str]:
        """
        List all registered custom handlers.

        Note: This only lists explicitly registered custom handlers,
        not database entities. Use MetadataProvider.list_entities()
        to get all database entities.

        Args:
            context: Execution context

        Returns:
            List of registered entity names
        """
        _ = context
        # Only registered custom handlers
        return sorted(self._handlers.keys())
