"""Metadata providers for UMA."""

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation


class DefaultMetadataProvider:
    """
    Default metadata provider.
    Can be extended to provide additional metadata from external sources (e.g., Redis).
    """

    def __init__(self, metadata_store: dict[str, dict] | None = None) -> None:
        """
        Initialize metadata provider.

        Args:
            metadata_store: Optional dictionary with custom metadata
        """
        self._metadata_store = metadata_store or {}

    async def get_metadata(
        self,
        entity_name: EntityName,
        context: UMAContext,  # noqa: ARG002
    ) -> dict:
        """
        Get metadata for an entity.

        Args:
            entity_name: Entity name
            context: Execution context (unused in default implementation)

        Returns:
            Metadata dictionary (empty if not found)
        """
        key = str(entity_name)
        return self._metadata_store.get(key, {})

    def set_metadata(
        self,
        entity_name: str | EntityName,
        metadata: dict,
    ) -> None:
        """
        Set custom metadata for an entity.

        Args:
            entity_name: Entity name
            metadata: Metadata dictionary
        """
        key = str(entity_name) if isinstance(entity_name, EntityName) else entity_name
        self._metadata_store[key] = metadata

    def update_metadata(
        self,
        entity_name: str | EntityName,
        metadata: dict,
    ) -> None:
        """
        Update (merge) metadata for an entity.

        Args:
            entity_name: Entity name
            metadata: Metadata dictionary to merge
        """
        key = str(entity_name) if isinstance(entity_name, EntityName) else entity_name
        if key in self._metadata_store:
            self._metadata_store[key].update(metadata)
        else:
            self._metadata_store[key] = metadata

    def clear_metadata(self, entity_name: str | EntityName) -> None:
        """
        Clear metadata for an entity.

        Args:
            entity_name: Entity name
        """
        key = str(entity_name) if isinstance(entity_name, EntityName) else entity_name
        self._metadata_store.pop(key, None)

    def can(
        self,
        entity_name: str,
        operation: Operation | str,
        context: UMAContext,  # noqa: ARG002
    ) -> bool:
        """
        Check access to operation.
        Base implementation allows everything.
        Override in project for real access control.

        Args:
            entity_name: Entity name (empty string for "list entities" operation)
            operation: Operation to check
            context: Execution context (can be used for user role checking)

        Returns:
            True if access allowed, False otherwise
        """
        # Special case: listing entities
        if entity_name == '' and (operation == Operation.META or operation == 'meta'):
            # Allow listing for everyone by default
            return True

        # Base implementation: allow everything
        # In project, implement real access control based on:
        # - context.user_context (user role, permissions)
        # - entity_name (which entity)
        # - operation (which operation)

        return True
