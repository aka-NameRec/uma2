"""Type definitions for UMA."""

from dataclasses import dataclass
from enum import Enum
from typing import Any
from typing import Protocol
from typing import runtime_checkable

from sqlalchemy import Select


class Operation(str, Enum):
    """UMA operations enum."""

    SELECT = 'select'
    READ = 'read'
    CREATE = 'create'
    UPDATE = 'update'
    DELETE = 'delete'
    META = 'meta'
    LIST = 'list'  # For listing entities


@dataclass(frozen=True)
class EntityName:
    """
    Structured entity name representation.
    Supports namespaces for disambiguation (e.g., 'db:users' vs 'virtual:users').
    """

    entity: str
    namespace: str | None = None

    def __str__(self) -> str:
        """Format as string for display."""
        if self.namespace:
            return f'{self.namespace}:{self.entity}'
        return self.entity

    def __repr__(self) -> str:
        """Detailed representation."""
        if self.namespace:
            return f"EntityName(entity='{self.entity}', namespace='{self.namespace}')"
        return f"EntityName(entity='{self.entity}')"


@runtime_checkable
class EntityHandler(Protocol):
    """
    Protocol for entity handlers.
    All methods are classmethods - handlers are stateless.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: 'UMAContext',
    ) -> Select:
        """
        Return SQLAlchemy Select for data selection.

        Args:
            entity_name: Entity name
            params: Query parameters from JSQL
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        ...

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: 'UMAContext',
    ) -> dict:
        """
        Read a single record by id.
        Uses select for access checking.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        ...

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: 'UMAContext',
    ) -> Any:
        """
        Save a record (create if id=None, otherwise update).

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        ...

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: 'UMAContext',
    ) -> bool:
        """
        Delete a record by id.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        ...

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: 'UMAContext',
    ) -> dict:
        """
        Return entity metadata.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        ...


@runtime_checkable
class MetadataProvider(Protocol):
    """Protocol for metadata providers."""

    async def get_metadata(
        self,
        entity_name: EntityName,
        context: 'UMAContext',
    ) -> dict:
        """
        Get metadata for an entity.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        ...


# Forward declaration for type checking
from namerec.uma.core.context import UMAContext  # noqa: E402, F401
