"""Type definitions for UMA."""

from dataclasses import dataclass
from enum import Enum
from typing import Any
from typing import Protocol
from typing import runtime_checkable

from sqlalchemy import Engine
from sqlalchemy import MetaData
from sqlalchemy import Select
from sqlalchemy import Table


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
class UMAContextSpec(Protocol):
    """
    Protocol specification for UMA execution context.

    Defines the minimal interface required by MetadataProvider and EntityHandler.
    This protocol eliminates circular dependencies between context and protocol definitions.
    """

    engine: Engine
    metadata_provider: 'MetadataProvider'
    namespace: str
    user_context: Any
    cache: Any
    extra: dict[str, Any]

    @property
    def metadata(self) -> MetaData:
        """Get cached metadata for namespace (sync access for JSQL parser)."""
        ...

    def _set_metadata(self, metadata: MetaData) -> None:
        """Set cached metadata (internal use only)."""
        ...


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
        context: UMAContextSpec,
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
        context: UMAContextSpec,
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
        context: UMAContextSpec,
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
        context: UMAContextSpec,
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
        context: UMAContextSpec,
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
    """
    Protocol for metadata providers.

    Metadata providers are responsible for:
    - Lazy loading of database schema (via reflect())
    - Listing entities in a namespace
    - Checking entity existence
    - Providing entity metadata
    """

    async def get_metadata(
        self,
        entity_name: EntityName,
        context: UMAContextSpec,
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

    async def list_entities(
        self,
        namespace: str,
        context: UMAContextSpec,
    ) -> list[str]:
        """
        List all entities in namespace.

        Args:
            namespace: Namespace name
            context: Execution context

        Returns:
            List of entity names (without namespace prefix)

        Raises:
            ValueError: If namespace not found
            ConnectionError: If cannot connect to database
        """
        ...

    async def entity_exists(
        self,
        entity_name: EntityName,
        context: UMAContextSpec,
    ) -> bool:
        """
        Check if entity exists in namespace.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            True if entity exists, False otherwise

        Raises:
            ValueError: If namespace not found
            ConnectionError: If cannot connect to database
        """
        ...

    async def get_table(
        self,
        entity_name: EntityName,
        engine: Engine,
        context: UMAContextSpec | None = None,
    ) -> Table:
        """
        Get SQLAlchemy Table object for entity (lazy loading).

        Performs reflect() if needed and caches result.

        Args:
            entity_name: Entity name
            engine: SQLAlchemy Engine for the namespace
            context: Optional UMAContext to cache metadata in

        Returns:
            SQLAlchemy Table object

        Raises:
            UMANotFoundError: If table not found
            RuntimeError: If reflection fails
        """
        ...

    async def preload(self, engine: Engine, namespace: str = 'default') -> None:
        """
        Preload metadata (optional, for production).

        Call after uma_initialize() to avoid first-request latency.
        This method is optional and may be a no-op for some providers.

        Args:
            engine: SQLAlchemy Engine for the namespace
            namespace: Namespace to preload (default: 'default')
        """
        ...
