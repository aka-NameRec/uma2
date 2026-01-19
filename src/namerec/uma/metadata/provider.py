"""Metadata provider implementation."""

from sqlalchemy import Engine
from sqlalchemy import Table

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.metadata.cache import MetadataCache
from namerec.uma.metadata.reflector import DatabaseReflector


class DefaultMetadataProvider:
    """
    Default metadata provider with lazy loading (composition pattern).

    This class is now a thin facade over specialized components:
    - MetadataCache: Manages caching of reflected schema
    - DatabaseReflector: Handles database reflection

    Features:
    - Lazy loading of database schema via reflect()
    - Per-namespace caching of metadata
    - Optional schema support (PostgreSQL)
    - Optional preloading for production
    - Custom metadata storage per entity

    Can be extended to provide additional metadata from external sources.
    """

    def __init__(
        self,
        schema: str | None = None,
        cache: MetadataCache | None = None,
        reflector: DatabaseReflector | None = None,
    ) -> None:
        """
        Initialize metadata provider.

        Args:
            schema: PostgreSQL schema name (None = default/public)
            cache: Optional custom cache (default: new MetadataCache)
            reflector: Optional custom reflector (default: new DatabaseReflector)
        """
        self._schema = schema
        self._cache = cache or MetadataCache()
        self._reflector = reflector or DatabaseReflector(schema=schema)

    @property
    def schema(self) -> str | None:
        """Get schema name."""
        return self._schema

    async def get_metadata(
        self,
        entity_name: EntityName,
        context: UMAContext,  # noqa: ARG002
    ) -> dict:
        """
        Get custom metadata for an entity.

        Args:
            entity_name: Entity name
            context: Execution context (unused in default implementation)

        Returns:
            Metadata dictionary (empty if not found)
        """
        key = str(entity_name)
        return self._cache.get_custom_metadata(key)

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
        self._cache.set_custom_metadata(key, metadata)

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
        self._cache.update_custom_metadata(key, metadata)

    def clear_metadata(self, entity_name: str | EntityName) -> None:
        """
        Clear metadata for an entity.

        Args:
            entity_name: Entity name
        """
        key = str(entity_name) if isinstance(entity_name, EntityName) else entity_name
        self._cache.clear_custom_metadata(key)

    async def get_table(
        self,
        entity_name: EntityName,
        engine: Engine,
        context: UMAContext | None = None,
    ) -> Table:
        """
        Get SQLAlchemy Table object for entity (lazy loading).

        Args:
            entity_name: Entity name
            engine: SQLAlchemy Engine
            context: Optional UMAContext to cache metadata in

        Returns:
            SQLAlchemy Table object

        Raises:
            UMANotFoundError: If table not found
            RuntimeError: If reflection fails
        """
        namespace = self._resolve_namespace(entity_name, context)

        # Check cache
        table = self._cache.get_table(namespace, entity_name.entity)
        if table is not None:
            return table

        # Load metadata if not cached
        if not self._cache.has_namespace(namespace):
            await self._load_metadata(namespace, engine, context)

        # Check again after loading
        table = self._cache.get_table(namespace, entity_name.entity)
        if table is not None:
            return table

        raise UMANotFoundError(
            str(entity_name),
            f'Table "{entity_name.entity}" not found in namespace "{namespace}"',
        )

    async def list_entities(
        self,
        namespace: str,
        context: UMAContext,
    ) -> list[str]:
        """
        List all entities in namespace.

        Args:
            namespace: Namespace name
            context: Execution context

        Returns:
            List of entity names (without namespace prefix)
        """
        # Load metadata if needed
        if not self._cache.has_namespace(namespace):
            await self._load_metadata(namespace, context.engine, context)

        return self._cache.list_tables(namespace)

    async def entity_exists(
        self,
        entity_name: EntityName,
        context: UMAContext,
    ) -> bool:
        """
        Check if entity exists.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            True if entity exists, False otherwise
        """
        try:
            await self.get_table(entity_name, context.engine, context)
            return True
        except UMANotFoundError:
            return False

    async def preload(self, engine: Engine, namespace: str = 'default') -> None:
        """
        Preload metadata (optional, for production).

        Call after initialization to avoid first-request latency.

        Args:
            engine: SQLAlchemy Engine
            namespace: Namespace to preload (default: 'default')
        """
        if not self._cache.has_namespace(namespace):
            await self._load_metadata(namespace, engine)

    async def _load_metadata(
        self,
        namespace: str,
        engine: Engine,
        context: UMAContext | None = None,
    ) -> None:
        """
        Load metadata from database via reflector.

        Args:
            namespace: Namespace identifier for caching
            engine: SQLAlchemy Engine
            context: Optional UMAContext to cache metadata in

        Raises:
            RuntimeError: If reflection fails
        """
        # Reflect database schema
        metadata = await self._reflector.reflect(engine)

        # Cache metadata
        self._cache.cache_metadata(namespace, metadata)

        # Also cache in context if provided (for JSQL parser compatibility)
        if context is not None:
            context._set_metadata(metadata)

    def _resolve_namespace(
        self,
        entity_name: EntityName,
        context: UMAContext | None,
    ) -> str:
        """
        Resolve namespace for entity.

        Priority: entity's namespace > context's namespace > 'default'

        Args:
            entity_name: Entity name
            context: Optional execution context

        Returns:
            Resolved namespace identifier
        """
        if entity_name.namespace:
            return entity_name.namespace
        if context is not None:
            return context.namespace
        return 'default'

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
        if entity_name == '' and operation in {Operation.META, 'meta'}:
            # Allow listing for everyone by default
            return True

        # Base implementation: allow everything
        # In project, implement real access control based on:
        # - context.user_context (user role, permissions)
        # - entity_name (which entity)
        # - operation (which operation)

        return True
