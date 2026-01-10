"""Metadata providers for UMA."""

from sqlalchemy import Engine
from sqlalchemy import MetaData
from sqlalchemy import Table

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation


class DefaultMetadataProvider:
    """
    Default metadata provider with lazy loading.

    Features:
    - Lazy loading of database schema via reflect()
    - Per-namespace caching of metadata
    - Optional schema support (PostgreSQL)
    - Optional preloading for production

    Can be extended to provide additional metadata from external sources.
    """

    def __init__(
        self,
        schema: str | None = None,
        metadata_store: dict[str, dict] | None = None,
    ) -> None:
        """
        Initialize metadata provider.

        Args:
            schema: PostgreSQL schema name (None = default/public)
            metadata_store: Optional dictionary with custom metadata
        """
        self._schema = schema
        self._metadata_store = metadata_store or {}

        # Lazy loading caches (per namespace)
        self._metadata_cache: dict[str, MetaData] = {}
        self._tables_cache: dict[str, dict[str, Table]] = {}

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
        # Determine namespace: use entity's namespace, or context's namespace, or 'default'
        if entity_name.namespace:
            namespace = entity_name.namespace
        elif context is not None:
            namespace = context.namespace
        else:
            namespace = 'default'

        # Check cache
        if namespace in self._tables_cache:
            if entity_name.entity in self._tables_cache[namespace]:
                return self._tables_cache[namespace][entity_name.entity]

        # Load metadata for namespace if not cached
        if namespace not in self._metadata_cache:
            await self._load_metadata(namespace, engine, context)

        # Check if table exists after loading
        if entity_name.entity not in self._tables_cache[namespace]:
            raise UMANotFoundError(
                str(entity_name),
                f'Table "{entity_name.entity}" not found in namespace "{namespace}"',
            )

        return self._tables_cache[namespace][entity_name.entity]

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
        if namespace not in self._tables_cache:
            await self._load_metadata(namespace, context.engine, context)

        return sorted(self._tables_cache[namespace].keys())

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

        Call after uma_initialize() to avoid first-request latency.

        Args:
            engine: SQLAlchemy Engine
            namespace: Namespace to preload (default: 'default')
        """
        if namespace not in self._metadata_cache:
            await self._load_metadata(namespace, engine)

    async def _load_metadata(
        self,
        namespace: str,
        engine: Engine,
        context: UMAContext | None = None,
    ) -> None:
        """
        Load metadata from database.

        Args:
            namespace: Namespace identifier for caching
            engine: SQLAlchemy Engine
            context: Optional UMAContext to cache metadata in

        Raises:
            RuntimeError: If reflection fails
        """
        try:
            metadata = MetaData()

            # Perform reflection (sync operation via run_sync)
            # Check if engine is async or sync using dialect attribute
            if hasattr(engine.dialect, 'is_async') and engine.dialect.is_async:
                # Async engine
                async with engine.begin() as conn:
                    await conn.run_sync(metadata.reflect, schema=self._schema)
            else:
                # Sync engine
                metadata.reflect(bind=engine, schema=self._schema)

            # Cache metadata and tables
            self._metadata_cache[namespace] = metadata
            self._tables_cache[namespace] = {t.name: t for t in metadata.tables.values()}

            # Also cache in context if provided (for JSQL parser compatibility)
            if context is not None:
                context._set_metadata(metadata)

        except Exception as e:
            msg = f'Failed to reflect metadata for namespace "{namespace}": {e}'
            raise RuntimeError(msg) from e

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
