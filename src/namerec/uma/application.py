"""UMA application class - main entry point for UMA operations."""

from collections.abc import Mapping
from dataclasses import dataclass
from dataclasses import field
from typing import Any

from namerec.uma.core.access import check_access
from namerec.uma.core.context import UMAContext
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.jsql.cache import CacheBackend
from namerec.uma.jsql.cache import MemoryCacheBackend
from namerec.uma.jsql.executor import JSQLExecutor
from namerec.uma.registry import EntityRegistry


@dataclass
class UMA:
    """
    UMA application instance with encapsulated state.

    Manages entity registry, namespace configurations, and provides
    high-level API for data operations with optional query caching.

    Example:
        # Create UMA instance with default memory cache
        uma = UMA.create({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Create with Redis cache
        from namerec.uma.jsql.cache.redis import RedisCacheBackend
        uma = UMA.create(
            namespace_configs={'main': config},
            cache_backend=RedisCacheBackend('redis://localhost:6379/0'),
        )

        # Use UMA
        user = await uma.read('users', id=1)
        await uma.save('users', {'name': 'John', 'email': 'john@example.com'})

        # Query with caching
        result = await uma.select({'from': 'users', 'select': ['*']})

        # Cache management
        stats = uma.cache_stats()
        uma.disable_cache()  # Temporarily disable for debugging
        uma.enable_cache()
        uma.clear_cache()
    """

    @dataclass
    class _EntityOperationParams:
        """Internal helper: parameters prepared for entity operation."""

        entity: EntityName
        context: UMAContext
        handler: EntityHandler

    registry: EntityRegistry
    namespace_configs: Mapping[str, NamespaceConfig]
    default_namespace: str | None = None
    cache_backend: CacheBackend | None = field(default=None)
    cache_enabled: bool = field(default=True)

    @classmethod
    def create(
        cls,
        namespace_configs: Mapping[str, NamespaceConfig],
        default_handler: type[EntityHandler] | None = None,
        cache_backend: CacheBackend | None = None,
        cache_enabled: bool = True,
    ) -> 'UMA':
        """
        Create UMA application instance.

        Args:
            namespace_configs: Mapping {namespace: NamespaceConfig}
            default_handler: Default handler class for unregistered entities
            cache_backend: Optional cache backend (default: MemoryCacheBackend)
            cache_enabled: Enable caching by default (default: True)

        Returns:
            Initialized UMA instance

        Raises:
            ValueError: If namespace_configs is empty

        Behaviour:
        - If single namespace: it becomes default
        - If multiple namespaces: no default, explicit namespace required
        - Cache enabled by default with MemoryCacheBackend (128 queries)

        Example:
            # Single namespace with default memory cache
            uma = UMA.create({
                'main': NamespaceConfig(
                    engine=engine,
                    metadata_provider=DefaultMetadataProvider(),
                ),
            })

            # With Redis cache for multi-process deployment
            from namerec.uma.jsql.cache.redis import RedisCacheBackend
            uma = UMA.create(
                namespace_configs={'main': config},
                cache_backend=RedisCacheBackend('redis://localhost:6379/0'),
            )

            # Disable cache
            uma = UMA.create(
                namespace_configs={'main': config},
                cache_enabled=False,
            )
        """
        if not namespace_configs:
            msg = 'At least one namespace configuration required'
            raise ValueError(msg)

        # Determine default namespace
        default_ns = next(iter(namespace_configs.keys())) if len(namespace_configs) == 1 else None

        # Create registry
        registry = EntityRegistry(default_handler or DefaultEntityHandler)

        # Create default cache backend if not provided
        if cache_backend is None and cache_enabled:
            cache_backend = MemoryCacheBackend(max_size=128)

        return cls(
            registry=registry,
            namespace_configs=namespace_configs,
            default_namespace=default_ns,
            cache_backend=cache_backend,
            cache_enabled=cache_enabled,
        )

    # ========== Public API Methods ==========

    async def read(
        self,
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
        """
        params = await self._prepare_operation(
            entity_name, Operation.READ, namespace, user_context
        )
        return await params.handler.read(params.entity, id_value, params.context)

    async def save(
        self,
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
        """
        # Determine operation (create or update)
        has_id = any(data.get(key) is not None for key in ['id', 'ID'])
        operation = Operation.UPDATE if has_id else Operation.CREATE

        params = await self._prepare_operation(
            entity_name, operation, namespace, user_context
        )
        return await params.handler.save(params.entity, data, params.context)

    async def delete(
        self,
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
        """
        params = await self._prepare_operation(
            entity_name, Operation.DELETE, namespace, user_context
        )
        return await params.handler.delete(params.entity, id_value, params.context)

    async def entity_details(
        self,
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
        """
        params = await self._prepare_operation(
            entity_name, Operation.META, namespace, user_context
        )
        return await params.handler.meta(params.entity, params.context)

    async def entity_list(
        self,
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
            ValueError: If namespace not found
        """
        # Create context for namespace
        context = self._create_context(namespace, user_context)

        # Check access (empty entity_name means "list entities" operation)
        self._check_access('', Operation.META, context)

        # Get list from MetadataProvider
        entities = await context.metadata_provider.list_entities(
            context.namespace,
            context,
        )

        # Format with namespace prefix (if not default)
        if context.namespace != self.default_namespace:
            return [f'{context.namespace}:{e}' for e in entities]

        return entities

    async def select(
        self,
        jsql: dict,
        params: dict | None = None,
        user_context: Any = None,
        namespace: str | None = None,
    ) -> dict:
        """
        Execute JSQL query.

        All logic (FROM parsing, namespace resolution, context creation,
        access control) is handled by stateless JSQLExecutor.

        Args:
            jsql: JSQL query dictionary (must contain 'from' field)
            params: Query parameters
            user_context: User context for access control
            namespace: Optional namespace (ignored - namespace from FROM clause is used)

        Returns:
            Dictionary with 'meta' and 'data' keys

        Raises:
            UMAAccessDeniedError: If access denied
            ValueError: If JSQL invalid
            JSQLSyntaxError: If JSQL syntax is invalid
            JSQLExecutionError: If query execution fails
        """
        _ = namespace
        result = await JSQLExecutor.execute(
            jsql=jsql,
            params=params,
            namespace_configs=self.namespace_configs,
            user_context=user_context,
            cache_backend=self.cache_backend if self.cache_enabled else None,
        )

        return result.to_dict()

    # ========== Internal Helper Methods ==========

    async def _prepare_operation(
        self,
        entity_name: str,
        operation: Operation,
        namespace: str | None = None,
        user_context: Any = None,
    ) -> _EntityOperationParams:
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
            ValueError: If namespace not found
        """
        # Parse entity name
        entity = parse_entity_name(entity_name)
        if namespace and not entity.namespace:
            entity = EntityName(entity=entity.entity, namespace=namespace)

        # Create context for namespace
        context = self._create_context(entity.namespace, user_context)

        # Check access
        self._check_access(str(entity), operation, context)

        # Get handler
        handler = await self.registry.get_handler(entity, context)

        return self._EntityOperationParams(entity, context, handler)

    def _create_context(
        self,
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
            ValueError: If namespace not found or default not set
            RuntimeError: If cannot determine namespace
        """
        # Get namespace config (resolves default if namespace is None)
        config = self._get_namespace_config(namespace)

        # Determine actual namespace name
        actual_namespace = namespace or self.default_namespace
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

    def _get_namespace_config(self, namespace: str | None = None) -> NamespaceConfig:
        """
        Get configuration for a namespace.

        Args:
            namespace: Namespace name (None = use default)

        Returns:
            NamespaceConfig for the requested namespace

        Raises:
            ValueError: If namespace not found or default not set when required
        """
        # Resolve namespace
        if namespace is None:
            if self.default_namespace is None:
                msg = (
                    'No default namespace configured. '
                    'Explicit namespace required when multiple namespaces are configured.'
                )
                raise ValueError(msg)
            namespace = self.default_namespace

        if namespace not in self.namespace_configs:
            available = ', '.join(self.namespace_configs.keys())
            msg = f'Namespace "{namespace}" not found. Available namespaces: {available}'
            raise ValueError(msg)

        return self.namespace_configs[namespace]

    def _check_access(
        self,
        entity_name: str,
        operation: Operation | str,
        context: UMAContext,
    ) -> None:
        """
        Check access and raise exception if access denied.

        Args:
            entity_name: Entity name (empty string for "list entities" operation)
            operation: Operation name
            context: Execution context

        Raises:
            UMAAccessDeniedError: If access denied
        """
        check_access(
            metadata_provider=context.metadata_provider,
            entity_name=entity_name,
            operation=operation,
            user_context=context.user_context,
        )

    # ========== Cache Management API ==========

    def enable_cache(self) -> None:
        """
        Enable query caching.

        Useful for re-enabling cache after temporary disabling for debugging.

        Example:
            uma.disable_cache()
            # ... debug without cache ...
            uma.enable_cache()
        """
        self.cache_enabled = True

    def disable_cache(self) -> None:
        """
        Disable query caching.

        Useful for debugging when you need to ensure queries hit the database.
        Cache contents are preserved - only lookup is disabled.

        Example:
            # Disable cache for debugging
            uma.disable_cache()
            result = await uma.select({'from': 'users', 'select': ['*']})
            uma.enable_cache()
        """
        self.cache_enabled = False

    def clear_cache(self) -> None:
        """
        Clear all cached queries.

        Use when you need to invalidate cache (e.g., after schema changes).

        Example:
            # After schema migration
            uma.clear_cache()
        """
        if self.cache_backend:
            self.cache_backend.clear()

    def cache_stats(self) -> dict[str, Any]:
        """
        Get cache statistics.

        Returns:
            Dictionary with cache statistics:
            - backend: Backend type ('memory', 'redis', etc.)
            - size: Current number of cached queries
            - hits: Cache hit count
            - misses: Cache miss count
            - hit_rate: Hit rate (0.0-1.0)
            - Additional backend-specific stats

        Example:
            stats = uma.cache_stats()
            print(f"Cache hit rate: {stats['hit_rate']:.2%}")
            print(f"Cached queries: {stats['size']}")
        """
        if self.cache_backend:
            return self.cache_backend.stats()

        return {
            'backend': 'none',
            'size': 0,
            'hits': 0,
            'misses': 0,
            'hit_rate': 0.0,
        }
