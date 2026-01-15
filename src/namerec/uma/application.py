"""UMA application class - main entry point for UMA operations."""

from collections.abc import Mapping
from dataclasses import dataclass
from dataclasses import field
from typing import Any

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
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
            from namerec.uma.jsql.cache_backend import RedisCacheBackend
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
            JSQLSyntaxError: If JSQL syntax is invalid
            JSQLExecutionError: If query execution fails
        """
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
        context = self._create_context(entity.namespace, user_context)

        # Extract all entities from JSQL (including CTEs, joins)
        entities_to_check = self._extract_entities_from_jsql(jsql)

        # Check access for all entities
        for entity_name_to_check in entities_to_check:
            self._check_access(entity_name_to_check, Operation.SELECT, context)

        # Execute JSQL query with caching
        executor = JSQLExecutor(
            context,
            cache_backend=self.cache_backend,
            cache_enabled=self.cache_enabled,
        )
        result = await executor.execute(jsql, params)

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

    def _extract_entities_from_jsql(self, jsql: dict[str, Any]) -> set[str]:
        """
        Extract all entity names from JSQL query for access control.

        Recursively extracts entities from:
        - FROM clause
        - WITH clause (CTEs)
        - JOIN clauses
        - Subqueries

        Args:
            jsql: JSQL query dictionary

        Returns:
            Set of entity names (qualified with namespace if present)

        Note:
            This is a conservative extraction - it may include entities that
            are not directly queried (e.g., in expressions), but this is
            safer for access control.
        """
        entities: set[str] = set()

        # Extract FROM entity
        if from_entity := jsql.get('from'):
            if isinstance(from_entity, str):
                entities.add(from_entity)
            elif isinstance(from_entity, dict):
                # Handle subquery in FROM
                entities.update(self._extract_entities_from_jsql(from_entity))

        # Extract CTEs (WITH clause)
        # CTE can be either list of dicts [{'name': 'cte1', 'query': {...}}]
        # or dict of named CTEs {'cte1': {...}}
        # Note: CTEs themselves don't need access control (they're query definitions),
        # but we need to check access to entities used in CTE definitions
        if with_clauses := jsql.get('with'):
            if isinstance(with_clauses, list):
                for cte in with_clauses:
                    if isinstance(cte, dict):
                        # Extract from CTE definition
                        if cte_query := cte.get('query'):
                            if isinstance(cte_query, dict):
                                entities.update(self._extract_entities_from_jsql(cte_query))
            elif isinstance(with_clauses, dict):
                for cte_def in with_clauses.values():
                    if isinstance(cte_def, dict):
                        entities.update(self._extract_entities_from_jsql(cte_def))

        # Extract JOINs
        if joins := jsql.get('joins'):
            for join in joins:
                if isinstance(join, dict):
                    # Extract join table
                    if join_table := join.get('table'):
                        if isinstance(join_table, str):
                            entities.add(join_table)
                        elif isinstance(join_table, dict):
                            # Subquery in join
                            entities.update(self._extract_entities_from_jsql(join_table))

                    # Extract entities from ON clause
                    if on_clause := join.get('on'):
                        if isinstance(on_clause, dict):
                            entities.update(self._extract_entities_from_expression(on_clause))

        # Extract entities from WHERE, HAVING, GROUP_BY, ORDER_BY
        for clause in ['where', 'having', 'group_by', 'order_by']:
            if clause_data := jsql.get(clause):
                if isinstance(clause_data, dict):
                    entities.update(self._extract_entities_from_expression(clause_data))
                elif isinstance(clause_data, list):
                    for item in clause_data:
                        if isinstance(item, dict):
                            entities.update(self._extract_entities_from_expression(item))

        # Extract entities from SELECT clause
        if select_clause := jsql.get('select'):
            if isinstance(select_clause, list):
                for select_item in select_clause:
                    if isinstance(select_item, dict):
                        entities.update(self._extract_entities_from_expression(select_item))

        return entities

    def _extract_entities_from_expression(self, expr: dict[str, Any]) -> set[str]:
        """
        Extract entity names from JSQL expression.

        Recursively processes:
        - Field references
        - Function calls
        - Operators
        - Subqueries

        Args:
            expr: JSQL expression dictionary

        Returns:
            Set of entity names
        """
        entities: set[str] = set()

        # Field reference
        if field := expr.get('field'):
            if isinstance(field, str):
                # Extract entity from qualified field (table.column)
                if '.' in field:
                    entity_part = field.split('.', 1)[0]
                    # Skip if it's a function or special value
                    if not entity_part.isupper():
                        entities.add(entity_part)
            elif isinstance(field, dict):
                entities.update(self._extract_entities_from_expression(field))

        # Function call
        if 'func' in expr:
            if args := expr.get('args'):
                if isinstance(args, list):
                    for arg in args:
                        if isinstance(arg, dict):
                            entities.update(self._extract_entities_from_expression(arg))
                elif isinstance(args, dict):
                    entities.update(self._extract_entities_from_expression(args))

        # Binary operator
        if 'left' in expr and 'right' in expr:
            if isinstance(expr['left'], dict):
                entities.update(self._extract_entities_from_expression(expr['left']))
            if isinstance(expr['right'], dict):
                entities.update(self._extract_entities_from_expression(expr['right']))

        # Subquery
        if 'subquery' in expr:
            if isinstance(expr['subquery'], dict):
                entities.update(self._extract_entities_from_jsql(expr['subquery']))

        # Expression
        if 'expr' in expr:
            if isinstance(expr['expr'], dict):
                entities.update(self._extract_entities_from_expression(expr['expr']))

        return entities

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
        if isinstance(operation, str):
            operation = Operation(operation)

        # Check if metadata provider has can() method
        if hasattr(context.metadata_provider, 'can'):
            if not context.metadata_provider.can(entity_name, operation, context):  # type: ignore[union-attr]
                raise UMAAccessDeniedError(entity_name, operation.value)

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

        # Check if metadata provider has can() method
        if hasattr(context.metadata_provider, 'can'):
            if not context.metadata_provider.can(entity_name, operation, context):  # type: ignore[union-attr]
                raise UMAAccessDeniedError(entity_name, operation.value)
