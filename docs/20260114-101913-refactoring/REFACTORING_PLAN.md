# План рефакторинга UMA
**Дата:** 2026-01-13  
**Приоритет:** Проблемы 1-6, Рекомендации 8-10

---

## Проблема 1: Нарушение DRY в `api.py`

### Текущее состояние
Функции `uma_read`, `uma_save`, `uma_delete`, `uma_entity_details` имеют идентичную структуру:

```python
async def uma_read(entity_name: str, id_value: Any, user_context: Any = None, namespace: str | None = None) -> dict:
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)
    context = _create_context(entity.namespace, user_context)
    _check_access(str(entity), Operation.READ, context)
    handler = await get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)
```

### Решение

**Вариант A: Базовая функция-диспетчер**

```python
async def _dispatch_entity_operation(
    entity_name: str,
    operation: Operation,
    handler_method: str,
    namespace: str | None,
    user_context: Any,
    **operation_args: Any
) -> Any:
    """
    Common dispatcher for entity operations.
    
    Args:
        entity_name: Entity name (with or without namespace prefix)
        operation: Operation type for access control
        handler_method: Name of handler method to call
        namespace: Optional namespace override
        user_context: User context for access control
        **operation_args: Arguments to pass to handler method
    
    Returns:
        Result from handler method
    """
    # Parse entity name and resolve namespace
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)
    
    # Create context and check access
    context = _create_context(entity.namespace, user_context)
    _check_access(str(entity), operation, context)
    
    # Execute operation
    handler = await get_registry().get_handler(entity, context)
    method = getattr(handler, handler_method)
    return await method(entity, context, **operation_args)


# Simplified API functions
async def uma_read(
    entity_name: str,
    id_value: Any,
    user_context: Any = None,
    namespace: str | None = None,
) -> dict:
    """Read a record by id."""
    return await _dispatch_entity_operation(
        entity_name=entity_name,
        operation=Operation.READ,
        handler_method='read',
        namespace=namespace,
        user_context=user_context,
        id_value=id_value,
    )


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,
    namespace: str | None = None,
) -> Any:
    """Save a record (create if id=None, otherwise update)."""
    # Determine operation type
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE
    
    return await _dispatch_entity_operation(
        entity_name=entity_name,
        operation=operation,
        handler_method='save',
        namespace=namespace,
        user_context=user_context,
        data=data,
    )


async def uma_delete(
    entity_name: str,
    id_value: Any,
    user_context: Any = None,
    namespace: str | None = None,
) -> bool:
    """Delete a record by id."""
    return await _dispatch_entity_operation(
        entity_name=entity_name,
        operation=Operation.DELETE,
        handler_method='delete',
        namespace=namespace,
        user_context=user_context,
        id_value=id_value,
    )


async def uma_entity_details(
    entity_name: str,
    user_context: Any = None,
    namespace: str | None = None,
) -> dict:
    """Get entity metadata."""
    return await _dispatch_entity_operation(
        entity_name=entity_name,
        operation=Operation.META,
        handler_method='meta',
        namespace=namespace,
        user_context=user_context,
    )
```

**Результат:**
- Сокращение кода с ~200 строк до ~80 строк
- Единая точка для логики доступа и диспетчеризации
- Легче тестировать и поддерживать

**Трудозатраты:** 2-3 часа  
**Риски:** Низкие (не breaking change)

---

## Проблема 2: Глобальное состояние

### Текущее состояние
```python
# api.py
_registry: EntityRegistry | None = None

# namespace_config.py
_namespace_configs: Mapping[str, NamespaceConfig] | None = None
_default_namespace: str | None = None

# registry.py
_global_registry: EntityRegistry | None = None
```

### Решение

**Создать контейнер приложения:**

```python
# src/namerec/uma/application.py

from dataclasses import dataclass, field
from typing import Any, Mapping

from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.types import EntityHandler, EntityName, Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry


@dataclass
class UMAApplication:
    """
    UMA application instance.
    
    Encapsulates all UMA state and configuration.
    Replaces global variables with explicit dependency injection.
    """
    
    namespace_configs: Mapping[str, NamespaceConfig]
    default_namespace: str | None = None
    registry: EntityRegistry = field(default_factory=lambda: EntityRegistry(DefaultEntityHandler))
    
    def __post_init__(self) -> None:
        """Validate configuration."""
        if not self.namespace_configs:
            msg = 'At least one namespace configuration required'
            raise ValueError(msg)
        
        # Auto-set default namespace if single namespace
        if len(self.namespace_configs) == 1 and self.default_namespace is None:
            self.default_namespace = next(iter(self.namespace_configs.keys()))
    
    def get_namespace_config(self, namespace: str | None = None) -> NamespaceConfig:
        """Get namespace configuration."""
        if namespace is None:
            if self.default_namespace is None:
                msg = 'No default namespace configured'
                raise ValueError(msg)
            namespace = self.default_namespace
        
        if namespace not in self.namespace_configs:
            available = ', '.join(self.namespace_configs.keys())
            msg = f'Namespace "{namespace}" not found. Available: {available}'
            raise ValueError(msg)
        
        return self.namespace_configs[namespace]
    
    # API Methods
    async def read(
        self,
        entity_name: str,
        id_value: Any,
        user_context: Any = None,
        namespace: str | None = None,
    ) -> dict:
        """Read a record by id."""
        return await self._dispatch_operation(
            entity_name=entity_name,
            operation=Operation.READ,
            handler_method='read',
            namespace=namespace,
            user_context=user_context,
            id_value=id_value,
        )
    
    async def save(
        self,
        entity_name: str,
        data: dict,
        user_context: Any = None,
        namespace: str | None = None,
    ) -> Any:
        """Save a record."""
        has_id = any(data.get(key) is not None for key in ['id', 'ID'])
        operation = Operation.UPDATE if has_id else Operation.CREATE
        
        return await self._dispatch_operation(
            entity_name=entity_name,
            operation=operation,
            handler_method='save',
            namespace=namespace,
            user_context=user_context,
            data=data,
        )
    
    async def delete(
        self,
        entity_name: str,
        id_value: Any,
        user_context: Any = None,
        namespace: str | None = None,
    ) -> bool:
        """Delete a record by id."""
        return await self._dispatch_operation(
            entity_name=entity_name,
            operation=Operation.DELETE,
            handler_method='delete',
            namespace=namespace,
            user_context=user_context,
            id_value=id_value,
        )
    
    async def entity_details(
        self,
        entity_name: str,
        user_context: Any = None,
        namespace: str | None = None,
    ) -> dict:
        """Get entity metadata."""
        return await self._dispatch_operation(
            entity_name=entity_name,
            operation=Operation.META,
            handler_method='meta',
            namespace=namespace,
            user_context=user_context,
        )
    
    async def entity_list(
        self,
        user_context: Any = None,
        namespace: str | None = None,
    ) -> list[str]:
        """Get list of all available entities."""
        from namerec.uma.core.context import UMAContext
        
        config = self.get_namespace_config(namespace)
        actual_namespace = namespace or self.default_namespace
        
        context = UMAContext(
            engine=config.engine,
            metadata_provider=config.metadata_provider,
            namespace=actual_namespace,
            user_context=user_context,
        )
        
        # Check access
        if hasattr(config.metadata_provider, 'can'):
            if not config.metadata_provider.can('', Operation.META, context):
                from namerec.uma.core.exceptions import UMAAccessDeniedError
                raise UMAAccessDeniedError('', Operation.META.value)
        
        entities = await config.metadata_provider.list_entities(actual_namespace, context)
        
        # Format with namespace prefix if not default
        if actual_namespace != self.default_namespace:
            return [f'{actual_namespace}:{e}' for e in entities]
        
        return entities
    
    async def select(
        self,
        jsql: dict,
        params: dict | None = None,
        user_context: Any = None,
        namespace: str | None = None,
    ) -> dict:
        """Execute JSQL query."""
        from namerec.uma.jsql.executor import JSQLExecutor
        from namerec.uma.core.context import UMAContext
        
        # Extract entity_name from JSQL
        entity_name = jsql.get('from')
        if not entity_name:
            msg = "JSQL must contain 'from' field"
            raise ValueError(msg)
        
        # Parse entity name
        entity = parse_entity_name(entity_name)
        if namespace and not entity.namespace:
            entity = EntityName(entity=entity.entity, namespace=namespace)
        
        # Get config and create context
        config = self.get_namespace_config(entity.namespace)
        actual_namespace = entity.namespace or self.default_namespace
        
        context = UMAContext(
            engine=config.engine,
            metadata_provider=config.metadata_provider,
            namespace=actual_namespace,
            user_context=user_context,
        )
        
        # Check access
        if hasattr(config.metadata_provider, 'can'):
            if not config.metadata_provider.can(str(entity), Operation.SELECT, context):
                from namerec.uma.core.exceptions import UMAAccessDeniedError
                raise UMAAccessDeniedError(str(entity), Operation.SELECT.value)
        
        # Execute JSQL query
        executor = JSQLExecutor(context)
        result = await executor.execute(jsql, params)
        
        return result.to_dict()
    
    # Internal helper
    async def _dispatch_operation(
        self,
        entity_name: str,
        operation: Operation,
        handler_method: str,
        namespace: str | None,
        user_context: Any,
        **operation_args: Any
    ) -> Any:
        """Common dispatcher for entity operations."""
        from namerec.uma.core.context import UMAContext
        
        # Parse entity name
        entity = parse_entity_name(entity_name)
        if namespace and not entity.namespace:
            entity = EntityName(entity=entity.entity, namespace=namespace)
        
        # Get config and create context
        config = self.get_namespace_config(entity.namespace)
        actual_namespace = entity.namespace or self.default_namespace
        
        context = UMAContext(
            engine=config.engine,
            metadata_provider=config.metadata_provider,
            namespace=actual_namespace,
            user_context=user_context,
        )
        
        # Check access
        if hasattr(config.metadata_provider, 'can'):
            if not config.metadata_provider.can(str(entity), operation, context):
                from namerec.uma.core.exceptions import UMAAccessDeniedError
                raise UMAAccessDeniedError(str(entity), operation.value)
        
        # Get handler and execute
        handler = await self.registry.get_handler(entity, context)
        method = getattr(handler, handler_method)
        return await method(entity, **operation_args, context=context)
```

**Обновить API для обратной совместимости:**

```python
# src/namerec/uma/api.py

from typing import Any
from namerec.uma.application import UMAApplication
from namerec.uma.core.namespace_config import NamespaceConfig

# Global application instance (for backward compatibility)
_app: UMAApplication | None = None


def uma_initialize(
    namespace_configs: dict[str, NamespaceConfig],
    default_handler: type | None = None,
) -> UMAApplication:
    """
    Initialize UMA application.
    
    Returns UMAApplication instance that should be used directly.
    Also sets global instance for backward compatibility.
    """
    global _app
    
    app = UMAApplication(
        namespace_configs=namespace_configs,
    )
    
    if default_handler:
        app.registry.set_default_handler(default_handler)
    
    _app = app
    return app


def get_app() -> UMAApplication:
    """Get initialized UMA application."""
    if _app is None:
        msg = 'UMA not initialized. Call uma_initialize() first.'
        raise RuntimeError(msg)
    return _app


# Backward compatible API (delegates to app)
async def uma_read(entity_name: str, id_value: Any, user_context: Any = None, namespace: str | None = None) -> dict:
    """Read a record by id."""
    return await get_app().read(entity_name, id_value, user_context, namespace)


async def uma_save(entity_name: str, data: dict, user_context: Any = None, namespace: str | None = None) -> Any:
    """Save a record."""
    return await get_app().save(entity_name, data, user_context, namespace)


async def uma_delete(entity_name: str, id_value: Any, user_context: Any = None, namespace: str | None = None) -> bool:
    """Delete a record."""
    return await get_app().delete(entity_name, id_value, user_context, namespace)


# ... остальные функции аналогично
```

**Использование (новый стиль):**

```python
# Вместо глобальной инициализации
app = uma_initialize({
    'main': NamespaceConfig(engine=engine, metadata_provider=provider)
})

# Использование через app
result = await app.read('users', id=1)
result = await app.select({'from': 'users', 'select': [{'field': '*'}]})

# Или старый стиль (через глобальные функции)
result = await uma_read('users', id=1)
```

**Результат:**
- Устранение глобального состояния
- Возможность создавать несколько изолированных инстансов
- Обратная совместимость сохранена
- Легче тестировать

**Трудозатраты:** 8-10 часов  
**Риски:** Средние (breaking change для advanced use cases)

---

## Проблема 3: Гигантский `converter.py`

### Решение: Разбить на модули

**Новая структура:**

```
jsql/converter/
├── __init__.py                 # Public API
├── jsql_to_sql.py             # JSQL → SQL (главная функция)
├── sql_to_jsql.py             # SQL → JSQL (главная функция)
├── expressions.py             # Конверсия выражений
├── conditions.py              # Конверсия условий
├── operators.py               # Обработка операторов
├── joins.py                   # Обработка JOIN'ов
└── constants.py               # Маппинги и константы (перенести из jsql/constants.py)
```

**Детали см. в файле REFACTORING_PLAN_PART2.md**

**Трудозатраты:** 6-8 часов  
**Риски:** Низкие (чисто внутренний рефакторинг)

---

## Проблема 4: `DefaultMetadataProvider` - нарушение SRP

### Решение: Разделить на компоненты

```python
# src/namerec/uma/metadata/cache.py
from sqlalchemy import MetaData, Table

class MetadataCache:
    """Cache for database metadata and tables."""
    
    def __init__(self, max_size: int = 100) -> None:
        self._metadata_cache: dict[str, MetaData] = {}
        self._tables_cache: dict[str, dict[str, Table]] = {}
        self._max_size = max_size
    
    def get_metadata(self, namespace: str) -> MetaData | None:
        """Get cached metadata for namespace."""
        return self._metadata_cache.get(namespace)
    
    def set_metadata(self, namespace: str, metadata: MetaData) -> None:
        """Cache metadata for namespace."""
        self._metadata_cache[namespace] = metadata
        self._tables_cache[namespace] = {t.name: t for t in metadata.tables.values()}
    
    def get_table(self, namespace: str, table_name: str) -> Table | None:
        """Get cached table."""
        return self._tables_cache.get(namespace, {}).get(table_name)
    
    def list_tables(self, namespace: str) -> list[str]:
        """List all tables in namespace."""
        return sorted(self._tables_cache.get(namespace, {}).keys())


# src/namerec/uma/metadata/reflector.py
class DatabaseReflector:
    """Reflects database schema using SQLAlchemy."""
    
    def __init__(self, schema: str | None = None) -> None:
        self._schema = schema
    
    async def reflect(self, engine: Engine) -> MetaData:
        """Reflect database schema."""
        metadata = MetaData()
        
        if hasattr(engine.dialect, 'is_async') and engine.dialect.is_async:
            async with engine.begin() as conn:
                await conn.run_sync(metadata.reflect, schema=self._schema)
        else:
            metadata.reflect(bind=engine, schema=self._schema)
        
        return metadata


# src/namerec/uma/metadata/provider.py
class DefaultMetadataProvider:
    """
    Default metadata provider with lazy loading.
    Now uses composition instead of doing everything itself.
    """
    
    def __init__(
        self,
        schema: str | None = None,
        metadata_store: dict[str, dict] | None = None,
        cache_size: int = 100,
    ) -> None:
        self._schema = schema
        self._metadata_store = metadata_store or {}
        
        # Components
        self._cache = MetadataCache(max_size=cache_size)
        self._reflector = DatabaseReflector(schema=schema)
    
    async def get_table(self, entity_name: EntityName, engine: Engine, context: UMAContext | None = None) -> Table:
        """Get SQLAlchemy Table object."""
        namespace = entity_name.namespace or (context.namespace if context else 'default')
        
        # Check cache
        if table := self._cache.get_table(namespace, entity_name.entity):
            return table
        
        # Load metadata if not cached
        if not self._cache.get_metadata(namespace):
            await self._load_metadata(namespace, engine, context)
        
        # Check again after loading
        if table := self._cache.get_table(namespace, entity_name.entity):
            return table
        
        raise UMANotFoundError(str(entity_name), f'Table not found: {entity_name.entity}')
    
    async def _load_metadata(self, namespace: str, engine: Engine, context: UMAContext | None = None) -> None:
        """Load metadata using reflector and cache it."""
        metadata = await self._reflector.reflect(engine)
        self._cache.set_metadata(namespace, metadata)
        
        if context:
            context._set_metadata(metadata)
```

**Результат:**
- Четкое разделение ответственностей
- Легче тестировать каждый компонент
- Можно заменить кэш на Redis, если нужно

**Трудозатраты:** 4-5 часов  
**Риски:** Низкие (внутренний рефакторинг)

---

## Проблема 5: Неэффективный поиск колонок в `parser.py`

### Решение: Создать индекс колонок

```python
# В классе JSQLParser добавить:

class JSQLParser:
    def __init__(self, context: UMAContext) -> None:
        self.context = context
        self.ctes: dict[str, Select] = {}
        self.table_aliases: dict[str, Any] = {}
        self.select_aliases: dict[str, ColumnElement] = {}
        
        # NEW: Column index for O(1) lookups
        self._column_index: dict[str, list[tuple[str, ColumnElement]]] = {}
    
    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """Register table alias and index its columns."""
        # Register alias
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table
        
        # NEW: Index columns for fast lookup
        if hasattr(table, 'columns'):
            for col in table.columns:
                if col.name not in self._column_index:
                    self._column_index[col.name] = []
                self._column_index[col.name].append((alias_name, col))
    
    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """Resolve column reference with O(1) lookup."""
        # Qualified column (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)
            
            # Check table aliases
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]
            
            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]
            
            raise JSQLSyntaxError(f'Column "{field_spec}" not found')
        
        # NEW: Unqualified column - use index for O(1) lookup
        if candidates := self._column_index.get(field_spec):
            if len(candidates) == 1:
                return candidates[0][1]
            
            # Multiple matches - ambiguous
            tables = [c[0] for c in candidates]
            raise JSQLSyntaxError(
                f'Ambiguous column "{field_spec}" found in tables: {", ".join(tables)}. '
                f'Please qualify with table name.'
            )
        
        # Fallback to FROM clause
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]
        
        raise JSQLSyntaxError(f'Column "{field_spec}" not found')
```

**Результат:**
- Сложность поиска: O(n*m) → O(1)
- Лучшая диагностика (сообщение об ambiguous columns)
- Значительно быстрее при большом количестве JOIN'ов

**Трудозатраты:** 3-4 часа  
**Риски:** Низкие

---

## Проблема 6: Дублирование логики алиасов

### Решение: Создать `AliasManager`

```python
# src/namerec/uma/jsql/alias_manager.py

from typing import Any
from sqlalchemy import Column
from sqlalchemy.sql import ColumnElement

class AliasManager:
    """
    Manages table and column aliases for query parsing.
    Eliminates duplication between converter and parser.
    """
    
    def __init__(self) -> None:
        self._table_aliases: dict[str, Any] = {}
        self._column_index: dict[str, list[tuple[str, ColumnElement]]] = {}
        self._select_aliases: dict[str, ColumnElement] = {}
    
    def register_table(self, alias: str, table: Any, *additional_aliases: str) -> None:
        """Register table with one or more aliases."""
        self._table_aliases[alias] = table
        for additional_alias in additional_aliases:
            self._table_aliases[additional_alias] = table
        
        # Index columns
        if hasattr(table, 'columns'):
            for col in table.columns:
                if col.name not in self._column_index:
                    self._column_index[col.name] = []
                self._column_index[col.name].append((alias, col))
    
    def register_select_alias(self, alias: str, expression: ColumnElement) -> None:
        """Register SELECT clause alias."""
        self._select_aliases[alias] = expression
    
    def resolve_column(self, field_spec: str, from_clause: Any = None) -> ColumnElement:
        """
        Resolve column reference.
        
        Supports:
        - Qualified: table.column
        - Unqualified: column
        - SELECT aliases
        """
        # Check SELECT aliases first
        if field_spec in self._select_aliases:
            return self._select_aliases[field_spec]
        
        # Qualified column
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)
            if table_name in self._table_aliases:
                table = self._table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]
            raise ValueError(f'Column not found: {field_spec}')
        
        # Unqualified - use index
        if candidates := self._column_index.get(field_spec):
            if len(candidates) == 1:
                return candidates[0][1]
            tables = [c[0] for c in candidates]
            raise ValueError(f'Ambiguous column "{field_spec}" in: {", ".join(tables)}')
        
        # Fallback to FROM clause
        if from_clause and hasattr(from_clause, 'columns') and field_spec in from_clause.columns:
            return from_clause.columns[field_spec]
        
        raise ValueError(f'Column not found: {field_spec}')
    
    def get_table(self, alias: str) -> Any:
        """Get table by alias."""
        return self._table_aliases.get(alias)
```

**Использование в parser.py:**

```python
class JSQLParser:
    def __init__(self, context: UMAContext) -> None:
        self.context = context
        self.ctes: dict[str, Select] = {}
        self.alias_manager = AliasManager()  # NEW
        
        # OLD - remove these:
        # self.table_aliases: dict[str, Any] = {}
        # self.select_aliases: dict[str, ColumnElement] = {}
    
    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """Register table alias."""
        self.alias_manager.register_table(alias_name, table, *additional_aliases)
    
    async def _resolve_column(self, field_spec: str, from_clause: Any) -> ColumnElement:
        """Resolve column reference."""
        try:
            return self.alias_manager.resolve_column(field_spec, from_clause)
        except ValueError as e:
            raise JSQLSyntaxError(str(e)) from e
```

**Результат:**
- Единая логика управления алиасами
- Можно переиспользовать в converter.py
- Проще тестировать

**Трудозатраты:** 2-3 часа  
**Риски:** Низкие

---

## Рекомендация 8: Кэширование скомпилированных запросов

### Решение

```python
# src/namerec/uma/jsql/query_cache.py

import hashlib
import json
from typing import Any
from sqlalchemy import Select
from functools import lru_cache

class JSQLQueryCache:
    """Cache for parsed JSQL queries."""
    
    def __init__(self, max_size: int = 1000) -> None:
        self._cache: dict[str, Select] = {}
        self._max_size = max_size
    
    def make_key(self, jsql: dict) -> str:
        """Create cache key from JSQL (excluding params)."""
        # Remove debug and params from cache key
        cache_jsql = {k: v for k, v in jsql.items() if k not in ('debug', 'params')}
        # Stable JSON serialization
        json_str = json.dumps(cache_jsql, sort_keys=True)
        return hashlib.sha256(json_str.encode()).hexdigest()
    
    def get(self, key: str) -> Select | None:
        """Get cached query."""
        return self._cache.get(key)
    
    def set(self, key: str, query: Select) -> None:
        """Cache query with LRU eviction."""
        if len(self._cache) >= self._max_size:
            # Remove oldest item (simple FIFO, could use collections.OrderedDict)
            first_key = next(iter(self._cache))
            del self._cache[first_key]
        self._cache[key] = query


# В JSQLExecutor:

class JSQLExecutor:
    def __init__(self, context: UMAContext, enable_cache: bool = True) -> None:
        self.context = context
        self.parser = JSQLParser(context)
        self._cache = JSQLQueryCache() if enable_cache else None
    
    async def execute(self, jsql: dict, params: dict | None = None) -> QueryResult:
        """Execute JSQL query with caching."""
        try:
            # Try cache first
            cache_key = None
            if self._cache:
                cache_key = self._cache.make_key(jsql)
                if cached_query := self._cache.get(cache_key):
                    # Use cached query
                    query = cached_query
                else:
                    # Parse and cache
                    query = await self.parser.parse(jsql, params)
                    self._cache.set(cache_key, query)
            else:
                # No cache - just parse
                query = await self.parser.parse(jsql, params)
            
            # Generate debug SQL if requested
            debug_sql: str | None = None
            if jsql.get('debug', False):
                debug_sql = self._compile_query_to_sql(query)
            
            # Execute query
            async with self.context.engine.connect() as conn:
                result = await conn.execute(query)
                query_result = JSQLResultBuilder.build_result(result, query, debug_sql)
                return query_result
        
        except (JSQLExecutionError, JSQLSyntaxError):
            raise
        except Exception as e:
            raise JSQLExecutionError(
                message=f'Failed to execute JSQL query: {e!s}',
                query=jsql,
                original_error=e,
            ) from e
```

**Результат:**
- Ускорение повторяющихся запросов
- Особенно эффективно для dashboard-подобных UI
- Опциональное включение (для дебага можно отключить)

**Трудозатраты:** 4-5 часов  
**Риски:** Низкие

---

## Рекомендация 9: Устранение циркулярных импортов

### Текущая проблема

```
registry.py → handlers/base.py (импорт внутри функции)
utils.py → api.py → registry.py (импорт внутри функции)
parser.py → utils.py (импорт внутри функции)
```

### Решение: Реорганизация структуры

**Новая иерархия зависимостей:**

```
Level 1: core/types.py, core/exceptions.py
         ↓
Level 2: core/context.py, core/namespace_config.py
         ↓
Level 3: core/utils.py, metadata.py
         ↓
Level 4: handlers/base.py, handlers/virtual.py
         ↓
Level 5: registry.py
         ↓
Level 6: jsql/* (parser, executor, etc.)
         ↓
Level 7: application.py
         ↓
Level 8: api.py (backward compatibility layer)
```

**Изменения:**

1. **Переместить `get_table` из utils в metadata provider:**

```python
# metadata.py
class DefaultMetadataProvider:
    async def get_table(self, entity_name: EntityName, engine: Engine, context: UMAContext | None = None) -> Table:
        """Get table - now primary method, not helper."""
        ...

# utils.py - remove get_table, replace with direct calls to metadata_provider
```

2. **Переместить registry в application:**

```python
# Вместо get_registry() в utils.py
# Использовать app.registry в application.py
```

3. **Удалить импорты внутри функций** - все импорты в начале файла

**Результат:**
- Чистая иерархия зависимостей
- Нет циркулярных импортов
- Быстрее запуск (импорты выполняются сразу)

**Трудозатраты:** 3-4 часа  
**Риски:** Средние (нужно аккуратно)

---

## Рекомендация 10: Использование паттернов Builder/Factory

### Builder для UMAContext

```python
# src/namerec/uma/builders.py

from dataclasses import dataclass, field
from typing import Any
from sqlalchemy import Engine
from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import MetadataProvider

@dataclass
class UMAContextBuilder:
    """
    Builder for UMAContext.
    Simplifies creation and makes testing easier.
    """
    
    _engine: Engine | None = None
    _metadata_provider: MetadataProvider | None = None
    _namespace: str = 'default'
    _user_context: Any = None
    _cache: Any = None
    _extra: dict[str, Any] = field(default_factory=dict)
    
    def with_engine(self, engine: Engine) -> 'UMAContextBuilder':
        """Set engine."""
        self._engine = engine
        return self
    
    def with_metadata_provider(self, provider: MetadataProvider) -> 'UMAContextBuilder':
        """Set metadata provider."""
        self._metadata_provider = provider
        return self
    
    def with_namespace(self, namespace: str) -> 'UMAContextBuilder':
        """Set namespace."""
        self._namespace = namespace
        return self
    
    def with_user_context(self, user_context: Any) -> 'UMAContextBuilder':
        """Set user context."""
        self._user_context = user_context
        return self
    
    def with_cache(self, cache: Any) -> 'UMAContextBuilder':
        """Set cache."""
        self._cache = cache
        return self
    
    def with_extra(self, **kwargs: Any) -> 'UMAContextBuilder':
        """Add extra fields."""
        self._extra.update(kwargs)
        return self
    
    def build(self) -> UMAContext:
        """Build UMAContext instance."""
        if not self._engine:
            raise ValueError('Engine is required')
        if not self._metadata_provider:
            raise ValueError('MetadataProvider is required')
        if not self._namespace:
            raise ValueError('Namespace is required')
        
        return UMAContext(
            engine=self._engine,
            metadata_provider=self._metadata_provider,
            namespace=self._namespace,
            user_context=self._user_context,
            cache=self._cache,
            extra=self._extra,
        )


# Factory для JSQLExecutor
class JSQLExecutorFactory:
    """Factory for creating JSQLExecutor instances."""
    
    @staticmethod
    def create(
        context: UMAContext,
        enable_cache: bool = True,
        cache_size: int = 1000,
    ) -> 'JSQLExecutor':
        """Create JSQLExecutor with configuration."""
        from namerec.uma.jsql.executor import JSQLExecutor
        return JSQLExecutor(context, enable_cache=enable_cache, cache_size=cache_size)
```

**Использование:**

```python
# Testing
context = (UMAContextBuilder()
    .with_engine(test_engine)
    .with_metadata_provider(test_provider)
    .with_namespace('test')
    .with_user_context({'user_id': 123})
    .build())

# Production
executor = JSQLExecutorFactory.create(
    context=context,
    enable_cache=True,
    cache_size=5000,
)
```

**Результат:**
- Fluent interface для создания объектов
- Легче создавать тестовые фикстуры
- Централизованная конфигурация

**Трудозатраты:** 2-3 часа  
**Риски:** Низкие

---

## Итоговая оценка

| Проблема/Рекомендация | Приоритет | Часы | Риск | Польза |
|----------------------|-----------|------|------|--------|
| 1. DRY в api.py | 🔴 Высокий | 2-3 | Низкий | Высокая |
| 2. Глобальное состояние | 🔴 Высокий | 8-10 | Средний | Очень высокая |
| 3. Разбить converter.py | 🔴 Высокий | 6-8 | Низкий | Высокая |
| 4. Разбить MetadataProvider | 🟠 Средний | 4-5 | Низкий | Средняя |
| 5. Индекс колонок | 🟠 Средний | 3-4 | Низкий | Высокая |
| 6. AliasManager | 🟠 Средний | 2-3 | Низкий | Средняя |
| 8. Кэш запросов | 🟡 Низкий | 4-5 | Низкий | Средняя |
| 9. Циркулярные импорты | 🟡 Низкий | 3-4 | Средний | Средняя |
| 10. Builder/Factory | 🟡 Низкий | 2-3 | Низкий | Низкая |

**ИТОГО:** 37-47 часов (5-6 рабочих дней)

---

## Рекомендуемый порядок выполнения

### Фаза 1: Quick wins (1 день)
1. Проблема 1: DRY в api.py
2. Проблема 6: AliasManager
3. Проблема 5: Индекс колонок

### Фаза 2: Важный рефакторинг (2-3 дня)
4. Проблема 3: Разбить converter.py
5. Проблема 4: Разбить MetadataProvider

### Фаза 3: Архитектурные изменения (2 дня)
6. Проблема 2: Глобальное состояние → UMAApplication
7. Рекомендация 9: Устранить циркулярные импорты

### Фаза 4: Оптимизации (опционально)
8. Рекомендация 8: Кэш запросов
9. Рекомендация 10: Builder/Factory

---

**Конец плана**
