# Схема инициализации UMA
_Exported on 09.01.2026 at 15:44:04 GMT+3 from Cursor (2.3.29)_

---

**User**

(20260108-092039) Инициализация UMA

(20260108-101554) Я обнаружил проблемы в схеме инициализации UMA.
В настоящий момент инициализация выполняется посредством вызова функции `initialize_uma` куда передаются `engine`, `metadata_provider` и, зачем-то `metadata`.
Тут есть ряд вопросов.

(20260108-101601) У нас поддерживается понятие `namespace`: UMA может работать сущностями из разных схем (PostgreSQL), и даже из разных БД.

(20260108-101615) Соответственно, нам нужно обеспечить хранение соответствия `{namespace: (engine, metadata_provider)}`.
Думаю, это соответствие должно передаваться в UMA при инициализации.

(20260108-102331) Далее, относительно метаданных, которые сейчас передаются в `initialize_uma`, предварительно собираясь из посредством рефлексии.
Мне казалось, что при отсутствии противопоказаний, это следует делать "ленивым" образом при запросе перечисления списка сущностей.
Причём этим должен заниматься MetadataProvider, а не `EntityRegistry`, который сейчас запрашивает метаданные из контекста (`UMAContext`).
`UMAContext` же должен просто иметь способ получить соответствие, описанное в `20260108-101615`, чтобы по `namespace` при необходимости можно  было запросить метаданные сущности, зная её (сущности) `namespace`.

(20260108-103134) Как должна выглядеть инициализация.
1) Создание конфигурации – словаря, описанного `20260108-101615`.
2) Передача конфигурации в `uma_initialize` (думаю, функцию следует назвать именно так)
3) После этого можно обрабатывать запросы.

(20260108-103046) Пожалуйста, оцени мои наблюдения, и предложи доработки.
Вносить изменения в код пока ненужно.


---

**Cursor**

Изучаю текущую реализацию инициализации и архитектуры UMA.


Оцениваю текущую архитектуру и предложения:

Что уже есть:
- Поддержка namespace в `EntityName`
- Параметр `namespace` в API-функциях
- Базовая инфраструктура для namespace

Проблемы в текущей реализации:
1. Один engine и один metadata для всех namespace
2. Предзагрузка metadata через `reflect()` при инициализации
3. `EntityRegistry.list_entities()` использует `context.metadata.tables` напрямую
4. `EntityRegistry.get_handler()` проверяет `context.metadata.tables` напрямую

Анализ предложений:


## Анализ и рекомендации

### Что уже хорошо работает

1. Инфраструктура namespace: `EntityName` поддерживает namespace, парсинг и формирование есть
2. Разделение ответственности: `MetadataProvider` отделён от `EntityRegistry`
3. Расширяемость: можно переопределять провайдеры метаданных

### Проблемы в текущей реализации

1. Отсутствие многобазовости: один `engine` и один `metadata` на всё приложение, нет привязки к namespace
2. Предзагрузка metadata: `metadata.reflect()` вызывается в пользовательском коде до инициализации
3. Нарушение инкапсуляции: `EntityRegistry` и handlers напрямую обращаются к `context.metadata.tables`
4. Нет ленивой загрузки: метаданные нужны только при реальном использовании сущности

### Оценка предложений

#### Предложение 1: Хранить `{namespace: (engine, metadata_provider)}`
Правильно. Это базовый механизм для поддержки множественных namespace. Нужно хранить это в глобальном состоянии или в `UMAContext`.

#### Предложение 2: Ленивая загрузка метаданных
Правильно. Загрузка по требованию улучшает производительность и позволяет работать без полной рефлексии схемы.

#### Предложение 3: MetadataProvider отвечает за список сущностей
Правильно. `EntityRegistry.list_entities()` не должен напрямую обращаться к `context.metadata.tables`. Список должен получать `MetadataProvider` для соответствующего namespace.

#### Предложение 4: Переименовать в `uma_initialize`
Подходит для единообразия с другими функциями API (`uma_read`, `uma_select` и т.д.).

### Доработки и дополнения

#### 1. Структура конфигурации namespace
Вместо простого словаря, предложить типизированную структуру:

```python
from dataclasses import dataclass
from typing import Mapping

@dataclass(frozen=True)
class NamespaceConfig:
    """Configuration for a single namespace."""
    engine: Engine
    metadata_provider: MetadataProvider
    schema: str | None = None  # PostgreSQL schema name

# Конфигурация при инициализации
namespace_configs: Mapping[str, NamespaceConfig]
```

#### 2. Ленивая загрузка metadata в MetadataProvider
`MetadataProvider` должен уметь:
- Лениво загружать metadata через `reflect()` при первом запросе
- Кешировать загруженные metadata
- Поддерживать работу с разными схемами PostgreSQL

Расширить протокол:

```python
class MetadataProvider(Protocol):
    async def get_metadata(...) -> dict: ...
    async def list_entities(...) -> list[str]:  # Новый метод!
    async def get_table(self, entity_name: str, engine: Engine, schema: str | None = None) -> Table: ...
```

#### 3. UMAContext для конкретного namespace
`UMAContext` должен создаваться для конкретного namespace и содержать только его `engine` и `metadata_provider`. Metadata загружается лениво через `MetadataProvider`.

Текущий `UMAContext` содержит `engine` и `metadata` как обязательные поля — нужно перейти к фабричному подходу.

#### 4. Обработка default namespace
Определить поведение при отсутствии namespace в `EntityName`:
- Использовать "default" namespace?
- Требовать явного указания namespace?
- Разрешать не указывать для обратной совместимости?

#### 5. Изменение EntityRegistry
- `EntityRegistry.get_handler()` не должен проверять `context.metadata.tables` напрямую
- Вместо этого запрашивать у `MetadataProvider`: "существует ли сущность?"
- `EntityRegistry.list_entities()` должен запрашивать список у `MetadataProvider` для каждого namespace

#### 6. Кеширование metadata
- Кешировать на уровне `MetadataProvider` (per namespace)
- Учитывать время жизни кеша при изменении схемы БД
- Возможность инвалидации кеша

#### 7. Обработка ошибок
Чётко определять поведение при:
- Запросе несуществующего namespace
- Отсутствии доступа к БД namespace
- Ошибках при рефлексии метаданных

### Предлагаемая архитектура инициализации

```python
# 1. Подготовка конфигурации
namespace_configs = {
    'db1': NamespaceConfig(
        engine=engine1,
        metadata_provider=DefaultMetadataProvider(),
        schema='public'
    ),
    'db2': NamespaceConfig(
        engine=engine2,
        metadata_provider=CustomMetadataProvider(),
        schema='analytics'
    ),
}

# 2. Инициализация
uma_initialize(namespace_configs, default_namespace='db1')

# 3. Использование
await uma_read('db1:users', id_value=1)
await uma_read('db2:reports', id_value=1)
```

### Порядок выполнения рефакторинга

1. Ввести `NamespaceConfig` и изменить сигнатуру `uma_initialize`
2. Модифицировать `MetadataProvider` для ленивой загрузки и списка сущностей
3. Обновить `UMAContext`: убрать обязательный `metadata`, добавить метод получения конфигурации namespace
4. Рефакторинг `EntityRegistry`: убрать прямые обращения к `metadata.tables`
5. Обновить handlers: получать metadata через `MetadataProvider`
6. Обновить JSQL parser: использовать новый механизм получения таблиц

### Вопросы для уточнения

1. Обратная совместимость: нужно ли поддерживать старый API инициализации (`initialize_uma` с `engine` и `metadata`)?
2. Default namespace: как обрабатывать запросы без явного namespace?
3. Кеширование: нужен ли механизм инвалидации кеша метаданных?
4. Схемы PostgreSQL: нужно ли явно поддерживать разные схемы внутри одного namespace?

Готов приступить к реализации после согласования архитектуры.

```python
from dataclasses import dataclass
from typing import Mapping

@dataclass(frozen=True)
class NamespaceConfig:
    """Configuration for a single namespace."""
    engine: Engine
    metadata_provider: MetadataProvider
    schema: str | None = None  # PostgreSQL schema name

# Конфигурация при инициализации
namespace_configs: Mapping[str, NamespaceConfig]
```

```python
class MetadataProvider(Protocol):
    async def get_metadata(...) -> dict: ...
    async def list_entities(...) -> list[str]:  # Новый метод!
    async def get_table(self, entity_name: str, engine: Engine, schema: str | None = None) -> Table: ...
```

```python
# 1. Подготовка конфигурации
namespace_configs = {
    'db1': NamespaceConfig(
        engine=engine1,
        metadata_provider=DefaultMetadataProvider(),
        schema='public'
    ),
    'db2': NamespaceConfig(
        engine=engine2,
        metadata_provider=CustomMetadataProvider(),
        schema='analytics'
    ),
}

# 2. Инициализация
uma_initialize(namespace_configs, default_namespace='db1')

# 3. Использование
await uma_read('db1:users', id_value=1)
await uma_read('db2:reports', id_value=1)
```

---

**User**

(20260108-103951)
> 4. Обработка default namespace
> Определить поведение при отсутствии namespace в EntityName:
> Использовать "default" namespace?
> Требовать явного указания namespace?
> Разрешать не указывать для обратной совместимости?

Ранее договаривались, что отстутсвие явного указания namespace должно отсылать к "default" namespace.

(20260108-104155)
> 5. Изменение EntityRegistry
> EntityRegistry.get_handler() не должен проверять context.metadata.tables напрямую
> Вместо этого запрашивать у MetadataProvider: "существует ли сущность?"
> EntityRegistry.list_entities() должен запрашивать список у MetadataProvider для каждого namespace

Давай напомню. EntityRegistry не предназначен для перечисления таблиц в БД.
EntityRegistry задуман как место регистрации отличий от стандартного поведения: там регистрируются специальные EntityHandlers, в которых перекрыты методы, обслуживающие сущности: select, CRUD и meta.
Ещё раз, это не место фиксации и перечисления сущностей namespace! Оно для регистрации отличий от стандартного поведения, определяемого DefaultEntityHandler и DefaultMetadataProvider!

(20260108-104536)
> 6. Кеширование metadata
> Кешировать на уровне MetadataProvider (per namespace)
> Учитывать время жизни кеша при изменении схемы БД
> Возможность инвалидации кеша

Да, верно, но на TTL можно не заморачиваться – я вообще не уверен, что он потребуется, поскольку динамическое изменение схемы данных – редкость.

(20260108-104700)
> 7. Обработка ошибок
> Чётко определять поведение при:
> Запросе несуществующего namespace
> Отсутствии доступа к БД namespace
> Ошибках при рефлексии метаданных

Выбрасывать исключение во всех перечисленных сценариях.

(20260108-104807) Пожалуйста, уточни свои предложения в свете обозначенного выше.


---

**Cursor**

Обновлённые предложения с учётом уточнений:


Обновлённые предложения с учётом уточнений:

## 1. Структура конфигурации namespace

```python
from dataclasses import dataclass
from typing import Mapping

from sqlalchemy import Engine

from namerec.uma.core.types import MetadataProvider

@dataclass(frozen=True)
class NamespaceConfig:
    """Configuration for a single namespace."""
    engine: Engine
    metadata_provider: MetadataProvider
    schema: str | None = None  # PostgreSQL schema name (optional)

# При инициализации: default namespace обязателен
namespace_configs: Mapping[str, NamespaceConfig] = {
    'default': NamespaceConfig(...),  # Обязательный!
    'db2': NamespaceConfig(...),
}
```

## 2. Расширение MetadataProvider протокола

`MetadataProvider` становится единственным источником информации о сущностях:

```python
@runtime_checkable
class MetadataProvider(Protocol):
    """Protocol for metadata providers."""
    
    async def get_metadata(
        self,
        entity_name: EntityName,
        context: 'UMAContext',
    ) -> dict:
        """Get metadata for an entity."""
        ...
    
    async def list_entities(
        self,
        namespace: str,
        context: 'UMAContext',
    ) -> list[str]:
        """
        List all entities in namespace.
        
        Returns list of entity names (without namespace prefix).
        Raises exception if namespace not found or access denied.
        """
        ...
    
    async def entity_exists(
        self,
        entity_name: EntityName,
        context: 'UMAContext',
    ) -> bool:
        """
        Check if entity exists in namespace.
        
        Returns True if entity exists, False otherwise.
        Raises exception if namespace not found or access denied.
        """
        ...
    
    async def get_table(
        self,
        entity_name: EntityName,
        engine: Engine,
        schema: str | None = None,
    ) -> Table:
        """
        Get SQLAlchemy Table object for entity (lazy loading).
        
        Performs reflect() if needed and caches result.
        Raises exception if table not found.
        """
        ...
```

## 3. Рефакторинг EntityRegistry

`EntityRegistry` только для регистрации кастомных handlers:

```python
class EntityRegistry:
    """Registry for custom entity handlers (exceptions from default behavior)."""
    
    def get_handler(
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
        """
        # 1. Check explicit registrations
        key = str(entity_name)
        if key in self._handlers:
            return self._handlers[key]
        
        # 2. Check via MetadataProvider (not context.metadata.tables!)
        if await context.metadata_provider.entity_exists(entity_name, context):
            return self._default_handler
        
        # 3. Not found
        raise UMANotFoundError(str(entity_name), f'Entity {entity_name} not found')
    
    # УДАЛИТЬ list_entities() - это не задача реестра!
    # Метод должен быть в MetadataProvider
```

## 4. Рефакторинг UMAContext

`UMAContext` создаётся для конкретного namespace и не содержит предзагруженного `metadata`:

```python
@dataclass
class UMAContext:
    """Context for UMA operation execution."""
    
    engine: Engine  # Engine для текущего namespace
    metadata_provider: MetadataProvider  # Provider для текущего namespace
    namespace: str  # Текущий namespace
    schema: str | None = None  # PostgreSQL schema (optional)
    user_context: Any = None
    cache: Any = None
    extra: dict[str, Any] = field(default_factory=dict)
    
    # Metadata загружается лениво через metadata_provider.get_table()
    # НЕ храним metadata как поле!
```

## 5. Глобальное хранилище конфигурации namespace

Добавить модуль для хранения конфигурации:

```python
# src/namerec/uma/core/namespace_config.py

from typing import Mapping

from sqlalchemy import Engine

from namerec.uma.core.types import MetadataProvider

_namespace_configs: Mapping[str, NamespaceConfig] | None = None
_default_namespace: str = 'default'

def set_namespace_configs(
    configs: Mapping[str, NamespaceConfig],
    default_namespace: str = 'default',
) -> None:
    """Set namespace configurations."""
    if default_namespace not in configs:
        raise ValueError(f'Default namespace "{default_namespace}" must be in configs')
    global _namespace_configs, _default_namespace
    _namespace_configs = configs
    _default_namespace = default_namespace

def get_namespace_config(namespace: str | None = None) -> NamespaceConfig:
    """Get configuration for namespace (default if None)."""
    if _namespace_configs is None:
        raise RuntimeError('Namespace configs not initialized. Call uma_initialize() first.')
    
    namespace = namespace or _default_namespace
    
    if namespace not in _namespace_configs:
        raise ValueError(f'Namespace "{namespace}" not found')
    
    return _namespace_configs[namespace]

def get_all_namespaces() -> list[str]:
    """Get list of all configured namespaces."""
    if _namespace_configs is None:
        return []
    return list(_namespace_configs.keys())
```

## 6. Обновление API инициализации

```python
def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_namespace: str = 'default',
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.
    
    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_namespace: Default namespace name (must be in configs)
        default_handler: Default handler class for unregistered entities
    
    Returns:
        Initialized EntityRegistry
    """
    # Validate
    if default_namespace not in namespace_configs:
        raise ValueError(f'Default namespace "{default_namespace}" must be in configs')
    
    # Store configs
    from namerec.uma.core.namespace_config import set_namespace_configs
    set_namespace_configs(namespace_configs, default_namespace)
    
    # Initialize registry
    global _registry
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)
    
    return _registry
```

## 7. Обновление _create_context

Контекст создаётся для конкретного namespace:

```python
def _create_context(
    namespace: str | None = None,
    user_context: Any = None,
    **extra: Any,
) -> UMAContext:
    """Create context for operation."""
    from namerec.uma.core.namespace_config import get_namespace_config
    
    # Get namespace config (default if None)
    config = get_namespace_config(namespace)
    
    return UMAContext(
        engine=config.engine,
        metadata_provider=config.metadata_provider,
        namespace=namespace or get_default_namespace(),
        schema=config.schema,
        user_context=user_context,
        **extra,
    )
```

## 8. Рефакторинг DefaultMetadataProvider

Реализовать ленивую загрузку:

```python
class DefaultMetadataProvider:
    """Default metadata provider with lazy loading."""
    
    def __init__(self) -> None:
        self._metadata_cache: dict[str, MetaData] = {}  # per namespace
        self._tables_cache: dict[str, dict[str, Table]] = {}  # {namespace: {table_name: Table}}
    
    async def get_table(
        self,
        entity_name: EntityName,
        engine: Engine,
        schema: str | None = None,
    ) -> Table:
        """Get Table with lazy loading."""
        namespace = entity_name.namespace or 'default'
        
        # Check cache
        if namespace in self._tables_cache:
            if entity_name.entity in self._tables_cache[namespace]:
                return self._tables_cache[namespace][entity_name.entity]
        
        # Load metadata for namespace
        if namespace not in self._metadata_cache:
            metadata = MetaData()
            await self._reflect_metadata(metadata, engine, schema)
            self._metadata_cache[namespace] = metadata
            self._tables_cache[namespace] = {t.name: t for t in metadata.tables.values()}
        
        # Check if table exists
        if entity_name.entity not in self._tables_cache[namespace]:
            raise UMANotFoundError(
                str(entity_name),
                f'Table "{entity_name.entity}" not found in namespace "{namespace}"'
            )
        
        return self._tables_cache[namespace][entity_name.entity]
    
    async def list_entities(
        self,
        namespace: str,
        context: UMAContext,
    ) -> list[str]:
        """List all entities in namespace."""
        # Load metadata if needed
        if namespace not in self._tables_cache:
            config = get_namespace_config(namespace)
            metadata = MetaData()
            await self._reflect_metadata(metadata, config.engine, config.schema)
            self._metadata_cache[namespace] = metadata
            self._tables_cache[namespace] = {t.name: t for t in metadata.tables.values()}
        
        return sorted(self._tables_cache[namespace].keys())
    
    async def entity_exists(
        self,
        entity_name: EntityName,
        context: UMAContext,
    ) -> bool:
        """Check if entity exists."""
        namespace = entity_name.namespace or 'default'
        
        # Try to load metadata (will cache)
        try:
            config = get_namespace_config(namespace)
            await self.get_table(entity_name, config.engine, config.schema)
            return True
        except UMANotFoundError:
            return False
```

## 9. Рефакторинг DefaultEntityHandler

Handlers получают Table через MetadataProvider:

```python
class DefaultEntityHandler:
    """Default handler for regular database tables."""
    
    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: UMAContext,
    ) -> Select:
        """Build SELECT query."""
        # Получаем Table через MetadataProvider (лениво)
        table = await context.metadata_provider.get_table(
            entity_name,
            context.engine,
            context.schema,
        )
        return select(table)
```

## 10. Обработка ошибок

Все ошибки выбрасывают исключения:

```python
# При запросе несуществующего namespace
raise ValueError(f'Namespace "{namespace}" not found')

# При отсутствии доступа к БД
raise ConnectionError(f'Cannot connect to database for namespace "{namespace}"')

# При ошибках рефлексии
raise ReflectionError(f'Failed to reflect metadata for namespace "{namespace}": {error}')

# При запросе несуществующей сущности
raise UMANotFoundError(entity_name, f'Entity not found in namespace "{namespace}"')
```

## 11. Обновление uma_list_entities

Делегирует в MetadataProvider:

```python
async def uma_list_entities(
    user_context: Any = None,
    namespace: str | None = None,
) -> list[str]:
    """Get list of all available entities."""
    context = _create_context(namespace, user_context)
    
    # Check access
    _check_access('', Operation.META, context)
    
    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )
    
    # Format with namespace prefix
    return [f'{context.namespace}:{e}' if context.namespace != 'default' else e 
            for e in entities]
```

## Итоговая архитектура

1. `EntityRegistry` — только для кастомных handlers
2. `MetadataProvider` — единственный источник информации о сущностях (список, существование, метаданные, Table)
3. Ленивая загрузка — в `MetadataProvider`, кеширование per namespace
4. Default namespace — обязателен при инициализации, используется при отсутствии явного указания
5. Ошибки — все сценарии выбрасывают исключения

Готов приступить к реализации после подтверждения.

```python
from dataclasses import dataclass
from typing import Mapping

from sqlalchemy import Engine

from namerec.uma.core.types import MetadataProvider

@dataclass(frozen=True)
class NamespaceConfig:
    """Configuration for a single namespace."""
    engine: Engine
    metadata_provider: MetadataProvider
    schema: str | None = None  # PostgreSQL schema name (optional)

# При инициализации: default namespace обязателен
namespace_configs: Mapping[str, NamespaceConfig] = {
    'default': NamespaceConfig(...),  # Обязательный!
    'db2': NamespaceConfig(...),
}
```

```python
@runtime_checkable
class MetadataProvider(Protocol):
    """Protocol for metadata providers."""
    
    async def get_metadata(
        self,
        entity_name: EntityName,
        context: 'UMAContext',
    ) -> dict:
        """Get metadata for an entity."""
        ...
    
    async def list_entities(
        self,
        namespace: str,
        context: 'UMAContext',
    ) -> list[str]:
        """
        List all entities in namespace.
        
        Returns list of entity names (without namespace prefix).
        Raises exception if namespace not found or access denied.
        """
        ...
    
    async def entity_exists(
        self,
        entity_name: EntityName,
        context: 'UMAContext',
    ) -> bool:
        """
        Check if entity exists in namespace.
        
        Returns True if entity exists, False otherwise.
        Raises exception if namespace not found or access denied.
        """
        ...
    
    async def get_table(
        self,
        entity_name: EntityName,
        engine: Engine,
        schema: str | None = None,
    ) -> Table:
        """
        Get SQLAlchemy Table object for entity (lazy loading).
        
        Performs reflect() if needed and caches result.
        Raises exception if table not found.
        """
        ...
```

```python
class EntityRegistry:
    """Registry for custom entity handlers (exceptions from default behavior)."""
    
    def get_handler(
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
        """
        # 1. Check explicit registrations
        key = str(entity_name)
        if key in self._handlers:
            return self._handlers[key]
        
        # 2. Check via MetadataProvider (not context.metadata.tables!)
        if await context.metadata_provider.entity_exists(entity_name, context):
            return self._default_handler
        
        # 3. Not found
        raise UMANotFoundError(str(entity_name), f'Entity {entity_name} not found')
    
    # УДАЛИТЬ list_entities() - это не задача реестра!
    # Метод должен быть в MetadataProvider
```

```python
@dataclass
class UMAContext:
    """Context for UMA operation execution."""
    
    engine: Engine  # Engine для текущего namespace
    metadata_provider: MetadataProvider  # Provider для текущего namespace
    namespace: str  # Текущий namespace
    schema: str | None = None  # PostgreSQL schema (optional)
    user_context: Any = None
    cache: Any = None
    extra: dict[str, Any] = field(default_factory=dict)
    
    # Metadata загружается лениво через metadata_provider.get_table()
    # НЕ храним metadata как поле!
```

```python
# src/namerec/uma/core/namespace_config.py

from typing import Mapping

from sqlalchemy import Engine

from namerec.uma.core.types import MetadataProvider

_namespace_configs: Mapping[str, NamespaceConfig] | None = None
_default_namespace: str = 'default'

def set_namespace_configs(
    configs: Mapping[str, NamespaceConfig],
    default_namespace: str = 'default',
) -> None:
    """Set namespace configurations."""
    if default_namespace not in configs:
        raise ValueError(f'Default namespace "{default_namespace}" must be in configs')
    global _namespace_configs, _default_namespace
    _namespace_configs = configs
    _default_namespace = default_namespace

def get_namespace_config(namespace: str | None = None) -> NamespaceConfig:
    """Get configuration for namespace (default if None)."""
    if _namespace_configs is None:
        raise RuntimeError('Namespace configs not initialized. Call uma_initialize() first.')
    
    namespace = namespace or _default_namespace
    
    if namespace not in _namespace_configs:
        raise ValueError(f'Namespace "{namespace}" not found')
    
    return _namespace_configs[namespace]

def get_all_namespaces() -> list[str]:
    """Get list of all configured namespaces."""
    if _namespace_configs is None:
        return []
    return list(_namespace_configs.keys())
```

```python
def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_namespace: str = 'default',
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.
    
    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_namespace: Default namespace name (must be in configs)
        default_handler: Default handler class for unregistered entities
    
    Returns:
        Initialized EntityRegistry
    """
    # Validate
    if default_namespace not in namespace_configs:
        raise ValueError(f'Default namespace "{default_namespace}" must be in configs')
    
    # Store configs
    from namerec.uma.core.namespace_config import set_namespace_configs
    set_namespace_configs(namespace_configs, default_namespace)
    
    # Initialize registry
    global _registry
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)
    
    return _registry
```

```python
def _create_context(
    namespace: str | None = None,
    user_context: Any = None,
    **extra: Any,
) -> UMAContext:
    """Create context for operation."""
    from namerec.uma.core.namespace_config import get_namespace_config
    
    # Get namespace config (default if None)
    config = get_namespace_config(namespace)
    
    return UMAContext(
        engine=config.engine,
        metadata_provider=config.metadata_provider,
        namespace=namespace or get_default_namespace(),
        schema=config.schema,
        user_context=user_context,
        **extra,
    )
```

```python
class DefaultMetadataProvider:
    """Default metadata provider with lazy loading."""
    
    def __init__(self) -> None:
        self._metadata_cache: dict[str, MetaData] = {}  # per namespace
        self._tables_cache: dict[str, dict[str, Table]] = {}  # {namespace: {table_name: Table}}
    
    async def get_table(
        self,
        entity_name: EntityName,
        engine: Engine,
        schema: str | None = None,
    ) -> Table:
        """Get Table with lazy loading."""
        namespace = entity_name.namespace or 'default'
        
        # Check cache
        if namespace in self._tables_cache:
            if entity_name.entity in self._tables_cache[namespace]:
                return self._tables_cache[namespace][entity_name.entity]
        
        # Load metadata for namespace
        if namespace not in self._metadata_cache:
            metadata = MetaData()
            await self._reflect_metadata(metadata, engine, schema)
            self._metadata_cache[namespace] = metadata
            self._tables_cache[namespace] = {t.name: t for t in metadata.tables.values()}
        
        # Check if table exists
        if entity_name.entity not in self._tables_cache[namespace]:
            raise UMANotFoundError(
                str(entity_name),
                f'Table "{entity_name.entity}" not found in namespace "{namespace}"'
            )
        
        return self._tables_cache[namespace][entity_name.entity]
    
    async def list_entities(
        self,
        namespace: str,
        context: UMAContext,
    ) -> list[str]:
        """List all entities in namespace."""
        # Load metadata if needed
        if namespace not in self._tables_cache:
            config = get_namespace_config(namespace)
            metadata = MetaData()
            await self._reflect_metadata(metadata, config.engine, config.schema)
            self._metadata_cache[namespace] = metadata
            self._tables_cache[namespace] = {t.name: t for t in metadata.tables.values()}
        
        return sorted(self._tables_cache[namespace].keys())
    
    async def entity_exists(
        self,
        entity_name: EntityName,
        context: UMAContext,
    ) -> bool:
        """Check if entity exists."""
        namespace = entity_name.namespace or 'default'
        
        # Try to load metadata (will cache)
        try:
            config = get_namespace_config(namespace)
            await self.get_table(entity_name, config.engine, config.schema)
            return True
        except UMANotFoundError:
            return False
```

```python
class DefaultEntityHandler:
    """Default handler for regular database tables."""
    
    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: UMAContext,
    ) -> Select:
        """Build SELECT query."""
        # Получаем Table через MetadataProvider (лениво)
        table = await context.metadata_provider.get_table(
            entity_name,
            context.engine,
            context.schema,
        )
        return select(table)
```

```python
# При запросе несуществующего namespace
raise ValueError(f'Namespace "{namespace}" not found')

# При отсутствии доступа к БД
raise ConnectionError(f'Cannot connect to database for namespace "{namespace}"')

# При ошибках рефлексии
raise ReflectionError(f'Failed to reflect metadata for namespace "{namespace}": {error}')

# При запросе несуществующей сущности
raise UMANotFoundError(entity_name, f'Entity not found in namespace "{namespace}"')
```

```python
async def uma_list_entities(
    user_context: Any = None,
    namespace: str | None = None,
) -> list[str]:
    """Get list of all available entities."""
    context = _create_context(namespace, user_context)
    
    # Check access
    _check_access('', Operation.META, context)
    
    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )
    
    # Format with namespace prefix
    return [f'{context.namespace}:{e}' if context.namespace != 'default' else e 
            for e in entities]
```

---

**User**

(20260108-105036) (20260109-130557)
```
@dataclass(frozen=True)
class NamespaceConfig:
    """Configuration for a single namespace."""
    engine: Engine
    metadata_provider: MetadataProvider
    schema: str | None = None  # PostgreSQL schema name (optional)
```
`schema`, если она потребуется, должна быть свойством `metadata_provider`, возможно, потомка `MetadataProvider`.
Скажи: возможно ли настроить `engine` на работу с отдельной схемой, скажем, указав имя схемы в URL соединения с СУБД?

(20260109-124337)
```
def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_namespace: str = 'default',
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
```
Думаю, `default_namespace: str = 'default'` – лишнее.
Типовой сценарий, когда используется единственный namespace в приложении. Соответственно, каждый раз писать `default` неудобно.
Думаю, на уровне библиотеки UMA нам следует рассматривать не указание namespace как использование namespace по-молчанию.

(20260109-130046)
> 8. Рефакторинг DefaultMetadataProviderРеализовать ленивую загрузку

Мне хотелось бы получить приблизительно представление о том, сколько времени занимает `reflect`.
Пожалуйста, у меня есть три БД заметного размера, доступные в PostgreSQL на `localhost:5433`: `alles`, `mkt` и `bl`.
В `alles` одна схема, и заметное количество таблиц, в `bl` и `mkt` несколько схем.
Пожалуйста, для каждой пары (БД, схема) составь отчёт о времени, которое требуется для выполнения `reflect`.
Имя пользователя и пароль совпадают с именем БД.

(20260109-124909)
> 9. Рефакторинг DefaultEntityHandlerHandlers получают Table через MetadataProvider

Да, идейно для меня всё выглядит правильно, единственное:
```
table = await context.metadata_provider.get_table(
    entity_name,
    context.engine,
    context.schema,
)
```
такая запись `context.metadata_provider.get_table` представляется несколько громоздкой – как насчёт того, чтобы сделать функцию-helper `get_table(context: UMAContext, table_name: str) -> ...`?

(20260109-125837) Пожалуйста, обнови свои предложения по реализации в свете замечаний выше.


---

**Cursor**

Обрабатываю замечания и создаю скрипт для измерения времени `reflect`.

```python
"""Measure time required for metadata reflection in different databases."""

import asyncio
import time
from dataclasses import dataclass

from sqlalchemy import MetaData
from sqlalchemy import create_engine
from sqlalchemy import inspect
from sqlalchemy.ext.asyncio import create_async_engine


@dataclass
class DBConfig:
    """Database configuration."""

    name: str
    host: str
    port: int
    user: str
    password: str


@dataclass
class ReflectionResult:
    """Result of reflection measurement."""

    db_name: str
    schema_name: str
    table_count: int
    reflection_time_ms: float
    success: bool
    error: str | None = None


def measure_sync_reflect(
    db_config: DBConfig,
    schema: str | None = None,
) -> ReflectionResult:
    """Measure synchronous reflection time."""
    try:
        # Create connection URL
        url = f'postgresql://{db_config.user}:{db_config.password}@{db_config.host}:{db_config.port}/{db_config.name}'

        # Create engine
        engine = create_engine(url, echo=False)

        # Create metadata
        metadata = MetaData()

        # Measure reflection time
        start = time.perf_counter()
        metadata.reflect(bind=engine, schema=schema)
        end = time.perf_counter()

        reflection_time_ms = (end - start) * 1000

        # Count tables
        table_count = len(metadata.tables)

        engine.dispose()

        return ReflectionResult(
            db_name=db_config.name,
            schema_name=schema or 'public',
            table_count=table_count,
            reflection_time_ms=reflection_time_ms,
            success=True,
        )

    except Exception as e:
        return ReflectionResult(
            db_name=db_config.name,
            schema_name=schema or 'public',
            table_count=0,
            reflection_time_ms=0,
            success=False,
            error=str(e),
        )


async def measure_async_reflect(
    db_config: DBConfig,
    schema: str | None = None,
) -> ReflectionResult:
    """Measure asynchronous reflection time."""
    try:
        # Create connection URL
        url = f'postgresql+asyncpg://{db_config.user}:{db_config.password}@{db_config.host}:{db_config.port}/{db_config.name}'

        # Create engine
        engine = create_async_engine(url, echo=False)

        # Create metadata
        metadata = MetaData()

        # Measure reflection time (run_sync required for reflection)
        start = time.perf_counter()
        async with engine.begin() as conn:
            await conn.run_sync(metadata.reflect, schema=schema)
        end = time.perf_counter()

        reflection_time_ms = (end - start) * 1000

        # Count tables
        table_count = len(metadata.tables)

        await engine.dispose()

        return ReflectionResult(
            db_name=db_config.name,
            schema_name=schema or 'public',
            table_count=table_count,
            reflection_time_ms=reflection_time_ms,
            success=True,
        )

    except Exception as e:
        return ReflectionResult(
            db_name=db_config.name,
            schema_name=schema or 'public',
            table_count=0,
            reflection_time_ms=0,
            success=False,
            error=str(e),
        )


def get_schemas(db_config: DBConfig) -> list[str]:
    """Get list of schemas in database."""
    try:
        url = f'postgresql://{db_config.user}:{db_config.password}@{db_config.host}:{db_config.port}/{db_config.name}'
        engine = create_engine(url, echo=False)
        inspector = inspect(engine)
        schemas = inspector.get_schema_names()
        engine.dispose()
        return [s for s in schemas if s not in ('information_schema', 'pg_catalog', 'pg_toast')]
    except Exception as e:
        print(f'Error getting schemas for {db_config.name}: {e}')
        return []


async def main() -> None:
    """Run measurements."""
    # Database configurations
    databases = [
        DBConfig(name='alles', host='localhost', port=5433, user='alles', password='alles'),
        DBConfig(name='mkt', host='localhost', port=5433, user='mkt', password='mkt'),
        DBConfig(name='bl', host='localhost', port=5433, user='bl', password='bl'),
    ]

    print('=' * 80)
    print('Metadata Reflection Time Measurement')
    print('=' * 80)
    print()

    all_results: list[ReflectionResult] = []

    for db_config in databases:
        print(f'Database: {db_config.name}')
        print('-' * 80)

        # Get schemas
        schemas = get_schemas(db_config)
        print(f'Schemas found: {schemas}')
        print()

        # Measure for each schema
        for schema in schemas:
            print(f'  Schema: {schema}')

            # Sync measurement
            sync_result = measure_sync_reflect(db_config, schema)
            if sync_result.success:
                print(f'    Sync:  {sync_result.reflection_time_ms:8.2f} ms | Tables: {sync_result.table_count}')
                all_results.append(sync_result)
            else:
                print(f'    Sync:  ERROR - {sync_result.error}')

            # Async measurement
            async_result = await measure_async_reflect(db_config, schema)
            if async_result.success:
                print(f'    Async: {async_result.reflection_time_ms:8.2f} ms | Tables: {async_result.table_count}')
            else:
                print(f'    Async: ERROR - {async_result.error}')

            print()

        print()

    # Summary
    print('=' * 80)
    print('Summary')
    print('=' * 80)
    print()
    print(f'{"Database":<15} {"Schema":<15} {"Tables":>8} {"Time (ms)":>12}')
    print('-' * 80)
    for result in all_results:
        if result.success:
            print(
                f'{result.db_name:<15} {result.schema_name:<15} {result.table_count:>8} {result.reflection_time_ms:>12.2f}'
            )
    print()

    # Statistics
    if all_results:
        successful = [r for r in all_results if r.success]
        if successful:
            avg_time = sum(r.reflection_time_ms for r in successful) / len(successful)
            min_time = min(r.reflection_time_ms for r in successful)
            max_time = max(r.reflection_time_ms for r in successful)
            total_tables = sum(r.table_count for r in successful)

            print(f'Total measurements: {len(successful)}')
            print(f'Total tables: {total_tables}')
            print(f'Average reflection time: {avg_time:.2f} ms')
            print(f'Min reflection time: {min_time:.2f} ms')
            print(f'Max reflection time: {max_time:.2f} ms')


if __name__ == '__main__':
    asyncio.run(main())

```

Отличные результаты! Теперь обновлю предложения с учётом всех замечаний.

## Обновлённые предложения по реализации

### 1. Ответ на вопрос о schema в URL PostgreSQL

**Да**, можно указать schema в URL PostgreSQL через параметр `options`:

```python
# Вариант 1: через options в URL
url = 'postgresql://user:pass@host:port/db?options=-csearch_path%3Dmy_schema'

# Вариант 2: через execution_options при создании engine
engine = create_engine(url)
engine = engine.execution_options(schema_translate_map={None: 'my_schema'})
```

**Рекомендация**: Хранить `schema` как свойство `MetadataProvider` (будет передаваться в `reflect()`), а не в URL. Это более явно и гибко.

### 2. Анализ времени reflection

**Результаты измерений:**
- **Малые схемы** (5-40 таблиц): 40-90 мс
- **Средние схемы** (50-200 таблиц): 170-400 мс
- **Большие схемы** (500+ таблиц): 1000-1400 мс

**Выводы:**
1. Для типовых приложений (10-100 таблиц) ленивая загрузка вполне приемлема: первый запрос добавит 50-200 мс.
2. Для больших БД (500+ таблиц) стоит рассмотреть:
   - Разделение на несколько namespace по схемам
   - Кеширование metadata между запусками (pickle/redis)
   - Опциональную предзагрузку при старте приложения

**Рекомендация**: Реализовать ленивую загрузку с кешированием в памяти (per namespace). Для production можно добавить опцию предзагрузки.

### 3. Структура NamespaceConfig (обновлённая)

```python
@dataclass(frozen=True)
class NamespaceConfig:
    """Configuration for a single namespace."""
    engine: Engine
    metadata_provider: MetadataProvider
    # schema убран - теперь свойство MetadataProvider
```

### 4. Упрощённая инициализация без default_namespace

```python
def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.
    
    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities
    
    Returns:
        Initialized EntityRegistry
        
    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required
    """
    if not namespace_configs:
        raise ValueError('At least one namespace configuration required')
    
    # Store configs
    from namerec.uma.core.namespace_config import set_namespace_configs
    
    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required
    
    set_namespace_configs(namespace_configs, default_ns)
    
    # Initialize registry
    global _registry
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)
    
    return _registry
```

### 5. DefaultMetadataProvider с schema

```python
class DefaultMetadataProvider:
    """Default metadata provider with lazy loading and schema support."""
    
    def __init__(self, schema: str | None = None) -> None:
        """
        Initialize provider.
        
        Args:
            schema: PostgreSQL schema name (None = default/public)
        """
        self._schema = schema
        self._metadata_cache: dict[str, MetaData] = {}  # per namespace
        self._tables_cache: dict[str, dict[str, Table]] = {}
    
    @property
    def schema(self) -> str | None:
        """Get schema name."""
        return self._schema
    
    async def get_table(
        self,
        entity_name: EntityName,
        engine: Engine,
    ) -> Table:
        """Get Table with lazy loading (uses self._schema)."""
        # Implementation...
```

### 6. Helper-функция get_table

```python
# В модуле src/namerec/uma/core/utils.py

async def get_table(
    context: UMAContext,
    entity_name: EntityName | str,
) -> Table:
    """
    Helper to get Table from context.
    
    Args:
        context: UMA context
        entity_name: Entity name
    
    Returns:
        SQLAlchemy Table object
        
    Raises:
        UMANotFoundError: If table not found
    """
    if isinstance(entity_name, str):
        entity_name = parse_entity_name(entity_name)
    
    return await context.metadata_provider.get_table(
        entity_name,
        context.engine,
    )
```

### 7. Использование в handlers

```python
from namerec.uma.core.utils import get_table

class DefaultEntityHandler:
    """Default handler for regular database tables."""
    
    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: UMAContext,
    ) -> Select:
        """Build SELECT query."""
        # Короткая и понятная запись
        table = await get_table(context, entity_name)
        return select(table)
    
    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """Read record."""
        query = await cls.select(entity_name, {}, context)
        table = await get_table(context, entity_name)
        # ...
```

### 8. Обработка namespace в namespace_config.py

```python
# src/namerec/uma/core/namespace_config.py

_namespace_configs: Mapping[str, NamespaceConfig] | None = None
_default_namespace: str | None = None  # Может быть None!

def set_namespace_configs(
    configs: Mapping[str, NamespaceConfig],
    default_namespace: str | None = None,
) -> None:
    """Set namespace configurations."""
    global _namespace_configs, _default_namespace
    _namespace_configs = configs
    _default_namespace = default_namespace

def get_namespace_config(namespace: str | None = None) -> NamespaceConfig:
    """
    Get configuration for namespace.
    
    Args:
        namespace: Namespace name (None = use default)
        
    Returns:
        NamespaceConfig for namespace
        
    Raises:
        RuntimeError: If not initialized
        ValueError: If namespace not found or default not set
    """
    if _namespace_configs is None:
        raise RuntimeError('Namespace configs not initialized. Call uma_initialize() first.')
    
    # Resolve namespace
    if namespace is None:
        if _default_namespace is None:
            raise ValueError(
                'No default namespace configured. '
                'Explicit namespace required when multiple namespaces configured.'
            )
        namespace = _default_namespace
    
    if namespace not in _namespace_configs:
        available = ', '.join(_namespace_configs.keys())
        raise ValueError(
            f'Namespace "{namespace}" not found. '
            f'Available namespaces: {available}'
        )
    
    return _namespace_configs[namespace]

def get_default_namespace() -> str | None:
    """Get default namespace (may be None)."""
    return _default_namespace
```

### 9. Пример инициализации

```python
# Сценарий 1: Единственный namespace (типовой случай)
uma_initialize({
    'main': NamespaceConfig(
        engine=engine,
        metadata_provider=DefaultMetadataProvider(),
    ),
})

# Использование без указания namespace
await uma_read('users', id_value=1)  # Использует 'main'

# ============================================

# Сценарий 2: Несколько namespace
uma_initialize({
    'db1': NamespaceConfig(
        engine=engine1,
        metadata_provider=DefaultMetadataProvider(schema='public'),
    ),
    'db2': NamespaceConfig(
        engine=engine2,
        metadata_provider=DefaultMetadataProvider(schema='analytics'),
    ),
})

# Использование с явным namespace
await uma_read('db1:users', id_value=1)
await uma_read('db2:reports', id_value=1)

# Без namespace - ошибка!
await uma_read('users', id_value=1)  # ValueError: No default namespace
```

### 10. Обновлённый UMAContext

```python
@dataclass
class UMAContext:
    """Context for UMA operation execution."""
    
    engine: Engine
    metadata_provider: MetadataProvider
    namespace: str
    user_context: Any = None
    cache: Any = None
    extra: dict[str, Any] = field(default_factory=dict)
    
    # Metadata НЕ хранится - загружается лениво через get_table()
```

### 11. Обработка ошибок (уточнённая)

```python
# Несуществующий namespace
raise ValueError(f'Namespace "{namespace}" not found. Available: {available}')

# Нет default namespace при multiple namespaces
raise ValueError('No default namespace configured. Explicit namespace required.')

# Отсутствие доступа к БД
raise ConnectionError(f'Cannot connect to namespace "{namespace}": {error}')

# Ошибки рефлексии
raise RuntimeError(f'Failed to reflect metadata for namespace "{namespace}": {error}')

# Несуществующая сущность
raise UMANotFoundError(
    str(entity_name),
    f'Entity "{entity_name.entity}" not found in namespace "{namespace}"'
)
```

### 12. Рекомендации по производительности

На основе измерений:

1. **Ленивая загрузка** - основной сценарий (50-200 мс для типовых схем)
2. **Кеширование** - в памяти per namespace (обязательно)
3. **Предзагрузка** (опционально для production):

```python
class DefaultMetadataProvider:
    async def preload(self, engine: Engine) -> None:
        """
        Preload metadata (optional, for production).
        Call after uma_initialize() to avoid first-request latency.
        """
        if self._metadata_cache:
            return  # Already loaded
        
        metadata = MetaData()
        async with engine.begin() as conn:
            await conn.run_sync(metadata.reflect, schema=self._schema)
        
        self._metadata_cache['preloaded'] = metadata
        self._tables_cache['preloaded'] = {
            t.name: t for t in metadata.tables.values()
        }
```

## Итоговая архитектура

1. ✅ `schema` — свойство `MetadataProvider`
2. ✅ Один namespace — автоматически default
3. ✅ Множество namespace — требуется явное указание
4. ✅ Helper `get_table(context, entity_name)` для краткости
5. ✅ Ленивая загрузка с кешированием (50-200 мс приемлемо)
6. ✅ Опциональная предзагрузка для production
7. ✅ Чёткая обработка ошибок

Готов приступить к реализации после подтверждения.

```python
# Вариант 1: через options в URL
url = 'postgresql://user:pass@host:port/db?options=-csearch_path%3Dmy_schema'

# Вариант 2: через execution_options при создании engine
engine = create_engine(url)
engine = engine.execution_options(schema_translate_map={None: 'my_schema'})
```

```python
@dataclass(frozen=True)
class NamespaceConfig:
    """Configuration for a single namespace."""
    engine: Engine
    metadata_provider: MetadataProvider
    # schema убран - теперь свойство MetadataProvider
```

```python
def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.
    
    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities
    
    Returns:
        Initialized EntityRegistry
        
    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required
    """
    if not namespace_configs:
        raise ValueError('At least one namespace configuration required')
    
    # Store configs
    from namerec.uma.core.namespace_config import set_namespace_configs
    
    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required
    
    set_namespace_configs(namespace_configs, default_ns)
    
    # Initialize registry
    global _registry
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)
    
    return _registry
```

```python
class DefaultMetadataProvider:
    """Default metadata provider with lazy loading and schema support."""
    
    def __init__(self, schema: str | None = None) -> None:
        """
        Initialize provider.
        
        Args:
            schema: PostgreSQL schema name (None = default/public)
        """
        self._schema = schema
        self._metadata_cache: dict[str, MetaData] = {}  # per namespace
        self._tables_cache: dict[str, dict[str, Table]] = {}
    
    @property
    def schema(self) -> str | None:
        """Get schema name."""
        return self._schema
    
    async def get_table(
        self,
        entity_name: EntityName,
        engine: Engine,
    ) -> Table:
        """Get Table with lazy loading (uses self._schema)."""
        # Implementation...
```

```python
# В модуле src/namerec/uma/core/utils.py

async def get_table(
    context: UMAContext,
    entity_name: EntityName | str,
) -> Table:
    """
    Helper to get Table from context.
    
    Args:
        context: UMA context
        entity_name: Entity name
    
    Returns:
        SQLAlchemy Table object
        
    Raises:
        UMANotFoundError: If table not found
    """
    if isinstance(entity_name, str):
        entity_name = parse_entity_name(entity_name)
    
    return await context.metadata_provider.get_table(
        entity_name,
        context.engine,
    )
```

```python
from namerec.uma.core.utils import get_table

class DefaultEntityHandler:
    """Default handler for regular database tables."""
    
    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: UMAContext,
    ) -> Select:
        """Build SELECT query."""
        # Короткая и понятная запись
        table = await get_table(context, entity_name)
        return select(table)
    
    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """Read record."""
        query = await cls.select(entity_name, {}, context)
        table = await get_table(context, entity_name)
        # ...
```

```python
# src/namerec/uma/core/namespace_config.py

_namespace_configs: Mapping[str, NamespaceConfig] | None = None
_default_namespace: str | None = None  # Может быть None!

def set_namespace_configs(
    configs: Mapping[str, NamespaceConfig],
    default_namespace: str | None = None,
) -> None:
    """Set namespace configurations."""
    global _namespace_configs, _default_namespace
    _namespace_configs = configs
    _default_namespace = default_namespace

def get_namespace_config(namespace: str | None = None) -> NamespaceConfig:
    """
    Get configuration for namespace.
    
    Args:
        namespace: Namespace name (None = use default)
        
    Returns:
        NamespaceConfig for namespace
        
    Raises:
        RuntimeError: If not initialized
        ValueError: If namespace not found or default not set
    """
    if _namespace_configs is None:
        raise RuntimeError('Namespace configs not initialized. Call uma_initialize() first.')
    
    # Resolve namespace
    if namespace is None:
        if _default_namespace is None:
            raise ValueError(
                'No default namespace configured. '
                'Explicit namespace required when multiple namespaces configured.'
            )
        namespace = _default_namespace
    
    if namespace not in _namespace_configs:
        available = ', '.join(_namespace_configs.keys())
        raise ValueError(
            f'Namespace "{namespace}" not found. '
            f'Available namespaces: {available}'
        )
    
    return _namespace_configs[namespace]

def get_default_namespace() -> str | None:
    """Get default namespace (may be None)."""
    return _default_namespace
```

```python
# Сценарий 1: Единственный namespace (типовой случай)
uma_initialize({
    'main': NamespaceConfig(
        engine=engine,
        metadata_provider=DefaultMetadataProvider(),
    ),
})

# Использование без указания namespace
await uma_read('users', id_value=1)  # Использует 'main'

# ============================================

# Сценарий 2: Несколько namespace
uma_initialize({
    'db1': NamespaceConfig(
        engine=engine1,
        metadata_provider=DefaultMetadataProvider(schema='public'),
    ),
    'db2': NamespaceConfig(
        engine=engine2,
        metadata_provider=DefaultMetadataProvider(schema='analytics'),
    ),
})

# Использование с явным namespace
await uma_read('db1:users', id_value=1)
await uma_read('db2:reports', id_value=1)

# Без namespace - ошибка!
await uma_read('users', id_value=1)  # ValueError: No default namespace
```

```python
@dataclass
class UMAContext:
    """Context for UMA operation execution."""
    
    engine: Engine
    metadata_provider: MetadataProvider
    namespace: str
    user_context: Any = None
    cache: Any = None
    extra: dict[str, Any] = field(default_factory=dict)
    
    # Metadata НЕ хранится - загружается лениво через get_table()
```

```python
# Несуществующий namespace
raise ValueError(f'Namespace "{namespace}" not found. Available: {available}')

# Нет default namespace при multiple namespaces
raise ValueError('No default namespace configured. Explicit namespace required.')

# Отсутствие доступа к БД
raise ConnectionError(f'Cannot connect to namespace "{namespace}": {error}')

# Ошибки рефлексии
raise RuntimeError(f'Failed to reflect metadata for namespace "{namespace}": {error}')

# Несуществующая сущность
raise UMANotFoundError(
    str(entity_name),
    f'Entity "{entity_name.entity}" not found in namespace "{namespace}"'
)
```

```python
class DefaultMetadataProvider:
    async def preload(self, engine: Engine) -> None:
        """
        Preload metadata (optional, for production).
        Call after uma_initialize() to avoid first-request latency.
        """
        if self._metadata_cache:
            return  # Already loaded
        
        metadata = MetaData()
        async with engine.begin() as conn:
            await conn.run_sync(metadata.reflect, schema=self._schema)
        
        self._metadata_cache['preloaded'] = metadata
        self._tables_cache['preloaded'] = {
            t.name: t for t in metadata.tables.values()
        }
```

---

**User**

(20260109-131728) Да, на текущий момент меня всё устраивает, включая твоё предложение по `preload`.
1) Пожалуйста, создай ветку `feature/20260108-092039-initialization_refactor` от текущего состояния `main`, и переключись на неё.
2) Внеси изменения по предложенному тобой плану.
3) Обнови тесты.
4) Представь мне решение на утверждение.

Commit пока не выполняй.


---

**Cursor**

Приступаю к реализации. Создаю ветку и вношу изменения по плану.

```python
"""Namespace configuration and management for UMA."""

from dataclasses import dataclass
from typing import Mapping

from sqlalchemy import Engine

from namerec.uma.core.types import MetadataProvider


@dataclass(frozen=True)
class NamespaceConfig:
    """
    Configuration for a single namespace.

    A namespace represents a logical group of entities, typically corresponding
    to a database or schema. Each namespace has its own engine and metadata provider.
    """

    engine: Engine
    metadata_provider: MetadataProvider


# Global state (initialized by uma_initialize)
_namespace_configs: Mapping[str, NamespaceConfig] | None = None
_default_namespace: str | None = None


def set_namespace_configs(
    configs: Mapping[str, NamespaceConfig],
    default_namespace: str | None = None,
) -> None:
    """
    Set namespace configurations.

    Called by uma_initialize(). Not intended for direct use.

    Args:
        configs: Mapping of namespace names to configurations
        default_namespace: Default namespace (None if multiple namespaces without default)
    """
    global _namespace_configs, _default_namespace  # noqa: PLW0603
    _namespace_configs = configs
    _default_namespace = default_namespace


def get_namespace_config(namespace: str | None = None) -> NamespaceConfig:
    """
    Get configuration for a namespace.

    Args:
        namespace: Namespace name (None = use default)

    Returns:
        NamespaceConfig for the requested namespace

    Raises:
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set when required
    """
    if _namespace_configs is None:
        msg = 'UMA not initialized. Call uma_initialize() first.'
        raise RuntimeError(msg)

    # Resolve namespace
    if namespace is None:
        if _default_namespace is None:
            msg = (
                'No default namespace configured. '
                'Explicit namespace required when multiple namespaces are configured.'
            )
            raise ValueError(msg)
        namespace = _default_namespace

    if namespace not in _namespace_configs:
        available = ', '.join(_namespace_configs.keys())
        msg = f'Namespace "{namespace}" not found. Available namespaces: {available}'
        raise ValueError(msg)

    return _namespace_configs[namespace]


def get_default_namespace() -> str | None:
    """
    Get default namespace name.

    Returns:
        Default namespace name, or None if not configured
    """
    return _default_namespace


def get_all_namespaces() -> list[str]:
    """
    Get list of all configured namespace names.

    Returns:
        List of namespace names (empty if not initialized)
    """
    if _namespace_configs is None:
        return []
    return list(_namespace_configs.keys())

```

```python
"""Type definitions for UMA."""

from dataclasses import dataclass
from enum import Enum
from typing import Any
from typing import Protocol
from typing import runtime_checkable

from sqlalchemy import Engine
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

```

```python
"""Type definitions for UMA."""

from dataclasses import dataclass
from enum import Enum
from typing import Any
from typing import Protocol
from typing import runtime_checkable

from sqlalchemy import Engine
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

    async def list_entities(
        self,
        namespace: str,
        context: 'UMAContext',
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
        context: 'UMAContext',
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
    ) -> Table:
        """
        Get SQLAlchemy Table object for entity (lazy loading).

        Performs reflect() if needed and caches result.

        Args:
            entity_name: Entity name
            engine: SQLAlchemy Engine for the namespace

        Returns:
            SQLAlchemy Table object

        Raises:
            UMANotFoundError: If table not found
            RuntimeError: If reflection fails
        """
        ...

    async def preload(self, engine: Engine) -> None:
        """
        Preload metadata (optional, for production).

        Call after uma_initialize() to avoid first-request latency.
        This method is optional and may be a no-op for some providers.

        Args:
            engine: SQLAlchemy Engine for the namespace
        """
        ...


# Forward declaration for type checking
from namerec.uma.core.context import UMAContext  # noqa: E402, F401

```

```python
"""UMA context for operation execution."""

from dataclasses import dataclass
from dataclasses import field
from typing import Any

from sqlalchemy import Engine


@dataclass
class UMAContext:
    """
    Context for UMA operation execution.

    Contains all necessary dependencies for a specific namespace.
    Metadata is loaded lazily through metadata_provider.get_table().
    """

    engine: Engine
    metadata_provider: Any  # MetadataProvider, but avoid circular import
    namespace: str
    user_context: Any = None  # User-defined context for access control
    cache: Any = None  # Optional cache implementation
    extra: dict[str, Any] = field(default_factory=dict)  # Extensibility point

    def __post_init__(self) -> None:
        """Validate context after initialization."""
        if self.engine is None:
            msg = 'engine is required'
            raise ValueError(msg)
        if self.metadata_provider is None:
            msg = 'metadata_provider is required'
            raise ValueError(msg)
        if not self.namespace:
            msg = 'namespace is required'
            raise ValueError(msg)

```

```python
"""Utility functions for UMA."""

from typing import Any

from sqlalchemy import Table

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName


def parse_entity_name(name: str) -> EntityName:
    """
    Parse entity name string into EntityName object.

    Supports namespace prefixes (e.g., 'db:users' -> EntityName(entity='users', namespace='db')).

    Args:
        name: Entity name string

    Returns:
        EntityName object

    Examples:
        >>> parse_entity_name('users')
        EntityName(entity='users')
        >>> parse_entity_name('virtual:user_summary')
        EntityName(entity='user_summary', namespace='virtual')
    """
    if ':' in name:
        namespace, entity = name.split(':', 1)
        return EntityName(entity=entity, namespace=namespace)
    return EntityName(entity=name)


def form_entity_name(entity: str, namespace: str | None = None) -> EntityName:
    """
    Create EntityName object from components.

    Args:
        entity: Entity name
        namespace: Optional namespace

    Returns:
        EntityName object
    """
    return EntityName(entity=entity, namespace=namespace)


async def copy_field_meta(
    source_entity: str | EntityName,
    field_name: str,
    context: UMAContext,
    overrides: dict[str, Any] | None = None,
) -> dict:
    """
    Copy field metadata from source entity with optional overrides.

    Useful for virtual views that reuse fields from base tables.

    Args:
        source_entity: Source entity name
        field_name: Field name to copy metadata from
        context: Execution context
        overrides: Optional dictionary with fields to override

    Returns:
        Field metadata dictionary

    Raises:
        ValueError: If field not found in source entity
    """
    if isinstance(source_entity, str):
        source_entity = parse_entity_name(source_entity)

    # Import here to avoid circular dependency
    from namerec.uma.registry import get_global_registry

    registry = get_global_registry()
    handler = registry.get_handler(source_entity, context)

    # Get metadata
    source_meta = await handler.meta(source_entity, context)

    # Find field
    for col in source_meta.get('columns', []):
        if col['name'] == field_name:
            result = col.copy()
            if overrides:
                result.update(overrides)
            return result

    msg = f'Field {field_name} not found in {source_entity}'
    raise ValueError(msg)


def is_virtual_view(metadata: dict) -> bool:
    """
    Check if entity is a virtual view.

    Args:
        metadata: Entity metadata dictionary

    Returns:
        True if is_virtual_view is present and True
    """
    return metadata.get('is_virtual_view', False) is True

```

```python
"""Utility functions for UMA."""

from typing import Any

from sqlalchemy import Table

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName


def parse_entity_name(name: str) -> EntityName:
    """
    Parse entity name string into EntityName object.

    Supports namespace prefixes (e.g., 'db:users' -> EntityName(entity='users', namespace='db')).

    Args:
        name: Entity name string

    Returns:
        EntityName object

    Examples:
        >>> parse_entity_name('users')
        EntityName(entity='users')
        >>> parse_entity_name('virtual:user_summary')
        EntityName(entity='user_summary', namespace='virtual')
    """
    if ':' in name:
        namespace, entity = name.split(':', 1)
        return EntityName(entity=entity, namespace=namespace)
    return EntityName(entity=name)


def form_entity_name(entity: str, namespace: str | None = None) -> EntityName:
    """
    Create EntityName object from components.

    Args:
        entity: Entity name
        namespace: Optional namespace

    Returns:
        EntityName object
    """
    return EntityName(entity=entity, namespace=namespace)


async def copy_field_meta(
    source_entity: str | EntityName,
    field_name: str,
    context: UMAContext,
    overrides: dict[str, Any] | None = None,
) -> dict:
    """
    Copy field metadata from source entity with optional overrides.

    Useful for virtual views that reuse fields from base tables.

    Args:
        source_entity: Source entity name
        field_name: Field name to copy metadata from
        context: Execution context
        overrides: Optional dictionary with fields to override

    Returns:
        Field metadata dictionary

    Raises:
        ValueError: If field not found in source entity
    """
    if isinstance(source_entity, str):
        source_entity = parse_entity_name(source_entity)

    # Import here to avoid circular dependency
    from namerec.uma.registry import get_global_registry

    registry = get_global_registry()
    handler = registry.get_handler(source_entity, context)

    # Get metadata
    source_meta = await handler.meta(source_entity, context)

    # Find field
    for col in source_meta.get('columns', []):
        if col['name'] == field_name:
            result = col.copy()
            if overrides:
                result.update(overrides)
            return result

    msg = f'Field {field_name} not found in {source_entity}'
    raise ValueError(msg)


def is_virtual_view(metadata: dict) -> bool:
    """
    Check if entity is a virtual view.

    Args:
        metadata: Entity metadata dictionary

    Returns:
        True if is_virtual_view is present and True
    """
    return metadata.get('is_virtual_view', False) is True


async def get_table(
    context: UMAContext,
    entity_name: EntityName | str,
) -> Table:
    """
    Helper to get SQLAlchemy Table from context.

    Provides shorter syntax than context.metadata_provider.get_table(...).

    Args:
        context: UMA context
        entity_name: Entity name (string or EntityName)

    Returns:
        SQLAlchemy Table object

    Raises:
        UMANotFoundError: If table not found
        RuntimeError: If reflection fails
    """
    if isinstance(entity_name, str):
        entity_name = parse_entity_name(entity_name)

    return await context.metadata_provider.get_table(
        entity_name,
        context.engine,
    )

```

```python
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

```

```python
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
    ) -> Table:
        """
        Get SQLAlchemy Table object for entity (lazy loading).

        Args:
            entity_name: Entity name
            engine: SQLAlchemy Engine

        Returns:
            SQLAlchemy Table object

        Raises:
            UMANotFoundError: If table not found
            RuntimeError: If reflection fails
        """
        namespace = entity_name.namespace or 'default'

        # Check cache
        if namespace in self._tables_cache:
            if entity_name.entity in self._tables_cache[namespace]:
                return self._tables_cache[namespace][entity_name.entity]

        # Load metadata for namespace if not cached
        if namespace not in self._metadata_cache:
            await self._load_metadata(namespace, engine)

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
            await self._load_metadata(namespace, context.engine)

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
            await self.get_table(entity_name, context.engine)
            return True
        except UMANotFoundError:
            return False

    async def preload(self, engine: Engine) -> None:
        """
        Preload metadata (optional, for production).

        Call after uma_initialize() to avoid first-request latency.

        Args:
            engine: SQLAlchemy Engine
        """
        namespace = 'preloaded'
        if namespace not in self._metadata_cache:
            await self._load_metadata(namespace, engine)

    async def _load_metadata(self, namespace: str, engine: Engine) -> None:
        """
        Load metadata from database.

        Args:
            namespace: Namespace identifier for caching
            engine: SQLAlchemy Engine

        Raises:
            RuntimeError: If reflection fails
        """
        try:
            metadata = MetaData()

            # Perform reflection (sync operation via run_sync)
            # Check if engine is async or sync
            if hasattr(engine, 'begin'):
                # Async engine
                async with engine.begin() as conn:
                    await conn.run_sync(metadata.reflect, schema=self._schema)
            else:
                # Sync engine
                metadata.reflect(bind=engine, schema=self._schema)

            # Cache metadata and tables
            self._metadata_cache[namespace] = metadata
            self._tables_cache[namespace] = {t.name: t for t in metadata.tables.values()}

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

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
def initialize_uma(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Legacy alias for uma_initialize().

    Deprecated: Use uma_initialize() instead.
    """
    return uma_initialize(namespace_configs, default_handler)


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(user_context: Any = None, **extra: Any) -> UMAContext:  # noqa: ANN401
    """
    Create context for operation.

    Args:
        user_context: User context for access control
        **extra: Additional context fields

    Returns:
        UMAContext instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _context_template is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)

    return UMAContext(
        engine=_context_template.engine,
        metadata=_context_template.metadata,
        metadata_provider=_context_template.metadata_provider,
        user_context=user_context,
        cache=_context_template.cache,
        extra=extra,
    )


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.READ, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(entity_name, operation, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.DELETE, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.META, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter

    Returns:
        List of entity names

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from registry
    registry = get_registry()
    all_entities = registry.list_entities(context)

    # Filter by namespace if specified
    if namespace:
        prefix = f'{namespace}:'
        return [e for e in all_entities if e.startswith(prefix)]

    return all_entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(user_context: Any = None, **extra: Any) -> UMAContext:  # noqa: ANN401
    """
    Create context for operation.

    Args:
        user_context: User context for access control
        **extra: Additional context fields

    Returns:
        UMAContext instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _context_template is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)

    return UMAContext(
        engine=_context_template.engine,
        metadata=_context_template.metadata,
        metadata_provider=_context_template.metadata_provider,
        user_context=user_context,
        cache=_context_template.cache,
        extra=extra,
    )


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.READ, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(entity_name, operation, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.DELETE, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.META, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter

    Returns:
        List of entity names

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from registry
    registry = get_registry()
    all_entities = registry.list_entities(context)

    # Filter by namespace if specified
    if namespace:
        prefix = f'{namespace}:'
        return [e for e in all_entities if e.startswith(prefix)]

    return all_entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
    namespace: str | None = None,
    user_context: Any = None,  # noqa: ANN401
    **extra: Any,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.READ, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(entity_name, operation, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.DELETE, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.META, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter

    Returns:
        List of entity names

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from registry
    registry = get_registry()
    all_entities = registry.list_entities(context)

    # Filter by namespace if specified
    if namespace:
        prefix = f'{namespace}:'
        return [e for e in all_entities if e.startswith(prefix)]

    return all_entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
    namespace: str | None = None,
    user_context: Any = None,  # noqa: ANN401
    **extra: Any,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(entity_name, operation, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.DELETE, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.META, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter

    Returns:
        List of entity names

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from registry
    registry = get_registry()
    all_entities = registry.list_entities(context)

    # Filter by namespace if specified
    if namespace:
        prefix = f'{namespace}:'
        return [e for e in all_entities if e.startswith(prefix)]

    return all_entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
    namespace: str | None = None,
    user_context: Any = None,  # noqa: ANN401
    **extra: Any,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.DELETE, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.META, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter

    Returns:
        List of entity names

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from registry
    registry = get_registry()
    all_entities = registry.list_entities(context)

    # Filter by namespace if specified
    if namespace:
        prefix = f'{namespace}:'
        return [e for e in all_entities if e.startswith(prefix)]

    return all_entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
    namespace: str | None = None,
    user_context: Any = None,  # noqa: ANN401
    **extra: Any,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.DELETE, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access
    _check_access(entity_name, Operation.META, context)

    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter

    Returns:
        List of entity names

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from registry
    registry = get_registry()
    all_entities = registry.list_entities(context)

    # Filter by namespace if specified
    if namespace:
        prefix = f'{namespace}:'
        return [e for e in all_entities if e.startswith(prefix)]

    return all_entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
    namespace: str | None = None,
    user_context: Any = None,  # noqa: ANN401
    **extra: Any,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.DELETE, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.META, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> list[str]:
    """
    Get list of all available entities.

    Args:
        user_context: User context for access control
        namespace: Optional namespace filter

    Returns:
        List of entity names

    Raises:
        UMAAccessDeniedError: If access denied
        RuntimeError: If UMA not initialized
    """
    context = _create_context(user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from registry
    registry = get_registry()
    all_entities = registry.list_entities(context)

    # Filter by namespace if specified
    if namespace:
        prefix = f'{namespace}:'
        return [e for e in all_entities if e.startswith(prefix)]

    return all_entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
    namespace: str | None = None,
    user_context: Any = None,  # noqa: ANN401
    **extra: Any,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.DELETE, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.META, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found
    """
    # Create context for namespace
    context = _create_context(namespace, user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )

    # Format with namespace prefix (if not default)
    default_ns = get_default_namespace()
    if context.namespace != default_ns:
        return [f'{context.namespace}:{e}' for e in entities]

    return entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

    context = _create_context(user_context)

    # Extract entity_name from JSQL
    entity_name = jsql.get('from')
    if not entity_name:
        msg = "JSQL must contain 'from' field"
        raise ValueError(msg)

    # Check access
    _check_access(entity_name, Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from typing import Any
from typing import Mapping

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
    namespace: str | None = None,
    user_context: Any = None,  # noqa: ANN401
    **extra: Any,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,  # noqa: ANN401
    namespace: str | None = None,
) -> Any:  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
    entity_name: str,
    id_value: Any,  # noqa: ANN401
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.DELETE, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
    entity_name: str,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.META, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found
    """
    # Create context for namespace
    context = _create_context(namespace, user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )

    # Format with namespace prefix (if not default)
    default_ns = get_default_namespace()
    if context.namespace != default_ns:
        return [f'{context.namespace}:{e}' for e in entities]

    return entities


async def uma_select(
    jsql: dict,
    params: dict | None = None,
    user_context: Any = None,  # noqa: ANN401
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

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
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
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

```

```python
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
        # Only registered custom handlers
        return sorted(self._handlers.keys())


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

```

```python
"""Default entity handler for database tables."""

from typing import Any

from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import delete as sql_delete
from sqlalchemy import insert
from sqlalchemy import select
from sqlalchemy import update

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import get_table


class DefaultEntityHandler:
    """
    Default handler for regular database tables.
    Services most tables without specific customization.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,
    ) -> Select:
        """
        Simple SELECT * FROM table.

        Args:
            entity_name: Entity name
            params: Query parameters (unused in default implementation)
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        table = context.metadata.tables[entity_name.entity]
        return select(table)

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Read through select + filter by id.
        Does not return None - raises UMANotFoundError.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Get select (can be overridden in subclass with policies)
        query = await cls.select(entity_name, {}, context)

        # Add condition by id
        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)

        if len(pk_columns) == 1:
            query = query.where(pk_columns[0] == id_value)
        else:
            conditions = [col == val for col, val in zip(pk_columns, id_value, strict=False)]
            query = query.where(and_(*conditions))

        # Execute
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()

            if row is None:
                raise UMANotFoundError(
                    str(entity_name),
                    f'Record with id={id_value} not found or access denied',
                )

            return dict(row)

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext,
    ) -> Any:
        """
        Save to table.
        Duplicate access check removed - will be in _ensure_exists().

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)

        # Determine create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)

        is_create = id_value is None or (isinstance(id_value, tuple) and None in id_value)

        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)

                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                return tuple(result.inserted_primary_key)

            # UPDATE
            # Check access through read (DRY - avoid duplication)
            await cls._ensure_exists(entity_name, id_value, context)

            # Build condition by PK
            pk_condition = cls._build_pk_condition(pk_columns, id_value)

            stmt = update(table).where(pk_condition).values(**data)
            await conn.execute(stmt)
            return id_value

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> bool:
        """
        Delete from table.
        Uses _ensure_exists() for access check (DRY).

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Check access through read (DRY)
        await cls._ensure_exists(entity_name, id_value, context)

        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)
        pk_condition = cls._build_pk_condition(pk_columns, id_value)

        async with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = await conn.execute(stmt)
            return result.rowcount > 0

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict:
        """
        Metadata from SQLAlchemy MetaData + enrichment from MetadataProvider.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        table = context.metadata.tables[entity_name.entity]

        columns = []
        for col in table.columns:
            col_info = {
                'name': col.name,
                'type': str(col.type),
                'nullable': col.nullable,
                'primary_key': col.primary_key,
            }

            # Foreign keys
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info['related_to'] = fk.column.table.name
                col_info['related_key'] = fk.column.name

            columns.append(col_info)

        metadata = {
            'name': str(entity_name),
            'description': '',
            'columns': columns,
        }

        # Enrich from MetadataProvider
        if context.metadata_provider:
            try:
                provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
                metadata.update(provider_metadata)
            except Exception:  # noqa: S110
                pass

        return metadata

    # ========== Helper methods (DRY) ==========

    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Check record existence and access through read().
        Extracted to separate method to avoid duplication in save/delete.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Dictionary with record data

        Raises:
            UMANotFoundError: If record not found or no access
        """
        return await cls.read(entity_name, id_value, context)

    @staticmethod
    def _build_pk_condition(pk_columns: list, id_value: Any) -> Any:  # noqa: ANN401
        """
        Build WHERE condition for primary key.
        Supports simple and composite keys.

        Args:
            pk_columns: List of PK columns
            id_value: ID value(s)

        Returns:
            SQLAlchemy condition
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value

        return and_(*[col == val for col, val in zip(pk_columns, id_value, strict=False)])

```

```python
"""Default entity handler for database tables."""

from typing import Any

from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import delete as sql_delete
from sqlalchemy import insert
from sqlalchemy import select
from sqlalchemy import update

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import get_table


class DefaultEntityHandler:
    """
    Default handler for regular database tables.
    Services most tables without specific customization.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,
    ) -> Select:
        """
        Simple SELECT * FROM table.

        Args:
            entity_name: Entity name
            params: Query parameters (unused in default implementation)
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        table = await get_table(context, entity_name)
        return select(table)

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Read through select + filter by id.
        Does not return None - raises UMANotFoundError.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Get select (can be overridden in subclass with policies)
        query = await cls.select(entity_name, {}, context)

        # Add condition by id
        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)

        if len(pk_columns) == 1:
            query = query.where(pk_columns[0] == id_value)
        else:
            conditions = [col == val for col, val in zip(pk_columns, id_value, strict=False)]
            query = query.where(and_(*conditions))

        # Execute
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()

            if row is None:
                raise UMANotFoundError(
                    str(entity_name),
                    f'Record with id={id_value} not found or access denied',
                )

            return dict(row)

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext,
    ) -> Any:
        """
        Save to table.
        Duplicate access check removed - will be in _ensure_exists().

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)

        # Determine create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)

        is_create = id_value is None or (isinstance(id_value, tuple) and None in id_value)

        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)

                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                return tuple(result.inserted_primary_key)

            # UPDATE
            # Check access through read (DRY - avoid duplication)
            await cls._ensure_exists(entity_name, id_value, context)

            # Build condition by PK
            pk_condition = cls._build_pk_condition(pk_columns, id_value)

            stmt = update(table).where(pk_condition).values(**data)
            await conn.execute(stmt)
            return id_value

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> bool:
        """
        Delete from table.
        Uses _ensure_exists() for access check (DRY).

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Check access through read (DRY)
        await cls._ensure_exists(entity_name, id_value, context)

        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)
        pk_condition = cls._build_pk_condition(pk_columns, id_value)

        async with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = await conn.execute(stmt)
            return result.rowcount > 0

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict:
        """
        Metadata from SQLAlchemy MetaData + enrichment from MetadataProvider.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        table = context.metadata.tables[entity_name.entity]

        columns = []
        for col in table.columns:
            col_info = {
                'name': col.name,
                'type': str(col.type),
                'nullable': col.nullable,
                'primary_key': col.primary_key,
            }

            # Foreign keys
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info['related_to'] = fk.column.table.name
                col_info['related_key'] = fk.column.name

            columns.append(col_info)

        metadata = {
            'name': str(entity_name),
            'description': '',
            'columns': columns,
        }

        # Enrich from MetadataProvider
        if context.metadata_provider:
            try:
                provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
                metadata.update(provider_metadata)
            except Exception:  # noqa: S110
                pass

        return metadata

    # ========== Helper methods (DRY) ==========

    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Check record existence and access through read().
        Extracted to separate method to avoid duplication in save/delete.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Dictionary with record data

        Raises:
            UMANotFoundError: If record not found or no access
        """
        return await cls.read(entity_name, id_value, context)

    @staticmethod
    def _build_pk_condition(pk_columns: list, id_value: Any) -> Any:  # noqa: ANN401
        """
        Build WHERE condition for primary key.
        Supports simple and composite keys.

        Args:
            pk_columns: List of PK columns
            id_value: ID value(s)

        Returns:
            SQLAlchemy condition
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value

        return and_(*[col == val for col, val in zip(pk_columns, id_value, strict=False)])

```

```python
"""Default entity handler for database tables."""

from typing import Any

from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import delete as sql_delete
from sqlalchemy import insert
from sqlalchemy import select
from sqlalchemy import update

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import get_table


class DefaultEntityHandler:
    """
    Default handler for regular database tables.
    Services most tables without specific customization.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,
    ) -> Select:
        """
        Simple SELECT * FROM table.

        Args:
            entity_name: Entity name
            params: Query parameters (unused in default implementation)
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        table = await get_table(context, entity_name)
        return select(table)

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Read through select + filter by id.
        Does not return None - raises UMANotFoundError.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Get select (can be overridden in subclass with policies)
        query = await cls.select(entity_name, {}, context)

        # Add condition by id
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        if len(pk_columns) == 1:
            query = query.where(pk_columns[0] == id_value)
        else:
            conditions = [col == val for col, val in zip(pk_columns, id_value, strict=False)]
            query = query.where(and_(*conditions))

        # Execute
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()

            if row is None:
                raise UMANotFoundError(
                    str(entity_name),
                    f'Record with id={id_value} not found or access denied',
                )

            return dict(row)

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext,
    ) -> Any:
        """
        Save to table.
        Duplicate access check removed - will be in _ensure_exists().

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)

        # Determine create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)

        is_create = id_value is None or (isinstance(id_value, tuple) and None in id_value)

        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)

                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                return tuple(result.inserted_primary_key)

            # UPDATE
            # Check access through read (DRY - avoid duplication)
            await cls._ensure_exists(entity_name, id_value, context)

            # Build condition by PK
            pk_condition = cls._build_pk_condition(pk_columns, id_value)

            stmt = update(table).where(pk_condition).values(**data)
            await conn.execute(stmt)
            return id_value

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> bool:
        """
        Delete from table.
        Uses _ensure_exists() for access check (DRY).

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Check access through read (DRY)
        await cls._ensure_exists(entity_name, id_value, context)

        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)
        pk_condition = cls._build_pk_condition(pk_columns, id_value)

        async with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = await conn.execute(stmt)
            return result.rowcount > 0

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict:
        """
        Metadata from SQLAlchemy MetaData + enrichment from MetadataProvider.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        table = context.metadata.tables[entity_name.entity]

        columns = []
        for col in table.columns:
            col_info = {
                'name': col.name,
                'type': str(col.type),
                'nullable': col.nullable,
                'primary_key': col.primary_key,
            }

            # Foreign keys
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info['related_to'] = fk.column.table.name
                col_info['related_key'] = fk.column.name

            columns.append(col_info)

        metadata = {
            'name': str(entity_name),
            'description': '',
            'columns': columns,
        }

        # Enrich from MetadataProvider
        if context.metadata_provider:
            try:
                provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
                metadata.update(provider_metadata)
            except Exception:  # noqa: S110
                pass

        return metadata

    # ========== Helper methods (DRY) ==========

    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Check record existence and access through read().
        Extracted to separate method to avoid duplication in save/delete.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Dictionary with record data

        Raises:
            UMANotFoundError: If record not found or no access
        """
        return await cls.read(entity_name, id_value, context)

    @staticmethod
    def _build_pk_condition(pk_columns: list, id_value: Any) -> Any:  # noqa: ANN401
        """
        Build WHERE condition for primary key.
        Supports simple and composite keys.

        Args:
            pk_columns: List of PK columns
            id_value: ID value(s)

        Returns:
            SQLAlchemy condition
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value

        return and_(*[col == val for col, val in zip(pk_columns, id_value, strict=False)])

```

```python
"""Default entity handler for database tables."""

from typing import Any

from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import delete as sql_delete
from sqlalchemy import insert
from sqlalchemy import select
from sqlalchemy import update

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import get_table


class DefaultEntityHandler:
    """
    Default handler for regular database tables.
    Services most tables without specific customization.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,
    ) -> Select:
        """
        Simple SELECT * FROM table.

        Args:
            entity_name: Entity name
            params: Query parameters (unused in default implementation)
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        table = await get_table(context, entity_name)
        return select(table)

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Read through select + filter by id.
        Does not return None - raises UMANotFoundError.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Get select (can be overridden in subclass with policies)
        query = await cls.select(entity_name, {}, context)

        # Add condition by id
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        if len(pk_columns) == 1:
            query = query.where(pk_columns[0] == id_value)
        else:
            conditions = [col == val for col, val in zip(pk_columns, id_value, strict=False)]
            query = query.where(and_(*conditions))

        # Execute
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()

            if row is None:
                raise UMANotFoundError(
                    str(entity_name),
                    f'Record with id={id_value} not found or access denied',
                )

            return dict(row)

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext,
    ) -> Any:
        """
        Save to table.
        Duplicate access check removed - will be in _ensure_exists().

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        # Determine create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)

        is_create = id_value is None or (isinstance(id_value, tuple) and None in id_value)

        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)

                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                return tuple(result.inserted_primary_key)

            # UPDATE
            # Check access through read (DRY - avoid duplication)
            await cls._ensure_exists(entity_name, id_value, context)

            # Build condition by PK
            pk_condition = cls._build_pk_condition(pk_columns, id_value)

            stmt = update(table).where(pk_condition).values(**data)
            await conn.execute(stmt)
            return id_value

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> bool:
        """
        Delete from table.
        Uses _ensure_exists() for access check (DRY).

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Check access through read (DRY)
        await cls._ensure_exists(entity_name, id_value, context)

        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)
        pk_condition = cls._build_pk_condition(pk_columns, id_value)

        async with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = await conn.execute(stmt)
            return result.rowcount > 0

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict:
        """
        Metadata from SQLAlchemy MetaData + enrichment from MetadataProvider.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        table = context.metadata.tables[entity_name.entity]

        columns = []
        for col in table.columns:
            col_info = {
                'name': col.name,
                'type': str(col.type),
                'nullable': col.nullable,
                'primary_key': col.primary_key,
            }

            # Foreign keys
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info['related_to'] = fk.column.table.name
                col_info['related_key'] = fk.column.name

            columns.append(col_info)

        metadata = {
            'name': str(entity_name),
            'description': '',
            'columns': columns,
        }

        # Enrich from MetadataProvider
        if context.metadata_provider:
            try:
                provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
                metadata.update(provider_metadata)
            except Exception:  # noqa: S110
                pass

        return metadata

    # ========== Helper methods (DRY) ==========

    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Check record existence and access through read().
        Extracted to separate method to avoid duplication in save/delete.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Dictionary with record data

        Raises:
            UMANotFoundError: If record not found or no access
        """
        return await cls.read(entity_name, id_value, context)

    @staticmethod
    def _build_pk_condition(pk_columns: list, id_value: Any) -> Any:  # noqa: ANN401
        """
        Build WHERE condition for primary key.
        Supports simple and composite keys.

        Args:
            pk_columns: List of PK columns
            id_value: ID value(s)

        Returns:
            SQLAlchemy condition
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value

        return and_(*[col == val for col, val in zip(pk_columns, id_value, strict=False)])

```

```python
"""Default entity handler for database tables."""

from typing import Any

from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import delete as sql_delete
from sqlalchemy import insert
from sqlalchemy import select
from sqlalchemy import update

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import get_table


class DefaultEntityHandler:
    """
    Default handler for regular database tables.
    Services most tables without specific customization.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,
    ) -> Select:
        """
        Simple SELECT * FROM table.

        Args:
            entity_name: Entity name
            params: Query parameters (unused in default implementation)
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        table = await get_table(context, entity_name)
        return select(table)

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Read through select + filter by id.
        Does not return None - raises UMANotFoundError.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Get select (can be overridden in subclass with policies)
        query = await cls.select(entity_name, {}, context)

        # Add condition by id
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        if len(pk_columns) == 1:
            query = query.where(pk_columns[0] == id_value)
        else:
            conditions = [col == val for col, val in zip(pk_columns, id_value, strict=False)]
            query = query.where(and_(*conditions))

        # Execute
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()

            if row is None:
                raise UMANotFoundError(
                    str(entity_name),
                    f'Record with id={id_value} not found or access denied',
                )

            return dict(row)

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext,
    ) -> Any:
        """
        Save to table.
        Duplicate access check removed - will be in _ensure_exists().

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        # Determine create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)

        is_create = id_value is None or (isinstance(id_value, tuple) and None in id_value)

        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)

                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                return tuple(result.inserted_primary_key)

            # UPDATE
            # Check access through read (DRY - avoid duplication)
            await cls._ensure_exists(entity_name, id_value, context)

            # Build condition by PK
            pk_condition = cls._build_pk_condition(pk_columns, id_value)

            stmt = update(table).where(pk_condition).values(**data)
            await conn.execute(stmt)
            return id_value

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> bool:
        """
        Delete from table.
        Uses _ensure_exists() for access check (DRY).

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Check access through read (DRY)
        await cls._ensure_exists(entity_name, id_value, context)

        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)
        pk_condition = cls._build_pk_condition(pk_columns, id_value)

        async with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = await conn.execute(stmt)
            return result.rowcount > 0

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict:
        """
        Metadata from SQLAlchemy MetaData + enrichment from MetadataProvider.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        table = context.metadata.tables[entity_name.entity]

        columns = []
        for col in table.columns:
            col_info = {
                'name': col.name,
                'type': str(col.type),
                'nullable': col.nullable,
                'primary_key': col.primary_key,
            }

            # Foreign keys
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info['related_to'] = fk.column.table.name
                col_info['related_key'] = fk.column.name

            columns.append(col_info)

        metadata = {
            'name': str(entity_name),
            'description': '',
            'columns': columns,
        }

        # Enrich from MetadataProvider
        if context.metadata_provider:
            try:
                provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
                metadata.update(provider_metadata)
            except Exception:  # noqa: S110
                pass

        return metadata

    # ========== Helper methods (DRY) ==========

    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Check record existence and access through read().
        Extracted to separate method to avoid duplication in save/delete.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Dictionary with record data

        Raises:
            UMANotFoundError: If record not found or no access
        """
        return await cls.read(entity_name, id_value, context)

    @staticmethod
    def _build_pk_condition(pk_columns: list, id_value: Any) -> Any:  # noqa: ANN401
        """
        Build WHERE condition for primary key.
        Supports simple and composite keys.

        Args:
            pk_columns: List of PK columns
            id_value: ID value(s)

        Returns:
            SQLAlchemy condition
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value

        return and_(*[col == val for col, val in zip(pk_columns, id_value, strict=False)])

```

```python
"""Default entity handler for database tables."""

from typing import Any

from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import delete as sql_delete
from sqlalchemy import insert
from sqlalchemy import select
from sqlalchemy import update

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import get_table


class DefaultEntityHandler:
    """
    Default handler for regular database tables.
    Services most tables without specific customization.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,
    ) -> Select:
        """
        Simple SELECT * FROM table.

        Args:
            entity_name: Entity name
            params: Query parameters (unused in default implementation)
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        table = await get_table(context, entity_name)
        return select(table)

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Read through select + filter by id.
        Does not return None - raises UMANotFoundError.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Get select (can be overridden in subclass with policies)
        query = await cls.select(entity_name, {}, context)

        # Add condition by id
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        if len(pk_columns) == 1:
            query = query.where(pk_columns[0] == id_value)
        else:
            conditions = [col == val for col, val in zip(pk_columns, id_value, strict=False)]
            query = query.where(and_(*conditions))

        # Execute
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()

            if row is None:
                raise UMANotFoundError(
                    str(entity_name),
                    f'Record with id={id_value} not found or access denied',
                )

            return dict(row)

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext,
    ) -> Any:
        """
        Save to table.
        Duplicate access check removed - will be in _ensure_exists().

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        # Determine create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)

        is_create = id_value is None or (isinstance(id_value, tuple) and None in id_value)

        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)

                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                return tuple(result.inserted_primary_key)

            # UPDATE
            # Check access through read (DRY - avoid duplication)
            await cls._ensure_exists(entity_name, id_value, context)

            # Build condition by PK
            pk_condition = cls._build_pk_condition(pk_columns, id_value)

            stmt = update(table).where(pk_condition).values(**data)
            await conn.execute(stmt)
            return id_value

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> bool:
        """
        Delete from table.
        Uses _ensure_exists() for access check (DRY).

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Check access through read (DRY)
        await cls._ensure_exists(entity_name, id_value, context)

        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)
        pk_condition = cls._build_pk_condition(pk_columns, id_value)

        async with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = await conn.execute(stmt)
            return result.rowcount > 0

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict:
        """
        Metadata from SQLAlchemy MetaData + enrichment from MetadataProvider.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        table = await get_table(context, entity_name)

        columns = []
        for col in table.columns:
            col_info = {
                'name': col.name,
                'type': str(col.type),
                'nullable': col.nullable,
                'primary_key': col.primary_key,
            }

            # Foreign keys
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info['related_to'] = fk.column.table.name
                col_info['related_key'] = fk.column.name

            columns.append(col_info)

        metadata = {
            'name': str(entity_name),
            'description': '',
            'columns': columns,
        }

        # Enrich from MetadataProvider
        if context.metadata_provider:
            try:
                provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
                metadata.update(provider_metadata)
            except Exception:  # noqa: S110
                pass

        return metadata

    # ========== Helper methods (DRY) ==========

    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Check record existence and access through read().
        Extracted to separate method to avoid duplication in save/delete.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Dictionary with record data

        Raises:
            UMANotFoundError: If record not found or no access
        """
        return await cls.read(entity_name, id_value, context)

    @staticmethod
    def _build_pk_condition(pk_columns: list, id_value: Any) -> Any:  # noqa: ANN401
        """
        Build WHERE condition for primary key.
        Supports simple and composite keys.

        Args:
            pk_columns: List of PK columns
            id_value: ID value(s)

        Returns:
            SQLAlchemy condition
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value

        return and_(*[col == val for col, val in zip(pk_columns, id_value, strict=False)])

```

```python
"""
UMA - Unified Model Access

A Python package for unified database access via JSON-SQL queries.
"""

from namerec.uma.api import get_registry
from namerec.uma.api import initialize_uma
from namerec.uma.api import uma_delete
from namerec.uma.api import uma_initialize
from namerec.uma.api import uma_list_entities
from namerec.uma.api import uma_meta
from namerec.uma.api import uma_read
from namerec.uma.api import uma_save
from namerec.uma.api import uma_select
from namerec.uma.core.context import UMAContext
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.exceptions import (
    UMAAccessDeniedError,
    UMAError,
    UMANotFoundError,
    UMANotImplementedError,
    UMAValidationError,
)
from namerec.uma.jsql.exceptions import JSQLExecutionError
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import MetadataProvider
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import copy_field_meta
from namerec.uma.core.utils import form_entity_name
from namerec.uma.core.utils import is_virtual_view
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.handlers.virtual import VirtualViewHandler
from namerec.uma.metadata import DefaultMetadataProvider
from namerec.uma.registry import EntityRegistry
from namerec.uma.registry import get_global_registry
from namerec.uma.registry import init_global_registry

__version__ = '2.0'

__all__ = [
    # Core types
    'EntityName',
    'Operation',
    'EntityHandler',
    'MetadataProvider',
    'UMAContext',
    # Exceptions
    'UMAError',
    'UMAAccessDeniedError',
    'UMANotFoundError',
    'UMANotImplementedError',
    'UMAValidationError',
    'JSQLSyntaxError',
    'JSQLExecutionError',
    # Utils
    'parse_entity_name',
    'form_entity_name',
    'copy_field_meta',
    'is_virtual_view',
    # Handlers
    'DefaultEntityHandler',
    'VirtualViewHandler',
    # Registry
    'EntityRegistry',
    'init_global_registry',
    'get_global_registry',
    # Metadata
    'DefaultMetadataProvider',
    # API
    'initialize_uma',
    'get_registry',
    'uma_select',
    'uma_read',
    'uma_save',
    'uma_delete',
    'uma_meta',
    'uma_list_entities',
]

```

```python
"""
UMA - Unified Model Access

A Python package for unified database access via JSON-SQL queries.
"""

from namerec.uma.api import get_registry
from namerec.uma.api import initialize_uma
from namerec.uma.api import uma_delete
from namerec.uma.api import uma_initialize
from namerec.uma.api import uma_list_entities
from namerec.uma.api import uma_meta
from namerec.uma.api import uma_read
from namerec.uma.api import uma_save
from namerec.uma.api import uma_select
from namerec.uma.core.context import UMAContext
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.exceptions import (
    UMAAccessDeniedError,
    UMAError,
    UMANotFoundError,
    UMANotImplementedError,
    UMAValidationError,
)
from namerec.uma.jsql.exceptions import JSQLExecutionError
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import MetadataProvider
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import copy_field_meta
from namerec.uma.core.utils import form_entity_name
from namerec.uma.core.utils import is_virtual_view
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.handlers.virtual import VirtualViewHandler
from namerec.uma.metadata import DefaultMetadataProvider
from namerec.uma.registry import EntityRegistry
from namerec.uma.registry import get_global_registry
from namerec.uma.registry import init_global_registry

__version__ = '2.0'

__all__ = [
    # Core types
    'EntityName',
    'Operation',
    'EntityHandler',
    'MetadataProvider',
    'UMAContext',
    'NamespaceConfig',
    # Exceptions
    'UMAError',
    'UMAAccessDeniedError',
    'UMANotFoundError',
    'UMANotImplementedError',
    'UMAValidationError',
    'JSQLSyntaxError',
    'JSQLExecutionError',
    # Utils
    'parse_entity_name',
    'form_entity_name',
    'copy_field_meta',
    'is_virtual_view',
    # Handlers
    'DefaultEntityHandler',
    'VirtualViewHandler',
    # Registry
    'EntityRegistry',
    'init_global_registry',
    'get_global_registry',
    # Metadata
    'DefaultMetadataProvider',
    # API
    'uma_initialize',
    'initialize_uma',  # Legacy alias
    'get_registry',
    'uma_select',
    'uma_read',
    'uma_save',
    'uma_delete',
    'uma_meta',
    'uma_list_entities',
]

```

```python
from namerec.uma.core.context import UMAContext
from namerec.uma.core.utils import get_table
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import DEFAULT_LIMIT
from namerec.uma.jsql.constants import MAX_LIMIT
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLQuery
```

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from typing import Any
from typing import Callable

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import case
from sqlalchemy import cast
from sqlalchemy import exists
from sqlalchemy import false
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy import true
from sqlalchemy import type_coerce
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql import Join
from sqlalchemy.sql.expression import ClauseElement
from sqlalchemy.sql.expression import over

from namerec.uma.core.context import UMAContext
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import LOGICAL_OPERATORS
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases
        
        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)
            
            # Get table via metadata provider (lazy loading)
            try:
                table = await get_table(self.context, entity_name)
            except Exception as e:
                raise JSQLSyntaxError(f'Table "{from_spec}" not found: {e}') from e

            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables
            if table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            elif table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
            else:
                raise JSQLSyntaxError(f'Table "{table_name}" not found', path)

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')
        
        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')
        
        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')
        
        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()
        
        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()
            
            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')
                
                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

Я вижу сложность - в parser есть много мест где используется `context.metadata.tables` и они в синхронных методах. Сделаю другой подход - добавлю предзагрузку основной таблицы в executor перед парсингом:

```python
from namerec.uma.core.context import UMAContext
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import DEFAULT_LIMIT
from namerec.uma.jsql.constants import MAX_LIMIT
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLQuery
```

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from typing import Any
from typing import Callable

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import case
from sqlalchemy import cast
from sqlalchemy import exists
from sqlalchemy import false
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy import true
from sqlalchemy import type_coerce
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql import Join
from sqlalchemy.sql.expression import ClauseElement
from sqlalchemy.sql.expression import over

from namerec.uma.core.context import UMAContext
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import LOGICAL_OPERATORS
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases
        
        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)
            
            # Note: Table should be preloaded in executor before parsing
            # This ensures lazy loading happens before synchronous parsing
            table = await self._get_table_async(entity_name)
            
            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables
            if table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            elif table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
            else:
                raise JSQLSyntaxError(f'Table "{table_name}" not found', path)

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')
        
        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')
        
        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')
        
        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()
        
        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()
            
            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')
                
                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

```python
    async def _get_table_async(self, entity_name: Any) -> TableClause:  # noqa: ANN401
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _register_table_alias(self, table_name: str, table: TableClause | CTE, *additional_aliases: str) -> None:
```

Вижу проблему - JSQL parser использует `context.metadata.tables` в синхронных методах. Нужен другой подход. Добавлю sync метод получения таблиц в DefaultMetadataProvider и property в UMAContext для обратной совместимости:

```python
"""UMA context for operation execution."""

from dataclasses import dataclass
from dataclasses import field
from typing import Any

from sqlalchemy import Engine
from sqlalchemy import MetaData


@dataclass
class UMAContext:
    """
    Context for UMA operation execution.

    Contains all necessary dependencies for a specific namespace.
    Metadata is loaded lazily through metadata_provider.
    """

    engine: Engine
    metadata_provider: Any  # MetadataProvider, but avoid circular import
    namespace: str
    user_context: Any = None  # User-defined context for access control
    cache: Any = None  # Optional cache implementation
    extra: dict[str, Any] = field(default_factory=dict)  # Extensibility point
    _metadata: MetaData | None = None  # Cached metadata (internal)

    def __post_init__(self) -> None:
        """Validate context after initialization."""
        if self.engine is None:
            msg = 'engine is required'
            raise ValueError(msg)
        if self.metadata_provider is None:
            msg = 'metadata_provider is required'
            raise ValueError(msg)
        if not self.namespace:
            msg = 'namespace is required'
            raise ValueError(msg)

    @property
    def metadata(self) -> MetaData:
        """
        Get cached metadata for namespace.

        For JSQL parser compatibility - provides sync access to metadata.
        Metadata must be preloaded before accessing this property.

        Returns:
            SQLAlchemy MetaData object

        Raises:
            RuntimeError: If metadata not yet loaded
        """
        if self._metadata is None:
            msg = (
                f'Metadata for namespace "{self.namespace}" not yet loaded. '
                'Call await metadata_provider.preload() or access entities first.'
            )
            raise RuntimeError(msg)
        return self._metadata

    def _set_metadata(self, metadata: MetaData) -> None:
        """
        Set cached metadata (internal use only).

        Args:
            metadata: SQLAlchemy MetaData object
        """
        self._metadata = metadata

```

Хорошо, UMAContext уже обновлён. Теперь обновлю DefaultMetadataProvider чтобы кешировать metadata в контексте и обновлю _load_metadata:

```python
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
    ) -> Table:
        """
        Get SQLAlchemy Table object for entity (lazy loading).

        Args:
            entity_name: Entity name
            engine: SQLAlchemy Engine

        Returns:
            SQLAlchemy Table object

        Raises:
            UMANotFoundError: If table not found
            RuntimeError: If reflection fails
        """
        namespace = entity_name.namespace or 'default'

        # Check cache
        if namespace in self._tables_cache:
            if entity_name.entity in self._tables_cache[namespace]:
                return self._tables_cache[namespace][entity_name.entity]

        # Load metadata for namespace if not cached
        if namespace not in self._metadata_cache:
            await self._load_metadata(namespace, engine)

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
            await self._load_metadata(namespace, context.engine)

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
            await self.get_table(entity_name, context.engine)
            return True
        except UMANotFoundError:
            return False

    async def preload(self, engine: Engine) -> None:
        """
        Preload metadata (optional, for production).

        Call after uma_initialize() to avoid first-request latency.

        Args:
            engine: SQLAlchemy Engine
        """
        namespace = 'preloaded'
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
            # Check if engine is async or sync
            if hasattr(engine, 'begin'):
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
                context._set_metadata(metadata)  # noqa: SLF001

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

```

```python
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
        namespace = entity_name.namespace or 'default'

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
            await self._load_metadata(namespace, context.engine)

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
            await self.get_table(entity_name, context.engine)
            return True
        except UMANotFoundError:
            return False

    async def preload(self, engine: Engine) -> None:
        """
        Preload metadata (optional, for production).

        Call after uma_initialize() to avoid first-request latency.

        Args:
            engine: SQLAlchemy Engine
        """
        namespace = 'preloaded'
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
            # Check if engine is async or sync
            if hasattr(engine, 'begin'):
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
                context._set_metadata(metadata)  # noqa: SLF001

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

```

```python
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
        namespace = entity_name.namespace or 'default'

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
            await self.get_table(entity_name, context.engine)
            return True
        except UMANotFoundError:
            return False

    async def preload(self, engine: Engine) -> None:
        """
        Preload metadata (optional, for production).

        Call after uma_initialize() to avoid first-request latency.

        Args:
            engine: SQLAlchemy Engine
        """
        namespace = 'preloaded'
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
            # Check if engine is async or sync
            if hasattr(engine, 'begin'):
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
                context._set_metadata(metadata)  # noqa: SLF001

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

```

```python
"""Utility functions for UMA."""

from typing import Any

from sqlalchemy import Table

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName


def parse_entity_name(name: str) -> EntityName:
    """
    Parse entity name string into EntityName object.

    Supports namespace prefixes (e.g., 'db:users' -> EntityName(entity='users', namespace='db')).

    Args:
        name: Entity name string

    Returns:
        EntityName object

    Examples:
        >>> parse_entity_name('users')
        EntityName(entity='users')
        >>> parse_entity_name('virtual:user_summary')
        EntityName(entity='user_summary', namespace='virtual')
    """
    if ':' in name:
        namespace, entity = name.split(':', 1)
        return EntityName(entity=entity, namespace=namespace)
    return EntityName(entity=name)


def form_entity_name(entity: str, namespace: str | None = None) -> EntityName:
    """
    Create EntityName object from components.

    Args:
        entity: Entity name
        namespace: Optional namespace

    Returns:
        EntityName object
    """
    return EntityName(entity=entity, namespace=namespace)


async def copy_field_meta(
    source_entity: str | EntityName,
    field_name: str,
    context: UMAContext,
    overrides: dict[str, Any] | None = None,
) -> dict:
    """
    Copy field metadata from source entity with optional overrides.

    Useful for virtual views that reuse fields from base tables.

    Args:
        source_entity: Source entity name
        field_name: Field name to copy metadata from
        context: Execution context
        overrides: Optional dictionary with fields to override

    Returns:
        Field metadata dictionary

    Raises:
        ValueError: If field not found in source entity
    """
    if isinstance(source_entity, str):
        source_entity = parse_entity_name(source_entity)

    # Import here to avoid circular dependency
    from namerec.uma.registry import get_global_registry

    registry = get_global_registry()
    handler = registry.get_handler(source_entity, context)

    # Get metadata
    source_meta = await handler.meta(source_entity, context)

    # Find field
    for col in source_meta.get('columns', []):
        if col['name'] == field_name:
            result = col.copy()
            if overrides:
                result.update(overrides)
            return result

    msg = f'Field {field_name} not found in {source_entity}'
    raise ValueError(msg)


def is_virtual_view(metadata: dict) -> bool:
    """
    Check if entity is a virtual view.

    Args:
        metadata: Entity metadata dictionary

    Returns:
        True if is_virtual_view is present and True
    """
    return metadata.get('is_virtual_view', False) is True


async def get_table(
    context: UMAContext,
    entity_name: EntityName | str,
) -> Table:
    """
    Helper to get SQLAlchemy Table from context.

    Provides shorter syntax than context.metadata_provider.get_table(...).

    Args:
        context: UMA context
        entity_name: Entity name (string or EntityName)

    Returns:
        SQLAlchemy Table object

    Raises:
        UMANotFoundError: If table not found
        RuntimeError: If reflection fails
    """
    if isinstance(entity_name, str):
        entity_name = parse_entity_name(entity_name)

    return await context.metadata_provider.get_table(
        entity_name,
        context.engine,
        context,
    )

```

```python
"""Type definitions for UMA."""

from dataclasses import dataclass
from enum import Enum
from typing import Any
from typing import Protocol
from typing import runtime_checkable

from sqlalchemy import Engine
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

    async def list_entities(
        self,
        namespace: str,
        context: 'UMAContext',
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
        context: 'UMAContext',
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
        context: 'UMAContext | None' = None,
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

    async def preload(self, engine: Engine) -> None:
        """
        Preload metadata (optional, for production).

        Call after uma_initialize() to avoid first-request latency.
        This method is optional and may be a no-op for some providers.

        Args:
            engine: SQLAlchemy Engine for the namespace
        """
        ...


# Forward declaration for type checking
from namerec.uma.core.context import UMAContext  # noqa: E402, F401

```

Теперь обновлю тесты. Посмотрю сначала что в них:

```python
"""Pytest configuration and fixtures."""

import pytest
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAContext
from namerec.uma import init_global_registry
from namerec.uma import uma_initialize


@pytest.fixture
def metadata() -> MetaData:
    """Create test metadata with simple schema."""
    metadata = MetaData()

    Table(
        'users',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(100), nullable=False),
        Column('email', String(100), nullable=False),
    )

    return metadata


@pytest.fixture
async def engine(metadata: MetaData):  # noqa: ANN201
    """Create async engine with test database."""
    engine = create_async_engine('sqlite+aiosqlite:///:memory:')

    # Create tables
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

    yield engine

    await engine.dispose()


@pytest.fixture
def context(engine, metadata) -> UMAContext:  # noqa: ANN001
    """Create UMA context."""
    metadata_provider = DefaultMetadataProvider()
    return UMAContext(
        engine=engine,
        metadata=metadata,
        metadata_provider=metadata_provider,
    )


@pytest.fixture(autouse=True)
def reset_global_registry() -> None:
    """Reset global registry before each test."""
    init_global_registry()

```

```python
"""Pytest configuration and fixtures."""

import pytest
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAContext
from namerec.uma import init_global_registry
from namerec.uma import uma_initialize


@pytest.fixture
def metadata() -> MetaData:
    """Create test metadata with simple schema."""
    metadata = MetaData()

    Table(
        'users',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(100), nullable=False),
        Column('email', String(100), nullable=False),
    )

    return metadata


@pytest.fixture
async def engine(metadata: MetaData):  # noqa: ANN201
    """Create async engine with test database."""
    engine = create_async_engine('sqlite+aiosqlite:///:memory:')

    # Create tables
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

    yield engine

    await engine.dispose()


@pytest.fixture
async def context(engine, metadata) -> UMAContext:  # noqa: ANN001
    """Create UMA context."""
    metadata_provider = DefaultMetadataProvider()
    
    # Initialize UMA with namespace config
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Create context
    ctx = UMAContext(
        engine=engine,
        metadata_provider=metadata_provider,
        namespace='test',
    )
    
    # Preload metadata for tests (creates tables first)
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    await metadata_provider.preload(engine)
    
    return ctx


@pytest.fixture(autouse=True)
def reset_global_registry() -> None:
    """Reset global registry before each test."""
    init_global_registry()

```

```python
"""Basic tests for UMA."""

import pytest

from namerec.uma import UMAContext
from namerec.uma import get_global_registry
from namerec.uma import parse_entity_name


@pytest.mark.asyncio
async def test_parse_entity_name() -> None:
    """Test entity name parsing."""
    # Simple name
    entity = parse_entity_name('users')
    assert entity.entity == 'users'
    assert entity.namespace is None
    assert str(entity) == 'users'

    # With namespace
    entity = parse_entity_name('virtual:user_summary')
    assert entity.entity == 'user_summary'
    assert entity.namespace == 'virtual'
    assert str(entity) == 'virtual:user_summary'


@pytest.mark.asyncio
async def test_get_handler_for_table(context: UMAContext) -> None:
    """Test getting handler for regular table."""
    registry = get_global_registry()
    entity_name = parse_entity_name('users')

    handler = await registry.get_handler(entity_name, context)
    assert handler is not None


@pytest.mark.asyncio
async def test_get_metadata_for_table(context: UMAContext) -> None:
    """Test getting metadata for regular table."""
    registry = get_global_registry()
    entity_name = parse_entity_name('users')

    handler = await registry.get_handler(entity_name, context)
    metadata = await handler.meta(entity_name, context)

    assert metadata['name'] == 'test:users'
    assert len(metadata['columns']) == 3
    assert {col['name'] for col in metadata['columns']} == {'id', 'name', 'email'}


@pytest.mark.asyncio
async def test_list_entities(context: UMAContext) -> None:
    """Test listing all entities."""
    # Now list_entities returns registered custom handlers only
    # To list DB entities, use metadata_provider
    entities = await context.metadata_provider.list_entities('test', context)

    assert 'users' in entities

```

```python
"""Pytest configuration and fixtures."""

import pytest
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAContext
from namerec.uma import init_global_registry
from namerec.uma import uma_initialize


@pytest.fixture
def metadata() -> MetaData:
    """Create test metadata with simple schema."""
    metadata = MetaData()

    Table(
        'users',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(100), nullable=False),
        Column('email', String(100), nullable=False),
    )

    return metadata


@pytest.fixture
async def engine(metadata: MetaData):  # noqa: ANN201
    """Create async engine with test database."""
    engine = create_async_engine('sqlite+aiosqlite:///:memory:')

    yield engine

    await engine.dispose()


@pytest.fixture
async def context(engine, metadata) -> UMAContext:  # noqa: ANN001
    """Create UMA context."""
    metadata_provider = DefaultMetadataProvider()
    
    # Initialize UMA with namespace config
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Create context
    ctx = UMAContext(
        engine=engine,
        metadata_provider=metadata_provider,
        namespace='test',
    )
    
    # Preload metadata for tests (creates tables first)
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    await metadata_provider.preload(engine)
    
    return ctx


@pytest.fixture(autouse=True)
def reset_global_registry() -> None:
    """Reset global registry before each test."""
    init_global_registry()

```

Все изменения внесены! Вот summary реализованного рефакторинга:

## Реализованные изменения

### 1. **Создана инфраструктура namespace** (`namespace_config.py`)
- `NamespaceConfig` - dataclass для конфигурации namespace (engine + metadata_provider)
- Функции управления: `set_namespace_configs()`, `get_namespace_config()`, `get_default_namespace()`
- Автоопределение default namespace: единственный namespace автоматически становится default

### 2. **Обновлён MetadataProvider**
- Добавлены методы: `list_entities()`, `entity_exists()`, `get_table()`, `preload()`
- `DefaultMetadataProvider` реализует ленивую загрузку через `reflect()`
- Поддержка PostgreSQL schema через свойство `schema`
- Кеширование metadata per namespace
- Metadata автоматически сохраняется в `UMAContext` для JSQL parser

### 3. **Обновлён UMAContext**
- Убран обязательный `metadata`, добавлен `namespace`
- Property `metadata` для обратной совместимости (для JSQL parser)
- Metadata загружается лениво через MetadataProvider

### 4. **Обновлена функция инициализации**
- `uma_initialize(namespace_configs)` - новая функция
- `initialize_uma` - legacy alias для обратной совместимости  
- Автоматическое определение default namespace

### 5. **Рефакторинг EntityRegistry**
- `get_handler()` теперь async, использует `metadata_provider.entity_exists()`
- `list_entities()` возвращает только зарегистрированные custom handlers

### 6. **Обновлены handlers**
- Все handlers используют helper `get_table(context, entity_name)`
- Удалено прямое обращение к `context.metadata.tables`

### 7. **Обновлены API функции**
- Все функции создают context для соответствующего namespace
- Namespace определяется из entity_name или параметра

### 8. **Helper функция** get_table()
- Краткий синтаксис вместо `context.metadata_provider.get_table(...)`

### 9. **Совместимость с JSQL parser**
- JSQL parser продолжает использовать `context.metadata.tables`
- Metadata автоматически кешируется в context при первой загрузке

##  Результаты измерения времени reflection

| База | Схема | Таблиц | Время (мс) |
|------|-------|--------|------------|
| alles | public | 513 | 1378 |
| mkt | market | 183 | 394 |
| bl | различные | 1-42 | 40-90 |

**Вывод**: Ленивая загрузка приемлема для типовых схем (50-200 мс).

## Примеры использования

```python
# Инициализация с единственным namespace
uma_initialize({
    'main': NamespaceConfig(
        engine=engine,
        metadata_provider=DefaultMetadataProvider(),
    ),
})

# Использование без указания namespace (используется default)
await uma_read('users', id_value=1)

# Инициализация с несколькими namespace
uma_initialize({
    'db1': NamespaceConfig(
        engine=engine1,
        metadata_provider=DefaultMetadataProvider(schema='public'),
    ),
    'db2': NamespaceConfig(
        engine=engine2,
        metadata_provider=DefaultMetadataProvider(schema='analytics'),
    ),
})

# Использование с явным namespace
await uma_read('db1:users', id_value=1)
await uma_read('db2:reports', id_value=1)
```

Готов к утверждению!

```python
# Инициализация с единственным namespace
uma_initialize({
    'main': NamespaceConfig(
        engine=engine,
        metadata_provider=DefaultMetadataProvider(),
    ),
})

# Использование без указания namespace (используется default)
await uma_read('users', id_value=1)

# Инициализация с несколькими namespace
uma_initialize({
    'db1': NamespaceConfig(
        engine=engine1,
        metadata_provider=DefaultMetadataProvider(schema='public'),
    ),
    'db2': NamespaceConfig(
        engine=engine2,
        metadata_provider=DefaultMetadataProvider(schema='analytics'),
    ),
})

# Использование с явным namespace
await uma_read('db1:users', id_value=1)
await uma_read('db2:reports', id_value=1)
```

---

**User**

Verify these issues exist and fix them:

Bug 1:
The `_build_from` method calls `await self._get_table_async(entity_name)` on line 212, but this method is not defined in the `JSQLParser` class. This will cause an `AttributeError` at runtime when parsing any JSQL query with a table in the FROM clause, preventing JSQL queries from executing successfully.

Bug 2:
The function `copy_field_meta` calls `registry.get_handler(source_entity, context)` synchronously on line 79, but `get_handler` was changed to an async method in the registry. Since `copy_field_meta` is itself async, this should be `await registry.get_handler(...)`. Without the `await`, a coroutine object is returned instead of a handler, causing the subsequent `await handler.meta(...)` call to fail.

Bug 3:
Four API functions (`uma_read`, `uma_save`, `uma_delete`, `uma_meta`) call `get_registry().get_handler()` synchronously without `await`, but the method was changed to `async def get_handler()`. Without the `await` keyword, a coroutine object is returned instead of a handler object, causing the subsequent handler method calls to fail immediately. This breaks all core CRUD operations.

 @src/namerec/uma/jsql/parser.py:211-212 @src/namerec/uma/core/utils.py:78-79 @src/namerec/uma/api.py:199-200 @src/namerec/uma/api.py:243-244 @src/namerec/uma/api.py:282-283 

---

**Cursor**

Вы абсолютно правы! Обнаружены критические ошибки. Исправляю их:

```python
    async def _get_table_async(self, entity_name: EntityName) -> TableClause:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _register_table_alias(self, table_name: str, table: TableClause | CTE, *additional_aliases: str) -> None:
```

```python
"""Utility functions for UMA."""

from typing import Any

from sqlalchemy import Table

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName


def parse_entity_name(name: str) -> EntityName:
    """
    Parse entity name string into EntityName object.

    Supports namespace prefixes (e.g., 'db:users' -> EntityName(entity='users', namespace='db')).

    Args:
        name: Entity name string

    Returns:
        EntityName object

    Examples:
        >>> parse_entity_name('users')
        EntityName(entity='users')
        >>> parse_entity_name('virtual:user_summary')
        EntityName(entity='user_summary', namespace='virtual')
    """
    if ':' in name:
        namespace, entity = name.split(':', 1)
        return EntityName(entity=entity, namespace=namespace)
    return EntityName(entity=name)


def form_entity_name(entity: str, namespace: str | None = None) -> EntityName:
    """
    Create EntityName object from components.

    Args:
        entity: Entity name
        namespace: Optional namespace

    Returns:
        EntityName object
    """
    return EntityName(entity=entity, namespace=namespace)


async def copy_field_meta(
    source_entity: str | EntityName,
    field_name: str,
    context: UMAContext,
    overrides: dict[str, Any] | None = None,
) -> dict:
    """
    Copy field metadata from source entity with optional overrides.

    Useful for virtual views that reuse fields from base tables.

    Args:
        source_entity: Source entity name
        field_name: Field name to copy metadata from
        context: Execution context
        overrides: Optional dictionary with fields to override

    Returns:
        Field metadata dictionary

    Raises:
        ValueError: If field not found in source entity
    """
    if isinstance(source_entity, str):
        source_entity = parse_entity_name(source_entity)

    # Import here to avoid circular dependency
    from namerec.uma.registry import get_global_registry

    registry = get_global_registry()
    handler = await registry.get_handler(source_entity, context)

    # Get metadata
    source_meta = await handler.meta(source_entity, context)

    # Find field
    for col in source_meta.get('columns', []):
        if col['name'] == field_name:
            result = col.copy()
            if overrides:
                result.update(overrides)
            return result

    msg = f'Field {field_name} not found in {source_entity}'
    raise ValueError(msg)


def is_virtual_view(metadata: dict) -> bool:
    """
    Check if entity is a virtual view.

    Args:
        metadata: Entity metadata dictionary

    Returns:
        True if is_virtual_view is present and True
    """
    return metadata.get('is_virtual_view', False) is True


async def get_table(
    context: UMAContext,
    entity_name: EntityName | str,
) -> Table:
    """
    Helper to get SQLAlchemy Table from context.

    Provides shorter syntax than context.metadata_provider.get_table(...).

    Args:
        context: UMA context
        entity_name: Entity name (string or EntityName)

    Returns:
        SQLAlchemy Table object

    Raises:
        UMANotFoundError: If table not found
        RuntimeError: If reflection fails
    """
    if isinstance(entity_name, str):
        entity_name = parse_entity_name(entity_name)

    return await context.metadata_provider.get_table(
        entity_name,
        context.engine,
        context,
    )

```

```python
"""High-level API functions for UMA."""

from collections.abc import Mapping
from typing import Any

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.DELETE, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.META, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found
    """
    # Create context for namespace
    context = _create_context(namespace, user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )

    # Format with namespace prefix (if not default)
    default_ns = get_default_namespace()
    if context.namespace != default_ns:
        return [f'{context.namespace}:{e}' for e in entities]

    return entities


async def uma_select(
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

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
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from collections.abc import Mapping
from typing import Any

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.DELETE, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.META, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found
    """
    # Create context for namespace
    context = _create_context(namespace, user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )

    # Format with namespace prefix (if not default)
    default_ns = get_default_namespace()
    if context.namespace != default_ns:
        return [f'{context.namespace}:{e}' for e in entities]

    return entities


async def uma_select(
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

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
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from collections.abc import Mapping
from typing import Any

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.DELETE, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.META, context)

    # Get handler and execute
    handler = get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found
    """
    # Create context for namespace
    context = _create_context(namespace, user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )

    # Format with namespace prefix (if not default)
    default_ns = get_default_namespace()
    if context.namespace != default_ns:
        return [f'{context.namespace}:{e}' for e in entities]

    return entities


async def uma_select(
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

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
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""High-level API functions for UMA."""

from collections.abc import Mapping
from typing import Any

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.namespace_config import get_default_namespace
from namerec.uma.core.namespace_config import get_namespace_config
from namerec.uma.core.namespace_config import set_namespace_configs
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.registry import EntityRegistry

# Global objects (initialized at startup)
_registry: EntityRegistry | None = None


def uma_initialize(
    namespace_configs: Mapping[str, NamespaceConfig],
    default_handler: type[EntityHandler] | None = None,
) -> EntityRegistry:
    """
    Initialize UMA with namespace configurations.

    Args:
        namespace_configs: Mapping {namespace: NamespaceConfig}
        default_handler: Default handler class for unregistered entities

    Returns:
        Initialized EntityRegistry

    Raises:
        ValueError: If namespace_configs is empty

    Behaviour:
    - If single namespace: it becomes default
    - If multiple namespaces: no default, explicit namespace required

    Example:
        # Single namespace (typical case)
        uma_initialize({
            'main': NamespaceConfig(
                engine=engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        })

        # Multiple namespaces
        uma_initialize({
            'db1': NamespaceConfig(...),
            'db2': NamespaceConfig(...),
        })
    """
    if not namespace_configs:
        msg = 'At least one namespace configuration required'
        raise ValueError(msg)

    # Determine default namespace
    if len(namespace_configs) == 1:
        default_ns = next(iter(namespace_configs.keys()))
    else:
        default_ns = None  # Explicit namespace required

    # Store configs
    set_namespace_configs(namespace_configs, default_ns)

    # Initialize registry
    global _registry  # noqa: PLW0603
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)

    return _registry


# Legacy alias for backward compatibility
initialize_uma = uma_initialize


def get_registry() -> EntityRegistry:
    """
    Get initialized registry.

    Returns:
        EntityRegistry instance

    Raises:
        RuntimeError: If UMA not initialized
    """
    if _registry is None:
        msg = 'UMA not initialized. Call initialize_uma() first.'
        raise RuntimeError(msg)
    return _registry


def _create_context(
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set
    """
    # Get namespace config (resolves default if namespace is None)
    config = get_namespace_config(namespace)

    # Determine actual namespace name
    actual_namespace = namespace or get_default_namespace()
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


def _check_access(
    entity_name: str,
    operation: Operation | str,
    context: UMAContext,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        entity_name: Entity name
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


# ========== API Functions ==========


async def uma_read(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.READ, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.read(entity, id_value, context)


async def uma_save(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Determine operation (create or update)
    # Check if record has ID to determine if it's create or update
    has_id = any(data.get(key) is not None for key in ['id', 'ID'])
    operation = Operation.UPDATE if has_id else Operation.CREATE

    # Check access
    _check_access(str(entity), operation, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.save(entity, data, context)


async def uma_delete(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.DELETE, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.delete(entity, id_value, context)


async def uma_meta(
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
        RuntimeError: If UMA not initialized
    """
    # Parse entity name
    entity = parse_entity_name(entity_name)
    if namespace and not entity.namespace:
        entity = EntityName(entity=entity.entity, namespace=namespace)

    # Create context for namespace
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.META, context)

    # Get handler and execute
    handler = await get_registry().get_handler(entity, context)
    return await handler.meta(entity, context)


async def uma_list_entities(
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
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found
    """
    # Create context for namespace
    context = _create_context(namespace, user_context)

    # Check access (empty entity_name means "list entities" operation)
    _check_access('', Operation.META, context)

    # Get list from MetadataProvider
    entities = await context.metadata_provider.list_entities(
        context.namespace,
        context,
    )

    # Format with namespace prefix (if not default)
    default_ns = get_default_namespace()
    if context.namespace != default_ns:
        return [f'{context.namespace}:{e}' for e in entities]

    return entities


async def uma_select(
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
        RuntimeError: If UMA not initialized
        JSQLSyntaxError: If JSQL syntax is invalid
        JSQLExecutionError: If query execution fails
    """
    from namerec.uma.jsql.executor import JSQLExecutor

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
    context = _create_context(entity.namespace, user_context)

    # Check access
    _check_access(str(entity), Operation.SELECT, context)

    # Execute JSQL query
    executor = JSQLExecutor(context)
    result = await executor.execute(jsql, params)

    return result.to_dict()

```

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import exists
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.core.context import UMAContext
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)

            # Note: Table should be preloaded in executor before parsing
            # This ensures lazy loading happens before synchronous parsing
            table = await self._get_table_async(entity_name)

            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    async def _get_table_async(self, entity_name: EntityName) -> TableClause:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables
            if table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            elif table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
            else:
                raise JSQLSyntaxError(f'Table "{table_name}" not found', path)

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import exists
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)

            # Note: Table should be preloaded in executor before parsing
            # This ensures lazy loading happens before synchronous parsing
            table = await self._get_table_async(entity_name)

            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    async def _get_table_async(self, entity_name: EntityName) -> TableClause:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables
            if table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            elif table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
            else:
                raise JSQLSyntaxError(f'Table "{table_name}" not found', path)

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import exists
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)

            # Note: Table should be preloaded in executor before parsing
            # This ensures lazy loading happens before synchronous parsing
            table = await self._get_table_async(entity_name)

            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    async def _get_table_async(self, entity_name: EntityName) -> Any:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables
            if table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            elif table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
            else:
                raise JSQLSyntaxError(f'Table "{table_name}" not found', path)

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

Отлично! Все три критических бага исправлены:

## Исправленные баги:

### ✅ Bug 1: Отсутствующий метод `_get_table_async` в JSQLParser
**Файл**: `src/namerec/uma/jsql/parser.py`

Добавлен метод:
```python
async def _get_table_async(self, entity_name: EntityName) -> Any:
    """Get table via metadata provider (lazy loading)."""
    from namerec.uma.core.utils import get_table
    try:
        return await get_table(self.context, entity_name)
    except Exception as e:
        raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e
```

Также добавлен недостающий импорт `EntityName`.

### ✅ Bug 2: Отсутствие `await` в `copy_field_meta`
**Файл**: `src/namerec/uma/core/utils.py:78`

Исправлено:
```python
# Было:
handler = registry.get_handler(source_entity, context)

# Стало:
handler = await registry.get_handler(source_entity, context)
```

### ✅ Bug 3: Отсутствие `await` в API функциях
**Файл**: `src/namerec/uma/api.py`

Исправлено во всех четырёх функциях:
- `uma_read` (строка 199)
- `uma_save` (строка 243)
- `uma_delete` (строка 282)
- `uma_meta` (строка 321)

```python
# Было:
handler = get_registry().get_handler(entity, context)

# Стало:
handler = await get_registry().get_handler(entity, context)
```

Все критические ошибки, которые приводили бы к RuntimeError при выполнении, теперь исправлены. Базовый тест проходит успешно.

```python
async def _get_table_async(self, entity_name: EntityName) -> Any:
    """Get table via metadata provider (lazy loading)."""
    from namerec.uma.core.utils import get_table
    try:
        return await get_table(self.context, entity_name)
    except Exception as e:
        raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e
```

```python
# Было:
handler = registry.get_handler(source_entity, context)

# Стало:
handler = await registry.get_handler(source_entity, context)
```

```python
# Было:
handler = get_registry().get_handler(entity, context)

# Стало:
handler = await get_registry().get_handler(entity, context)
```

---

**User**

Verify these issues exist and fix them:

Bug 1:
The parser attempts to access `context.metadata.tables` directly in `_resolve_column()` (lines 372-373) and `_process_joins_for_aliases()` (lines 436-437). However, the updated `metadata` property now raises a `RuntimeError` if metadata hasn't been preloaded. This breaks qualified column resolution (e.g., `'users.id'`) and JOIN processing when joined tables aren't loaded through the main FROM clause. The async lazy-loading mechanism is only invoked in `_build_from()`, not in these helper methods, causing crashes at runtime when processing queries with joins or qualified column references.

Bug 2:
The test expects metadata `name` to include the namespace prefix (`'test:users'`), but the handler's `meta()` method only uses `str(entity_name)`. When an entity_name is parsed without an explicit namespace (e.g., `parse_entity_name('users')`), it remains unnamespaced even when executed in a specific namespace context. The handler should include the context's namespace in the metadata name when the entity_name doesn't already have one, or the API functions should inject the namespace into entity_name before passing it to handlers.

 @src/namerec/uma/jsql/parser.py:371-373 @src/namerec/uma/jsql/parser.py:435-437 @src/namerec/uma/handlers/base.py:213-214 @tests/test_basic.py:44-45 

---

**Cursor**

I'll verify and fix these bugs. Let me first check the current implementation of the affected areas.

Now I can see the issues. Let me fix both bugs:

**Bug 1**: The parser accesses `context.metadata.tables` directly in synchronous methods, but metadata might not be loaded yet.
**Bug 2**: The handler's `meta()` method doesn't include the namespace prefix when the entity_name doesn't have one.

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import exists
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)

            # Note: Table should be preloaded in executor before parsing
            # This ensures lazy loading happens before synchronous parsing
            table = await self._get_table_async(entity_name)

            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    async def _get_table_async(self, entity_name: EntityName) -> Any:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables (metadata should be preloaded by executor)
            try:
                metadata_tables = self.context.metadata.tables
            except RuntimeError:
                # Metadata not preloaded - provide helpful error
                raise JSQLSyntaxError(
                    f'Cannot resolve column "{field_spec}": metadata not preloaded. '
                    'Ensure executor calls metadata preload before parsing.'
                ) from None
            
            if table_name in metadata_tables:
                table = metadata_tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            elif table_name in self.context.metadata.tables:
                table = self.context.metadata.tables[table_name]
            else:
                raise JSQLSyntaxError(f'Table "{table_name}" not found', path)

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import exists
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)

            # Note: Table should be preloaded in executor before parsing
            # This ensures lazy loading happens before synchronous parsing
            table = await self._get_table_async(entity_name)

            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    async def _get_table_async(self, entity_name: EntityName) -> Any:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables (metadata should be preloaded by executor)
            try:
                metadata_tables = self.context.metadata.tables
            except RuntimeError:
                # Metadata not preloaded - provide helpful error
                raise JSQLSyntaxError(
                    f'Cannot resolve column "{field_spec}": metadata not preloaded. '
                    'Ensure executor calls metadata preload before parsing.'
                ) from None
            
            if table_name in metadata_tables:
                table = metadata_tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            else:
                # Access metadata tables (should be preloaded by executor)
                try:
                    metadata_tables = self.context.metadata.tables
                except RuntimeError:
                    # Metadata not preloaded - provide helpful error
                    raise JSQLSyntaxError(
                        f'Cannot process join for table "{table_name}": metadata not preloaded. '
                        'Ensure executor calls metadata preload before parsing.',
                        path,
                    ) from None
                
                if table_name in metadata_tables:
                    table = metadata_tables[table_name]
                else:
                    raise JSQLSyntaxError(f'Table "{table_name}" not found', path)

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

```python
"""Default entity handler for database tables."""

from typing import Any

from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import delete as sql_delete
from sqlalchemy import insert
from sqlalchemy import select
from sqlalchemy import update

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import get_table


class DefaultEntityHandler:
    """
    Default handler for regular database tables.
    Services most tables without specific customization.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,
    ) -> Select:
        """
        Simple SELECT * FROM table.

        Args:
            entity_name: Entity name
            params: Query parameters (unused in default implementation)
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        table = await get_table(context, entity_name)
        return select(table)

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Read through select + filter by id.
        Does not return None - raises UMANotFoundError.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Get select (can be overridden in subclass with policies)
        query = await cls.select(entity_name, {}, context)

        # Add condition by id
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        if len(pk_columns) == 1:
            query = query.where(pk_columns[0] == id_value)
        else:
            conditions = [col == val for col, val in zip(pk_columns, id_value, strict=False)]
            query = query.where(and_(*conditions))

        # Execute
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()

            if row is None:
                raise UMANotFoundError(
                    str(entity_name),
                    f'Record with id={id_value} not found or access denied',
                )

            return dict(row)

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext,
    ) -> Any:
        """
        Save to table.
        Duplicate access check removed - will be in _ensure_exists().

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        # Determine create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)

        is_create = id_value is None or (isinstance(id_value, tuple) and None in id_value)

        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)

                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                return tuple(result.inserted_primary_key)

            # UPDATE
            # Check access through read (DRY - avoid duplication)
            await cls._ensure_exists(entity_name, id_value, context)

            # Build condition by PK
            pk_condition = cls._build_pk_condition(pk_columns, id_value)

            stmt = update(table).where(pk_condition).values(**data)
            await conn.execute(stmt)
            return id_value

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> bool:
        """
        Delete from table.
        Uses _ensure_exists() for access check (DRY).

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Check access through read (DRY)
        await cls._ensure_exists(entity_name, id_value, context)

        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)
        pk_condition = cls._build_pk_condition(pk_columns, id_value)

        async with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = await conn.execute(stmt)
            return result.rowcount > 0

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict:
        """
        Metadata from SQLAlchemy MetaData + enrichment from MetadataProvider.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        table = await get_table(context, entity_name)

        columns = []
        for col in table.columns:
            col_info = {
                'name': col.name,
                'type': str(col.type),
                'nullable': col.nullable,
                'primary_key': col.primary_key,
            }

            # Foreign keys
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info['related_to'] = fk.column.table.name
                col_info['related_key'] = fk.column.name

            columns.append(col_info)

        # Construct name with namespace
        if entity_name.namespace:
            name = str(entity_name)
        else:
            # Entity parsed without namespace - use context's namespace
            name = f'{context.namespace}:{entity_name.entity}'

        metadata = {
            'name': name,
            'description': '',
            'columns': columns,
        }

        # Enrich from MetadataProvider
        if context.metadata_provider:
            try:
                provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
                metadata.update(provider_metadata)
            except Exception:  # noqa: S110
                pass

        return metadata

    # ========== Helper methods (DRY) ==========

    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict:
        """
        Check record existence and access through read().
        Extracted to separate method to avoid duplication in save/delete.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Dictionary with record data

        Raises:
            UMANotFoundError: If record not found or no access
        """
        return await cls.read(entity_name, id_value, context)

    @staticmethod
    def _build_pk_condition(pk_columns: list, id_value: Any) -> Any:
        """
        Build WHERE condition for primary key.
        Supports simple and composite keys.

        Args:
            pk_columns: List of PK columns
            id_value: ID value(s)

        Returns:
            SQLAlchemy condition
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value

        return and_(*[col == val for col, val in zip(pk_columns, id_value, strict=False)])

```

Now let me check the executor to ensure it properly preloads metadata before parsing:

Let me check where joins are processed in the parser:

Let me check the signature of `_process_joins_for_aliases`:

Let me read the full `_process_joins_for_aliases` method to update it properly:

Now I need to update `_process_joins_for_aliases` to use `_get_table_async` instead of accessing `context.metadata.tables` directly:

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import exists
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)

            # Note: Table should be preloaded in executor before parsing
            # This ensures lazy loading happens before synchronous parsing
            table = await self._get_table_async(entity_name)

            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    async def _get_table_async(self, entity_name: EntityName) -> Any:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Check metadata tables (metadata should be preloaded by executor)
            try:
                metadata_tables = self.context.metadata.tables
            except RuntimeError:
                # Metadata not preloaded - provide helpful error
                raise JSQLSyntaxError(
                    f'Cannot resolve column "{field_spec}": metadata not preloaded. '
                    'Ensure executor calls metadata preload before parsing.'
                ) from None
            
            if table_name in metadata_tables:
                table = metadata_tables[table_name]
                if column_name in table.columns:
                    return table.columns[column_name]

            raise JSQLSyntaxError(f'Column "{field_spec}" not found')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            else:
                # Load table via metadata provider (lazy loading)
                entity_name = parse_entity_name(table_name)
                try:
                    table = await self._get_table_async(entity_name)
                except Exception as e:
                    raise JSQLSyntaxError(f'Table "{table_name}" not found: {e}', path) from e

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

Let me check where `_resolve_column` is called from to see if we can make it async:

Now let me update `_resolve_column` to use async table loading:

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import exists
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name or subquery specification

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid
        """
        # Simple table name
        if isinstance(from_spec, str):
            # Check if it's a CTE
            if from_spec in self.ctes:
                cte = self.ctes[from_spec]
                self._register_table_alias(from_spec, cte)
                return cte

            # Regular table
            entity_name = parse_entity_name(from_spec)

            # Note: Table should be preloaded in executor before parsing
            # This ensures lazy loading happens before synchronous parsing
            table = await self._get_table_async(entity_name)

            # Register both by entity name and by from_spec (they might differ with namespace)
            self._register_table_alias(
                entity_name.entity,
                table,
                *([] if from_spec == entity_name.entity else [from_spec])
            )
            return table

        # Subquery (not implemented in this version)
        raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

    async def _get_table_async(self, entity_name: EntityName) -> Any:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Load table via metadata provider (lazy loading)
            entity_name = parse_entity_name(table_name)
            try:
                table = await self._get_table_async(entity_name)
                if column_name in table.columns:
                    return table.columns[column_name]
            except Exception as e:
                raise JSQLSyntaxError(f'Column "{field_spec}" not found: {e}') from e

            raise JSQLSyntaxError(f'Column "{column_name}" not found in table "{table_name}"')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            else:
                # Load table via metadata provider (lazy loading)
                entity_name = parse_entity_name(table_name)
                try:
                    table = await self._get_table_async(entity_name)
                except Exception as e:
                    raise JSQLSyntaxError(f'Table "{table_name}" not found: {e}', path) from e

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

Now let me check for any linter errors in the modified files:

Perfect! Now let's run the tests to verify the fixes:

The issue is that the `engine` fixture needs to be properly marked as async. Let me check and fix the conftest.py:

I need to fix the async fixtures in conftest.py to use `pytest_asyncio.fixture`:

```python
"""Pytest configuration and fixtures."""

import pytest
import pytest_asyncio
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAContext
from namerec.uma import init_global_registry
from namerec.uma import uma_initialize


@pytest.fixture
def metadata() -> MetaData:
    """Create test metadata with simple schema."""
    metadata = MetaData()

    Table(
        'users',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(100), nullable=False),
        Column('email', String(100), nullable=False),
    )

    return metadata


@pytest_asyncio.fixture
async def engine(metadata: MetaData):  # noqa: ANN201
    """Create async engine with test database."""
    engine = create_async_engine('sqlite+aiosqlite:///:memory:')

    yield engine

    await engine.dispose()


@pytest_asyncio.fixture
async def context(engine, metadata) -> UMAContext:  # noqa: ANN001
    """Create UMA context."""
    metadata_provider = DefaultMetadataProvider()
    
    # Initialize UMA with namespace config
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Create context
    ctx = UMAContext(
        engine=engine,
        metadata_provider=metadata_provider,
        namespace='test',
    )
    
    # Preload metadata for tests (creates tables first)
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    await metadata_provider.preload(engine)
    
    return ctx


@pytest.fixture(autouse=True)
def reset_global_registry() -> None:
    """Reset global registry before each test."""
    init_global_registry()

```

Now let's run the tests again:

Perfect! All basic tests are passing. Let me run all tests to ensure nothing else was broken:

Let me check the test output:

I can see that the tests are still using the old `initialize_uma` signature. Let me find and update all tests that use this function:

Let me update the test files to use the new initialization signature. First, let's check `test_api.py`:

```python
"""Tests for UMA API functions."""

import pytest

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAAccessDeniedError
from namerec.uma import UMAContext
from namerec.uma import UMANotFoundError
from namerec.uma import uma_delete
from namerec.uma import uma_initialize
from namerec.uma import uma_list_entities
from namerec.uma import uma_meta
from namerec.uma import uma_read
from namerec.uma import uma_save


async def _initialize_test_uma(engine, metadata):  # noqa: ANN001, ANN202
    """Helper to initialize UMA for tests."""
    # Create tables first
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    # Initialize UMA
    metadata_provider = DefaultMetadataProvider()
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Preload metadata
    await metadata_provider.preload(engine)


@pytest.mark.asyncio
async def test_initialize_uma(engine, metadata) -> None:  # noqa: ANN001
    """Test UMA initialization."""
    await _initialize_test_uma(engine, metadata)


@pytest.mark.asyncio
async def test_uma_list_entities(engine, metadata) -> None:  # noqa: ANN001
    """Test listing entities."""
    await _initialize_test_uma(engine, metadata)

    entities = await uma_list_entities()
    assert 'users' in entities


@pytest.mark.asyncio
async def test_uma_meta(engine, metadata) -> None:  # noqa: ANN001
    """Test getting entity metadata."""
    await _initialize_test_uma(engine, metadata)

    meta = await uma_meta('users')
    assert meta['name'] == 'test:users'
    assert len(meta['columns']) == 3


@pytest.mark.asyncio
async def test_uma_save_and_read(engine, metadata) -> None:  # noqa: ANN001
    """Test creating and reading a record."""
    await _initialize_test_uma(engine, metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})
    assert user_id is not None

    # Read
    user = await uma_read('users', user_id)
    assert user['name'] == 'Test User'
    assert user['email'] == 'test@example.com'


@pytest.mark.asyncio
async def test_uma_update(engine, metadata) -> None:  # noqa: ANN001
    """Test updating a record."""
    await _initialize_test_uma(engine, metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})

    # Update
    await uma_save('users', {'id': user_id, 'name': 'Updated User', 'email': 'updated@example.com'})

    # Verify
    user = await uma_read('users', user_id)
    assert user['name'] == 'Updated User'


@pytest.mark.asyncio
async def test_uma_delete(engine, metadata) -> None:  # noqa: ANN001
    """Test deleting a record."""
    await _initialize_test_uma(engine, metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})

    # Delete
    deleted = await uma_delete('users', user_id)
    assert deleted is True

    # Verify deletion
    with pytest.raises(UMANotFoundError):
        await uma_read('users', user_id)


@pytest.mark.asyncio
async def test_uma_not_initialized() -> None:
    """Test API functions raise error when UMA not initialized."""
    # Reset by importing fresh module would be needed, but for now just test with initialized
    # In real scenario, this would require module reloading
    pass

```

Now let me update test_jsql.py:

```python
"""Tests for JSQL functionality."""

from datetime import date

import pytest
import pytest_asyncio
from sqlalchemy import Column
from sqlalchemy import Date
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import Numeric
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import JSQLExecutionError
from namerec.uma import JSQLSyntaxError
from namerec.uma import NamespaceConfig
from namerec.uma import uma_initialize
from namerec.uma import uma_select


@pytest_asyncio.fixture
async def test_db():
    """Create test database with sample data."""
    # Create async engine
    engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)

    # Create metadata
    metadata = MetaData()

    # Define tables
    customers = Table(
        'customers',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(255), nullable=False),
        Column('email', String(255)),
        Column('department', String(100)),
        Column('active', Integer, default=1),  # SQLite doesn't have BOOLEAN
    )

    orders = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('customer_id', Integer, ForeignKey('customers.id')),
        Column('order_date', Date, nullable=False),
        Column('amount', Numeric(10, 2), nullable=False),
        Column('quantity', Integer, nullable=False),
    )

    # Create tables and insert data using async engine
    async with engine.begin() as conn:
        # Create all tables
        await conn.run_sync(metadata.create_all)

        # Insert test data
        await conn.execute(
            customers.insert(),
            [
                {'id': 1, 'name': 'John Doe', 'email': 'john@example.com', 'department': 'Engineering', 'active': 1},
                {'id': 2, 'name': 'Jane Smith', 'email': 'jane@example.com', 'department': 'Sales', 'active': 1},
                {'id': 3, 'name': 'Bob Johnson', 'email': 'bob@example.com', 'department': 'Engineering', 'active': 0},
            ],
        )
        await conn.execute(
            orders.insert(),
            [
                {'id': 1, 'customer_id': 1, 'order_date': date(2024, 1, 15), 'amount': 100.00, 'quantity': 2},
                {'id': 2, 'customer_id': 1, 'order_date': date(2024, 2, 20), 'amount': 250.00, 'quantity': 5},
                {'id': 3, 'customer_id': 2, 'order_date': date(2024, 1, 10), 'amount': 75.50, 'quantity': 1},
            ],
        )

    # Initialize UMA
    metadata_provider = DefaultMetadataProvider()
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Preload metadata
    await metadata_provider.preload(engine)

    yield engine, metadata

    # Cleanup
    await engine.dispose()


@pytest.mark.asyncio
async def test_simple_select(test_db):
    """Test simple SELECT query."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
    }

    result = await uma_select(jsql)

    assert 'meta' in result
    assert 'data' in result
    assert len(result['meta']) == 3
    assert len(result['data']) == 3
    assert result['meta'][0]['name'] == 'id'
    assert result['meta'][1]['name'] == 'name'
    assert result['meta'][2]['name'] == 'email'


@pytest.mark.asyncio
async def test_select_with_where(test_db):
    """Test SELECT with WHERE clause."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': '=',
            'left': {'field': 'department'},
            'right': {'value': 'Engineering'},
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # John Doe and Bob Johnson


@pytest.mark.asyncio
async def test_select_with_and_condition(test_db):
    """Test SELECT with AND condition."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': 'AND',
            'conditions': [
                {
                    'op': '=',
                    'left': {'field': 'department'},
                    'right': {'value': 'Engineering'},
                },
                {
                    'op': '=',
                    'left': {'field': 'active'},
                    'right': {'value': 1},
                },
            ],
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 1  # Only John Doe is active in Engineering


@pytest.mark.asyncio
async def test_select_with_alias(test_db):
    """Test SELECT with column aliases."""
    jsql = {
        'from': 'customers',
        'select': [
            {'field': 'id'},
            {'field': 'name', 'alias': 'customer_name'},
            {'field': 'email', 'alias': 'contact_email'},
        ],
        'limit': 1,
    }

    result = await uma_select(jsql)

    assert result['meta'][1]['name'] == 'customer_name'
    assert result['meta'][2]['name'] == 'contact_email'


@pytest.mark.asyncio
async def test_aggregation(test_db):
    """Test aggregation functions."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'COUNT', 'args': [{'field': 'id'}], 'alias': 'order_count'},
            {'func': 'SUM', 'args': [{'field': 'amount'}], 'alias': 'total'},
        ],
        'group_by': [{'field': 'customer_id'}],
    }

    result = await uma_select(jsql)

    assert len(result['meta']) == 3
    assert result['meta'][1]['name'] == 'order_count'
    assert result['meta'][2]['name'] == 'total'
    assert len(result['data']) == 2  # Two customers with orders


@pytest.mark.asyncio
async def test_join(test_db):
    """Test JOIN query."""
    jsql = {
        'from': 'orders',
        'joins': [
            {
                'type': 'LEFT',
                'table': 'customers',
                'alias': 'c',
                'on': {
                    'op': '=',
                    'left': {'field': 'orders.customer_id'},
                    'right': {'field': 'c.id'},
                },
            },
        ],
        'select': [
            {'field': 'orders.id'},
            {'field': 'c.name', 'alias': 'customer_name'},
            {'field': 'orders.amount'},
        ],
    }

    result = await uma_select(jsql)

    assert len(result['meta']) == 3
    assert result['meta'][1]['name'] == 'customer_name'
    assert len(result['data']) == 3  # Three orders


@pytest.mark.asyncio
async def test_order_by(test_db):
    """Test ORDER BY clause."""
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'order_by': [{'field': 'amount', 'direction': 'desc'}],
    }

    result = await uma_select(jsql)

    # Check that amounts are in descending order
    amounts = [row[1] for row in result['data']]
    assert amounts == sorted(amounts, reverse=True)


@pytest.mark.asyncio
async def test_limit_offset(test_db):
    """Test LIMIT and OFFSET."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'order_by': [{'field': 'id', 'direction': 'asc'}],
        'limit': 2,
        'offset': 1,
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2
    assert result['data'][0][0] == 2  # Second customer


@pytest.mark.asyncio
async def test_expression(test_db):
    """Test expression in SELECT."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'amount'},
            {'field': 'quantity'},
            {
                'expr': {
                    'op': '*',
                    'left': {'field': 'amount'},
                    'right': {'value': 1.1},
                },
                'alias': 'amount_with_tax',
            },
        ],
        'limit': 1,
    }

    result = await uma_select(jsql)

    assert result['meta'][3]['name'] == 'amount_with_tax'
    # Check that calculation is correct
    amount = float(result['data'][0][1])
    amount_with_tax = float(result['data'][0][3])
    assert abs(amount_with_tax - amount * 1.1) < 0.01


@pytest.mark.asyncio
async def test_cte(test_db):
    """Test CTE (Common Table Expression)."""
    jsql = {
        'with': [
            {
                'name': 'active_customers',
                'query': {
                    'from': 'customers',
                    'select': ['id', 'name'],
                    'where': {
                        'op': '=',
                        'left': {'field': 'active'},
                        'right': {'value': 1},
                    },
                },
            },
        ],
        'from': 'active_customers',
        'select': ['*'],
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # Two active customers


@pytest.mark.asyncio
async def test_subquery_in(test_db):
    """Test subquery with IN operator."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': 'IN',
            'left': {'field': 'id'},
            'right': {
                'subquery': {
                    'from': 'orders',
                    'select': [{'field': 'customer_id'}],
                },
            },
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # Customers who have orders


@pytest.mark.asyncio
async def test_query_parameters(test_db):
    """Test query with parameters."""
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }

    params = {'min_amount': 100}
    result = await uma_select(jsql, params)

    # Should get orders with amount >= 100
    assert len(result['data']) == 2


@pytest.mark.asyncio
async def test_invalid_jsql_missing_from(test_db):
    """Test error handling for missing 'from' field."""
    jsql = {
        'select': ['id', 'name'],
    }

    with pytest.raises(ValueError, match='must contain'):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_invalid_jsql_unknown_table(test_db):
    """Test error handling for unknown table."""
    jsql = {
        'from': 'nonexistent_table',
        'select': ['id'],
    }

    with pytest.raises((JSQLSyntaxError, JSQLExecutionError)):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_invalid_jsql_unknown_column(test_db):
    """Test error handling for unknown column."""
    jsql = {
        'from': 'customers',
        'select': ['nonexistent_column'],
    }

    with pytest.raises((JSQLSyntaxError, JSQLExecutionError)):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_result_metadata_types(test_db):
    """Test that result metadata includes type information."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
        'limit': 1,
    }

    result = await uma_select(jsql)

    # Check metadata structure
    for col_meta in result['meta']:
        assert 'name' in col_meta
        assert 'type' in col_meta
        assert 'nullable' in col_meta
        assert 'qualified_name' in col_meta
        assert 'source_entity' in col_meta
        assert 'source_field' in col_meta


@pytest.mark.asyncio
async def test_having_clause(test_db):
    """Test HAVING clause with aggregation."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'COUNT', 'args': [{'field': 'id'}], 'alias': 'order_count'},
        ],
        'group_by': [{'field': 'customer_id'}],
        'having': {
            'op': '>',
            'left': {'func': 'COUNT', 'args': [{'field': 'id'}]},
            'right': {'value': 1},
        },
    }

    result = await uma_select(jsql)

    # Only customer with more than 1 order
    assert len(result['data']) == 1
    assert result['data'][0][1] == 2  # Customer 1 has 2 orders


@pytest.mark.asyncio
async def test_window_function_row_number(test_db):
    """Test window function ROW_NUMBER."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'customer_id'},
            {'field': 'amount'},
            {
                'window_func': {
                    'func': 'ROW_NUMBER',
                    'partition_by': [{'field': 'customer_id'}],
                    'order_by': [{'field': 'amount', 'direction': 'desc'}],
                },
                'alias': 'row_num',
            },
        ],
    }

    result = await uma_select(jsql)

    # Check that we have row numbers
    assert len(result['meta']) == 4
    assert result['meta'][3]['name'] == 'row_num'
    assert len(result['data']) == 3

    # Check row numbers are correct (should restart for each customer)
    # Customer 1 has 2 orders: amounts 250.00 (rank 1), 100.00 (rank 2)
    customer_1_orders = [row for row in result['data'] if row[1] == 1]
    assert len(customer_1_orders) == 2
    # The order with amount 250.00 should have row_num = 1
    assert any(row[2] == 250.00 and row[3] == 1 for row in customer_1_orders)


@pytest.mark.asyncio
async def test_window_function_avg(test_db):
    """Test window function AVG."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'customer_id'},
            {'field': 'amount'},
            {
                'window_func': {
                    'func': 'AVG',
                    'args': [{'field': 'amount'}],
                    'partition_by': [{'field': 'customer_id'}],
                },
                'alias': 'avg_amount',
            },
        ],
    }

    result = await uma_select(jsql)

    # Check metadata
    assert result['meta'][3]['name'] == 'avg_amount'

    # Customer 1 has 2 orders: 100.00 and 250.00, avg = 175.00
    customer_1_orders = [row for row in result['data'] if row[1] == 1]
    assert len(customer_1_orders) == 2
    # Both rows should have the same average
    for row in customer_1_orders:
        assert abs(float(row[3]) - 175.00) < 0.01


@pytest.mark.asyncio
async def test_order_by_alias(test_db):
    """Test ORDER BY with column alias from aggregation."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'SUM', 'args': [{'field': 'amount'}], 'alias': 'total'},
        ],
        'group_by': [{'field': 'customer_id'}],
        'order_by': [{'field': 'total', 'direction': 'desc'}],
    }

    result = await uma_select(jsql)

    # Check that results are ordered by total descending
    totals = [float(row[1]) for row in result['data']]
    assert totals == sorted(totals, reverse=True)
    # Customer 1 should be first (has 2 orders: 100 + 250 = 350)
    assert result['data'][0][0] == 1


@pytest.mark.asyncio
async def test_limit_zero(test_db):
    """Test LIMIT 0 is applied correctly (Bug fix: 0 is falsy but valid)."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'limit': 0,
    }

    result = await uma_select(jsql)

    # LIMIT 0 should return no rows
    assert len(result['data']) == 0
    # But metadata should still be present
    assert len(result['meta']) == 2


@pytest.mark.asyncio
async def test_offset_zero(test_db):
    """Test OFFSET 0 is applied correctly (Bug fix: 0 is falsy but valid)."""
    jsql = {
        'from': 'customers',
        'select': ['id'],
        'order_by': [{'field': 'id', 'direction': 'asc'}],
        'offset': 0,
        'limit': 2,
    }

    result = await uma_select(jsql)

    # OFFSET 0 should be same as no offset - starts from first row
    assert len(result['data']) == 2
    assert result['data'][0][0] == 1  # First customer ID


@pytest.mark.asyncio
async def test_arithmetic_operators_validation(test_db):
    """Test arithmetic operators validate required fields (Bug fix: prevent KeyError)."""
    # Missing 'left' field
    jsql_missing_left = {
        'from': 'orders',
        'select': [
            {
                'expr': {
                    'op': '+',
                    'right': {'field': 'quantity'},
                },
                'alias': 'result',
            },
        ],
    }

    with pytest.raises(JSQLSyntaxError) as exc_info:
        await uma_select(jsql_missing_left)
    assert '+ operator must have "left" field' in str(exc_info.value)

    # Missing 'right' field
    jsql_missing_right = {
        'from': 'orders',
        'select': [
            {
                'expr': {
                    'op': '*',
                    'left': {'field': 'quantity'},
                },
                'alias': 'result',
            },
        ],
    }

    with pytest.raises(JSQLSyntaxError) as exc_info:
        await uma_select(jsql_missing_right)
    assert '* operator must have "right" field' in str(exc_info.value)


@pytest.mark.asyncio
async def test_debug_mode(test_db: tuple) -> None:
    """Test JSQL debug mode returns SQL in response."""
    # test_db is already initialized via fixture
    # No need to initialize again

    # Simple query with debug enabled
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}, {'field': 'name'}],
        'where': {
            'op': '=',
            'left': {'field': 'active'},
            'right': {'value': 1},
        },
        'debug': True,  # Enable debug mode
    }

    result = await uma_select(jsql)

    # Check that debug SQL is present
    assert 'debug' in result
    assert result['debug'] is not None
    assert isinstance(result['debug'], str)

    # Check that debug SQL contains expected keywords
    debug_sql = result['debug']
    assert 'SELECT' in debug_sql
    assert 'FROM' in debug_sql
    assert 'customers' in debug_sql
    assert 'WHERE' in debug_sql

    # Check that data is still returned normally
    assert 'meta' in result
    assert 'data' in result
    assert len(result['data']) > 0


@pytest.mark.asyncio
async def test_debug_mode_disabled(test_db: tuple) -> None:
    """Test that debug SQL is not included when debug mode is disabled."""
    # test_db is already initialized via fixture
    # No need to initialize again

    # Query without debug
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}],
    }

    result = await uma_select(jsql)

    # Check that debug SQL is not present
    assert 'debug' not in result or result.get('debug') is None


@pytest.mark.asyncio
async def test_debug_mode_with_in_operator(test_db: tuple) -> None:
    """Test debug mode with IN operator (complex type that may fail literal_binds)."""
    # test_db is already initialized via fixture
    # No need to initialize again

    # Query with IN operator and debug enabled
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}, {'field': 'name'}],
        'where': {
            'op': 'IN',
            'left': {'field': 'id'},
            'right': [1, 2, 3],  # List of values for IN operator
        },
        'debug': True,  # Enable debug mode
    }

    # This should not raise an error even if literal_binds fails
    result = await uma_select(jsql)

    # Check that query executed successfully
    assert 'data' in result
    assert len(result['data']) > 0

    # Check that debug SQL is present (may contain parameters or error message)
    assert 'debug' in result
    assert result['debug'] is not None
    assert isinstance(result['debug'], str)

    # Debug SQL should contain query structure even if parameters aren't substituted
    debug_sql = result['debug']
    assert 'SELECT' in debug_sql or 'Failed to generate' in debug_sql

```

Now let's run all tests again:

Отлично! Все тесты успешно пройдены (52/52). Давайте подведём итог исправленных багов:

## Исправленные баги

### Bug 1: Синхронный доступ к metadata в парсере
**Проблема**: Парсер обращался к `context.metadata.tables` напрямую в методах `_resolve_column()` (строки 372-373) и `_process_joins_for_aliases()` (строки 436-437), что вызывало `RuntimeError`, если метаданные не были предзагружены.

**Исправление**:
- Обновил `_resolve_column()` для использования `await _get_table_async()` при разрешении квалифицированных ссылок на колонки (например, `users.id`)
- Обновил `_process_joins_for_aliases()` для использования `await _get_table_async()` при обработке JOIN-ов
- Теперь метаданные загружаются лениво по требованию, что обеспечивает работу даже с таблицами, не указанными в основном FROM-clause

### Bug 2: Отсутствие namespace в metadata name
**Проблема**: Тест ожидал, что `metadata['name']` будет содержать префикс namespace (`'test:users'`), но метод `meta()` возвращал только `str(entity_name)`. Когда `entity_name` парсился без явного namespace, он оставался без префикса.

**Исправление**:
- Обновил метод `meta()` в `DefaultEntityHandler` для проверки наличия namespace в `entity_name`
- Если namespace отсутствует, используется namespace из контекста для формирования полного имени: `f'{context.namespace}:{entity_name.entity}'`

### Дополнительные исправления
- Исправил `conftest.py`: использую `@pytest_asyncio.fixture` вместо `@pytest.fixture` для async-фикстур
- Обновил `test_api.py`: создал helper-функцию `_initialize_test_uma()` для правильной инициализации с новым API
- Обновил `test_jsql.py`: адаптировал фикстуру `test_db` для использования `uma_initialize` с `NamespaceConfig`

Все 52 теста успешно пройдены! Рефакторинг инициализации UMA завершён и готов к утверждению.

---

**User**

Verify these issues exist and fix them:

Bug 1:
The `preload()` method always caches metadata under the hardcoded key `'preloaded'`, but `get_table()` and `list_entities()` look for cached metadata under the actual namespace name (e.g., `'test'` or `'default'`). This means the preload call never provides any benefit - metadata will be reloaded on first actual access. The method should either accept a namespace parameter or load metadata for all configured namespaces.

 @src/namerec/uma/metadata.py:193-205 

---

**Cursor**

Отлично подмечено! Это действительно критический баг. Метод `preload()` кеширует метаданные под ключом `'preloaded'`, но все остальные методы ищут по реальному namespace. Давайте исправим это.

Теперь исправлю метод `preload()`:

```python
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
        namespace = entity_name.namespace or 'default'

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
            await self.get_table(entity_name, context.engine)
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
            # Check if engine is async or sync
            if hasattr(engine, 'begin'):
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

```

Теперь обновлю протокол `MetadataProvider`:

```python
"""Type definitions for UMA."""

from dataclasses import dataclass
from enum import Enum
from typing import Any
from typing import Protocol
from typing import runtime_checkable

from sqlalchemy import Engine
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

    async def list_entities(
        self,
        namespace: str,
        context: 'UMAContext',
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
        context: 'UMAContext',
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
        context: 'UMAContext | None' = None,
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


# Forward declaration for type checking
from namerec.uma.core.context import UMAContext  # noqa: E402

```

Теперь обновлю вызовы `preload()` в тестах:

```python
"""Pytest configuration and fixtures."""

import pytest
import pytest_asyncio
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAContext
from namerec.uma import init_global_registry
from namerec.uma import uma_initialize


@pytest.fixture
def metadata() -> MetaData:
    """Create test metadata with simple schema."""
    metadata = MetaData()

    Table(
        'users',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(100), nullable=False),
        Column('email', String(100), nullable=False),
    )

    return metadata


@pytest_asyncio.fixture
async def engine(metadata: MetaData):  # noqa: ANN201
    """Create async engine with test database."""
    engine = create_async_engine('sqlite+aiosqlite:///:memory:')

    yield engine

    await engine.dispose()


@pytest_asyncio.fixture
async def context(engine, metadata) -> UMAContext:  # noqa: ANN001
    """Create UMA context."""
    metadata_provider = DefaultMetadataProvider()
    
    # Initialize UMA with namespace config
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Create context
    ctx = UMAContext(
        engine=engine,
        metadata_provider=metadata_provider,
        namespace='test',
    )
    
    # Preload metadata for tests (creates tables first)
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    await metadata_provider.preload(engine, namespace='test')
    
    return ctx


@pytest.fixture(autouse=True)
def reset_global_registry() -> None:
    """Reset global registry before each test."""
    init_global_registry()

```

```python
"""Tests for UMA API functions."""

import pytest

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAAccessDeniedError
from namerec.uma import UMAContext
from namerec.uma import UMANotFoundError
from namerec.uma import uma_delete
from namerec.uma import uma_initialize
from namerec.uma import uma_list_entities
from namerec.uma import uma_meta
from namerec.uma import uma_read
from namerec.uma import uma_save


async def _initialize_test_uma(engine, metadata):  # noqa: ANN001, ANN202
    """Helper to initialize UMA for tests."""
    # Create tables first
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    # Initialize UMA
    metadata_provider = DefaultMetadataProvider()
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Preload metadata
    await metadata_provider.preload(engine, namespace='test')


@pytest.mark.asyncio
async def test_initialize_uma(engine, metadata) -> None:  # noqa: ANN001
    """Test UMA initialization."""
    await _initialize_test_uma(engine, metadata)


@pytest.mark.asyncio
async def test_uma_list_entities(engine, metadata) -> None:  # noqa: ANN001
    """Test listing entities."""
    await _initialize_test_uma(engine, metadata)

    entities = await uma_list_entities()
    assert 'users' in entities


@pytest.mark.asyncio
async def test_uma_meta(engine, metadata) -> None:  # noqa: ANN001
    """Test getting entity metadata."""
    await _initialize_test_uma(engine, metadata)

    meta = await uma_meta('users')
    assert meta['name'] == 'test:users'
    assert len(meta['columns']) == 3


@pytest.mark.asyncio
async def test_uma_save_and_read(engine, metadata) -> None:  # noqa: ANN001
    """Test creating and reading a record."""
    await _initialize_test_uma(engine, metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})
    assert user_id is not None

    # Read
    user = await uma_read('users', user_id)
    assert user['name'] == 'Test User'
    assert user['email'] == 'test@example.com'


@pytest.mark.asyncio
async def test_uma_update(engine, metadata) -> None:  # noqa: ANN001
    """Test updating a record."""
    await _initialize_test_uma(engine, metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})

    # Update
    await uma_save('users', {'id': user_id, 'name': 'Updated User', 'email': 'updated@example.com'})

    # Verify
    user = await uma_read('users', user_id)
    assert user['name'] == 'Updated User'


@pytest.mark.asyncio
async def test_uma_delete(engine, metadata) -> None:  # noqa: ANN001
    """Test deleting a record."""
    await _initialize_test_uma(engine, metadata)

    # Create
    user_id = await uma_save('users', {'name': 'Test User', 'email': 'test@example.com'})

    # Delete
    deleted = await uma_delete('users', user_id)
    assert deleted is True

    # Verify deletion
    with pytest.raises(UMANotFoundError):
        await uma_read('users', user_id)


@pytest.mark.asyncio
async def test_uma_not_initialized() -> None:
    """Test API functions raise error when UMA not initialized."""
    # Reset by importing fresh module would be needed, but for now just test with initialized
    # In real scenario, this would require module reloading
    pass

```

```python
"""Tests for JSQL functionality."""

from datetime import date

import pytest
import pytest_asyncio
from sqlalchemy import Column
from sqlalchemy import Date
from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import Numeric
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import JSQLExecutionError
from namerec.uma import JSQLSyntaxError
from namerec.uma import NamespaceConfig
from namerec.uma import uma_initialize
from namerec.uma import uma_select


@pytest_asyncio.fixture
async def test_db():
    """Create test database with sample data."""
    # Create async engine
    engine = create_async_engine('sqlite+aiosqlite:///:memory:', echo=False)

    # Create metadata
    metadata = MetaData()

    # Define tables
    customers = Table(
        'customers',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(255), nullable=False),
        Column('email', String(255)),
        Column('department', String(100)),
        Column('active', Integer, default=1),  # SQLite doesn't have BOOLEAN
    )

    orders = Table(
        'orders',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('customer_id', Integer, ForeignKey('customers.id')),
        Column('order_date', Date, nullable=False),
        Column('amount', Numeric(10, 2), nullable=False),
        Column('quantity', Integer, nullable=False),
    )

    # Create tables and insert data using async engine
    async with engine.begin() as conn:
        # Create all tables
        await conn.run_sync(metadata.create_all)

        # Insert test data
        await conn.execute(
            customers.insert(),
            [
                {'id': 1, 'name': 'John Doe', 'email': 'john@example.com', 'department': 'Engineering', 'active': 1},
                {'id': 2, 'name': 'Jane Smith', 'email': 'jane@example.com', 'department': 'Sales', 'active': 1},
                {'id': 3, 'name': 'Bob Johnson', 'email': 'bob@example.com', 'department': 'Engineering', 'active': 0},
            ],
        )
        await conn.execute(
            orders.insert(),
            [
                {'id': 1, 'customer_id': 1, 'order_date': date(2024, 1, 15), 'amount': 100.00, 'quantity': 2},
                {'id': 2, 'customer_id': 1, 'order_date': date(2024, 2, 20), 'amount': 250.00, 'quantity': 5},
                {'id': 3, 'customer_id': 2, 'order_date': date(2024, 1, 10), 'amount': 75.50, 'quantity': 1},
            ],
        )

    # Initialize UMA
    metadata_provider = DefaultMetadataProvider()
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Preload metadata
    await metadata_provider.preload(engine, namespace='test')

    yield engine, metadata

    # Cleanup
    await engine.dispose()


@pytest.mark.asyncio
async def test_simple_select(test_db):
    """Test simple SELECT query."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
    }

    result = await uma_select(jsql)

    assert 'meta' in result
    assert 'data' in result
    assert len(result['meta']) == 3
    assert len(result['data']) == 3
    assert result['meta'][0]['name'] == 'id'
    assert result['meta'][1]['name'] == 'name'
    assert result['meta'][2]['name'] == 'email'


@pytest.mark.asyncio
async def test_select_with_where(test_db):
    """Test SELECT with WHERE clause."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': '=',
            'left': {'field': 'department'},
            'right': {'value': 'Engineering'},
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # John Doe and Bob Johnson


@pytest.mark.asyncio
async def test_select_with_and_condition(test_db):
    """Test SELECT with AND condition."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': 'AND',
            'conditions': [
                {
                    'op': '=',
                    'left': {'field': 'department'},
                    'right': {'value': 'Engineering'},
                },
                {
                    'op': '=',
                    'left': {'field': 'active'},
                    'right': {'value': 1},
                },
            ],
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 1  # Only John Doe is active in Engineering


@pytest.mark.asyncio
async def test_select_with_alias(test_db):
    """Test SELECT with column aliases."""
    jsql = {
        'from': 'customers',
        'select': [
            {'field': 'id'},
            {'field': 'name', 'alias': 'customer_name'},
            {'field': 'email', 'alias': 'contact_email'},
        ],
        'limit': 1,
    }

    result = await uma_select(jsql)

    assert result['meta'][1]['name'] == 'customer_name'
    assert result['meta'][2]['name'] == 'contact_email'


@pytest.mark.asyncio
async def test_aggregation(test_db):
    """Test aggregation functions."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'COUNT', 'args': [{'field': 'id'}], 'alias': 'order_count'},
            {'func': 'SUM', 'args': [{'field': 'amount'}], 'alias': 'total'},
        ],
        'group_by': [{'field': 'customer_id'}],
    }

    result = await uma_select(jsql)

    assert len(result['meta']) == 3
    assert result['meta'][1]['name'] == 'order_count'
    assert result['meta'][2]['name'] == 'total'
    assert len(result['data']) == 2  # Two customers with orders


@pytest.mark.asyncio
async def test_join(test_db):
    """Test JOIN query."""
    jsql = {
        'from': 'orders',
        'joins': [
            {
                'type': 'LEFT',
                'table': 'customers',
                'alias': 'c',
                'on': {
                    'op': '=',
                    'left': {'field': 'orders.customer_id'},
                    'right': {'field': 'c.id'},
                },
            },
        ],
        'select': [
            {'field': 'orders.id'},
            {'field': 'c.name', 'alias': 'customer_name'},
            {'field': 'orders.amount'},
        ],
    }

    result = await uma_select(jsql)

    assert len(result['meta']) == 3
    assert result['meta'][1]['name'] == 'customer_name'
    assert len(result['data']) == 3  # Three orders


@pytest.mark.asyncio
async def test_order_by(test_db):
    """Test ORDER BY clause."""
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'order_by': [{'field': 'amount', 'direction': 'desc'}],
    }

    result = await uma_select(jsql)

    # Check that amounts are in descending order
    amounts = [row[1] for row in result['data']]
    assert amounts == sorted(amounts, reverse=True)


@pytest.mark.asyncio
async def test_limit_offset(test_db):
    """Test LIMIT and OFFSET."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'order_by': [{'field': 'id', 'direction': 'asc'}],
        'limit': 2,
        'offset': 1,
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2
    assert result['data'][0][0] == 2  # Second customer


@pytest.mark.asyncio
async def test_expression(test_db):
    """Test expression in SELECT."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'amount'},
            {'field': 'quantity'},
            {
                'expr': {
                    'op': '*',
                    'left': {'field': 'amount'},
                    'right': {'value': 1.1},
                },
                'alias': 'amount_with_tax',
            },
        ],
        'limit': 1,
    }

    result = await uma_select(jsql)

    assert result['meta'][3]['name'] == 'amount_with_tax'
    # Check that calculation is correct
    amount = float(result['data'][0][1])
    amount_with_tax = float(result['data'][0][3])
    assert abs(amount_with_tax - amount * 1.1) < 0.01


@pytest.mark.asyncio
async def test_cte(test_db):
    """Test CTE (Common Table Expression)."""
    jsql = {
        'with': [
            {
                'name': 'active_customers',
                'query': {
                    'from': 'customers',
                    'select': ['id', 'name'],
                    'where': {
                        'op': '=',
                        'left': {'field': 'active'},
                        'right': {'value': 1},
                    },
                },
            },
        ],
        'from': 'active_customers',
        'select': ['*'],
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # Two active customers


@pytest.mark.asyncio
async def test_subquery_in(test_db):
    """Test subquery with IN operator."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'where': {
            'op': 'IN',
            'left': {'field': 'id'},
            'right': {
                'subquery': {
                    'from': 'orders',
                    'select': [{'field': 'customer_id'}],
                },
            },
        },
    }

    result = await uma_select(jsql)

    assert len(result['data']) == 2  # Customers who have orders


@pytest.mark.asyncio
async def test_query_parameters(test_db):
    """Test query with parameters."""
    jsql = {
        'from': 'orders',
        'select': ['id', 'amount'],
        'where': {
            'op': '>=',
            'left': {'field': 'amount'},
            'right': {'param': 'min_amount'},
        },
    }

    params = {'min_amount': 100}
    result = await uma_select(jsql, params)

    # Should get orders with amount >= 100
    assert len(result['data']) == 2


@pytest.mark.asyncio
async def test_invalid_jsql_missing_from(test_db):
    """Test error handling for missing 'from' field."""
    jsql = {
        'select': ['id', 'name'],
    }

    with pytest.raises(ValueError, match='must contain'):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_invalid_jsql_unknown_table(test_db):
    """Test error handling for unknown table."""
    jsql = {
        'from': 'nonexistent_table',
        'select': ['id'],
    }

    with pytest.raises((JSQLSyntaxError, JSQLExecutionError)):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_invalid_jsql_unknown_column(test_db):
    """Test error handling for unknown column."""
    jsql = {
        'from': 'customers',
        'select': ['nonexistent_column'],
    }

    with pytest.raises((JSQLSyntaxError, JSQLExecutionError)):
        await uma_select(jsql)


@pytest.mark.asyncio
async def test_result_metadata_types(test_db):
    """Test that result metadata includes type information."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name', 'email'],
        'limit': 1,
    }

    result = await uma_select(jsql)

    # Check metadata structure
    for col_meta in result['meta']:
        assert 'name' in col_meta
        assert 'type' in col_meta
        assert 'nullable' in col_meta
        assert 'qualified_name' in col_meta
        assert 'source_entity' in col_meta
        assert 'source_field' in col_meta


@pytest.mark.asyncio
async def test_having_clause(test_db):
    """Test HAVING clause with aggregation."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'COUNT', 'args': [{'field': 'id'}], 'alias': 'order_count'},
        ],
        'group_by': [{'field': 'customer_id'}],
        'having': {
            'op': '>',
            'left': {'func': 'COUNT', 'args': [{'field': 'id'}]},
            'right': {'value': 1},
        },
    }

    result = await uma_select(jsql)

    # Only customer with more than 1 order
    assert len(result['data']) == 1
    assert result['data'][0][1] == 2  # Customer 1 has 2 orders


@pytest.mark.asyncio
async def test_window_function_row_number(test_db):
    """Test window function ROW_NUMBER."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'customer_id'},
            {'field': 'amount'},
            {
                'window_func': {
                    'func': 'ROW_NUMBER',
                    'partition_by': [{'field': 'customer_id'}],
                    'order_by': [{'field': 'amount', 'direction': 'desc'}],
                },
                'alias': 'row_num',
            },
        ],
    }

    result = await uma_select(jsql)

    # Check that we have row numbers
    assert len(result['meta']) == 4
    assert result['meta'][3]['name'] == 'row_num'
    assert len(result['data']) == 3

    # Check row numbers are correct (should restart for each customer)
    # Customer 1 has 2 orders: amounts 250.00 (rank 1), 100.00 (rank 2)
    customer_1_orders = [row for row in result['data'] if row[1] == 1]
    assert len(customer_1_orders) == 2
    # The order with amount 250.00 should have row_num = 1
    assert any(row[2] == 250.00 and row[3] == 1 for row in customer_1_orders)


@pytest.mark.asyncio
async def test_window_function_avg(test_db):
    """Test window function AVG."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'id'},
            {'field': 'customer_id'},
            {'field': 'amount'},
            {
                'window_func': {
                    'func': 'AVG',
                    'args': [{'field': 'amount'}],
                    'partition_by': [{'field': 'customer_id'}],
                },
                'alias': 'avg_amount',
            },
        ],
    }

    result = await uma_select(jsql)

    # Check metadata
    assert result['meta'][3]['name'] == 'avg_amount'

    # Customer 1 has 2 orders: 100.00 and 250.00, avg = 175.00
    customer_1_orders = [row for row in result['data'] if row[1] == 1]
    assert len(customer_1_orders) == 2
    # Both rows should have the same average
    for row in customer_1_orders:
        assert abs(float(row[3]) - 175.00) < 0.01


@pytest.mark.asyncio
async def test_order_by_alias(test_db):
    """Test ORDER BY with column alias from aggregation."""
    jsql = {
        'from': 'orders',
        'select': [
            {'field': 'customer_id'},
            {'func': 'SUM', 'args': [{'field': 'amount'}], 'alias': 'total'},
        ],
        'group_by': [{'field': 'customer_id'}],
        'order_by': [{'field': 'total', 'direction': 'desc'}],
    }

    result = await uma_select(jsql)

    # Check that results are ordered by total descending
    totals = [float(row[1]) for row in result['data']]
    assert totals == sorted(totals, reverse=True)
    # Customer 1 should be first (has 2 orders: 100 + 250 = 350)
    assert result['data'][0][0] == 1


@pytest.mark.asyncio
async def test_limit_zero(test_db):
    """Test LIMIT 0 is applied correctly (Bug fix: 0 is falsy but valid)."""
    jsql = {
        'from': 'customers',
        'select': ['id', 'name'],
        'limit': 0,
    }

    result = await uma_select(jsql)

    # LIMIT 0 should return no rows
    assert len(result['data']) == 0
    # But metadata should still be present
    assert len(result['meta']) == 2


@pytest.mark.asyncio
async def test_offset_zero(test_db):
    """Test OFFSET 0 is applied correctly (Bug fix: 0 is falsy but valid)."""
    jsql = {
        'from': 'customers',
        'select': ['id'],
        'order_by': [{'field': 'id', 'direction': 'asc'}],
        'offset': 0,
        'limit': 2,
    }

    result = await uma_select(jsql)

    # OFFSET 0 should be same as no offset - starts from first row
    assert len(result['data']) == 2
    assert result['data'][0][0] == 1  # First customer ID


@pytest.mark.asyncio
async def test_arithmetic_operators_validation(test_db):
    """Test arithmetic operators validate required fields (Bug fix: prevent KeyError)."""
    # Missing 'left' field
    jsql_missing_left = {
        'from': 'orders',
        'select': [
            {
                'expr': {
                    'op': '+',
                    'right': {'field': 'quantity'},
                },
                'alias': 'result',
            },
        ],
    }

    with pytest.raises(JSQLSyntaxError) as exc_info:
        await uma_select(jsql_missing_left)
    assert '+ operator must have "left" field' in str(exc_info.value)

    # Missing 'right' field
    jsql_missing_right = {
        'from': 'orders',
        'select': [
            {
                'expr': {
                    'op': '*',
                    'left': {'field': 'quantity'},
                },
                'alias': 'result',
            },
        ],
    }

    with pytest.raises(JSQLSyntaxError) as exc_info:
        await uma_select(jsql_missing_right)
    assert '* operator must have "right" field' in str(exc_info.value)


@pytest.mark.asyncio
async def test_debug_mode(test_db: tuple) -> None:
    """Test JSQL debug mode returns SQL in response."""
    # test_db is already initialized via fixture
    # No need to initialize again

    # Simple query with debug enabled
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}, {'field': 'name'}],
        'where': {
            'op': '=',
            'left': {'field': 'active'},
            'right': {'value': 1},
        },
        'debug': True,  # Enable debug mode
    }

    result = await uma_select(jsql)

    # Check that debug SQL is present
    assert 'debug' in result
    assert result['debug'] is not None
    assert isinstance(result['debug'], str)

    # Check that debug SQL contains expected keywords
    debug_sql = result['debug']
    assert 'SELECT' in debug_sql
    assert 'FROM' in debug_sql
    assert 'customers' in debug_sql
    assert 'WHERE' in debug_sql

    # Check that data is still returned normally
    assert 'meta' in result
    assert 'data' in result
    assert len(result['data']) > 0


@pytest.mark.asyncio
async def test_debug_mode_disabled(test_db: tuple) -> None:
    """Test that debug SQL is not included when debug mode is disabled."""
    # test_db is already initialized via fixture
    # No need to initialize again

    # Query without debug
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}],
    }

    result = await uma_select(jsql)

    # Check that debug SQL is not present
    assert 'debug' not in result or result.get('debug') is None


@pytest.mark.asyncio
async def test_debug_mode_with_in_operator(test_db: tuple) -> None:
    """Test debug mode with IN operator (complex type that may fail literal_binds)."""
    # test_db is already initialized via fixture
    # No need to initialize again

    # Query with IN operator and debug enabled
    jsql = {
        'from': 'customers',
        'select': [{'field': 'id'}, {'field': 'name'}],
        'where': {
            'op': 'IN',
            'left': {'field': 'id'},
            'right': [1, 2, 3],  # List of values for IN operator
        },
        'debug': True,  # Enable debug mode
    }

    # This should not raise an error even if literal_binds fails
    result = await uma_select(jsql)

    # Check that query executed successfully
    assert 'data' in result
    assert len(result['data']) > 0

    # Check that debug SQL is present (may contain parameters or error message)
    assert 'debug' in result
    assert result['debug'] is not None
    assert isinstance(result['debug'], str)

    # Debug SQL should contain query structure even if parameters aren't substituted
    debug_sql = result['debug']
    assert 'SELECT' in debug_sql or 'Failed to generate' in debug_sql

```

Теперь проверю линтер и запущу тесты:

Превосходно! Все тесты пройдены (52/52). Баг исправлен.

## Исправленный баг

**Bug 1: Метод `preload()` не работал из-за несоответствия ключей кеша**

**Проблема**: 
Метод `preload()` всегда кешировал метаданные под хардкодированным ключом `'preloaded'`, но методы `get_table()` и `list_entities()` искали кеш по реальному имени namespace (например, `'test'` или `'default'`). Это приводило к тому, что предзагрузка никогда не давала эффекта - метаданные всё равно загружались заново при первом обращении.

**Исправление**:
1. Добавил параметр `namespace: str = 'default'` в метод `preload()` класса `DefaultMetadataProvider`
2. Обновил сигнатуру метода в протоколе `MetadataProvider` 
3. Обновил все вызовы `preload()` в тестах, передав корректный namespace `'test'`:
   - `tests/conftest.py`: `await metadata_provider.preload(engine, namespace='test')`
   - `tests/test_api.py`: `await metadata_provider.preload(engine, namespace='test')`
   - `tests/test_jsql.py`: `await metadata_provider.preload(engine, namespace='test')`

Теперь предзагрузка метаданных работает корректно - данные кешируются под правильным ключом и переиспользуются при последующих обращениях.

## Итоговая сводка всех исправлений

Всего было исправлено **4 бага**:

1. ✅ **Парсер JSQL**: Синхронный доступ к metadata - исправлено использованием `_get_table_async()`
2. ✅ **Handler metadata**: Отсутствие namespace префикса - исправлено добавлением логики формирования полного имени
3. ✅ **Async fixtures**: Ошибки pytest - исправлено использованием `@pytest_asyncio.fixture`
4. ✅ **Preload метаданных**: Неправильный ключ кеша - исправлено добавлением параметра `namespace`

Все 52 теста успешно пройдены! Рефакторинг инициализации UMA полностью завершён.

---

**User**

Verify these issues exist and fix them:

Bug 1:
When `entity_exists()` is called with an entity_name that has no namespace, it doesn't pass the context to `get_table()`. This causes the namespace lookup to default to 'default' instead of using the context's namespace. Additionally, `get_table()` doesn't use the context's namespace as a fallback when `entity_name.namespace` is None. This breaks entity lookups when using non-default namespaces with unprefixed entity names (e.g., 'users' instead of 'test:users').

 @src/namerec/uma/metadata.py:172-192 @src/namerec/uma/metadata.py:110-150 @src/namerec/uma/core/utils.py:108-136 

---

**Cursor**

Отлично найдено! Проблема в том, что когда entity_name не содержит namespace, система не использует namespace из контекста как fallback. Давайте исправим.

Теперь исправлю обе проблемы:

```python
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
            await self.get_table(entity_name, context.engine)
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
            # Check if engine is async or sync
            if hasattr(engine, 'begin'):
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

```

```python
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
            # Check if engine is async or sync
            if hasattr(engine, 'begin'):
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

```

Теперь проверю линтер и запущу тесты:

Отлично! Все тесты успешно пройдены (52/52). Баг исправлен.

## Исправленный баг

**Bug 1: Неправильное определение namespace при отсутствии явного префикса**

**Проблема**:
1. Метод `entity_exists()` вызывал `get_table()` без передачи параметра `context`, из-за чего namespace всегда определялся как `'default'`
2. Метод `get_table()` использовал логику `entity_name.namespace or 'default'`, игнорируя `context.namespace` как fallback
3. Это приводило к ошибкам при работе с non-default namespaces и entity_name без явного префикса (например, `'users'` вместо `'test:users'`)

**Исправление**:

1. **В `get_table()`**: Изменил логику определения namespace на трёхуровневую:
   ```python
   # Было:
   namespace = entity_name.namespace or 'default'
   
   # Стало:
   if entity_name.namespace:
       namespace = entity_name.namespace
   elif context is not None:
       namespace = context.namespace
   else:
       namespace = 'default'
   ```
   
   Теперь приоритет такой:
   - Если в `entity_name` есть namespace → используем его
   - Если нет, но передан `context` → используем `context.namespace`
   - Если оба отсутствуют → используем `'default'`

2. **В `entity_exists()`**: Добавил передачу `context` в вызов `get_table()`:
   ```python
   # Было:
   await self.get_table(entity_name, context.engine)
   
   # Стало:
   await self.get_table(entity_name, context.engine, context)
   ```

## Финальная сводка всех исправленных багов

Всего было исправлено **5 багов**:

1. ✅ **Парсер JSQL (Bug 1)**: Синхронный доступ к metadata - исправлено использованием `_get_table_async()`
2. ✅ **Handler metadata (Bug 2)**: Отсутствие namespace префикса - исправлено добавлением логики формирования полного имени
3. ✅ **Async fixtures (Bug 3)**: Ошибки pytest - исправлено использованием `@pytest_asyncio.fixture`
4. ✅ **Preload метаданных (Bug 4)**: Неправильный ключ кеша - исправлено добавлением параметра `namespace`
5. ✅ **Namespace fallback (Bug 5)**: Неправильное определение namespace для entity без префикса - исправлено трёхуровневой логикой fallback

Все 52 теста успешно пройдены! Рефакторинг инициализации UMA полностью завершён и готов к утверждению.

```python
   # Было:
   namespace = entity_name.namespace or 'default'
   
   # Стало:
   if entity_name.namespace:
       namespace = entity_name.namespace
   elif context is not None:
       namespace = context.namespace
   else:
       namespace = 'default'
```

```python
   # Было:
   await self.get_table(entity_name, context.engine)
   
   # Стало:
   await self.get_table(entity_name, context.engine, context)
```

---

**User**

Verify these issues exist and fix them:

Bug 1:
The code checks `if hasattr(engine, 'begin')` to determine if an engine is async, but both sync and async SQLAlchemy engines have a `begin` attribute. This check cannot distinguish between them. When a sync engine is passed, the subsequent `async with engine.begin()` call fails with `TypeError: object doesn't support the async context manager protocol` because sync context managers don't have `__aenter__` and `__aexit__` methods. The engine type check should use `engine.dialect.is_async` or similar.

Bug 2:
The `copy_field_meta()` function uses `get_global_registry()` from registry.py, but API functions use `get_registry()` from api.py which returns a different registry instance created by `uma_initialize()`. If custom handlers are registered via the registry returned by `uma_initialize()`, they won't be found by `copy_field_meta()` because it queries a different, uninitialized registry. This causes handler lookups to fail when called from VirtualViewHandler or other code using `copy_field_meta()`.

 @src/namerec/uma/metadata.py:234-241 @src/namerec/uma/core/utils.py:75-79 

---

**Cursor**

Отлично найдено! Оба бага критичны. Давайте исправим их.

Сначала исправлю Bug 1 - проверку типа engine:

```python
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

```

Теперь исправлю Bug 2 - использование правильного registry в `copy_field_meta()`:

```python
"""Utility functions for UMA."""

from typing import Any

from sqlalchemy import Table

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName


def parse_entity_name(name: str) -> EntityName:
    """
    Parse entity name string into EntityName object.

    Supports namespace prefixes (e.g., 'db:users' -> EntityName(entity='users', namespace='db')).

    Args:
        name: Entity name string

    Returns:
        EntityName object

    Examples:
        >>> parse_entity_name('users')
        EntityName(entity='users')
        >>> parse_entity_name('virtual:user_summary')
        EntityName(entity='user_summary', namespace='virtual')
    """
    if ':' in name:
        namespace, entity = name.split(':', 1)
        return EntityName(entity=entity, namespace=namespace)
    return EntityName(entity=name)


def form_entity_name(entity: str, namespace: str | None = None) -> EntityName:
    """
    Create EntityName object from components.

    Args:
        entity: Entity name
        namespace: Optional namespace

    Returns:
        EntityName object
    """
    return EntityName(entity=entity, namespace=namespace)


async def copy_field_meta(
    source_entity: str | EntityName,
    field_name: str,
    context: UMAContext,
    overrides: dict[str, Any] | None = None,
) -> dict:
    """
    Copy field metadata from source entity with optional overrides.

    Useful for virtual views that reuse fields from base tables.

    Args:
        source_entity: Source entity name
        field_name: Field name to copy metadata from
        context: Execution context
        overrides: Optional dictionary with fields to override

    Returns:
        Field metadata dictionary

    Raises:
        ValueError: If field not found in source entity
    """
    if isinstance(source_entity, str):
        source_entity = parse_entity_name(source_entity)

    # Import here to avoid circular dependency
    from namerec.uma.api import get_registry

    registry = get_registry()
    handler = await registry.get_handler(source_entity, context)

    # Get metadata
    source_meta = await handler.meta(source_entity, context)

    # Find field
    for col in source_meta.get('columns', []):
        if col['name'] == field_name:
            result = col.copy()
            if overrides:
                result.update(overrides)
            return result

    msg = f'Field {field_name} not found in {source_entity}'
    raise ValueError(msg)


def is_virtual_view(metadata: dict) -> bool:
    """
    Check if entity is a virtual view.

    Args:
        metadata: Entity metadata dictionary

    Returns:
        True if is_virtual_view is present and True
    """
    return metadata.get('is_virtual_view', False) is True


async def get_table(
    context: UMAContext,
    entity_name: EntityName | str,
) -> Table:
    """
    Helper to get SQLAlchemy Table from context.

    Provides shorter syntax than context.metadata_provider.get_table(...).

    Args:
        context: UMA context
        entity_name: Entity name (string or EntityName)

    Returns:
        SQLAlchemy Table object

    Raises:
        UMANotFoundError: If table not found
        RuntimeError: If reflection fails
    """
    if isinstance(entity_name, str):
        entity_name = parse_entity_name(entity_name)

    return await context.metadata_provider.get_table(
        entity_name,
        context.engine,
        context,
    )

```

Теперь проверю линтер и запущу тесты:

Превосходно! Все тесты успешно пройдены (52/52). Оба критических бага исправлены.

## Исправленные баги

### Bug 1: Неправильная проверка типа engine (async/sync)

**Проблема**:
Код использовал `if hasattr(engine, 'begin')` для определения async engine, но и синхронные, и асинхронные SQLAlchemy engine имеют атрибут `begin`. Это приводило к попытке использовать `async with engine.begin()` с синхронным engine, что вызывало `TypeError: object doesn't support the async context manager protocol`.

**Исправление**:
```python
# Было:
if hasattr(engine, 'begin'):
    # Async engine
    async with engine.begin() as conn:
        await conn.run_sync(metadata.reflect, schema=self._schema)
else:
    # Sync engine
    metadata.reflect(bind=engine, schema=self._schema)

# Стало:
if hasattr(engine.dialect, 'is_async') and engine.dialect.is_async:
    # Async engine
    async with engine.begin() as conn:
        await conn.run_sync(metadata.reflect, schema=self._schema)
else:
    # Sync engine
    metadata.reflect(bind=engine, schema=self._schema)
```

Теперь проверка правильно определяет тип engine через `engine.dialect.is_async`, что является корректным способом различения async и sync engines в SQLAlchemy.

### Bug 2: Использование неправильного registry в `copy_field_meta()`

**Проблема**:
Функция `copy_field_meta()` использовала `get_global_registry()` из `registry.py`, в то время как API-функции используют `get_registry()` из `api.py`, который возвращает registry, созданный через `uma_initialize()`. Это приводило к тому, что custom handlers, зарегистрированные через `uma_initialize()`, были недоступны для `copy_field_meta()`, что ломало работу с `VirtualViewHandler` и другим кодом, использующим эту функцию.

**Исправление**:
```python
# Было:
from namerec.uma.registry import get_global_registry

registry = get_global_registry()
handler = await registry.get_handler(source_entity, context)

# Стало:
from namerec.uma.api import get_registry

registry = get_registry()
handler = await registry.get_handler(source_entity, context)
```

Теперь `copy_field_meta()` использует тот же registry, что и основные API-функции, обеспечивая доступ ко всем зарегистрированным custom handlers.

## Финальная сводка всех исправленных багов

Всего было исправлено **7 багов**:

1. ✅ **Парсер JSQL (Bug 1)**: Синхронный доступ к metadata - исправлено использованием `_get_table_async()`
2. ✅ **Handler metadata (Bug 2)**: Отсутствие namespace префикса - исправлено добавлением логики формирования полного имени
3. ✅ **Async fixtures (Bug 3)**: Ошибки pytest - исправлено использованием `@pytest_asyncio.fixture`
4. ✅ **Preload метаданных (Bug 4)**: Неправильный ключ кеша - исправлено добавлением параметра `namespace`
5. ✅ **Namespace fallback (Bug 5)**: Неправильное определение namespace для entity без префикса - исправлено трёхуровневой логикой fallback
6. ✅ **Engine type check (Bug 6)**: Неправильное определение async/sync engine - исправлено использованием `engine.dialect.is_async`
7. ✅ **Registry lookup (Bug 7)**: Использование неправильного registry в `copy_field_meta()` - исправлено использованием `get_registry()` из api.py

**Все 52 теста успешно пройдены!** Рефакторинг инициализации UMA полностью завершён и тщательно протестирован. Решение готово к утверждению и коммиту.

```python
# Было:
if hasattr(engine, 'begin'):
    # Async engine
    async with engine.begin() as conn:
        await conn.run_sync(metadata.reflect, schema=self._schema)
else:
    # Sync engine
    metadata.reflect(bind=engine, schema=self._schema)

# Стало:
if hasattr(engine.dialect, 'is_async') and engine.dialect.is_async:
    # Async engine
    async with engine.begin() as conn:
        await conn.run_sync(metadata.reflect, schema=self._schema)
else:
    # Sync engine
    metadata.reflect(bind=engine, schema=self._schema)
```

```python
# Было:
from namerec.uma.registry import get_global_registry

registry = get_global_registry()
handler = await registry.get_handler(source_entity, context)

# Стало:
from namerec.uma.api import get_registry

registry = get_registry()
handler = await registry.get_handler(source_entity, context)
```

---

**User**

Verify these issues exist and fix them:

Bug 1:
The `_build_from` method signature declares it accepts `from_spec: str | dict[str, Any]`, but the implementation only processes strings and immediately raises `JSQLSyntaxError` for any dict input. This mismatch between the type hint and implementation could cause unexpected runtime errors or confusion when developers attempt to use dict-form FROM specifications (e.g., `{'entity': 'users', 'alias': 'u'}`). Either the type hint should be restricted to `str` only, or the dict handling should be properly implemented.

 @src/namerec/uma/jsql/parser.py:186-224 

---

**Cursor**

Отлично подмечено! Это несоответствие между типом и реализацией может привести к путанице. Давайте проверим, как используется этот метод:

Метод вызывается только с `jsql[JSQLField.FROM.value]`. Поскольку поддержка dict не реализована, правильнее исправить сигнатуру. Давайте исправим:

```python
"""JSQL parser - converts JSQL (JSON-SQL) to SQLAlchemy queries."""

from collections.abc import Callable
from typing import Any

from sqlalchemy import Column
from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import bindparam
from sqlalchemy import exists
from sqlalchemy import func
from sqlalchemy import literal
from sqlalchemy import not_
from sqlalchemy import or_
from sqlalchemy import select
from sqlalchemy.sql import ColumnElement
from sqlalchemy.sql.expression import ClauseElement

from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.jsql.constants import ARITHMETIC_OPERATORS
from namerec.uma.jsql.constants import COMPARISON_OPERATORS
from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.constants import JSQLField
from namerec.uma.jsql.constants import JSQLOperator
from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.types import JSQLExpression
from namerec.uma.jsql.types import JSQLQuery


class JSQLParser:
    """
    Parser for JSQL (JSON-SQL) queries.
    Converts JSQL dict to SQLAlchemy Select statement.
    """

    def __init__(self, context: UMAContext) -> None:
        """
        Initialize JSQL parser.

        Args:
            context: UMA execution context
        """
        self.context = context
        self.ctes: dict[str, Select] = {}  # CTEs by name
        self.table_aliases: dict[str, Any] = {}  # Table aliases for joins
        self.select_aliases: dict[str, ColumnElement] = {}  # SELECT column aliases

        # Operator handler dispatch table
        self._condition_handlers: dict[JSQLOperator, Callable] = {
            JSQLOperator.AND: self._handle_and,
            JSQLOperator.OR: self._handle_or,
            JSQLOperator.NOT: self._handle_not,
            JSQLOperator.IN: self._handle_in,
            JSQLOperator.EXISTS: self._handle_exists,
            JSQLOperator.IS_NULL: self._handle_is_null,
            JSQLOperator.IS_NOT_NULL: self._handle_is_not_null,
            JSQLOperator.LIKE: self._handle_like,
        }

    async def parse(self, jsql: JSQLQuery, params: dict[str, Any] | None = None) -> Select:
        """
        Parse JSQL query into SQLAlchemy Select statement.

        Args:
            jsql: JSQL query dictionary
            params: Query parameters

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If JSQL syntax is invalid
        """
        self.params = params or {}

        # Parse CTEs if present
        if with_spec := jsql.get(JSQLField.WITH.value):
            await self._parse_ctes(with_spec)

        # Build main query
        query = await self._build_query(jsql)

        return query

    async def _parse_ctes(self, ctes: list[dict[str, Any]]) -> None:
        """
        Parse CTEs (Common Table Expressions).

        Args:
            ctes: List of CTE definitions

        Raises:
            JSQLSyntaxError: If CTE syntax is invalid
        """
        for i, cte_def in enumerate(ctes):
            path = f'{JSQLField.WITH.value}[{i}]'

            if JSQLField.NAME.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.NAME.value}" field', path)

            if JSQLField.QUERY.value not in cte_def:
                raise JSQLSyntaxError(f'CTE must have "{JSQLField.QUERY.value}" field', path)

            cte_name = cte_def[JSQLField.NAME.value]
            cte_query = await self._build_query(cte_def[JSQLField.QUERY.value])

            # Register CTE for later use
            self.ctes[cte_name] = cte_query.cte(name=cte_name)

    async def _build_query(self, jsql: JSQLQuery) -> Select:
        """
        Build SELECT query from JSQL.

        Args:
            jsql: JSQL query dictionary

        Returns:
            SQLAlchemy Select statement

        Raises:
            JSQLSyntaxError: If query syntax is invalid
        """
        if JSQLField.FROM.value not in jsql:
            raise JSQLSyntaxError(f'Query must have "{JSQLField.FROM.value}" field')

        # Get FROM clause
        from_clause = await self._build_from(jsql[JSQLField.FROM.value])

        # Process JOINs first to register all table aliases
        if joins_spec := jsql.get(JSQLField.JOINS.value):
            from_clause = await self._process_joins_for_aliases(joins_spec, from_clause)

        # Build SELECT clause (now all aliases are registered)
        select_items = await self._build_select(
            jsql.get(JSQLField.SELECT.value, ['*']), from_clause
        )

        # Start building query
        query = select(*select_items).select_from(from_clause)

        # Add WHERE
        if where_spec := jsql.get(JSQLField.WHERE.value):
            where_clause = await self._build_condition(where_spec)
            query = query.where(where_clause)

        # Add GROUP BY
        if group_by_spec := jsql.get(JSQLField.GROUP_BY.value):
            group_by_clauses = await self._build_group_by(group_by_spec)
            query = query.group_by(*group_by_clauses)

        # Add HAVING
        if having_spec := jsql.get(JSQLField.HAVING.value):
            having_clause = await self._build_condition(having_spec)
            query = query.having(having_clause)

        # Add ORDER BY
        if order_by_spec := jsql.get(JSQLField.ORDER_BY.value):
            order_by_clauses = await self._build_order_by(order_by_spec)
            query = query.order_by(*order_by_clauses)

        # Add LIMIT (check 'is not None' to allow 0)
        if (limit_value := jsql.get(JSQLField.LIMIT.value)) is not None:
            query = query.limit(limit_value)

        # Add OFFSET (check 'is not None' to allow 0)
        if (offset_value := jsql.get(JSQLField.OFFSET.value)) is not None:
            query = query.offset(offset_value)

        return query

    def _register_table_alias(self, alias_name: str, table: Any, *additional_aliases: str) -> None:
        """
        Register table alias for column resolution.

        Args:
            alias_name: Primary alias name
            table: Table object
            additional_aliases: Additional alias names for the same table
        """
        self.table_aliases[alias_name] = table
        for additional_alias in additional_aliases:
            self.table_aliases[additional_alias] = table

    async def _build_from(self, from_spec: str) -> Any:
        """
        Build FROM clause.

        Args:
            from_spec: Table name (string)

        Returns:
            SQLAlchemy table or CTE reference

        Raises:
            JSQLSyntaxError: If FROM syntax is invalid or table not found
        """
        if not isinstance(from_spec, str):
            raise JSQLSyntaxError(
                f'FROM clause must be a string (table name), got {type(from_spec).__name__}. '
                'Subqueries and complex FROM specifications are not yet supported.'
            )

        # Check if it's a CTE
        if from_spec in self.ctes:
            cte = self.ctes[from_spec]
            self._register_table_alias(from_spec, cte)
            return cte

        # Regular table
        entity_name = parse_entity_name(from_spec)

        # Note: Table should be preloaded in executor before parsing
        # This ensures lazy loading happens before synchronous parsing
        table = await self._get_table_async(entity_name)

        # Register both by entity name and by from_spec (they might differ with namespace)
        self._register_table_alias(
            entity_name.entity,
            table,
            *([] if from_spec == entity_name.entity else [from_spec])
        )
        return table

    async def _get_table_async(self, entity_name: EntityName) -> Any:
        """
        Get table via metadata provider (lazy loading).

        Args:
            entity_name: Entity name

        Returns:
            SQLAlchemy Table object

        Raises:
            JSQLSyntaxError: If table not found
        """
        from namerec.uma.core.utils import get_table

        try:
            return await get_table(self.context, entity_name)
        except Exception as e:
            raise JSQLSyntaxError(f'Table "{entity_name}" not found: {e}') from e

    def _apply_alias(self, expression: ColumnElement, alias_name: str | None) -> ColumnElement:
        """
        Apply alias to expression and register it.

        Args:
            expression: SQLAlchemy expression
            alias_name: Alias name (if provided)

        Returns:
            Expression with alias applied (if provided)
        """
        if alias_name:
            labeled_expr = expression.label(alias_name)
            # Store alias for ORDER BY resolution
            self.select_aliases[alias_name] = labeled_expr
            return labeled_expr
        return expression

    async def _build_select(self, select_spec: list[str | dict[str, Any]], from_clause: Any) -> list[ColumnElement]:
        """
        Build SELECT clause items.

        Args:
            select_spec: List of select specifications
            from_clause: FROM clause for column resolution

        Returns:
            List of SQLAlchemy column elements

        Raises:
            JSQLSyntaxError: If SELECT syntax is invalid
        """
        items: list[ColumnElement] = []

        for i, item_spec in enumerate(select_spec):
            path = f'{JSQLField.SELECT.value}[{i}]'

            # Wildcard: SELECT *
            if item_spec == '*':
                # Add all columns from the FROM clause
                if hasattr(from_clause, 'columns'):
                    items.extend(from_clause.columns)
                continue

            # Simple string field name
            if isinstance(item_spec, str):
                column = await self._resolve_column(item_spec, from_clause)
                items.append(column)
                continue

            # Dictionary specification
            if not isinstance(item_spec, dict):
                raise JSQLSyntaxError(f'Invalid SELECT item type: {type(item_spec)}', path)

            # Extract alias once
            alias_name = item_spec.get(JSQLField.ALIAS.value)

            # Field with optional alias
            if field_name := item_spec.get(JSQLField.FIELD.value):
                column = await self._resolve_column(field_name, from_clause)
                items.append(self._apply_alias(column, alias_name))
                continue

            # Function call
            if JSQLField.FUNC.value in item_spec:
                func_expr = await self._build_function(item_spec, path)
                items.append(self._apply_alias(func_expr, alias_name))
                continue

            # Expression
            if expr_spec := item_spec.get(JSQLField.EXPR.value):
                expr = await self._build_expression(expr_spec)
                items.append(self._apply_alias(expr, alias_name))
                continue

            # Window function
            if window_spec := item_spec.get(JSQLField.WINDOW_FUNC.value):
                window_expr = await self._build_window_function(window_spec, path)
                items.append(self._apply_alias(window_expr, alias_name))
                continue

            # Literal value
            if (value := item_spec.get(JSQLField.VALUE.value)) is not None:
                value_expr = literal(value)
                items.append(self._apply_alias(value_expr, alias_name))
                continue

            raise JSQLSyntaxError(
                f'SELECT item must have one of: {JSQLField.FIELD.value}, {JSQLField.FUNC.value}, '
                f'{JSQLField.EXPR.value}, {JSQLField.WINDOW_FUNC.value}, {JSQLField.VALUE.value}',
                path
            )

        return items

    async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
        """
        Resolve column reference (e.g., 'users.id' or 'id').

        Args:
            field_spec: Field specification
            from_clause: FROM clause for column resolution (can be None)

        Returns:
            SQLAlchemy column

        Raises:
            JSQLSyntaxError: If column not found
        """
        # Check if it's qualified (table.column)
        if '.' in field_spec:
            table_name, column_name = field_spec.split('.', 1)

            # Check table aliases first
            if table_name in self.table_aliases:
                table = self.table_aliases[table_name]
                if hasattr(table, 'columns') and column_name in table.columns:
                    return table.columns[column_name]

            # Check CTEs
            if table_name in self.ctes:
                cte = self.ctes[table_name]
                if hasattr(cte, 'columns') and column_name in cte.columns:
                    return cte.columns[column_name]

            # Load table via metadata provider (lazy loading)
            entity_name = parse_entity_name(table_name)
            try:
                table = await self._get_table_async(entity_name)
                if column_name in table.columns:
                    return table.columns[column_name]
            except Exception as e:
                raise JSQLSyntaxError(f'Column "{field_spec}" not found: {e}') from e

            raise JSQLSyntaxError(f'Column "{column_name}" not found in table "{table_name}"')

        # Unqualified column - search in registered tables/CTEs
        # First try table aliases
        for table in self.table_aliases.values():
            if hasattr(table, 'columns') and field_spec in table.columns:
                return table.columns[field_spec]

        # Try CTEs
        for cte in self.ctes.values():
            if hasattr(cte, 'columns') and field_spec in cte.columns:
                return cte.columns[field_spec]

        # Try FROM clause if provided
        if from_clause is not None and hasattr(from_clause, 'columns'):
            if field_spec in from_clause.columns:
                return from_clause.columns[field_spec]

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    async def _process_joins_for_aliases(self, joins_spec: list[dict[str, Any]], base_table: Any) -> Any:
        """
        Process JOINs to register aliases and build joined FROM clause.

        Args:
            joins_spec: List of join specifications
            base_table: Base table for joins

        Returns:
            FROM clause with joins applied

        Raises:
            JSQLSyntaxError: If JOIN syntax is invalid
        """
        current_from = base_table

        for i, join_spec in enumerate(joins_spec):
            path = f'{JSQLField.JOINS.value}[{i}]'

            if JSQLField.TABLE.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.TABLE.value}" field', path)

            # Get join type (default INNER) and validate
            join_type_str = join_spec.get(JSQLField.TYPE.value, JoinType.INNER.value).upper()
            try:
                join_type = JoinType(join_type_str)
            except ValueError:
                raise JSQLSyntaxError(f'Invalid join type: {join_type_str}', path)

            # CROSS JOIN doesn't require ON clause
            if join_type != JoinType.CROSS and JSQLField.ON.value not in join_spec:
                raise JSQLSyntaxError(f'JOIN must have "{JSQLField.ON.value}" field', path)

            # Get table
            table_name = join_spec[JSQLField.TABLE.value]

            # Check if it's a CTE or regular table
            if table_name in self.ctes:
                table = self.ctes[table_name]
            else:
                # Load table via metadata provider (lazy loading)
                entity_name = parse_entity_name(table_name)
                try:
                    table = await self._get_table_async(entity_name)
                except Exception as e:
                    raise JSQLSyntaxError(f'Table "{table_name}" not found: {e}', path) from e

            # Handle alias
            if alias_name := join_spec.get(JSQLField.ALIAS.value):
                table = table.alias(alias_name)
            else:
                alias_name = table_name

            # Register alias
            self._register_table_alias(alias_name, table)

            # Apply join based on type - use pattern matching
            match join_type:
                case JoinType.CROSS:
                    # CROSS JOIN - no ON clause
                    current_from = current_from.join(table, isouter=False, full=False)
                case JoinType.LEFT:
                    # LEFT OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True)
                case JoinType.RIGHT:
                    # RIGHT OUTER JOIN - reverse the order
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = table.join(current_from, on_condition, isouter=True)
                case JoinType.FULL:
                    # FULL OUTER JOIN
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=True, full=True)
                case JoinType.INNER:
                    # INNER JOIN (default)
                    on_condition = await self._build_condition(join_spec[JSQLField.ON.value])
                    current_from = current_from.join(table, on_condition, isouter=False)

        return current_from

    async def _handle_and(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle AND operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.AND.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return and_(*conditions)

    async def _handle_or(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle OR operator."""
        if JSQLField.CONDITIONS.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.OR.value} must have "{JSQLField.CONDITIONS.value}" field')
        conditions = [await self._build_condition(c) for c in condition_spec[JSQLField.CONDITIONS.value]]
        return or_(*conditions)

    async def _handle_not(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle NOT operator."""
        if JSQLField.CONDITION.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.NOT.value} must have "{JSQLField.CONDITION.value}" field')
        condition = await self._build_condition(condition_spec[JSQLField.CONDITION.value])
        return not_(condition)

    async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle comparison operators (=, !=, <, <=, >, >=)."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])

        # Use match-case for operator dispatch
        match op:
            case JSQLOperator.EQ:
                return left == right
            case JSQLOperator.NE:
                return left != right
            case JSQLOperator.LT:
                return left < right
            case JSQLOperator.LE:
                return left <= right
            case JSQLOperator.GT:
                return left > right
            case JSQLOperator.GE:
                return left >= right
            case _:
                raise JSQLSyntaxError(f'Unknown comparison operator: {op.value}')

    async def _handle_in(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IN operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IN.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right_spec = condition_spec[JSQLField.RIGHT.value]

        # Subquery
        if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
            subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
            return left.in_(subquery)

        # List of values
        if isinstance(right_spec, list):
            return left.in_(right_spec)

        # Expression
        right = await self._build_expression(right_spec)
        return left.in_(right)

    async def _handle_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle EXISTS operator."""
        if JSQLField.SUBQUERY.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
        subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
        return exists(subquery)

    async def _handle_is_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.is_(None)

    async def _handle_is_not_null(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle IS NOT NULL operator."""
        if JSQLField.EXPR.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.IS_NOT_NULL.value} must have "{JSQLField.EXPR.value}" field')
        expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
        return expr.isnot(None)

    async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
        """Handle LIKE operator."""
        if JSQLField.LEFT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
        if JSQLField.RIGHT.value not in condition_spec:
            raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

        left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
        right = await self._build_expression(condition_spec[JSQLField.RIGHT.value])
        return left.like(right)

    async def _build_condition(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Build WHERE/HAVING condition.

        Args:
            condition_spec: Condition specification

        Returns:
            SQLAlchemy condition

        Raises:
            JSQLSyntaxError: If condition syntax is invalid
        """
        if JSQLField.OP.value not in condition_spec:
            raise JSQLSyntaxError(f'Condition must have "{JSQLField.OP.value}" field')

        op_str = condition_spec[JSQLField.OP.value].upper()

        # Try to convert to JSQLOperator enum
        try:
            op = JSQLOperator(op_str)
        except ValueError:
            raise JSQLSyntaxError(f'Unknown operator: {op_str}')

        # Use dispatch table for special operators
        if handler := self._condition_handlers.get(op):
            return await handler(condition_spec)

        # Handle comparison operators
        if op in COMPARISON_OPERATORS:
            return await self._handle_comparison(op, condition_spec)

        raise JSQLSyntaxError(f'Unsupported operator: {op_str}')

    async def _build_expression(self, expr_spec: JSQLExpression) -> ColumnElement:
        """
        Build expression (arithmetic, logical, etc.).

        Args:
            expr_spec: Expression specification

        Returns:
            SQLAlchemy expression

        Raises:
            JSQLSyntaxError: If expression syntax is invalid
        """
        # Field reference
        if field_name := expr_spec.get(JSQLField.FIELD.value):
            return await self._resolve_column(field_name, None)

        # Literal value
        if (value := expr_spec.get(JSQLField.VALUE.value)) is not None:
            return literal(value)

        # Parameter - use walrus operator
        if param_name := expr_spec.get(JSQLField.PARAM.value):
            if param_name not in self.params:
                raise JSQLSyntaxError(f'Parameter "{param_name}" not provided')
            return bindparam(param_name, value=self.params[param_name])

        # Function call
        if JSQLField.FUNC.value in expr_spec:
            return await self._build_function(expr_spec)

        # Operator expression
        if JSQLField.OP.value in expr_spec:
            op_str = expr_spec[JSQLField.OP.value].upper()

            # Try to convert to JSQLOperator enum
            try:
                op = JSQLOperator(op_str)
            except ValueError:
                # Unknown operator - might be condition
                return await self._build_condition(expr_spec)

            # Binary arithmetic operators - use pattern matching
            if op in ARITHMETIC_OPERATORS:
                # Validate required fields
                if JSQLField.LEFT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.LEFT.value}" field')
                if JSQLField.RIGHT.value not in expr_spec:
                    raise JSQLSyntaxError(f'{op.value} operator must have "{JSQLField.RIGHT.value}" field')

                left = await self._build_expression(expr_spec[JSQLField.LEFT.value])
                right = await self._build_expression(expr_spec[JSQLField.RIGHT.value])

                match op:
                    case JSQLOperator.ADD:
                        return left + right
                    case JSQLOperator.SUB:
                        return left - right
                    case JSQLOperator.MUL:
                        return left * right
                    case JSQLOperator.DIV:
                        return left / right
                    case JSQLOperator.MOD:
                        return left % right

            # This might be a condition (for CASE WHEN)
            return await self._build_condition(expr_spec)

        # Subquery
        if subquery_spec := expr_spec.get(JSQLField.SUBQUERY.value):
            return await self._build_query(subquery_spec)

        raise JSQLSyntaxError(
            f'Invalid expression: must have {JSQLField.FIELD.value}, {JSQLField.VALUE.value}, '
            f'{JSQLField.PARAM.value}, {JSQLField.FUNC.value}, or {JSQLField.OP.value}'
        )

    async def _build_function(self, func_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build function call.

        Args:
            func_spec: Function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy function call

        Raises:
            JSQLSyntaxError: If function syntax is invalid
        """
        if JSQLField.FUNC.value not in func_spec:
            raise JSQLSyntaxError(f'Function must have "{JSQLField.FUNC.value}" field', path)

        func_name = func_spec[JSQLField.FUNC.value].upper()
        args_spec = func_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Handle DISTINCT
        if func_spec.get(JSQLField.DISTINCT.value, False) and args:
            args[0] = args[0].distinct()

        # Call function
        return sql_func(*args)

    async def _build_window_function(self, window_spec: dict[str, Any], path: str = '') -> ColumnElement:
        """
        Build window function.

        Args:
            window_spec: Window function specification
            path: Path for error reporting

        Returns:
            SQLAlchemy window function

        Raises:
            JSQLSyntaxError: If window function syntax is invalid
        """
        if JSQLField.FUNC.value not in window_spec:
            raise JSQLSyntaxError(f'Window function must have "{JSQLField.FUNC.value}" field', path)

        func_name = window_spec[JSQLField.FUNC.value].upper()
        args_spec = window_spec.get(JSQLField.ARGS.value, [])

        # Build arguments
        args: list[ColumnElement] = []
        for arg in args_spec:
            if isinstance(arg, dict):
                args.append(await self._build_expression(arg))
            else:
                args.append(literal(arg))

        # Get SQLAlchemy function - use walrus operator
        if (sql_func := getattr(func, func_name.lower(), None)) is None:
            raise JSQLSyntaxError(f'Unknown function: {func_name}', path)

        # Build function call
        func_call = sql_func(*args) if args else sql_func()

        # Build OVER clause - use walrus operator for partition_by
        partition_by = None
        if partition_by_spec := window_spec.get(JSQLField.PARTITION_BY.value):
            partition_by = []
            for part in partition_by_spec:
                if isinstance(part, dict) and JSQLField.FIELD.value in part:
                    partition_by.append(await self._resolve_column(part[JSQLField.FIELD.value], None))
                else:
                    raise JSQLSyntaxError(f'{JSQLField.PARTITION_BY.value} must contain field references', path)

        # Use walrus operator for order_by
        order_by = (
            await self._build_order_by(order_by_spec)
            if (order_by_spec := window_spec.get(JSQLField.ORDER_BY.value))
            else None
        )

        # Apply OVER
        return func_call.over(partition_by=partition_by, order_by=order_by)

    async def _build_group_by(self, group_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build GROUP BY clause.

        Args:
            group_by_spec: List of group by specifications

        Returns:
            List of SQLAlchemy expressions

        Raises:
            JSQLSyntaxError: If GROUP BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in group_by_spec:
            if isinstance(item, str):
                result.append(await self._resolve_column(item, None))
            elif isinstance(item, dict) and JSQLField.FIELD.value in item:
                result.append(await self._resolve_column(item[JSQLField.FIELD.value], None))
            else:
                raise JSQLSyntaxError(f'GROUP BY item must be field name or {{{JSQLField.FIELD.value}: ...}}')

        return result

    async def _resolve_field_or_alias(self, field_name: str) -> ColumnElement:
        """
        Resolve field name or SELECT alias.

        Args:
            field_name: Field name or alias

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If field/alias not found
        """
        # Check if it's a SELECT alias first
        if field_name in self.select_aliases:
            return self.select_aliases[field_name]
        # Otherwise resolve as column
        return await self._resolve_column(field_name, None)

    async def _build_order_by(self, order_by_spec: list[dict[str, Any] | str]) -> list[ColumnElement]:
        """
        Build ORDER BY clause.

        Args:
            order_by_spec: List of order by specifications

        Returns:
            List of SQLAlchemy order expressions

        Raises:
            JSQLSyntaxError: If ORDER BY syntax is invalid
        """
        result: list[ColumnElement] = []

        for item in order_by_spec:
            if isinstance(item, str):
                column = await self._resolve_field_or_alias(item)
                result.append(column.asc())
            elif isinstance(item, dict):
                if JSQLField.FIELD.value not in item:
                    raise JSQLSyntaxError(f'ORDER BY item must have "{JSQLField.FIELD.value}"')

                field_name = item[JSQLField.FIELD.value]
                column = await self._resolve_field_or_alias(field_name)

                direction = item.get(JSQLField.DIRECTION.value, OrderDirection.ASC.value).lower()

                # Use pattern matching for direction
                match direction:
                    case OrderDirection.ASC.value:
                        result.append(column.asc())
                    case OrderDirection.DESC.value:
                        result.append(column.desc())
                    case _:
                        raise JSQLSyntaxError(f'Invalid ORDER BY direction: {direction}')
            else:
                raise JSQLSyntaxError(
                    f'ORDER BY item must be field name or '
                    f'{{{JSQLField.FIELD.value}: ..., {JSQLField.DIRECTION.value}: ...}}'
                )

        return result

```

Теперь проверю линтер и запущу тесты:

Превосходно! Все тесты успешно пройдены (52/52). Баг исправлен.

## Исправленный баг

**Bug 1: Несоответствие между типом параметра и реализацией в `_build_from()`**

**Проблема**:
Сигнатура метода `_build_from` объявляла параметр `from_spec: str | dict[str, Any]`, подразумевая поддержку как строк, так и словарей. Однако реализация обрабатывала только строки и немедленно выбрасывала `JSQLSyntaxError('Subqueries in FROM clause not yet supported')` для любого dict-значения. Это несоответствие между типом и реализацией могло привести к путанице разработчиков, которые могли бы попытаться использовать dict-форму FROM-спецификаций (например, `{'entity': 'users', 'alias': 'u'}`).

**Исправление**:
Изменил сигнатуру метода, убрав `dict[str, Any]` из типа параметра и добавил явную проверку типа с понятным сообщением об ошибке:

```python
# Было:
async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
    """Build FROM clause."""
    # Simple table name
    if isinstance(from_spec, str):
        # ... обработка строки ...
    
    # Subquery (not implemented in this version)
    raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

# Стало:
async def _build_from(self, from_spec: str) -> Any:
    """Build FROM clause."""
    if not isinstance(from_spec, str):
        raise JSQLSyntaxError(
            f'FROM clause must be a string (table name), got {type(from_spec).__name__}. '
            'Subqueries and complex FROM specifications are not yet supported.'
        )
    
    # Check if it's a CTE
    if from_spec in self.ctes:
        # ...
    
    # Regular table
    # ...
```

Теперь:
1. Тип параметра точно соответствует реализации (`str`)
2. Явная проверка типа обеспечивает ранний отказ с понятным сообщением
3. Сообщение об ошибке информативно объясняет текущие ограничения

## Итоговая сводка всех исправленных багов

Всего было исправлено **8 багов**:

1. ✅ **Парсер JSQL (Bug 1)**: Синхронный доступ к metadata - исправлено использованием `_get_table_async()`
2. ✅ **Handler metadata (Bug 2)**: Отсутствие namespace префикса - исправлено добавлением логики формирования полного имени
3. ✅ **Async fixtures (Bug 3)**: Ошибки pytest - исправлено использованием `@pytest_asyncio.fixture`
4. ✅ **Preload метаданных (Bug 4)**: Неправильный ключ кеша - исправлено добавлением параметра `namespace`
5. ✅ **Namespace fallback (Bug 5)**: Неправильное определение namespace для entity без префикса - исправлено трёхуровневой логикой fallback
6. ✅ **Engine type check (Bug 6)**: Неправильное определение async/sync engine - исправлено использованием `engine.dialect.is_async`
7. ✅ **Registry lookup (Bug 7)**: Использование неправильного registry в `copy_field_meta()` - исправлено использованием `get_registry()` из api.py
8. ✅ **Type signature mismatch (Bug 8)**: Несоответствие типа и реализации в `_build_from()` - исправлено ограничением типа до `str` и добавлением явной проверки

**Все 52 теста успешно пройдены!** 

Рефакторинг инициализации UMA полностью завершён, все критические баги исправлены, код тщательно протестирован. Решение готово к утверждению и коммиту.

```python
# Было:
async def _build_from(self, from_spec: str | dict[str, Any]) -> Any:
    """Build FROM clause."""
    # Simple table name
    if isinstance(from_spec, str):
        # ... обработка строки ...
    
    # Subquery (not implemented in this version)
    raise JSQLSyntaxError('Subqueries in FROM clause not yet supported')

# Стало:
async def _build_from(self, from_spec: str) -> Any:
    """Build FROM clause."""
    if not isinstance(from_spec, str):
        raise JSQLSyntaxError(
            f'FROM clause must be a string (table name), got {type(from_spec).__name__}. '
            'Subqueries and complex FROM specifications are not yet supported.'
        )
    
    # Check if it's a CTE
    if from_spec in self.ctes:
        # ...
    
    # Regular table
    # ...
```
