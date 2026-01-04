# Дизайн и регистрация обработчиков сущностей
_Exported on 04.01.2026 at 14:29:16 GMT+3 from Cursor (2.3.21)_

---

**User**

(20260103-132214) (20260103-144530) (20260103-162928)
(1) Пожалуйста, ознакомься с содержимым каталога `docs`, изучи документы в хронологическом порядке.

(2) Предлагается обсудить тему регистрации обработчиков сущностей.

(3) Чтобы избежать когнитивных искажений, я буду использовать обозначение `eh` от "Entity Handlers" применительно к классам, содержащим реализацию операционного языка UMA (select, CRUD, meta) вместо ранее использованного обозначения сущностей `entity`.
Термин же `entity` я буду использовать применительно к объектам БД (таблицы, представления).
Также я буду говорить о *виртуальном представлении*, когда представление эмулируется на уровне UMA, и может быть использовано в UMA-запросах как обычная таблица. 

(4) В свете твоих предложений в `docs/20260103-130403-log-impl_proposal.md` я решил сейчас сосредоточиться на теме регистрации, поскольку позже это будет непросто изменить.

(5) Итак, несколько тезисов в этой связи:

(.1) (`entity_name`) Ключом регистрации является имя сущности.

(.2) Мне хотелось бы регистрировать способ, алгоритм (получения выборки, выполнения CRUD, получения метаданных), у которого нет состояния, а всё необходимое для его (способа, алгоритма) работы передаётся в параметрах при вызове соответствующих операций (select, CRUD, meta).

(.3) Поскольку речь идёт о способе, то способ (алгоритм) не обязан быть привязан к имени сущности отношением one-to-one – здесь как правило one-to-many: один и тот же способ обработки UMA-операций может соответствовать множеству сущностей, как скорее всего будет с большинством таблицы БД – их будет обслуживать Entity Handler по-умолчанию.
Соответственно, имя сущности и метаданные в Entity Handler храниться не должны. Но метаданные должны быть доступны через передачу в соответствующий метод имени сущности (см. ниже).
Исключением здесь будут виртуальные представления, у которых практически гарантировано будет специфический `select`, а метаданные удобно хранить в полях класса, поскольку SQLAlchemy и СУБД о них ничего "не знает". 

(.4) Элемент регистрации – обработчик сущности я предлагаю обозначать `eh`, говорил о нём выше.

(.5) Мы никогда не будем создавать экземпляры `eh`, мы будем через механизм регистрации получать по имени сущности соответствующий класс-обработчик, и обращаться к его методам, передавая имя сущности в качестве параметра, в случае необходимости.

(.6) Механизм регистрации должен использовать `eh` по-умолчанию, если:
1) для имени сущности отсутствует элемент регистрации – например, если у сущности нет специфики
2) соответствующая `entity` существует – в БД есть такая таблица.

(.7) Для механизма регистрации должен быть предусмотрен способ указания `eh` по-умолчанию.
Как правило, в проекте, использующем UMA может будет определяться класс-потомок от `eh` по-молчанию, реализация которого содержится в UMA. 

(6) Контекст UMA-операций в `eh`.

(.1) `select`: нужны имя таблицы, и доступ к параметрам запроса – это актуально для виртуальных представлений.

(.2) CRUD-операции: имя таблицы, словарь с данными для `save`, или `id` для `read` и `delete`.

(.3) `meta`: имя таблицы. Метаданные не обязательно хранить в `eh` – это можно организовать в другом месте – да в том же Redis, хотя сейчас для простоты можно предусмотреть специальный словарь в глобальном контексте.
Вариант с Redis интересен тем. что метаданные могут разделяться между разными экземплярами сервиса, использующего UMA.

(7) Метаданные.
Основной источник метаданных – SQAlchemy. В первую очередь будут интересовать сведения о полях таблицы/представления, их типе, размере, а также (очень важно) спецификации внешнего ключа: на какую таблицу/поле этот ключ направлен.
Также пригодится информация о m2m-полях (many-to-many), поскольку это важно на frontend для поддержки соответствующего способа ввода значений таких полей.

(8) Тезисы относительно обработки запроса на выборку.
1) Определяем имя сущности (`entity_name`) из блока `FROM`.
2) Находим обслуживающий эту сущность Entity Handler (`eh`).
3) Вызываем `eh.select`, куда так или иначе передаём имя сущности и параметры запроса. Получаем запрос (`query`).
4) Аналогично пп. 2, 3 поступаем в отношении имён сущностей, указанных в  `JSON`.
5) Учитываем, что сущность может быть виртуальным представлением, и может неоднократно присутствовать в запросе (подзапросы и `JOIN`) – поэтому результат `select` в виртуальном представлении всегда оформляется как CTE в формируемом запросе, чтобы избежать дублирования.
—

Пожалуйста, оцени идею, подсвети риски и, если нет возражений – предложи реализацию.


---

**Cursor**

Давайте начнём с изучения документации в каталоге `docs`, а затем обсудим вашу концепцию регистрации обработчиков сущностей.

Изучил документацию. Вижу эволюцию концепции от entity как классов с экземплярами к **entity handlers (eh)** как stateless обработчиков. Это существенное изменение подхода. Давайте проанализирую предложенную концепцию.

## 📊 Оценка концепции Entity Handlers (eh)

### ✅ Сильные стороны

1. **Stateless подход**: Отсутствие состояния в `eh` упрощает параллелизм, масштабирование и тестирование.

2. **One-to-many маппинг**: Один `eh` на множество сущностей — элегантное решение, уменьшающее дублирование кода (например, один `DefaultTableHandler` для всех обычных таблиц).

3. **Разделение терминов**: `eh` (обработчик) vs `entity` (объект БД) — чёткое разделение ответственности.

4. **Явная передача контекста**: Все необходимое передаётся в параметрах — делает зависимости явными, упрощает отладку.

5. **Fallback-механизм**: Автоматическое использование `eh` по-умолчанию для незарегистрированных сущностей.

### ⚠️ Критика и риски

#### 1. **Концептуальные риски**

**Риск: Путаница с классами vs экземплярами**

```python
# Если мы "никогда не создаём экземпляры eh":
class UserHandler:
    @classmethod
    def select(cls, entity_name: str, params: dict) -> Select:
        # Как получить доступ к engine, metadata?
        # Где хранить connection pool?
        pass
```

**Проблема**: Если `eh` — это класс без экземпляров, то:
- Где хранить зависимости (`engine`, `metadata`, конфигурацию)?
- Как передавать глобальный контекст (connection pool, кэш)?
- Как тестировать с mock-зависимостями?

**Решение 1**: Singleton-экземпляр `eh` с ленивой инициализацией
```python
class EntityHandler:
    _instance = None
    _engine = None
    _metadata = None
    
    @classmethod
    def initialize(cls, engine, metadata):
        cls._engine = engine
        cls._metadata = metadata
```

**Решение 2**: Передавать контекст в каждый вызов
```python
class Context:
    engine: Engine
    metadata: MetaData
    cache: Cache
    
def select(entity_name: str, params: dict, context: Context) -> Select:
    # Всё необходимое передано явно
    pass
```

**Риск: Метаданные для виртуальных представлений**

> "метаданные удобно хранить в полях класса, поскольку SQLAlchemy и СУБД о них ничего не знает"

Но если мы не создаём экземпляры, то где хранить эти метаданные?

```python
class VirtualViewHandler:
    # Атрибуты класса? Но тогда для каждого виртуального представления 
    # нужен отдельный класс-наследник
    virtual_view_metadata = {
        "columns": [...],
        "description": "..."
    }
```

**Решение**: Отдельный registry метаданных виртуальных представлений
```python
virtual_views_metadata = {
    "user_summary": {
        "columns": [...],
        "handler": VirtualViewHandler
    }
}
```

#### 2. **Технические риски**

**Риск: Определение `eh` по-умолчанию**

> "в проекте, использующем UMA может будет определяться класс-потомок от `eh` по-молчанию"

Как механизм регистрации узнает о новом `eh` по-умолчанию?

```python
# Вариант 1: Явная регистрация
uma.set_default_handler(MyCustomDefaultHandler)

# Вариант 2: Convention - ищем класс с определённым именем
class DefaultEntityHandler:  # Автоматически используется
    pass
```

**Риск: Получение метаданных из разных источников**

> "метаданные не обязательно хранить в `eh` – это можно организовать в другом месте"

Но операция `meta` — часть `eh`. Как `eh` будет получать метаданные из Redis/другого хранилища?

```python
class EntityHandler:
    @staticmethod
    def meta(entity_name: str, context: Context) -> dict:
        # Откуда брать метаданные?
        # 1. Из SQLAlchemy MetaData?
        # 2. Из Redis?
        # 3. Из обоих?
        pass
```

**Решение**: Явная абстракция для хранилища метаданных
```python
class MetadataProvider(Protocol):
    def get_metadata(self, entity_name: str) -> dict:
        ...

class SQLAlchemyMetadataProvider:
    def get_metadata(self, entity_name: str) -> dict:
        # Из metadata.tables[entity_name]
        pass

class RedisMetadataProvider:
    def get_metadata(self, entity_name: str) -> dict:
        # Из Redis
        pass

class CompositeMetadataProvider:
    def get_metadata(self, entity_name: str) -> dict:
        # Сначала SQLAlchemy, затем обогащаем из Redis
        pass
```

**Риск: CTE для виртуальных представлений при множественном использовании**

> "результат `select` в виртуальном представлении всегда оформляется как CTE"

Это корректно, но нужно учитывать:
- CTE материализуется в PostgreSQL 12+ (может быть overhead)
- Нужна дедупликация: если виртуальное представление используется 3 раза, CTE создаётся один раз

#### 3. **Архитектурные риски**

**Риск: Регистрация по имени сущности**

> "Ключом регистрации является имя сущности"

Что если:
- Таблица `users` и виртуальное представление `users`?
- Две разные БД с таблицей `users`?

```python
# Конфликт имён
registry.register("users", CustomUserHandler)  # Таблица users
registry.register("users", VirtualUsersHandler)  # Виртуальное представление users
```

**Решение**: Namespace или префиксы
```python
registry.register("db:users", CustomUserHandler)
registry.register("virtual:users", VirtualUsersHandler)

# Или явное разделение
table_registry.register("users", CustomUserHandler)
virtual_registry.register("users", VirtualUsersHandler)
```

**Риск: Как определить, что сущность виртуальная?**

При обработке JSQL нужно понять:
- Это таблица БД?
- Это виртуальное представление?
- Это VIEW в БД?

```python
def resolve_entity(entity_name: str) -> tuple[EntityHandler, bool]:
    # 1. Проверяем явные регистрации
    if entity_name in explicit_registry:
        handler, is_virtual = explicit_registry[entity_name]
        return handler, is_virtual
    
    # 2. Проверяем таблицы БД
    if entity_name in metadata.tables:
        return default_handler, False
    
    # 3. Не найдено
    raise ValueError(f"Entity {entity_name} not found")
```

### 💡 Предложения по улучшению

#### 1. **Явный Context объект**

```python
@dataclass
class UMAContext:
    """Контекст выполнения UMA операций"""
    engine: Engine
    metadata: MetaData
    metadata_provider: MetadataProvider
    user_context: Any  # Для проверки доступа
    cache: Optional[Cache] = None
```

#### 2. **Разделение обязанностей**

```python
class EntityHandler(Protocol):
    """Базовый протокол для entity handlers"""
    
    @staticmethod
    def select(entity_name: str, params: dict, context: UMAContext) -> Select:
        """Возвращает SQLAlchemy Select"""
        ...
    
    @staticmethod
    def read(entity_name: str, id_value: Any, context: UMAContext) -> dict | None:
        """Читает запись по id"""
        ...
    
    @staticmethod
    def save(entity_name: str, data: dict, context: UMAContext) -> Any:
        """Сохраняет запись"""
        ...
    
    @staticmethod
    def delete(entity_name: str, id_value: Any, context: UMAContext) -> bool:
        """Удаляет запись"""
        ...

class MetadataHandler(Protocol):
    """Отдельный протокол для метаданных"""
    
    @staticmethod
    def meta(entity_name: str, context: UMAContext) -> dict:
        """Возвращает метаданные сущности"""
        ...
```

## 🏗️ Техническая реализация

### 1. Базовая архитектура

```python
from typing import Protocol, Any, runtime_checkable
from dataclasses import dataclass
from sqlalchemy import Engine, MetaData, Select

# ================== Контекст ==================

@dataclass
class UMAContext:
    """Контекст выполнения UMA операций"""
    engine: Engine
    metadata: MetaData
    user_context: Any = None
    cache: Any = None
    
    @property
    def connection(self):
        """Получить connection из engine"""
        return self.engine.connect()

# ================== Протокол EntityHandler ==================

@runtime_checkable
class EntityHandler(Protocol):
    """
    Протокол для entity handler.
    Все методы статические - нет состояния.
    """
    
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        """
        Возвращает SQLAlchemy Select для выборки данных.
        
        Args:
            entity_name: имя сущности (таблицы)
            params: параметры запроса из JSQL
            context: контекст выполнения
        """
        ...
    
    @staticmethod
    def read(entity_name: str, id_value: Any, context: UMAContext) -> dict | None:
        """
        Читает одну запись по id.
        Использует select для проверки доступа.
        """
        ...
    
    @staticmethod
    def save(entity_name: str, data: dict, context: UMAContext) -> Any:
        """
        Сохраняет запись (create если id=None, иначе update).
        Возвращает id сохранённой записи.
        """
        ...
    
    @staticmethod
    def delete(entity_name: str, id_value: Any, context: UMAContext) -> bool:
        """
        Удаляет запись по id.
        Проверяет доступ через read.
        """
        ...
    
    @staticmethod
    def meta(entity_name: str, context: UMAContext) -> dict:
        """
        Возвращает метаданные сущности.
        """
        ...

# ================== Реализация по-умолчанию ==================

class DefaultEntityHandler:
    """
    Обработчик по-умолчанию для обычных таблиц БД.
    Обслуживает большинство таблиц без специфики.
    """
    
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        """Простой SELECT * FROM table"""
        from sqlalchemy import select
        
        table = context.metadata.tables[entity_name]
        return select(table)
    
    @staticmethod
    def read(entity_name: str, id_value: Any, context: UMAContext) -> dict | None:
        """Чтение через select + фильтр по id"""
        from sqlalchemy import select
        
        # Получаем select (с политиками доступа, если есть)
        query = DefaultEntityHandler.select(entity_name, {}, context)
        
        # Добавляем условие по id
        table = context.metadata.tables[entity_name]
        # Предполагаем, что PK - это "id"
        # TODO: определять PK из table.primary_key
        query = query.where(table.c.id == id_value)
        
        # Выполняем
        with context.connection as conn:
            result = conn.execute(query).mappings().first()
            return dict(result) if result else None
    
    @staticmethod
    def save(entity_name: str, data: dict, context: UMAContext) -> Any:
        """Сохранение в таблицу"""
        from sqlalchemy import insert, update
        
        table = context.metadata.tables[entity_name]
        id_value = data.get("id")  # TODO: определять PK динамически
        is_create = (id_value is None)
        
        with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = conn.execute(stmt)
                return result.inserted_primary_key[0]
            else:
                # UPDATE
                # Проверяем доступ
                existing = DefaultEntityHandler.read(entity_name, id_value, context)
                if existing is None:
                    raise PermissionError(f"No access to {entity_name}:{id_value}")
                
                stmt = update(table).where(table.c.id == id_value).values(**data)
                conn.execute(stmt)
                return id_value
    
    @staticmethod
    def delete(entity_name: str, id_value: Any, context: UMAContext) -> bool:
        """Удаление из таблицы"""
        from sqlalchemy import delete as sql_delete
        
        # Проверяем доступ
        existing = DefaultEntityHandler.read(entity_name, id_value, context)
        if existing is None:
            raise PermissionError(f"No access to delete {entity_name}:{id_value}")
        
        table = context.metadata.tables[entity_name]
        
        with context.engine.begin() as conn:
            stmt = sql_delete(table).where(table.c.id == id_value)
            result = conn.execute(stmt)
            return result.rowcount > 0
    
    @staticmethod
    def meta(entity_name: str, context: UMAContext) -> dict:
        """Метаданные из SQLAlchemy MetaData"""
        table = context.metadata.tables[entity_name]
        
        columns = []
        for col in table.columns:
            col_info = {
                "name": col.name,
                "type": str(col.type),
                "nullable": col.nullable,
                "primary_key": col.primary_key,
            }
            
            # Внешние ключи
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info["related_to"] = fk.column.table.name
                col_info["related_key"] = fk.column.name
            
            columns.append(col_info)
        
        return {
            "name": entity_name,
            "description": "",  # Можно обогатить из COMMENT
            "columns": columns
        }

# ================== Пример кастомного handler ==================

class RestrictedUserHandler:
    """
    Handler для сущности users с политикой доступа:
    показываем только активных пользователей.
    """
    
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        """SELECT с политикой доступа"""
        from sqlalchemy import select
        
        table = context.metadata.tables[entity_name]
        query = select(table).where(table.c.active == True)
        
        # Дополнительные фильтры из params
        if "department_id" in params:
            query = query.where(table.c.department_id == params["department_id"])
        
        return query
    
    @staticmethod
    def read(entity_name: str, id_value: Any, context: UMAContext) -> dict | None:
        """Используем DefaultEntityHandler.read, но с нашим select"""
        # Можем переиспользовать логику из default
        query = RestrictedUserHandler.select(entity_name, {}, context)
        
        table = context.metadata.tables[entity_name]
        query = query.where(table.c.id == id_value)
        
        with context.connection as conn:
            result = conn.execute(query).mappings().first()
            return dict(result) if result else None
    
    # save и delete можно переиспользовать из DefaultEntityHandler
    save = DefaultEntityHandler.save
    delete = DefaultEntityHandler.delete
    meta = DefaultEntityHandler.meta

# ================== Виртуальные представления ==================

class VirtualViewHandler:
    """
    Handler для виртуальных представлений.
    Метаданные хранятся отдельно в registry.
    """
    
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        """
        Для виртуальных представлений select определяется при регистрации.
        Этот метод не вызывается напрямую, вместо этого используется
        зарегистрированный callable.
        """
        raise NotImplementedError("Virtual views must provide select function at registration")
    
    @staticmethod
    def read(entity_name: str, id_value: Any, context: UMAContext) -> dict | None:
        """Виртуальные представления могут не поддерживать read"""
        raise NotImplementedError(f"Virtual view {entity_name} does not support read")
    
    @staticmethod
    def save(entity_name: str, data: dict, context: UMAContext) -> Any:
        """Виртуальные представления обычно readonly"""
        raise NotImplementedError(f"Virtual view {entity_name} is read-only")
    
    @staticmethod
    def delete(entity_name: str, id_value: Any, context: UMAContext) -> bool:
        """Виртуальные представления обычно readonly"""
        raise NotImplementedError(f"Virtual view {entity_name} is read-only")
    
    @staticmethod
    def meta(entity_name: str, context: UMAContext) -> dict:
        """Метаданные из registry виртуальных представлений"""
        # TODO: получить из virtual_views_registry
        pass
```

### 2. Entity Registry

```python
from typing import Callable, Type

class EntityRegistry:
    """
    Реестр обработчиков сущностей.
    Поддерживает fallback на handler по-умолчанию.
    """
    
    def __init__(self, default_handler: Type[EntityHandler] = DefaultEntityHandler):
        # Явные регистрации: entity_name -> handler class
        self._handlers: dict[str, Type[EntityHandler]] = {}
        
        # Handler по-умолчанию
        self._default_handler: Type[EntityHandler] = default_handler
        
        # Виртуальные представления: entity_name -> select function
        self._virtual_views: dict[str, Callable] = {}
        
        # Метаданные виртуальных представлений
        self._virtual_metadata: dict[str, dict] = {}
    
    def register_handler(self, entity_name: str, handler: Type[EntityHandler]):
        """
        Регистрирует handler для сущности.
        Если handler уже зарегистрирован, заменяет его.
        """
        if not isinstance(handler, type):
            raise TypeError("handler must be a class, not instance")
        
        self._handlers[entity_name] = handler
    
    def register_virtual_view(
        self,
        name: str,
        select_fn: Callable[[dict, UMAContext], Select],
        metadata: dict
    ):
        """
        Регистрирует виртуальное представление.
        
        Args:
            name: имя виртуального представления
            select_fn: функция, возвращающая Select
            metadata: метаданные (columns, description, etc.)
        """
        self._virtual_views[name] = select_fn
        self._virtual_metadata[name] = metadata
    
    def get_handler(self, entity_name: str, context: UMAContext) -> Type[EntityHandler]:
        """
        Возвращает handler для сущности.
        
        Порядок разрешения:
        1. Явно зарегистрированный handler
        2. Виртуальное представление
        3. Таблица БД -> default handler
        4. ValueError
        """
        # 1. Явные регистрации
        if entity_name in self._handlers:
            return self._handlers[entity_name]
        
        # 2. Виртуальные представления
        if entity_name in self._virtual_views:
            return VirtualViewHandler
        
        # 3. Таблицы БД
        if entity_name in context.metadata.tables:
            return self._default_handler
        
        # 4. Не найдено
        raise ValueError(f"Entity {entity_name} not found")
    
    def is_virtual(self, entity_name: str) -> bool:
        """Проверяет, является ли сущность виртуальным представлением"""
        return entity_name in self._virtual_views
    
    def get_virtual_select(
        self,
        entity_name: str,
        params: dict,
        context: UMAContext
    ) -> Select:
        """Возвращает Select для виртуального представления"""
        if entity_name not in self._virtual_views:
            raise ValueError(f"{entity_name} is not a virtual view")
        
        select_fn = self._virtual_views[entity_name]
        return select_fn(params, context)
    
    def list_all(self, context: UMAContext) -> list[str]:
        """Возвращает список всех доступных сущностей"""
        explicit = set(self._handlers.keys())
        virtual = set(self._virtual_views.keys())
        tables = set(context.metadata.tables.keys())
        
        return sorted(explicit | virtual | tables)
    
    def set_default_handler(self, handler: Type[EntityHandler]):
        """Устанавливает handler по-умолчанию"""
        self._default_handler = handler
```

### 3. Интеграция с JSQL

```python
def process_jsql(jsql: dict, params: dict, context: UMAContext, registry: EntityRegistry):
    """
    Обрабатывает JSQL запрос с учётом зарегистрированных handlers.
    """
    from_entity = jsql["from"]
    
    # Получаем handler
    handler = registry.get_handler(from_entity, context)
    
    # Определяем, нужен ли CTE
    if registry.is_virtual(from_entity):
        # Виртуальное представление - всегда CTE
        entity_select = registry.get_virtual_select(from_entity, params, context)
        entity_cte = entity_select.cte(name=from_entity)
        base_selectable = entity_cte
    elif from_entity in registry._handlers:
        # Явно зарегистрированный handler - используем CTE для применения политик
        entity_select = handler.select(from_entity, params, context)
        entity_cte = entity_select.cte(name=from_entity)
        base_selectable = entity_cte
    else:
        # Обычная таблица - используем напрямую
        base_selectable = context.metadata.tables[from_entity]
    
    # Строим основной запрос
    query = build_query_from_jsql(jsql, base_selectable, registry, params, context)
    
    return query
```

### 4. API функции

```python
# Глобальный registry (инициализируется при запуске приложения)
_registry: EntityRegistry = None
_context: UMAContext = None

def initialize_uma(engine: Engine, metadata: MetaData, default_handler = None):
    """Инициализация UMA"""
    global _registry, _context
    
    _context = UMAContext(engine=engine, metadata=metadata)
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)
    
    return _registry

def uma_select(jsql: dict, params: dict = None, user_context: Any = None) -> dict:
    """Выполняет JSQL запрос"""
    ctx = UMAContext(
        engine=_context.engine,
        metadata=_context.metadata,
        user_context=user_context
    )
    
    query = process_jsql(jsql, params or {}, ctx, _registry)
    
    # Выполняем и возвращаем результат
    with ctx.connection as conn:
        result = conn.execute(query)
        return {
            "metadata": {},  # TODO
            "data": [dict(row) for row in result.mappings()]
        }

def uma_read(entity_name: str, id_value: Any, user_context: Any = None) -> dict | None:
    """Читает запись"""
    ctx = UMAContext(
        engine=_context.engine,
        metadata=_context.metadata,
        user_context=user_context
    )
    
    handler = _registry.get_handler(entity_name, ctx)
    return handler.read(entity_name, id_value, ctx)

def uma_save(entity_name: str, data: dict, user_context: Any = None) -> Any:
    """Сохраняет запись"""
    ctx = UMAContext(
        engine=_context.engine,
        metadata=_context.metadata,
        user_context=user_context
    )
    
    handler = _registry.get_handler(entity_name, ctx)
    return handler.save(entity_name, data, ctx)

def uma_delete(entity_name: str, id_value: Any, user_context: Any = None) -> bool:
    """Удаляет запись"""
    ctx = UMAContext(
        engine=_context.engine,
        metadata=_context.metadata,
        user_context=user_context
    )
    
    handler = _registry.get_handler(entity_name, ctx)
    return handler.delete(entity_name, id_value, ctx)

def uma_meta(entity_name: str, user_context: Any = None) -> dict:
    """Возвращает метаданные сущности"""
    ctx = UMAContext(
        engine=_context.engine,
        metadata=_context.metadata,
        user_context=user_context
    )
    
    handler = _registry.get_handler(entity_name, ctx)
    return handler.meta(entity_name, ctx)
```

### 5. Примеры использования

```python
# ========== Инициализация ==========

engine = create_engine("postgresql://...")
metadata = MetaData()
metadata.reflect(bind=engine)

registry = initialize_uma(engine, metadata)

# ========== Регистрация handlers ==========

# 1. Регистрируем handler для конкретной таблицы
registry.register_handler("users", RestrictedUserHandler)

# 2. Регистрируем виртуальное представление
def user_summary_select(params: dict, context: UMAContext) -> Select:
    from sqlalchemy import select, func
    
    users = context.metadata.tables["users"]
    orders = context.metadata.tables["orders"]
    
    return select(
        users.c.id,
        users.c.name,
        func.count(orders.c.id).label("order_count"),
        func.sum(orders.c.total).label("total_spent")
    ).select_from(
        users.outerjoin(orders)
    ).group_by(users.c.id, users.c.name).where(
        # Политика доступа через params
        users.c.id == params.get("user_id")
    )

registry.register_virtual_view(
    name="user_summary",
    select_fn=user_summary_select,
    metadata={
        "description": "Сводка по заказам пользователя",
        "columns": [
            {"name": "id", "type": "integer"},
            {"name": "name", "type": "string"},
            {"name": "order_count", "type": "integer"},
            {"name": "total_spent", "type": "numeric"}
        ]
    }
)

# 3. Устанавливаем кастомный handler по-умолчанию
class MyDefaultHandler(DefaultEntityHandler):
    @staticmethod
    def select(entity_name: str, params: dict, context: UMAContext) -> Select:
        # Добавляем логирование
        print(f"Selecting from {entity_name}")
        return DefaultEntityHandler.select(entity_name, params, context)

registry.set_default_handler(MyDefaultHandler)

# ========== Использование ==========

# 1. Обычная таблица (fallback на default handler)
orders_data = uma_read("orders", 123)

# 2. Таблица с кастомным handler (политика доступа)
user = uma_read("users", 456)  # Используется RestrictedUserHandler

# 3. Виртуальное представление
jsql = {
    "select": ["*"],
    "from": "user_summary",
    "params": {"user_id": 456}
}
summary = uma_select(jsql, user_context=current_user)

# 4. JSQL с JOIN виртуального представления и таблицы
jsql = {
    "select": ["user_summary.name", "user_summary.total_spent", "departments.name"],
    "from": "user_summary",  # Виртуальное представление (CTE)
    "joins": [{
        "type": "inner",
        "table": "departments",  # Обычная таблица
        "on": {"user_summary.id": "departments.manager_id"}
    }],
    "params": {"user_id": 456}
}
result = uma_select(jsql, user_context=current_user)
```

## 📝 Итоговые рекомендации

### ✅ Что работает отлично:

1. **Stateless handlers** — упрощает многопоточность и тестирование
2. **One-to-many mapping** — минимизирует дублирование
3. **Разделение `eh` vs `entity`** — чёткая архитектура
4. **Fallback механизм** — минимальный boilerplate

### ⚠️ Ключевые решения:

1. **Context объект** — явная передача зависимостей (engine, metadata, user_context)
2. **Отдельный registry для виртуальных представлений** — хранение метаданных вне handlers
3. **CTE для зарегистрированных handlers и виртуальных представлений** — применение политик доступа
4. **Статические методы** — handlers как namespace для функций, а не объекты

### 🚧 Риски требующие внимания:

1. **Определение PK динамически** — не все таблицы используют `id`
2. **Производительность CTE** — для PostgreSQL 12+ можно управлять материализацией
3. **Метаданные из разных источников** — нужен MetadataProvider
4. **Namespace для имён** — избежать конфликтов table vs virtual view

Концепция **жизнеспособна и элегантна**! Ключевое отличие от предыдущего подхода — handlers как stateless алгоритмы, а не объекты с состоянием. Это упрощает архитектуру и масштабирование.

Готов обсудить детали реализации и начать разработку кода.

---

**User**

(20260103-163225) (20260104-001939)
> 1. Концептуальные риски
> Риск: Путаница с классами vs экземплярами
> ... ... ...
> Решение 2: Передавать контекст в каждый вызов

Да, именно так. Передавать контекст в каждый вызов. Возможно, не самой плохой идеей будет использовать контекстные переменные.

> Риск: Метаданные для виртуальных представлений
>> "метаданные удобно хранить в полях класса, поскольку SQLAlchemy и СУБД о них ничего не знает"
> Но если мы не создаём экземпляры, то где хранить эти метаданные?
> ```python
> class VirtualViewHandler:
>     # Атрибуты класса? Но тогда для каждого виртуального представления
>     # нужен отдельный класс-наследник
> ```

Да, именно так. Для каждого виртуального представления чаще всего будет отдельный наследник.
Но у нас здесь всё равно достаточно широкий простор для сохранения чистоты архитектуры: например, в проекте мы можем договориться хранить метаданные виртуальных представлений в константе, или даже читать их из БД.
Мы можем определить там базовый класс для проектных виртуальных представлений, который будет учитывать способ хранения метаданных, и задействовать этот способ с учётом `entity_name`, передаваемого при запросе метаданных.

> 2. Технические риски
> Риск: Определение eh по-умолчанию
>> "в проекте, использующем UMA может будет определяться класс-потомок от eh по-молчанию"
> Как механизм регистрации узнает о новом eh по-умолчанию?
> ... ... ...
> ```
> # Вариант 1: Явная регистрация
> uma.set_default_handler(MyCustomDefaultHandler)
> ```

Да, именно так. Явное лучше неявного.

> Риск: Получение метаданных из разных источников
>> "метаданные не обязательно хранить в eh – это можно организовать в другом месте"
> Но операция meta — часть eh. Как eh будет получать метаданные из Redis/другого хранилища?
> ```
> class EntityHandler:
>     @staticmethod
>     def meta(entity_name: str, context: Context) -> dict:
> ```

Думаю, нам следует предусмотреть способ пополнения `context`, поместить туда экземпляр `MetadataProvider`, чтобы в `EntityHandler.meta` можно было к нему обратиться.

> Риск: CTE для виртуальных представлений при множественном использовании
>> "результат select в виртуальном представлении всегда оформляется как CTE"
> Это корректно, но нужно учитывать:
> CTE материализуется в PostgreSQL 12+ (может быть overhead)
> Нужна дедупликация: если виртуальное представление используется 3 раза, CTE создаётся один раз

(.1) 
> CTE материализуется в PostgreSQL 12+ (может быть overhead)

Расскажи пожалуйста подробнее о "материализации CTE" – никогда об этом не слышал.
Ну, и у нас не только PostgreSQL, не думаю, что завязываться на эту замечательную СУБД на уровне UMA хорошая идея.

(.2) Я думаю, дедупликацию можно обеспечить тем, что виртуальное представление всегда связано с именем сущности.
В процессе формирования набора инструкций SQLAlchemy можно хранить словарь технических CTE, ключом в котором будет имя сущности.
При преобразовании результата `select` от виртуального представления, зная имя сущности  этого представления, можно просто сослаться на существующий в словаре CTE, если он уже сформирован, или создать CTE и поместить его в словарь с ключом по имени сущности.

> 3. Архитектурные риски
> Риск: Регистрация по имени сущности
>> "Ключом регистрации является имя сущности"
> Что если:
> Таблица users и виртуальное представление users?
> Две разные БД с таблицей users?
> ... ... ...
> Решение: Namespace или префиксы

Да, всё верно. Используем подход с namesapaces. Имя сущности должно быть уникальным.

> Риск: Как определить, что сущность виртуальная?
> При обработке JSQL нужно понять:
> Это таблица БД?
> Это виртуальное представление?
> Это VIEW в БД?

Как насчёт специального атрибута в метаданных сущности виртуального представления?
Например: `is_vitrual_view = True`?

> 1. Явный Context объект

Да, отличная мысль. Принимается.

> 2. Разделение обязанностей

Хм... 🤔 Вот тут ты меня немного озадачил.
Ты почему-то выделил два интерфейса: для select/CRUD и, отдельно, для метаданных.
Ну, я бы понял бы, если бы ты определил три интерфейса: отдельно для `select`, CRUD и метаданных...
Но давай сначала ответим на вопрос: а что это нам даст?
Смотри: известные типовые сценарии разработчика, когда он будет создавать EntityHandler такие:

(.1) Нужно ограничить выборку таблицы, например, в зависимости от полномочий пользователя. — Реализуем `select`.

(.2) Специальная бизнес-логика для таблицы. — Реализуем `save` и/или `delete`.

(.3) Хотим виртуальное представление. — Чаще всего ограничиваемся реализацией `select` и `meta`, поскольку CRUD операции над виртуальными представлениями трудно укладываются в голову: ну какая там CRUD-логика, если `select` возвращает CROSS JOIN?

Но в более простых случаях CRUD-операции могут оказаться востребованы.

(.4) Хотим сообщить что-то дополнительное о сущности. — Реализуем `meta`, где вызываем метод предка, после чего добавляем в полученный словарь что-то дополнительно.

(.5) Источник дополнительных сведений для метаданных. Например, переводы. — Реализуем свой `MetadataProvider`, или `EntityHandler` по-умолчанию.

(.6) (ВЫЖНО!) Нужно перечисление всех сущностей.
Я об этом не говорил ранее, но это действительно нужно – получить перечисление имён всех сущностей в формате `<workspace>:<name>`, чтобы затем элемент этого перечисления можно было использовать в качестве `entity_name`.
Поскольку их может быть много (запросто мы можем работать с несколькими схемами, или даже с разными БД!) обработка может занять заметное время, и результат лучше кэшировать, чтобы не тратить эти время и ресурсы далее.
Думаю, `MetadataProvider` хорошее место для размещения соответствующей функциональности.

> ⚠️ Ключевые решения:
> ... ... ...
> Отдельный registry для виртуальных представлений — хранение метаданных вне handlers

В свете обозначенного выше я не нахожу оснований вводить специальный реестр именно для виртуальных представлений.
Думаю, можно сделать специальный атрибут в метаданных сущности.

> CTE для зарегистрированных handlers и виртуальных представлений — применение политик доступа

Относительно политик доступа. Предлагаю добавить в `MetadataProvider` метод `can`, в который реализация библиотеки будет передавать:
* `entity_name` – имя сущности
* `operation` – тип операции
* `context` – экземпляр `Context`

Где тип операции это:
`list` – доступ к списку записей. Если `can` возвращает `True`, значит, `select` EntityHandler доступен. Иначе выбрасываем исключение `UmaAccessDeniedError`.
`read` – доступ к чтению записи. Определяет доступность `read`.
`write` – доступ к сохранению изменений. Определяет доступность `save`.
`delete` – доступ к удалению. Определяет доступность `delete`.
`meta` – определяет доступность `meta` в EntityHandler.

Дополнительно, в `can` можно поддержать сценарий, когда в качестве `entity_name` передана пустая строка, а в качестве операции `meta` – так можно проверять доступность списка сущностей.
Технически, контекст должен давать доступ к информации, необходимой для реализации `can` в проекте.

(.Наблюдение) В свете `MetadataProvider.can` легко смоделировать `read_only` – `read` для сущности доступен, а `save` и `delete` – нет.
—

Пожалуйста, уточни оценку, свои вопросы и предложения по реализации в свете изложенного выше.


---

**Cursor**

Отлично! Вижу, что концепция становится более чёткой. Давайте проработаю детали с учётом ваших уточнений.

## 1. О материализации CTE в PostgreSQL

### Что такое материализация CTE?

В PostgreSQL 12+ появилась возможность управлять тем, как СУБД обрабатывает CTE:

**Материализованный CTE** (по умолчанию для модифицирующих запросов):
```sql
WITH user_orders AS MATERIALIZED (
    SELECT user_id, COUNT(*) as order_count
    FROM orders
    GROUP BY user_id
)
SELECT * FROM user_orders WHERE order_count > 10;
```
- CTE выполняется **один раз**
- Результат **сохраняется во временную структуру**
- При повторном обращении к CTE используется сохранённый результат
- **Плюсы**: гарантия однократного выполнения, полезно для дорогих операций
- **Минусы**: может помешать оптимизатору (не может "протолкнуть" фильтры внутрь CTE)

**Немaterialизованный CTE** (можно указать явно):
```sql
WITH user_orders AS NOT MATERIALIZED (
    SELECT user_id, COUNT(*) as order_count
    FROM orders
    GROUP BY user_id
)
SELECT * FROM user_orders WHERE order_count > 10;
```
- CTE **встраивается** в основной запрос (inline)
- Оптимизатор может применить фильтры и оптимизации
- **Плюсы**: лучшая оптимизация
- **Минусы**: может выполниться несколько раз, если CTE используется многократно

### Почему это важно для UMA?

Если виртуальное представление оформляется как CTE, то:
- PostgreSQL 12+ **по умолчанию материализует** его
- Это может быть **overhead**, если в основном запросе есть фильтры, которые можно было бы применить внутри CTE

**Решение**: 
1. Для межплатформенности **игнорируем** MATERIALIZED/NOT MATERIALIZED (это расширение PostgreSQL)
2. SQLAlchemy Core по умолчанию создаёт CTE без явного указания материализации
3. Для виртуальных представлений CTE - правильный выбор, т.к. гарантирует:
   - Однократное выполнение (важно для дорогих операций)
   - Применение политик доступа
   - Дедупликацию при множественном использовании

**Вывод**: Не нужно привязываться к PostgreSQL. CTE в SQLAlchemy работает кросс-платформенно, материализация - это деталь оптимизации конкретной СУБД.

## 2. Уточнённая архитектура

### Убираем разделение EntityHandler/MetadataHandler

Вы правы - нет смысла разделять. Все операции логически связаны с сущностью. Остаёмся с **единым интерфейсом EntityHandler**.

### Пересмотр с учётом ваших уточнений

```python
from typing import Protocol, Any, runtime_checkable, Literal
from dataclasses import dataclass, field
from sqlalchemy import Engine, MetaData, Select

# ================== Типы операций ==================

Operation = Literal["list", "read", "write", "delete", "meta"]

# ================== MetadataProvider ==================

@runtime_checkable
class MetadataProvider(Protocol):
    """
    Провайдер метаданных и контроля доступа.
    """
    
    def get_metadata(self, entity_name: str, context: 'UMAContext') -> dict:
        """
        Возвращает метаданные сущности.
        Может обогащать метаданные из разных источников (БД, Redis, конфиг).
        """
        ...
    
    def list_entities(self, context: 'UMAContext') -> list[str]:
        """
        Возвращает список всех доступных сущностей в формате <workspace>:<name>.
        Результат должен кэшироваться провайдером.
        """
        ...
    
    def can(
        self,
        entity_name: str,
        operation: Operation,
        context: 'UMAContext'
    ) -> bool:
        """
        Проверяет доступ к операции над сущностью.
        
        Args:
            entity_name: имя сущности (или "" для проверки доступа к списку)
            operation: тип операции (list/read/write/delete/meta)
            context: контекст выполнения
            
        Returns:
            True если доступ разрешён
            
        Raises:
            UmaAccessDeniedError если доступ запрещён
            
        Специальные случаи:
        - entity_name="" + operation="meta" -> проверка доступа к списку сущностей
        """
        ...

# ================== Context ==================

@dataclass
class UMAContext:
    """
    Контекст выполнения UMA операций.
    Передаётся в каждый вызов методов EntityHandler.
    """
    engine: Engine
    metadata: MetaData
    metadata_provider: MetadataProvider
    
    # Дополнительный контекст пользователя (для проверки прав)
    user_context: Any = None
    
    # Опциональный кэш
    cache: Any = None
    
    # Словарь для хранения CTE (дедупликация)
    _cte_cache: dict[str, Any] = field(default_factory=dict)
    
    @property
    def connection(self):
        """Получить connection из engine"""
        return self.engine.connect()
    
    def get_or_create_cte(self, entity_name: str, select_fn) -> Any:
        """
        Получает CTE из кэша или создаёт новый.
        Обеспечивает дедупликацию CTE для виртуальных представлений.
        
        Args:
            entity_name: имя сущности
            select_fn: функция, возвращающая Select (вызывается только если CTE нет в кэше)
        """
        if entity_name not in self._cte_cache:
            select_query = select_fn()
            self._cte_cache[entity_name] = select_query.cte(name=entity_name)
        return self._cte_cache[entity_name]

# ================== Исключения ==================

class UmaAccessDeniedError(PermissionError):
    """Исключение при отказе в доступе"""
    def __init__(self, entity_name: str, operation: Operation, message: str = None):
        self.entity_name = entity_name
        self.operation = operation
        super().__init__(message or f"Access denied to {operation} on {entity_name}")

# ================== EntityHandler Protocol ==================

@runtime_checkable
class EntityHandler(Protocol):
    """
    Протокол для обработчика сущностей.
    Все методы статические - нет состояния.
    Метаданные виртуальных представлений хранятся как атрибуты класса.
    """
    
    # Атрибуты класса для виртуальных представлений
    is_virtual_view: bool = False
    description: str = ""
    # Другие метаданные по необходимости
    
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        """
        Возвращает SQLAlchemy Select для выборки данных.
        Инкапсулирует политики доступа (фильтрация строк).
        
        Args:
            entity_name: имя сущности
            params: параметры запроса из JSQL
            context: контекст выполнения
            
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
    
    @staticmethod
    def read(entity_name: str, id_value: Any, context: UMAContext) -> dict | None:
        """
        Читает одну запись по id.
        Использует select() для применения политик доступа.
        
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
    
    @staticmethod
    def save(entity_name: str, data: dict, context: UMAContext) -> Any:
        """
        Сохраняет запись (create если id=None, иначе update).
        Возвращает id сохранённой записи.
        
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
    
    @staticmethod
    def delete(entity_name: str, id_value: Any, context: UMAContext) -> bool:
        """
        Удаляет запись по id.
        Проверяет доступ через read() перед удалением.
        
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
    
    @staticmethod
    def meta(entity_name: str, context: UMAContext) -> dict:
        """
        Возвращает метаданные сущности.
        Может использовать context.metadata_provider для обогащения метаданных.
        
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
```

### Реализация DefaultEntityHandler

```python
class DefaultEntityHandler:
    """
    Обработчик по-умолчанию для обычных таблиц БД.
    Используется как fallback для незарегистрированных сущностей.
    """
    
    is_virtual_view = False
    
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        """Простой SELECT * FROM table"""
        from sqlalchemy import select
        
        table = context.metadata.tables[entity_name]
        query = select(table)
        
        # params можно использовать для фильтрации, если нужно
        # Но обычно это делается в производных классах
        
        return query
    
    @staticmethod
    def read(entity_name: str, id_value: Any, context: UMAContext) -> dict | None:
        """Чтение через select + фильтр по id"""
        # Получаем select (с политиками доступа, если они определены в производном классе)
        query = DefaultEntityHandler.select(entity_name, {}, context)
        
        # Добавляем условие по id
        table = context.metadata.tables[entity_name]
        pk_columns = list(table.primary_key.columns)
        
        if len(pk_columns) == 1:
            # Простой PK
            query = query.where(pk_columns[0] == id_value)
        else:
            # Составной PK
            from sqlalchemy import and_
            conditions = [col == val for col, val in zip(pk_columns, id_value)]
            query = query.where(and_(*conditions))
        
        # Выполняем
        with context.connection as conn:
            result = conn.execute(query).mappings().first()
            return dict(result) if result else None
    
    @staticmethod
    def save(entity_name: str, data: dict, context: UMAContext) -> Any:
        """Сохранение в таблицу"""
        from sqlalchemy import insert, update
        
        table = context.metadata.tables[entity_name]
        pk_columns = list(table.primary_key.columns)
        
        # Определяем create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)
        
        is_create = (id_value is None or (isinstance(id_value, tuple) and None in id_value))
        
        with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = conn.execute(stmt)
                
                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                else:
                    return result.inserted_primary_key
            else:
                # UPDATE
                # Проверяем доступ
                existing = DefaultEntityHandler.read(entity_name, id_value, context)
                if existing is None:
                    raise UmaAccessDeniedError(
                        entity_name,
                        "write",
                        f"No access to update {entity_name}:{id_value}"
                    )
                
                # Строим условие по PK
                if len(pk_columns) == 1:
                    pk_condition = pk_columns[0] == id_value
                else:
                    from sqlalchemy import and_
                    pk_condition = and_(*[col == val for col, val in zip(pk_columns, id_value)])
                
                stmt = update(table).where(pk_condition).values(**data)
                conn.execute(stmt)
                return id_value
    
    @staticmethod
    def delete(entity_name: str, id_value: Any, context: UMAContext) -> bool:
        """Удаление из таблицы"""
        from sqlalchemy import delete as sql_delete
        
        # Проверяем доступ
        existing = DefaultEntityHandler.read(entity_name, id_value, context)
        if existing is None:
            raise UmaAccessDeniedError(
                entity_name,
                "delete",
                f"No access to delete {entity_name}:{id_value}"
            )
        
        table = context.metadata.tables[entity_name]
        pk_columns = list(table.primary_key.columns)
        
        # Строим условие по PK
        if len(pk_columns) == 1:
            pk_condition = pk_columns[0] == id_value
        else:
            from sqlalchemy import and_
            pk_condition = and_(*[col == val for col, val in zip(pk_columns, id_value)])
        
        with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = conn.execute(stmt)
            return result.rowcount > 0
    
    @staticmethod
    def meta(entity_name: str, context: UMAContext) -> dict:
        """Метаданные из SQLAlchemy MetaData + обогащение из MetadataProvider"""
        table = context.metadata.tables[entity_name]
        
        columns = []
        for col in table.columns:
            col_info = {
                "name": col.name,
                "type": str(col.type),
                "nullable": col.nullable,
                "primary_key": col.primary_key,
            }
            
            # Внешние ключи
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info["related_to"] = fk.column.table.name
                col_info["related_key"] = fk.column.name
            
            columns.append(col_info)
        
        metadata = {
            "name": entity_name,
            "description": "",
            "is_virtual_view": False,
            "columns": columns
        }
        
        # Обогащаем из MetadataProvider
        try:
            provider_metadata = context.metadata_provider.get_metadata(entity_name, context)
            metadata.update(provider_metadata)
        except Exception:
            pass  # Если провайдер не может получить метаданные - не страшно
        
        return metadata
```

### Пример виртуального представления

```python
class UserSummaryView:
    """
    Виртуальное представление: сводка по пользователям.
    Метаданные хранятся как атрибуты класса.
    """
    
    # Атрибуты класса - метаданные
    is_virtual_view = True
    description = "Сводка по заказам пользователей"
    
    # Можно добавить дополнительные метаданные
    columns_metadata = [
        {"name": "user_id", "type": "integer", "description": "ID пользователя"},
        {"name": "name", "type": "string", "description": "Имя пользователя"},
        {"name": "order_count", "type": "integer", "description": "Количество заказов"},
        {"name": "total_spent", "type": "numeric", "description": "Общая сумма заказов"}
    ]
    
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        """Формирует выборку с агрегацией"""
        from sqlalchemy import select, func
        
        users = context.metadata.tables["users"]
        orders = context.metadata.tables["orders"]
        
        query = select(
            users.c.id.label("user_id"),
            users.c.name,
            func.count(orders.c.id).label("order_count"),
            func.coalesce(func.sum(orders.c.total), 0).label("total_spent")
        ).select_from(
            users.outerjoin(orders, users.c.id == orders.c.user_id)
        ).group_by(users.c.id, users.c.name)
        
        # Политика доступа через params
        if "user_id" in params:
            query = query.where(users.c.id == params["user_id"])
        
        # Можно использовать user_context для дополнительных ограничений
        if context.user_context:
            # Например, показывать только своего департамента
            user_dept = getattr(context.user_context, "department_id", None)
            if user_dept:
                query = query.where(users.c.department_id == user_dept)
        
        return query
    
    @staticmethod
    def read(entity_name: str, id_value: Any, context: UMAContext) -> dict | None:
        """Для виртуальных представлений read может не поддерживаться"""
        # Можно реализовать через select + фильтр
        query = UserSummaryView.select(entity_name, {"user_id": id_value}, context)
        
        with context.connection as conn:
            result = conn.execute(query).mappings().first()
            return dict(result) if result else None
    
    @staticmethod
    def save(entity_name: str, data: dict, context: UMAContext) -> Any:
        """Виртуальное представление - read-only"""
        raise UmaAccessDeniedError(entity_name, "write", "Virtual view is read-only")
    
    @staticmethod
    def delete(entity_name: str, id_value: Any, context: UMAContext) -> bool:
        """Виртуальное представление - read-only"""
        raise UmaAccessDeniedError(entity_name, "delete", "Virtual view is read-only")
    
    @staticmethod
    def meta(entity_name: str, context: UMAContext) -> dict:
        """Метаданные из атрибутов класса"""
        return {
            "name": entity_name,
            "description": UserSummaryView.description,
            "is_virtual_view": True,
            "columns": UserSummaryView.columns_metadata
        }
```

### EntityRegistry

```python
class EntityRegistry:
    """
    Реестр обработчиков сущностей.
    Поддерживает namespace для имён сущностей.
    """
    
    def __init__(self, default_handler: type[EntityHandler] = DefaultEntityHandler):
        # Явные регистрации: <namespace>:<entity_name> -> handler class
        self._handlers: dict[str, type[EntityHandler]] = {}
        
        # Handler по-умолчанию
        self._default_handler: type[EntityHandler] = default_handler
    
    def register(
        self,
        entity_name: str,
        handler: type[EntityHandler],
        namespace: str = "default"
    ):
        """
        Регистрирует handler для сущности.
        
        Args:
            entity_name: имя сущности (без namespace)
            handler: класс handler
            namespace: пространство имён (по умолчанию "default")
        """
        if not isinstance(handler, type):
            raise TypeError("handler must be a class, not instance")
        
        full_name = f"{namespace}:{entity_name}"
        self._handlers[full_name] = handler
    
    def get_handler(
        self,
        entity_name: str,
        context: UMAContext,
        namespace: str = "default"
    ) -> type[EntityHandler]:
        """
        Возвращает handler для сущности.
        
        Порядок разрешения:
        1. Явно зарегистрированный handler (с namespace)
        2. Таблица БД (без namespace) -> default handler
        3. ValueError
        
        Args:
            entity_name: имя сущности (может быть с namespace: "ns:name" или без)
            context: контекст
            namespace: namespace по умолчанию (используется если entity_name без namespace)
        """
        # Разбираем entity_name
        if ":" in entity_name:
            # Явно указан namespace
            full_name = entity_name
            _, pure_name = entity_name.split(":", 1)
        else:
            # Используем namespace по умолчанию
            full_name = f"{namespace}:{entity_name}"
            pure_name = entity_name
        
        # 1. Явные регистрации
        if full_name in self._handlers:
            return self._handlers[full_name]
        
        # 2. Таблицы БД (только для default namespace)
        if namespace == "default" and pure_name in context.metadata.tables:
            return self._default_handler
        
        # 3. Не найдено
        raise ValueError(f"Entity {full_name} not found")
    
    def is_registered(self, entity_name: str, namespace: str = "default") -> bool:
        """Проверяет, является ли сущность явно зарегистрированной"""
        if ":" in entity_name:
            full_name = entity_name
        else:
            full_name = f"{namespace}:{entity_name}"
        
        return full_name in self._handlers
    
    def is_virtual_view(
        self,
        entity_name: str,
        context: UMAContext,
        namespace: str = "default"
    ) -> bool:
        """Проверяет, является ли сущность виртуальным представлением"""
        try:
            handler = self.get_handler(entity_name, context, namespace)
            return getattr(handler, "is_virtual_view", False)
        except ValueError:
            return False
    
    def set_default_handler(self, handler: type[EntityHandler]):
        """Устанавливает handler по-умолчанию"""
        self._default_handler = handler
    
    def list_registered(self) -> list[str]:
        """Возвращает список всех зарегистрированных сущностей (с namespace)"""
        return list(self._handlers.keys())
```

### MetadataProvider - базовая реализация

```python
class DefaultMetadataProvider:
    """
    Базовая реализация провайдера метаданных.
    Может быть расширена в проекте для добавления кастомной логики.
    """
    
    def __init__(self, registry: EntityRegistry):
        self.registry = registry
        self._entities_cache: dict[str, list[str]] = {}  # workspace -> list of entities
        self._cache_timestamp: dict[str, float] = {}
        self._cache_ttl: float = 300.0  # 5 минут
    
    def get_metadata(self, entity_name: str, context: UMAContext) -> dict:
        """
        Возвращает дополнительные метаданные сущности.
        Базовая реализация возвращает пустой словарь.
        В проекте можно переопределить для загрузки из Redis/конфига.
        """
        # Пример: можно загружать переводы, дополнительные описания и т.д.
        return {}
    
    def list_entities(self, context: UMAContext, namespace: str = "default") -> list[str]:
        """
        Возвращает список всех доступных сущностей в формате <namespace>:<name>.
        Кэширует результат.
        """
        import time
        
        # Проверяем кэш
        if namespace in self._entities_cache:
            cache_age = time.time() - self._cache_timestamp.get(namespace, 0)
            if cache_age < self._cache_ttl:
                return self._entities_cache[namespace]
        
        # Собираем список
        entities = set()
        
        # 1. Зарегистрированные handlers
        for full_name in self.registry.list_registered():
            if full_name.startswith(f"{namespace}:"):
                entities.add(full_name)
        
        # 2. Таблицы БД (только для default namespace)
        if namespace == "default":
            for table_name in context.metadata.tables.keys():
                entities.add(f"{namespace}:{table_name}")
        
        result = sorted(entities)
        
        # Кэшируем
        self._entities_cache[namespace] = result
        self._cache_timestamp[namespace] = time.time()
        
        return result
    
    def can(
        self,
        entity_name: str,
        operation: Operation,
        context: UMAContext
    ) -> bool:
        """
        Проверяет доступ к операции.
        Базовая реализация разрешает всё.
        В проекте переопределить для реальной проверки прав.
        
        Raises:
            UmaAccessDeniedError если доступ запрещён
        """
        # Специальный случай: проверка доступа к списку сущностей
        if entity_name == "" and operation == "meta":
            # Разрешаем получение списка всем
            return True
        
        # Базовая реализация: всё разрешено
        # В проекте здесь должна быть реальная проверка на основе:
        # - context.user_context (роль, права пользователя)
        # - entity_name (какая сущность)
        # - operation (какая операция)
        
        return True
```

### API функции с проверкой доступа

```python
# Глобальные объекты (инициализируются при старте)
_registry: EntityRegistry = None
_context_template: UMAContext = None

def initialize_uma(
    engine: Engine,
    metadata: MetaData,
    metadata_provider: MetadataProvider = None,
    default_handler: type[EntityHandler] = None
) -> EntityRegistry:
    """
    Инициализация UMA.
    
    Args:
        engine: SQLAlchemy Engine
        metadata: SQLAlchemy MetaData (отрефлекченный)
        metadata_provider: провайдер метаданных (если None - создаётся DefaultMetadataProvider)
        default_handler: handler по-умолчанию (если None - используется DefaultEntityHandler)
    """
    global _registry, _context_template
    
    _registry = EntityRegistry(default_handler or DefaultEntityHandler)
    
    if metadata_provider is None:
        metadata_provider = DefaultMetadataProvider(_registry)
    
    _context_template = UMAContext(
        engine=engine,
        metadata=metadata,
        metadata_provider=metadata_provider
    )
    
    return _registry

def _create_context(user_context: Any = None) -> UMAContext:
    """Создаёт контекст для операции"""
    return UMAContext(
        engine=_context_template.engine,
        metadata=_context_template.metadata,
        metadata_provider=_context_template.metadata_provider,
        user_context=user_context
    )

def _check_access(entity_name: str, operation: Operation, context: UMAContext):
    """Проверяет доступ и выбрасывает исключение если доступ запрещён"""
    if not context.metadata_provider.can(entity_name, operation, context):
        raise UmaAccessDeniedError(entity_name, operation)

# ========== API функции ==========

def uma_select(
    jsql: dict,
    params: dict = None,
    user_context: Any = None,
    namespace: str = "default"
) -> dict:
    """Выполняет JSQL запрос"""
    context = _create_context(user_context)
    
    # Извлекаем entity_name из JSQL
    entity_name = jsql.get("from")
    if not entity_name:
        raise ValueError("JSQL must contain 'from' field")
    
    # Проверяем доступ к операции list
    _check_access(entity_name, "list", context)
    
    # Обрабатываем JSQL
    query = process_jsql(jsql, params or {}, context, _registry, namespace)
    
    # Выполняем
    with context.connection as conn:
        result = conn.execute(query)
        return {
            "metadata": {},  # TODO: заполнить метаданные результата
            "data": [dict(row) for row in result.mappings()]
        }

def uma_read(
    entity_name: str,
    id_value: Any,
    user_context: Any = None,
    namespace: str = "default"
) -> dict | None:
    """Читает запись"""
    context = _create_context(user_context)
    
    # Проверяем доступ
    _check_access(entity_name, "read", context)
    
    handler = _registry.get_handler(entity_name, context, namespace)
    return handler.read(entity_name, id_value, context)

def uma_save(
    entity_name: str,
    data: dict,
    user_context: Any = None,
    namespace: str = "default"
) -> Any:
    """Сохраняет запись"""
    context = _create_context(user_context)
    
    # Проверяем доступ
    _check_access(entity_name, "write", context)
    
    handler = _registry.get_handler(entity_name, context, namespace)
    return handler.save(entity_name, data, context)

def uma_delete(
    entity_name: str,
    id_value: Any,
    user_context: Any = None,
    namespace: str = "default"
) -> bool:
    """Удаляет запись"""
    context = _create_context(user_context)
    
    # Проверяем доступ
    _check_access(entity_name, "delete", context)
    
    handler = _registry.get_handler(entity_name, context, namespace)
    return handler.delete(entity_name, id_value, context)

def uma_meta(
    entity_name: str,
    user_context: Any = None,
    namespace: str = "default"
) -> dict:
    """Возвращает метаданные сущности"""
    context = _create_context(user_context)
    
    # Проверяем доступ
    _check_access(entity_name, "meta", context)
    
    handler = _registry.get_handler(entity_name, context, namespace)
    return handler.meta(entity_name, context)

def uma_list_entities(
    user_context: Any = None,
    namespace: str = "default"
) -> list[str]:
    """
    Возвращает список всех доступных сущностей.
    Результат кэшируется в MetadataProvider.
    """
    context = _create_context(user_context)
    
    # Проверяем доступ к получению списка
    _check_access("", "meta", context)
    
    return context.metadata_provider.list_entities(context, namespace)
```

### Пример использования

```python
from sqlalchemy import create_engine, MetaData

# ========== Инициализация ==========

engine = create_engine("postgresql://...")
metadata = MetaData()
metadata.reflect(bind=engine)

# Создаём кастомный MetadataProvider с проверкой прав
class MyMetadataProvider(DefaultMetadataProvider):
    def can(self, entity_name: str, operation: Operation, context: UMAContext) -> bool:
        user = context.user_context
        
        # Специальный случай: список сущностей
        if entity_name == "" and operation == "meta":
            return user is not None  # Только авторизованные
        
        # Проверяем права на основе роли пользователя
        if not user:
            return False
        
        user_role = getattr(user, "role", "guest")
        
        # Пример логики прав
        if user_role == "admin":
            return True  # Админу всё можно
        elif user_role == "user":
            # Обычному пользователю: read/list - да, write/delete - нет
            return operation in ("list", "read", "meta")
        else:
            return False

# Инициализируем UMA
registry = initialize_uma(
    engine,
    metadata,
    metadata_provider=MyMetadataProvider(None)  # registry будет установлен автоматически
)
MyMetadataProvider(_registry)  # Передаём registry в провайдер

# ========== Регистрация handlers ==========

# 1. Обычная таблица с ограничением выборки
class RestrictedUserHandler(DefaultEntityHandler):
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        from sqlalchemy import select
        table = context.metadata.tables[entity_name]
        query = select(table).where(table.c.active == True)
        return query

registry.register("users", RestrictedUserHandler)

# 2. Виртуальное представление
registry.register("user_summary", UserSummaryView)

# 3. Кастомный handler по-умолчанию с логированием
class LoggingDefaultHandler(DefaultEntityHandler):
    @staticmethod
    def select(entity_name: str, params: dict[str, Any], context: UMAContext) -> Select:
        print(f"[LOG] Selecting from {entity_name}")
        return DefaultEntityHandler.select(entity_name, params, context)

registry.set_default_handler(LoggingDefaultHandler)

# ========== Использование ==========

# Создаём user context
class User:
    def __init__(self, id, role):
        self.id = id
        self.role = role

current_user = User(id=123, role="user")

# 1. Получение списка сущностей
try:
    entities = uma_list_entities(user_context=current_user)
    print(f"Available entities: {entities}")
except UmaAccessDeniedError as e:
    print(f"Access denied: {e}")

# 2. Чтение записи
user = uma_read("users", 456, user_context=current_user)

# 3. JSQL запрос с виртуальным представлением (будет CTE)
jsql = {
    "select": ["*"],
    "from": "user_summary",
    "params": {"user_id": 123}
}
summary = uma_select(jsql, user_context=current_user)

# 4. Попытка записи (будет UmaAccessDeniedError для role="user")
try:
    uma_save("users", {"id": 456, "name": "New Name"}, user_context=current_user)
except UmaAccessDeniedError as e:
    print(f"Cannot save: {e}")
```

## 3. Итоговая оценка архитектуры

### ✅ Решённые вопросы:

1. **Единый EntityHandler** - нет разделения на отдельные интерфейсы
2. **MetadataProvider.can()** - централизованная проверка доступа
3. **MetadataProvider.list_entities()** - перечисление сущностей с кэшированием
4. **Namespace** - поддержка `<namespace>:<entity_name>`
5. **is_virtual_view** - атрибут класса для определения виртуальности
6. **Дедупликация CTE** - через `context.get_or_create_cte()`
7. **Явная передача контекста** - `UMAContext` в каждый вызов
8. **set_default_handler()** - явная регистрация

### 💡 Ключевые преимущества:

1. **Гибкость**: MetadataProvider можно расширить для любой логики прав и метаданных
2. **Чистота**: Stateless handlers, явные зависимости
3. **Масштабируемость**: Кэширование списка сущностей, дедупликация CTE
4. **Безопасность**: Проверка доступа на уровне библиотеки через `can()`
5. **Простота**: Fallback на default handler минимизирует boilerplate

### 🤔 Вопросы для обсуждения:

1. **Контекстные переменные**: Хотите ли использовать `contextvars` для передачи `UMAContext` неявно? Это упростит сигнатуры, но сделает зависимости менее явными.

2. **Формат namespace**: `<namespace>:<name>` или можно другой формат? Например `<namespace>.<name>` или `<namespace>/<name>`?

3. **Кэш TTL**: 5 минут для кэша списка сущностей - подходит? Должен ли быть настраиваемым?

4. **Проверка is_virtual_view**: Сейчас через атрибут класса. Может быть полезно добавить метод `EntityHandler.is_virtual_view()` для динамического определения?

5. **Порядок проверки прав**: Сейчас проверка в API функциях (`uma_read`, `uma_save`, etc.). Нужна ли дополнительная проверка внутри handlers?

Готов к следующему этапу - начать создание кода или обсудить оставшиеся детали!

---

**User**

(20260104-020125) (20260104-140013)

(20260104-131512) (ВАЖНО!) Пожалуйста, прими во внимание, что следует создавать асинхронную реализацию библиотеки.

(20260104-020952)
> ```python
> class EntityHandler(Protocol):
>     # Атрибуты класса для виртуальных представлений
>     is_virtual_view: bool = False
> ```

Я думаю, `is_virtual_view` лучше разместить в метаданных, и обрабатывать только в случае, если этот атрибут присутствует.

(20260104-020956)
> Реализация DefaultEntityHandler

Хм... В методе `save` вижу такое:
```
else:
    # UPDATE
    # Проверяем доступ
    existing = DefaultEntityHandler.read(entity_name, id_value, context)
    if existing is None:
        raise UmaAccessDeniedError(
            entity_name,
            "write",
            f"No access to update {entity_name}:{id_value}"
        )

```
(.1) Это неправильно. От `DefaultEntityHandler` в проекте будут наследовать, будут перекрывать и `read` и `select`.
Здесь нужно, чтобы был вызван `read` того же класса, которому принадлежит `save`.
Соответственно, `@staticmethod` для UMA-операций в `EntityHandler` не представляется мне хорошей идеей. Думаю, эти методы следует декорировать `classmethod`. Что скажешь?

(.2) `read` никогда не должен возвращать `None`: эта и все остальные функции должны работать по принципу – или результат работы (тело прочитанной записи в данном случае), или выброс соответствующего возникшей проблеме исключения.

(.3) Этот фрагмент кода, который я процитировал выше у тебя дублируется (ниже, в `delete`), что совсем не есть хорошо.

(20260104-022042)
```python
class UserSummaryView:
    ... ... ...
    # Можно добавить дополнительные метаданные
    columns_metadata = [
        {"name": "user_id", "type": "integer", "description": "ID пользователя"},
        {"name": "name", "type": "string", "description": "Имя пользователя"},
        {"name": "order_count", "type": "integer", "description": "Количество заказов"},
        {"name": "total_spent", "type": "numeric", "description": "Общая сумма заказов"}
    ]
```
По-моему, здесь пригодились бы удобные сервисные функции, которых позволили бы скопировать метаданные поля из другой сущности, например, содержащей соответствующее поле:
```python
    columns_metadata = [
        ... ... ...
        copy_field_meta('users', 'name'),
        ... ... ...
    ]
```

(20260104-124447)
> Пример виртуального представления

```python
class UserSummaryView:
    ... ... ...
    def save(entity_name: str, data: dict, context: UMAContext) -> Any:
        """Виртуальное представление - read-only"""
        raise UmaAccessDeniedError(entity_name, "write", "Virtual view is read-only")
```
Думаю, такое поведение (выбрасывать `UmaAccessDeniedError`/`UmaNotImplementedError` с детализацией, описывающей контекст) следует реализовать в базовом классе для Entity Handlers. Или в базовом классе для виртуальных представлений, поскольку, действительно, `read` и CRUD-методы там чаще всего реализованы не будут, и не хочется дублировать код, повторяясь с реализацией этих методов только потому, что они определены в интерфейсе `EntityHandler`.

(20260104-125753)
```python
        # Разбираем entity_name
        if ":" in entity_name:
            # Явно указан namespace
            full_name = entity_name
            _, pure_name = entity_name.split(":", 1)
        else:
            # Используем namespace по умолчанию
            full_name = f"{namespace}:{entity_name}"
            pure_name = entity_name
```
Думаю, этот код является самоценным, и его следует вынести в функцию `parse_entity_name`, возвращающую dataclass, содержащий `workspace` и `entity` – соответственно, пространство имён (например, схема в PostgreSQL) и имя сущности в нём.
Далее, можно добавить обратную функцию `form_entity_name`, которая будет принимать этот dataclass в качестве параметра, и возвращать строковое представление, отражающее наши правила формата квалифицированного имени: `<workspace>:<entity>`.

Дополнительно, предлагаю считать запись `default:users` и `users` эквивалентными. Это избавить от бойлерплейта в обычных сценариях.

(20260104-133346)
```python
    def can(
        self,
        entity_name: str,
        operation: Operation,
        context: UMAContext
    ) -> bool:
```
С базовой реализацией `can` я согласен, единственное, предлагаю заменить `entity_name` → `target`, чтобы иметь свободу трактовки этого параметра: в конце концов, мы не указываем имя сущности, когда запрашиваем полномочия на доступ к перечислению этих имён.

(20260104-133844)
> 🤔 Вопросы для обсуждения:
> Контекстные переменные: Хотите ли использовать contextvars для передачи UMAContext неявно? Это упростит сигнатуры, но сделает зависимости менее явными.

Давай пока без context vars... Не люблю магию. Явное лучше неявного ⓒ

> Формат namespace: <namespace>:<name> или можно другой формат? Например <namespace>.<name> или <namespace>/<name>?

Думаю, я ответил выше, в разделе `(20260104-125753)`. Слэш интересная альтенатива, возможно мы к ней вернёмся, но позже, и в другом контексте.

> Кэш TTL: 5 минут для кэша списка сущностей - подходит? Должен ли быть настраиваемым?

Думаю, на текущем этапе можно обойтись без TTL. Структура метаданных меняется редко (обычно deploy), в этих случаях может оказаться более простым решением просто перезагрузить сервис.

> Проверка is_virtual_view: Сейчас через атрибут класса. Может быть полезно добавить метод EntityHandler.is_virtual_view() для динамического определения?

Думаю, из атрибутов класса это следует убрать, вместо этого сделать функцию, которая будет анализировать метаданные сущности: должен присутствовать атрибут `is_virtual_view`, и он должен содержать значение `True`, чтобы сущность считалась виртуальным представлением.
Кроме того, для entity handlers виртуальных представлений можно создать базовый класс, который обеспечит появление в метадажнных этого атрибута.

> Порядок проверки прав: Сейчас проверка в API функциях (uma_read, uma_save, etc.). Нужна ли дополнительная проверка внутри handlers?

Сейчас нет. Сейчас мне известно по теме следующее:
1) Определять в проекте entity handlers и `can` – легитимный путь расширения UMA "снаружи".
2) В методах проектных entity handlers нормально вызывать `can` из UMA, но там может вызываться и проектный `can` с другим набором параметров (например, `user_id`, правила вычисления которого находятся за рамками компетенций UMA) для использования которых потребуется контекст.
3) Соответственно, нам нужен механизм расширения `Context` UMA, но этим займёмся позже.
4) Обращаться к функциональности entity handlers из проектного кода напрямую (не через обёртки вроде `uma_save`) – создавать брешь в системе безопасности и сейчас для меня это антипаттерн.
5) Нагружать проверками полномочий реализацию entity handlers в UMA – провоцировать "гонку" при реализации проектных entity handlers, поскольку некоторые вещи наверняка захочется выполнять как до, так и после проверки полномочий, а о том, какова должна быть эта проверка – на уровне UMA мы знать не знаем.
—

Пожалуйста, уточни свои вопросы, подсветку рисков и предложения в свете обозначенного выше.


---

**Cursor**

Отлично! Принимаю все замечания. Самое важное изменение - **асинхронная реализация**. Давайте пересмотрим архитектуру с учётом всех уточнений.

## Пересмотр архитектуры с учётом async и замечаний

### 1. Исключения UMA

```python
class UmaError(Exception):
    """Базовый класс для исключений UMA"""
    pass

class UmaAccessDeniedError(UmaError, PermissionError):
    """Исключение при отказе в доступе"""
    def __init__(self, target: str, operation: str, message: str = None):
        self.target = target
        self.operation = operation
        super().__init__(message or f"Access denied to {operation} on {target}")

class UmaNotFoundError(UmaError, ValueError):
    """Исключение когда сущность/запись не найдена"""
    def __init__(self, target: str, message: str = None):
        self.target = target
        super().__init__(message or f"Not found: {target}")

class UmaNotImplementedError(UmaError, NotImplementedError):
    """Исключение когда операция не реализована для сущности"""
    def __init__(self, target: str, operation: str, message: str = None):
        self.target = target
        self.operation = operation
        super().__init__(message or f"Operation {operation} not implemented for {target}")
```

### 2. Утилиты для работы с квалифицированными именами

```python
from dataclasses import dataclass
from typing import Optional

@dataclass(frozen=True)
class EntityName:
    """
    Квалифицированное имя сущности.
    Формат: <namespace>:<entity>
    """
    namespace: str
    entity: str
    
    def __str__(self) -> str:
        """Строковое представление в формате namespace:entity"""
        return form_entity_name(self)
    
    @property
    def is_default_namespace(self) -> bool:
        """Проверяет, является ли namespace дефолтным"""
        return self.namespace == "default"

def parse_entity_name(name: str, default_namespace: str = "default") -> EntityName:
    """
    Парсит квалифицированное имя сущности.
    
    Args:
        name: имя в формате "namespace:entity" или просто "entity"
        default_namespace: namespace по умолчанию (если не указан в name)
        
    Returns:
        EntityName с разобранным namespace и entity
        
    Examples:
        parse_entity_name("users") -> EntityName(namespace="default", entity="users")
        parse_entity_name("default:users") -> EntityName(namespace="default", entity="users")
        parse_entity_name("schema1:users") -> EntityName(namespace="schema1", entity="users")
    """
    if ":" in name:
        namespace, entity = name.split(":", 1)
        return EntityName(namespace=namespace, entity=entity)
    else:
        return EntityName(namespace=default_namespace, entity=name)

def form_entity_name(entity_name: EntityName, omit_default: bool = True) -> str:
    """
    Формирует строковое представление квалифицированного имени.
    
    Args:
        entity_name: объект EntityName
        omit_default: если True, то для default namespace возвращает просто entity
        
    Returns:
        Строка в формате "namespace:entity" или "entity" (для default namespace)
        
    Examples:
        form_entity_name(EntityName("default", "users")) -> "users"
        form_entity_name(EntityName("default", "users"), omit_default=False) -> "default:users"
        form_entity_name(EntityName("schema1", "users")) -> "schema1:users"
    """
    if omit_default and entity_name.namespace == "default":
        return entity_name.entity
    return f"{entity_name.namespace}:{entity_name.entity}"
```

### 3. Типы и протоколы (async)

```python
from typing import Protocol, Any, runtime_checkable, Literal
from sqlalchemy.ext.asyncio import AsyncEngine, AsyncConnection
from sqlalchemy import MetaData, Select

Operation = Literal["list", "read", "write", "delete", "meta"]

# ================== MetadataProvider (async) ==================

@runtime_checkable
class MetadataProvider(Protocol):
    """
    Провайдер метаданных и контроля доступа.
    Все методы асинхронные.
    """
    
    async def get_metadata(self, entity_name: EntityName, context: 'UMAContext') -> dict:
        """
        Возвращает метаданные сущности.
        Может обогащать метаданные из разных источников (БД, Redis, конфиг).
        """
        ...
    
    async def list_entities(self, context: 'UMAContext', namespace: str = "default") -> list[str]:
        """
        Возвращает список всех доступных сущностей в формате <namespace>:<entity>.
        Результат кэшируется провайдером.
        """
        ...
    
    async def can(
        self,
        target: str,  # entity_name или "" для проверки списка
        operation: Operation,
        context: 'UMAContext'
    ) -> bool:
        """
        Проверяет доступ к операции.
        
        Args:
            target: имя сущности (или "" для проверки доступа к списку)
            operation: тип операции (list/read/write/delete/meta)
            context: контекст выполнения
            
        Returns:
            True если доступ разрешён
            
        Raises:
            UmaAccessDeniedError если доступ запрещён
            
        Специальные случаи:
        - target="" + operation="meta" -> проверка доступа к списку сущностей
        """
        ...

# ================== Context (async) ==================

@dataclass
class UMAContext:
    """
    Контекст выполнения UMA операций.
    Передаётся в каждый вызов методов EntityHandler.
    """
    engine: AsyncEngine
    metadata: MetaData
    metadata_provider: MetadataProvider
    
    # Дополнительный контекст пользователя (для проверки прав)
    user_context: Any = None
    
    # Опциональный кэш
    cache: Any = None
    
    # Словарь для хранения CTE (дедупликация)
    _cte_cache: dict[str, Any] = field(default_factory=dict)
    
    def get_or_create_cte(self, entity_name: EntityName, select_fn) -> Any:
        """
        Получает CTE из кэша или создаёт новый.
        Обеспечивает дедупликацию CTE для виртуальных представлений.
        
        Args:
            entity_name: имя сущности
            select_fn: функция, возвращающая Select (вызывается только если CTE нет в кэше)
        """
        key = str(entity_name)
        if key not in self._cte_cache:
            select_query = select_fn()
            self._cte_cache[key] = select_query.cte(name=entity_name.entity)
        return self._cte_cache[key]

# ================== EntityHandler Protocol (async, classmethod) ==================

@runtime_checkable
class EntityHandler(Protocol):
    """
    Протокол для обработчика сущностей.
    Все методы классовые (@classmethod) - это позволяет вызывать методы своего класса при наследовании.
    Все методы асинхронные.
    """
    
    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: UMAContext
    ) -> Select:
        """
        Возвращает SQLAlchemy Select для выборки данных.
        Инкапсулирует политики доступа (фильтрация строк).
        
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
    
    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext
    ) -> dict:
        """
        Читает одну запись по id.
        Использует select() для применения политик доступа.
        
        Returns:
            Словарь с данными записи
            
        Raises:
            UmaNotFoundError если запись не найдена или нет доступа
            
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
    
    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext
    ) -> Any:
        """
        Сохраняет запись (create если id=None, иначе update).
        Возвращает id сохранённой записи.
        
        Raises:
            UmaNotImplementedError если операция не поддерживается
            
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
    
    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext
    ) -> bool:
        """
        Удаляет запись по id.
        
        Returns:
            True если запись удалена
            
        Raises:
            UmaNotFoundError если запись не найдена или нет доступа
            UmaNotImplementedError если операция не поддерживается
            
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
    
    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext
    ) -> dict:
        """
        Возвращает метаданные сущности.
        Может использовать context.metadata_provider для обогащения метаданных.
        
        Note:
            Перед вызовом библиотека проверяет доступ через context.metadata_provider.can()
        """
        ...
```

### 4. Утилиты для работы с метаданными

```python
async def copy_field_meta(
    source_entity: str | EntityName,
    field_name: str,
    context: UMAContext,
    overrides: dict = None
) -> dict:
    """
    Копирует метаданные поля из другой сущности.
    
    Args:
        source_entity: имя сущности-источника
        field_name: имя поля для копирования
        context: контекст UMA
        overrides: словарь для переопределения отдельных атрибутов
        
    Returns:
        Словарь с метаданными поля
        
    Example:
        columns_metadata = [
            await copy_field_meta('users', 'id', context),
            await copy_field_meta('users', 'name', context, {'description': 'Имя в сводке'}),
            {"name": "order_count", "type": "integer", "description": "Количество заказов"}
        ]
    """
    if isinstance(source_entity, str):
        source_entity = parse_entity_name(source_entity)
    
    # Получаем handler источника
    from .registry import get_global_registry  # будет определено позже
    registry = get_global_registry()
    handler = registry.get_handler(source_entity, context)
    
    # Получаем метаданные
    source_meta = await handler.meta(source_entity, context)
    
    # Ищем поле
    for col in source_meta.get("columns", []):
        if col["name"] == field_name:
            result = col.copy()
            if overrides:
                result.update(overrides)
            return result
    
    raise ValueError(f"Field {field_name} not found in {source_entity}")

def is_virtual_view(metadata: dict) -> bool:
    """
    Проверяет, является ли сущность виртуальным представлением.
    
    Args:
        metadata: словарь метаданных сущности
        
    Returns:
        True если is_virtual_view присутствует и равен True
    """
    return metadata.get("is_virtual_view", False) is True
```

### 5. Базовые классы EntityHandler

```python
class DefaultEntityHandler:
    """
    Обработчик по-умолчанию для обычных таблиц БД.
    Используется как fallback для незарегистрированных сущностей.
    """
    
    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: UMAContext
    ) -> Select:
        """Простой SELECT * FROM table"""
        from sqlalchemy import select
        
        table = context.metadata.tables[entity_name.entity]
        query = select(table)
        
        return query
    
    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext
    ) -> dict:
        """
        Чтение через select + фильтр по id.
        Не возвращает None - выбрасывает UmaNotFoundError.
        """
        # Получаем select (может быть переопределен в подклассе с политиками)
        query = await cls.select(entity_name, {}, context)
        
        # Добавляем условие по id
        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)
        
        if len(pk_columns) == 1:
            query = query.where(pk_columns[0] == id_value)
        else:
            from sqlalchemy import and_
            conditions = [col == val for col, val in zip(pk_columns, id_value)]
            query = query.where(and_(*conditions))
        
        # Выполняем
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()
            
            if row is None:
                raise UmaNotFoundError(
                    str(entity_name),
                    f"Record with id={id_value} not found or access denied"
                )
            
            return dict(row)
    
    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext
    ) -> Any:
        """
        Сохранение в таблицу.
        Убрана дублирующаяся проверка доступа - она будет в _ensure_exists().
        """
        from sqlalchemy import insert, update
        
        table = context.metadata.tables[entity_name.entity]
        pk_columns = list(table.primary_key.columns)
        
        # Определяем create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)
        
        is_create = (id_value is None or (isinstance(id_value, tuple) and None in id_value))
        
        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)
                
                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                else:
                    return tuple(result.inserted_primary_key)
            else:
                # UPDATE
                # Проверяем доступ через read (DRY - избегаем дублирования)
                await cls._ensure_exists(entity_name, id_value, context)
                
                # Строим условие по PK
                pk_condition = cls._build_pk_condition(pk_columns, id_value)
                
                stmt = update(table).where(pk_condition).values(**data)
                await conn.execute(stmt)
                return id_value
    
    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext
    ) -> bool:
        """
        Удаление из таблицы.
        Использует _ensure_exists() для проверки доступа (DRY).
        """
        from sqlalchemy import delete as sql_delete
        
        # Проверяем доступ через read (DRY)
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
        context: UMAContext
    ) -> dict:
        """Метаданные из SQLAlchemy MetaData + обогащение из MetadataProvider"""
        table = context.metadata.tables[entity_name.entity]
        
        columns = []
        for col in table.columns:
            col_info = {
                "name": col.name,
                "type": str(col.type),
                "nullable": col.nullable,
                "primary_key": col.primary_key,
            }
            
            # Внешние ключи
            if col.foreign_keys:
                fk = list(col.foreign_keys)[0]
                col_info["related_to"] = fk.column.table.name
                col_info["related_key"] = fk.column.name
            
            columns.append(col_info)
        
        metadata = {
            "name": str(entity_name),
            "description": "",
            "columns": columns
        }
        
        # Обогащаем из MetadataProvider
        try:
            provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
            metadata.update(provider_metadata)
        except Exception:
            pass
        
        return metadata
    
    # ========== Вспомогательные методы (DRY) ==========
    
    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext
    ) -> dict:
        """
        Проверяет существование записи и доступ к ней через read().
        Вынесено в отдельный метод для избежания дублирования в save/delete.
        
        Returns:
            Словарь с данными записи
            
        Raises:
            UmaNotFoundError если запись не найдена или нет доступа
        """
        return await cls.read(entity_name, id_value, context)
    
    @staticmethod
    def _build_pk_condition(pk_columns: list, id_value: Any):
        """
        Строит условие WHERE для первичного ключа.
        Поддерживает простые и составные ключи.
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value
        else:
            from sqlalchemy import and_
            return and_(*[col == val for col, val in zip(pk_columns, id_value)])


class VirtualViewHandler:
    """
    Базовый класс для виртуальных представлений.
    Предоставляет дефолтные реализации, выбрасывающие UmaNotImplementedError.
    Автоматически добавляет is_virtual_view=True в метаданные.
    """
    
    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: UMAContext
    ) -> Select:
        """
        Должен быть переопределён в подклассе.
        """
        raise UmaNotImplementedError(
            str(entity_name),
            "select",
            f"Virtual view {entity_name} must implement select()"
        )
    
    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext
    ) -> dict:
        """
        По умолчанию не поддерживается для виртуальных представлений.
        Можно переопределить в подклассе если нужно.
        """
        raise UmaNotImplementedError(
            str(entity_name),
            "read",
            f"Virtual view {entity_name} does not support read operation"
        )
    
    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,
        context: UMAContext
    ) -> Any:
        """Виртуальные представления по умолчанию read-only"""
        raise UmaNotImplementedError(
            str(entity_name),
            "write",
            f"Virtual view {entity_name} is read-only"
        )
    
    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext
    ) -> bool:
        """Виртуальные представления по умолчанию read-only"""
        raise UmaNotImplementedError(
            str(entity_name),
            "delete",
            f"Virtual view {entity_name} is read-only"
        )
    
    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext
    ) -> dict:
        """
        Базовая реализация метаданных.
        Автоматически добавляет is_virtual_view=True.
        Должна быть расширена в подклассе для описания columns.
        """
        return {
            "name": str(entity_name),
            "description": getattr(cls, "description", ""),
            "is_virtual_view": True,
            "columns": getattr(cls, "columns_metadata", [])
        }
```

### 6. Пример использования

```python
# ========== Пример виртуального представления ==========

class UserSummaryView(VirtualViewHandler):
    """
    Виртуальное представление: сводка по пользователям.
    """
    
    description = "Сводка по заказам пользователей"
    
    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],
        context: UMAContext
    ) -> Select:
        from sqlalchemy import select, func
        
        users = context.metadata.tables["users"]
        orders = context.metadata.tables["orders"]
        
        query = select(
            users.c.id.label("user_id"),
            users.c.name,
            func.count(orders.c.id).label("order_count"),
            func.coalesce(func.sum(orders.c.total), 0).label("total_spent")
        ).select_from(
            users.outerjoin(orders, users.c.id == orders.c.user_id)
        ).group_by(users.c.id, users.c.name)
        
        # Политика доступа через params
        if "user_id" in params:
            query = query.where(users.c.id == params["user_id"])
        
        return query
    
    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext
    ) -> dict:
        """Переопределяем read для поддержки чтения по user_id"""
        query = await cls.select(entity_name, {"user_id": id_value}, context)
        
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()
            
            if row is None:
                raise UmaNotFoundError(
                    str(entity_name),
                    f"User summary with id={id_value} not found"
                )
            
            return dict(row)
    
    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext
    ) -> dict:
        """Метаданные с использованием copy_field_meta()"""
        
        # Можем использовать copy_field_meta для повторного использования метаданных
        columns_metadata = [
            await copy_field_meta("users", "id", context, {"name": "user_id"}),
            await copy_field_meta("users", "name", context),
            {"name": "order_count", "type": "integer", "description": "Количество заказов"},
            {"name": "total_spent", "type": "numeric", "description": "Общая сумма заказов"}
        ]
        
        return {
            "name": str(entity_name),
            "description": cls.description,
            "is_virtual_view": True,
            "columns": columns_metadata
        }
```

### 7. Вопросы и предложения

**Вопрос 1: Материализация CTE в PostgreSQL**

Вы были правы - это специфика PostgreSQL 12+, не нужно на неё завязываться. SQLAlchemy создаёт обычные CTE, которые работают кросс-платформенно.

**Вопрос 2: Структура проекта**

Предлагаю следующую структуру:

```
uma/
├── __init__.py              # Public API
├── core/
│   ├── __init__.py
│   ├── types.py            # EntityName, Operation, Protocols
│   ├── context.py          # UMAContext
│   ├── exceptions.py       # UmaError, UmaAccessDeniedError, etc.
│   └── utils.py            # parse_entity_name, form_entity_name, copy_field_meta
├── handlers/
│   ├── __init__.py
│   ├── base.py             # DefaultEntityHandler
│   └── virtual.py          # VirtualViewHandler
├── registry.py              # EntityRegistry
├── metadata.py              # DefaultMetadataProvider
└── api.py                   # uma_select, uma_read, uma_save, etc.
```

**Вопрос 3: Расширение Context в проектах**

Для расширения `UMAContext` в проектах можно использовать:

```python
# В проекте
class ProjectContext(UMAContext):
    project_config: Any = None
    custom_cache: Any = None

# При инициализации
context = ProjectContext(
    engine=engine,
    metadata=metadata,
    metadata_provider=provider,
    project_config=my_config
)
```

Все методы handlers будут получать этот расширенный контекст.

**Предложение: Дополнительные утилиты**

Возможно, будут полезны:

```python
async def get_entity_metadata(
    entity_name: str | EntityName,
    context: UMAContext
) -> dict:
    """
    Удобная функция для получения метаданных без прямого обращения к handler.
    """
    if isinstance(entity_name, str):
        entity_name = parse_entity_name(entity_name)
    
    registry = get_global_registry()
    handler = registry.get_handler(entity_name, context)
    return await handler.meta(entity_name, context)
```

Готов продолжить разработку. Начинать с создания файлов кода?

