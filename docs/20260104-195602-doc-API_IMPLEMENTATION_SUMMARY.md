# API Implementation Summary

## ✅ Реализовано

В дополнение к базовой инфраструктуре, был создан полноценный API-слой.

### Новые файлы

1. **`src/namerec/uma/api.py`** (320 строк)
   - Глобальные объекты `_registry` и `_context_template`
   - 8 публичных функций
   - Проверка доступа через `can()` метод

2. **`examples/api_usage.py`** (300+ строк)
   - Полный пример использования API
   - Реализация `AccessControlMetadataProvider` с ролями
   - Демонстрация всех операций CRUD
   - Проверка прав доступа

3. **`tests/test_api.py`**
   - Тесты для всех API функций
   - Покрытие успешных сценариев

4. **`API_SUMMARY.md`**
   - Полная документация API
   - Примеры использования
   - Best practices

### Обновлённые файлы

1. **`src/namerec/uma/__init__.py`**
   - Добавлены экспорты всех API функций

2. **`src/namerec/uma/core/types.py`**
   - Добавлена операция `OP_LIST`

3. **`src/namerec/uma/metadata.py`**
   - Добавлен метод `can()` для проверки доступа
   - Базовая реализация разрешает всё

4. **Документация**
   - Обновлены QUICKSTART.md, VERIFICATION.md, CREATED_FILES_SUMMARY.md

## API Методы

### 1. `UMA.create()`

```python
from namerec.uma import UMA, DefaultMetadataProvider, NamespaceConfig

uma = UMA.create({
    'main': NamespaceConfig(
        engine=engine,
        metadata_provider=DefaultMetadataProvider(),
    ),
})
```

**Назначение**: Инициализация UMA при старте приложения

**Возвращает**: `UMA` instance

### 2. `uma.read()`

```python
user = await uma.read(
    entity_name='users',
    id_value=123,
    user_context=current_user,  # Optional
    namespace=None,             # Optional
)
```

**Назначение**: Чтение записи по ID

**Возвращает**: `dict` с данными записи

**Исключения**: `UMAAccessDeniedError`, `UMANotFoundError`

### 3. `uma.save()`

```python
# Create
user_id = await uma.save('users', {'name': 'John'})

# Update
await uma.save('users', {'id': 123, 'name': 'John Smith'})
```

**Назначение**: Создание или обновление записи

**Возвращает**: ID сохранённой записи

**Логика**: Если в `data` есть `id` - UPDATE, иначе CREATE

### 4. `uma.delete()`

```python
deleted = await uma.delete('users', 123, user_context=current_user)
```

**Назначение**: Удаление записи по ID

**Возвращает**: `bool` - True если удалено

### 5. `uma.entity_details()`

```python
metadata = await uma.entity_details('users')
print(metadata['columns'])
```

**Назначение**: Получение метаданных сущности

**Возвращает**: `dict` с метаданными (columns, description, etc.)

### 6. `uma.entity_list()`

```python
entities = await uma.entity_list(
    user_context=current_user,
    namespace='virtual',  # Optional filter
)
```

**Назначение**: Список всех доступных сущностей

**Возвращает**: `list[str]` - имена сущностей

### 7. `uma.select()` ✅

```python
# JSQL query execution
result = await uma.select(
    jsql={
        'from': 'users',
        'select': ['id', 'name'],
        'where': {
            'op': '=',
            'left': {'field': 'active'},
            'right': {'value': True}
        }
    },
    params={},
    user_context=current_user,
)
```

**Статус**: ✅ Fully implemented

**Реализация**: 
- Парсер JSQL → SQLAlchemy (`jsql/parser.py`)
- Executor для выполнения (`jsql/executor.py`)
- Построитель результатов с метаданными (`jsql/result.py`)

**Документация**: См. [JSQL Specification](20260104-220450-doc-JSQL_SPECIFICATION.md)

**Поддерживаемые возможности**:
- Все типы SELECT запросов
- WHERE с операторами, AND/OR/NOT
- JOINs всех типов
- Агрегации и GROUP BY
- ORDER BY (включая по алиасам)
- CTEs, подзапросы, оконные функции
- Параметризованные запросы

### 8. `uma.registry`

```python
# Registry доступен как атрибут UMA instance
uma.registry.register('custom_view', MyViewHandler)
```

**Назначение**: Регистрация виртуальных представлений и кастомных обработчиков

## Контроль Доступа

### Реализация `can()` метода

```python
from namerec.uma import DefaultMetadataProvider, OP_LIST, OP_META, OP_READ, UMAContext

class MyMetadataProvider(DefaultMetadataProvider):
    async def can(
        self,
        entity_name: str,
        operation: str,
        context: UMAContext,
    ) -> bool:
        user = context.user_context
        
        if not user:
            return False
        
        if user.role == 'admin':
            return True
        
        if user.role == 'user':
            return operation in (OP_READ, OP_META, OP_LIST)
        
        return False
```

### Использование с User Context

```python
class User:
    def __init__(self, user_id, role):
        self.id = user_id
        self.role = role

admin = User(1, 'admin')
user = User(2, 'user')

# Админ может всё
await uma.save('users', {'name': 'Admin'}, user_context=admin)

# Пользователь только читать
user_data = await uma.read('users', 1, user_context=user)

# Пользователь не может писать
try:
    await uma.save('users', {'name': 'Hack'}, user_context=user)
except UMAAccessDeniedError:
    print("Доступ запрещён!")
```

## Типы Операций

```python
from namerec.uma import OP_CREATE, OP_DELETE, OP_LIST, OP_META, OP_READ, OP_UPDATE

OP_READ    # Чтение/SELECT (включая JSQL)
OP_CREATE  # Создание записи
OP_UPDATE  # Обновление записи
OP_DELETE  # Удаление записи
OP_META    # Получение метаданных
OP_LIST    # Список сущностей
```

## Примеры Запуска

```bash
# API пример с контролем доступа
uv run python examples/api_usage.py

# Базовый пример (низкоуровневый API)
uv run python examples/basic_usage.py

# Тесты
uv run pytest tests/test_api.py -v
```

## Особенности Реализации

### 1. Глобальное Состояние

- `_registry` - глобальный registry
- `_context_template` - шаблон контекста
- Инициализируются через `initialize_uma()`
- Проверка инициализации в каждой API функции

### 2. Создание Контекста

Функция `_create_context()`:
- Копирует настройки из `_context_template`
- Добавляет `user_context` для конкретного вызова
- Поддерживает дополнительные параметры через `**extra`

### 3. Проверка Доступа

Функция `_check_access()`:
- Вызывает `metadata_provider.can()`
- Операции задаются строками (константы `OP_*` для удобства)
- Выбрасывает `UMAAccessDeniedError` при отказе

### 4. Namespace Support

- Можно указать в имени сущности: `'virtual:users'`
- Можно передать параметром: `namespace='virtual'`
- Используется `parse_entity_name()` для разбора

## Следующие Шаги

### Реализовано ✅

- [x] Базовая инфраструктура (handlers, registry, context)
- [x] UMA класс с методами (read, save, delete, entity_details, entity_list, select)
- [x] Контроль доступа через `can()` метод
- [x] User context поддержка
- [x] Namespace поддержка (через NamespaceConfig)
- [x] JSQL парсер и executor
- [x] Кэширование запросов
- [x] Примеры и тесты
- [x] Документация

### Не Реализовано (будущее)

- [ ] Many-to-many поддержка
- [ ] Оптимизация производительности
- [ ] Расширенные метаданные (constraints, indexes)

## Проверка

```bash
# Нет ошибок линтера
ruff check src/

# Проверка типов
mypy src/

# Все тесты проходят
uv run pytest

# Примеры работают
uv run python examples/api_usage.py
```

## Заключение

API-слой полностью реализован:

✅ `UMA.create()` - инициализация  
✅ `uma.read()` - чтение  
✅ `uma.save()` - создание/обновление  
✅ `uma.delete()` - удаление  
✅ `uma.entity_details()` - метаданные  
✅ `uma.entity_list()` - список сущностей  
✅ `uma.select()` - JSQL запросы  
✅ Контроль доступа через `can()`  
✅ User context поддержка  
✅ Namespace поддержка через NamespaceConfig  
✅ Кэширование запросов  
✅ Примеры и тесты  

Проект готов к использованию!
