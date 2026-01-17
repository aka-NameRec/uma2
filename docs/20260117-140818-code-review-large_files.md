# Code Review: Large Files Analysis
**Date:** 2026-01-17  
**Branch:** `feature/20260117-090803-addional_operator_support`  
**Reviewer:** Auto (AI Assistant)

## Executive Summary

Проведен code review файлов `parser.py` (1158 строк) и `conditions.py` (1037 строк). Выявлены критические проблемы с дублированием кода (DRY), нарушением принципов SOLID и потенциальными проблемами производительности.

## 1. parser.py (1158 строк)

### 1.1 Критические проблемы DRY

#### Проблема: Дублирование извлечения типа в обработчиках строковых операторов

**Местоположение:** Строки 713, 725, 738, 750, 806, 820, 834

**Код:**
```python
left_type = getattr(left, 'type', None) if hasattr(left, 'type') else None
```

**Проблема:** Эта строка повторяется 7 раз в методах:
- `_handle_like` (строка 713)
- `_handle_not_like` (строка 725)
- `_handle_ilike` (строка 738)
- `_handle_not_ilike` (строка 750)
- `_handle_similar_to` (строка 806)
- `_handle_regexp` (строка 820)
- `_handle_rlike` (строка 834)

**Решение:**
```python
def _extract_expression_type(self, expr: ColumnElement) -> Any | None:
    """Extract type from SQLAlchemy expression."""
    return getattr(expr, 'type', None) if hasattr(expr, 'type') else None
```

#### Проблема: Дублирование логики обработки строковых операторов

**Местоположение:** Строки 704-838

**Проблема:** Методы `_handle_like`, `_handle_not_like`, `_handle_ilike`, `_handle_not_ilike`, `_handle_similar_to`, `_handle_regexp`, `_handle_rlike` имеют идентичную структуру:

1. Проверка наличия `LEFT` и `RIGHT` полей
2. Извлечение типа из левого выражения
3. Построение правого выражения с ожидаемым типом
4. Применение оператора

**Решение:** Создать общий метод:

```python
async def _handle_string_operator(
    self,
    condition_spec: JSQLExpression,
    operator: JSQLOperator,
    sql_method: str
) -> ClauseElement:
    """Handle string matching operators (LIKE, ILIKE, SIMILAR TO, REGEXP, RLIKE)."""
    if JSQLField.LEFT.value not in condition_spec:
        raise JSQLSyntaxError(f'{operator.value} operator must have "{JSQLField.LEFT.value}" field')
    if JSQLField.RIGHT.value not in condition_spec:
        raise JSQLSyntaxError(f'{operator.value} operator must have "{JSQLField.RIGHT.value}" field')

    left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
    left_type = self._extract_expression_type(left)
    right = await self._build_expression(condition_spec[JSQLField.RIGHT.value], expected_type=left_type)
    
    # Use op() for custom operators, direct method for standard ones
    if sql_method in ('like', 'ilike'):
        result = getattr(left, sql_method)(right)
    else:
        result = left.op(sql_method.upper())(right)
    
    # Apply NOT if needed
    if operator in (JSQLOperator.NOT_LIKE, JSQLOperator.NOT_ILIKE):
        return ~result
    
    return result
```

#### Проблема: Дублирование в обработчиках BETWEEN

**Местоположение:** Строки 754-796

**Проблема:** `_handle_between` и `_handle_not_between` имеют идентичную логику, отличаются только применением NOT.

**Решение:**
```python
async def _handle_between(
    self,
    condition_spec: JSQLExpression,
    negate: bool = False
) -> ClauseElement:
    """Handle BETWEEN operator (with optional negation)."""
    if JSQLField.EXPR.value not in condition_spec:
        raise JSQLSyntaxError(f'BETWEEN must have "{JSQLField.EXPR.value}" field')
    if 'low' not in condition_spec:
        raise JSQLSyntaxError('BETWEEN must have "low" field')
    if 'high' not in condition_spec:
        raise JSQLSyntaxError('BETWEEN must have "high" field')

    expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
    expr_type = self._extract_expression_type(expr)
    low = await self._build_expression(condition_spec['low'], expected_type=expr_type)
    high = await self._build_expression(condition_spec['high'], expected_type=expr_type)
    
    result = expr.between(low, high)
    return ~result if negate else result
```

### 1.2 Нарушения SOLID

#### Нарушение Single Responsibility Principle (SRP)

**Проблема:** Класс `JSQLParser` выполняет слишком много обязанностей:
1. Парсинг JSQL в SQLAlchemy
2. Обработка всех типов операторов (19+ методов)
3. Управление алиасами
4. Управление параметрами
5. Построение выражений
6. Построение функций
7. Построение оконных функций

**Решение:** Выделить обработчики операторов в отдельные классы/функции, используя `Protocol` для структурной типизации (соответствует паттерну проекта - см. `EntityHandler`, `MetadataProvider`, `CacheBackend`).

**Почему `Protocol`, а не `ABC`:**
- Не требует явного наследования - любой callable или класс с нужными методами подходит
- Более гибкий - можно использовать функции или классы
- Соответствует существующему паттерну в проекте
- Меньше boilerplate кода

```python
# src/namerec/uma/jsql/parser/operators.py
from typing import Protocol
from sqlalchemy.sql.expression import ClauseElement
from namerec.uma.jsql.types import JSQLExpression

class OperatorHandler(Protocol):
    """
    Protocol for operator handlers.
    
    Allows structural subtyping - any callable or class implementing
    this interface can be used as an operator handler.
    """
    
    async def __call__(self, condition_spec: JSQLExpression) -> ClauseElement:
        """
        Handle operator condition.
        
        Args:
            condition_spec: JSQL condition specification
            
        Returns:
            SQLAlchemy clause element
        """
        ...

# Конкретные реализации могут быть как классами, так и функциями:
class StringOperatorHandler:
    """Handler for string matching operators."""
    
    def __init__(self, parser: 'JSQLParser', operator: JSQLOperator):
        self.parser = parser
        self.operator = operator
    
    async def __call__(self, condition_spec: JSQLExpression) -> ClauseElement:
        # Implementation
        ...

# Или просто функции (также соответствуют Protocol):
async def handle_like(parser: 'JSQLParser', condition_spec: JSQLExpression) -> ClauseElement:
    """Handle LIKE operator."""
    # Implementation
    ...

# В parser.py:
class JSQLParser:
    def __init__(self, ...):
        self._operator_handlers: dict[JSQLOperator, OperatorHandler] = {
            JSQLOperator.LIKE: StringOperatorHandler(self, JSQLOperator.LIKE),
            JSQLOperator.ILIKE: StringOperatorHandler(self, JSQLOperator.ILIKE),
            # Или функции:
            JSQLOperator.LIKE: lambda spec: handle_like(self, spec),
            # ...
        }
```

#### Нарушение Open/Closed Principle (OCP)

**Проблема:** Для добавления нового оператора нужно модифицировать класс `JSQLParser` (добавлять метод `_handle_*` и регистрировать в `_condition_handlers`).

**Решение:** Использовать паттерн Strategy с регистрацией обработчиков через `Protocol` (соответствует паттерну проекта):

```python
class JSQLParser:
    def __init__(self, ...):
        self._operator_handlers: dict[JSQLOperator, OperatorHandler] = {}
        self._register_default_handlers()
    
    def _register_default_handlers(self):
        """Register default operator handlers."""
        self._operator_handlers[JSQLOperator.LIKE] = StringOperatorHandler(self, JSQLOperator.LIKE)
        # ...
    
    def register_handler(self, operator: JSQLOperator, handler: OperatorHandler):
        """
        Register custom operator handler (extensibility).
        
        Handler can be a function or a class instance implementing OperatorHandler protocol.
        """
        self._operator_handlers[operator] = handler
```

### 1.3 Проблемы производительности

#### Проблема: Множественные проверки `hasattr` и `getattr`

**Местоположение:** Строки 713, 725, 738, 750, 806, 820, 834

**Проблема:** В каждом обработчике строковых операторов выполняется:
```python
left_type = getattr(left, 'type', None) if hasattr(left, 'type') else None
```

Это две операции для каждого вызова. В SQLAlchemy все `ColumnElement` имеют атрибут `type`, поэтому `hasattr` избыточен.

**Решение:**
```python
def _extract_expression_type(self, expr: ColumnElement) -> Any | None:
    """Extract type from SQLAlchemy expression."""
    return getattr(expr, 'type', None)
```

#### Проблема: Отсутствие кэширования результатов разрешения колонок

**Местоположение:** Метод `_resolve_column` (строка 468)

**Проблема:** При каждом вызове `_resolve_column` выполняется поиск через `AliasManager`, который может быть медленным для сложных запросов.

**Решение:** Добавить кэш разрешенных колонок:

```python
def __init__(self, ...):
    # ...
    self._column_cache: dict[tuple[str, Any], Column | ColumnElement] = {}

async def _resolve_column(self, field_spec: str, from_clause: Any) -> Column | ColumnElement:
    """Resolve column reference with caching."""
    cache_key = (field_spec, id(from_clause) if from_clause else None)
    if cache_key in self._column_cache:
        return self._column_cache[cache_key]
    
    # ... existing resolution logic ...
    
    self._column_cache[cache_key] = column
    return column
```

## 2. conditions.py (1037 строк)

### 2.1 Критические проблемы DRY

#### Проблема: Огромная функция `convert_comparison_operator` (293 строки)

**Местоположение:** Строки 212-504

**Проблема:** Функция обрабатывает 15+ различных операторов с большим количеством дублирования.

**Решение:** Разбить на специализированные функции:

```python
# src/namerec/uma/jsql/converter/conditions/string_operators.py
def convert_string_operator(
    op: str,
    cond_spec: dict[str, Any],
    sqlglot_class: type[exp.Expression]
) -> exp.Expression:
    """Convert string matching operator (LIKE, ILIKE, SIMILAR_TO, REGEXP, RLIKE)."""
    left_expr = _extract_left_expression(cond_spec)
    pattern = _extract_pattern(cond_spec)
    
    if pattern is not None:
        return sqlglot_class(this=left_expr, expression=exp.Literal.string(pattern))
    else:
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
        return sqlglot_class(this=left_expr, expression=right_expr)

# В conditions.py:
def convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert comparison operator - delegates to specialized handlers."""
    # Handle special cases first
    if op == JSQLOperator.IS_NULL.value:
        return convert_is_null(cond_spec)
    if op == JSQLOperator.IS_NOT_NULL.value:
        return convert_is_not_null(cond_spec)
    if op == JSQLOperator.BETWEEN.value:
        return convert_between(cond_spec)
    if op == JSQLOperator.NOT_BETWEEN.value:
        return convert_not_between(cond_spec)
    if op in (JSQLOperator.IN.value, JSQLOperator.NOT_IN.value):
        return convert_in_operator(op, cond_spec)
    if op in STRING_OPERATORS:
        return convert_string_operator(op, cond_spec, STRING_OP_TO_SQLGLOT[op])
    
    # Handle standard comparison operators
    return convert_standard_comparison(op, cond_spec)
```

#### Проблема: Дублирование извлечения pattern в строковых операторах

**Местоположение:** Строки 376-484

**Проблема:** Логика извлечения pattern повторяется 5 раз:
- LIKE (строки 376-391)
- NOT_LIKE (строки 393-408)
- ILIKE (строки 410-423)
- NOT_ILIKE (строки 425-438)
- SIMILAR_TO (строки 440-453)
- REGEXP (строки 455-468)
- RLIKE (строки 470-484)

**Решение:**
```python
def _extract_pattern(cond_spec: dict[str, Any]) -> str | None:
    """Extract pattern from condition spec (supports both 'pattern' and 'right' formats)."""
    if 'pattern' in cond_spec:
        return cond_spec['pattern']
    
    if 'right' in cond_spec:
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
        if isinstance(right_expr, exp.Literal) and right_expr.is_string:
            return right_expr.this
    
    return None
```

#### Проблема: Дублирование обработки NOT операторов в `convert_condition_to_jsql`

**Местоположение:** Строки 713-845

**Проблема:** Обработка NOT(IN(...)), NOT(LIKE(...)), NOT(ILIKE(...)), NOT(BETWEEN(...)), NOT(EXISTS(...)) имеет похожую структуру.

**Решение:**
```python
def _handle_not_wrapper(inner: exp.Expression) -> dict[str, Any] | None:
    """Handle NOT wrapper around various operators."""
    handlers = {
        exp.In: _convert_not_in,
        exp.Like: _convert_not_like,
        exp.ILike: _convert_not_ilike,
        exp.Between: _convert_not_between,
        exp.Exists: _convert_not_exists,
    }
    
    for expr_type, handler in handlers.items():
        if isinstance(inner, expr_type):
            return handler(inner)
    
    return None
```

### 2.2 Нарушения SOLID

#### Нарушение Single Responsibility Principle (SRP)

**Проблема:** Файл `conditions.py` содержит:
1. Конверсию JSQL → SQL (строки 23-504)
2. Конверсию SQL → JSQL (строки 507-1036)
3. Вспомогательные функции для подзапросов (строки 118-191, 507-592)

**Решение:** Разделить на модули:
- `conditions_to_sql.py` - JSQL → SQL
- `conditions_to_jsql.py` - SQL → JSQL
- `condition_helpers.py` - общие вспомогательные функции

#### Нарушение Open/Closed Principle (OCP)

**Проблема:** Для добавления нового оператора нужно модифицировать несколько функций в одном файле.

**Решение:** Использовать регистрацию обработчиков:

```python
# conditions/registry.py
class ConditionConverterRegistry:
    """Registry for condition converters."""
    
    def __init__(self):
        self._jsql_to_sql: dict[str, Callable] = {}
        self._sql_to_jsql: dict[type, Callable] = {}
    
    def register_jsql_to_sql(self, operator: str, handler: Callable):
        """Register JSQL → SQL converter."""
        self._jsql_to_sql[operator] = handler
    
    def register_sql_to_jsql(self, expr_type: type, handler: Callable):
        """Register SQL → JSQL converter."""
        self._sql_to_jsql[expr_type] = handler
```

### 2.3 Проблемы производительности

#### Проблема: Множественные проверки `isinstance` в `convert_condition_to_jsql`

**Местоположение:** Строки 595-1036

**Проблема:** Функция содержит длинную цепочку `if isinstance(expr, ...)` проверок (15+ проверок), что неэффективно.

**Решение:** Использовать словарь типов:

```python
CONDITION_CONVERTERS: dict[type, Callable] = {
    exp.And: _convert_and,
    exp.Or: _convert_or,
    exp.ILike: _convert_ilike,
    exp.SimilarTo: _convert_similar_to,
    # ...
}

def convert_condition_to_jsql(expr: exp.Expression) -> dict[str, Any]:
    """Convert sqlglot condition to JSQL condition."""
    # Check for registered converter
    converter = CONDITION_CONVERTERS.get(type(expr))
    if converter:
        return converter(expr)
    
    # Fallback to generic handlers
    # ...
```

## 3. Общие рекомендации

### 3.1 Рефакторинг структуры

**Рекомендация:** Выделить обработчики операторов в отдельные модули:

```
src/namerec/uma/jsql/
├── parser/
│   ├── __init__.py
│   ├── parser.py          # Основной парсер (упрощенный)
│   ├── operators/
│   │   ├── __init__.py
│   │   ├── base.py        # Базовый класс OperatorHandler
│   │   ├── comparison.py  # Операторы сравнения
│   │   ├── string.py      # Строковые операторы
│   │   ├── logical.py    # Логические операторы
│   │   └── special.py     # BETWEEN, IN, EXISTS и т.д.
│   └── expressions.py     # Построение выражений
└── converter/
    ├── conditions/
    │   ├── __init__.py
    │   ├── to_sql.py      # JSQL → SQL
    │   ├── to_jsql.py     # SQL → JSQL
    │   ├── operators/     # Специализированные обработчики
    │   └── helpers.py     # Вспомогательные функции
    └── ...
```

### 3.2 Улучшение тестируемости

**Проблема:** Большие функции сложно тестировать изолированно.

**Решение:** Разбить на маленькие, тестируемые функции с четкими контрактами.

### 3.3 Документация

**Проблема:** Недостаточно документации для сложных функций.

**Решение:** Добавить подробные docstrings с примерами использования.

## 4. Приоритеты исправлений

### Критично (P0)
1. ✅ Вынести дублирование извлечения типа в `parser.py`
2. ✅ Объединить обработчики строковых операторов в `parser.py`
3. ✅ Разбить `convert_comparison_operator` на специализированные функции

### Высокий приоритет (P1)
4. Выделить обработчики операторов в отдельные классы/модули
5. Устранить дублирование извлечения pattern в `conditions.py`
6. Добавить кэширование разрешения колонок

### Средний приоритет (P2)
7. Разделить `conditions.py` на модули (to_sql, to_jsql, helpers)
8. Использовать регистрацию обработчиков вместо длинных if-цепочек
9. Улучшить документацию

## 5. Метрики улучшения

После рефакторинга ожидаемые результаты:
- `parser.py`: ~800 строк (снижение на ~30%)
- `conditions.py`: ~600 строк (снижение на ~40%)
- Устранение ~200+ строк дублированного кода
- Улучшение покрытия тестами (более мелкие функции легче тестировать)
- Улучшение производительности (кэширование, оптимизация проверок)

## 6. Конкретные примеры рефакторинга

### 6.1 Пример: Устранение дублирования в parser.py

**До рефакторинга:**
```python
async def _handle_like(self, condition_spec: JSQLExpression) -> ClauseElement:
    if JSQLField.LEFT.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
    if JSQLField.RIGHT.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

    left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
    left_type = getattr(left, 'type', None) if hasattr(left, 'type') else None
    right = await self._build_expression(condition_spec[JSQLField.RIGHT.value], expected_type=left_type)
    return left.like(right)

async def _handle_not_like(self, condition_spec: JSQLExpression) -> ClauseElement:
    if JSQLField.LEFT.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
    if JSQLField.RIGHT.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

    left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
    left_type = getattr(left, 'type', None) if hasattr(left, 'type') else None
    right = await self._build_expression(condition_spec[JSQLField.RIGHT.value], expected_type=left_type)
    return ~left.like(right)
```

**После рефакторинга:**
```python
def _extract_expression_type(self, expr: ColumnElement) -> Any | None:
    """Extract type from SQLAlchemy expression."""
    return getattr(expr, 'type', None)

async def _handle_string_operator(
    self,
    condition_spec: JSQLExpression,
    operator: JSQLOperator,
    negate: bool = False
) -> ClauseElement:
    """Handle string matching operators (LIKE, ILIKE, SIMILAR_TO, REGEXP, RLIKE)."""
    if JSQLField.LEFT.value not in condition_spec:
        raise JSQLSyntaxError(f'{operator.value} operator must have "{JSQLField.LEFT.value}" field')
    if JSQLField.RIGHT.value not in condition_spec:
        raise JSQLSyntaxError(f'{operator.value} operator must have "{JSQLField.RIGHT.value}" field')

    left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
    left_type = self._extract_expression_type(left)
    right = await self._build_expression(condition_spec[JSQLField.RIGHT.value], expected_type=left_type)
    
    # Map operator to SQLAlchemy method
    operator_map = {
        JSQLOperator.LIKE: lambda l, r: l.like(r),
        JSQLOperator.ILIKE: lambda l, r: l.ilike(r),
        JSQLOperator.SIMILAR_TO: lambda l, r: l.op('SIMILAR TO')(r),
        JSQLOperator.REGEXP: lambda l, r: l.op('REGEXP')(r),
        JSQLOperator.RLIKE: lambda l, r: l.op('RLIKE')(r),
    }
    
    result = operator_map[operator](left, right)
    return ~result if negate else result

# В _condition_handlers:
self._condition_handlers: dict[JSQLOperator, Callable] = {
    # ...
    JSQLOperator.LIKE: lambda spec: self._handle_string_operator(spec, JSQLOperator.LIKE, False),
    JSQLOperator.NOT_LIKE: lambda spec: self._handle_string_operator(spec, JSQLOperator.LIKE, True),
    JSQLOperator.ILIKE: lambda spec: self._handle_string_operator(spec, JSQLOperator.ILIKE, False),
    JSQLOperator.NOT_ILIKE: lambda spec: self._handle_string_operator(spec, JSQLOperator.ILIKE, True),
    # ...
}
```

**Результат:** Сокращение с ~70 строк до ~30 строк, устранение дублирования.

### 6.2 Пример: Разбиение convert_comparison_operator

**До рефакторинга:**
```python
def convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert comparison operator to sqlglot expression."""
    # 293 строки кода с множественными if-проверками
    if op == JSQLOperator.IS_NULL.value:
        # ... 10 строк
    if op == JSQLOperator.IS_NOT_NULL.value:
        # ... 10 строк
    if op == JSQLOperator.BETWEEN.value:
        # ... 15 строк
    if op == JSQLOperator.LIKE.value:
        # ... 20 строк
    # ... и так далее
```

**После рефакторинга:**
```python
# conditions/string_operators.py
def convert_string_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert string matching operator."""
    left_expr = _extract_left_expression(cond_spec)
    pattern = _extract_pattern(cond_spec)
    
    operator_map = {
        JSQLOperator.LIKE.value: exp.Like,
        JSQLOperator.ILIKE.value: exp.ILike,
        JSQLOperator.SIMILAR_TO.value: exp.SimilarTo,
        JSQLOperator.REGEXP.value: exp.RegexpLike,
        JSQLOperator.RLIKE.value: exp.RegexpLike,
    }
    
    sqlglot_class = operator_map[op]
    
    if pattern is not None:
        return sqlglot_class(this=left_expr, expression=exp.Literal.string(pattern))
    else:
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
        return sqlglot_class(this=left_expr, expression=right_expr)

# conditions.py
def convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert comparison operator - delegates to specialized handlers."""
    # Dispatch to specialized handlers
    if op == JSQLOperator.IS_NULL.value:
        return convert_is_null(cond_spec)
    if op == JSQLOperator.IS_NOT_NULL.value:
        return convert_is_not_null(cond_spec)
    if op == JSQLOperator.BETWEEN.value:
        return convert_between(cond_spec)
    if op == JSQLOperator.NOT_BETWEEN.value:
        return convert_not_between(cond_spec)
    if op in (JSQLOperator.IN.value, JSQLOperator.NOT_IN.value):
        return convert_in_operator(op, cond_spec)
    if op in STRING_OPERATORS:
        return convert_string_operator(op, cond_spec)
    
    # Standard comparison operators
    return convert_standard_comparison(op, cond_spec)
```

**Результат:** Сокращение функции с 293 строк до ~20 строк, улучшение читаемости и тестируемости.

## 7. Заключение

Оба файла требуют серьезного рефакторинга для соблюдения принципов DRY и SOLID. Основные проблемы:
1. Значительное дублирование кода (~200+ строк дублированного кода)
2. Нарушение принципа единственной ответственности (класс/файл делает слишком много)
3. Сложность расширения (нарушение OCP - нужно модифицировать существующий код)
4. Потенциальные проблемы производительности (множественные проверки, отсутствие кэширования)

**Рекомендации:**
- Выполнить рефакторинг поэтапно, начиная с критических проблем (P0)
- Обязательное покрытие тестами на каждом этапе
- Использовать feature flags для постепенного внедрения изменений
- Провести performance testing до и после рефакторинга

**Ожидаемые результаты:**
- Сокращение размера файлов на 30-40%
- Улучшение покрытия тестами (более мелкие функции легче тестировать)
- Улучшение производительности (кэширование, оптимизация проверок)
- Упрощение добавления новых операторов (соблюдение OCP)
