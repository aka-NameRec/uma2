# Error Handling Strategy: No Silent Failures

**Date:** 2026-01-05  
**Severity:** 🔴 CRITICAL  
**Category:** Code Quality, Debugging, Reliability

---

## Problem Statement

В текущем коде используются **две разные стратегии** обработки ошибок:

### Стратегия 1: Исключения (публичные функции)
```python
def jsql_to_sql(...):
    try:
        # ... код ...
    except Exception as e:
        raise JSQLSyntaxError(...) from e
```

### Стратегия 2: "Тихий None" (внутренние функции)
```python
def _convert_expression(...):
    if not valid:
        return None  # ⚠️ Теряется контекст ошибки!
```

**Проблема:** При возврате `None` теряется информация:
- **Что** пошло не так?
- **Где** именно проблема?
- **Почему** преобразование не удалось?

Пользователь получает только общее сообщение без деталей.

---

## Real Example of Silent Failure

### Входные данные:
```python
jsql = {
    'from': 'users',
    'where': {'field': 'id', 'op': 'UNKNOWN_OP', 'value': 1}
}
```

### Текущее поведение:
```
1. _convert_comparison_operator('UNKNOWN_OP', ...) → возвращает None
2. _jsql_condition_to_sqlglot(...) → возвращает None  
3. jsql_to_sql(...) → выбрасывает: "Failed to convert JSQL to SQL"
```

**Пользователь видит:** "Failed to convert JSQL to SQL"  
**Пользователь НЕ видит:** "Unknown operator: UNKNOWN_OP at path: where.op"

### Желаемое поведение:
```python
JSQLSyntaxError: Unknown comparison operator: 'UNKNOWN_OP'
  Path: where.op
  Supported operators: =, !=, >, >=, <, <=, IN, LIKE, IS NULL, IS NOT NULL
```

---

## Proposed Rule for Project Standards

### Формулировка для `.cursor/rules`:

```markdown
## Error Handling Strategy

**КАТЕГОРИЧЕСКОЕ ТРЕБОВАНИЕ:** Функции НИКОГДА не должны "тихо проглатывать" ошибки!

### Правило обработки ошибок:

Функция ДОЛЖНА либо:
1. **Успешно выполнить** свою задачу и вернуть результат
2. **Выбросить исключение** с детальным описанием проблемы

### ЗАПРЕЩЕНО:

- ❌ Возвращать `None` при ошибке без исключения
- ❌ Возвращать пустые коллекции (`[]`, `{}`) вместо ошибки
- ❌ Возвращать "магические значения" (`-1`, `""`) для индикации ошибки
- ❌ Логировать ошибку и продолжать выполнение
- ❌ "Тихо проглатывать" исключения в `except` блоках

### РАЗРЕШЕНО:

- ✅ `Optional[T]` возвращаемый тип **ТОЛЬКО** для legitimate отсутствия значения (не ошибка!)
- ✅ Выбрасывать кастомные исключения с контекстом
- ✅ Использовать `from e` для цепочки исключений
- ✅ Добавлять путь к проблемному полю в сообщение об ошибке

### Примеры:

**❌ ПЛОХО:**
```python
def parse_config(data: dict) -> Config | None:
    if 'required_field' not in data:
        return None  # Теряется информация о проблеме!
    return Config(**data)
```

**✅ ХОРОШО:**
```python
def parse_config(data: dict) -> Config:
    if 'required_field' not in data:
        raise ConfigError(
            "Missing required field: 'required_field'",
            path='required_field'
        )
    return Config(**data)
```

**✅ ДОПУСТИМО (если None - это валидное значение):**
```python
def find_user(user_id: int) -> User | None:
    """Return user if found, None if not exists (NOT an error)."""
    user = db.query(User).filter_by(id=user_id).first()
    return user  # None - это нормально, юзер может не существовать
```

### Кастомные исключения:

Всегда создавайте осмысленные кастомные исключения:

```python
class JSQLSyntaxError(Exception):
    """Raised when JSQL syntax is invalid."""
    def __init__(self, message: str, path: str = ''):
        self.message = message
        self.path = path
        super().__init__(f"{message} (at path: {path})" if path else message)
```

### Цепочка исключений:

Используйте `from e` для сохранения оригинального stack trace:

```python
try:
    result = complex_operation()
except ValueError as e:
    raise ProcessingError("Failed to process data") from e
```
```

---

## Impact on Current Code

### Функции требующие исправления:

В `converter.py` следующие функции возвращают `None` при ошибках:

1. ✅ `_jsql_expression_to_sqlglot` (строки 216-235)
2. ✅ `_convert_field_reference` - на самом деле всегда возвращает результат
3. ✅ `_convert_literal_value` - всегда возвращает результат
4. ✅ `_convert_function_call` - всегда возвращает результат
5. ✅ `_convert_arithmetic_op` (строки 274-289)
6. ✅ `_jsql_condition_to_sqlglot` (строки 291-309)
7. ✅ `_convert_logical_operator` (строки 312-336)
8. ✅ `_convert_comparison_operator` (строки 339-378)
9. ✅ `_jsql_join_to_sqlglot` (строки 382-404)
10. ✅ `_jsql_from_to_sqlglot` (строки 192-213)
11. ✅ `_convert_expression_to_jsql` (строки 538-588)
12. ✅ `_convert_condition_to_jsql` (строки 591-679)
13. ✅ `_convert_join_to_jsql` (строки 682-718)
14. ✅ `_convert_order_to_jsql` (строки 721-735)

**Всего:** ~14 функций

---

## Implementation Strategy

### Phase 1: Create detailed exceptions

```python
class JSQLConversionError(JSQLSyntaxError):
    """Base class for JSQL conversion errors."""
    pass

class UnknownOperatorError(JSQLConversionError):
    """Raised when unknown operator is encountered."""
    def __init__(self, operator: str, path: str, supported: list[str]):
        self.operator = operator
        self.supported = supported
        message = f"Unknown operator: '{operator}'. Supported: {', '.join(supported)}"
        super().__init__(message, path)

class InvalidExpressionError(JSQLConversionError):
    """Raised when expression structure is invalid."""
    pass

class MissingFieldError(JSQLConversionError):
    """Raised when required field is missing."""
    def __init__(self, field: str, path: str):
        message = f"Missing required field: '{field}'"
        super().__init__(message, path)
```

### Phase 2: Update functions to raise exceptions

**Пример преобразования:**

**До:**
```python
def _convert_arithmetic_op(expr_spec: dict[str, Any]) -> exp.Expression | None:
    op = expr_spec['op']
    operator_class = ARITHMETIC_OP_TO_SQLGLOT.get(op)
    if operator_class:
        return operator_class(this=left, expression=right)
    return None  # ⚠️ Тихая ошибка
```

**После:**
```python
def _convert_arithmetic_op(expr_spec: dict[str, Any]) -> exp.Expression:
    op = expr_spec['op']
    operator_class = ARITHMETIC_OP_TO_SQLGLOT.get(op)
    
    if not operator_class:
        raise UnknownOperatorError(
            operator=op,
            path='op',
            supported=list(ARITHMETIC_OP_TO_SQLGLOT.keys())
        )
    
    left = _jsql_expression_to_sqlglot(expr_spec.get('left'))
    right = _jsql_expression_to_sqlglot(expr_spec.get('right'))
    
    return operator_class(this=left, expression=right)
```

### Phase 3: Update type hints

Убрать `| None` из возвращаемых типов:

```python
# До:
def _convert_arithmetic_op(...) -> exp.Expression | None:

# После:
def _convert_arithmetic_op(...) -> exp.Expression:
```

### Phase 4: Update tests

Добавить тесты для каждого типа ошибки:

```python
def test_unknown_arithmetic_operator():
    jsql = {'op': 'UNKNOWN', 'left': {'value': 1}, 'right': {'value': 2}}
    with pytest.raises(UnknownOperatorError) as exc_info:
        _convert_arithmetic_op(jsql)
    
    assert 'UNKNOWN' in str(exc_info.value)
    assert '+, -, *, /' in str(exc_info.value)
```

---

## Benefits

### 1. Better Error Messages

**До:**
```
JSQLSyntaxError: Failed to convert JSQL to SQL
```

**После:**
```
UnknownOperatorError: Unknown operator: 'UNKNOWN_OP' at path: where.op
  Supported operators: =, !=, >, >=, <, <=, IN, LIKE, IS NULL, IS NOT NULL
```

### 2. Faster Debugging

- Точное место ошибки
- Список поддерживаемых значений
- Stack trace с контекстом

### 3. Better API for Users

```python
try:
    sql = jsql_to_sql(jsql)
except UnknownOperatorError as e:
    print(f"Fix operator '{e.operator}' at {e.path}")
    print(f"Use one of: {e.supported}")
except MissingFieldError as e:
    print(f"Add required field: {e.field}")
```

### 4. Type Safety

```python
# До: нужна проверка на None
result = _convert_expression(expr)
if result is None:
    # Что делать? Непонятно что пошло не так!
    return None

# После: либо результат, либо исключение
result = _convert_expression(expr)  # Всегда exp.Expression
# Можно сразу использовать
```

---

## Migration Path

### Step 1: Create exceptions module (new file)

```python
# src/namerec/uma/jsql/conversion_exceptions.py
```

### Step 2: Update functions incrementally

Обновлять по одной функции за раз, запускать тесты.

### Step 3: Update all call sites

Убрать проверки `if result is None`.

### Step 4: Update documentation

Документировать какие исключения может выбросить каждая публичная функция.

---

## Conclusion

**Статус:** 🔴 КРИТИЧНО - требует немедленного исправления

**Причина:**
- Тихие ошибки - источник серьёзнейших проблем
- Невозможно отлаживать
- Плохой UX для пользователей API

**Решение:**
- ✅ Добавить правило в project standards
- ✅ Создать кастомные исключения
- ✅ Обновить все функции
- ✅ Убрать `| None` из type hints
- ✅ Добавить тесты на ошибки

**Приоритет:** ВЫСОЧАЙШИЙ 🔴  
**Estimated time:** 2-3 часа  
**Risk:** MEDIUM (требует обновления тестов)
