# Proposal: Debug Logging в converter.py

## Где добавить логирование

### 1. Начало/конец основных функций (INFO level)

```python
import logging

logger = logging.getLogger(__name__)

def jsql_to_sql(jsql: JSQLQuery, dialect: str = 'generic') -> str:
    logger.info(f"Converting JSQL to SQL, dialect={dialect}")
    logger.debug(f"Input JSQL: {jsql}")
    
    try:
        # ... существующий код ...
        
        logger.info(f"Successfully converted JSQL to SQL ({dialect})")
        logger.debug(f"Generated SQL:\n{formatted_sql}")
        return formatted_sql
    except JSQLSyntaxError as e:
        logger.error(f"Failed to convert JSQL: {e}", exc_info=True)
        raise

def sql_to_jsql(sql: str, dialect: str = 'generic') -> JSQLQuery:
    logger.info(f"Converting SQL to JSQL, dialect={dialect}")
    logger.debug(f"Input SQL: {sql}")
    
    try:
        # ... существующий код ...
        
        logger.info("Successfully converted SQL to JSQL")
        logger.debug(f"Generated JSQL: {jsql}")
        return jsql
    except JSQLSyntaxError as e:
        logger.error(f"Failed to parse SQL: {e}", exc_info=True)
        raise
```

**Польза:** Видно начало/конец конвертации, можно измерить время.

---

### 2. Обработка сложных условий (DEBUG level)

```python
def _convert_logical_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression | None:
    logger.debug(f"Converting logical operator: {op}")
    
    match op:
        case 'AND' | 'OR':
            conditions = cond_spec.get('conditions', [])
            logger.debug(f"Processing {len(conditions)} conditions for {op}")
            
            # ... существующий код ...
            
            logger.debug(f"Built {op} expression with {len(conditions)} conditions")
            return result
        
        case 'NOT':
            logger.debug("Processing NOT condition")
            # ... существующий код ...
```

**Польза:** При сложных вложенных условиях видно, как строится дерево.

---

### 3. Обработка неизвестных операторов (WARNING level)

```python
def _convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression | None:
    # ... существующий код ...
    
    match op:
        case '=':
            return exp.EQ(this=field_expr, expression=right_expr)
        # ... остальные операторы ...
        case _:
            logger.warning(f"Unknown comparison operator: {op}, returning None")
            return None
```

**Польза:** Сразу видно, какой оператор не поддерживается.

---

### 4. Обработка JOIN-ов (DEBUG level)

```python
def _jsql_join_to_sqlglot(join_spec: dict[str, Any]) -> dict[str, Any] | None:
    if not isinstance(join_spec, dict):
        logger.debug("Invalid join_spec type, expected dict")
        return None
    
    entity = join_spec.get('entity')
    if not entity:
        logger.debug("Missing 'entity' in join_spec")
        return None
    
    join_type = join_spec.get('type', 'INNER').upper()
    logger.debug(f"Processing {join_type} JOIN to {entity}")
    
    # ... существующий код ...
    
    logger.debug(f"Built JOIN: {join_type} {entity} with ON condition")
    return result
```

**Польза:** При проблемах с JOIN-ами видно, где именно проблема.

---

### 5. Обработка функций (DEBUG level)

```python
def _convert_function_call(expr_spec: dict[str, Any]) -> exp.Expression:
    func = expr_spec['func']
    logger.debug(f"Converting function call: {func}")
    
    args = [_jsql_expression_to_sqlglot(arg) for arg in expr_spec.get('args', [])]
    args = [arg for arg in args if arg is not None]
    logger.debug(f"Function {func} has {len(args)} arguments")
    
    # Try to find specific sqlglot function class
    func_class = getattr(exp, func.upper(), None)
    if func_class and issubclass(func_class, exp.Func):
        logger.debug(f"Using specific sqlglot function: {func_class.__name__}")
        return func_class(*args)
    
    # Fallback to generic Anonymous function
    logger.debug(f"Using Anonymous function for: {func}")
    return exp.Anonymous(this=func, expressions=args)
```

**Польза:** Видно, какие функции используются и как они обрабатываются.

---

### 6. SQL Parsing (DEBUG level)

```python
def sql_to_jsql(sql: str, dialect: str = 'generic') -> JSQLQuery:
    try:
        # Parse SQL with sqlglot
        read_dialect = None if dialect == 'generic' else dialect
        logger.debug(f"Parsing SQL with dialect: {read_dialect or 'generic'}")
        parsed = sqlglot.parse_one(sql, read=read_dialect)
        
        logger.debug(f"Parsed expression type: {type(parsed).__name__}")
        
        if not isinstance(parsed, exp.Select):
            logger.warning(f"Unsupported query type: {type(parsed).__name__}")
            raise JSQLSyntaxError(
                message='Only SELECT queries are supported',
                path='',
            )
        
        # ... остальной код ...
```

**Польза:** Видно, как sqlglot парсит SQL.

---

## Пример использования

### В коде приложения:

```python
import logging

# Настройка логирования
logging.basicConfig(
    level=logging.INFO,  # По умолчанию INFO
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

# Для debug режима:
logging.getLogger('namerec.uma.jsql.converter').setLevel(logging.DEBUG)
```

### В тестах:

```python
import logging

def test_complex_query(caplog):
    caplog.set_level(logging.DEBUG)
    
    jsql = {...}
    result = jsql_to_sql(jsql)
    
    # Проверяем, что логи содержат нужную информацию
    assert "Converting JSQL to SQL" in caplog.text
    assert "Processing INNER JOIN" in caplog.text
```

---

## Уровни логирования

- **DEBUG**: Детальная информация о каждом шаге преобразования
- **INFO**: Начало/конец операций, успешные конвертации
- **WARNING**: Неизвестные операторы, fallback сценарии
- **ERROR**: Ошибки конвертации с stack trace

---

## Performance Impact

Логирование с `if logger.isEnabledFor(logging.DEBUG)` практически не влияет на производительность:

```python
# Для дорогих операций (например, сериализация больших объектов):
if logger.isEnabledFor(logging.DEBUG):
    logger.debug(f"Full JSQL structure: {json.dumps(jsql, indent=2)}")
```

Но для простых строк это избыточно - современные логгеры оптимизированы.

---

## Итого

**Рекомендуемые точки логирования:**
1. ✅ Начало/конец `jsql_to_sql` и `sql_to_jsql` (INFO)
2. ✅ Каждый логический оператор AND/OR/NOT (DEBUG)
3. ✅ Каждый JOIN (DEBUG)
4. ✅ Каждая функция (DEBUG)
5. ✅ Неизвестные операторы (WARNING)
6. ✅ SQL parsing этапы (DEBUG)
7. ✅ Ошибки с полным stack trace (ERROR)

**Примерное количество строк:** +20-30 строк кода
**Польза:** Значительно упрощает отладку сложных запросов
