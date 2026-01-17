# Plan: Add Convenience Operators (NOT IN, NOT LIKE, NOT BETWEEN, NOT EXISTS, <>)

**Date:** 2026-01-17  
**Task:** Add support for convenience operators and ISO-standard `<>` operator  
**Status:** Planning

## Overview

This plan covers implementation of:
1. `<>` (ISO-standard not-equal operator, alternative to `!=`)
2. `NOT IN` (explicit operator)
3. `NOT LIKE` (explicit operator)
4. `NOT BETWEEN` (explicit operator)
5. `NOT EXISTS` (explicit operator)

These operators improve developer experience and provide standard SQL syntax support.

---

## Operator Details

### 1. `<>` (Not Equal - ISO Standard)
- **JSQL Format:** `{"op": "<>", "left": {...}, "right": {...}}`
- **SQL Equivalent:** `column <> value`
- **SQLAlchemy:** Same as `!=` (uses `!=` operator)
- **sqlglot:** `exp.NEQ` (same as `!=`)

### 2. `NOT IN`
- **JSQL Format:** `{"op": "NOT IN", "left": {...}, "right": [...]}` or `{"op": "NOT IN", "field": "...", "values": [...]}`
- **SQL Equivalent:** `column NOT IN (value1, value2, ...)`
- **SQLAlchemy:** `~column.in_(values)` or `column.notin_(values)`
- **sqlglot:** `exp.NotIn` or `exp.Not(this=exp.In(...))`

### 3. `NOT LIKE`
- **JSQL Format:** `{"op": "NOT LIKE", "left": {...}, "right": {...}}` or `{"op": "NOT LIKE", "field": "...", "pattern": "..."}`
- **SQL Equivalent:** `column NOT LIKE 'pattern'`
- **SQLAlchemy:** `~column.like(pattern)` or `column.notlike(pattern)`
- **sqlglot:** `exp.NotLike` or `exp.Not(this=exp.Like(...))`

### 4. `NOT BETWEEN`
- **JSQL Format:** `{"op": "NOT BETWEEN", "expr": {...}, "low": {...}, "high": {...}}`
- **SQL Equivalent:** `column NOT BETWEEN low AND high`
- **SQLAlchemy:** `~column.between(low, high)`
- **sqlglot:** `exp.NotBetween` or `exp.Not(this=exp.Between(...))`

### 5. `NOT EXISTS`
- **JSQL Format:** `{"op": "NOT EXISTS", "subquery": {...}}`
- **SQL Equivalent:** `NOT EXISTS (SELECT ...)`
- **SQLAlchemy:** `~exists(subquery)`
- **sqlglot:** `exp.NotExists` or `exp.Not(this=exp.Exists(...))`

---

## Implementation Plan

### Phase 1: Add Operators to Constants

**File:** `src/namerec/uma/jsql/constants.py`

#### 1.1 Add new operators to `JSQLOperator` enum
```python
class JSQLOperator(str, Enum):
    # ... existing operators ...
    
    # Comparison operators
    EQ = '='
    NE = '!='
    NEQ_ISO = '<>'  # NEW: ISO standard not-equal
    LT = '<'
    # ... rest of comparison operators ...
    
    # Special operators
    IN = 'IN'
    NOT_IN = 'NOT IN'  # NEW
    BETWEEN = 'BETWEEN'
    NOT_BETWEEN = 'NOT BETWEEN'  # NEW
    EXISTS = 'EXISTS'
    NOT_EXISTS = 'NOT EXISTS'  # NEW
    IS_NULL = 'IS NULL'
    IS_NOT_NULL = 'IS NOT NULL'
    LIKE = 'LIKE'
    NOT_LIKE = 'NOT LIKE'  # NEW
```

**Location:** After line 72 (after comparison operators) and after line 80 (after LIKE)

#### 1.2 Update `COMPARISON_OPERATORS` set
```python
COMPARISON_OPERATORS = frozenset({
    JSQLOperator.EQ,
    JSQLOperator.NE,
    JSQLOperator.NEQ_ISO,  # NEW
    JSQLOperator.LT,
    JSQLOperator.LE,
    JSQLOperator.GT,
    JSQLOperator.GE,
})
```

**Impact:** Low risk, no breaking changes

---

### Phase 2: Update Parser

**File:** `src/namerec/uma/jsql/parser.py`

#### 2.1 Add handlers for NOT operators in `_condition_handlers`
```python
self._condition_handlers: dict[JSQLOperator, Callable] = {
    # ... existing handlers ...
    JSQLOperator.IN: self._handle_in,
    JSQLOperator.NOT_IN: self._handle_not_in,  # NEW
    JSQLOperator.BETWEEN: self._handle_between,
    JSQLOperator.NOT_BETWEEN: self._handle_not_between,  # NEW
    JSQLOperator.EXISTS: self._handle_exists,
    JSQLOperator.NOT_EXISTS: self._handle_not_exists,  # NEW
    JSQLOperator.LIKE: self._handle_like,
    JSQLOperator.NOT_LIKE: self._handle_not_like,  # NEW
}
```

**Location:** After line 70

#### 2.2 Update `_handle_comparison` to support `<>`
The `<>` operator should be handled in `_handle_comparison` method. Add case for `JSQLOperator.NEQ_ISO`:
```python
async def _handle_comparison(self, op: JSQLOperator, condition_spec: JSQLExpression) -> ClauseElement:
    # ... existing code ...
    match op:
        case JSQLOperator.EQ:
            return left == right
        case JSQLOperator.NE | JSQLOperator.NEQ_ISO:  # Both map to !=
            return left != right
        # ... rest of cases ...
```

**Location:** Around line 606-619

#### 2.3 Implement `_handle_not_in` method
```python
async def _handle_not_in(self, condition_spec: JSQLExpression) -> ClauseElement:
    """Handle NOT IN operator."""
    if JSQLField.LEFT.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_IN.value} operator must have "{JSQLField.LEFT.value}" field')
    if JSQLField.RIGHT.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_IN.value} operator must have "{JSQLField.RIGHT.value}" field')

    left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
    right_spec = condition_spec[JSQLField.RIGHT.value]

    # Subquery
    if isinstance(right_spec, dict) and JSQLField.SUBQUERY.value in right_spec:
        subquery = await self._build_query(right_spec[JSQLField.SUBQUERY.value])
        return ~left.in_(subquery)  # Use ~ for negation

    # List of values
    if isinstance(right_spec, list):
        return ~left.in_(right_spec)

    # Expression
    right = await self._build_expression(right_spec)
    return ~left.in_(right)
```

**Location:** After `_handle_in` method (around line 642)

#### 2.4 Implement `_handle_not_like` method
```python
async def _handle_not_like(self, condition_spec: JSQLExpression) -> ClauseElement:
    """Handle NOT LIKE operator."""
    if JSQLField.LEFT.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_LIKE.value} operator must have "{JSQLField.LEFT.value}" field')
    if JSQLField.RIGHT.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_LIKE.value} operator must have "{JSQLField.RIGHT.value}" field')

    left = await self._build_expression(condition_spec[JSQLField.LEFT.value])
    left_type = getattr(left, 'type', None) if hasattr(left, 'type') else None
    right = await self._build_expression(condition_spec[JSQLField.RIGHT.value], expected_type=left_type)
    return ~left.like(right)  # Use ~ for negation
```

**Location:** After `_handle_like` method (around line 676)

#### 2.5 Implement `_handle_not_between` method
```python
async def _handle_not_between(self, condition_spec: JSQLExpression) -> ClauseElement:
    """Handle NOT BETWEEN operator."""
    if JSQLField.EXPR.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_BETWEEN.value} must have "{JSQLField.EXPR.value}" field')
    if 'low' not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_BETWEEN.value} must have "low" field')
    if 'high' not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_BETWEEN.value} must have "high" field')

    expr = await self._build_expression(condition_spec[JSQLField.EXPR.value])
    expr_type = getattr(expr, 'type', None) if hasattr(expr, 'type') else None
    low = await self._build_expression(condition_spec['low'], expected_type=expr_type)
    high = await self._build_expression(condition_spec['high'], expected_type=expr_type)
    
    return ~expr.between(low, high)  # Use ~ for negation
```

**Location:** After `_handle_between` method (around line 698)

#### 2.6 Implement `_handle_not_exists` method
```python
async def _handle_not_exists(self, condition_spec: JSQLExpression) -> ClauseElement:
    """Handle NOT EXISTS operator."""
    if JSQLField.SUBQUERY.value not in condition_spec:
        raise JSQLSyntaxError(f'{JSQLOperator.NOT_EXISTS.value} must have "{JSQLField.SUBQUERY.value}" field')
    subquery = await self._build_query(condition_spec[JSQLField.SUBQUERY.value])
    return ~exists(subquery)  # Use ~ for negation
```

**Location:** After `_handle_exists` method (around line 649)

**Impact:** Low risk, adds new functionality only

---

### Phase 3: Update JSQL→SQL Converter

**File:** `src/namerec/uma/jsql/converter/conditions.py`

#### 3.1 Update `convert_comparison_operator` to handle `<>`
The `<>` operator is handled as a comparison operator, so it should work automatically if we add it to `COMPARISON_OP_TO_SQLGLOT`. However, we need to check if sqlglot handles `<>` correctly.

**Note:** sqlglot's `exp.NEQ` handles both `!=` and `<>`, so the mapping should work.

#### 3.2 Update `convert_comparison_operator` to handle NOT operators

Add handling for NOT operators in `convert_comparison_operator` method:

```python
def convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    # ... existing code for BETWEEN ...
    
    # Handle NOT IN
    if op == JSQLOperator.NOT_IN.value:
        # Similar to IN but use exp.NotIn
        if 'values' not in cond_spec:
            raise MissingFieldError('values', path='values', context='NOT IN operator')
        values = cond_spec['values']
        logger.debug(f'Processing NOT IN operator with {len(values)} values')
        val_exprs = [jsql_expression_to_sqlglot({'value': v}) for v in values]
        return exp.NotIn(this=left_expr, expressions=val_exprs)
    
    # Handle NOT LIKE
    if op == JSQLOperator.NOT_LIKE.value:
        if 'pattern' not in cond_spec:
            raise MissingFieldError('pattern', path='pattern', context='NOT LIKE operator')
        pattern = cond_spec['pattern']
        logger.debug(f'Processing NOT LIKE operator with pattern: {pattern}')
        return exp.NotLike(this=left_expr, expression=exp.Literal.string(pattern))
    
    # Handle NOT BETWEEN
    if op == JSQLOperator.NOT_BETWEEN.value:
        if 'expr' not in cond_spec:
            raise MissingFieldError('expr', path='expr', context='NOT BETWEEN operator')
        if 'low' not in cond_spec:
            raise MissingFieldError('low', path='low', context='NOT BETWEEN operator')
        if 'high' not in cond_spec:
            raise MissingFieldError('high', path='high', context='NOT BETWEEN operator')
        
        expr = jsql_expression_to_sqlglot(cond_spec['expr'])
        low_expr = jsql_expression_to_sqlglot(cond_spec['low'])
        high_expr = jsql_expression_to_sqlglot(cond_spec['high'])
        
        logger.debug('Processing NOT BETWEEN operator')
        return exp.NotBetween(this=expr, low=low_expr, high=high_expr)
    
    # Handle NOT EXISTS - this is handled separately as it doesn't follow comparison pattern
    
    # ... rest of existing code ...
```

**Location:** After LIKE handling (around line 188)

#### 3.3 Add NOT EXISTS handling in `jsql_condition_to_sqlglot`

NOT EXISTS should be handled at the top level (similar to EXISTS):

```python
def jsql_condition_to_sqlglot(cond_spec: dict[str, Any]) -> exp.Expression:
    # ... existing code ...
    
    op = cond_spec['op']
    
    # Handle NOT EXISTS separately (similar to EXISTS)
    if op == JSQLOperator.NOT_EXISTS.value:
        return convert_not_exists_operator(cond_spec)
    
    # Try logical operators first
    if op in LOGICAL_OPERATORS:
        return convert_logical_operator(op, cond_spec)
    
    # Then try comparison operators
    return convert_comparison_operator(op, cond_spec)
```

Add helper function:
```python
def convert_not_exists_operator(cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert NOT EXISTS operator to sqlglot expression."""
    if 'subquery' not in cond_spec:
        raise MissingFieldError('subquery', path='subquery', context='NOT EXISTS operator')
    subquery_spec = cond_spec['subquery']
    subquery = jsql_query_to_sqlglot(subquery_spec)  # Need to import this
    return exp.NotExists(expression=subquery)
```

**Note:** Need to check if `jsql_query_to_sqlglot` exists or create it.

#### 3.4 Update supported operators list

Update the supported operators list in error messages:
```python
supported = list(COMPARISON_OP_TO_SQLGLOT.keys()) + [
    JSQLOperator.IN.value,
    JSQLOperator.NOT_IN.value,  # NEW
    JSQLOperator.BETWEEN.value,
    JSQLOperator.NOT_BETWEEN.value,  # NEW
    JSQLOperator.LIKE.value,
    JSQLOperator.NOT_LIKE.value,  # NEW
    JSQLOperator.IS_NULL.value,
    JSQLOperator.IS_NOT_NULL.value,
]
```

**Location:** Around line 197-203

**Impact:** Low risk, extends existing functionality

---

### Phase 4: Update SQL→JSQL Converter

**File:** `src/namerec/uma/jsql/converter/conditions.py`

#### 4.1 Handle `<>` in comparison operators

Update `convert_condition_to_jsql` to handle `<>` (which sqlglot represents as `exp.NEQ`):
```python
# Handle comparison operators
if isinstance(expr, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE)):
    op = SQLGLOT_TO_COMPARISON.get(type(expr))
    
    # Special handling for <> vs !=
    # Both map to exp.NEQ, but we want to preserve <> when possible
    # For now, we'll use != as default, but this could be configurable
    if type(expr) == exp.NEQ:
        # Check if original SQL had <> or !=
        # sqlglot doesn't preserve this distinction, so we'll default to !=
        # This is acceptable as both are equivalent
        op = JSQLOperator.NE.value  # or JSQLOperator.NEQ_ISO.value if we want <>
    
    # ... rest of existing code ...
```

**Note:** sqlglot doesn't preserve whether the original SQL used `<>` or `!=`, so we'll default to `!=`. If preserving `<>` is important, we might need custom handling.

#### 4.2 Add handling for NOT IN

```python
# Handle NOT IN
if isinstance(expr, exp.NotIn):
    left = convert_expression_to_jsql(expr.this)
    if 'field' not in left:
        raise InvalidExpressionError(
            message='NOT IN operator left side must be a field reference',
            path='left',
            expression=left
        )

    values = []
    if isinstance(expr.expressions, list):
        for val_expr in expr.expressions:
            val = convert_expression_to_jsql(val_expr)
            if 'value' not in val:
                raise InvalidExpressionError(
                    message='NOT IN operator values must be literals',
                    path='values',
                    expression=val
                )
            values.append(val['value'])

    return {
        'field': left['field'],
        'op': JSQLOperator.NOT_IN.value,
        'values': values,
    }
```

**Location:** After IN handling (around line 307)

#### 4.3 Handle `Not(In(...))` pattern

sqlglot might represent `NOT IN` as `Not(In(...))`. We need to detect this pattern:
```python
# Handle NOT(IN(...)) pattern - sqlglot might use this instead of NotIn
if isinstance(expr, exp.Not):
    inner = expr.this
    if isinstance(inner, exp.In):
        # Convert to NOT IN
        left = convert_expression_to_jsql(inner.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message='NOT IN operator left side must be a field reference',
                path='left',
                expression=left
            )
        
        values = []
        if isinstance(inner.expressions, list):
            for val_expr in inner.expressions:
                val = convert_expression_to_jsql(val_expr)
                if 'value' not in val:
                    raise InvalidExpressionError(
                        message='NOT IN operator values must be literals',
                        path='values',
                        expression=val
                    )
                values.append(val['value'])
        
        return {
            'field': left['field'],
            'op': JSQLOperator.NOT_IN.value,
            'values': values,
        }
    
    # Handle other NOT patterns (NOT LIKE, NOT BETWEEN, etc.)
    # ... similar pattern matching ...
```

**Location:** In `convert_condition_to_jsql`, before general NOT handling

#### 4.4 Add handling for NOT LIKE

```python
# Handle NOT LIKE
if isinstance(expr, exp.NotLike):
    left = convert_expression_to_jsql(expr.this)
    right = convert_expression_to_jsql(expr.expression)

    if 'field' not in left:
        raise InvalidExpressionError(
            message='NOT LIKE operator left side must be a field reference',
            path='left',
            expression=left
        )

    if 'value' not in right:
        raise InvalidExpressionError(
            message='NOT LIKE operator right side must be a string literal',
            path='right',
            expression=right
        )

    return {
        'field': left['field'],
        'op': JSQLOperator.NOT_LIKE.value,
        'pattern': right['value'],
    }
```

**Location:** After LIKE handling (around line 332)

#### 4.5 Handle `Not(Like(...))` pattern

Similar to NOT IN, handle `Not(Like(...))`:
```python
# In the NOT(IN(...)) handling section, also check for NOT LIKE
if isinstance(expr, exp.Not):
    inner = expr.this
    
    # ... existing NOT IN handling ...
    
    if isinstance(inner, exp.Like):
        # Convert to NOT LIKE
        left = convert_expression_to_jsql(inner.this)
        right = convert_expression_to_jsql(inner.expression)
        
        if 'field' not in left:
            raise InvalidExpressionError(...)
        if 'value' not in right:
            raise InvalidExpressionError(...)
        
        return {
            'field': left['field'],
            'op': JSQLOperator.NOT_LIKE.value,
            'pattern': right['value'],
        }
```

#### 4.6 Add handling for NOT BETWEEN

```python
# Handle NOT BETWEEN
if isinstance(expr, exp.NotBetween):
    # Similar to BETWEEN but with NOT_BETWEEN operator
    # ... implementation similar to BETWEEN ...
```

#### 4.7 Handle `Not(Between(...))` pattern

Check for `Not(Between(...))` in the NOT handling section.

#### 4.8 Add handling for NOT EXISTS

```python
# Handle NOT EXISTS
if isinstance(expr, exp.NotExists):
    # Extract subquery and convert
    # ... implementation ...
```

#### 4.9 Handle `Not(Exists(...))` pattern

Check for `Not(Exists(...))` in the NOT handling section.

**Impact:** Low risk, extends SQL parsing capabilities

---

### Phase 5: Update Operator Mappings (if needed)

**File:** `src/namerec/uma/jsql/converter/operators.py`

#### 5.1 Add `<>` to comparison operator mapping

```python
COMPARISON_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.EQ.value: exp.EQ,
    JSQLOperator.NE.value: exp.NEQ,
    JSQLOperator.NEQ_ISO.value: exp.NEQ,  # NEW: Maps to same as !=
    JSQLOperator.GT.value: exp.GT,
    JSQLOperator.GE.value: exp.GTE,
    JSQLOperator.LT.value: exp.LT,
    JSQLOperator.LE.value: exp.LTE,
}
```

**Impact:** None, both `!=` and `<>` map to `exp.NEQ`

---

### Phase 6: Testing

**Files:** `tests/test_converter.py`, `tests/test_jsql.py`

#### 6.1 Tests for `<>` operator
- Test `<>` in WHERE clause
- Test `<>` with parameters
- Test SQL→JSQL conversion preserves `!=` (or converts to `<>` if we implement that)
- Test JSQL→SQL conversion generates correct SQL

#### 6.2 Tests for NOT IN
- Test NOT IN with value list
- Test NOT IN with subquery
- Test NOT IN in WHERE clause
- Test NOT IN in HAVING clause
- Test SQL→JSQL conversion (both `NotIn` and `Not(In(...))` patterns)
- Test JSQL→SQL conversion

#### 6.3 Tests for NOT LIKE
- Test NOT LIKE with pattern
- Test NOT LIKE with parameters
- Test NOT LIKE in WHERE clause
- Test SQL→JSQL conversion (both `NotLike` and `Not(Like(...))` patterns)
- Test JSQL→SQL conversion

#### 6.4 Tests for NOT BETWEEN
- Test NOT BETWEEN with different data types
- Test NOT BETWEEN in WHERE clause
- Test SQL→JSQL conversion
- Test JSQL→SQL conversion

#### 6.5 Tests for NOT EXISTS
- Test NOT EXISTS with subquery
- Test NOT EXISTS in WHERE clause
- Test SQL→JSQL conversion
- Test JSQL→SQL conversion

#### 6.6 Integration tests
- Test all NOT operators together
- Test combination with other operators (AND, OR)
- Test with all supported databases

**Impact:** Ensures correctness and prevents regressions

---

### Phase 7: Documentation Updates

**Files:** Documentation files mentioning operators

#### 7.1 Update JSQL specification
- Add `<>`, `NOT IN`, `NOT LIKE`, `NOT BETWEEN`, `NOT EXISTS` to supported operators
- Document syntax and usage for each operator
- Add examples

#### 7.2 Update examples
- Add examples with new operators
- Show equivalent forms (e.g., `NOT IN` vs `NOT` + `IN`)

#### 7.3 Update API documentation
- Document all new operators in operator reference

**Impact:** Improves developer experience

---

## Implementation Order

1. ✅ Phase 1: Constants (foundation)
2. ✅ Phase 2: Parser (core functionality)
3. ✅ Phase 3: JSQL→SQL Converter (output)
4. ✅ Phase 4: SQL→JSQL Converter (input)
5. ✅ Phase 5: Operator Mappings (if needed)
6. ✅ Phase 6: Testing (verification)
7. ✅ Phase 7: Documentation (completion)

**Note:** Phases 1-4 can be done in parallel for different operators, but Phase 1 must be completed first.

---

## Special Considerations

### sqlglot Pattern Handling

sqlglot may represent NOT operators in two ways:
1. **Explicit classes:** `exp.NotIn`, `exp.NotLike`, `exp.NotBetween`, `exp.NotExists`
2. **Wrapped classes:** `exp.Not(this=exp.In(...))`, `exp.Not(this=exp.Like(...))`, etc.

We need to handle both patterns in SQL→JSQL conversion.

### SQLAlchemy Negation

SQLAlchemy uses `~` operator for negation:
- `~column.in_(values)` for NOT IN
- `~column.like(pattern)` for NOT LIKE
- `~column.between(low, high)` for NOT BETWEEN
- `~exists(subquery)` for NOT EXISTS

Some SQLAlchemy versions may also support:
- `column.notin_(values)`
- `column.notlike(pattern)`

We'll use `~` for consistency.

### Database Compatibility

All NOT operators are standard SQL and work across all databases:
- ✅ PostgreSQL: Full support
- ✅ MySQL: Full support
- ✅ SQLite: Full support
- ✅ SQL Server: Full support

The `<>` operator is ISO standard and works in all databases (equivalent to `!=`).

---

## Risk Assessment

### Low Risk Items
- Adding enum values (Phase 1)
- Adding handler methods (Phase 2)
- Adding converter methods (Phase 3, 4)
- Adding tests (Phase 6)

### Medium Risk Items
- Pattern matching for `Not(X(...))` in SQL→JSQL conversion
- Ensuring consistent behavior across all databases

### Mitigation Strategies
1. Comprehensive testing with both sqlglot patterns
2. Test with all supported databases
3. Follow existing patterns (e.g., LIKE implementation)
4. Ensure backward compatibility (existing NOT + operator still works)

---

## Success Criteria

- [ ] `<>` operator can be used in JSQL WHERE clauses
- [ ] `NOT IN` operator can be used in JSQL WHERE/HAVING clauses
- [ ] `NOT LIKE` operator can be used in JSQL WHERE/HAVING clauses
- [ ] `NOT BETWEEN` operator can be used in JSQL WHERE/HAVING clauses
- [ ] `NOT EXISTS` operator can be used in JSQL WHERE clauses
- [ ] SQL queries with these operators can be converted to JSQL
- [ ] JSQL queries with these operators can be converted to SQL
- [ ] Tests pass for all supported databases
- [ ] Documentation updated
- [ ] No breaking changes introduced
- [ ] Existing `NOT` + operator syntax still works

---

## Notes

- All NOT operators should follow the same syntax pattern as their positive counterparts
- The implementation should be consistent with existing operators
- We should preserve the ability to use `NOT` + operator syntax for backward compatibility
- Consider documenting both explicit operators and `NOT` + operator as equivalent options
