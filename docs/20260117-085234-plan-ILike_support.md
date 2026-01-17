# Plan: Add ILIKE Operator Support

**Date:** 2026-01-17  
**Task:** Add support for case-insensitive LIKE operator (ILIKE)  
**Status:** Planning

## Problem Statement

Currently, UMA supports the `LIKE` operator for pattern matching, but does not support `ILIKE` (case-insensitive LIKE), which is commonly used in PostgreSQL and other databases for case-insensitive text searches.

## Current State Analysis

### 1. Constants (`src/namerec/uma/jsql/constants.py`)
- ❌ `JSQLOperator` enum does not include `ILIKE`
- ✅ `LIKE` is defined as `JSQLOperator.LIKE = 'LIKE'`

### 2. Parser (`src/namerec/uma/jsql/parser.py`)
- ✅ `_handle_like()` method exists (lines 665-676)
- ✅ `JSQLOperator.LIKE` is registered in `_condition_handlers` (line 70)
- ❌ No handler for `ILIKE` operator
- ❌ `ILIKE` not handled in operator dispatch

### 3. Converter - JSQL→SQL (`src/namerec/uma/jsql/converter/conditions.py`)
- ✅ `convert_comparison_operator()` handles `LIKE` (lines 183-188)
- ✅ `jsql_condition_to_sqlglot()` converts LIKE to `exp.Like`
- ❌ No handling for `ILIKE` operator
- ❌ `ILIKE` not in supported operators list

### 4. Converter - SQL→JSQL (`src/namerec/uma/jsql/converter/conditions.py`)
- ✅ `convert_condition_to_jsql()` handles `exp.Like` (lines 309-332)
- ❌ No handling for `exp.ILike` (sqlglot expression)
- ❌ `ILIKE` not in reverse mapping

### 5. Converter - Operators Mapping (`src/namerec/uma/jsql/converter/operators.py`)
- ❌ No mapping for `ILIKE` operator
- ❌ No reverse mapping for `exp.ILike`

### 6. SQLAlchemy Support
- ✅ SQLAlchemy Core provides `.ilike()` method on column expressions
- ✅ SQLAlchemy supports `ILIKE` for PostgreSQL and other databases

### 7. sqlglot Support
- ✅ sqlglot supports `exp.ILike` for PostgreSQL
- ✅ sqlglot can parse `ILIKE` in SQL queries

## Implementation Plan

### Phase 1: Add ILIKE to Constants

**File:** `src/namerec/uma/jsql/constants.py`

1. Add `ILIKE` to `JSQLOperator` enum:
   ```python
   ILIKE = 'ILIKE'
   ```
   Place it after `LIKE = 'LIKE'` (around line 80)

**Impact:** Low risk, no breaking changes

---

### Phase 2: Add ILIKE Handler to Parser

**File:** `src/namerec/uma/jsql/parser.py`

1. Create `_handle_ilike()` method:
   - Copy logic from `_handle_like()` (lines 665-676)
   - Change `.like()` to `.ilike()` on SQLAlchemy expression
   - Use `JSQLOperator.ILIKE` in error messages

2. Register handler in `_condition_handlers`:
   - Add `JSQLOperator.ILIKE: self._handle_ilike` (after LIKE, around line 71)

**Impact:** Low risk, adds new functionality only

---

### Phase 3: Add ILIKE to JSQL→SQL Converter

**File:** `src/namerec/uma/jsql/converter/conditions.py`

1. In `convert_comparison_operator()` method:
   - Add `ILIKE` handling after `LIKE` handling (around line 188)
   - Use `exp.ILike` instead of `exp.Like`
   - Follow same pattern as `LIKE`

2. Update supported operators list:
   - Add `JSQLOperator.ILIKE.value` to supported list (around line 200)

**Impact:** Low risk, extends existing functionality

---

### Phase 4: Add ILIKE to SQL→JSQL Converter

**File:** `src/namerec/uma/jsql/converter/conditions.py`

1. In `convert_condition_to_jsql()` method:
   - Add handling for `exp.ILike` expression (after `exp.Like`, around line 310)
   - Return JSQL with `'op': JSQLOperator.ILIKE.value`
   - Follow same pattern as `LIKE` conversion

**Impact:** Low risk, extends SQL parsing capabilities

---

### Phase 5: Update Operator Mappings (if needed)

**File:** `src/namerec/uma/jsql/converter/operators.py`

**Note:** ILIKE is not a comparison operator, so it doesn't need to be added to `COMPARISON_OP_TO_SQLGLOT` or `SQLGLOT_TO_COMPARISON`. However, verify that sqlglot properly handles `exp.ILike`.

**Impact:** None, ILIKE is handled as special operator like LIKE

---

### Phase 6: Testing

**Files:** `tests/test_converter.py`, `tests/test_jsql.py`

1. Add tests for ILIKE in JSQL queries:
   - Test ILIKE in WHERE clause
   - Test ILIKE with parameters
   - Test ILIKE in HAVING clause

2. Add tests for SQL→JSQL conversion:
   - Test parsing SQL with ILIKE operator
   - Verify conversion to JSQL format

3. Add tests for JSQL→SQL conversion:
   - Test converting JSQL with ILIKE to SQL
   - Verify SQLAlchemy generates correct ILIKE SQL

4. Add integration tests:
   - Test end-to-end: JSQL → SQLAlchemy → SQL
   - Test with PostgreSQL (which supports ILIKE natively)
   - Test with SQLite (which doesn't support ILIKE natively - verify fallback behavior)

**Impact:** Ensures correctness and prevents regressions

---

### Phase 7: Documentation Updates

**Files:** Documentation files mentioning LIKE operator

1. Update JSQL specification:
   - Add ILIKE to supported operators list
   - Document ILIKE syntax and usage
   - Note database-specific behavior

2. Update examples:
   - Add example with ILIKE operator
   - Show difference between LIKE and ILIKE

3. Update API documentation:
   - Document ILIKE in operator reference

**Impact:** Improves developer experience

---

## Database Compatibility Considerations

### PostgreSQL
- ✅ Native support for `ILIKE`
- ✅ SQLAlchemy generates `ILIKE` directly

### MySQL
- ❌ Does not support `ILIKE` natively
- ✅ SQLAlchemy may convert to `LOWER()` function calls
- **Action:** Verify SQLAlchemy behavior, document limitations

### SQLite
- ❌ Does not support `ILIKE` natively
- ✅ SQLAlchemy may convert to `LOWER()` function calls
- **Action:** Verify SQLAlchemy behavior, document limitations

### SQL Server
- ❌ Does not support `ILIKE` natively
- ✅ SQLAlchemy may convert to case-insensitive comparison
- **Action:** Verify SQLAlchemy behavior, document limitations

**Recommendation:** Test with all supported databases and document behavior differences.

---

## Implementation Order

1. ✅ Phase 1: Constants (foundation)
2. ✅ Phase 2: Parser (core functionality)
3. ✅ Phase 3: JSQL→SQL Converter (output)
4. ✅ Phase 4: SQL→JSQL Converter (input)
5. ✅ Phase 6: Testing (verification)
6. ✅ Phase 7: Documentation (completion)

**Note:** Phases 1-4 can be done in parallel, but Phase 1 must be completed first.

---

## Risk Assessment

### Low Risk Items
- Adding enum value (Phase 1)
- Adding handler methods (Phase 2, 3, 4)
- Adding tests (Phase 6)

### Medium Risk Items
- Database compatibility differences (handled by SQLAlchemy)
- Potential breaking changes if ILIKE already exists somewhere (unlikely)

### Mitigation Strategies
1. Comprehensive testing across all supported databases
2. Document database-specific behavior
3. Follow existing patterns (LIKE implementation)
4. Ensure backward compatibility (no changes to LIKE)

---

## Success Criteria

- [ ] `ILIKE` operator can be used in JSQL WHERE clauses
- [ ] `ILIKE` operator can be used in JSQL HAVING clauses
- [ ] SQL queries with `ILIKE` can be converted to JSQL
- [ ] JSQL queries with `ILIKE` can be converted to SQL
- [ ] Tests pass for all supported databases
- [ ] Documentation updated
- [ ] No breaking changes introduced

---

## Notes

- ILIKE should follow the same syntax pattern as LIKE:
  ```json
  {
    "op": "ILIKE",
    "left": {"field": "name"},
    "right": {"value": "%john%"}
  }
  ```
  or
  ```json
  {
    "op": "ILIKE",
    "field": "name",
    "pattern": "%john%"
  }
  ```

- The implementation should mirror LIKE as closely as possible to maintain consistency.

- Consider future extensions:
  - `NOT ILIKE` operator (if needed)
  - Pattern escaping utilities (if needed)
