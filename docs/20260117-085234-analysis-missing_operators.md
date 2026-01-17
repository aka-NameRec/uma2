# Analysis: Missing SQL Operators in UMA

**Date:** 2026-01-17  
**Task:** Identify standard SQL operators not currently supported in UMA  
**Status:** Analysis Complete

## Summary

This document analyzes which standard SQL operators are missing from UMA's JSQL implementation. Operators are categorized by priority and implementation complexity.

---

## Currently Supported Operators

### Comparison Operators
- ✅ `=` (EQ)
- ✅ `!=` (NE) - Note: `<>` is not explicitly supported, but `!=` serves the same purpose
- ✅ `<` (LT)
- ✅ `<=` (LE)
- ✅ `>` (GT)
- ✅ `>=` (GE)

### Logical Operators
- ✅ `AND`
- ✅ `OR`
- ✅ `NOT`

### Special Operators
- ✅ `IN` (with values list and subqueries)
- ✅ `BETWEEN ... AND ...`
- ✅ `LIKE` (pattern matching)
- ✅ `IS NULL`
- ✅ `IS NOT NULL`
- ✅ `EXISTS` (with subqueries)

### Arithmetic Operators
- ✅ `+` (ADD)
- ✅ `-` (SUB)
- ✅ `*` (MUL)
- ✅ `/` (DIV)
- ✅ `%` (MOD)

---

## Missing Operators (By Priority)

### 🔴 High Priority - Standard SQL Operators

#### 1. **ILIKE** (Case-Insensitive LIKE)
- **Status:** ❌ Not supported
- **Standard:** PostgreSQL-specific, but commonly used
- **Workaround:** Use `LOWER()` function with `LIKE`
- **Priority:** HIGH (already identified)
- **Implementation:** Similar to LIKE, use `.ilike()` in SQLAlchemy
- **Database Support:**
  - PostgreSQL: Native support
  - MySQL: Not native (SQLAlchemy converts to `LOWER()`)
  - SQLite: Not native (SQLAlchemy converts to `LOWER()`)
  - SQL Server: Not native (SQLAlchemy converts to case-insensitive comparison)

#### 2. **<>** (Not Equal - ISO Standard)
- **Status:** ⚠️ Not explicitly supported
- **Standard:** ISO SQL standard (alternative to `!=`)
- **Workaround:** Use `!=` instead
- **Priority:** LOW (functionally equivalent to `!=`)
- **Note:** Most databases support both `!=` and `<>`, but `<>` is the ISO standard
- **Implementation:** Add to comparison operators, map to `!=` in SQLAlchemy

---

### 🟡 Medium Priority - Convenience Operators

#### 3. **NOT IN** (as explicit operator)
- **Status:** ⚠️ Can be achieved via `NOT` + `IN`, but not as explicit operator
- **Standard:** Common SQL pattern
- **Current Support:** Can write `{"op": "NOT", "condition": {"op": "IN", ...}}`
- **Priority:** MEDIUM (convenience, not functionality)
- **Implementation:** Add `NOT_IN` operator, convert to `NOT(IN(...))` internally
- **Note:** Some databases optimize `NOT IN` differently than `NOT(IN(...))`

#### 4. **NOT LIKE** (as explicit operator)
- **Status:** ⚠️ Can be achieved via `NOT` + `LIKE`
- **Standard:** Common SQL pattern
- **Current Support:** Can write `{"op": "NOT", "condition": {"op": "LIKE", ...}}`
- **Priority:** MEDIUM (convenience, not functionality)
- **Implementation:** Add `NOT_LIKE` operator, convert to `NOT(LIKE(...))` internally

#### 5. **NOT ILIKE** (as explicit operator)
- **Status:** ❌ Not supported (depends on ILIKE support)
- **Standard:** PostgreSQL-specific
- **Priority:** MEDIUM (depends on ILIKE implementation)
- **Implementation:** Add after ILIKE is implemented

#### 6. **NOT BETWEEN** (as explicit operator)
- **Status:** ⚠️ Can be achieved via `NOT` + `BETWEEN`
- **Standard:** Common SQL pattern
- **Current Support:** Can write `{"op": "NOT", "condition": {"op": "BETWEEN", ...}}`
- **Priority:** MEDIUM (convenience, not functionality)
- **Implementation:** Add `NOT_BETWEEN` operator, convert to `NOT(BETWEEN(...))` internally

#### 7. **NOT EXISTS** (as explicit operator)
- **Status:** ⚠️ Can be achieved via `NOT` + `EXISTS`
- **Standard:** Common SQL pattern
- **Current Support:** Can write `{"op": "NOT", "condition": {"op": "EXISTS", ...}}`
- **Priority:** MEDIUM (convenience, not functionality)
- **Implementation:** Add `NOT_EXISTS` operator, convert to `NOT(EXISTS(...))` internally

---

### 🟢 Low Priority - Database-Specific or Advanced Operators

#### 8. **SIMILAR TO** (PostgreSQL pattern matching)
- **Status:** ❌ Not supported
- **Standard:** PostgreSQL-specific
- **Priority:** LOW (database-specific, less common)
- **Implementation:** PostgreSQL-specific, requires regex support
- **Note:** Can be achieved via functions or `LIKE` with escape characters

#### 9. **REGEXP / RLIKE** (Regular expression matching)
- **Status:** ❌ Not supported
- **Standard:** MySQL/PostgreSQL-specific
- **Priority:** LOW (database-specific, complex)
- **Implementation:** Database-specific, requires regex engine
- **Database Support:**
  - MySQL: `REGEXP` / `RLIKE`
  - PostgreSQL: `~` (case-sensitive), `~*` (case-insensitive)
  - SQLite: Limited regex support via functions
  - SQL Server: Limited regex support

#### 10. **~ / ~*** (PostgreSQL regex operators)
- **Status:** ❌ Not supported
- **Standard:** PostgreSQL-specific
- **Priority:** LOW (database-specific)
- **Implementation:** PostgreSQL-specific regex operators
- **Note:** `~` is case-sensitive, `~*` is case-insensitive

#### 11. **Array Operators** (PostgreSQL)
- **Status:** ❌ Not supported
- **Standard:** PostgreSQL-specific
- **Priority:** LOW (database-specific, advanced)
- **Operators:**
  - `@>` (contains)
  - `<@` (contained by)
  - `&&` (overlaps)
  - `||` (concatenate arrays)
- **Implementation:** PostgreSQL-specific, requires array type support

#### 12. **JSON Operators** (PostgreSQL, MySQL 5.7+)
- **Status:** ❌ Not supported
- **Standard:** Database-specific
- **Priority:** LOW (database-specific, advanced)
- **Operators:**
  - `->` (get JSON object field)
  - `->>` (get JSON object field as text)
  - `@>` (JSON contains)
  - `?` (key exists)
- **Implementation:** Database-specific, requires JSON type support

#### 13. **String Concatenation Operators**
- **Status:** ⚠️ Not supported as operators
- **Standard:** `||` (SQL standard), `CONCAT()` (function)
- **Priority:** LOW (can use `CONCAT()` function)
- **Current Support:** Functions are supported, but `||` operator is not
- **Implementation:** Add `||` as arithmetic operator, map to SQLAlchemy's `+` for strings or `||` for PostgreSQL

#### 14. **NULL-Safe Comparison Operators**
- **Status:** ❌ Not supported
- **Standard:** MySQL-specific (`<=>`)
- **Priority:** LOW (database-specific, rarely needed)
- **Implementation:** MySQL-specific NULL-safe equality operator

---

## Operators That Can Be Achieved via NOT

These operators are **functionally available** through `NOT` operator, but not as explicit operators:

1. ✅ `NOT IN` → `{"op": "NOT", "condition": {"op": "IN", ...}}`
2. ✅ `NOT LIKE` → `{"op": "NOT", "condition": {"op": "LIKE", ...}}`
3. ✅ `NOT BETWEEN` → `{"op": "NOT", "condition": {"op": "BETWEEN", ...}}`
4. ✅ `NOT EXISTS` → `{"op": "NOT", "condition": {"op": "EXISTS", ...}}`

**Recommendation:** Consider adding explicit operators for convenience, but they are not critical.

---

## Functions vs Operators

Some SQL features are implemented as **functions** rather than operators. UMA supports functions, so these are available:

- ✅ `COALESCE()` - Available as function
- ✅ `NULLIF()` - Available as function
- ✅ `CONCAT()` - Available as function
- ✅ `LOWER()` / `UPPER()` - Available as functions
- ✅ String functions - Available as functions

**Note:** The `||` string concatenation operator is not supported, but `CONCAT()` function can be used instead.

---

## Recommendations

### Immediate Actions (High Priority)
1. **Implement ILIKE** - Already planned, high priority
2. **Consider adding `<>`** - ISO standard, low effort

### Future Enhancements (Medium Priority)
3. **Add explicit NOT operators** - Convenience operators:
   - `NOT IN`
   - `NOT LIKE`
   - `NOT ILIKE` (after ILIKE is implemented)
   - `NOT BETWEEN`
   - `NOT EXISTS`

### Optional Enhancements (Low Priority)
4. **Database-specific operators** - Only if needed:
   - PostgreSQL regex operators (`~`, `~*`)
   - PostgreSQL array operators
   - JSON operators
   - String concatenation operator (`||`)

---

## Implementation Complexity

| Operator | Complexity | Files to Modify | Notes |
|----------|-----------|-----------------|-------|
| `ILIKE` | Low | 4 files | Similar to LIKE |
| `<>` | Low | 2 files | Add to comparison operators |
| `NOT IN` | Low | 3 files | Wrapper around NOT + IN |
| `NOT LIKE` | Low | 3 files | Wrapper around NOT + LIKE |
| `NOT ILIKE` | Low | 3 files | Depends on ILIKE |
| `NOT BETWEEN` | Low | 3 files | Wrapper around NOT + BETWEEN |
| `NOT EXISTS` | Low | 3 files | Wrapper around NOT + EXISTS |
| `SIMILAR TO` | Medium | 4 files | PostgreSQL-specific |
| `REGEXP` | Medium | 4 files | Database-specific |
| `~ / ~*` | Medium | 4 files | PostgreSQL-specific |
| Array operators | High | 5+ files | Requires type system changes |
| JSON operators | High | 5+ files | Requires type system changes |
| `||` (string concat) | Low | 2 files | Add to arithmetic operators |

---

## Testing Considerations

For each new operator:
1. **JSQL → SQLAlchemy** - Verify correct SQLAlchemy expression generation
2. **SQL → JSQL** - Verify SQL parsing and conversion
3. **Database compatibility** - Test with all supported databases
4. **Edge cases** - NULL handling, type coercion, etc.
5. **Performance** - Ensure no performance regressions

---

## Conclusion

**Critical Missing Operators:**
- `ILIKE` (already identified, high priority)

**Convenience Operators (Medium Priority):**
- Explicit `NOT IN`, `NOT LIKE`, `NOT BETWEEN`, `NOT EXISTS` operators
- `<>` (ISO standard alternative to `!=`)

**Database-Specific Operators (Low Priority):**
- PostgreSQL regex operators
- Array and JSON operators
- String concatenation operator

**Recommendation:** Focus on `ILIKE` first, then consider convenience operators based on user needs. Database-specific operators should be added only if there's a clear use case.
