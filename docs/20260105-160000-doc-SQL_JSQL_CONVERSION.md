# SQL ↔ JSQL Conversion Guide

## Overview

UMA provides bidirectional conversion between SQL and JSQL (JSON-SQL) formats. This allows you to:

- Convert JSQL queries to SQL for debugging and inspection
- Convert SQL queries to JSQL for programmatic use
- Debug JSQL queries by viewing the generated SQL

## Features

### 1. Debug Mode in JSQL Queries

Enable debug mode to see the generated SQL alongside your query results.

#### Usage

Add `"debug": true` to your JSQL query at the root level (same level as `from` and `select`):

```python
import asyncio
from namerec.uma import initialize_uma, uma_select
from sqlalchemy.ext.asyncio import create_async_engine
from sqlalchemy import MetaData

async def main():
    # Initialize UMA
    engine = create_async_engine('sqlite+aiosqlite:///mydb.db')
    metadata = MetaData()
    await engine.run_sync(metadata.reflect)
    initialize_uma(engine=engine, metadata=metadata)

    # Query with debug mode enabled
    jsql = {
        "from": "users",
        "select": [
            {"field": "id"},
            {"field": "name"}
        ],
        "where": {
            "field": "active",
            "op": "=",
            "value": True
        },
        "debug": True  # Enable debug mode
    }

    result = await uma_select(jsql)
    result_dict = result.to_dict()

    # Access debug SQL
    print("Generated SQL:")
    print(result_dict['debug'])
    print("\nData:")
    print(result_dict['data'])

asyncio.run(main())
```

#### Response Format

When debug mode is enabled, the response includes a `debug` field at the root level:

```json
{
  "meta": [
    {"name": "id", "type": "INTEGER", "nullable": false, ...},
    {"name": "name", "type": "VARCHAR", "nullable": false, ...}
  ],
  "data": [
    [1, "Alice"],
    [2, "Bob"]
  ],
  "debug": "SELECT users.id,\n       users.name\nFROM users\nWHERE users.active = TRUE"
}
```

The debug SQL is formatted for readability with:
- Keywords in UPPERCASE
- Proper indentation
- Parameter values substituted (literal binds)

### 2. Console Script

UMA provides a unified `convert-query` script with automatic format detection:

#### convert-query - Bidirectional Conversion with Auto-Detection

The script automatically detects input format:
- **JSON input** → converts to SQL
- **Text input** → converts to JSQL

```bash
# Auto-detect: JSON → SQL
echo '{"from": "users", "select": [{"field": "id"}, {"field": "name"}]}' | uv run convert-query

# Auto-detect: SQL → JSQL
echo "SELECT id, name FROM users WHERE active = 1" | uv run convert-query

# From file (auto-detect)
uv run convert-query input.json
uv run convert-query query.sql

# Force specific output format
echo "SELECT * FROM users" | uv run convert-query --format jsql
echo '{"from": "users", "select": [{"field": "*"}]}' | uv run convert-query --format sql

# Specify SQL dialect
uv run convert-query input.json --dialect postgresql

# Compact output (no formatting)
echo '{"from": "users", "select": [{"field": "id"}]}' | uv run convert-query --no-pretty
```

**Options:**
- `-d, --dialect`: SQL dialect (generic, postgresql, mysql, sqlite, etc.) [default: generic]
- `--pretty/--no-pretty`: Pretty print output [default: pretty]
- `-f, --format`: Force output format (`sql` or `jsql`) [default: auto-detect]

**Example 1: JSQL → SQL (auto-detected)**

Input:
```json
{
  "from": "users",
  "select": [{"field": "id"}, {"field": "name"}],
  "where": {"field": "active", "op": "=", "value": true},
  "limit": 10
}
```

Command:
```bash
echo '<jsql>' | uv run convert-query
```

Output:
```sql
SELECT users.id,
       users.name
FROM users
WHERE users.active = TRUE
LIMIT 10
```

**Example 2: SQL → JSQL (auto-detected)**

Input:
```sql
SELECT id, name FROM users WHERE active = 1 ORDER BY name LIMIT 10
```

Command:
```bash
echo "SELECT id, name FROM users WHERE active = 1 ORDER BY name LIMIT 10" | uv run convert-query
```

Output:
```json
{
  "from": "users",
  "select": [
    {"field": "id"},
    {"field": "name"}
  ],
  "where": {
    "field": "active",
    "op": "=",
    "value": 1
  },
  "order_by": [
    {"field": "name", "direction": "ASC"}
  ],
  "limit": 10
}
```

### 3. Programmatic API

Use the converter functions directly in your Python code:

```python
from namerec.uma.jsql.converter import jsql_to_sql, sql_to_jsql

# JSQL to SQL
jsql = {
    "from": "users",
    "select": [{"field": "id"}, {"field": "name"}],
    "where": {"field": "active", "op": "=", "value": True}
}
sql = jsql_to_sql(jsql, dialect='postgresql')
print(sql)

# SQL to JSQL
sql = "SELECT id, name FROM users WHERE active = 1"
jsql = sql_to_jsql(sql, dialect='generic')
print(jsql)
```

## Supported Features

### JSQL → SQL Conversion

The `jsql_to_sql()` function supports all JSQL features:

- ✅ SELECT with fields and aliases
- ✅ WHERE with all operators (=, !=, <, >, <=, >=, AND, OR, NOT, IN, LIKE, EXISTS, IS NULL)
- ✅ JOINs (INNER, LEFT, RIGHT, FULL, CROSS)
- ✅ Aggregations (COUNT, SUM, AVG, MIN, MAX)
- ✅ GROUP BY and HAVING
- ✅ ORDER BY (including by aliases)
- ✅ LIMIT and OFFSET
- ✅ CTEs (Common Table Expressions)
- ✅ Subqueries
- ✅ Window functions
- ✅ Parameterized queries

### SQL → JSQL Conversion

The `sql_to_jsql()` function supports:

- ✅ SELECT with fields and aliases
- ✅ WHERE with comparison operators (=, !=, <, >, <=, >=)
- ✅ WHERE with logical operators (AND, OR, NOT)
- ✅ WHERE with IN and LIKE
- ✅ JOINs (INNER, LEFT, RIGHT, FULL)
- ✅ Aggregation functions (COUNT, SUM, etc.)
- ✅ GROUP BY and HAVING
- ✅ ORDER BY with direction
- ✅ LIMIT and OFFSET
- ⚠️ Limited CTE support
- ⚠️ Limited subquery support
- ⚠️ Limited window function support

**Note:** SQL → JSQL conversion uses the `sqlglot` library for parsing. Complex queries may not be fully supported. Always test the conversion output for correctness.

## Error Handling

Both conversion functions raise `JSQLSyntaxError` on invalid input:

```python
from namerec.uma.jsql.converter import sql_to_jsql
from namerec.uma.jsql.exceptions import JSQLSyntaxError

try:
    jsql = sql_to_jsql("INVALID SQL")
except JSQLSyntaxError as e:
    print(f"Error: {e.message}")
    if e.path:
        print(f"Path: {e.path}")
```

## SQL Dialects

The converter supports multiple SQL dialects via the `dialect` parameter:

- `generic` - Generic SQL (default)
- `postgresql` - PostgreSQL
- `mysql` - MySQL
- `sqlite` - SQLite
- `mssql` - Microsoft SQL Server
- `oracle` - Oracle Database
- `snowflake` - Snowflake
- `bigquery` - Google BigQuery
- `redshift` - Amazon Redshift
- And many more...

**Note:** Dialect conversion uses `sqlglot` for transpilation. Not all SQL features may be supported across all dialects.

## Use Cases

### 1. Debugging JSQL Queries

Enable debug mode to see the actual SQL being executed:

```python
jsql = {
    "from": "orders",
    "select": [
        {"func": "SUM", "args": [{"field": "amount"}], "alias": "total"}
    ],
    "joins": [
        {
            "type": "INNER",
            "entity": "customers",
            "on": {"field": "orders.customer_id", "op": "=", "right_field": "customers.id"}
        }
    ],
    "group_by": [{"field": "customers.id"}],
    "debug": True
}

result = await uma_select(jsql)
print(result.to_dict()['debug'])
```

### 2. Learning JSQL Syntax

Convert SQL you're familiar with to JSQL to learn the syntax:

```bash
echo "SELECT id, name FROM users WHERE email LIKE '%@example.com' ORDER BY name DESC" | \
  uv run python sql2jsql.py
```

### 3. Migrating from SQL to JSQL

Convert existing SQL queries to JSQL for use in your application:

```bash
# Convert a file with SQL queries
uv run python sql2jsql.py existing_queries.sql > converted_jsql.json
```

### 4. Generating SQL Documentation

Generate SQL versions of your JSQL queries for documentation:

```bash
# Convert JSQL queries to SQL for documentation
uv run python jsql2sql.py api_queries.json > api_queries.sql
```

### 5. Cross-Database Testing

Test the same query across different databases:

```python
from namerec.uma.jsql.converter import jsql_to_sql

jsql = {"from": "users", "select": [{"field": "*"}], "limit": 10}

# Generate SQL for different databases
pg_sql = jsql_to_sql(jsql, dialect='postgresql')
mysql_sql = jsql_to_sql(jsql, dialect='mysql')
mssql_sql = jsql_to_sql(jsql, dialect='mssql')

print("PostgreSQL:", pg_sql)
print("MySQL:", mysql_sql)
print("MSSQL:", mssql_sql)
```

## Limitations

### SQL → JSQL Conversion

- Only `SELECT` queries are supported (no INSERT, UPDATE, DELETE)
- Complex subqueries may not be fully preserved
- Window functions may require manual adjustment
- CTEs (WITH clauses) have limited support
- Some dialect-specific features may not convert correctly

### JSQL → SQL Conversion

- Requires a temporary SQLite engine for parsing (lightweight, no database needed)
- Parameterized queries are rendered with literal values for debugging
- Custom functions may not be available in all dialects

## Best Practices

1. **Always test converted queries**: SQL → JSQL conversion is best-effort. Always test the output.

2. **Use debug mode during development**: Enable debug mode to understand what SQL is being generated.

3. **Specify dialect when needed**: Use the appropriate dialect parameter for database-specific SQL.

4. **Format for readability**: Use `--pretty` (default) for human-readable output.

5. **Validate complex queries**: For queries with CTEs, window functions, or subqueries, manually verify the conversion.

## Dependencies

The conversion features require additional packages:

```toml
dependencies = [
    "sqlalchemy>=2.0.0",
    "sqlglot>=25.0.0",  # For SQL parsing and transpilation
    "sqlparse>=0.5.0",  # For SQL formatting
    "click>=8.1.0",     # For console scripts
]
```

These are automatically installed when you install UMA.

## Examples

See `examples/converter_usage.py` for complete working examples of all conversion features.

## Troubleshooting

### "Failed to parse SQL"

- Check that your SQL is valid SELECT syntax
- Try specifying the correct dialect with `-d`
- Some complex SQL features may not be supported

### "Invalid JSQL"

- Verify that your JSQL follows the correct structure
- Check the error message for the specific path where the error occurred
- See `docs/20260104-220450-doc-JSQL_SPECIFICATION.md` for full JSQL syntax

### "Import error: sqlglot not found"

Install dependencies:
```bash
uv pip install sqlglot sqlparse click
```

## See Also

- [JSQL Specification](20260104-220450-doc-JSQL_SPECIFICATION.md) - Complete JSQL syntax reference
- [Quickstart Guide](20260104-195802-doc-QUICKSTART.md) - Getting started with UMA
- [API Summary](20260104-195632-doc-API_SUMMARY.md) - Complete API reference
