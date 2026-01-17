# UMA - Unified Model Access

**UMA** (Unified Model Access) is a Python package that provides unified access to relational databases for frontend applications, eliminating the need to write numerous REST API endpoints or deal with the complexity of GraphQL.

## Motivation

Modern web application development faces two main alternatives:

1. **REST API** - requires creating numerous endpoints that essentially do the same thing
2. **GraphQL** - requires significant backend work and has a high learning curve

UMA solves these problems for an important use case: when there's a relational database behind the frontend. It enables frontend developers to use SQL-like queries in JSON format while keeping backend developers free to focus on creative tasks instead of writing boilerplate endpoints.

## Core Concept

UMA allows frontend developers to create JSON-based SQL queries (JSQL) that are:

1. Translated into SQLAlchemy Core expressions on the backend
2. Validated against user permissions
3. Executed safely via SQLAlchemy
4. Returned as structured JSON with metadata

## Key Features

### 1. JSQL Query Execution (`uma-select`)
Execute JSON-formatted SQL queries that are:
- Converted to SQLAlchemy Core objects
- Validated for access permissions
- Safely executed against the database

### 2. Virtual Views (`views`)
Backend developers can define reusable views (CTEs in SQLAlchemy terms) that frontend developers can use as regular tables in JSQL queries.

### 3. Access Control Hooks
Integration points for implementing custom permission checks on tables accessed in JSQL queries.

### 4. Metadata API
- `uma-meta-tables`: List all tables and registered views
- `uma-meta-details`: Get detailed field information for a specific table/view

### 5. CRUD Operations
Full support for Create, Read, Update, Delete operations with the same permission model.

### 6. SQL ↔ JSQL Conversion
Bidirectional conversion between SQL and JSQL formats:
- **Debug Mode**: View generated SQL alongside query results
- **Console Scripts**: Convert between SQL and JSQL from command line
- **Programmatic API**: Use conversion functions in your code

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Frontend Layer                       │
│              (JSQL JSON Requests)                       │
└────────────────────┬────────────────────────────────────┘
                     │
┌────────────────────▼────────────────────────────────────┐
│              UMA Package Components                     │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  1. JSQL Parser                                         │
│     - JSON → AST → SQLAlchemy Core Expressions          │
│                                                         │
│  2. View Registry                                       │
│     - CTE definitions as SQLAlchemy Selectable          │
│     - View metadata (name, description, fields)         │
│                                                         │
│  3. Access Control Hook System                          │
│     - Table extraction from query                       │
│     - User-defined permission checks                    │
│                                                         │
│  4. Query Executor                                      │
│     - SQLAlchemy Core execution                         │
│     - Result serialization                              │
│                                                         │
│  5. Metadata Providers                                  │
│     - uma-meta-tables: List all tables/views            │
│     - uma-meta-details: Field details for table/view    │
└─────────────────────────────────────────────────────────┘
```

## Advantages Over Alternatives

### vs PostgREST
- **Database independence**: Works with PostgreSQL, MySQL, SQLite, SQL Server, Oracle via SQLAlchemy
- **External access control**: Application-level permission hooks instead of RLS
- **Programmatic views**: Register views as SQLAlchemy Selectable objects

### vs Hasura
- **External access control**: Full control over permission logic in application code
- **JSQL format**: Closer to SQL, more intuitive for fullstack developers
- **Python package**: Integrates directly into existing applications

### vs Supabase
- **Database independence**: Not tied to PostgreSQL
- **External access control**: Application-level permissions
- **Integration**: Python package that integrates into existing projects

## Technology Stack

- **Python 3.11+**
- **SQLAlchemy 2.0 Core** (without ORM)
- **uv** package manager
- Database-agnostic (supports all SQLAlchemy-compatible databases)

## Use Cases

UMA is designed for:

- Complex admin panels and CMS systems
- Applications requiring flexible data access with many relationships
- Fullstack development where developers understand database operations
- Projects needing m2m (many-to-many) field support in frontend forms
- Applications with custom permission systems

## Quick Examples

### Basic JSQL Query

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

    # Execute JSQL query
    jsql = {
        "from": "users",
        "select": [{"field": "id"}, {"field": "name"}],
        "where": {
            "op": "=",
            "left": {"field": "active"},
            "right": {"value": True}
        },
        "limit": 10
    }
    
    result = await uma_select(jsql)
    print(result.to_dict())

asyncio.run(main())
```

### Query Parameters

JSQL supports parameterized queries for security and performance. Parameters use `{"param": "name"}` syntax and values are passed separately:

```python
# JSQL query with parameter
jsql = {
    "from": "orders",
    "select": ["id", "amount"],
    "where": {
        "op": ">=",
        "left": {"field": "amount"},
        "right": {"param": "min_amount"}  # Parameter reference
    }
}

# Pass parameter values separately
params = {"min_amount": 100}
result = await uma.select(jsql, params=params)
```

**Key benefits:**
- **Security**: Parameters are bound using SQLAlchemy's parameterized queries, preventing SQL injection
- **Caching**: SQL with parameter placeholders is cached once and reused with different parameter values
- **Performance**: Database can optimize parameterized queries better than literal values

**Multiple parameters:**
```python
jsql = {
    "from": "orders",
    "select": ["id", "amount"],
    "where": {
        "op": "AND",
        "conditions": [
            {"op": ">=", "left": {"field": "amount"}, "right": {"param": "min_amount"}},
            {"op": "<=", "left": {"field": "amount"}, "right": {"param": "max_amount"}}
        ]
    }
}

params = {"min_amount": 50, "max_amount": 200}
result = await uma.select(jsql, params=params)
```

### Debug Mode

```python
# Enable debug mode to see generated SQL
jsql = {
    "from": "users",
    "select": [{"field": "id"}],
    "debug": True  # Returns SQL in response
}

result = await uma_select(jsql)
print(result.to_dict()['debug'])  # Formatted SQL
```

### SQL ↔ JSQL Conversion

```bash
# Auto-detect format and convert
echo '{"from": "users", "select": [{"field": "*"}]}' | uv run convert-query
echo "SELECT * FROM users WHERE id = 1" | uv run convert-query

# Force specific output format
echo "SELECT id FROM users" | uv run convert-query --format jsql
```

## Documentation

- [Quick Start Guide](docs/20260104-195802-doc-QUICKSTART.md) - Get started with UMA in minutes
- [JSQL Specification](docs/20260104-220450-doc-JSQL_SPECIFICATION.md) - Complete JSQL (JSON-SQL) query syntax and examples
- [SQL ↔ JSQL Conversion Guide](docs/20260105-160000-doc-SQL_JSQL_CONVERSION.md) - Debug mode and conversion tools
- [API Summary](docs/20260104-195632-doc-API_SUMMARY.md) - Complete API reference and examples
- [API Implementation Summary](docs/20260104-195602-doc-API_IMPLEMENTATION_SUMMARY.md) - Implementation details and internals
- [Project Structure](docs/20260104-195714-doc-PROJECT_STRUCTURE.md) - Architecture and design documentation

## License

This project is released into the public domain. The contents of this repository are free for any use without restrictions.

## Contributing

Contributions are welcome, but please note that UMA is a low-level architectural library with a carefully designed core.

If you want to contribute:

- **Bug reports and documentation improvements** are always welcome.
- **Feature requests** should be discussed via issues before submitting a PR.
- **Large architectural changes** should be proposed and discussed in advance.

Please keep contributions:
- focused and minimal
- consistent with the existing architecture
- covered with clear reasoning and examples

By contributing, you agree that your contributions will be released into the public domain under the same terms as this project.

## Author

Created by [aka-NameRec](https://github.com/aka-NameRec).
