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

## Documentation

- [Quick Start Guide](docs/20260104-195802-doc-QUICKSTART.md) - Get started with UMA in minutes
- [API Summary](docs/20260104-195632-doc-API_SUMMARY.md) - Complete API reference and examples
- [API Implementation Summary](docs/20260104-195602-doc-API_IMPLEMENTATION_SUMMARY.md) - Implementation details and internals
- [Project Structure](docs/20260104-195714-doc-PROJECT_STRUCTURE.md) - Architecture and design documentation

## License

[License information to be added]

## Contributing

[Contributing guidelines to be added]
