# UMA Project Structure

This document describes the structure of the UMA (Unified Model Access) project.

## Directory Layout

```
uma2/
├── src/                          # Source code (src-layout)
│   └── namerec/                  # Vendor namespace
│       ├── __init__.py           # Namespace package marker
│       └── uma/                  # UMA package
│           ├── __init__.py       # Public API exports
│           ├── py.typed          # PEP 561 marker for type checking
│           ├── core/             # Core components
│           │   ├── __init__.py
│           │   ├── types.py      # Type definitions (EntityName, Operation, Protocols)
│           │   ├── context.py    # UMAContext
│           │   ├── exceptions.py # Exception hierarchy
│           │   └── utils.py      # Utility functions
│           ├── handlers/         # Entity handlers
│           │   ├── __init__.py
│           │   ├── base.py       # DefaultEntityHandler
│           │   └── virtual.py    # VirtualViewHandler
│           ├── jsql/             # JSQL (JSON-SQL) implementation
│           │   ├── __init__.py
│           │   ├── types.py      # JSQL type definitions
│           │   ├── exceptions.py # JSQL-specific exceptions
│           │   ├── parser.py     # JSQL to SQLAlchemy parser
│           │   ├── executor.py   # JSQL query executor
│           │   └── result.py     # Result builder with metadata
│           ├── application.py    # UMA application class
│           ├── registry.py       # EntityRegistry
│           └── metadata.py       # DefaultMetadataProvider
├── tests/                        # Tests
│   ├── __init__.py
│   ├── conftest.py              # Pytest fixtures
│   ├── test_basic.py            # Basic tests
│   ├── test_api.py              # API tests
│   └── test_jsql.py             # JSQL parser and executor tests
├── examples/                     # Usage examples
│   ├── basic_usage.py           # Basic CRUD operations
│   ├── api_usage.py             # API usage with access control
│   └── jsql_usage.py            # JSQL query examples
├── docs/                         # Documentation
│   ├── 20251201-010207-intro.md
│   ├── 20251202-050343-log-concept.md
│   ├── 20260103-130403-log-impl_proposal.md
│   ├── 20260104-142932-log-impl_proposal-entity_handlers.md
│   ├── 20260104-195602-doc-API_IMPLEMENTATION_SUMMARY.md
│   ├── 20260104-195632-doc-API_SUMMARY.md
│   ├── 20260104-195714-doc-PROJECT_STRUCTURE.md
│   ├── 20260104-195802-doc-QUICKSTART.md
│   ├── 20260104-203856-log-project_setup.md
│   └── 20260104-220450-doc-JSQL_SPECIFICATION.md
├── context_portal/              # ConPort data (gitignored)
├── logs/                        # Logs (gitignored)
├── README.md                    # Project overview
├── PROJECT_STRUCTURE.md         # This file
├── pyproject.toml              # Project configuration
└── .gitignore                  # Git ignore rules
```

## Package Import Structure

After installation, the package can be imported as:

```python
from namerec.uma import (
    # Main class
    UMA,
    
    # Core types
    EntityName,
    Operation,
    EntityHandler,
    MetadataProvider,
    UMAContext,
    NamespaceConfig,
    
    # Exceptions
    UMAError,
    UMAAccessDeniedError,
    UMANotFoundError,
    UMANotImplementedError,
    UMAValidationError,
    
    # Utils
    parse_entity_name,
    form_entity_name,
    copy_field_meta,
    is_virtual_view,
    
    # Handlers
    DefaultEntityHandler,
    VirtualViewHandler,
    
    # Registry
    EntityRegistry,
    
    # Metadata
    DefaultMetadataProvider,
)
```

## Key Components

### Core Module (`namerec.uma.core`)

- **types.py**: Defines core types and protocols
  - `EntityName`: Structured entity name with optional namespace
  - `Operation`: Enum for UMA operations (SELECT, READ, CREATE, UPDATE, DELETE, META)
  - `EntityHandler`: Protocol for entity handlers
  - `MetadataProvider`: Protocol for metadata providers

- **context.py**: Execution context
  - `UMAContext`: Contains engine, metadata, metadata_provider, user_context, cache, and extra fields

- **exceptions.py**: Exception hierarchy
  - `UMAError`: Base exception
  - `UMAAccessDeniedError`: Access denied
  - `UMANotFoundError`: Entity/record not found
  - `UMANotImplementedError`: Operation not implemented
  - `UMAValidationError`: Validation error

- **utils.py**: Utility functions
  - `parse_entity_name()`: Parse entity name string
  - `form_entity_name()`: Create EntityName from components
  - `copy_field_meta()`: Copy field metadata from source entity
  - `is_virtual_view()`: Check if entity is virtual view

### Handlers Module (`namerec.uma.handlers`)

- **base.py**: Default handler for database tables
  - `DefaultEntityHandler`: Implements all operations for regular tables
  - Supports simple and composite primary keys
  - Automatic metadata extraction from SQLAlchemy

- **virtual.py**: Base class for virtual views
  - `VirtualViewHandler`: Base class with default implementations
  - Raises `UMANotImplementedError` for operations that must be overridden
  - Automatically marks metadata with `is_virtual_view=True`

### Registry Module (`namerec.uma.registry`)

- `EntityRegistry`: Maps entity names to handlers
- Global registry singleton pattern
- Fallback to default handler for unregistered entities
- Auto-discovery of database tables

### Metadata Module (`namerec.uma.metadata`)

- `DefaultMetadataProvider`: Stores and retrieves custom metadata
- Can be extended for Redis or other storage backends

## Design Principles

1. **Stateless Handlers**: Entity handlers are classes with only classmethods, no instance state
2. **Explicit Context**: All necessary dependencies passed via `UMAContext`
3. **Protocol-Based**: Uses Python protocols for type safety and flexibility
4. **Async-First**: All operations are async for better scalability
5. **Extensible**: Easy to extend with custom handlers, metadata providers, and context fields

## Development Workflow

### Setup

```bash
# Install dependencies
uv sync --dev

# Run tests
uv run pytest

# Run linters
ruff check src/
mypy src/
```

### Creating Custom Handlers

1. Inherit from `DefaultEntityHandler` or `VirtualViewHandler`
2. Override necessary methods
3. Register with `EntityRegistry`

Example:

```python
from namerec.uma import VirtualViewHandler, UMA, DefaultMetadataProvider, NamespaceConfig

class MyView(VirtualViewHandler):
    @classmethod
    async def select(cls, entity_name, params, context):
        # Implementation
        pass

uma = UMA.create({
    'main': NamespaceConfig(
        engine=engine,
        metadata_provider=DefaultMetadataProvider(),
    ),
})
uma.registry.register('my_view', MyView)
```

## Testing Strategy

- Unit tests for core components
- Integration tests with SQLite in-memory database
- Async tests using pytest-asyncio
- Fixtures in `conftest.py` for common setup

## Future Enhancements

- JSQL parser (JSON to SQLAlchemy) - ✅ Implemented (see [JSQL Specification](20260104-220450-doc-JSQL_SPECIFICATION.md))
- Access control hooks - ✅ Implemented (via MetadataProvider.can())
- Query executor - ✅ Implemented (JSQLExecutor)
- API layer (UMA class with methods) - ✅ Implemented
- Support for many-to-many relationships
- Caching layer - ✅ Implemented (MemoryCacheBackend, RedisCacheBackend)
- Performance optimizations
