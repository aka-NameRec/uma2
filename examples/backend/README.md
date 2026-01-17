# UMA Backend Demo

FastAPI backend application demonstrating UMA (Unified Model Access) capabilities for processing JSQL queries.

## Features

- **JSQL Query Execution**: Execute JSON-formatted SQL queries
- **CRUD Operations**: Read, save, delete records with access control
- **Entity Metadata**: List entities and get detailed structure information
- **SQL ↔ JSQL Transformation**: Convert between SQL and JSQL formats
- **Structured Logging**: Production-ready logging with structlog
- **Error Handling**: Unified error response format with debug mode
- **Async-First**: Full async/await support for high performance

## Requirements

- Python 3.11+
- PostgreSQL database (with asyncpg driver)
- UMA package (installed from GitHub)

## Installation

1. **Clone the repository** (if not already done):
   ```bash
   git clone https://github.com/aka-NameRec/uma2.git
   cd uma2/examples/backend
   ```

2. **Create virtual environment**:
   ```bash
   python -m venv .venv
   source .venv/bin/activate  # On Windows: .venv\Scripts\activate
   ```

3. **Install dependencies**:
   ```bash
   pip install -e .
   ```

4. **Configure environment**:
   ```bash
   cp .env.example .env
   # Edit .env with your database credentials
   ```

## Configuration

Edit `.env` file:

```env
# PostgreSQL connection string
DATABASE_URL=postgresql+asyncpg://user:password@localhost:5432/database_name

# Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
LOG_LEVEL=INFO

# Debug mode (true/false) - adds detailed traceback to error responses
DEBUG_MODE=false
```

## Running the Application

### Development Mode

```bash
cd src
uvicorn main:app --reload --host 0.0.0.0 --port 8000
```

Or using Python directly:

```bash
cd src
python main.py
```

### Production Mode

```bash
cd src
uvicorn main:app --host 0.0.0.0 --port 8000 --workers 4
```

## API Endpoints

All endpoints use POST method for consistency.

### JSQL Query Execution

**POST** `/api/uma/select`

Execute JSQL query:

```json
{
  "jsql": {
    "from": "users",
    "select": [{"field": "id"}, {"field": "name"}],
    "where": {"op": "=", "left": {"field": "active"}, "right": {"value": true}},
    "limit": 10
  },
  "params": {}
}
```

### CRUD Operations

**POST** `/api/uma/read`

Read a record by ID:

```json
{
  "entity_name": "users",
  "id": 123
}
```

**POST** `/api/uma/save`

Save (create or update) a record:

```json
{
  "entity_name": "users",
  "data": {
    "name": "John Doe",
    "email": "john@example.com"
  }
}
```

Returns the full record body after save.

**POST** `/api/uma/delete`

Delete a record:

```json
{
  "entity_name": "users",
  "id": 123
}
```

### Entity Metadata

**POST** `/api/uma/meta/entity_list`

List available entities:

```json
{
  "namespace": null
}
```

**POST** `/api/uma/meta/entity_details`

Get entity structure:

```json
{
  "entity_name": "users"
}
```

### SQL ↔ JSQL Transformation

**POST** `/api/uma/transform/sql2jsql`

Convert SQL to JSQL:

```json
{
  "data": "SELECT id, name FROM users WHERE active = true LIMIT 10",
  "dialect": "postgresql"
}
```

**POST** `/api/uma/transform/jsql2sql`

Convert JSQL to SQL:

```json
{
  "data": {
    "from": "users",
    "select": [{"field": "id"}, {"field": "name"}],
    "where": {"op": "=", "left": {"field": "active"}, "right": {"value": true}},
    "limit": 10
  },
  "dialect": "postgresql"
}
```

## Error Handling

All errors return a unified format:

```json
{
  "id": "UMAAccessDeniedError",
  "message": "Access denied for read on users",
  "details": {
    "entity_name": "users",
    "operation": "read"
  }
}
```

In `DEBUG_MODE=true`, errors include full traceback in `details.traceback`.

## Logging

Logs use ISO timestamp format with structured context:

```
2026-01-10T17:45:12.123456 INFO: UMA initialized successfully namespace=main
2026-01-10T17:45:13.234567 ERROR: JSQL syntax error path=where.conditions[0].op message=Invalid operator
```

Log verbosity is controlled by `LOG_LEVEL` environment variable.

## Health Check

**GET** `/health`

Returns application status:

```json
{
  "status": "ok",
  "service": "uma-backend-demo"
}
```

## Interactive API Documentation

Once running, visit:

- Swagger UI: http://localhost:8000/docs
- ReDoc: http://localhost:8000/redoc

## Architecture

- **FastAPI**: Modern async web framework
- **UMA**: Unified Model Access for database operations
- **structlog**: Structured logging with standard logging integration
- **dependency-injector**: Dependency injection container
- **Pydantic**: Request/response validation
- **asyncpg**: Async PostgreSQL driver

## License

Public Domain - free for any use without restrictions.
