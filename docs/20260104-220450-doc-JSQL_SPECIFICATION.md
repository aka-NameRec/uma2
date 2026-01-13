# JSQL (JSON-SQL) Specification

**Document ID**: 20260104-220450  
**Status**: Proposal  
**Version**: 1.0

## Overview

JSQL is a JSON representation of SQL SELECT queries for UMA (Unified Model Access). It provides a structured way to express SQL SELECT queries in JSON format, supporting most common SQL features while maintaining simplicity and type safety.

## Design Principles

1. **SELECT-only**: Only SELECT queries are supported (no DML/DDL)
2. **Async-first**: All execution is asynchronous
3. **Type-safe**: Result metadata includes column types
4. **Composable**: Support for CTEs and subqueries
5. **Expressive**: Support for joins, aggregations, window functions

## JSQL Structure

### Basic Query

```json
{
  "from": "users",
  "select": ["id", "name", "email"],
  "where": {
    "op": "=",
    "left": {"field": "active"},
    "right": {"value": true}
  },
  "order_by": [{"field": "name", "direction": "asc"}],
  "limit": 10,
  "offset": 0
}
```

**Equivalent SQL**:
```sql
SELECT id, name, email
FROM users
WHERE active = true
ORDER BY name ASC
LIMIT 10 OFFSET 0
```

### Select Clause

The `select` field supports:
- Simple field names: `"id"`
- Aliased fields: `{"field": "first_name", "alias": "name"}`
- Expressions: `{"expr": {...}, "alias": "total"}`
- Aggregations: `{"func": "COUNT", "args": ["id"], "alias": "count"}`
- Wildcard: `"*"` (selects all columns)

**Example with aliases and expressions**:
```json
{
  "from": "orders",
  "select": [
    {"field": "id"},
    {"field": "customer_id", "alias": "customer"},
    {
      "func": "SUM",
      "args": [{"field": "amount"}],
      "alias": "total_amount"
    },
    {
      "expr": {
        "op": "*",
        "left": {"field": "quantity"},
        "right": {"field": "price"}
      },
      "alias": "line_total"
    }
  ],
  "group_by": [{"field": "customer_id"}]
}
```

**Equivalent SQL**:
```sql
SELECT 
  id,
  customer_id AS customer,
  SUM(amount) AS total_amount,
  quantity * price AS line_total
FROM orders
GROUP BY customer_id
```

### Expressions

Expressions support operators and functions:

```json
{
  "op": "+",
  "left": {"field": "base_price"},
  "right": {
    "op": "*",
    "left": {"field": "base_price"},
    "right": {"value": 0.2}
  }
}
```

**Equivalent SQL**: `base_price + (base_price * 0.2)`

**Supported operators**: `=`, `!=`, `<`, `<=`, `>`, `>=`, `+`, `-`, `*`, `/`, `%`, `AND`, `OR`, `NOT`, `IN`, `BETWEEN`, `LIKE`, `IS NULL`, `IS NOT NULL`

### Functions

**Standard aggregations**:
```json
{
  "func": "COUNT",
  "args": [{"field": "id"}],
  "distinct": true
}
```

**Equivalent SQL**: `COUNT(DISTINCT id)`

**Supported functions**:
- Aggregations: `COUNT`, `SUM`, `AVG`, `MIN`, `MAX`
- Array aggregation: `ARRAY_AGG` (PostgreSQL only - not supported in SQLite)
- String functions: `CONCAT`, `UPPER`, `LOWER`, `SUBSTRING`
- Date functions: `DATE`, `YEAR`, `MONTH`, `DAY`
- Mathematical: `ROUND`, `ABS`, `CEIL`, `FLOOR`

**Note**: `ARRAY_AGG` is specific to PostgreSQL and will not work with SQLite. For SQLite, consider using `GROUP_CONCAT` instead (though it returns a string, not an array).

**Array aggregation example**:
```json
{
  "from": "users",
  "select": [
    {"field": "department"},
    {
      "func": "ARRAY_AGG",
      "args": [{"field": "name"}],
      "alias": "employee_names"
    }
  ],
  "group_by": [{"field": "department"}]
}
```

**Equivalent SQL**:
```sql
SELECT department, ARRAY_AGG(name) AS employee_names
FROM users
GROUP BY department
```

### Joins

```json
{
  "from": "orders",
  "joins": [
    {
      "type": "LEFT",
      "table": "customers",
      "alias": "c",
      "on": {
        "op": "=",
        "left": {"field": "orders.customer_id"},
        "right": {"field": "c.id"}
      }
    },
    {
      "type": "INNER",
      "table": "products",
      "alias": "p",
      "on": {
        "op": "AND",
        "conditions": [
          {
            "op": "=",
            "left": {"field": "orders.product_id"},
            "right": {"field": "p.id"}
          },
          {
            "op": "=",
            "left": {"field": "p.active"},
            "right": {"value": true}
          }
        ]
      }
    }
  ],
  "select": [
    {"field": "orders.id"},
    {"field": "c.name", "alias": "customer_name"},
    {"field": "p.name", "alias": "product_name"}
  ]
}
```

**Equivalent SQL**:
```sql
SELECT 
  orders.id,
  c.name AS customer_name,
  p.name AS product_name
FROM orders
LEFT JOIN customers AS c ON orders.customer_id = c.id
INNER JOIN products AS p ON orders.product_id = p.id AND p.active = true
```

**Join types**: `INNER`, `LEFT`, `RIGHT`, `FULL`, `CROSS`

### Common Table Expressions (CTEs)

```json
{
  "with": [
    {
      "name": "active_users",
      "query": {
        "from": "users",
        "select": ["id", "name", "department"],
        "where": {
          "op": "=",
          "left": {"field": "active"},
          "right": {"value": true}
        }
      }
    },
    {
      "name": "dept_counts",
      "query": {
        "from": "active_users",
        "select": [
          {"field": "department"},
          {"func": "COUNT", "args": ["id"], "alias": "user_count"}
        ],
        "group_by": [{"field": "department"}]
      }
    }
  ],
  "from": "dept_counts",
  "select": ["*"],
  "order_by": [{"field": "user_count", "direction": "desc"}]
}
```

**Equivalent SQL**:
```sql
WITH active_users AS (
  SELECT id, name, department
  FROM users
  WHERE active = true
),
dept_counts AS (
  SELECT department, COUNT(id) AS user_count
  FROM active_users
  GROUP BY department
)
SELECT *
FROM dept_counts
ORDER BY user_count DESC
```

### Subqueries with IN

```json
{
  "from": "products",
  "select": ["id", "name", "category_id"],
  "where": {
    "op": "IN",
    "left": {"field": "category_id"},
    "right": {
      "subquery": {
        "from": "categories",
        "select": [{"field": "id"}],
        "where": {
          "op": "=",
          "left": {"field": "active"},
          "right": {"value": true}
        }
      }
    }
  }
}
```

**Equivalent SQL**:
```sql
SELECT id, name, category_id
FROM products
WHERE category_id IN (
  SELECT id
  FROM categories
  WHERE active = true
)
```

### EXISTS

```json
{
  "from": "customers",
  "select": ["id", "name"],
  "where": {
    "op": "EXISTS",
    "subquery": {
      "from": "orders",
      "select": [{"value": 1}],
      "where": {
        "op": "AND",
        "conditions": [
          {
            "op": "=",
            "left": {"field": "orders.customer_id"},
            "right": {"field": "customers.id"}
          },
          {
            "op": ">",
            "left": {"field": "orders.total"},
            "right": {"value": 1000}
          }
        ]
      }
    }
  }
}
```

**Equivalent SQL**:
```sql
SELECT id, name
FROM customers
WHERE EXISTS (
  SELECT 1
  FROM orders
  WHERE orders.customer_id = customers.id
    AND orders.total > 1000
)
```

### BETWEEN

The `BETWEEN` operator provides a more compact and readable way to check if a value falls within a range.

```json
{
  "from": "orders",
  "select": ["id", "customer_id", "order_date", "total"],
  "where": {
    "op": "BETWEEN",
    "expr": {"field": "order_date"},
    "low": {"value": "2025-12-18"},
    "high": {"value": "2025-12-31"}
  }
}
```

**Equivalent SQL**:
```sql
SELECT id, customer_id, order_date, total
FROM orders
WHERE order_date BETWEEN '2025-12-18' AND '2025-12-31'
```

**With parameters**:
```json
{
  "from": "orders",
  "select": ["id", "total"],
  "where": {
    "op": "BETWEEN",
    "expr": {"field": "order_date"},
    "low": {"param": "start_date"},
    "high": {"param": "end_date"}
  }
}
```

**Note**: The `BETWEEN` operator is inclusive on both ends. It is equivalent to:
```json
{
  "op": "AND",
  "conditions": [
    {"op": ">=", "left": {"field": "order_date"}, "right": {"value": "2025-12-18"}},
    {"op": "<=", "left": {"field": "order_date"}, "right": {"value": "2025-12-31"}}
  ]
}
```

The parser automatically handles type inference from the `expr` field, ensuring proper type casting for timestamp and other data types.

### Grouping and Having

```json
{
  "from": "orders",
  "select": [
    {"field": "customer_id"},
    {"func": "COUNT", "args": [{"field": "id"}], "alias": "order_count"},
    {"func": "SUM", "args": [{"field": "total"}], "alias": "total_spent"}
  ],
  "group_by": [{"field": "customer_id"}],
  "having": {
    "op": "AND",
    "conditions": [
      {
        "op": ">",
        "left": {"func": "COUNT", "args": [{"field": "id"}]},
        "right": {"value": 5}
      },
      {
        "op": ">",
        "left": {"func": "SUM", "args": [{"field": "total"}]},
        "right": {"value": 1000}
      }
    ]
  }
}
```

**Equivalent SQL**:
```sql
SELECT 
  customer_id,
  COUNT(id) AS order_count,
  SUM(total) AS total_spent
FROM orders
GROUP BY customer_id
HAVING COUNT(id) > 5 AND SUM(total) > 1000
```

### Window Functions

```json
{
  "from": "employees",
  "select": [
    {"field": "id"},
    {"field": "name"},
    {"field": "department"},
    {"field": "salary"},
    {
      "window_func": {
        "func": "ROW_NUMBER",
        "partition_by": [{"field": "department"}],
        "order_by": [{"field": "salary", "direction": "desc"}]
      },
      "alias": "salary_rank"
    },
    {
      "window_func": {
        "func": "AVG",
        "args": [{"field": "salary"}],
        "partition_by": [{"field": "department"}]
      },
      "alias": "dept_avg_salary"
    }
  ]
}
```

**Equivalent SQL**:
```sql
SELECT 
  id,
  name,
  department,
  salary,
  ROW_NUMBER() OVER (PARTITION BY department ORDER BY salary DESC) AS salary_rank,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg_salary
FROM employees
```

**Supported window functions**: `ROW_NUMBER`, `RANK`, `DENSE_RANK`, `LAG`, `LEAD`, `FIRST_VALUE`, `LAST_VALUE`, and all aggregation functions (`SUM`, `AVG`, `COUNT`, `MIN`, `MAX`) can be used as window functions

**Window function features**:
- `PARTITION BY` - to partition data into groups
- `ORDER BY` - to define ordering within partitions
- Both can be combined in the same window function

### Query Parameters

Parameters can be passed separately and referenced in queries:

```json
{
  "from": "users",
  "select": ["id", "name", "email"],
  "where": {
    "op": "AND",
    "conditions": [
      {
        "op": "=",
        "left": {"field": "department"},
        "right": {"param": "dept_id"}
      },
      {
        "op": ">=",
        "left": {"field": "hire_date"},
        "right": {"param": "start_date"}
      }
    ]
  }
}
```

**With parameters**:
```python
jsql = {...}  # as above
params = {
    "dept_id": 5,
    "start_date": "2020-01-01"
}
result = await uma_select(jsql, params)
```

## Result Format

The result of `uma_select` is a dictionary with two keys: `meta` and `data`.

### Result Structure

```python
{
    "meta": [
        {
            "name": str,              # Column name (or alias)
            "type": str,              # SQL type (e.g., "INTEGER", "VARCHAR(255)")
            "nullable": bool,         # Whether NULL values are allowed
            "qualified_name": str | None,  # Fully qualified field name (e.g., "users.id")
            "source_entity": str | None,   # Source entity/table name
            "source_field": str | None,    # Source field name (if not computed)
            "size": int | None,       # Size for string/binary types
            "precision": int | None,  # Precision for numeric types
            "scale": int | None       # Scale for numeric types
        }
    ],
    "data": [
        [value1, value2, ...],  # First row
        [value1, value2, ...],  # Second row
        ...
    ]
}
```

### Result Example 1: Simple Query

**Query**:
```json
{
  "from": "users",
  "select": ["id", "name", "email", "active"]
}
```

**Result**:
```json
{
  "meta": [
    {
      "name": "id",
      "type": "INTEGER",
      "nullable": false,
      "qualified_name": "users.id",
      "source_entity": "users",
      "source_field": "id",
      "size": null,
      "precision": null,
      "scale": null
    },
    {
      "name": "name",
      "type": "VARCHAR",
      "nullable": false,
      "qualified_name": "users.name",
      "source_entity": "users",
      "source_field": "name",
      "size": 255,
      "precision": null,
      "scale": null
    },
    {
      "name": "email",
      "type": "VARCHAR",
      "nullable": true,
      "qualified_name": "users.email",
      "source_entity": "users",
      "source_field": "email",
      "size": 255,
      "precision": null,
      "scale": null
    },
    {
      "name": "active",
      "type": "BOOLEAN",
      "nullable": false,
      "qualified_name": "users.active",
      "source_entity": "users",
      "source_field": "active",
      "size": null,
      "precision": null,
      "scale": null
    }
  ],
  "data": [
    [1, "John Doe", "john@example.com", true],
    [2, "Jane Smith", "jane@example.com", true],
    [3, "Bob Johnson", null, false]
  ]
}
```

### Result Example 2: Aggregation with Aliases

**Query**:
```json
{
  "from": "orders",
  "select": [
    {"field": "customer_id", "alias": "customer"},
    {"func": "COUNT", "args": [{"field": "id"}], "alias": "order_count"},
    {"func": "SUM", "args": [{"field": "amount"}], "alias": "total_amount"},
    {"func": "AVG", "args": [{"field": "amount"}], "alias": "avg_amount"}
  ],
  "group_by": [{"field": "customer_id"}],
  "order_by": [{"field": "total_amount", "direction": "desc"}]
}
```

**Result**:
```json
{
  "meta": [
    {
      "name": "customer",
      "type": "INTEGER",
      "nullable": false,
      "qualified_name": "orders.customer_id",
      "source_entity": "orders",
      "source_field": "customer_id",
      "size": null,
      "precision": null,
      "scale": null
    },
    {
      "name": "order_count",
      "type": "BIGINT",
      "nullable": false,
      "qualified_name": null,
      "source_entity": "orders",
      "source_field": "id",
      "size": null,
      "precision": null,
      "scale": null
    },
    {
      "name": "total_amount",
      "type": "NUMERIC",
      "nullable": true,
      "qualified_name": null,
      "source_entity": "orders",
      "source_field": "amount",
      "size": null,
      "precision": 10,
      "scale": 2
    },
    {
      "name": "avg_amount",
      "type": "NUMERIC",
      "nullable": true,
      "qualified_name": null,
      "source_entity": "orders",
      "source_field": "amount",
      "size": null,
      "precision": 10,
      "scale": 2
    }
  ],
  "data": [
    [5, 12, 15600.50, 1300.04],
    [3, 8, 9200.00, 1150.00],
    [7, 5, 3750.25, 750.05]
  ]
}
```

### Result Example 3: Join with Expression

**Query**:
```json
{
  "from": "orders",
  "joins": [
    {
      "type": "LEFT",
      "table": "customers",
      "alias": "c",
      "on": {
        "op": "=",
        "left": {"field": "orders.customer_id"},
        "right": {"field": "c.id"}
      }
    }
  ],
  "select": [
    {"field": "orders.id"},
    {"field": "c.name", "alias": "customer_name"},
    {"field": "orders.quantity"},
    {"field": "orders.price"},
    {
      "expr": {
        "op": "*",
        "left": {"field": "orders.quantity"},
        "right": {"field": "orders.price"}
      },
      "alias": "line_total"
    }
  ]
}
```

**Result**:
```json
{
  "meta": [
    {
      "name": "id",
      "type": "INTEGER",
      "nullable": false,
      "qualified_name": "orders.id",
      "source_entity": "orders",
      "source_field": "id",
      "size": null,
      "precision": null,
      "scale": null
    },
    {
      "name": "customer_name",
      "type": "VARCHAR",
      "nullable": true,
      "qualified_name": "customers.name",
      "source_entity": "customers",
      "source_field": "name",
      "size": 255,
      "precision": null,
      "scale": null
    },
    {
      "name": "quantity",
      "type": "INTEGER",
      "nullable": false,
      "qualified_name": "orders.quantity",
      "source_entity": "orders",
      "source_field": "quantity",
      "size": null,
      "precision": null,
      "scale": null
    },
    {
      "name": "price",
      "type": "NUMERIC",
      "nullable": false,
      "qualified_name": "orders.price",
      "source_entity": "orders",
      "source_field": "price",
      "size": null,
      "precision": 10,
      "scale": 2
    },
    {
      "name": "line_total",
      "type": "NUMERIC",
      "nullable": true,
      "qualified_name": null,
      "source_entity": null,
      "source_field": null,
      "size": null,
      "precision": 20,
      "scale": 2
    }
  ],
  "data": [
    [1, "John Doe", 3, 25.50, 76.50],
    [2, "Jane Smith", 1, 199.99, 199.99],
    [3, null, 5, 10.00, 50.00]
  ]
}
```

### Result Example 4: Array Aggregation

**Query**:
```json
{
  "from": "users",
  "select": [
    {"field": "department"},
    {"func": "ARRAY_AGG", "args": [{"field": "name"}], "alias": "employees"}
  ],
  "group_by": [{"field": "department"}]
}
```

**Result**:
```json
{
  "meta": [
    {
      "name": "department",
      "type": "VARCHAR",
      "nullable": true,
      "qualified_name": "users.department",
      "source_entity": "users",
      "source_field": "department",
      "size": 100,
      "precision": null,
      "scale": null
    },
    {
      "name": "employees",
      "type": "ARRAY",
      "nullable": true,
      "qualified_name": null,
      "source_entity": "users",
      "source_field": "name",
      "size": null,
      "precision": null,
      "scale": null
    }
  ],
  "data": [
    ["Engineering", ["John Doe", "Jane Smith", "Bob Wilson"]],
    ["Sales", ["Alice Brown", "Charlie Davis"]],
    ["HR", ["Diana Evans"]]
  ]
}
```

## Implementation Notes

1. **Type Mapping**: SQLAlchemy types are mapped to standard SQL type names
2. **Qualified Names**: When possible, include table.column format
3. **Computed Columns**: For expressions and aggregations, `qualified_name` may be null
4. **Array Aggregation**: Uses SQLAlchemy's `array_agg` function
5. **Window Functions**: Fully supported through SQLAlchemy window constructs
6. **Access Control**: All queries respect entity-level access control via `can()` method
7. **Parameter Binding**: Parameters are bound using SQLAlchemy's parameter binding for security

## Error Handling

JSQL parsing and execution can raise:
- `ValueError`: Invalid JSQL structure
- `UMAAccessDeniedError`: Access denied to entity
- `UMANotFoundError`: Referenced entity not found
- `JSQLSyntaxError`: Invalid JSQL syntax (custom exception)
- `JSQLExecutionError`: Query execution error (custom exception)
