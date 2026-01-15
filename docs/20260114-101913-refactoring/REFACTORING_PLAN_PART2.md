# План рефакторинга - Детализация проблемы 3

## Проблема 3: Разбиение гигантского `converter.py` (1033 строки)

### Анализ текущей структуры

**Текущий файл содержит:**
- JSQL → SQL конверсию (строки 74-192)
- Вспомогательные функции для JSQL → SQL (строки 194-582)
- SQL → JSQL конверсию (строки 584-720)
- Вспомогательные функции для SQL → JSQL (строки 723-1033)
- Маппинги операторов (строки 28-72)

### Новая структура модулей

```
jsql/converter/
├── __init__.py                 # Public API, re-exports
├── types.py                    # Shared types
├── constants.py                # Operator mappings (перенести из jsql/constants.py)
├── jsql_to_sql.py             # Main function jsql_to_sql()
├── sql_to_jsql.py             # Main function sql_to_jsql()
├── expressions.py             # Expression conversion (both directions)
├── conditions.py              # Condition conversion (both directions)
├── operators.py               # Operator handlers (both directions)
└── joins.py                   # JOIN conversion (both directions)
```

---

## Детальная разбивка файлов

### 1. `jsql/converter/__init__.py`

```python
"""
JSQL ↔ SQL conversion utilities.

Public API for bidirectional conversion between JSQL and SQL formats.
"""

from namerec.uma.jsql.converter.jsql_to_sql import jsql_to_sql
from namerec.uma.jsql.converter.sql_to_jsql import sql_to_jsql

__all__ = ['jsql_to_sql', 'sql_to_jsql']
```

---

### 2. `jsql/converter/constants.py`

Перенести все маппинги из `jsql/constants.py` и добавить новые:

```python
"""Operator and type mappings for JSQL ↔ SQL conversion."""

from enum import Enum
import sqlglot.expressions as exp
from namerec.uma.jsql.constants import JSQLOperator

# JSQL → SQLGlot mappings
ARITHMETIC_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.ADD.value: exp.Add,
    JSQLOperator.SUB.value: exp.Sub,
    JSQLOperator.MUL.value: exp.Mul,
    JSQLOperator.DIV.value: exp.Div,
}

COMPARISON_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.EQ.value: exp.EQ,
    JSQLOperator.NE.value: exp.NEQ,
    JSQLOperator.GT.value: exp.GT,
    JSQLOperator.GE.value: exp.GTE,
    JSQLOperator.LT.value: exp.LT,
    JSQLOperator.LE.value: exp.LTE,
}

LOGICAL_OP_TO_SQLGLOT: dict[str, type[exp.Expression]] = {
    JSQLOperator.AND.value: exp.And,
    JSQLOperator.OR.value: exp.Or,
}

# SQLGlot → JSQL mappings (reverse)
SQLGLOT_TO_ARITHMETIC: dict[type[exp.Expression], str] = {
    v: k for k, v in ARITHMETIC_OP_TO_SQLGLOT.items()
}

SQLGLOT_TO_COMPARISON: dict[type[exp.Expression], str] = {
    v: k for k, v in COMPARISON_OP_TO_SQLGLOT.items()
}

SQLGLOT_TO_LOGICAL: dict[type[exp.Expression], str] = {
    v: k for k, v in LOGICAL_OP_TO_SQLGLOT.items()
}

# Join type mappings
SQLGLOT_JOIN_SIDE_TO_TYPE: dict[str, str] = {
    'LEFT': 'LEFT',
    'RIGHT': 'RIGHT',
    'FULL': 'FULL',
}
```

---

### 3. `jsql/converter/expressions.py`

```python
"""Expression conversion utilities for JSQL ↔ SQL."""

import logging
from typing import Any
import sqlglot.expressions as exp

from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError, UnknownOperatorError
from namerec.uma.jsql.converter.constants import (
    ARITHMETIC_OP_TO_SQLGLOT,
    SQLGLOT_TO_ARITHMETIC,
)

logger = logging.getLogger(__name__)


# ===== JSQL → SQL =====

def jsql_expression_to_sqlglot(expr_spec: dict[str, Any] | str) -> exp.Expression:
    """
    Convert JSQL expression to sqlglot expression.
    
    Handles:
    - Field references: {"field": "name"} or "name"
    - Literals: {"value": 123}
    - Functions: {"func": "COUNT", "args": [...]}
    - Arithmetic: {"op": "+", "left": {...}, "right": {...}}
    """
    if isinstance(expr_spec, str):
        return convert_field_reference(expr_spec)
    
    if isinstance(expr_spec, dict):
        if 'field' in expr_spec:
            return convert_field_reference(expr_spec['field'])
        if 'value' in expr_spec:
            return convert_literal_value(expr_spec['value'])
        if 'func' in expr_spec:
            return convert_function_call(expr_spec)
        if 'op' in expr_spec:
            return convert_arithmetic_op(expr_spec)
        
        raise InvalidExpressionError(
            message="Expression must contain: 'field', 'value', 'func', or 'op'",
            path='',
            expression=expr_spec
        )
    
    raise InvalidExpressionError(
        message=f'Expression must be string or dict, got {type(expr_spec).__name__}',
        path='',
        expression={'type': type(expr_spec).__name__}
    )


def convert_field_reference(field: str) -> exp.Expression:
    """Convert field reference to sqlglot column."""
    if field == '*':
        return exp.Star()
    if '.' in field:
        table, column = field.split('.', 1)
        return exp.column(column, table=table)
    return exp.column(field)


def convert_literal_value(value: Any) -> exp.Expression:
    """Convert literal value to sqlglot expression."""
    match value:
        case bool():
            return exp.Boolean(this=value)
        case int() | float():
            return exp.Literal.number(value)
        case _:
            return exp.Literal.string(str(value))


def convert_function_call(expr_spec: dict[str, Any]) -> exp.Expression:
    """Convert function call to sqlglot expression."""
    func = expr_spec['func']
    logger.debug(f'Converting function: {func}')
    
    # Convert args
    args = [
        converted
        for arg in expr_spec.get('args', [])
        if (converted := jsql_expression_to_sqlglot(arg)) is not None
    ]
    
    # Try specific sqlglot function class
    func_class = getattr(exp, func.upper(), None)
    if func_class and issubclass(func_class, exp.Func):
        return func_class(*args)
    
    # Fallback to Anonymous function
    return exp.Anonymous(this=func, expressions=args)


def convert_arithmetic_op(expr_spec: dict[str, Any]) -> exp.Expression:
    """Convert arithmetic operation to sqlglot expression."""
    from namerec.uma.jsql.conversion_exceptions import MissingFieldError
    
    op = expr_spec.get('op')
    if not op:
        raise MissingFieldError('op', path='op', context='arithmetic operation')
    
    if 'left' not in expr_spec:
        raise MissingFieldError('left', path='left', context='arithmetic operation')
    if 'right' not in expr_spec:
        raise MissingFieldError('right', path='right', context='arithmetic operation')
    
    left = jsql_expression_to_sqlglot(expr_spec['left'])
    right = jsql_expression_to_sqlglot(expr_spec['right'])
    
    operator_class = ARITHMETIC_OP_TO_SQLGLOT.get(op)
    if not operator_class:
        raise UnknownOperatorError(
            operator=op,
            path='op',
            supported=list(ARITHMETIC_OP_TO_SQLGLOT.keys())
        )
    
    return operator_class(this=left, expression=right)


# ===== SQL → JSQL =====

def sqlglot_expression_to_jsql(expr: exp.Expression) -> dict[str, Any]:
    """
    Convert sqlglot expression to JSQL.
    
    Raises:
        InvalidExpressionError: If expression type not supported
        UnknownOperatorError: If operator not recognized
    """
    from namerec.uma.jsql.conversion_exceptions import UnsupportedOperationError
    
    # Aliased expressions
    if isinstance(expr, exp.Alias):
        base_expr = sqlglot_expression_to_jsql(expr.this)
        base_expr['alias'] = expr.alias
        return base_expr
    
    # Column references
    if isinstance(expr, exp.Column):
        result = {'field': expr.name}
        if expr.table:
            result['field'] = f'{expr.table}.{expr.name}'
        return result
    
    # Literals
    if isinstance(expr, exp.Literal):
        return {'value': expr.this}
    
    # Star
    if isinstance(expr, exp.Star):
        return {'field': '*'}
    
    # Functions
    if isinstance(expr, exp.Func):
        func_name = expr.sql_name()
        args = [
            converted
            for arg in expr.args.values()
            if isinstance(arg, exp.Expression) and (converted := sqlglot_expression_to_jsql(arg)) is not None
        ]
        return {'func': func_name, 'args': args}
    
    # Arithmetic operations
    if isinstance(expr, (exp.Add, exp.Sub, exp.Mul, exp.Div)):
        op = SQLGLOT_TO_ARITHMETIC.get(type(expr))
        if not op:
            raise UnknownOperatorError(
                operator=type(expr).__name__,
                path='',
                supported=list(SQLGLOT_TO_ARITHMETIC.values())
            )
        
        return {
            'op': op,
            'left': sqlglot_expression_to_jsql(expr.left),
            'right': sqlglot_expression_to_jsql(expr.right),
        }
    
    # Unsupported
    raise UnsupportedOperationError(
        operation=f'Expression type: {type(expr).__name__}',
        reason='This expression type is not supported',
        path=''
    )
```

---

### 4. `jsql/converter/conditions.py`

```python
"""Condition conversion utilities for JSQL ↔ SQL."""

import logging
from typing import Any
import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JSQLOperator, LOGICAL_OPERATORS
from namerec.uma.jsql.conversion_exceptions import (
    InvalidExpressionError,
    MissingFieldError,
    UnknownOperatorError,
)
from namerec.uma.jsql.converter.constants import (
    COMPARISON_OP_TO_SQLGLOT,
    LOGICAL_OP_TO_SQLGLOT,
    SQLGLOT_TO_COMPARISON,
)
from namerec.uma.jsql.converter.expressions import (
    jsql_expression_to_sqlglot,
    sqlglot_expression_to_jsql,
)

logger = logging.getLogger(__name__)


# ===== JSQL → SQL =====

def jsql_condition_to_sqlglot(cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert JSQL condition to sqlglot expression.
    
    Handles:
    - Logical: AND, OR, NOT
    - Comparison: =, !=, <, >, <=, >=
    - Special: IN, LIKE, BETWEEN, IS NULL, IS NOT NULL
    """
    if not isinstance(cond_spec, dict):
        raise InvalidExpressionError(
            message=f'Condition must be dict, got {type(cond_spec).__name__}',
            path='',
            expression={'type': type(cond_spec).__name__}
        )
    
    if 'op' not in cond_spec:
        raise MissingFieldError('op', path='op', context='condition')
    
    op = cond_spec['op']
    
    # Logical operators
    if op in LOGICAL_OPERATORS:
        return convert_logical_operator(op, cond_spec)
    
    # Comparison operators
    return convert_comparison_operator(op, cond_spec)


def convert_logical_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """Convert logical operator (AND/OR/NOT)."""
    logger.debug(f'Converting logical operator: {op}')
    
    # AND/OR
    if op in (JSQLOperator.AND.value, JSQLOperator.OR.value):
        if 'conditions' not in cond_spec:
            raise MissingFieldError('conditions', path='conditions', context=f'{op} operator')
        
        conditions = cond_spec['conditions']
        if not conditions:
            raise InvalidExpressionError(
                message=f'{op} operator requires at least one condition',
                path='conditions'
            )
        
        result = jsql_condition_to_sqlglot(conditions[0])
        operator_class = LOGICAL_OP_TO_SQLGLOT[op]
        
        for cond in conditions[1:]:
            next_cond = jsql_condition_to_sqlglot(cond)
            result = operator_class(this=result, expression=next_cond)
        
        return result
    
    # NOT
    if op == JSQLOperator.NOT.value:
        if 'condition' not in cond_spec:
            raise MissingFieldError('condition', path='condition', context='NOT operator')
        
        condition = jsql_condition_to_sqlglot(cond_spec['condition'])
        return exp.Not(this=condition)
    
    raise UnknownOperatorError(
        operator=op,
        path='op',
        supported=[JSQLOperator.AND.value, JSQLOperator.OR.value, JSQLOperator.NOT.value]
    )


def convert_comparison_operator(op: str, cond_spec: dict[str, Any]) -> exp.Expression:
    """
    Convert comparison operator.
    
    Supports old and new JSQL formats:
    - Old: {"op": "=", "field": "x", "value": 1}
    - New: {"op": "=", "left": {"field": "x"}, "right": {"value": 1}}
    """
    logger.debug(f'Converting comparison operator: {op}')
    
    # BETWEEN has special structure
    if op == JSQLOperator.BETWEEN.value:
        if 'expr' not in cond_spec:
            raise MissingFieldError('expr', path='expr', context='BETWEEN')
        if 'low' not in cond_spec:
            raise MissingFieldError('low', path='low', context='BETWEEN')
        if 'high' not in cond_spec:
            raise MissingFieldError('high', path='high', context='BETWEEN')
        
        expr = jsql_expression_to_sqlglot(cond_spec['expr'])
        low_expr = jsql_expression_to_sqlglot(cond_spec['low'])
        high_expr = jsql_expression_to_sqlglot(cond_spec['high'])
        
        return exp.Between(this=expr, low=low_expr, high=high_expr)
    
    # Get left/right expressions (support both formats)
    if 'left' in cond_spec and 'right' in cond_spec:
        # New format
        left_expr = jsql_expression_to_sqlglot(cond_spec['left'])
        right_expr = jsql_expression_to_sqlglot(cond_spec['right'])
    elif 'field' in cond_spec:
        # Old format
        left_expr = jsql_expression_to_sqlglot(cond_spec['field'])
        
        if 'value' in cond_spec:
            right_expr = jsql_expression_to_sqlglot({'value': cond_spec['value']})
        elif 'right_field' in cond_spec:
            right_expr = jsql_expression_to_sqlglot(cond_spec['right_field'])
        else:
            raise MissingFieldError(
                'value or right_field',
                path='',
                context='comparison requires right operand'
            )
    else:
        raise MissingFieldError(
            'left/right or field',
            path='',
            context='comparison requires operands'
        )
    
    # Standard comparison operators
    if operator_class := COMPARISON_OP_TO_SQLGLOT.get(op):
        return operator_class(this=left_expr, expression=right_expr)
    
    # Special operators
    if op == JSQLOperator.IN.value:
        if 'values' not in cond_spec:
            raise MissingFieldError('values', path='values', context='IN operator')
        val_exprs = [jsql_expression_to_sqlglot({'value': v}) for v in cond_spec['values']]
        return exp.In(this=left_expr, expressions=val_exprs)
    
    if op == JSQLOperator.LIKE.value:
        if 'pattern' not in cond_spec:
            raise MissingFieldError('pattern', path='pattern', context='LIKE operator')
        return exp.Like(this=left_expr, expression=exp.Literal.string(cond_spec['pattern']))
    
    if op == JSQLOperator.IS_NULL.value:
        return exp.Is(this=left_expr, expression=exp.Null())
    
    if op == JSQLOperator.IS_NOT_NULL.value:
        return exp.Not(this=exp.Is(this=left_expr, expression=exp.Null()))
    
    # Unknown operator
    supported = list(COMPARISON_OP_TO_SQLGLOT.keys()) + [
        JSQLOperator.IN.value,
        JSQLOperator.BETWEEN.value,
        JSQLOperator.LIKE.value,
        JSQLOperator.IS_NULL.value,
        JSQLOperator.IS_NOT_NULL.value,
    ]
    raise UnknownOperatorError(operator=op, path='op', supported=supported)


# ===== SQL → JSQL =====

def sqlglot_condition_to_jsql(expr: exp.Expression) -> dict[str, Any]:
    """
    Convert sqlglot condition to JSQL.
    
    Handles all comparison and logical operators.
    """
    from namerec.uma.jsql.conversion_exceptions import UnsupportedOperationError
    
    # AND/OR
    if isinstance(expr, exp.And):
        return {
            'op': JSQLOperator.AND.value,
            'conditions': [
                sqlglot_condition_to_jsql(expr.left),
                sqlglot_condition_to_jsql(expr.right),
            ],
        }
    
    if isinstance(expr, exp.Or):
        return {
            'op': JSQLOperator.OR.value,
            'conditions': [
                sqlglot_condition_to_jsql(expr.left),
                sqlglot_condition_to_jsql(expr.right),
            ],
        }
    
    # NOT
    if isinstance(expr, exp.Not):
        return {
            'op': JSQLOperator.NOT.value,
            'condition': sqlglot_condition_to_jsql(expr.this),
        }
    
    # Comparison operators
    if isinstance(expr, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE)):
        op = SQLGLOT_TO_COMPARISON[type(expr)]
        left = sqlglot_expression_to_jsql(expr.left)
        right = sqlglot_expression_to_jsql(expr.right)
        
        if 'field' not in left:
            raise InvalidExpressionError(
                message='Left side must be field',
                path='left',
                expression=left
            )
        
        result = {'field': left['field'], 'op': op}
        
        if 'value' in right:
            result['value'] = right['value']
        elif 'field' in right:
            result['right_field'] = right['field']
        else:
            raise InvalidExpressionError(
                message='Right side must be value or field',
                path='right',
                expression=right
            )
        
        return result
    
    # IN
    if isinstance(expr, exp.In):
        left = sqlglot_expression_to_jsql(expr.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message='IN left side must be field',
                path='left',
                expression=left
            )
        
        values = []
        for val_expr in expr.expressions:
            val = sqlglot_expression_to_jsql(val_expr)
            if 'value' not in val:
                raise InvalidExpressionError(
                    message='IN values must be literals',
                    path='values',
                    expression=val
                )
            values.append(val['value'])
        
        return {
            'field': left['field'],
            'op': JSQLOperator.IN.value,
            'values': values,
        }
    
    # LIKE
    if isinstance(expr, exp.Like):
        left = sqlglot_expression_to_jsql(expr.this)
        right = sqlglot_expression_to_jsql(expr.expression)
        
        if 'field' not in left or 'value' not in right:
            raise InvalidExpressionError(
                message='LIKE requires field and value',
                path='',
                expression={'left': left, 'right': right}
            )
        
        return {
            'field': left['field'],
            'op': JSQLOperator.LIKE.value,
            'pattern': right['value'],
        }
    
    # IS NULL
    if isinstance(expr, exp.Is) and isinstance(expr.expression, exp.Null):
        left = sqlglot_expression_to_jsql(expr.this)
        if 'field' not in left:
            raise InvalidExpressionError(
                message='IS NULL requires field',
                path='left',
                expression=left
            )
        
        return {
            'field': left['field'],
            'op': JSQLOperator.IS_NULL.value,
        }
    
    # Unsupported
    raise UnsupportedOperationError(
        operation=f'Condition type: {type(expr).__name__}',
        reason='Condition type not supported',
        path=''
    )
```

---

### 5. `jsql/converter/joins.py`

```python
"""JOIN conversion utilities for JSQL ↔ SQL."""

import logging
from typing import Any
import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JoinType
from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError, MissingFieldError
from namerec.uma.jsql.converter.conditions import jsql_condition_to_sqlglot, sqlglot_condition_to_jsql
from namerec.uma.jsql.converter.constants import SQLGLOT_JOIN_SIDE_TO_TYPE

logger = logging.getLogger(__name__)


# ===== JSQL → SQL =====

def jsql_join_to_sqlglot(join_spec: dict[str, Any]) -> dict[str, Any]:
    """
    Convert JSQL JOIN to sqlglot join components.
    
    Returns dict with:
    - table: sqlglot Table expression
    - on: sqlglot condition (optional)
    - type: join type string
    """
    if not isinstance(join_spec, dict):
        raise InvalidExpressionError(
            message=f'JOIN must be dict, got {type(join_spec).__name__}',
            path='joins',
            expression={'type': type(join_spec).__name__}
        )
    
    if 'entity' not in join_spec:
        raise MissingFieldError('entity', path='joins.entity', context='JOIN')
    
    entity = join_spec['entity']
    join_type = join_spec.get('type', JoinType.INNER.value).upper()
    logger.debug(f'Converting {join_type} JOIN to {entity}')
    
    # Build table expression
    table = exp.table_(entity)
    if alias := join_spec.get('alias'):
        table = exp.alias_(table, alias, table=True)
    
    # Build ON condition
    on_cond = None
    if on_spec := join_spec.get('on'):
        on_cond = jsql_condition_to_sqlglot(on_spec)
    
    return {
        'table': table,
        'on': on_cond,
        'type': join_type,
    }


# ===== SQL → JSQL =====

def sqlglot_join_to_jsql(join: exp.Join) -> dict[str, Any]:
    """
    Convert sqlglot JOIN to JSQL.
    
    Returns JSQL join specification.
    """
    if not isinstance(join, exp.Join):
        raise InvalidExpressionError(
            message=f'Expected exp.Join, got {type(join).__name__}',
            path='joins',
            expression={'type': type(join).__name__}
        )
    
    # Get join type
    join_type = JoinType.INNER.value
    if join.side:
        join_type = SQLGLOT_JOIN_SIDE_TO_TYPE.get(join.side, JoinType.INNER.value)
    
    # Get joined table
    table = join.this
    if not isinstance(table, exp.Table):
        raise InvalidExpressionError(
            message=f'JOIN table must be Table, got {type(table).__name__}',
            path='joins.this',
            expression={'type': type(table).__name__}
        )
    
    result: dict[str, Any] = {
        'type': join_type,
        'entity': table.name,
    }
    
    if table.alias:
        result['alias'] = table.alias
    
    # Get ON condition
    if join.args.get('on'):
        on_arg = join.args['on']
        
        # Determine actual condition expression
        if isinstance(on_arg, (exp.EQ, exp.NEQ, exp.GT, exp.GTE, exp.LT, exp.LTE, 
                              exp.And, exp.Or, exp.Not, exp.In, exp.Like, exp.Is)):
            on_expr = on_arg
        elif hasattr(on_arg, 'this') and on_arg.this is not None:
            on_expr = on_arg.this
        else:
            on_expr = on_arg
        
        on_jsql = sqlglot_condition_to_jsql(on_expr)
        result['on'] = on_jsql
    
    return result
```

---

### 6. `jsql/converter/jsql_to_sql.py`

```python
"""Main function for JSQL → SQL conversion."""

import logging
import sqlparse
import sqlglot.expressions as exp

from namerec.uma.jsql.constants import JoinType, OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.converter.expressions import jsql_expression_to_sqlglot
from namerec.uma.jsql.converter.conditions import jsql_condition_to_sqlglot
from namerec.uma.jsql.converter.joins import jsql_join_to_sqlglot

logger = logging.getLogger(__name__)


def jsql_to_sql(jsql: dict, dialect: str = 'generic') -> str:
    """
    Convert JSQL query to SQL string.
    
    Args:
        jsql: JSQL query dictionary
        dialect: SQL dialect (generic, postgresql, mysql, sqlite, etc.)
    
    Returns:
        Formatted SQL string
    
    Raises:
        JSQLSyntaxError: If JSQL syntax is invalid
    """
    logger.info(f'Converting JSQL to SQL, dialect={dialect}')
    logger.debug(f'Input JSQL: {jsql}')
    
    try:
        # Build SELECT expressions
        select_exprs = _build_select(jsql.get('select', []))
        
        # Build FROM expression
        from_expr = _build_from(jsql.get('from'))
        
        # Start building query
        query = exp.Select(expressions=select_exprs)
        query = query.from_(from_expr)
        
        # Add WHERE
        if where_spec := jsql.get('where'):
            where_expr = jsql_condition_to_sqlglot(where_spec)
            query = query.where(where_expr)
        
        # Add JOINs
        if joins := jsql.get('joins'):
            for join_spec in joins:
                join_expr = jsql_join_to_sqlglot(join_spec)
                query = query.join(
                    join_expr['table'],
                    on=join_expr.get('on'),
                    join_type=join_expr.get('type', JoinType.INNER.value),
                )
        
        # Add GROUP BY
        if group_by := jsql.get('group_by'):
            for group_expr_spec in group_by:
                group_expr = jsql_expression_to_sqlglot(group_expr_spec)
                if group_expr:
                    query = query.group_by(group_expr)
        
        # Add HAVING
        if having_spec := jsql.get('having'):
            having_expr = jsql_condition_to_sqlglot(having_spec)
            query.args['having'] = exp.Having(this=having_expr)
        
        # Add ORDER BY
        if order_by := jsql.get('order_by'):
            for order_spec in order_by:
                if isinstance(order_spec, dict):
                    order_expr = jsql_expression_to_sqlglot(order_spec.get('field') or order_spec)
                    direction = order_spec.get('direction', OrderDirection.ASC.value).upper()
                else:
                    order_expr = jsql_expression_to_sqlglot(order_spec)
                    direction = OrderDirection.ASC.value.upper()
                
                if order_expr:
                    desc = direction == OrderDirection.DESC.value.upper()
                    query = query.order_by(order_expr, desc=desc)
        
        # Add LIMIT/OFFSET
        if limit := jsql.get('limit'):
            query.args['limit'] = exp.Limit(expression=exp.Literal.number(limit))
        
        if offset := jsql.get('offset'):
            query.args['offset'] = exp.Offset(expression=exp.Literal.number(offset))
        
        # Generate SQL
        sql_str = query.sql(dialect=dialect if dialect != 'generic' else None, pretty=True)
        
        # Format
        formatted_sql = sqlparse.format(
            sql_str,
            reindent=True,
            keyword_case='upper',
        )
        
        logger.info(f'Successfully converted JSQL to SQL ({dialect})')
        return formatted_sql
    
    except JSQLSyntaxError:
        raise
    except Exception as e:
        logger.error(f'Conversion error: {e}', exc_info=True)
        raise JSQLSyntaxError(
            message=f'Failed to convert JSQL to SQL: {e!s}',
            path='',
        ) from e


def _build_select(select_spec: list | None) -> list[exp.Expression]:
    """Build SELECT clause expressions."""
    if not select_spec:
        return [exp.Star()]
    
    result = []
    for field_spec in select_spec:
        field_expr = jsql_expression_to_sqlglot(field_spec)
        if field_expr:
            alias = field_spec.get('alias') if isinstance(field_spec, dict) else None
            if alias:
                field_expr = exp.alias_(field_expr, alias)
            result.append(field_expr)
    
    return result if result else [exp.Star()]


def _build_from(from_spec: str | dict | None) -> exp.Table:
    """Build FROM clause expression."""
    from namerec.uma.jsql.conversion_exceptions import MissingFieldError, InvalidExpressionError
    
    if not from_spec:
        raise MissingFieldError('from', path='from', context='SELECT requires FROM')
    
    if isinstance(from_spec, str):
        return exp.table_(from_spec)
    
    if isinstance(from_spec, dict):
        if 'entity' not in from_spec:
            raise MissingFieldError('entity', path='from.entity', context='FROM with alias')
        
        entity = from_spec['entity']
        alias = from_spec.get('alias')
        table = exp.table_(entity)
        if alias:
            table = exp.alias_(table, alias, table=True)
        return table
    
    raise InvalidExpressionError(
        message=f'FROM must be string or dict, got {type(from_spec).__name__}',
        path='from',
        expression={'type': type(from_spec).__name__}
    )
```

---

### 7. `jsql/converter/sql_to_jsql.py`

```python
"""Main function for SQL → JSQL conversion."""

import logging
import sqlglot
import sqlglot.expressions as exp

from namerec.uma.jsql.constants import OrderDirection
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.jsql.converter.expressions import sqlglot_expression_to_jsql
from namerec.uma.jsql.converter.conditions import sqlglot_condition_to_jsql
from namerec.uma.jsql.converter.joins import sqlglot_join_to_jsql

logger = logging.getLogger(__name__)


def sql_to_jsql(sql: str, dialect: str = 'generic') -> dict:
    """
    Convert SQL query to JSQL dictionary.
    
    Args:
        sql: SQL query string
        dialect: SQL dialect (generic, postgresql, mysql, sqlite, etc.)
    
    Returns:
        JSQL query dictionary
    
    Raises:
        JSQLSyntaxError: If SQL cannot be parsed
    """
    logger.info(f'Converting SQL to JSQL, dialect={dialect}')
    logger.debug(f'Input SQL: {sql}')
    
    try:
        # Parse SQL
        read_dialect = None if dialect == 'generic' else dialect
        parsed = sqlglot.parse_one(sql, read=read_dialect)
        
        if not isinstance(parsed, exp.Select):
            raise JSQLSyntaxError(
                message='Only SELECT queries are supported',
                path='',
            )
        
        # Convert to JSQL
        jsql: dict = {}
        
        # FROM clause
        if from_expr := parsed.args.get('from_'):
            jsql['from'] = _convert_from(from_expr)
        
        # SELECT clause
        if parsed.expressions:
            jsql['select'] = [sqlglot_expression_to_jsql(expr) for expr in parsed.expressions]
        
        # WHERE clause
        if parsed.args.get('where'):
            where_expr = parsed.args['where'].this
            jsql['where'] = sqlglot_condition_to_jsql(where_expr)
        
        # JOIN clauses
        if parsed.args.get('joins'):
            jsql['joins'] = [sqlglot_join_to_jsql(join) for join in parsed.args['joins']]
        
        # ORDER BY clause
        if parsed.args.get('order'):
            jsql['order_by'] = [_convert_order(order_expr) for order_expr in parsed.args['order'].expressions]
        
        # LIMIT/OFFSET
        if parsed.args.get('limit'):
            limit_expr = parsed.args['limit'].expression
            if isinstance(limit_expr, exp.Literal):
                jsql['limit'] = int(limit_expr.this)
        
        if parsed.args.get('offset'):
            offset_expr = parsed.args['offset'].expression
            if isinstance(offset_expr, exp.Literal):
                jsql['offset'] = int(offset_expr.this)
        
        # GROUP BY
        if parsed.args.get('group'):
            jsql['group_by'] = [sqlglot_expression_to_jsql(g) for g in parsed.args['group'].expressions]
        
        # HAVING
        if parsed.args.get('having'):
            having_expr = parsed.args['having'].this
            jsql['having'] = sqlglot_condition_to_jsql(having_expr)
        
        logger.info('Successfully converted SQL to JSQL')
        return jsql
    
    except JSQLSyntaxError:
        raise
    except Exception as e:
        logger.error(f'Parsing error: {e}', exc_info=True)
        raise JSQLSyntaxError(
            message=f'Failed to parse SQL: {e!s}',
            path='',
        ) from e


def _convert_from(from_expr: exp.From) -> str | dict:
    """Convert FROM clause."""
    if isinstance(from_expr, exp.From):
        table = from_expr.this
        if isinstance(table, exp.Table):
            if table.alias:
                return {'entity': table.name, 'alias': table.alias}
            return table.name
    return 'unknown'


def _convert_order(order_expr: exp.Ordered) -> dict:
    """Convert ORDER BY expression."""
    from namerec.uma.jsql.conversion_exceptions import InvalidExpressionError
    
    if not isinstance(order_expr, exp.Ordered):
        raise InvalidExpressionError(
            message=f'Expected Ordered, got {type(order_expr).__name__}',
            path='order_by',
            expression={'type': type(order_expr).__name__}
        )
    
    field_expr = sqlglot_expression_to_jsql(order_expr.this)
    result = field_expr.copy()
    direction = OrderDirection.DESC if order_expr.args.get('desc') else OrderDirection.ASC
    result['direction'] = direction.value.upper()
    
    return result
```

---

## Результат рефакторинга

### До:
```
jsql/
├── converter.py (1033 строки, всё в одном файле)
└── ...
```

### После:
```
jsql/
├── converter/
│   ├── __init__.py (12 строк - public API)
│   ├── constants.py (60 строк - маппинги)
│   ├── expressions.py (180 строк - выражения)
│   ├── conditions.py (260 строк - условия)
│   ├── joins.py (120 строк - JOIN'ы)
│   ├── jsql_to_sql.py (150 строк - JSQL→SQL)
│   └── sql_to_jsql.py (140 строк - SQL→JSQL)
└── ...
```

### Преимущества:

1. **Четкое разделение ответственностей**
   - Каждый модуль отвечает за один аспект конверсии
   - Легче понять и поддерживать

2. **Переиспользование кода**
   - `expressions.py` используется и в JSQL→SQL и в SQL→JSQL
   - `conditions.py` - общая логика для обоих направлений

3. **Легче тестировать**
   - Можно тестировать каждый модуль независимо
   - Моки и фикстуры проще

4. **Параллельная разработка**
   - Разные разработчики могут работать над разными модулями

5. **Навигация по коду**
   - Легко найти нужную функцию
   - IDE лучше работает с небольшими файлами

---

**Трудозатраты:** 6-8 часов  
**Риски:** Низкие (чисто внутренний рефакторинг, API не меняется)
