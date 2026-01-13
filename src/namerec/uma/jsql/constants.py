"""Constants for JSQL module to avoid magic strings."""

from enum import Enum


class JSQLField(str, Enum):
    """Field names used in JSQL structure."""

    # Query structure
    WITH = 'with'
    FROM = 'from'
    SELECT = 'select'
    WHERE = 'where'
    JOINS = 'joins'
    GROUP_BY = 'group_by'
    HAVING = 'having'
    ORDER_BY = 'order_by'
    LIMIT = 'limit'
    OFFSET = 'offset'

    # Common fields
    NAME = 'name'
    QUERY = 'query'
    FIELD = 'field'
    ALIAS = 'alias'
    VALUE = 'value'
    PARAM = 'param'

    # Functions and expressions
    FUNC = 'func'
    EXPR = 'expr'
    ARGS = 'args'
    DISTINCT = 'distinct'

    # Operators
    OP = 'op'
    LEFT = 'left'
    RIGHT = 'right'
    CONDITIONS = 'conditions'
    CONDITION = 'condition'

    # Window functions
    WINDOW_FUNC = 'window_func'
    PARTITION_BY = 'partition_by'

    # Subqueries
    SUBQUERY = 'subquery'

    # Joins
    TABLE = 'table'
    TYPE = 'type'
    ON = 'on'

    # Order by
    DIRECTION = 'direction'


class JSQLOperator(str, Enum):
    """SQL operators used in JSQL."""

    # Logical operators
    AND = 'AND'
    OR = 'OR'
    NOT = 'NOT'

    # Comparison operators
    EQ = '='
    NE = '!='
    LT = '<'
    LE = '<='
    GT = '>'
    GE = '>='

    # Special operators
    IN = 'IN'
    BETWEEN = 'BETWEEN'
    EXISTS = 'EXISTS'
    IS_NULL = 'IS NULL'
    IS_NOT_NULL = 'IS NOT NULL'
    LIKE = 'LIKE'

    # Arithmetic operators
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'
    MOD = '%'


class JoinType(str, Enum):
    """SQL join types."""

    INNER = 'INNER'
    LEFT = 'LEFT'
    RIGHT = 'RIGHT'
    FULL = 'FULL'
    CROSS = 'CROSS'


class OrderDirection(str, Enum):
    """SQL order directions."""

    ASC = 'asc'
    DESC = 'desc'


# Operator groups for easier checking
LOGICAL_OPERATORS = frozenset({JSQLOperator.AND, JSQLOperator.OR, JSQLOperator.NOT})
COMPARISON_OPERATORS = frozenset({
    JSQLOperator.EQ,
    JSQLOperator.NE,
    JSQLOperator.LT,
    JSQLOperator.LE,
    JSQLOperator.GT,
    JSQLOperator.GE,
})
ARITHMETIC_OPERATORS = frozenset({
    JSQLOperator.ADD,
    JSQLOperator.SUB,
    JSQLOperator.MUL,
    JSQLOperator.DIV,
    JSQLOperator.MOD,
})
