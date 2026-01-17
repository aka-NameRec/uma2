"""Type definitions for JSQL module."""

from dataclasses import dataclass
from typing import Any
from typing import Literal
from typing import TypedDict


@dataclass(slots=True)
class ColumnMetadata:
    """Metadata for a result column."""

    name: str
    type: str
    nullable: bool
    qualified_name: str | None = None
    source_entity: str | None = None
    source_field: str | None = None
    size: int | None = None
    precision: int | None = None
    scale: int | None = None

    def to_dict(self) -> dict[str, Any]:
        """
        Convert to dictionary for JSON serialization.

        Returns:
            Dictionary representation
        """
        return {
            'name': self.name,
            'type': self.type,
            'nullable': self.nullable,
            'qualified_name': self.qualified_name,
            'source_entity': self.source_entity,
            'source_field': self.source_field,
            'size': self.size,
            'precision': self.precision,
            'scale': self.scale,
        }


@dataclass(slots=True)
class QueryResult:
    """Result of JSQL query execution."""

    meta: list[ColumnMetadata]
    data: list[list[Any]]
    debug: str | None = None

    def to_dict(self) -> dict[str, Any]:
        """
        Convert to dictionary for JSON serialization.

        Returns:
            Dictionary with 'meta' and 'data' keys, optionally 'debug'
        """
        result: dict[str, Any] = {
            'meta': [col.to_dict() for col in self.meta],
            'data': self.data,
        }
        if self.debug is not None:
            result['debug'] = self.debug
        return result


# Type aliases for JSQL structure
JSQLQuery = dict[str, Any]
JSQLExpression = dict[str, Any]
JSQLCondition = dict[str, Any]

# String operator types
JSQLStringOperator = Literal[
    'LIKE', 'NOT LIKE', 'ILIKE', 'NOT ILIKE',
    'SIMILAR TO', 'REGEXP', 'RLIKE'
]

# Comparison operator types
JSQLComparisonOperator = Literal[
    '=', '!=', '<>', '<', '<=', '>', '>='
]

# Logical operator types
JSQLLogicalOperator = Literal['AND', 'OR', 'NOT']

# Special operator types
JSQLSpecialOperator = Literal[
    'IN', 'NOT IN', 'BETWEEN', 'NOT BETWEEN',
    'IS NULL', 'IS NOT NULL', 'EXISTS', 'NOT EXISTS'
]


class JSQLFieldRef(TypedDict, total=False):
    """JSQL field reference specification."""
    field: str


class JSQLValueRef(TypedDict, total=False):
    """JSQL value literal specification."""
    value: Any


class JSQLBetweenFields(TypedDict, total=False):
    """Normalized BETWEEN fields structure."""
    expr: dict[str, Any]  # Can be field or value reference
    low: dict[str, Any]   # Can be field or value reference
    high: dict[str, Any]  # Can be field or value reference
