"""Entity extraction from SQLAlchemy AST."""

from typing import Any

from sqlalchemy.sql.expression import ColumnElement
from sqlalchemy.sql.expression import Select
from sqlalchemy import Table


def extract_select_entities_from_ast(select_ast: Select, from_entity: str) -> list[str]:
    """
    Extract entities from SELECT clause using SQLAlchemy AST.

    Pure function - no state, no side effects.

    Args:
        select_ast: Parsed SQLAlchemy SELECT statement
        from_entity: Entity name from FROM clause (for '*' resolution)

    Returns:
        List of entity names (qualified with namespace if present)
    """
    entities: set[str] = set()

    # Extract from SELECT clause columns (use selected_columns instead of deprecated columns)
    for column in select_ast.selected_columns:
        if isinstance(column, str):
            if column == '*' or '.' not in column:
                entities.add(from_entity)
            else:
                entity_part = column.split('.', 1)[0]
                entities.add(entity_part)

        elif isinstance(column, ColumnElement):
            # Column expression - extract from table
            if hasattr(column, 'table') and column.table is not None:
                table = column.table
                if isinstance(table, Table):
                    entities.add(table.name)
                else:
                    entities.add(str(table))

        elif isinstance(column, Select):
            # Subquery in SELECT - extract recursively
            subquery_entities = extract_select_entities_from_ast(column, from_entity)
            entities.update(subquery_entities)

    return list(entities)
