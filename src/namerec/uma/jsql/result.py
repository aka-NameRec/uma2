"""JSQL result builder - converts SQLAlchemy results to standardized format."""

from typing import Any

from sqlalchemy import CursorResult
from sqlalchemy.engine import Row
from sqlalchemy.sql.elements import Label

from namerec.uma.jsql.types import ColumnMetadata
from namerec.uma.jsql.types import QueryResult


class JSQLResultBuilder:
    """
    Builder for JSQL query results.
    Converts SQLAlchemy result set to standardized format with metadata.
    """

    @staticmethod
    def build_result(result: CursorResult, query: Any, debug_sql: str | None = None) -> QueryResult:  # noqa: ANN401
        """
        Build QueryResult from SQLAlchemy result.

        Args:
            result: SQLAlchemy cursor result
            query: SQLAlchemy query (for column metadata)
            debug_sql: Optional debug SQL string to include in result

        Returns:
            QueryResult with metadata and data

        """
        # Extract column metadata from query
        meta = JSQLResultBuilder._extract_metadata(result, query)

        # Extract data rows
        data = JSQLResultBuilder._extract_data(result)

        return QueryResult(meta=meta, data=data, debug=debug_sql)

    @staticmethod
    def _extract_metadata(result: CursorResult, query: Any) -> list[ColumnMetadata]:  # noqa: ANN401
        """
        Extract column metadata from query and result.

        Args:
            result: SQLAlchemy cursor result
            query: SQLAlchemy query

        Returns:
            List of ColumnMetadata

        """
        meta: list[ColumnMetadata] = []

        # Get column descriptions from result
        if not result.keys():
            return meta

        # Get columns from query (for better metadata)
        query_columns = list(query.selected_columns) if hasattr(query, 'selected_columns') else []

        for i, column_key in enumerate(result.keys()):
            # Get query column if available
            query_column = query_columns[i] if i < len(query_columns) else None

            # Build metadata
            column_meta = JSQLResultBuilder._build_column_metadata(
                column_key=column_key,
                query_column=query_column,
            )

            meta.append(column_meta)

        return meta

    @staticmethod
    def _build_column_metadata(column_key: str, query_column: Any) -> ColumnMetadata:  # noqa: ANN401
        """
        Build metadata for a single column.

        Args:
            column_key: Column key from result
            query_column: Query column element (if available)

        Returns:
            ColumnMetadata

        """
        # Default values
        name = column_key
        type_str = 'UNKNOWN'
        nullable = True
        qualified_name: str | None = None
        source_entity: str | None = None
        source_field: str | None = None
        size: int | None = None
        precision: int | None = None
        scale: int | None = None

        if query_column is not None:
            # Get column type
            if hasattr(query_column, 'type'):
                col_type = query_column.type
                type_str = str(col_type)

                # Extract size, precision, scale
                if hasattr(col_type, 'length') and col_type.length is not None:
                    size = col_type.length

                if hasattr(col_type, 'precision') and col_type.precision is not None:
                    precision = col_type.precision

                if hasattr(col_type, 'scale') and col_type.scale is not None:
                    scale = col_type.scale

            # Get nullable
            if hasattr(query_column, 'nullable'):
                nullable = query_column.nullable

            # Handle labeled columns (aliases)
            if isinstance(query_column, Label):
                name = query_column.name

                # Get the underlying element
                if hasattr(query_column, 'element'):
                    base_column = query_column.element

                    # Try to extract source info from base column
                    if hasattr(base_column, 'table') and hasattr(base_column, 'name'):
                        source_entity = base_column.table.name
                        source_field = base_column.name
                        qualified_name = f'{source_entity}.{source_field}'
            else:
                # Regular column
                if hasattr(query_column, 'name'):
                    name = query_column.name

                if hasattr(query_column, 'table') and query_column.table is not None:
                    source_entity = query_column.table.name
                    source_field = name
                    qualified_name = f'{source_entity}.{source_field}'
                elif hasattr(query_column, 'anon_map'):
                    # Try to extract from anonymous map (for expressions)
                    # This is best-effort
                    pass

        return ColumnMetadata(
            name=name,
            type=type_str,
            nullable=nullable,
            qualified_name=qualified_name,
            source_entity=source_entity,
            source_field=source_field,
            size=size,
            precision=precision,
            scale=scale,
        )

    @staticmethod
    def _extract_data(result: CursorResult) -> list[list[Any]]:
        """
        Extract data rows from result.

        Args:
            result: SQLAlchemy cursor result

        Returns:
            List of rows (each row is a list of values)

        """
        data: list[list[Any]] = []

        for row in result:
            # Convert Row to list of values
            if isinstance(row, Row):
                data.append(list(row))
            else:
                # Fallback for other result types
                data.append([row])

        return data
