"""Alias manager for JSQL/SQL table and column aliases."""

from dataclasses import dataclass
from dataclasses import field
from typing import Any

from sqlalchemy.sql import ColumnElement

from namerec.uma.jsql.exceptions import JSQLSyntaxError


@dataclass
class AliasManager:
    """
    Manages table aliases and column index for JSQL/SQL conversion.

    Provides O(1) column lookup through indexing and consistent
    alias management across parser and converter modules.

    Attributes:
        _table_aliases: Maps alias -> table/CTE object
        _column_index: Maps column_name -> [(source_alias, column), ...]
        _cte_aliases: Maps CTE name -> CTE object
    """

    _table_aliases: dict[str, Any] = field(default_factory=dict, init=False)
    _column_index: dict[str, list[tuple[str, ColumnElement]]] = field(
        default_factory=dict, init=False
    )
    _cte_aliases: dict[str, Any] = field(default_factory=dict, init=False)

    def register_table(
        self,
        alias: str,
        table: Any,
        *additional_aliases: str,
    ) -> None:
        """
        Register table with alias and index its columns.

        Args:
            alias: Primary alias for the table
            table: Table or CTE object
            additional_aliases: Additional aliases for the same table
        """
        # Register primary and additional aliases
        self._table_aliases[alias] = table
        for add_alias in additional_aliases:
            self._table_aliases[add_alias] = table

        # Index columns for fast unqualified lookup
        if hasattr(table, 'columns'):
            for col in table.columns:
                self._index_column(alias, col)

    def register_cte(self, name: str, cte: Any) -> None:
        """
        Register CTE and index its columns.

        Args:
            name: CTE name
            cte: CTE object
        """
        self._cte_aliases[name] = cte

        # Index CTE columns
        if hasattr(cte, 'columns'):
            for col in cte.columns:
                self._index_column(name, col)

    def _index_column(self, source_alias: str, column: ColumnElement) -> None:
        """
        Add column to index for fast lookup.

        Args:
            source_alias: Table/CTE alias
            column: Column object
        """
        col_name = column.name
        if col_name not in self._column_index:
            self._column_index[col_name] = []
        self._column_index[col_name].append((source_alias, column))

    def resolve_column(
        self,
        field_spec: str,
        from_clause: Any | None = None,
    ) -> ColumnElement:
        """
        Resolve column reference with O(1) lookup.

        Handles both qualified (table.column) and unqualified (column) references.

        Args:
            field_spec: Column name or qualified name (table.column)
            from_clause: Optional FROM clause for fallback lookup

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If column not found or ambiguous
        """
        # Qualified column (table.column)
        if '.' in field_spec:
            return self._resolve_qualified_column(field_spec)

        # Unqualified column - use index for O(1) lookup
        return self._resolve_unqualified_column(field_spec, from_clause)

    def _resolve_qualified_column(self, field_spec: str) -> ColumnElement:
        """
        Resolve qualified column reference (table.column).

        Args:
            field_spec: Qualified column name (table.column)

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If column not found
        """
        table_name, col_name = field_spec.split('.', 1)

        # Check table aliases
        if table_name in self._table_aliases:
            table = self._table_aliases[table_name]
            if hasattr(table, 'columns') and col_name in table.columns:
                return table.columns[col_name]

        # Check CTEs
        if table_name in self._cte_aliases:
            cte = self._cte_aliases[table_name]
            if hasattr(cte, 'columns') and col_name in cte.columns:
                return cte.columns[col_name]

        raise JSQLSyntaxError(
            f'Column "{col_name}" not found in table "{table_name}"'
        )

    def _resolve_unqualified_column(
        self,
        field_spec: str,
        from_clause: Any | None,
    ) -> ColumnElement:
        """
        Resolve unqualified column reference using index.

        Args:
            field_spec: Column name (unqualified)
            from_clause: Optional FROM clause for fallback

        Returns:
            Resolved column element

        Raises:
            JSQLSyntaxError: If column not found or ambiguous
        """
        # Check column index
        if field_spec in self._column_index:
            candidates = self._column_index[field_spec]

            # Unambiguous - single source
            if len(candidates) == 1:
                return candidates[0][1]

            # Ambiguous - multiple sources
            sources = [alias for alias, _ in candidates]
            raise JSQLSyntaxError(
                f'Ambiguous column "{field_spec}" found in: {", ".join(sources)}. '
                f'Use qualified name (e.g., {sources[0]}.{field_spec})'
            )

        # Fallback to FROM clause
        if from_clause is not None:
            try:
                if hasattr(from_clause, 'columns') and field_spec in from_clause.columns:
                    return from_clause.columns[field_spec]
            except (TypeError, AttributeError):
                # SQLAlchemy ClauseElement doesn't support bool() or columns access
                pass

        raise JSQLSyntaxError(f'Column "{field_spec}" not found')

    def get_table(self, alias: str) -> Any | None:
        """
        Get table by alias.

        Args:
            alias: Table or CTE alias

        Returns:
            Table/CTE object or None if not found
        """
        return self._table_aliases.get(alias) or self._cte_aliases.get(alias)

    def has_alias(self, alias: str) -> bool:
        """
        Check if alias is registered.

        Args:
            alias: Alias to check

        Returns:
            True if alias exists, False otherwise
        """
        return alias in self._table_aliases or alias in self._cte_aliases

    def clear(self) -> None:
        """Clear all aliases and indexes."""
        self._table_aliases.clear()
        self._column_index.clear()
        self._cte_aliases.clear()
