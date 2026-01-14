"""Metadata cache for database schema and tables."""

from sqlalchemy import MetaData
from sqlalchemy import Table


class MetadataCache:
    """
    Cache for database metadata and tables.

    Provides efficient storage and retrieval of reflected database schema
    organized by namespace, along with custom metadata management.

    Attributes:
        _metadata: Cached MetaData objects per namespace
        _tables: Cached Table objects per namespace
        _custom_metadata: Custom metadata dictionary per entity
    """

    def __init__(self) -> None:
        """Initialize empty cache."""
        self._metadata: dict[str, MetaData] = {}
        self._tables: dict[str, dict[str, Table]] = {}
        self._custom_metadata: dict[str, dict] = {}

    def get_metadata(self, namespace: str) -> MetaData | None:
        """
        Get cached metadata for namespace.

        Args:
            namespace: Namespace identifier

        Returns:
            Cached MetaData or None if not found
        """
        return self._metadata.get(namespace)

    def cache_metadata(self, namespace: str, metadata: MetaData) -> None:
        """
        Cache metadata for namespace.

        Automatically extracts and caches tables from metadata.

        Args:
            namespace: Namespace identifier
            metadata: MetaData object to cache
        """
        self._metadata[namespace] = metadata
        self._tables[namespace] = {t.name: t for t in metadata.tables.values()}

    def get_table(self, namespace: str, table_name: str) -> Table | None:
        """
        Get cached table.

        Args:
            namespace: Namespace identifier
            table_name: Table name

        Returns:
            Cached Table or None if not found
        """
        return self._tables.get(namespace, {}).get(table_name)

    def list_tables(self, namespace: str) -> list[str]:
        """
        List all tables in namespace.

        Args:
            namespace: Namespace identifier

        Returns:
            Sorted list of table names (empty if namespace not cached)
        """
        return sorted(self._tables.get(namespace, {}).keys())

    def has_namespace(self, namespace: str) -> bool:
        """
        Check if namespace is cached.

        Args:
            namespace: Namespace identifier

        Returns:
            True if namespace metadata is cached, False otherwise
        """
        return namespace in self._metadata

    def get_custom_metadata(self, entity_key: str) -> dict:
        """
        Get custom metadata for entity.

        Args:
            entity_key: Entity identifier (usually str(EntityName))

        Returns:
            Custom metadata dictionary (empty dict if not found)
        """
        return self._custom_metadata.get(entity_key, {})

    def set_custom_metadata(self, entity_key: str, metadata: dict) -> None:
        """
        Set custom metadata for entity.

        Args:
            entity_key: Entity identifier
            metadata: Custom metadata dictionary
        """
        self._custom_metadata[entity_key] = metadata

    def update_custom_metadata(self, entity_key: str, metadata: dict) -> None:
        """
        Update (merge) custom metadata for entity.

        Args:
            entity_key: Entity identifier
            metadata: Metadata dictionary to merge
        """
        if entity_key in self._custom_metadata:
            self._custom_metadata[entity_key].update(metadata)
        else:
            self._custom_metadata[entity_key] = metadata

    def clear_custom_metadata(self, entity_key: str) -> None:
        """
        Clear custom metadata for entity.

        Args:
            entity_key: Entity identifier
        """
        self._custom_metadata.pop(entity_key, None)

    def clear_namespace(self, namespace: str) -> None:
        """
        Clear all cached data for namespace.

        Args:
            namespace: Namespace identifier
        """
        self._metadata.pop(namespace, None)
        self._tables.pop(namespace, None)

    def clear_all(self) -> None:
        """Clear all cached data."""
        self._metadata.clear()
        self._tables.clear()
        self._custom_metadata.clear()
