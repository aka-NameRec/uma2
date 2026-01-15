"""Cache backend protocol definition."""

from typing import Any
from typing import Protocol
from typing import runtime_checkable

from namerec.uma.jsql.cache.keys import CachedQuery


@runtime_checkable
class CacheBackend(Protocol):
    """
    Protocol for cache backends.

    Allows structural subtyping - any class implementing these methods
    can be used as a cache backend without explicit inheritance.
    """

    def get(self, key: str) -> CachedQuery | None:
        """
        Get cached query by key.

        Args:
            key: Cache key

        Returns:
            Cached query or None if not found
        """
        ...

    def set(self, key: str, query: CachedQuery, ttl: int | None = None) -> None:
        """
        Store cached query.

        Args:
            key: Cache key
            query: Query to cache
            ttl: Optional TTL in seconds (None = use backend default)
        """
        ...

    def clear(self) -> None:
        """Clear all cached queries."""
        ...

    def stats(self) -> dict[str, Any]:
        """
        Get cache statistics.

        Returns:
            Statistics dictionary with at least:
            - backend: backend type name
            - size: current number of cached items
            - hits: cache hit count
            - misses: cache miss count
            - hit_rate: hit rate (0.0-1.0)
        """
        ...
