"""In-memory LRU cache backend."""

from dataclasses import dataclass
from dataclasses import field
from typing import Any

from namerec.uma.jsql.cache.keys import CachedQuery


@dataclass
class MemoryCacheBackend:
    """
    In-memory LRU cache backend (single process).

    Suitable for:
    - Development and testing
    - Single-process deployments
    - When Redis is not available

    Not suitable for:
    - Multi-process deployments (uvicorn workers)
    - Distributed systems (k8s pods)
    """

    max_size: int = 128
    _cache: dict[str, CachedQuery] = field(default_factory=dict, init=False)
    _order: list[str] = field(default_factory=list, init=False)  # LRU tracking
    _hits: int = field(default=0, init=False)
    _misses: int = field(default=0, init=False)

    def get(self, key: str) -> CachedQuery | None:
        """Get query from cache with LRU update."""
        if key in self._cache:
            # Move to end (most recently used)
            self._order.remove(key)
            self._order.append(key)
            self._hits += 1
            return self._cache[key]

        self._misses += 1
        return None

    def set(self, key: str, query: CachedQuery, ttl: int | None = None) -> None:
        """Store query in cache with LRU eviction."""
        # Evict oldest if cache full
        if key not in self._cache and len(self._cache) >= self.max_size:
            oldest = self._order.pop(0)
            del self._cache[oldest]

        # Update or add
        if key in self._cache:
            # Update existing - move to end
            self._order.remove(key)

        self._cache[key] = query
        self._order.append(key)

    def clear(self) -> None:
        """Clear all cache."""
        self._cache.clear()
        self._order.clear()
        self._hits = 0
        self._misses = 0

    def stats(self) -> dict[str, Any]:
        """Get cache statistics."""
        total = self._hits + self._misses
        return {
            'backend': 'memory',
            'size': len(self._cache),
            'max_size': self.max_size,
            'hits': self._hits,
            'misses': self._misses,
            'hit_rate': self._hits / total if total > 0 else 0.0,
        }
