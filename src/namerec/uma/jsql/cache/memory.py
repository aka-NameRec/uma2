"""In-memory LRU cache backend."""

import threading
from collections import OrderedDict
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

    Thread-safe: Yes (uses RLock for concurrent access)
    """

    max_size: int = 128
    _cache: OrderedDict[str, CachedQuery] = field(default_factory=OrderedDict, init=False)
    _hits: int = field(default=0, init=False)
    _misses: int = field(default=0, init=False)
    _lock: threading.RLock = field(default_factory=threading.RLock, init=False)

    def __post_init__(self) -> None:
        """Validate initialization parameters."""
        if self.max_size <= 0:
            msg = 'max_size must be positive integer'
            raise ValueError(msg)

    def get(self, key: str) -> CachedQuery | None:
        """Get query from cache with LRU update (O(1))."""
        with self._lock:
            if key in self._cache:
                # Move to end (most recently used) - O(1) with OrderedDict
                self._cache.move_to_end(key)
                self._hits += 1
                return self._cache[key]

            self._misses += 1
            return None

    def set(self, key: str, query: CachedQuery, ttl: int | None = None) -> None:
        """Store query in cache with LRU eviction (O(1))."""
        # TTL parameter is ignored for in-memory cache (kept for API compatibility)
        del ttl

        with self._lock:
            # Evict oldest if cache full and key is new
            if key not in self._cache and len(self._cache) >= self.max_size:
                self._cache.popitem(last=False)  # Remove first (least recently used) - O(1)

            # Update or add (moves to end automatically if exists)
            self._cache[key] = query

    def clear(self) -> None:
        """Clear all cache."""
        with self._lock:
            self._cache.clear()
            self._hits = 0
            self._misses = 0

    def stats(self) -> dict[str, Any]:
        """Get cache statistics."""
        with self._lock:
            total = self._hits + self._misses
            return {
                'backend': 'memory',
                'size': len(self._cache),
                'max_size': self.max_size,
                'hits': self._hits,
                'misses': self._misses,
                'hit_rate': self._hits / total if total > 0 else 0.0,
            }
