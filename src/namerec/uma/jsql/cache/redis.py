"""Redis cache backend for distributed deployments."""

from typing import Any

from namerec.uma.jsql.cache.keys import CachedQuery


class RedisCacheBackend:
    """
    Redis cache backend (multi-process, multi-container).

    Suitable for:
    - Production deployments
    - Multi-process servers (uvicorn workers)
    - Distributed systems (k8s pods)
    - When query cache needs to be shared across instances

    Requires:
    - redis package (install with: pip install redis)
    - Running Redis instance
    """

    def __init__(
        self,
        redis_url: str = 'redis://localhost:6379/0',
        prefix: str = 'uma:jsql:',
        default_ttl: int = 3600,
    ) -> None:
        """
        Initialize Redis cache.

        Args:
            redis_url: Redis connection URL
            prefix: Key prefix for namespacing
            default_ttl: Default TTL in seconds (1 hour)

        Raises:
            ImportError: If redis package not installed
        """
        try:
            import redis
        except ImportError as e:
            msg = 'redis package required for RedisCacheBackend. Install with: pip install redis'
            raise ImportError(msg) from e

        self._redis = redis.from_url(redis_url, decode_responses=True)
        self._prefix = prefix
        self._default_ttl = default_ttl

        # Stats keys (separate from cached queries)
        self._hits_key = f'{prefix}stats:hits'
        self._misses_key = f'{prefix}stats:misses'

    def get(self, key: str) -> CachedQuery | None:
        """Get query from Redis."""
        import json

        full_key = f'{self._prefix}{key}'
        data = self._redis.get(full_key)

        if data:
            self._redis.incr(self._hits_key)
            return CachedQuery.from_dict(json.loads(data))

        self._redis.incr(self._misses_key)
        return None

    def set(self, key: str, query: CachedQuery, ttl: int | None = None) -> None:
        """Store query in Redis with TTL."""
        import json

        full_key = f'{self._prefix}{key}'
        ttl = ttl or self._default_ttl

        self._redis.setex(
            full_key,
            ttl,
            json.dumps(query.to_dict()),
        )

    def clear(self) -> None:
        """Clear all UMA cache keys."""
        pattern = f'{self._prefix}*'
        # Use cursor-based scan for safe deletion
        cursor = 0
        while True:
            cursor, keys = self._redis.scan(cursor, match=pattern, count=100)
            if keys:
                self._redis.delete(*keys)
            if cursor == 0:
                break

    def stats(self) -> dict[str, Any]:
        """Get cache statistics from Redis."""
        hits = int(self._redis.get(self._hits_key) or 0)
        misses = int(self._redis.get(self._misses_key) or 0)
        total = hits + misses

        # Count cached queries (expensive for large caches)
        pattern = f'{self._prefix}*'
        # Approximate size using DBSIZE (includes all keys, not just ours)
        # For exact count, would need to scan all keys
        size = 0
        cursor = 0
        while True:
            cursor, keys = self._redis.scan(cursor, match=pattern, count=100)
            size += len([k for k in keys if not k.endswith(':stats:hits') and not k.endswith(':stats:misses')])
            if cursor == 0:
                break

        return {
            'backend': 'redis',
            'size': size,
            'hits': hits,
            'misses': misses,
            'hit_rate': hits / total if total > 0 else 0.0,
            'redis_url': self._redis.connection_pool.connection_kwargs.get('host', 'unknown'),
        }
