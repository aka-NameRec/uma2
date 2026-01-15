"""JSQL query caching package."""

from namerec.uma.jsql.cache.keys import CachedQuery
from namerec.uma.jsql.cache.keys import make_cache_key
from namerec.uma.jsql.cache.memory import MemoryCacheBackend
from namerec.uma.jsql.cache.protocol import CacheBackend

# Redis backend is imported lazily to avoid dependency issues
# Use: from namerec.uma.jsql.cache.redis import RedisCacheBackend

__all__ = [
    'CacheBackend',
    'CachedQuery',
    'make_cache_key',
    'MemoryCacheBackend',
]
