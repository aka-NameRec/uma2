"""Tests for JSQL query caching."""

import pytest
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy import create_engine

from namerec.uma.application import UMA
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.jsql.cache import CachedQuery
from namerec.uma.jsql.cache import make_cache_key
from namerec.uma.jsql.cache import MemoryCacheBackend
from namerec.uma.metadata.provider import DefaultMetadataProvider


@pytest.fixture
def test_engine():
    """Create in-memory SQLite engine for testing."""
    from sqlalchemy.ext.asyncio import create_async_engine

    engine = create_async_engine('sqlite+aiosqlite:///:memory:', future=True)

    # Create test table synchronously
    from sqlalchemy import create_engine as create_sync_engine

    sync_engine = create_sync_engine('sqlite:///:memory:', future=True)
    metadata = MetaData()
    users_table = Table(
        'users',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(50)),
        Column('email', String(100)),
    )
    metadata.create_all(sync_engine)

    # Insert test data
    with sync_engine.connect() as conn:
        conn.execute(
            users_table.insert(),
            [
                {'id': 1, 'name': 'Alice', 'email': 'alice@example.com'},
                {'id': 2, 'name': 'Bob', 'email': 'bob@example.com'},
            ],
        )
        conn.commit()

    # Copy database to async engine
    # For SQLite in-memory, we need to use a file-based DB for async
    import tempfile
    import shutil

    # Create temp file
    temp_db = tempfile.NamedTemporaryFile(delete=False, suffix='.db')
    temp_db.close()

    # Create schema and data in temp file
    file_engine = create_sync_engine(f'sqlite:///{temp_db.name}', future=True)
    metadata.create_all(file_engine)
    with file_engine.connect() as conn:
        conn.execute(
            users_table.insert(),
            [
                {'id': 1, 'name': 'Alice', 'email': 'alice@example.com'},
                {'id': 2, 'name': 'Bob', 'email': 'bob@example.com'},
            ],
        )
        conn.commit()

    # Create async engine for temp file
    async_engine = create_async_engine(f'sqlite+aiosqlite:///{temp_db.name}', future=True)

    yield async_engine

    # Cleanup
    import os
    import asyncio

    asyncio.run(async_engine.dispose())
    os.unlink(temp_db.name)


@pytest.fixture
def uma_with_cache(test_engine):
    """Create UMA instance with memory cache."""
    return UMA.create(
        namespace_configs={
            'main': NamespaceConfig(
                engine=test_engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        },
        cache_backend=MemoryCacheBackend(max_size=10),
    )


@pytest.fixture
def uma_without_cache(test_engine):
    """Create UMA instance without cache."""
    return UMA.create(
        namespace_configs={
            'main': NamespaceConfig(
                engine=test_engine,
                metadata_provider=DefaultMetadataProvider(),
            ),
        },
        cache_enabled=False,
    )


class TestCacheKey:
    """Test cache key generation."""

    def test_same_jsql_same_key(self):
        """Same JSQL should produce same key."""
        jsql = {'from': 'users', 'select': ['id', 'name']}
        key1 = make_cache_key(jsql)
        key2 = make_cache_key(jsql)
        assert key1 == key2

    def test_different_jsql_different_key(self):
        """Different JSQL should produce different keys."""
        jsql1 = {'from': 'users', 'select': ['id', 'name']}
        jsql2 = {'from': 'users', 'select': ['id', 'email']}
        key1 = make_cache_key(jsql1)
        key2 = make_cache_key(jsql2)
        assert key1 != key2

    def test_user_context_affects_key(self):
        """Different user contexts should produce different keys."""
        jsql = {'from': 'users', 'select': ['id', 'name']}
        key1 = make_cache_key(jsql, user_context={'role': 'admin'})
        key2 = make_cache_key(jsql, user_context={'role': 'user'})
        assert key1 != key2

    def test_namespace_affects_key(self):
        """Different namespaces should produce different keys."""
        jsql = {'from': 'users', 'select': ['id', 'name']}
        key1 = make_cache_key(jsql, namespace='db1')
        key2 = make_cache_key(jsql, namespace='db2')
        assert key1 != key2

    def test_key_is_compact(self):
        """Cache key should be compact (32 hex chars)."""
        jsql = {'from': 'users', 'select': ['id', 'name']}
        key = make_cache_key(jsql)
        assert len(key) == 32  # 16 bytes = 32 hex chars
        assert all(c in '0123456789abcdef' for c in key)


class TestMemoryCacheBackend:
    """Test memory cache backend."""

    def test_cache_miss(self):
        """First access should be cache miss."""
        cache = MemoryCacheBackend(max_size=10)
        result = cache.get('test_key')
        assert result is None

    def test_cache_hit(self):
        """Second access should be cache hit."""
        cache = MemoryCacheBackend(max_size=10)
        query = CachedQuery(
            sql='SELECT * FROM users',
            params_mapping={},
            dialect='sqlite',
        )
        cache.set('test_key', query)
        result = cache.get('test_key')
        assert result is not None
        assert result.sql == 'SELECT * FROM users'

    def test_lru_eviction(self):
        """Oldest entry should be evicted when cache full."""
        cache = MemoryCacheBackend(max_size=2)
        query1 = CachedQuery(sql='SELECT 1', params_mapping={}, dialect='sqlite')
        query2 = CachedQuery(sql='SELECT 2', params_mapping={}, dialect='sqlite')
        query3 = CachedQuery(sql='SELECT 3', params_mapping={}, dialect='sqlite')

        cache.set('key1', query1)
        cache.set('key2', query2)
        cache.set('key3', query3)  # Should evict key1

        assert cache.get('key1') is None  # Evicted
        assert cache.get('key2') is not None
        assert cache.get('key3') is not None

    def test_stats(self):
        """Stats should track hits/misses."""
        cache = MemoryCacheBackend(max_size=10)
        query = CachedQuery(sql='SELECT 1', params_mapping={}, dialect='sqlite')

        cache.set('key1', query)
        cache.get('key1')  # Hit
        cache.get('key2')  # Miss

        stats = cache.stats()
        assert stats['backend'] == 'memory'
        assert stats['size'] == 1
        assert stats['hits'] == 1
        assert stats['misses'] == 1
        assert stats['hit_rate'] == 0.5

    def test_clear(self):
        """Clear should remove all entries."""
        cache = MemoryCacheBackend(max_size=10)
        query = CachedQuery(sql='SELECT 1', params_mapping={}, dialect='sqlite')

        cache.set('key1', query)
        cache.set('key2', query)
        cache.clear()

        assert cache.get('key1') is None
        assert cache.get('key2') is None
        assert cache.stats()['size'] == 0


class TestCachedQuery:
    """Test CachedQuery serialization."""

    def test_to_dict(self):
        """Should serialize to dict."""
        query = CachedQuery(
            sql='SELECT * FROM users WHERE id = :id',
            params_mapping={'id': 'id'},
            dialect='postgresql',
        )
        data = query.to_dict()
        assert data['sql'] == 'SELECT * FROM users WHERE id = :id'
        assert data['params_mapping'] == {'id': 'id'}
        assert data['dialect'] == 'postgresql'

    def test_from_dict(self):
        """Should deserialize from dict."""
        data = {
            'sql': 'SELECT * FROM users WHERE id = :id',
            'params_mapping': {'id': 'id'},
            'dialect': 'postgresql',
        }
        query = CachedQuery.from_dict(data)
        assert query.sql == 'SELECT * FROM users WHERE id = :id'
        assert query.params_mapping == {'id': 'id'}
        assert query.dialect == 'postgresql'


@pytest.mark.asyncio
class TestUMACacheIntegration:
    """Test UMA cache integration."""

    async def test_cache_enabled_by_default(self, uma_with_cache):
        """Cache should be enabled by default."""
        assert uma_with_cache.cache_enabled is True
        assert uma_with_cache.cache_backend is not None

    async def test_cache_disabled(self, uma_without_cache):
        """Cache can be disabled."""
        assert uma_without_cache.cache_enabled is False

    async def test_query_caching(self, uma_with_cache):
        """Repeated queries should hit cache."""
        jsql = {'from': 'users', 'select': ['id', 'name']}

        # First query - cache miss
        result1 = await uma_with_cache.select(jsql)
        stats1 = uma_with_cache.cache_stats()
        assert stats1['misses'] == 1

        # Second query - cache hit
        result2 = await uma_with_cache.select(jsql)
        stats2 = uma_with_cache.cache_stats()
        assert stats2['hits'] == 1

        # Data should be identical (metadata may differ for cached queries)
        assert result1['data'] == result2['data']

    async def test_disable_cache(self, uma_with_cache):
        """Disabling cache should bypass cache lookup."""
        jsql = {'from': 'users', 'select': ['id', 'name']}

        # First query - populate cache
        await uma_with_cache.select(jsql)

        # Disable cache
        uma_with_cache.disable_cache()

        # Query should not hit cache
        await uma_with_cache.select(jsql)
        stats = uma_with_cache.cache_stats()
        assert stats['hits'] == 0  # No hits after disabling

    async def test_enable_cache(self, uma_with_cache):
        """Re-enabling cache should restore caching."""
        jsql = {'from': 'users', 'select': ['id', 'name']}

        uma_with_cache.disable_cache()
        await uma_with_cache.select(jsql)

        uma_with_cache.enable_cache()
        await uma_with_cache.select(jsql)  # Miss
        await uma_with_cache.select(jsql)  # Hit

        stats = uma_with_cache.cache_stats()
        assert stats['hits'] == 1

    async def test_clear_cache(self, uma_with_cache):
        """Clear should invalidate all cached queries."""
        jsql = {'from': 'users', 'select': ['id', 'name']}

        # Populate cache
        await uma_with_cache.select(jsql)
        stats1 = uma_with_cache.cache_stats()
        assert stats1['size'] == 1

        # Clear cache (also resets stats)
        uma_with_cache.clear_cache()
        stats2 = uma_with_cache.cache_stats()
        assert stats2['size'] == 0
        assert stats2['hits'] == 0
        assert stats2['misses'] == 0

        # Next query should be cache miss
        await uma_with_cache.select(jsql)
        stats3 = uma_with_cache.cache_stats()
        assert stats3['misses'] == 1  # New miss after clear

    async def test_cache_stats(self, uma_with_cache):
        """Cache stats should provide useful metrics."""
        jsql = {'from': 'users', 'select': ['id', 'name']}

        # Initial stats
        stats0 = uma_with_cache.cache_stats()
        assert stats0['backend'] == 'memory'
        assert stats0['size'] == 0
        assert stats0['hits'] == 0
        assert stats0['misses'] == 0

        # After queries
        await uma_with_cache.select(jsql)  # Miss
        await uma_with_cache.select(jsql)  # Hit

        stats1 = uma_with_cache.cache_stats()
        assert stats1['size'] == 1
        assert stats1['hits'] == 1
        assert stats1['misses'] == 1
        assert stats1['hit_rate'] == 0.5


class TestRedisCacheBackend:
    """Test Redis cache backend (requires redis)."""

    def test_redis_import(self):
        """Redis backend should be importable."""
        try:
            from namerec.uma.jsql.cache.redis import RedisCacheBackend

            backend = RedisCacheBackend('redis://localhost:6379/0')
            assert backend is not None
        except ImportError:
            pytest.skip('redis package not installed')
        except Exception:
            # Connection error is OK - we just test import
            pass
