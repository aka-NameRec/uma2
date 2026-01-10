"""Pytest configuration and fixtures."""

import pytest
import pytest_asyncio
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMAContext
from namerec.uma import init_global_registry
from namerec.uma import uma_initialize


@pytest.fixture
def metadata() -> MetaData:
    """Create test metadata with simple schema."""
    metadata = MetaData()

    Table(
        'users',
        metadata,
        Column('id', Integer, primary_key=True),
        Column('name', String(100), nullable=False),
        Column('email', String(100), nullable=False),
    )

    return metadata


@pytest_asyncio.fixture
async def engine(metadata: MetaData):  # noqa: ANN201
    """Create async engine with test database."""
    engine = create_async_engine('sqlite+aiosqlite:///:memory:')

    yield engine

    await engine.dispose()


@pytest_asyncio.fixture
async def context(engine, metadata) -> UMAContext:  # noqa: ANN001
    """Create UMA context."""
    metadata_provider = DefaultMetadataProvider()
    
    # Initialize UMA with namespace config
    uma_initialize({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })
    
    # Create context
    ctx = UMAContext(
        engine=engine,
        metadata_provider=metadata_provider,
        namespace='test',
    )
    
    # Preload metadata for tests (creates tables first)
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)
    
    await metadata_provider.preload(engine, namespace='test')
    
    return ctx


@pytest.fixture(autouse=True)
def reset_global_registry() -> None:
    """Reset global registry before each test."""
    init_global_registry()
