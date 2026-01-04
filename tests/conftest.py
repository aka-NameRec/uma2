"""Pytest configuration and fixtures."""

import pytest
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import UMAContext
from namerec.uma import init_global_registry


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


@pytest.fixture
async def engine(metadata: MetaData):  # noqa: ANN201
    """Create async engine with test database."""
    engine = create_async_engine('sqlite+aiosqlite:///:memory:')

    # Create tables
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

    yield engine

    await engine.dispose()


@pytest.fixture
def context(engine, metadata) -> UMAContext:  # noqa: ANN001
    """Create UMA context."""
    metadata_provider = DefaultMetadataProvider()
    return UMAContext(
        engine=engine,
        metadata=metadata,
        metadata_provider=metadata_provider,
    )


@pytest.fixture(autouse=True)
def reset_global_registry() -> None:
    """Reset global registry before each test."""
    init_global_registry()
