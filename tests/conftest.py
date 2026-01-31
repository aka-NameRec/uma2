"""Pytest configuration and fixtures."""

from pathlib import Path

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
from namerec.uma import UMA
from namerec.uma import UMAContext


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
async def engine(tmp_path: Path):  # noqa: ANN201
    """Create async engine with test database."""
    db_path = tmp_path / 'uma_test.db'
    engine = create_async_engine(f'sqlite+aiosqlite:///{db_path}')

    yield engine

    await engine.dispose()


@pytest_asyncio.fixture
async def uma(engine, metadata) -> UMA:  # noqa: ANN001
    """Create UMA instance for tests."""
    metadata_provider = DefaultMetadataProvider()

    # Create tables first
    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

    # Create UMA instance
    uma_instance = UMA.create({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=metadata_provider,
        ),
    })

    # Preload metadata for tests
    await metadata_provider.preload(engine, namespace='test')

    return uma_instance


@pytest_asyncio.fixture
async def context(uma: UMA) -> UMAContext:  # noqa: ANN001
    """Create UMA context."""
    # Create context from UMA instance
    return uma._create_context('test')
