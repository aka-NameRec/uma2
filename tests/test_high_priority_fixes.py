"""Regression tests for high-priority fixes."""

import pytest
from sqlalchemy import Column
from sqlalchemy import Integer
from sqlalchemy import MetaData
from sqlalchemy import String
from sqlalchemy import Table

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMA
from namerec.uma import UMAValidationError
from namerec.uma.core.context import UMAContext
from namerec.uma.core.types import EntityName


class ExplodingMetadataProvider(DefaultMetadataProvider):
    """Metadata provider used to verify fail-fast behavior."""

    async def get_metadata(
        self,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict:
        _ = entity_name
        _ = context
        raise RuntimeError('metadata provider failed')


async def _create_uma_with_metadata(
    engine,
    metadata: MetaData,
    metadata_provider: DefaultMetadataProvider | None = None,
) -> UMA:
    """Create UMA instance with provided schema."""
    provider = metadata_provider or DefaultMetadataProvider()

    async with engine.begin() as conn:
        await conn.run_sync(metadata.create_all)

    uma_instance = UMA.create({
        'test': NamespaceConfig(
            engine=engine,
            metadata_provider=provider,
        ),
    })
    await provider.preload(engine, namespace='test')
    return uma_instance


def _build_composite_pk_metadata() -> MetaData:
    """Build test schema with composite primary key."""
    metadata = MetaData()
    Table(
        'memberships',
        metadata,
        Column('user_id', Integer, primary_key=True),
        Column('group_id', Integer, primary_key=True),
        Column('role', String(50), nullable=False),
    )
    return metadata


@pytest.mark.asyncio
async def test_entity_details_does_not_swallow_metadata_provider_errors(engine, metadata) -> None:  # noqa: ANN001
    """meta() must raise when metadata provider fails."""
    uma = await _create_uma_with_metadata(
        engine=engine,
        metadata=metadata,
        metadata_provider=ExplodingMetadataProvider(),
    )

    with pytest.raises(RuntimeError, match='metadata provider failed'):
        await uma.entity_details('users')


@pytest.mark.asyncio
async def test_read_requires_full_composite_key(engine) -> None:  # noqa: ANN001
    """read() must validate composite primary key shape."""
    metadata = _build_composite_pk_metadata()
    uma = await _create_uma_with_metadata(
        engine=engine,
        metadata=metadata,
    )
    memberships = metadata.tables['memberships']
    async with engine.begin() as conn:
        await conn.execute(memberships.insert().values(user_id=1, group_id=10, role='member'))

    with pytest.raises(UMAValidationError, match='Composite primary key'):
        await uma.read('memberships', 1)

    with pytest.raises(UMAValidationError, match='requires exactly 2 values'):
        await uma.read('memberships', (1,))


@pytest.mark.asyncio
async def test_delete_requires_full_composite_key(engine) -> None:  # noqa: ANN001
    """delete() must validate composite primary key shape."""
    metadata = _build_composite_pk_metadata()
    uma = await _create_uma_with_metadata(
        engine=engine,
        metadata=metadata,
    )
    memberships = metadata.tables['memberships']
    async with engine.begin() as conn:
        await conn.execute(memberships.insert().values(user_id=1, group_id=10, role='member'))

    with pytest.raises(UMAValidationError, match='Composite primary key'):
        await uma.delete('memberships', 1)

    with pytest.raises(UMAValidationError, match='requires exactly 2 values'):
        await uma.delete('memberships', (1,))

    deleted = await uma.delete('memberships', (1, 10))
    assert deleted is True
