"""Default entity handler for database tables."""

from typing import Any

from sqlalchemy import Select
from sqlalchemy import and_
from sqlalchemy import delete as sql_delete
from sqlalchemy import insert
from sqlalchemy import select
from sqlalchemy import update

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.exceptions import UMAValidationError
from namerec.uma.core.types import EntityName
from namerec.uma.core.utils import get_table


class DefaultEntityHandler:
    """
    Default handler for regular database tables.
    Services most tables without specific customization.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,
    ) -> Select:
        """
        Simple SELECT * FROM table.

        Args:
            entity_name: Entity name
            params: Query parameters (unused in default implementation)
            context: Execution context

        Returns:
            SQLAlchemy Select object
        """
        table = await get_table(context, entity_name)
        return select(table)

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict[str, Any]:
        """
        Read through select + filter by id.
        Does not return None - raises UMANotFoundError.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Record data as dictionary

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Get select (can be overridden in subclass with policies)
        query = await cls.select(entity_name, {}, context)

        # Add condition by id
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)
        query = query.where(
            cls._build_pk_condition_from_id(pk_columns, id_value, entity_name)
        )

        # Execute
        async with context.engine.connect() as conn:
            result = await conn.execute(query)
            row = result.mappings().first()

            if row is None:
                raise UMANotFoundError(
                    str(entity_name),
                    f'Record with id={id_value} not found or access denied',
                )

            return dict(row)

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict[str, Any],
        context: UMAContext,
    ) -> Any:
        """
        Save to table.
        Duplicate access check removed - will be in _ensure_exists().

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Returns:
            ID of saved record
        """
        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)

        # Determine create vs update
        if len(pk_columns) == 1:
            id_value = data.get(pk_columns[0].name)
        else:
            id_value = tuple(data.get(col.name) for col in pk_columns)

        is_create = id_value is None or (isinstance(id_value, tuple) and None in id_value)

        async with context.engine.begin() as conn:
            if is_create:
                # INSERT
                stmt = insert(table).values(**data)
                result = await conn.execute(stmt)

                if len(pk_columns) == 1:
                    return result.inserted_primary_key[0]
                return tuple(result.inserted_primary_key)

            # UPDATE
            # Check access through read (DRY - avoid duplication)
            await cls._ensure_exists(entity_name, id_value, context)

            # Build condition by PK
            pk_condition = cls._build_pk_condition_from_id(
                pk_columns, id_value, entity_name
            )

            stmt = update(table).where(pk_condition).values(**data)
            await conn.execute(stmt)
            return id_value

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> bool:
        """
        Delete from table.
        Uses _ensure_exists() for access check (DRY).

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            True if deleted

        Raises:
            UMANotFoundError: If record not found or access denied
        """
        # Check access through read (DRY)
        await cls._ensure_exists(entity_name, id_value, context)

        table = await get_table(context, entity_name)
        pk_columns = list(table.primary_key.columns)
        pk_condition = cls._build_pk_condition_from_id(
            pk_columns, id_value, entity_name
        )

        async with context.engine.begin() as conn:
            stmt = sql_delete(table).where(pk_condition)
            result = await conn.execute(stmt)
            return result.rowcount > 0

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,
    ) -> dict[str, Any]:
        """
        Metadata from SQLAlchemy MetaData + enrichment from MetadataProvider.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        table = await get_table(context, entity_name)

        columns = []
        for col in table.columns:
            col_info = {
                'name': col.name,
                'type': str(col.type),
                'nullable': col.nullable,
                'primary_key': col.primary_key,
            }

            # Foreign keys
            if col.foreign_keys:
                fk = next(iter(col.foreign_keys))
                col_info['related_to'] = fk.column.table.name
                col_info['related_key'] = fk.column.name

            columns.append(col_info)

        # Construct name with namespace
        name = str(entity_name) if entity_name.namespace else f'{context.namespace}:{entity_name.entity}'

        metadata = {
            'name': name,
            'description': '',
            'columns': columns,
        }

        # Enrich from MetadataProvider
        if context.metadata_provider:
            provider_metadata = await context.metadata_provider.get_metadata(entity_name, context)
            if not isinstance(provider_metadata, dict):
                raise UMAValidationError(
                    message='Metadata provider must return a dictionary',
                    field_name='metadata',
                    entity_name=str(entity_name),
                )
            metadata.update(provider_metadata)

        return metadata

    # ========== Helper methods (DRY) ==========

    @classmethod
    async def _ensure_exists(
        cls,
        entity_name: EntityName,
        id_value: Any,
        context: UMAContext,
    ) -> dict[str, Any]:
        """
        Check record existence and access through read().
        Extracted to separate method to avoid duplication in save/delete.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Returns:
            Dictionary with record data

        Raises:
            UMANotFoundError: If record not found or no access
        """
        return await cls.read(entity_name, id_value, context)

    @staticmethod
    def _build_pk_condition(pk_columns: list[Any], id_value: Any) -> Any:
        """
        Build WHERE condition for primary key.
        Supports simple and composite keys.

        Args:
            pk_columns: List of PK columns
            id_value: ID value(s)

        Returns:
            SQLAlchemy condition
        """
        if len(pk_columns) == 1:
            return pk_columns[0] == id_value

        return and_(*[col == val for col, val in zip(pk_columns, id_value, strict=True)])

    @staticmethod
    def _normalize_pk_id_value(
        pk_columns: list[Any],
        id_value: Any,
        entity_name: EntityName,
    ) -> Any:
        """
        Normalize and validate id value against table primary key shape.

        Args:
            pk_columns: Primary key columns
            id_value: Input id value
            entity_name: Entity name for error context

        Returns:
            Scalar id for single-column PK, tuple for composite PK

        Raises:
            UMAValidationError: If id format does not match PK structure
        """
        if len(pk_columns) == 1:
            return id_value

        if not isinstance(id_value, (tuple, list)):
            raise UMAValidationError(
                message=(
                    f'Composite primary key for "{entity_name}" requires tuple/list '
                    f'with {len(pk_columns)} values'
                ),
                field_name='id_value',
                entity_name=str(entity_name),
            )

        if len(id_value) != len(pk_columns):
            raise UMAValidationError(
                message=(
                    f'Composite primary key for "{entity_name}" requires exactly '
                    f'{len(pk_columns)} values, got {len(id_value)}'
                ),
                field_name='id_value',
                entity_name=str(entity_name),
            )

        return tuple(id_value)

    @classmethod
    def _build_pk_condition_from_id(
        cls,
        pk_columns: list[Any],
        id_value: Any,
        entity_name: EntityName,
    ) -> Any:
        """
        Validate id value and build WHERE condition for primary key.

        Args:
            pk_columns: List of PK columns
            id_value: Input id value
            entity_name: Entity name for validation context

        Returns:
            SQLAlchemy condition for primary key lookup
        """
        normalized_id = cls._normalize_pk_id_value(pk_columns, id_value, entity_name)
        return cls._build_pk_condition(pk_columns, normalized_id)
