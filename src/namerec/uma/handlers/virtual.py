"""Virtual view handler base class."""

from typing import Any

from sqlalchemy import Select

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMANotImplementedError
from namerec.uma.core.types import EntityName


class VirtualViewHandler:
    """
    Base class for virtual views.
    Provides default implementations that raise UMANotImplementedError.
    Automatically adds is_virtual_view=True to metadata.
    """

    @classmethod
    async def select(
        cls,
        entity_name: EntityName,
        params: dict[str, Any],  # noqa: ARG003
        context: UMAContext,  # noqa: ARG003
    ) -> Select:
        """
        Must be overridden in subclass.

        Args:
            entity_name: Entity name
            params: Query parameters
            context: Execution context

        Raises:
            UMANotImplementedError: Always (must be overridden)
        """
        raise UMANotImplementedError(
            str(entity_name),
            'select',
            f'Virtual view {entity_name} must implement select()',
        )

    @classmethod
    async def read(
        cls,
        entity_name: EntityName,
        id_value: Any,  # noqa: ARG003
        context: UMAContext,  # noqa: ARG003
    ) -> dict:
        """
        Not supported by default for virtual views.
        Can be overridden in subclass if needed.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Raises:
            UMANotImplementedError: Always (unless overridden)
        """
        raise UMANotImplementedError(
            str(entity_name),
            'read',
            f'Virtual view {entity_name} does not support read operation',
        )

    @classmethod
    async def save(
        cls,
        entity_name: EntityName,
        data: dict,  # noqa: ARG003
        context: UMAContext,  # noqa: ARG003
    ) -> Any:
        """
        Virtual views are read-only by default.

        Args:
            entity_name: Entity name
            data: Record data
            context: Execution context

        Raises:
            UMANotImplementedError: Always (read-only)
        """
        raise UMANotImplementedError(
            str(entity_name),
            'write',
            f'Virtual view {entity_name} is read-only',
        )

    @classmethod
    async def delete(
        cls,
        entity_name: EntityName,
        id_value: Any,  # noqa: ARG003
        context: UMAContext,  # noqa: ARG003
    ) -> bool:
        """
        Virtual views are read-only by default.

        Args:
            entity_name: Entity name
            id_value: Record ID
            context: Execution context

        Raises:
            UMANotImplementedError: Always (read-only)
        """
        raise UMANotImplementedError(
            str(entity_name),
            'delete',
            f'Virtual view {entity_name} is read-only',
        )

    @classmethod
    async def meta(
        cls,
        entity_name: EntityName,
        context: UMAContext,  # noqa: ARG003
    ) -> dict:
        """
        Base metadata implementation.
        Automatically adds is_virtual_view=True.
        Should be extended in subclass to describe columns.

        Args:
            entity_name: Entity name
            context: Execution context

        Returns:
            Metadata dictionary
        """
        return {
            'name': str(entity_name),
            'description': getattr(cls, 'description', ''),
            'is_virtual_view': True,
            'columns': getattr(cls, 'columns_metadata', []),
        }
