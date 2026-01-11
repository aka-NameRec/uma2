"""UMA context for operation execution."""

from __future__ import annotations

from dataclasses import dataclass
from dataclasses import field
from typing import TYPE_CHECKING
from typing import Any

from sqlalchemy import Engine
from sqlalchemy import MetaData

if TYPE_CHECKING:
    from namerec.uma.core.types import MetadataProvider


@dataclass
class UMAContext:
    """
    Context for UMA operation execution.

    Contains all necessary dependencies for a specific namespace.
    Metadata is loaded lazily through metadata_provider.
    """

    engine: Engine
    metadata_provider: 'MetadataProvider'
    namespace: str
    user_context: Any = None  # User-defined context for access control
    cache: Any = None  # Optional cache implementation
    extra: dict[str, Any] = field(default_factory=dict)  # Extensibility point
    _metadata: MetaData | None = None  # Cached metadata (internal)

    def __post_init__(self) -> None:
        """Validate context after initialization."""
        if self.engine is None:
            msg = 'engine is required'
            raise ValueError(msg)
        if self.metadata_provider is None:
            msg = 'metadata_provider is required'
            raise ValueError(msg)
        if not self.namespace:
            msg = 'namespace is required'
            raise ValueError(msg)

    @property
    def metadata(self) -> MetaData:
        """
        Get cached metadata for namespace.

        For JSQL parser compatibility - provides sync access to metadata.
        Metadata must be preloaded before accessing this property.

        Returns:
            SQLAlchemy MetaData object

        Raises:
            RuntimeError: If metadata not yet loaded
        """
        if self._metadata is None:
            msg = (
                f'Metadata for namespace "{self.namespace}" not yet loaded. '
                'Call await metadata_provider.preload() or access entities first.'
            )
            raise RuntimeError(msg)
        return self._metadata

    def _set_metadata(self, metadata: MetaData) -> None:
        """
        Set cached metadata (internal use only).

        Args:
            metadata: SQLAlchemy MetaData object
        """
        self._metadata = metadata