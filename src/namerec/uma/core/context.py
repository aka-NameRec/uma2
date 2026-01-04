"""UMA context for operation execution."""

from dataclasses import dataclass
from dataclasses import field
from typing import Any

from sqlalchemy import Engine
from sqlalchemy import MetaData


@dataclass
class UMAContext:
    """
    Context for UMA operation execution.
    Contains all necessary dependencies and state.
    """

    engine: Engine
    metadata: MetaData
    metadata_provider: Any = None  # MetadataProvider, but avoid circular import
    user_context: Any = None  # User-defined context for access control
    cache: Any = None  # Optional cache implementation
    extra: dict[str, Any] = field(default_factory=dict)  # Extensibility point

    def __post_init__(self) -> None:
        """Validate context after initialization."""
        if self.engine is None:
            msg = 'engine is required'
            raise ValueError(msg)
        if self.metadata is None:
            msg = 'metadata is required'
            raise ValueError(msg)
