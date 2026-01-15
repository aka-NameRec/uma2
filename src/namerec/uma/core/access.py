"""Access control helper functions."""

from typing import Any

from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.types import MetadataProvider
from namerec.uma.core.types import Operation


def check_access(
    metadata_provider: MetadataProvider,
    entity_name: str,
    operation: Operation | str,
    user_context: Any,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        metadata_provider: Metadata provider with can() method
        entity_name: Entity name (empty string for "list entities" operation)
        operation: Operation name
        user_context: User context for access control

    Raises:
        UMAAccessDeniedError: If access denied
    """
    if isinstance(operation, str):
        operation = Operation(operation)

    # Check access using metadata provider's can() method
    if not metadata_provider.can(entity_name, operation, user_context):
        raise UMAAccessDeniedError(entity_name, operation.value)
