"""Access control helper functions."""

from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.types import MetadataProvider
from namerec.uma.core.types import UMAContextSpec


async def check_access(
    metadata_provider: MetadataProvider,
    entity_name: str,
    operation: str,
    context: UMAContextSpec,
) -> None:
    """
    Check access and raise exception if access denied.

    Args:
        metadata_provider: Metadata provider with can() method
        entity_name: Entity name (empty string for "list entities" operation)
        operation: Operation name
        context: Execution context for access control

    Raises:
        UMAAccessDeniedError: If access denied
    """
    # Check access using metadata provider's can() method
    if not await metadata_provider.can(entity_name, operation, context):
        raise UMAAccessDeniedError(entity_name, operation)
