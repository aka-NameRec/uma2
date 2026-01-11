"""Exception handlers for UMA errors."""

import traceback
from typing import Any

from namerec.uma import JSQLExecutionError
from namerec.uma import JSQLSyntaxError
from namerec.uma import UMAAccessDeniedError
from namerec.uma import UMAError
from namerec.uma import UMANotFoundError
from namerec.uma import UMAValidationError

from src.models.responses import ErrorResponse


def handle_uma_exception(exc: Exception, debug_mode: bool) -> tuple[ErrorResponse, int]:
    """
    Convert UMA exception to ErrorResponse with HTTP status code.

    Args:
        exc: Exception to handle
        debug_mode: If True, include detailed traceback in response

    Returns:
        Tuple of (ErrorResponse, HTTP status code)
    """
    error_id = exc.__class__.__name__
    message = str(exc)
    details: dict[str, Any] = {}

    # Determine HTTP status code based on exception type
    if isinstance(exc, UMAAccessDeniedError):
        status_code = 403
        details = {
            'entity_name': getattr(exc, 'entity_name', None),
            'operation': getattr(exc, 'operation', None),
        }
    elif isinstance(exc, UMANotFoundError):
        status_code = 404
        details = {
            'entity_name': getattr(exc, 'entity_name', None),
        }
    elif isinstance(exc, (JSQLSyntaxError, UMAValidationError)):
        status_code = 400
        if isinstance(exc, JSQLSyntaxError):
            details = {
                'path': getattr(exc, 'path', None),
            }
        elif isinstance(exc, UMAValidationError):
            details = {
                'field_name': getattr(exc, 'field_name', None),
                'entity_name': getattr(exc, 'entity_name', None),
            }
    elif isinstance(exc, (JSQLExecutionError, UMAError)):
        status_code = 500
        if isinstance(exc, JSQLExecutionError):
            details = {
                'query': getattr(exc, 'query', None),
            }
    else:
        # Unknown exception
        status_code = 500
        details = {
            'exception_type': error_id,
        }

    # Add debug information if enabled
    if debug_mode:
        details['traceback'] = traceback.format_exc()
        details['debug_mode'] = True

    error_response = ErrorResponse(
        id=error_id,
        message=message,
        details=details,
    )

    return error_response, status_code
