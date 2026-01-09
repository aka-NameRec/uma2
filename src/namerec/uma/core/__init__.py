"""Core UMA components."""

from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.exceptions import UMAError
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.exceptions import UMANotImplementedError
from namerec.uma.core.exceptions import UMAValidationError
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import MetadataProvider
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import copy_field_meta
from namerec.uma.core.utils import form_entity_name
from namerec.uma.core.utils import is_virtual_view
from namerec.uma.core.utils import parse_entity_name

__all__ = [
    'EntityHandler',
    'EntityName',
    'MetadataProvider',
    'Operation',
    'UMAAccessDeniedError',
    'UMAContext',
    'UMAError',
    'UMANotFoundError',
    'UMANotImplementedError',
    'UMAValidationError',
    'copy_field_meta',
    'form_entity_name',
    'is_virtual_view',
    'parse_entity_name',
]
