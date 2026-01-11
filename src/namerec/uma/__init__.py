"""
UMA - Unified Model Access

A Python package for unified database access via JSON-SQL queries.
"""

from namerec.uma.api import get_registry
from namerec.uma.api import initialize_uma
from namerec.uma.api import uma_delete
from namerec.uma.api import uma_initialize
from namerec.uma.api import uma_entity_details
from namerec.uma.api import uma_entity_list
from namerec.uma.api import uma_read
from namerec.uma.api import uma_save
from namerec.uma.api import uma_select
from namerec.uma.core.context import UMAContext
from namerec.uma.core.exceptions import UMAAccessDeniedError
from namerec.uma.core.exceptions import UMAError
from namerec.uma.core.exceptions import UMANotFoundError
from namerec.uma.core.exceptions import UMANotImplementedError
from namerec.uma.core.exceptions import UMAValidationError
from namerec.uma.core.namespace_config import NamespaceConfig
from namerec.uma.core.types import EntityHandler
from namerec.uma.core.types import EntityName
from namerec.uma.core.types import MetadataProvider
from namerec.uma.core.types import Operation
from namerec.uma.core.utils import copy_field_meta
from namerec.uma.core.utils import form_entity_name
from namerec.uma.core.utils import is_virtual_view
from namerec.uma.core.utils import parse_entity_name
from namerec.uma.handlers.base import DefaultEntityHandler
from namerec.uma.handlers.virtual import VirtualViewHandler
from namerec.uma.jsql.exceptions import JSQLExecutionError
from namerec.uma.jsql.exceptions import JSQLSyntaxError
from namerec.uma.metadata import DefaultMetadataProvider
from namerec.uma.registry import EntityRegistry
from namerec.uma.registry import get_global_registry
from namerec.uma.registry import init_global_registry

__version__ = '2.0'

__all__ = [
    # Core types
    'EntityName',
    'Operation',
    'EntityHandler',
    'MetadataProvider',
    'UMAContext',
    'NamespaceConfig',
    # Exceptions
    'UMAError',
    'UMAAccessDeniedError',
    'UMANotFoundError',
    'UMANotImplementedError',
    'UMAValidationError',
    'JSQLSyntaxError',
    'JSQLExecutionError',
    # Utils
    'parse_entity_name',
    'form_entity_name',
    'copy_field_meta',
    'is_virtual_view',
    # Handlers
    'DefaultEntityHandler',
    'VirtualViewHandler',
    # Registry
    'EntityRegistry',
    'init_global_registry',
    'get_global_registry',
    # Metadata
    'DefaultMetadataProvider',
    # API
    'uma_initialize',
    'initialize_uma',  # Legacy alias
    'get_registry',
    'uma_select',
    'uma_read',
    'uma_save',
    'uma_delete',
    'uma_entity_details',
    'uma_entity_list',
]
