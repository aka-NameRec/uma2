"""Cache key generation and cached query structure."""

import hashlib
import json
from dataclasses import dataclass
from dataclasses import field
from typing import Any

from namerec.uma.jsql.types import ColumnMetadata


def make_cache_key(
    jsql: dict[str, Any],
    user_context: Any = None,
    params: dict[str, Any] | None = None,  # Deprecated: not used in cache key
) -> str:
    """
    Create cache key for JSQL query.

    Includes user context hash to handle different permissions per user.
    Does NOT include parameter values - same JSQL structure uses same cached SQL.
    Parameter values are passed at execution time, allowing one SQL to be reused.
    Namespace is already in jsql dict (in FROM clause), so no need to pass separately.

    Args:
        jsql: JSQL query dictionary (contains namespace in FROM clause)
        user_context: User context (affects permissions/query results)
        params: Query parameters (deprecated - kept for backward compatibility, not used in cache key)

    Returns:
        Cache key (compact hash)

    Example:
        >>> jsql = {
        ...     "from": "users",
        ...     "select": ["id", "name"],
        ...     "where": {
        ...         "op": ">=",
        ...         "left": {"field": "created"},
        ...         "right": {"param": "date"},
        ...     },
        ... }
        >>> key1 = make_cache_key(jsql, {"role": "admin"})
        >>> key2 = make_cache_key(jsql, {"role": "user"})
        >>> key1 != key2  # Different users = different cache keys
        True
        >>> key3 = make_cache_key(jsql, None)
        >>> key4 = make_cache_key(jsql, None)  # Same JSQL structure
        >>> key3 == key4  # Same cache key - SQL will be reused with different param values
        True
    """
    _ = params
    # Stable JSON representation (includes namespace from FROM clause)
    jsql_str = json.dumps(jsql, sort_keys=True, ensure_ascii=False)

    # Include user context in key (for permission-based queries)
    if user_context:
        context_str = json.dumps(
            _extract_cache_key_context(user_context),
            sort_keys=True,
            ensure_ascii=False,
        )
    else:
        context_str = ''

    # Note: params are NOT included in cache key
    # Same JSQL structure (with same parameter names) should use same cached SQL
    # Parameter values are passed at execution time, not at cache key generation time
    # This allows caching SQL with placeholders and reusing it with different parameter values

    # Combine all components (without params)
    cache_input = f'{jsql_str}|{context_str}'

    # Use BLAKE2b for fast, secure hashing (16 bytes = 32 hex chars)
    return hashlib.blake2b(cache_input.encode(), digest_size=16).hexdigest()


def _extract_cache_key_context(user_context: Any) -> dict[str, Any]:
    """
    Extract cache-relevant fields from user context.

    Only include fields that affect query results (role, permissions),
    exclude session-specific data (session_id, last_login, etc.)

    Args:
        user_context: User context object/dict

    Returns:
        Dictionary with cache-relevant fields
    """
    if isinstance(user_context, dict):
        # Extract only permission-relevant fields
        return {
            'user_id': user_context.get('user_id'),
            'role': user_context.get('role'),
            'permissions': user_context.get('permissions'),
            'groups': user_context.get('groups'),
            # Exclude: session_id, last_login, ip_address, etc.
        }

    # For custom objects, try to extract standard attributes
    result = {}
    for attr in ('user_id', 'role', 'permissions', 'groups'):
        if hasattr(user_context, attr):
            result[attr] = getattr(user_context, attr)

    return result


@dataclass
class CachedQuery:
    """
    Cached query structure for Redis/external storage.

    Stores compiled SQL with parameter placeholders,
    not SQLAlchemy AST (which can't be serialized to Redis).
    """

    sql: str  # Compiled SQL with parameter placeholders
    params_mapping: dict[str, str]  # JSQL param names -> SQL param names
    dialect: str  # SQL dialect (postgresql, mysql, etc.)
    literal_params: dict[str, Any] = field(default_factory=dict)  # SQLAlchemy-generated param names -> literal values
    entities: list[str] = field(default_factory=list)  # Entities in SELECT clause
    meta: list[ColumnMetadata] = field(default_factory=list)  # Column metadata for cached results
    param_order: list[str] | None = None  # Parameter order for positional params (SQLite uses ?)
    param_types: dict[str, str] = field(default_factory=dict)  # JSQL param name -> type key
    debug_sql: str | None = None  # Formatted SQL for debug output (optional)

    def to_dict(self) -> dict[str, Any]:
        """Serialize for Redis storage."""
        return {
            'sql': self.sql,
            'params_mapping': self.params_mapping,
            'literal_params': self.literal_params,
            'param_order': self.param_order,
            'dialect': self.dialect,
            'entities': self.entities,
            'meta': [col.to_dict() for col in self.meta],
            'param_types': self.param_types,
            'debug_sql': self.debug_sql,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> 'CachedQuery':
        """Deserialize from Redis."""
        return cls(
            sql=data['sql'],
            params_mapping=data['params_mapping'],
            literal_params=data.get('literal_params', {}),
            param_order=data.get('param_order'),
            dialect=data['dialect'],
            entities=data.get('entities', []),
            meta=[ColumnMetadata(**col) for col in data.get('meta', [])],
            param_types=data.get('param_types', {}),
            debug_sql=data.get('debug_sql'),
        )
