"""Cache key generation and cached query structure."""

import hashlib
import json
from dataclasses import dataclass
from typing import Any


def make_cache_key(
    jsql: dict[str, Any],
    user_context: Any = None,
    namespace: str | None = None,
) -> str:
    """
    Create cache key for JSQL query.

    Includes user context hash to handle different permissions per user.
    Uses simple hashing without normalization for performance.

    Args:
        jsql: JSQL query dictionary
        user_context: User context (affects permissions/query results)
        namespace: Namespace (affects table resolution)

    Returns:
        Cache key (compact hash)

    Example:
        >>> jsql = {"from": "users", "select": ["id", "name"]}
        >>> key1 = make_cache_key(jsql, {"role": "admin"})
        >>> key2 = make_cache_key(jsql, {"role": "user"})
        >>> key1 != key2  # Different users = different cache keys
        True
    """
    # Stable JSON representation
    jsql_str = json.dumps(jsql, sort_keys=True, ensure_ascii=False)

    # Include user context in key (for permission-based queries)
    if user_context:
        context_str = json.dumps(
            _extract_cache_key_context(user_context),
            sort_keys=True,
            ensure_ascii=False,
        )
    else:
        context_str = ""

    # Include namespace
    namespace_str = namespace or ""

    # Combine all components
    cache_input = f"{jsql_str}|{context_str}|{namespace_str}"

    # Use BLAKE2b for fast, secure hashing (16 bytes = 32 hex chars)
    return hashlib.blake2b(cache_input.encode(), digest_size=16).hexdigest()


def _extract_cache_key_context(user_context: Any) -> dict:
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

    def to_dict(self) -> dict[str, Any]:
        """Serialize for Redis storage."""
        return {
            'sql': self.sql,
            'params_mapping': self.params_mapping,
            'dialect': self.dialect,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> 'CachedQuery':
        """Deserialize from Redis."""
        return cls(
            sql=data['sql'],
            params_mapping=data['params_mapping'],
            dialect=data['dialect'],
        )
