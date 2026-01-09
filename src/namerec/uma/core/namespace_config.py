"""Namespace configuration and management for UMA."""

from collections.abc import Mapping
from dataclasses import dataclass

from sqlalchemy import Engine

from namerec.uma.core.types import MetadataProvider


@dataclass(frozen=True)
class NamespaceConfig:
    """
    Configuration for a single namespace.

    A namespace represents a logical group of entities, typically corresponding
    to a database or schema. Each namespace has its own engine and metadata provider.
    """

    engine: Engine
    metadata_provider: MetadataProvider


# Global state (initialized by uma_initialize)
_namespace_configs: Mapping[str, NamespaceConfig] | None = None
_default_namespace: str | None = None


def set_namespace_configs(
    configs: Mapping[str, NamespaceConfig],
    default_namespace: str | None = None,
) -> None:
    """
    Set namespace configurations.

    Called by uma_initialize(). Not intended for direct use.

    Args:
        configs: Mapping of namespace names to configurations
        default_namespace: Default namespace (None if multiple namespaces without default)
    """
    global _namespace_configs, _default_namespace  # noqa: PLW0603
    _namespace_configs = configs
    _default_namespace = default_namespace


def get_namespace_config(namespace: str | None = None) -> NamespaceConfig:
    """
    Get configuration for a namespace.

    Args:
        namespace: Namespace name (None = use default)

    Returns:
        NamespaceConfig for the requested namespace

    Raises:
        RuntimeError: If UMA not initialized
        ValueError: If namespace not found or default not set when required
    """
    if _namespace_configs is None:
        msg = 'UMA not initialized. Call uma_initialize() first.'
        raise RuntimeError(msg)

    # Resolve namespace
    if namespace is None:
        if _default_namespace is None:
            msg = (
                'No default namespace configured. '
                'Explicit namespace required when multiple namespaces are configured.'
            )
            raise ValueError(msg)
        namespace = _default_namespace

    if namespace not in _namespace_configs:
        available = ', '.join(_namespace_configs.keys())
        msg = f'Namespace "{namespace}" not found. Available namespaces: {available}'
        raise ValueError(msg)

    return _namespace_configs[namespace]


def get_default_namespace() -> str | None:
    """
    Get default namespace name.

    Returns:
        Default namespace name, or None if not configured
    """
    return _default_namespace


def get_all_namespaces() -> list[str]:
    """
    Get list of all configured namespace names.

    Returns:
        List of namespace names (empty if not initialized)
    """
    if _namespace_configs is None:
        return []
    return list(_namespace_configs.keys())
