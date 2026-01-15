"""Namespace configuration for UMA."""

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
