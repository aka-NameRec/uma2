"""Dependency injection container and providers."""

from dependency_injector import containers
from dependency_injector import providers
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import uma_initialize


class Container(containers.DeclarativeContainer):
    """Application DI container."""

    # Configuration
    config = providers.Configuration()

    # Database engine (async)
    engine = providers.Singleton(
        create_async_engine,
        config.database_url,
        echo=False,
    )

    # UMA metadata provider
    metadata_provider = providers.Singleton(DefaultMetadataProvider)

    # UMA registry (initialized once at startup)
    registry = providers.Singleton(
        lambda engine, provider: uma_initialize(
            {
                'main': NamespaceConfig(
                    engine=engine,
                    metadata_provider=provider,
                ),
            }
        ),
        engine=engine,
        provider=metadata_provider,
    )

    # User context stub (for demo purposes)
    user_context = providers.Factory(lambda: {'user_id': 1, 'role': 'admin'})


# Global container instance
container = Container()


def get_user_context() -> dict:
    """
    FastAPI dependency to get user context.

    Returns:
        User context dictionary (stub for demo)
    """
    return container.user_context()
