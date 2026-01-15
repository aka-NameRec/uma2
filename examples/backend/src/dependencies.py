"""Dependency injection container and providers."""

from dependency_injector import containers
from dependency_injector import providers
from sqlalchemy.ext.asyncio import create_async_engine

from namerec.uma import DefaultMetadataProvider
from namerec.uma import NamespaceConfig
from namerec.uma import UMA


class Container(containers.DeclarativeContainer):
    """Application DI container."""

    # Configuration
    config = providers.Configuration()

    # Database engine (async)
    # Singleton ensures one engine instance per application
    # Engine is created lazily when first accessed, after database_url is set
    engine = providers.Singleton(
        create_async_engine,
        config.database_url,
        echo=False,
    )

    # UMA metadata provider
    metadata_provider = providers.Singleton(DefaultMetadataProvider)

    # UMA application instance (initialized once at startup)
    # Cache enabled - queries are compiled WITHOUT literal_binds=True
    # SQL with parameter placeholders is cached and reused with different parameter values
    # Cache key depends only on JSQL structure and user context, not parameter values
    uma_app = providers.Singleton(
        lambda engine, provider: UMA.create(
            {
                'main': NamespaceConfig(
                    engine=engine,
                    metadata_provider=provider,
                ),
            },
            # Cache stores SQL with parameter placeholders for parameterized queries
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


def get_uma() -> UMA:
    """
    FastAPI dependency to get UMA instance.

    Returns:
        UMA application instance
    """
    return container.uma_app()
