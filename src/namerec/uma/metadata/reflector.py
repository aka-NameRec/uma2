"""Database schema reflector."""

from sqlalchemy import Engine
from sqlalchemy import MetaData


class DatabaseReflector:
    """
    Reflects database schema using SQLAlchemy.

    Handles both async and sync engines, performing reflection
    with optional schema support (e.g., PostgreSQL schemas).

    Attributes:
        _schema: Optional schema name for reflection
    """

    def __init__(self, schema: str | None = None) -> None:
        """
        Initialize reflector.

        Args:
            schema: Optional schema name (e.g., 'public' for PostgreSQL)
        """
        self._schema = schema

    @property
    def schema(self) -> str | None:
        """Get configured schema name."""
        return self._schema

    async def reflect(self, engine: Engine) -> MetaData:
        """
        Reflect database schema.

        Automatically detects async vs sync engine and performs
        appropriate reflection.

        Args:
            engine: SQLAlchemy Engine (async or sync)

        Returns:
            MetaData with reflected tables

        Raises:
            RuntimeError: If reflection fails
        """
        try:
            metadata = MetaData()

            # Detect async/sync engine via dialect
            if hasattr(engine.dialect, 'is_async') and engine.dialect.is_async:
                # Async engine - use begin() to ensure proper transaction context
                # This ensures we're in the correct async context for asyncpg
                engine_url_str = str(engine.url)
                engine_url_safe = engine_url_str.split('@')[-1] if '@' in engine_url_str else '***'

                # Try to connect and reflect using begin() for proper async context
                try:
                    # Use begin() to ensure proper async context and transaction handling
                    async with engine.begin() as conn:
                        # Use run_sync to run synchronous reflect in async context
                        # This properly handles the event loop for asyncpg
                        await conn.run_sync(
                            lambda sync_conn: metadata.reflect(bind=sync_conn, schema=self._schema)
                        )
                except Exception as conn_error:
                    # More detailed error information
                    error_msg = (
                        f'Connection failed during reflection. '
                        f'Engine URL: {engine_url_safe}, '
                        f'Error type: {type(conn_error).__name__}, '
                        f'Error: {conn_error}'
                    )
                    raise RuntimeError(error_msg) from conn_error
            else:
                # Sync engine - direct reflection
                metadata.reflect(bind=engine, schema=self._schema)

            return metadata

        except RuntimeError:
            # Re-raise RuntimeError as-is (already formatted)
            raise
        except Exception as e:
            msg = 'Failed to reflect database schema'
            if self._schema:
                msg += f' (schema="{self._schema}")'
            msg += f': {e}'
            raise RuntimeError(msg) from e
