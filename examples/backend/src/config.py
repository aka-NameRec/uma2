"""Application configuration."""

from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    """Application settings loaded from environment variables."""

    database_url: str  # Required
    log_level: str = 'INFO'
    debug_mode: bool = False

    class Config:
        """Pydantic settings configuration."""

        env_file = '.env'
        case_sensitive = False


settings = Settings()
