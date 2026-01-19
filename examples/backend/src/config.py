"""Application configuration."""

from pathlib import Path

from pydantic_settings import BaseSettings

BASE_DIR = Path(__file__).resolve().parents[1]
ENV_FILE = BASE_DIR / '.env'
DEFAULT_CORS_ALLOWED_ORIGINS = ['*']


class Settings(BaseSettings):
    """Application settings loaded from environment variables."""

    database_url: str  # Required
    log_level: str = 'INFO'
    debug_mode: bool = False
    cors_allowed_origins: list[str] = DEFAULT_CORS_ALLOWED_ORIGINS

    class Config:
        """Pydantic settings configuration."""

        env_file = str(ENV_FILE)
        env_file_encoding = 'utf-8'
        case_sensitive = False


settings = Settings()
