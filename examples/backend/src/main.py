"""FastAPI application entry point."""

from contextlib import asynccontextmanager
from urllib.parse import urlparse

import structlog
import uvicorn
from fastapi import FastAPI
from fastapi import Request
from fastapi.middleware.cors import CORSMiddleware
from fastapi.responses import JSONResponse
from sqlalchemy import text

from namerec.uma import JSQLExecutionError
from namerec.uma import JSQLSyntaxError
from namerec.uma import UMAAccessDeniedError
from namerec.uma import UMAError
from namerec.uma import UMANotFoundError
from namerec.uma import UMAValidationError
from src.config import settings
from src.dependencies import container
from src.exceptions import handle_uma_exception
from src.logging_config import configure_logging
from src.routers import uma

# Configure logging
configure_logging(settings.log_level)
logger = structlog.get_logger()


@asynccontextmanager
async def lifespan(app: FastAPI):  # noqa: ARG001
    """
    Manage application lifecycle.

    Handles:
    - UMA initialization at startup
    - Database connection cleanup at shutdown
    """
    # Startup
    logger.info('Starting UMA backend demo', debug_mode=settings.debug_mode)

    # Log database URL (without password for security)
    parsed_url = urlparse(settings.database_url)
    db_url_safe = (
        f'{parsed_url.scheme}://{parsed_url.hostname}:'
        f'{parsed_url.port or "default"}/{parsed_url.path.lstrip("/")}'
    )
    logger.info('Database URL configured', database=db_url_safe)

    # Configure DI container - MUST be done before any engine access
    container.config.database_url.from_value(settings.database_url)

    # Verify configuration is set
    configured_url = container.config.database_url()
    if not configured_url:
        raise ValueError('Database URL is not configured')  # noqa: TRY003

    logger.info('Database URL verified in container', url_set=bool(configured_url))

    # Initialize UMA application
    # Note: Engine is created lazily when first accessed, after database_url is set
    # UMA instance creation will trigger engine creation through dependency injection
    try:
        container.uma_app()
        logger.info('UMA instance created successfully')

        # Verify engine was created correctly by accessing it
        test_engine = container.engine()
        logger.info('Engine verified', engine_type=type(test_engine).__name__, url_set=bool(str(test_engine.url)))

        # Test engine connection only in debug mode (to verify connection works)
        if settings.debug_mode:
            try:
                async with test_engine.connect() as conn:
                    result = await conn.execute(text('SELECT 1'))
                    value = result.scalar()
                    logger.info('Engine connection test successful', test_value=value)
            except Exception as conn_error:
                logger.exception(
                    'Engine connection test failed',
                    error=str(conn_error),
                    error_type=type(conn_error).__name__,
                )
                raise
    except Exception as e:
        logger.exception('Failed to initialize UMA', error=str(e), error_type=type(e).__name__)
        raise

    logger.info('UMA initialized successfully', namespace='main')

    yield

    # Shutdown
    logger.info('Shutting down UMA backend demo')

    # Dispose database engine
    await container.engine().dispose()

    logger.info('Database connections closed')


# Create FastAPI application
app = FastAPI(
    title='UMA Backend Demo',
    description='FastAPI backend demonstrating UMA (Unified Model Access) capabilities',
    version='0.1.0',
    lifespan=lifespan,
)

# CORS for browser-based clients
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.cors_allowed_origins,
    allow_credentials=True,
    allow_methods=['*'],
    allow_headers=['*'],
)

# Include routers
app.include_router(uma.router)


# Exception handlers
@app.exception_handler(UMAAccessDeniedError)
async def uma_access_denied_handler(request: Request, exc: UMAAccessDeniedError):  # noqa: ARG001
    """Handle UMA access denied errors."""
    error, status_code = handle_uma_exception(exc, settings.debug_mode)
    logger.warning(
        'Access denied',
        entity_name=getattr(exc, 'entity_name', None),
        operation=getattr(exc, 'operation', None),
    )
    return JSONResponse(status_code=status_code, content=error.model_dump())


@app.exception_handler(UMANotFoundError)
async def uma_not_found_handler(request: Request, exc: UMANotFoundError):  # noqa: ARG001
    """Handle UMA not found errors."""
    error, status_code = handle_uma_exception(exc, settings.debug_mode)
    logger.warning(
        'Entity or record not found',
        entity_name=getattr(exc, 'entity_name', None),
    )
    return JSONResponse(status_code=status_code, content=error.model_dump())


@app.exception_handler(JSQLSyntaxError)
async def jsql_syntax_handler(request: Request, exc: JSQLSyntaxError):  # noqa: ARG001
    """Handle JSQL syntax errors."""
    error, status_code = handle_uma_exception(exc, settings.debug_mode)
    logger.error(
        'JSQL syntax error',
        path=getattr(exc, 'path', None),
        message=str(exc),
    )
    return JSONResponse(status_code=status_code, content=error.model_dump())


@app.exception_handler(JSQLExecutionError)
async def jsql_execution_handler(request: Request, exc: JSQLExecutionError):  # noqa: ARG001
    """Handle JSQL execution errors."""
    error, status_code = handle_uma_exception(exc, settings.debug_mode)
    logger.error(
        'JSQL execution error',
        message=str(exc),
    )
    return JSONResponse(status_code=status_code, content=error.model_dump())


@app.exception_handler(UMAValidationError)
async def uma_validation_handler(request: Request, exc: UMAValidationError):  # noqa: ARG001
    """Handle UMA validation errors."""
    error, status_code = handle_uma_exception(exc, settings.debug_mode)
    logger.error(
        'Validation error',
        field_name=getattr(exc, 'field_name', None),
        message=str(exc),
    )
    return JSONResponse(status_code=status_code, content=error.model_dump())


@app.exception_handler(UMAError)
async def uma_error_handler(request: Request, exc: UMAError):  # noqa: ARG001
    """Handle generic UMA errors."""
    error, status_code = handle_uma_exception(exc, settings.debug_mode)
    logger.error('UMA error', message=str(exc))
    return JSONResponse(status_code=status_code, content=error.model_dump())


@app.exception_handler(Exception)
async def generic_exception_handler(request: Request, exc: Exception):  # noqa: ARG001
    """Handle unexpected exceptions."""
    error, status_code = handle_uma_exception(exc, settings.debug_mode)
    logger.exception('Unexpected error', exc_info=exc)
    return JSONResponse(status_code=status_code, content=error.model_dump())


# Health check endpoint
@app.get('/health')
async def health_check() -> dict:
    """Health check endpoint."""
    return {'status': 'ok', 'service': 'uma-backend-demo'}


def run() -> None:
    """Run development server with default settings."""
    uvicorn.run(
        'src.main:app',
        host='0.0.0.0',  # noqa: S104
        port=8000,
        reload=True,
        log_config=None,  # Use our custom logging configuration
    )


if __name__ == '__main__':
    run()
