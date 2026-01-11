"""Logging configuration with structlog and standard logging integration."""

import logging
import sys

import structlog


def configure_logging(log_level: str) -> None:
    """
    Configure structlog with standard logging integration.

    This ensures that:
    1. structlog is used for structured logging
    2. Standard logging (used by uvicorn, fastapi) uses the same format
    3. All logs are output to stdout with consistent formatting

    Args:
        log_level: Logging level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
    """
    # Convert string level to logging constant
    numeric_level = getattr(logging, log_level.upper(), logging.INFO)

    # Configure standard logging first
    logging.basicConfig(
        format='%(message)s',
        stream=sys.stdout,
        level=numeric_level,
    )

    # Configure structlog
    structlog.configure(
        processors=[
            structlog.contextvars.merge_contextvars,  # Merge context variables
            structlog.processors.add_log_level,  # Add log level
            structlog.processors.TimeStamper(fmt='iso', utc=False),  # ISO timestamp, local time
            structlog.stdlib.ProcessorFormatter.wrap_for_formatter,
        ],
        wrapper_class=structlog.stdlib.BoundLogger,
        logger_factory=structlog.stdlib.LoggerFactory(),
        cache_logger_on_first_use=True,
    )

    # Custom formatter for text output (ts log_level: message format)
    formatter = structlog.stdlib.ProcessorFormatter(
        processor=structlog.dev.ConsoleRenderer(),  # Text renderer
        foreign_pre_chain=[
            structlog.stdlib.add_log_level,
            structlog.processors.TimeStamper(fmt='iso', utc=False),
        ],
    )

    # Apply formatter to all loggers
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(formatter)

    # Configure root logger
    root_logger = logging.getLogger()
    root_logger.handlers = [handler]
    root_logger.setLevel(numeric_level)

    # Configure uvicorn and fastapi loggers to use the same format
    for logger_name in ['uvicorn', 'uvicorn.access', 'uvicorn.error', 'fastapi']:
        logger = logging.getLogger(logger_name)
        logger.handlers = [handler]
        logger.setLevel(numeric_level)
        logger.propagate = False
