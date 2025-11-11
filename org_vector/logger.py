import logging
import sys
import os
from pathlib import Path
from typing import Optional

def get_log_level_from_env() -> int:
    """Get log level from environment variable or return default ERROR level."""
    level_str = os.getenv('VECTOR_ORG_LOG_LEVEL', 'ERROR').upper()
    level_mapping = {
        'DEBUG': logging.DEBUG,
        'INFO': logging.INFO,
        'WARNING': logging.WARNING,
        'WARN': logging.WARNING,
        'ERROR': logging.ERROR,
        'CRITICAL': logging.CRITICAL,
        'FATAL': logging.CRITICAL
    }
    return level_mapping.get(level_str, logging.ERROR)

def should_log_to_file() -> bool:
    """Check if file logging is enabled via environment variable."""
    return os.getenv('VECTOR_ORG_LOG_TO_FILE', 'false').lower() in ('true', '1', 'yes', 'on')

def get_log_file_path() -> Path:
    """Get log file path from environment or use default."""
    log_dir_str = os.getenv('VECTOR_ORG_LOG_DIR', '')
    if log_dir_str:
        log_dir = Path(log_dir_str)
    else:
        log_dir = Path(__file__).resolve().parent / "logs"
    
    log_dir.mkdir(exist_ok=True)
    return log_dir / "vectored_notes.log"

def get_logger(name: Optional[str] = None, level: Optional[int] = None) -> logging.Logger:
    """
    Return a configured logger for the project.
    
    Configuration via environment variables:
    - VECTOR_ORG_LOG_LEVEL: DEBUG, INFO, WARNING, ERROR, CRITICAL (default: ERROR)
    - VECTOR_ORG_LOG_TO_FILE: true/false (default: false)
    - VECTOR_ORG_LOG_DIR: custom log directory path (optional)
    
    Args:
        name: Logger name (defaults to caller's module name)
        level: Override log level (optional)
    """
    logger_name = name or __name__
    logger = logging.getLogger(logger_name)
    
    if logger.handlers:
        return logger  # already configured

    # Use provided level or get from environment (default: ERROR)
    log_level = level if level is not None else get_log_level_from_env()
    logger.setLevel(log_level)

    # Log format
    fmt = logging.Formatter(
        "%(asctime)s [%(levelname)s] %(name)s: %(message)s",
        datefmt="%Y-%m-%d %H:%M:%S"
    )

    # Console handler - always enabled for errors and above
    console = logging.StreamHandler(sys.stderr)
    console.setFormatter(fmt)
    logger.addHandler(console)

    # File handler - only if enabled
    if should_log_to_file():
        try:
            log_file = get_log_file_path()
            file_handler = logging.FileHandler(log_file, mode="a")
            file_handler.setFormatter(fmt)
            logger.addHandler(file_handler)
        except Exception as e:
            # If file logging fails, at least log the error to console
            logger.error(f"Failed to setup file logging: {e}")

    return logger

def configure_logging(level: Optional[str] = None, log_to_file: Optional[bool] = None, log_dir: Optional[str] = None):
    """
    Programmatically configure logging settings.
    
    Args:
        level: Log level (DEBUG, INFO, WARNING, ERROR, CRITICAL)
        log_to_file: Enable/disable file logging
        log_dir: Custom log directory path
    """
    if level:
        os.environ['VECTOR_ORG_LOG_LEVEL'] = level.upper()
    if log_to_file is not None:
        os.environ['VECTOR_ORG_LOG_TO_FILE'] = str(log_to_file).lower()
    if log_dir:
        os.environ['VECTOR_ORG_LOG_DIR'] = log_dir