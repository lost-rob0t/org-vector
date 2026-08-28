import logging
import os
import sys
from pathlib import Path
from typing import Optional

ENV_LOG_LEVEL = "VECTOR_ORG_LOG_LEVEL"
ENV_LOG_TO_FILE = "VECTOR_ORG_LOG_TO_FILE"
ENV_LOG_DIR = "VECTOR_ORG_LOG_DIR"

LOG_FORMAT = "%(asctime)s [%(levelname)s] %(name)s: %(message)s"
DATE_FORMAT = "%Y-%m-%d %H:%M:%S"

_LEVEL_MAP = {
    "DEBUG": logging.DEBUG,
    "INFO": logging.INFO,
    "WARNING": logging.WARNING,
    "WARN": logging.WARNING,
    "ERROR": logging.ERROR,
    "CRITICAL": logging.CRITICAL,
    "FATAL": logging.CRITICAL,
}


def get_log_level_from_env() -> int:
    """Log level from VECTOR_ORG_LOG_LEVEL, defaulting to ERROR."""
    level_str = os.getenv(ENV_LOG_LEVEL, "ERROR").upper()
    return _LEVEL_MAP.get(level_str, logging.ERROR)


def should_log_to_file() -> bool:
    """Whether VECTOR_ORG_LOG_TO_FILE enables file logging."""
    return os.getenv(ENV_LOG_TO_FILE, "false").lower() in ("true", "1", "yes", "on")


def get_log_file_path() -> Path:
    """Log file location from VECTOR_ORG_LOG_DIR, defaulting to the user cache."""
    log_dir_str = os.getenv(ENV_LOG_DIR, "")
    log_dir = Path(log_dir_str) if log_dir_str else Path("~/.cache/org-vector/logs").expanduser()
    log_dir.mkdir(parents=True, exist_ok=True)
    return log_dir / "vectored_notes.log"


_console_handler: Optional[logging.Handler] = None


def _get_console_handler() -> logging.Handler:
    global _console_handler
    if _console_handler is None:
        handler = logging.StreamHandler(sys.stderr)
        handler.setFormatter(logging.Formatter(LOG_FORMAT, datefmt=DATE_FORMAT))
        _console_handler = handler
    return _console_handler


def _attach_file_handler(logger: logging.Logger) -> None:
    if getattr(logger, "_org_vector_file_handler", None) is not None:
        return
    try:
        file_handler = logging.FileHandler(get_log_file_path(), mode="a")
        file_handler.setFormatter(logging.Formatter(LOG_FORMAT, datefmt=DATE_FORMAT))
        logger.addHandler(file_handler)
        logger._org_vector_file_handler = file_handler
    except Exception as error:
        logger.error("Failed to setup file logging: %s", error)


def _apply_config(logger: logging.Logger, level: Optional[int]) -> None:
    logger.setLevel(level if level is not None else get_log_level_from_env())
    if should_log_to_file():
        _attach_file_handler(logger)
    else:
        file_handler = getattr(logger, "_org_vector_file_handler", None)
        if file_handler is not None:
            logger.removeHandler(file_handler)
            file_handler.close()
            logger._org_vector_file_handler = None


def get_logger(name: Optional[str] = None, level: Optional[int] = None) -> logging.Logger:
    """Return a configured logger under the 'org_vector' namespace.

    Configuration via environment variables:
    - VECTOR_ORG_LOG_LEVEL: DEBUG, INFO, WARNING, ERROR, CRITICAL (default: ERROR)
    - VECTOR_ORG_LOG_TO_FILE: true/false (default: false)
    - VECTOR_ORG_LOG_DIR: custom log directory path (optional)
    """
    logger_name = name or "org_vector"
    if not logger_name.startswith("org_vector"):
        logger_name = f"org_vector.{logger_name}"

    logger = logging.getLogger(logger_name)
    if not logger.handlers:
        logger.addHandler(_get_console_handler())
        logger.propagate = False

    _apply_config(logger, level)
    return logger


def configure_logging(
    level: Optional[str] = None,
    log_to_file: Optional[bool] = None,
    log_dir: Optional[str] = None,
) -> None:
    """(Re)configure all org_vector loggers, including ones created earlier.

    Safe to call at any time: loggers created before this call are updated in
    place, so CLI flags take effect even though modules create loggers at
    import time.
    """
    if level:
        os.environ[ENV_LOG_LEVEL] = level.upper()
    if log_to_file is not None:
        os.environ[ENV_LOG_TO_FILE] = str(log_to_file).lower()
    if log_dir:
        os.environ[ENV_LOG_DIR] = log_dir

    resolved_level = _LEVEL_MAP.get(level.upper()) if level else None
    for logger_name in list(logging.root.manager.loggerDict):
        if logger_name.startswith("org_vector"):
            _apply_config(logging.getLogger(logger_name), resolved_level)
