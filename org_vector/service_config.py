import os
from dataclasses import dataclass
from typing import Any, Dict, Optional

DEFAULT_CONFIG_PATH = "~/.config/org-vector/config.toml"


def _load_toml(path: str) -> Dict[str, Any]:
    try:
        import tomllib  # type: ignore[attr-defined]
    except ModuleNotFoundError:
        try:
            import tomli as tomllib  # type: ignore[no-redef]
        except ModuleNotFoundError as error:
            raise RuntimeError(
                "TOML parsing requires Python 3.11+ or the 'tomli' package on older Python versions"
            ) from error

    with open(path, "rb") as handle:
        return tomllib.load(handle)


def _optional_text(value: Any) -> Optional[str]:
    if value is None:
        return None

    text = str(value).strip()
    return text or None


def _optional_bool(value: Any) -> Optional[bool]:
    if value is None:
        return None
    if isinstance(value, bool):
        return value
    if isinstance(value, str):
        lowered = value.strip().lower()
        if lowered in {"1", "true", "yes", "on"}:
            return True
        if lowered in {"0", "false", "no", "off"}:
            return False

    raise ValueError(f"Expected a boolean value, got: {value!r}")


def _read_section(data: Dict[str, Any], key: str) -> Dict[str, Any]:
    section = data.get(key, {})
    if section is None:
        return {}
    if not isinstance(section, dict):
        raise ValueError(f"'{key}' section must be a TOML table")
    return section


def _read_float(config: Dict[str, Any], key: str, default: float) -> float:
    raw = config.get(key, default)
    try:
        value = float(raw)
    except (TypeError, ValueError) as error:
        raise ValueError(f"'{key}' must be a number") from error
    return value


def _read_int(config: Dict[str, Any], key: str, default: int) -> int:
    raw = config.get(key, default)
    try:
        value = int(raw)
    except (TypeError, ValueError) as error:
        raise ValueError(f"'{key}' must be an integer") from error
    return value


@dataclass
class ServiceConfig:
    roam_dir: str
    path: str = os.path.expanduser("~/.cache/vector-org/")
    model: str = "all-MiniLM-L6-v2"
    api_url: Optional[str] = None
    collection_name: str = "org-roam"
    ingestion_instructions: Optional[str] = None
    query_instructions: Optional[str] = None
    debounce_seconds: float = 1.0
    poll_timeout_ms: int = 500
    log_level: Optional[str] = None
    log_to_file: Optional[bool] = None
    log_dir: Optional[str] = None


def load_service_config(config_path: str = DEFAULT_CONFIG_PATH) -> ServiceConfig:
    expanded_path = os.path.expanduser(config_path)
    if not os.path.isfile(expanded_path):
        raise FileNotFoundError(
            f"Service config not found at {expanded_path}. "
            "Create ~/.config/org-vector/config.toml or pass --config."
        )

    data = _load_toml(expanded_path)
    service = _read_section(data, "service")
    logging_config = _read_section(data, "logging")

    if not service:
        service = data

    roam_dir_raw = service.get("dir") or service.get("roam_dir")
    roam_dir = _optional_text(roam_dir_raw)
    if not roam_dir:
        raise ValueError("Config is missing required key: 'dir' (or [service].dir)")

    path = os.path.expanduser(str(service.get("path") or "~/.cache/vector-org/"))
    model = str(service.get("model") or "all-MiniLM-L6-v2")
    api_url = _optional_text(service.get("url"))
    collection_name = str(service.get("collection") or "org-roam")
    ingestion_instructions = _optional_text(service.get("ingestion_instructions"))
    query_instructions = _optional_text(service.get("query_instructions"))
    debounce_seconds = _read_float(service, "debounce_seconds", 1.0)
    poll_timeout_ms = _read_int(service, "poll_timeout_ms", 500)

    if debounce_seconds < 0:
        raise ValueError("[service].debounce_seconds must be >= 0")
    if poll_timeout_ms <= 0:
        raise ValueError("[service].poll_timeout_ms must be > 0")

    log_level = _optional_text(logging_config.get("level"))
    log_to_file = _optional_bool(logging_config.get("to_file"))
    log_dir_raw = _optional_text(logging_config.get("dir"))
    log_dir = os.path.expanduser(log_dir_raw) if log_dir_raw else None

    return ServiceConfig(
        roam_dir=os.path.expanduser(roam_dir),
        path=path,
        model=model,
        api_url=api_url,
        collection_name=collection_name,
        ingestion_instructions=ingestion_instructions,
        query_instructions=query_instructions,
        debounce_seconds=debounce_seconds,
        poll_timeout_ms=poll_timeout_ms,
        log_level=log_level,
        log_to_file=log_to_file,
        log_dir=log_dir,
    )
