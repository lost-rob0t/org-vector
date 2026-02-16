import os
import signal
import time
from dataclasses import dataclass
from typing import Any, Dict, Optional, Set

from org_vector.embeddings import VectorClient
from org_vector.logger import get_logger
from org_vector.parse_org_files import OrgRoam
from org_vector.service_config import ServiceConfig


def _log():
    return get_logger(__name__)


@dataclass
class _SyncWindow:
    pending: bool = False
    deadline: Optional[float] = None


class BackgroundIndexerService:
    def __init__(self, config: ServiceConfig):
        self.config = config
        self.roam_dir = os.path.expanduser(config.roam_dir)
        self.roam = OrgRoam(self.roam_dir)
        self.vector_client = VectorClient(
            db_path=config.path,
            model_name=config.model,
            api_url=config.api_url,
            collection_name=config.collection_name,
            ingestion_instructions=config.ingestion_instructions,
            query_instructions=config.query_instructions,
        )

        self._inotify: Any = None
        self._flags: Any = None
        self._watch_mask = 0
        self._wd_to_dir: Dict[int, str] = {}
        self._dir_to_wd: Dict[str, int] = {}
        self._sync_window = _SyncWindow()
        self._stop_requested = False
        self._stop_signal_count = 0

    def _load_inotify(self):
        try:
            from inotify_simple import INotify, flags  # type: ignore[import-not-found]
        except ModuleNotFoundError as error:
            raise RuntimeError(
                "inotify support requires the 'inotify-simple' package. "
                "Install it with pip or run from a Nix environment that includes it."
            ) from error
        return INotify, flags

    def _build_watch_mask(self):
        return (
            self._flags.CLOSE_WRITE
            | self._flags.CREATE
            | self._flags.DELETE
            | self._flags.MOVED_FROM
            | self._flags.MOVED_TO
            | self._flags.ATTRIB
            | self._flags.DELETE_SELF
            | self._flags.MOVE_SELF
        )

    def _mark_stop(self, signum, frame):
        del frame
        self._stop_signal_count += 1
        if self._stop_signal_count == 1:
            _log().info(f"Received signal {signum}; shutting down service")
        else:
            _log().warning(f"Received signal {signum} again; forcing immediate exit")

        self._stop_requested = True
        if self._inotify is not None:
            try:
                self._inotify.close()
            except Exception:
                pass

        if self._stop_signal_count > 1:
            raise SystemExit(130)

    def _schedule_sync(self) -> None:
        self._sync_window.pending = True
        self._sync_window.deadline = time.monotonic() + self.config.debounce_seconds

    def _is_relevant_event(self, event_flags: Set, file_name: str) -> bool:
        if file_name.endswith(".org"):
            return True

        if self._flags.ISDIR in event_flags:
            if (
                self._flags.CREATE in event_flags
                or self._flags.DELETE in event_flags
                or self._flags.MOVED_FROM in event_flags
                or self._flags.MOVED_TO in event_flags
            ):
                return True

        if self._flags.DELETE_SELF in event_flags or self._flags.MOVE_SELF in event_flags:
            return True

        return False

    def _add_watch(self, directory: str) -> None:
        if directory in self._dir_to_wd:
            return
        if not os.path.isdir(directory):
            return

        wd = self._inotify.add_watch(directory, self._watch_mask)
        self._wd_to_dir[wd] = directory
        self._dir_to_wd[directory] = wd

    def _remove_stale_watches(self) -> None:
        stale_directories = [directory for directory in self._dir_to_wd if not os.path.isdir(directory)]
        for directory in stale_directories:
            wd = self._dir_to_wd.pop(directory)
            self._wd_to_dir.pop(wd, None)
            try:
                self._inotify.rm_watch(wd)
            except OSError:
                pass

    def _refresh_watches(self) -> None:
        if not os.path.isdir(self.roam_dir):
            raise FileNotFoundError(f"Watch directory does not exist: {self.roam_dir}")

        self._remove_stale_watches()

        for directory, _, _ in os.walk(self.roam_dir):
            self._add_watch(directory)

    def _sync_once(self) -> None:
        file_paths = self.roam.get_files(get_full=True)
        sync_plan = self.vector_client.plan_sync(file_paths)

        files_to_index = sync_plan["to_index"]
        parsed_files = self.roam.parse_files(file_paths=files_to_index) if files_to_index else []
        sync_stats = self.vector_client.sync_files(
            parsed_files,
            removed_files=sync_plan["to_remove"],
        )

        parse_failed = len(files_to_index) - len(parsed_files)
        _log().info(
            "Sync complete: indexed=%s unchanged=%s removed=%s parse_failed=%s embed_failed=%s",
            sync_stats["indexed"],
            len(sync_plan["unchanged"]),
            sync_stats["removed"],
            parse_failed,
            sync_stats["failed"],
        )

    def _run_sync_safely(self) -> None:
        try:
            self._sync_once()
        except Exception as error:
            _log().error(f"Background sync failed: {error}")

    def run(self) -> None:
        if not os.path.isdir(self.roam_dir):
            raise FileNotFoundError(f"Watch directory does not exist: {self.roam_dir}")

        INotify, flags = self._load_inotify()
        self._inotify = INotify()
        self._flags = flags
        self._watch_mask = self._build_watch_mask()

        signal.signal(signal.SIGINT, self._mark_stop)
        signal.signal(signal.SIGTERM, self._mark_stop)
        if hasattr(signal, "SIGQUIT"):
            signal.signal(signal.SIGQUIT, self._mark_stop)

        self._run_sync_safely()
        self._refresh_watches()
        _log().info(
            "Watching %s for org updates (debounce %.2fs)",
            self.roam_dir,
            self.config.debounce_seconds,
        )

        try:
            while not self._stop_requested:
                timeout_ms = self.config.poll_timeout_ms
                if self._sync_window.pending and self._sync_window.deadline is not None:
                    remaining_ms = int(max((self._sync_window.deadline - time.monotonic()) * 1000, 0))
                    timeout_ms = min(timeout_ms, remaining_ms)

                try:
                    events = self._inotify.read(timeout=timeout_ms)
                except (InterruptedError, ValueError, OSError):
                    continue
                if events:
                    for event in events:
                        event_flags = set(self._flags.from_mask(event.mask))
                        watched_dir = self._wd_to_dir.get(event.wd, self.roam_dir)
                        file_name = event.name or ""
                        file_path = os.path.join(watched_dir, file_name) if file_name else watched_dir

                        if self._is_relevant_event(event_flags, file_path):
                            self._schedule_sync()

                if self._sync_window.pending and self._sync_window.deadline is not None:
                    if time.monotonic() >= self._sync_window.deadline:
                        self._refresh_watches()
                        self._run_sync_safely()
                        self._sync_window = _SyncWindow()
        finally:
            try:
                self._inotify.close()
            except Exception:
                pass


def run_background_service(config: ServiceConfig) -> None:
    service = BackgroundIndexerService(config)
    service.run()
