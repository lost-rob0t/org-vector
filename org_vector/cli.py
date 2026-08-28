import argparse
import json
import os
import sys
from typing import List, Optional

import org_vector.embeddings as e
import org_vector.parse_org_files as o
from org_vector.background_service import run_background_service
from org_vector.logger import configure_logging
from org_vector.service_config import (
    DEFAULT_CONFIG_PATH,
    ServiceConfig,
    load_service_config,
)

DEFAULT_MODEL = "all-MiniLM-L6-v2"
DEFAULT_COLLECTION = "org-roam"
DEFAULT_RESULTS = 5

MODE_ALIASES = {
    "query": "search",
    "update": "embed",
}

QUERY_MODES = {"search", "emacs", "json"}


def normalize_mode(mode: str) -> str:
    """Map legacy mode aliases onto canonical modes."""
    return MODE_ALIASES.get(mode, mode)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="org-vector",
        description="A simple vector store util for org-roam",
    )
    parser.add_argument(
        "mode",
        choices=["embed", "update", "search", "query", "emacs", "json", "serve"],
        help="Mode: embed/update sync, search/query retrieval, "
        "or serve for inotify background indexing.",
    )
    parser.add_argument("--dir", "-d", help="Org roam directory")
    parser.add_argument("--model", "-m", help="embeddings model")
    parser.add_argument("--path", "-p", help="Path to store embeddings in")
    parser.add_argument("--query", "-q", help="Search query text")
    parser.add_argument(
        "--results",
        "-k",
        type=int,
        default=DEFAULT_RESULTS,
        help="Number of results to return for search/emacs/json modes (default: 5)",
    )
    parser.add_argument(
        "--ingestion-instructions",
        help="Optional instruction prefix/template for document embeddings",
    )
    parser.add_argument(
        "--query-instructions",
        help="Optional instruction prefix/template for query embeddings",
    )
    parser.add_argument("--collection", "-c", help="collection name for database")
    parser.add_argument(
        "--config",
        default=DEFAULT_CONFIG_PATH,
        help="Path to service config TOML (serve mode)",
    )
    parser.add_argument(
        "--debounce-seconds",
        type=float,
        help="Debounce window before syncing after file events (serve mode)",
    )
    parser.add_argument(
        "--poll-timeout-ms",
        type=int,
        help="Inotify poll timeout in milliseconds (serve mode)",
    )
    parser.add_argument(
        "--log-level",
        choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
        help="Set logging level (default: ERROR)",
    )
    parser.add_argument(
        "--log-to-file", action="store_true", help="Enable logging to file"
    )
    parser.add_argument("--log-dir", help="Directory for log files")
    return parser


def _validate_args(parser: argparse.ArgumentParser, args: argparse.Namespace) -> None:
    if args.results < 1:
        parser.error("--results must be >= 1")
    if args.mode == "embed" and not args.dir:
        parser.error("--dir is required for embed mode")
    if args.mode in QUERY_MODES and not args.query:
        parser.error("--query is required for search/query output modes")


def main(
    mode: str,
    query: str,
    roam_dir: str,
    path: str,
    model: str,
    collection_name: str = DEFAULT_COLLECTION,
    ingestion_instructions: Optional[str] = None,
    query_instructions: Optional[str] = None,
    results: int = DEFAULT_RESULTS,
) -> None:
    mode = normalize_mode(mode)
    vector_client = e.VectorClient(
        model_name=model,
        db_path=path,
        collection_name=collection_name,
        ingestion_instructions=ingestion_instructions,
        query_instructions=query_instructions,
    )

    if mode == "embed":
        roam = o.OrgRoam(roam_dir)
        file_paths = roam.get_files(get_full=True)
        sync_plan = vector_client.plan_sync(file_paths)

        files_to_index = sync_plan["to_index"]
        files = roam.parse_files(file_paths=files_to_index) if files_to_index else []
        sync_stats = vector_client.sync_files(
            files, removed_files=sync_plan["to_remove"]
        )

        parse_failed = len(files_to_index) - len(files)
        print(
            "Embed sync complete: "
            f"indexed={sync_stats['indexed']} "
            f"unchanged={len(sync_plan['unchanged'])} "
            f"removed={sync_stats['removed']} "
            f"parse_failed={parse_failed} "
            f"embed_failed={sync_stats['failed']}"
        )

    elif mode == "search":
        for result in vector_client.query(query, k=results):
            print(f"{result.metadata['filepath']}:\n{result.page_content}")

    elif mode == "emacs":
        for result in vector_client.query(query, k=results):
            title = result.metadata.get("title", "Result")
            filepath = result.metadata["filepath"]
            print(f"* Result [[file://{filepath}][{title}]]:\n{result.page_content}")

    elif mode == "json":
        for result in vector_client.query(query, k=results):
            print(json.dumps(result.model_dump()))


def _resolve_service_config(args: argparse.Namespace) -> ServiceConfig:
    config = load_service_config(args.config)

    roam_dir = os.path.expanduser(args.dir) if args.dir else config.roam_dir
    path = os.path.expanduser(args.path) if args.path else config.path
    model = args.model or config.model
    collection_name = args.collection or config.collection_name
    ingestion_instructions = (
        args.ingestion_instructions
        if args.ingestion_instructions is not None
        else config.ingestion_instructions
    )
    query_instructions = (
        args.query_instructions
        if args.query_instructions is not None
        else config.query_instructions
    )
    debounce_seconds = (
        args.debounce_seconds
        if args.debounce_seconds is not None
        else config.debounce_seconds
    )
    poll_timeout_ms = (
        args.poll_timeout_ms
        if args.poll_timeout_ms is not None
        else config.poll_timeout_ms
    )

    return ServiceConfig(
        roam_dir=roam_dir,
        path=path,
        model=model,
        collection_name=collection_name,
        ingestion_instructions=ingestion_instructions,
        query_instructions=query_instructions,
        debounce_seconds=debounce_seconds,
        poll_timeout_ms=poll_timeout_ms,
        log_level=args.log_level or config.log_level,
        log_to_file=True if args.log_to_file else config.log_to_file,
        log_dir=os.path.expanduser(args.log_dir) if args.log_dir else config.log_dir,
    )


def run(argv: Optional[List[str]] = None) -> int:
    default_path = os.path.expanduser("~/.cache/vector-org/")

    parser = build_parser()
    args = parser.parse_args(argv)
    _validate_args(parser, args)

    mode = normalize_mode(args.mode)

    configure_logging(
        level=args.log_level,
        log_to_file=args.log_to_file or None,
        log_dir=args.log_dir,
    )

    if mode == "serve":
        try:
            service_config = _resolve_service_config(args)
        except (FileNotFoundError, ValueError, RuntimeError) as error:
            parser.error(str(error))

        try:
            run_background_service(service_config)
        except (FileNotFoundError, RuntimeError, ValueError) as error:
            parser.error(str(error))
        return 0

    main(
        mode=mode,
        query=args.query or "",
        roam_dir=os.path.expanduser(args.dir or ""),
        path=os.path.expanduser(args.path or default_path),
        model=args.model or DEFAULT_MODEL,
        collection_name=args.collection or DEFAULT_COLLECTION,
        ingestion_instructions=args.ingestion_instructions,
        query_instructions=args.query_instructions,
        results=args.results,
    )
    return 0


if __name__ == "__main__":
    sys.exit(run())
