#!/usr/bin/env python
import json
import argparse
import os
from typing import Optional

import org_vector.embeddings as e
import org_vector.parse_org_files as o
from org_vector.background_service import run_background_service
from org_vector.logger import configure_logging
from org_vector.service_config import DEFAULT_CONFIG_PATH, ServiceConfig, load_service_config

def main(
    mode: str,
    query: str,
    roam_dir: str,
    path: str,
    model: str,
    collection_name: str = "org-roam",
    ingestion_instructions: Optional[str] = None,
    query_instructions: Optional[str] = None,
):
    vector_client = e.VectorClient(
        model_name=model,
        db_path=path,
        collection_name=collection_name,
        ingestion_instructions=ingestion_instructions,
        query_instructions=query_instructions,
    )

    if mode == "query":
        mode = "search"
    if mode == "update":
        mode = "embed"

    if mode == "embed":
        roam = o.OrgRoam(roam_dir)
        file_paths = roam.get_files(get_full=True)
        sync_plan = vector_client.plan_sync(file_paths)

        files_to_index = sync_plan["to_index"]
        files = roam.parse_files(file_paths=files_to_index) if files_to_index else []
        sync_stats = vector_client.sync_files(files, removed_files=sync_plan["to_remove"])

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
        resp = vector_client.query(query)
        for r in resp:
            print(f"{r.metadata['filepath']}:\n{r.page_content}")

    elif mode == "emacs":
        resp = vector_client.query(query)
        for r in resp:
            print(f"* Result [[file://{r.metadata['filepath']}][{r.metadata.get('title', 'Result')}]]:\n{r.page_content}")

    elif mode == "json":
        resp = vector_client.query(query)
        for r in resp:
            print(json.dumps(r.model_dump()))


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

    log_level = args.log_level or config.log_level
    log_to_file = True if args.log_to_file else config.log_to_file
    log_dir = os.path.expanduser(args.log_dir) if args.log_dir else config.log_dir

    return ServiceConfig(
        roam_dir=roam_dir,
        path=path,
        model=model,
        collection_name=collection_name,
        ingestion_instructions=ingestion_instructions,
        query_instructions=query_instructions,
        debounce_seconds=debounce_seconds,
        poll_timeout_ms=poll_timeout_ms,
        log_level=log_level,
        log_to_file=log_to_file,
        log_dir=log_dir,
    )


if __name__ == '__main__':
    default_path = os.path.expanduser("~/.cache/vector-org/")

    parser = argparse.ArgumentParser(description="A simple vector store util for org-roam")
    parser.add_argument("mode", choices=["embed", "update", "search", "query", "emacs", "json", "serve"],
                        help="Mode: embed/update sync, search/query retrieval, or serve for inotify background indexing.")
    parser.add_argument("--dir", "-d", help="Org roam directory")
    parser.add_argument("--model", "-m", help="embeddings model")
    parser.add_argument("--path", "-p", help="Path to store embeddings in")
    parser.add_argument("--query", "-q", help="Search query text")
    parser.add_argument(
        "--ingestion-instructions",
        help="Optional instruction prefix/template for document embeddings",
    )
    parser.add_argument(
        "--query-instructions",
        help="Optional instruction prefix/template for query embeddings",
    )
    parser.add_argument("--collection",
                        "-c", help="collection name for database. Default is org-roam")
    parser.add_argument("--config", default=DEFAULT_CONFIG_PATH,
                        help="Path to service config TOML (serve mode)")
    parser.add_argument("--debounce-seconds", type=float,
                        help="Debounce window before syncing after file events (serve mode)")
    parser.add_argument("--poll-timeout-ms", type=int,
                        help="Inotify poll timeout in milliseconds (serve mode)")
    parser.add_argument("--log-level", choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
                        help="Set logging level (default: ERROR)")
    parser.add_argument("--log-to-file", action="store_true",
                        help="Enable logging to file")
    parser.add_argument("--log-dir", help="Directory for log files")

    args = parser.parse_args()

    if args.mode == "serve":
        try:
            service_config = _resolve_service_config(args)
        except (FileNotFoundError, ValueError, RuntimeError) as error:
            parser.error(str(error))

        configure_logging(
            level=service_config.log_level,
            log_to_file=service_config.log_to_file,
            log_dir=service_config.log_dir,
        )
        try:
            run_background_service(service_config)
        except (FileNotFoundError, RuntimeError, ValueError) as error:
            parser.error(str(error))
        raise SystemExit(0)

    configure_logging(
        level=args.log_level,
        log_to_file=args.log_to_file,
        log_dir=args.log_dir
    )

    if args.mode == "query":
        mode = "search"
    elif args.mode == "update":
        mode = "embed"
    else:
        mode = args.mode

    model = args.model or "all-MiniLM-L6-v2"
    path = os.path.expanduser(args.path or default_path)
    collection_name = args.collection or "org-roam"

    if mode == "embed" and not args.dir:
        parser.error("--dir is required for embed mode")
    if mode in {"search", "emacs", "json"} and not args.query:
        parser.error("--query is required for search/query output modes")

    main(mode=mode,
        roam_dir=os.path.expanduser(args.dir or ""),
        query=args.query or "",
        model=model,
        collection_name=collection_name,
        ingestion_instructions=args.ingestion_instructions,
        query_instructions=args.query_instructions,
        path=path
    )
