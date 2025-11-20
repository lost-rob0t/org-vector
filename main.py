#!/usr/bin/env python
import argparse
import os
import json

import org_vector.embeddings as e
import org_vector.parse_org_files as o
from org_vector.logger import configure_logging

def main(mode: str, query: str, roam_dir: str, path: str, api_url: str, model: str, collection_name: str = "org-roam"):
    vector_client = e.VectorClient(api_url=api_url,
                                   model=model,
                                   db_path=path,
                                   collection_name=collection_name,
                                   )
 
    if mode == "embed":
        roam = o.OrgRoam(roam_dir)
        files = roam.parse_files()
        for org_file in files:
            vector_client.embed_file(org_file)
    
    if mode == "search":
        resp = vector_client.query(query)
        for r in resp:
            print(f"{r.metadata['filepath']}:\n{r.page_content}")

    if mode == "emacs":
        resp = vector_client.query(query)
        for r in resp:
            print(f"* Result [[file://{r.metadata['filepath']}][{r.metadata.get('title', 'Result')}]]:\n{r.page_content}")
    if mode == "json":
        resp = vector_client.query(query)
        for r in resp:
            print(json.dumps(r.model_dump()))


if __name__ == '__main__':
    default_path = os.path.expanduser("~/.cache/vector-org/")

    parser = argparse.ArgumentParser(description="A simple vector store util for org-roam")
    parser.add_argument("mode", choices=["embed", "search", "emacs", "json"],
                        help="Mode of operation: 'embed' to index files or 'search' to query them.")
    parser.add_argument("--dir", "-d", help="Org roam directory")
    parser.add_argument("--model", "-m", help="embeddings model (OLLAMA)", default="nomic-embed-text")
    parser.add_argument("--path", "-p", help="Path to store embeddings in", default=default_path)
    parser.add_argument("--query", "-q", help="Path to store embeddings in")
    parser.add_argument("--collection",
                        "-c", help="collection name for database. Default is org-roam",
                        default="org-roam")
    parser.add_argument("--url", "-u", help="Ollama API url endpoint.", default="http://localhost:11434")
    parser.add_argument("--log-level", choices=["DEBUG", "INFO", "WARNING", "ERROR", "CRITICAL"],
                        help="Set logging level (default: ERROR)")
    parser.add_argument("--log-to-file", action="store_true",
                        help="Enable logging to file")
    parser.add_argument("--log-dir", help="Directory for log files")

    args = parser.parse_args()

    configure_logging(
        level=args.log_level,
        log_to_file=args.log_to_file,
        log_dir=args.log_dir
    )

    main(mode=args.mode,
        roam_dir=os.path.expanduser(args.dir or ""),
        query=args.query or "",
        api_url=args.url,
        model=args.model,
        collection_name=args.collection,
        path=os.path.expanduser(args.path)
    )
