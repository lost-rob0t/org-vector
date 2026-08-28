"""Offline regression + behavior tests for org_vector.

Runs with stdlib unittest only (no pytest, no model downloads):

    nix develop -c python -m unittest discover -s tests -v
"""

import logging
import os
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from fakes import FakeCollection, FakeModel, make_vector_client  # noqa: E402

from org_vector.logger import configure_logging, get_logger  # noqa: E402
from org_vector.parse_org_files import OrgRoam  # noqa: E402

NESTED_ORG = """#+TITLE: Test File

* Top Level Heading
This is the body of the top level heading.

** Second Level Heading
This is content under the second level.

*** Third Level Heading
This is deeply nested content that should be included.

** Another Second Level
More content here.

* Another Top Level
Final content.
"""


def write_org(directory: str, name: str, content: str) -> str:
    path = os.path.join(directory, name)
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(content)
    return path


class NestedContentRegression(unittest.TestCase):
    """Ported from the original test_context_fix.py (A8)."""

    def test_nested_content_embedding(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            write_org(tmpdir, "test.org", NESTED_ORG)
            org_file = OrgRoam(tmpdir).parse_files()[0]
            nodes = org_file.get_all_nodes()
            self.assertEqual(len(nodes), 5)
            self.assertGreaterEqual(max(node.level for node in nodes), 3)

            client = make_vector_client()
            docs = client.make_document(org_file)
            self.assertEqual(len(docs), len(nodes))
            for doc in docs:
                self.assertTrue(doc.page_content.strip())
                self.assertIn("filepath", doc.metadata)
                self.assertIn("storage_id", doc.metadata)


class FallbackIdTests(unittest.TestCase):
    """A4: ID-less nodes must get deterministic storage ids."""

    def test_fallback_ids_deterministic(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            write_org(tmpdir, "no_ids.org", NESTED_ORG)
            first = OrgRoam(tmpdir).parse_files()[0]
            second = OrgRoam(tmpdir).parse_files()[0]

            client = make_vector_client()
            ids_first = [doc.metadata["storage_id"] for doc in client.make_document(first)]
            ids_second = [doc.metadata["storage_id"] for doc in client.make_document(second)]

            self.assertTrue(all(sid.startswith("auto-") for sid in ids_first))
            self.assertEqual(ids_first, ids_second)

    def test_explicit_ids_win(self):
        org = """#+TITLE: With IDs
* Tagged
:PROPERTIES:
:ID: abcd-1234
:END:
body
"""
        with tempfile.TemporaryDirectory() as tmpdir:
            write_org(tmpdir, "ids.org", org)
            org_file = OrgRoam(tmpdir).parse_files()[0]
            client = make_vector_client()
            docs = client.make_document(org_file)
            self.assertEqual(docs[0].metadata["storage_id"], "abcd-1234")


class InstructionTests(unittest.TestCase):
    """A5/A6: instruction prefixes for ingestion vs query."""

    def test_query_uses_query_instructions(self):
        model = FakeModel()
        client = make_vector_client(
            model=model,
            ingestion_instructions="DOC-PREFIX:",
            query_instructions="QUERY-PREFIX:",
        )
        client.query("hello world")
        self.assertEqual(len(model.encoded), 1)
        self.assertTrue(model.encoded[0].startswith("QUERY-PREFIX:"))

    def test_embed_uses_ingestion_instructions(self):
        model = FakeModel()
        client = make_vector_client(
            model=model,
            ingestion_instructions="DOC-PREFIX:",
            query_instructions="QUERY-PREFIX:",
        )
        with tempfile.TemporaryDirectory() as tmpdir:
            write_org(tmpdir, "x.org", NESTED_ORG)
            org_file = OrgRoam(tmpdir).parse_files()[0]
            client.embed_file(org_file)
        self.assertTrue(all(text.startswith("DOC-PREFIX:") for text in model.encoded))

    def test_default_instructions_by_model(self):
        from org_vector.embeddings import VectorClient

        nomic = VectorClient._default_instructions("nomic-embed-text:v1.5")
        self.assertEqual(nomic, ("search_document: ", "search_query: "))

        e5 = VectorClient._default_instructions("intfloat/e5-base-v2")
        self.assertEqual(e5, ("passage: ", "query: "))

        fallback = VectorClient._default_instructions("all-MiniLM-L6-v2")
        self.assertIn("org", fallback[0].lower())
        self.assertIn("org", fallback[1].lower())

    def test_template_instruction(self):
        from org_vector.embeddings import VectorClient

        rendered = VectorClient._apply_instruction("Wrap: {text}", "payload")
        self.assertEqual(rendered, "Wrap: payload")


class CliTests(unittest.TestCase):
    """A2: -k/--results flag and mode aliases."""

    def _parser(self):
        from org_vector.cli import build_parser

        return build_parser()

    def test_cli_accepts_results_flag(self):
        args = self._parser().parse_args(["json", "-q", "x", "-k", "3"])
        self.assertEqual(args.results, 3)

    def test_results_default_is_five(self):
        args = self._parser().parse_args(["search", "-q", "x"])
        self.assertEqual(args.results, 5)

    def test_alias_modes(self):
        from org_vector.cli import normalize_mode

        self.assertEqual(normalize_mode("query"), "search")
        self.assertEqual(normalize_mode("update"), "embed")
        self.assertEqual(normalize_mode("emacs"), "emacs")

    def test_embed_requires_dir(self):
        from org_vector.cli import _validate_args

        parser = self._parser()
        args = parser.parse_args(["embed"])
        with self.assertRaises(SystemExit):
            _validate_args(parser, args)

    def test_search_requires_query(self):
        from org_vector.cli import _validate_args

        parser = self._parser()
        args = parser.parse_args(["search"])
        with self.assertRaises(SystemExit):
            _validate_args(parser, args)

    def test_results_must_be_positive(self):
        from org_vector.cli import _validate_args

        parser = self._parser()
        args = parser.parse_args(["search", "-q", "x", "-k", "0"])
        with self.assertRaises(SystemExit):
            _validate_args(parser, args)


class LoggerTests(unittest.TestCase):
    """A7: configure_logging must affect already-created loggers."""

    def test_configure_logging_reconfigures(self):
        logger = get_logger("org_vector.test_reconfigure")
        self.assertEqual(logger.level, logging.ERROR)

        configure_logging(level="INFO", log_to_file=False)
        logger = get_logger("org_vector.test_reconfigure")
        self.assertEqual(logger.level, logging.INFO)

        configure_logging(level="ERROR", log_to_file=False)
        self.assertEqual(get_logger("org_vector.test_reconfigure").level, logging.ERROR)


class PlanSyncTests(unittest.TestCase):
    """A9: incremental sync planning on (mtime_ns, size)."""

    def test_plan_sync_states(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            unchanged_path = write_org(tmpdir, "unchanged.org", NESTED_ORG)
            changed_path = write_org(tmpdir, "changed.org", NESTED_ORG)

            stat = os.stat(unchanged_path)
            collection = FakeCollection()
            collection.entries["seed"] = {
                "embedding": [0.0],
                "document": "doc",
                "metadata": {
                    "filepath": unchanged_path,
                    "source_mtime_ns": str(stat.st_mtime_ns),
                    "source_size": int(stat.st_size),
                },
            }
            collection.entries["ghost"] = {
                "embedding": [0.0],
                "document": "doc",
                "metadata": {"filepath": "/nope/gone.org"},
            }

            client = make_vector_client(collection)
            plan = client.plan_sync([unchanged_path, changed_path])

            self.assertEqual(plan["unchanged"], [unchanged_path])
            self.assertEqual(plan["to_index"], [changed_path])
            self.assertEqual(plan["to_remove"], ["/nope/gone.org"])


class DiversifyTests(unittest.TestCase):
    """A10: at most one top result per file before backfill."""

    def test_diversify_caps_per_file(self):
        from langchain_core.documents import Document

        client = make_vector_client()
        docs = [
            Document(page_content=f"d{i}", metadata={"filepath": "f1.org" if i < 2 else "f2.org", "path": f"p{i}"})
            for i in range(4)
        ]
        picked = client._diversify_documents(docs, 3)
        self.assertEqual(len(picked), 3)
        self.assertEqual(picked[0].metadata["filepath"], "f1.org")
        self.assertEqual(picked[1].metadata["filepath"], "f2.org")


if __name__ == "__main__":
    unittest.main(verbosity=2)
