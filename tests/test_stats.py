"""Offline tests for stats mode, --json summaries, and telemetry suppression.

Runs with stdlib unittest (also collected by pytest):

    nix develop -c python test_context_fix.py
"""

import io
import json
import logging
import os
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parent))
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from fakes import FakeChromaClient, FakeCollection, make_vector_client  # noqa: E402

from org_vector.embeddings import collection_stats  # noqa: E402

STATS_JSON_KEYS = {
    "db_path",
    "collection",
    "model",
    "total_chunks",
    "source_files",
    "last_embed",
    "db_size_bytes",
    "db_exists",
}

EMBED_JSON_KEYS = {
    "files_indexed",
    "files_skipped",
    "files_removed",
    "duration_seconds",
    "model",
    "db_path",
}


def write_org(directory: str, name: str, content: str) -> str:
    path = os.path.join(directory, name)
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(content)
    return path


SAMPLE_ORG = """#+TITLE: Sample

* Heading One
Body one.

* Heading Two
Body two.
"""


def make_seeded_collection(files: int = 2, chunks_per_file: int = 2) -> FakeCollection:
    collection = FakeCollection(
        metadata={
            "embedding_dimension": 384,
            "embedding_model": "all-MiniLM-L6-v2",
        }
    )
    for file_index in range(files):
        for chunk_index in range(chunks_per_file):
            entry_id = f"file{file_index}-chunk{chunk_index}"
            collection.entries[entry_id] = {
                "embedding": [0.0],
                "document": f"doc {entry_id}",
                "metadata": {"filepath": f"/notes/file{file_index}.org"},
            }
    return collection


class CliOutputMixin:
    def _run_cli(self, argv):
        from org_vector.cli import run

        buffer = io.StringIO()
        with redirect_stdout(buffer):
            exit_code = run(list(argv))
        self.assertEqual(exit_code, 0)
        return buffer.getvalue()


class CollectionStatsTests(unittest.TestCase):
    """Function-level stats backed by the fake chroma surfaces."""

    def test_counts_chunks_and_files(self):
        collection = make_seeded_collection(files=2, chunks_per_file=3)
        with tempfile.TemporaryDirectory() as db_path:
            stats = collection_stats(
                db_path=db_path,
                collection_name="org-roam",
                chroma_client=FakeChromaClient(collection),
            )

        self.assertEqual(stats["total_chunks"], 6)
        self.assertEqual(stats["source_files"], 2)
        self.assertEqual(stats["collection"], "org-roam")
        self.assertEqual(stats["model"], "all-MiniLM-L6-v2")
        self.assertTrue(stats["db_exists"])
        self.assertGreaterEqual(stats["db_size_bytes"], 0)
        self.assertIsNone(stats["last_embed"])

    def test_reports_last_embed_from_collection_metadata(self):
        collection = make_seeded_collection()
        collection.metadata["last_embed"] = "2026-02-15T10:00:00+00:00"
        with tempfile.TemporaryDirectory() as db_path:
            stats = collection_stats(
                db_path=db_path,
                collection_name="org-roam",
                chroma_client=FakeChromaClient(collection),
            )

        self.assertEqual(stats["last_embed"], "2026-02-15T10:00:00+00:00")

    def test_missing_db_directory_degrades_gracefully(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            missing = os.path.join(tmpdir, "does-not-exist")
            stats = collection_stats(db_path=missing, collection_name="org-roam")

        self.assertEqual(set(stats), STATS_JSON_KEYS)
        self.assertEqual(stats["total_chunks"], 0)
        self.assertEqual(stats["source_files"], 0)
        self.assertEqual(stats["db_size_bytes"], 0)
        self.assertFalse(stats["db_exists"])
        self.assertIsNone(stats["last_embed"])

    def test_missing_collection_degrades_gracefully(self):
        class MissingCollectionClient:
            def get_collection(self, name=None, **kwargs):
                raise KeyError(name)

        with tempfile.TemporaryDirectory() as db_path:
            stats = collection_stats(
                db_path=db_path,
                collection_name="org-roam",
                chroma_client=MissingCollectionClient(),
            )

        self.assertEqual(stats["total_chunks"], 0)
        self.assertEqual(stats["source_files"], 0)
        self.assertTrue(stats["db_exists"])


class StatsCliTests(CliOutputMixin, unittest.TestCase):
    """stats mode through the CLI entrypoint."""

    def test_human_output_includes_collection_and_chunk_count(self):
        collection = make_seeded_collection(files=3, chunks_per_file=2)
        with tempfile.TemporaryDirectory() as db_path:
            with mock.patch(
                "org_vector.embeddings.PersistentClient",
                return_value=FakeChromaClient(collection),
            ):
                output = self._run_cli(["stats", "--path", db_path])

        self.assertIn("org-roam", output)
        self.assertIn("Chunks: 6", output)
        self.assertIn("Files: 3", output)

    def test_json_output_has_documented_keys(self):
        collection = make_seeded_collection(files=2, chunks_per_file=2)
        with tempfile.TemporaryDirectory() as db_path:
            with mock.patch(
                "org_vector.embeddings.PersistentClient",
                return_value=FakeChromaClient(collection),
            ):
                output = self._run_cli(["stats", "--path", db_path, "--json"])

        payload = json.loads(output)
        self.assertEqual(set(payload), STATS_JSON_KEYS)
        self.assertEqual(payload["total_chunks"], 4)
        self.assertEqual(payload["source_files"], 2)
        self.assertEqual(payload["collection"], "org-roam")
        self.assertEqual(payload["model"], "all-MiniLM-L6-v2")
        self.assertIsInstance(payload["db_size_bytes"], int)
        self.assertTrue(payload["db_exists"])

    def test_missing_db_via_cli_degrades_gracefully(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            missing = os.path.join(tmpdir, "does-not-exist")
            output = self._run_cli(["stats", "--path", missing, "--json"])

        payload = json.loads(output)
        self.assertEqual(set(payload), STATS_JSON_KEYS)
        self.assertEqual(payload["total_chunks"], 0)
        self.assertFalse(payload["db_exists"])

    def test_missing_db_via_cli_human_does_not_crash(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            missing = os.path.join(tmpdir, "does-not-exist")
            output = self._run_cli(["stats", "--path", missing])

        self.assertIn("Chunks: 0", output)


class EmbedJsonSummaryTests(CliOutputMixin, unittest.TestCase):
    """embed/update --json summaries via a stub VectorClient."""

    class StubVectorClient:
        def __init__(self, **kwargs):
            self.model_name = kwargs["model_name"]
            self.db_path = kwargs["db_path"]

        def plan_sync(self, file_paths, remove_missing=True):
            return {
                "to_index": list(file_paths),
                "unchanged": ["/notes/unchanged.org"],
                "to_remove": ["/notes/gone.org"],
            }

        def sync_files(self, files, removed_files=None):
            return {"indexed": len(files), "failed": 0, "removed": 1}

    def _embed(self, roam_dir: str, db_path: str, extra):
        from org_vector import embeddings

        argv = ["update", "--dir", roam_dir, "--path", db_path] + list(extra)
        with mock.patch.object(embeddings, "VectorClient", self.StubVectorClient):
            return self._run_cli(argv)

    def test_json_summary_has_documented_keys(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            write_org(tmpdir, "sample.org", SAMPLE_ORG)
            roam_dir = tmpdir
            db_path = os.path.join(tmpdir, "db")

            output = self._embed(roam_dir, db_path, ["--json"])

        payload = json.loads(output)
        self.assertEqual(set(payload), EMBED_JSON_KEYS)
        self.assertEqual(payload["files_indexed"], 1)
        self.assertEqual(payload["files_skipped"], 1)
        self.assertEqual(payload["files_removed"], 1)
        self.assertIsInstance(payload["duration_seconds"], float)
        self.assertGreaterEqual(payload["duration_seconds"], 0.0)
        self.assertEqual(payload["model"], "all-MiniLM-L6-v2")
        self.assertEqual(payload["db_path"], db_path)

    def test_human_output_unchanged_without_json(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            write_org(tmpdir, "sample.org", SAMPLE_ORG)
            db_path = os.path.join(tmpdir, "db")

            output = self._embed(tmpdir, db_path, [])

        self.assertIn("Embed sync complete:", output)
        self.assertNotIn("files_indexed", output)
        with self.assertRaises(json.JSONDecodeError):
            json.loads(output)


class LastEmbedPersistenceTests(unittest.TestCase):
    """sync_files records a last-embed timestamp for stats."""

    def test_sync_files_records_timestamp(self):
        collection = FakeCollection(
            metadata={
                "embedding_dimension": 384,
                "embedding_model": "all-MiniLM-L6-v2",
            }
        )
        client = make_vector_client(collection)
        self.assertIsNone(collection.metadata.get("last_embed"))

        client.sync_files([])

        self.assertIn("last_embed", collection.metadata)

    def test_stats_surfaces_recorded_timestamp(self):
        collection = FakeCollection(
            metadata={
                "embedding_dimension": 384,
                "embedding_model": "all-MiniLM-L6-v2",
            }
        )
        client = make_vector_client(collection)
        client.sync_files([])

        with tempfile.TemporaryDirectory() as db_path:
            stats = collection_stats(
                db_path=db_path,
                collection_name="org-roam",
                chroma_client=FakeChromaClient(collection),
            )

        self.assertIsNotNone(stats["last_embed"])


class TelemetrySuppressionTests(unittest.TestCase):
    """The suppression seam must be applied at import/startup."""

    def test_env_switches_set_at_import(self):
        import org_vector.embeddings as embeddings

        self.assertEqual(os.environ.get("ANONYMIZED_TELEMETRY"), "False")
        self.assertEqual(
            os.environ.get("CHROMA_TELEMETRY_IMPL"),
            "chromadb.telemetry.product.null.NullTelemetry",
        )
        self.assertTrue(callable(embeddings.suppress_chromadb_telemetry))

    def test_suppress_noops_capture_and_silences_loggers(self):
        import posthog

        import org_vector.embeddings as embeddings

        embeddings.suppress_chromadb_telemetry()

        self.assertIsNone(posthog.capture("ClientStartEvent", {}))
        for name in ("posthog", "chromadb.telemetry"):
            noisy = logging.getLogger(name)
            self.assertEqual(noisy.level, logging.CRITICAL)
            self.assertFalse(noisy.propagate)


if __name__ == "__main__":
    unittest.main(verbosity=2)
