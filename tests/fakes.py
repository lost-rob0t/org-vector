"""Offline fakes for VectorClient tests.

The fakes implement just enough of the SentenceTransformer and ChromaDB
surfaces that VectorClient touches, so tests run without model downloads
or a real database.
"""

from typing import Any, Dict, List, Optional

import numpy as np


class FakeModel:
    """Records encode() inputs; returns fixed-size zero vectors."""

    def __init__(self, dim: int = 384):
        self.dim = dim
        self.encoded: List[str] = []

    def get_sentence_embedding_dimension(self) -> int:
        return self.dim

    def encode(self, texts, batch_size=None, **kwargs):
        if isinstance(texts, str):
            texts = [texts]
        self.encoded.extend(texts)
        return np.zeros((len(texts), self.dim))


class FakeCollection:
    def __init__(self, metadata: Optional[dict] = None):
        self.name = "fake"
        self.metadata = metadata or {}
        # id -> {"embedding": [...], "document": str, "metadata": dict}
        self.entries: Dict[str, Dict[str, Any]] = {}
        self.upserts: List[Dict[str, Any]] = []
        self.deleted_wheres: List[dict] = []
        self.queries: List[dict] = []

    def get(self, include=None, where=None):
        metas = []
        for entry in self.entries.values():
            meta = entry["metadata"]
            if where and not all(meta.get(k) == v for k, v in where.items()):
                continue
            metas.append(meta)
        return {"metadatas": metas}

    def peek(self, limit=1, include=None):
        embeddings = [entry["embedding"] for entry in list(self.entries.values())[:limit]]
        return {"embeddings": embeddings}

    def delete(self, where=None):
        self.deleted_wheres.append(dict(where) if where else {})
        if where and "filepath" in where:
            doomed = [
                entry_id
                for entry_id, entry in self.entries.items()
                if entry["metadata"].get("filepath") == where["filepath"]
            ]
            for entry_id in doomed:
                del self.entries[entry_id]

    def upsert(self, ids=None, embeddings=None, documents=None, metadatas=None):
        payload = {
            "ids": list(ids or []),
            "embeddings": embeddings,
            "documents": documents,
            "metadatas": metadatas,
        }
        self.upserts.append(payload)
        for entry_id, embedding, document, metadata in zip(
            payload["ids"], embeddings or [], documents or [], metadatas or []
        ):
            self.entries[entry_id] = {
                "embedding": embedding,
                "document": document,
                "metadata": dict(metadata),
            }

    def add(self, ids=None, embeddings=None, documents=None, metadatas=None):
        self.upsert(ids=ids, embeddings=embeddings, documents=documents, metadatas=metadatas)

    def query(self, query_embeddings=None, n_results=5, include=None):
        self.queries.append({"n_results": n_results, "include": list(include or [])})
        docs: List[str] = []
        metas: List[dict] = []
        distances: List[float] = []
        for index, entry in enumerate(list(self.entries.values())[:n_results]):
            docs.append(entry["document"])
            meta = dict(entry["metadata"])
            meta["distance"] = float(index)
            metas.append(meta)
            distances.append(float(index))
        return {
            "documents": [docs],
            "metadatas": [metas],
            "distances": [distances],
        }


class FakeChromaClient:
    def __init__(self, collection: Optional[FakeCollection] = None):
        if collection is None:
            collection = FakeCollection()
        self.collection = collection
        self.deleted_collections: List[str] = []

    def get_or_create_collection(self, name="org-roam", metadata=None):
        self.collection.name = name
        if metadata:
            self.collection.metadata = metadata
        return self.collection

    def delete_collection(self, name):
        self.deleted_collections.append(name)


def make_vector_client(collection: Optional[FakeCollection] = None, model: Optional[FakeModel] = None,
                       model_name: str = "all-MiniLM-L6-v2", **kwargs):
    from org_vector.embeddings import VectorClient

    fake_model = model if model is not None else FakeModel()
    fake_client = FakeChromaClient(collection)
    return VectorClient(
        db_path="/tmp/unused",
        model=fake_model,
        chroma_client=fake_client,
        model_name=model_name,
        **kwargs,
    )
