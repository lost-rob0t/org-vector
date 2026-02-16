import hashlib
import os
from typing import Dict, List, Optional, Tuple, Union

os.environ.setdefault("ANONYMIZED_TELEMETRY", "False")
os.environ.setdefault("CHROMA_TELEMETRY_IMPL", "chromadb.telemetry.product.null.NullTelemetry")

try:
    import posthog

    def _capture_noop(*args, **kwargs):
        return None

    posthog.capture = _capture_noop
except Exception:
    pass

from chromadb import PersistentClient
from chromadb.config import Settings
from sentence_transformers import SentenceTransformer
from langchain_core.documents import Document

from org_vector.parse_org_files import OrgNode, OrgFile
from org_vector.logger import get_logger

log = get_logger()


class VectorClient:
    def __init__(
        self,
        db_path: str,
        model: Optional[Union[SentenceTransformer, str]] = None,
        chroma_client: Optional[PersistentClient] = None,
        model_name: str = "all-MiniLM-L6-v2",
        collection_name: str = "org-roam",
        ingestion_instructions: Optional[str] = None,
        query_instructions: Optional[str] = None,
        batch_size: int = 32,
    ):
        self.db_path = db_path
        self.model_name = self._resolve_model_name(model, model_name)
        self.collection_name = collection_name
        default_ingestion, default_query = self._default_instructions(self.model_name)
        self.ingestion_instructions = (
            ingestion_instructions if ingestion_instructions is not None else default_ingestion
        )
        self.query_instructions = (
            query_instructions if query_instructions is not None else default_query
        )
        self.batch_size = self._normalize_batch_size(batch_size)

        self.model = self._load_model(model, self.model_name)
        self.model_dimension = self._get_model_dimension(self.model)
        self.chroma_client = chroma_client or PersistentClient(
            path=db_path,
            settings=Settings(anonymized_telemetry=False),
        )
        self._get_or_create_collection()
        self._ensure_collection_dimension()

    @staticmethod
    def _resolve_model_name(
        model: Optional[Union[SentenceTransformer, str]],
        model_name: str,
    ) -> str:
        if isinstance(model, str):
            return model
        return model_name

    @staticmethod
    def _load_model(
        model: Optional[Union[SentenceTransformer, str]],
        model_name: str,
    ) -> SentenceTransformer:
        if isinstance(model, SentenceTransformer):
            return model
        if isinstance(model, str):
            return SentenceTransformer(model)
        return SentenceTransformer(model_name)

    @staticmethod
    def _normalize_batch_size(batch_size: int) -> int:
        try:
            normalized = int(batch_size)
        except (TypeError, ValueError):
            return 32
        return normalized if normalized > 0 else 32

    @staticmethod
    def _iter_batch_ranges(total: int, batch_size: int):
        for start in range(0, total, batch_size):
            end = min(start + batch_size, total)
            yield start, end

    @staticmethod
    def _get_model_dimension(model: SentenceTransformer) -> int:
        try:
            dimension = model.get_sentence_embedding_dimension()
            if isinstance(dimension, int) and dimension > 0:
                return dimension
        except Exception:
            pass

        probe = model.encode("dimension probe")
        try:
            return len(probe)
        except TypeError:
            return len(probe.tolist())

    def _collection_metadata(self) -> Dict[str, Union[str, int]]:
        return {
            "embedding_dimension": self.model_dimension,
            "embedding_model": self.model_name,
        }

    def _get_or_create_collection(self) -> None:
        metadata = self._collection_metadata()
        try:
            self.collection = self.chroma_client.get_or_create_collection(
                name=self.collection_name,
                metadata=metadata,
            )
        except TypeError:
            self.collection = self.chroma_client.get_or_create_collection(name=self.collection_name)

    @staticmethod
    def _extract_dimension_from_metadata(metadata: Optional[dict]) -> Optional[int]:
        if not isinstance(metadata, dict):
            return None

        for key in ("embedding_dimension", "dimension", "vector_dimension", "embedding_dim"):
            value = metadata.get(key)
            if value is None:
                continue
            try:
                value_int = int(value)
            except (TypeError, ValueError):
                continue
            if value_int > 0:
                return value_int

        return None

    def _get_collection_dimension(self) -> Optional[int]:
        metadata_dimension = self._extract_dimension_from_metadata(
            getattr(self.collection, "metadata", None)
        )
        if metadata_dimension is not None:
            return metadata_dimension

        inner_collection = getattr(self.collection, "_collection", None)
        metadata_dimension = self._extract_dimension_from_metadata(
            getattr(inner_collection, "metadata", None)
        )
        if metadata_dimension is not None:
            return metadata_dimension

        if not hasattr(self.collection, "peek"):
            return None

        try:
            result = self.collection.peek(limit=1, include=["embeddings"])
        except Exception as error:
            log.warning(f"Could not read collection dimension: {error}")
            return None

        if not isinstance(result, dict):
            return None

        embeddings = result.get("embeddings") or []
        if not embeddings or not embeddings[0]:
            return None

        return len(embeddings[0])

    def _reset_collection(self, reason: Optional[str] = None) -> None:
        if reason:
            log.warning("Resetting collection '%s': %s", self.collection_name, reason)
        else:
            log.warning(
                "Resetting collection '%s' to match model dimension %s.",
                self.collection_name,
                self.model_dimension,
            )

        try:
            self.chroma_client.delete_collection(name=self.collection_name)
        except Exception as error:
            raise RuntimeError(
                f"Could not reset collection '{self.collection_name}' to match model dimension {self.model_dimension}: {error}"
            ) from error

        self._get_or_create_collection()

    def _ensure_collection_dimension(self) -> None:
        existing_dimension = self._get_collection_dimension()
        if existing_dimension is None:
            return
        if existing_dimension == self.model_dimension:
            return

        log.warning(
            "Collection '%s' dimension %s does not match model dimension %s; recreating collection.",
            self.collection_name,
            existing_dimension,
            self.model_dimension,
        )
        self._reset_collection()

    @staticmethod
    def _default_instructions(model_name: str) -> Tuple[str, str]:
        return (
            "Represent this org note for semantic retrieval:",
            "Represent this search query for retrieving relevant org notes:",
        )

    @staticmethod
    def _apply_instruction(instruction: Optional[str], text: str) -> str:
        if not instruction:
            return text

        normalized_instruction = instruction.strip()
        if "{text}" in normalized_instruction:
            return normalized_instruction.format(text=text)
        if normalized_instruction.endswith(":"):
            return f"{normalized_instruction} {text}"
        return f"{normalized_instruction}\n\n{text}"

    @staticmethod
    def _is_dimension_mismatch_error(error: Exception) -> bool:
        message = str(error).lower()
        if "dimension" not in message:
            return False
        if "collection expecting embedding with dimension" in message:
            return True
        if "dimension mismatch" in message:
            return True
        if "expected" in message and "got" in message:
            return True
        return False

    def _get_node_storage_id(self, org_file: OrgFile, node: OrgNode, index: int) -> str:
        if node.id and str(node.id).strip():
            return str(node.id).strip()

        stable_source = f"{org_file.file_path}|{node.get_path()}|{index}"
        digest = hashlib.sha1(stable_source.encode("utf-8")).hexdigest()
        return f"auto-{digest}"

    @staticmethod
    def _get_source_state(file_path: str) -> Optional[Tuple[str, int]]:
        try:
            stat = os.stat(file_path)
        except OSError:
            return None
        return (str(stat.st_mtime_ns), int(stat.st_size))

    @staticmethod
    def _metadata_to_source_state(metadata: Optional[dict]) -> Optional[Tuple[str, int]]:
        if not metadata:
            return None

        mtime = metadata.get("source_mtime_ns")
        size = metadata.get("source_size")
        if mtime is None or size is None:
            return None

        try:
            return (str(mtime), int(size))
        except (TypeError, ValueError):
            return None

    def _get_indexed_file_states(self) -> Dict[str, Optional[Tuple[str, int]]]:
        states: Dict[str, Optional[Tuple[str, int]]] = {}

        try:
            results = self.collection.get(include=["metadatas"])
        except Exception as error:
            log.warning(f"Could not read indexed file states: {error}")
            return states

        for metadata in (results.get("metadatas") or []):
            if not metadata:
                continue

            filepath = metadata.get("filepath")
            if not filepath:
                continue

            state = self._metadata_to_source_state(metadata)
            existing_state = states.get(filepath)
            if existing_state is None:
                states[filepath] = state
            elif state is not None:
                states[filepath] = state

        return states

    def plan_sync(self, file_paths: List[str], remove_missing: bool = True) -> Dict[str, List[str]]:
        current_files = sorted({path for path in file_paths if os.path.isfile(path)})
        indexed_states = self._get_indexed_file_states()

        to_index: List[str] = []
        unchanged: List[str] = []

        for file_path in current_files:
            current_state = self._get_source_state(file_path)
            indexed_state = indexed_states.get(file_path)

            if current_state is not None and indexed_state == current_state:
                unchanged.append(file_path)
            else:
                to_index.append(file_path)

        to_remove: List[str] = []
        if remove_missing:
            to_remove = sorted(set(indexed_states.keys()) - set(current_files))

        return {
            "to_index": to_index,
            "unchanged": unchanged,
            "to_remove": to_remove,
        }

    def remove_files(self, file_paths: List[str]) -> int:
        removed_count = 0

        for file_path in file_paths:
            try:
                self.collection.delete(where={"filepath": file_path})
                removed_count += 1
            except Exception as error:
                log.warning(f"Could not remove indexed entries for {file_path}: {error}")

        return removed_count

    def sync_files(self, org_files: List[OrgFile], removed_files: Optional[List[str]] = None) -> Dict[str, int]:
        removed_count = self.remove_files(removed_files or [])
        indexed_count = 0
        failed_count = 0

        for org_file in org_files:
            try:
                self.embed_file(org_file)
                indexed_count += 1
            except Exception as error:
                failed_count += 1
                log.error(f"Failed to embed {org_file.file_path}: {error}")

        return {
            "indexed": indexed_count,
            "failed": failed_count,
            "removed": removed_count,
        }

    @staticmethod
    def _ensure_unique_ids(ids: List[str], file_path: str) -> List[str]:
        unique_ids: List[str] = []
        used: set[str] = set()

        for raw_id in ids:
            candidate = raw_id
            suffix = 1
            while candidate in used:
                candidate = f"{raw_id}__dup{suffix}"
                suffix += 1

            if candidate != raw_id:
                log.warning(f"Duplicate node id '{raw_id}' in {file_path}; using '{candidate}'")

            used.add(candidate)
            unique_ids.append(candidate)

        return unique_ids

    def _build_documents_and_ids(
        self,
        org_file: OrgFile,
        source_state: Optional[Tuple[str, int]] = None,
    ) -> Tuple[List[Document], List[str]]:
        documents: List[Document] = []
        node_ids: List[str] = []
        all_nodes = org_file.get_all_nodes()

        for index, node in enumerate(all_nodes):
            stars = '*' * max(1, node.level)
            path = node.get_path()
            heading_line = node.outline.split('\n')[0] if node.outline else ""
            if not heading_line.startswith('*'):
                heading_line = f"{stars} {heading_line}"

            context = [
                f"File: {org_file.title or os.path.basename(org_file.file_path)}",
                f"Path: {path}",
            ]
            if node.tags:
                context.append(f"Tags: {', '.join(sorted(node.tags))}")

            body = node.body.strip()
            page_content = "\n".join(context + ["", heading_line, body]).strip()
            storage_id = self._get_node_storage_id(org_file, node, index)
            metadata = {
                "title": org_file.title,
                "id": node.id or None,
                "filepath": org_file.file_path,
                "tags": ", ".join(sorted(node.tags)) if node.tags else "",
                "level": node.level,
                "path": path,
                "storage_id": storage_id,
            }
            if source_state is not None:
                metadata["source_mtime_ns"] = source_state[0]
                metadata["source_size"] = source_state[1]

            documents.append(
                Document(
                    page_content=page_content,
                    metadata=metadata,
                )
            )
            node_ids.append(storage_id)

        return documents, node_ids

    @staticmethod
    def _diversify_documents(documents: List[Document], k: int) -> List[Document]:
        if len(documents) <= k:
            return documents

        selected: List[Document] = []
        seen: set[Tuple[str, str, str]] = set()
        per_file_count: dict[str, int] = {}
        max_per_file = 1

        for doc in documents:
            filepath = str(doc.metadata.get("filepath", ""))
            path = str(doc.metadata.get("path", ""))
            key = (filepath, path, doc.page_content.strip())
            if key in seen:
                continue
            if per_file_count.get(filepath, 0) >= max_per_file:
                continue

            seen.add(key)
            selected.append(doc)
            per_file_count[filepath] = per_file_count.get(filepath, 0) + 1
            if len(selected) >= k:
                return selected

        for doc in documents:
            filepath = str(doc.metadata.get("filepath", ""))
            path = str(doc.metadata.get("path", ""))
            key = (filepath, path, doc.page_content.strip())
            if key in seen:
                continue

            seen.add(key)
            selected.append(doc)
            if len(selected) >= k:
                break

        return selected
    
    def make_document(self, org_file: OrgFile) -> List[Document]:
        source_state = self._get_source_state(org_file.file_path)
        documents, _ = self._build_documents_and_ids(org_file, source_state=source_state)
        return documents
    
    def embed_file(self, org_file: OrgFile) -> None:
        log.info(f"Embedding {org_file.title or 'file'}")
        source_state = self._get_source_state(org_file.file_path)
        docs, node_ids = self._build_documents_and_ids(org_file, source_state=source_state)
        if not docs:
            return

        unique_ids = self._ensure_unique_ids(node_ids, org_file.file_path)

        try:
            self.collection.delete(where={"filepath": org_file.file_path})
        except Exception as delete_error:
            log.warning(f"Could not clear prior embeddings for {org_file.file_path}: {delete_error}")

        def _upsert_batches() -> None:
            for start, end in self._iter_batch_ranges(len(docs), self.batch_size):
                batch_docs = docs[start:end]
                batch_texts = [doc.page_content for doc in batch_docs]
                embedding_texts = [
                    self._apply_instruction(self.ingestion_instructions, text)
                    for text in batch_texts
                ]
                batch_embeddings = self.model.encode(
                    embedding_texts,
                    batch_size=min(self.batch_size, len(batch_texts)),
                ).tolist()
                payload = {
                    "ids": unique_ids[start:end],
                    "embeddings": batch_embeddings,
                    "documents": batch_texts,
                    "metadatas": [doc.metadata for doc in batch_docs],
                }
                if hasattr(self.collection, "upsert"):
                    self.collection.upsert(**payload)
                else:
                    self.collection.add(**payload)

        try:
            _upsert_batches()
        except Exception as error:
            if not self._is_dimension_mismatch_error(error):
                raise

            log.warning(
                "Collection dimension mismatch while embedding %s; recreating collection and retrying.",
                org_file.file_path,
            )
            self._reset_collection(
                reason="dimension mismatch while embedding; full re-index required",
            )
            _upsert_batches()

    
    def query(self, query: str, k: int = 5) -> List[Document]:
        if not query.strip():
            return []

        log.info(f"Searching for {query}")
        
        query_text = self._apply_instruction(self.ingestion_instructions, query)
            
        query_embedding = self.model.encode(query_text).tolist()
        n_candidates = max(k * 6, k)
        
        results = self.collection.query(
            query_embeddings=[query_embedding],
            n_results=n_candidates,
            include=["documents", "metadatas", "distances"]
        )
        
        documents: List[Document] = []
        if results and results['documents'] and results['metadatas']:
            result_documents = results['documents'][0]
            result_metadatas = results['metadatas'][0]
            result_distances = results.get('distances', [[]])[0]

            for i in range(len(result_documents)):
                metadata = dict(result_metadatas[i] or {})
                if i < len(result_distances):
                    metadata['distance'] = result_distances[i]

                documents.append(
                    Document(
                        page_content=result_documents[i],
                        metadata=metadata
                    )
                )
        
        return self._diversify_documents(documents, k)
    
