import re
import uuid
from typing import Any, Dict, List

import ollama
from langchain_core.documents import Document
from langchain_chroma import Chroma
from langchain_ollama import OllamaEmbeddings

from org_vector.parse_org_files import OrgNode, OrgFile
from org_vector.logger import get_logger

log = get_logger()

class VectorClient:
    def __init__(self, api_url: str, db_path: str, model: str = "nomic-embed-text", collection_name: str = "org-roam"):
        self.model = model
        self.db_path = db_path
        self.api_url = api_url
        self.embedder = OllamaEmbeddings(model=model, base_url=api_url)
        self.vector_store = Chroma(
            persist_directory=self.db_path,
            embedding_function=self.embedder,
            collection_name=collection_name
        )
    
    def make_document(self, org_file: OrgFile) -> List[Document]:
        documents = []
        all_nodes = org_file.get_all_nodes()

        for node in all_nodes:
            stars = '*' * max(1, node.level)
            path = node.get_path()
            heading_line = node.outline.split('\n')[0] if node.outline else ""
            if not heading_line.startswith('*'):
                heading_line = f"{stars} {heading_line}"

            page_content = f"{heading_line}\n{node.body}".strip()

            documents.append(
                Document(
                    page_content=page_content,
                    metadata={
                        "title": org_file.title,
                        "id": node.id or None,
                        "filepath": org_file.file_path,
                        "tags": ", ".join(sorted(node.tags)) if node.tags else "",
                        "level": node.level,
                        "path": path
                    }
                )
            )
        return documents        
    def embed_file(self, org_file: OrgFile):
       log.info(f"Embedding {org_file.title or 'file'}")
       docs = self.make_document(org_file)
       if docs:
           all_nodes = org_file.get_all_nodes()
           self.vector_store.add_documents(documents=docs, ids=[node.id for node in all_nodes])

    
    def query(self, query: str, k: int = 5) -> List[Document]:
        log.info(f"Searching for {query}")
        resp = self.vector_store.similarity_search(query, k=k)
        return resp
    
