import os
import re
import uuid
from dataclasses import dataclass, field
from glob import glob
from typing import List, Optional

from org_vector.logger import get_logger

log = get_logger(__name__)

_TITLE_PATTERN = re.compile(r"^#\+title:\s*(.*)$", re.IGNORECASE)


@dataclass
class OrgNode:
    outline: str
    body: str
    tags: List[str] = field(default_factory=list)
    # Empty unless the org file declares an :ID: property. Deterministic
    # fallback storage ids are derived at indexing time (see embeddings).
    id: str = ""
    children: List["OrgNode"] = field(default_factory=list)
    parent: Optional["OrgNode"] = field(default=None, repr=False)
    level: int = 0

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        if not isinstance(other, OrgNode):
            return NotImplemented
        return self.id == other.id

    def add_child(self, child: "OrgNode") -> None:
        """Add a child node and set its parent reference."""
        child.parent = self
        child.level = self.level + 1
        self.children.append(child)

    def get_all_descendants(self) -> List["OrgNode"]:
        """Get all descendant nodes in a flat list (depth-first traversal)."""
        descendants: List[OrgNode] = []
        for child in self.children:
            descendants.append(child)
            descendants.extend(child.get_all_descendants())
        return descendants

    def get_ancestors(self) -> List["OrgNode"]:
        """Get all ancestor nodes from parent to root."""
        ancestors: List[OrgNode] = []
        current = self.parent
        while current:
            ancestors.append(current)
            current = current.parent
        return ancestors

    def get_path(self) -> str:
        """Get the hierarchical heading path to this node."""
        ancestors = self.get_ancestors()
        ancestors.reverse()
        parts = [node.heading_text() for node in ancestors]
        parts.append(self.heading_text())
        return " > ".join(parts)

    def heading_text(self) -> str:
        """First line of the outline, i.e. the heading itself."""
        return self.outline.split("\n")[0].strip() if self.outline else ""


@dataclass
class OrgFile:
    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    file_path: str = ""
    title: str = ""
    body: List[OrgNode] = field(default_factory=list)

    def get_all_nodes(self) -> List[OrgNode]:
        """Get all nodes in the file as a flat list."""
        all_nodes: List[OrgNode] = []
        for node in self.body:
            all_nodes.append(node)
            all_nodes.extend(node.get_all_descendants())
        return all_nodes

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        if not isinstance(other, OrgFile):
            return NotImplemented
        return self.file_path == other.file_path


class OrgRoam:
    def __init__(self, path: str, banned_files: Optional[set] = None):
        self.path = path
        self.banned_files = banned_files or set()

    def get_files(self, get_full: bool = False) -> List[str]:
        expanded_path = os.path.expanduser(self.path)
        matched_paths = glob(os.path.join(expanded_path, "**", "*.org"), recursive=True)
        files = sorted(
            file_path for file_path in matched_paths if os.path.isfile(file_path)
        )

        skipped = len(matched_paths) - len(files)
        if skipped:
            log.warning(
                "Skipped %d non-file .org path(s) under %s", skipped, expanded_path
            )

        if not get_full:
            note_titles = sorted(
                {os.path.splitext(os.path.basename(file))[0] for file in files}
            )
            return note_titles

        return files

    @staticmethod
    def get_id(node) -> Optional[str]:
        node_id = node.properties.get("ID")
        return str(node_id).strip() if node_id else None

    @staticmethod
    def get_title(node) -> str:
        """Gets the title of the org heading (or the #+TITLE of a file root)."""
        if node.heading:
            return node.heading

        match = _TITLE_PATTERN.search(node.body or "")
        if match:
            return match.group(1).strip()

        first_line = (node.body or "").split("\n")[0]
        return _TITLE_PATTERN.sub("", first_line, count=1).strip()

    @staticmethod
    def get_node_body(node) -> str:
        return node.body.strip()

    def parse_node_recursive(self, orgparse_node, level: int = 1) -> OrgNode:
        """Parse an orgparse node into an OrgNode, keeping parent-child links."""
        org_node = OrgNode(
            outline=str(orgparse_node),
            body=self.get_node_body(orgparse_node),
            level=level,
        )

        node_id = self.get_id(orgparse_node)
        if node_id:
            org_node.id = node_id

        if hasattr(orgparse_node, "tags"):
            org_node.tags = list(orgparse_node.tags or [])

        for child in orgparse_node.children:
            try:
                org_node.add_child(self.parse_node_recursive(child, level + 1))
            except Exception as error:
                log.error(
                    "Error parsing child node in %s: %s", org_node.heading_text(), error
                )

        return org_node

    def parse_org(self, root) -> Optional[tuple]:
        """Parse an orgparse root; returns (title, top-level nodes)."""
        try:
            heading = self.get_title(root)
            top_level_nodes: List[OrgNode] = []

            for child in root.children:
                try:
                    top_level_nodes.append(self.parse_node_recursive(child, level=1))
                except Exception as error:
                    log.error("Error parsing top-level node: %s", error)

            return heading, top_level_nodes
        except Exception as error:
            log.error("Error while parsing org file: %s", error, exc_info=True)
            return None

    def parse_file(self, file_path: str) -> Optional[OrgFile]:
        if not os.path.isfile(file_path):
            log.warning("Skipping non-file org path: %s", file_path)
            return None

        try:
            log.info("parsing: %s", file_path)
            root = orgparse_load(file_path)
            parsed = self.parse_org(root)
            if not parsed:
                log.warning("Could not parse org file: %s", file_path)
                return None

            title, nodes = parsed
            org_file = OrgFile(file_path=file_path, body=nodes, title=title)
            org_id = self.get_id(root)
            if org_id:
                org_file.id = org_id
            return org_file
        except Exception as error:
            log.error("Error processing file %s: %s", file_path, error)
            return None

    def parse_files(self, file_paths: Optional[List[str]] = None) -> List[OrgFile]:
        """Parse org files into a list of OrgFile objects with tree structure."""
        paths = file_paths if file_paths is not None else self.get_files(get_full=True)
        parsed_files = []
        for file_path in paths:
            parsed = self.parse_file(file_path)
            if parsed is not None:
                parsed_files.append(parsed)
        return parsed_files


def orgparse_load(file_path: str):
    import orgparse

    return orgparse.load(file_path)
