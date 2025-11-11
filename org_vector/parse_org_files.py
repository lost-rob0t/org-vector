from orgparse import load
import traceback
import pandas as pd
from typing import List, Optional
import os
from glob import glob
import re
import uuid as u
from dataclasses import dataclass, field, asdict

from org_vector.logger import get_logger

log = get_logger()

@dataclass
class OrgNode:
    outline: str
    body: str
    tags: list = field(default_factory=lambda: list())
    id: str = field(default_factory=lambda: str(u.uuid4()))
    children: List['OrgNode'] = field(default_factory=list)
    parent: Optional['OrgNode'] = field(default=None, repr=False)
    level: int = 0

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        if not isinstance(other, OrgNode):
            return False
        return self.id == other.id
    
    def add_child(self, child: 'OrgNode'):
        """Add a child node and set its parent reference."""
        child.parent = self
        child.level = self.level + 1
        self.children.append(child)
    
    def get_all_descendants(self) -> List['OrgNode']:
        """Get all descendant nodes in a flat list (depth-first traversal)."""
        descendants = []
        for child in self.children:
            descendants.append(child)
            descendants.extend(child.get_all_descendants())
        return descendants
    
    def get_ancestors(self) -> List['OrgNode']:
        """Get all ancestor nodes from parent to root."""
        ancestors = []
        current = self.parent
        while current:
            ancestors.append(current)
            current = current.parent
        return ancestors
    
    def get_path(self) -> str:
        """Get the hierarchical path to this node."""
        ancestors = self.get_ancestors()
        ancestors.reverse()
        path_parts = [node.outline.split('\n')[0].strip() for node in ancestors]
        path_parts.append(self.outline.split('\n')[0].strip())
        return ' > '.join(path_parts)
    
    def to_dataframe(self) -> pd.DataFrame:
        """Convert node to dataframe row, including hierarchy info."""
        data = asdict(self)
        data['children'] = len(self.children)
        data['parent_id'] = self.parent.id if self.parent else None
        data['path'] = self.get_path()
        return pd.DataFrame([data])

@dataclass
class OrgFile:
    id: str = field(default_factory=lambda: str(u.uuid4()))
    file_path: str = ""
    title: str = ""
    body: List[OrgNode] = field(default_factory=list)

    def get_all_nodes(self) -> List[OrgNode]:
        """Get all nodes in the file as a flat list."""
        all_nodes = []
        for node in self.body:
            all_nodes.append(node)
            all_nodes.extend(node.get_all_descendants())
        return all_nodes

    def to_dataframe(self) -> pd.DataFrame:
        all_nodes = self.get_all_nodes()
        if not all_nodes:
            return pd.DataFrame()

        df_list = [node.to_dataframe() for node in all_nodes]
        return pd.concat(df_list, ignore_index=True)

    def __hash__(self):
        return hash(self.id)

    def __eq__(self, other):
        if not isinstance(other, OrgFile):
            return False
        return self.id == other.id and self.title == other.title

class OrgRoam:
    def __init__(self, path: str, banned_files: set | None = None):
        self.path = path
        self.banned_files = banned_files or set()

    def get_files(self, get_full: bool = False) -> List[str]:
        expanded_path = os.path.expanduser(self.path)
        path = os.path.join(expanded_path, "**", "*.org")
        files = set(glob(path, recursive=True))

        if not get_full:
            files = [os.path.splitext(os.path.basename(file))[0] for file in files]

        return files

    def get_id(self, node):
        return node.properties.get('ID')
    
    def get_title(self, node):
        "Gets the title of the org heading"
        if node.heading:
            return node.heading
        else:
            title_pattern = re.compile(r'^#\+title:\s*(.*)$', re.IGNORECASE)
            match = title_pattern.search(node.body)
            if match:
                return match.group(1)
            else:
                return re.sub(r"#\+title:", "", node.body.split("\n")[0], flags=re.IGNORECASE).strip()

    def get_node_body(self, node) -> str:
        return node.body.strip()

    def parse_node_recursive(self, orgparse_node, level: int = 1) -> OrgNode:
        """
        Parse an orgparse node into our OrgNode structure recursively.
        Maintains proper parent-child relationships.
        """
        node_text = str(orgparse_node)
        body = self.get_node_body(orgparse_node)
        org_node = OrgNode(
            outline=node_text,
            body=body,
            level=level
        )

        node_id = self.get_id(orgparse_node)
        if node_id:
            org_node.id = node_id

        if hasattr(orgparse_node, 'tags'):
            org_node.tags = orgparse_node.tags or []

        for child in orgparse_node.children:
            try:
                child_node = self.parse_node_recursive(child, level + 1)
                org_node.add_child(child_node)
            except Exception as e:
                log.error(f"Error parsing child node: {e}")

        return org_node

    def parse_org(self, root):
        """Parse the org file, returns title and list of top-level nodes."""
        try:
            heading = self.get_title(root)
            org_id = self.get_id(root)

            top_level_nodes = []

            for child in root.children:
                if child:
                    try:
                        node = self.parse_node_recursive(child, level=1)
                        top_level_nodes.append(node)
                    except Exception as e:
                        log.error(f"Error parsing top-level node: {e}")

            return (heading, top_level_nodes)
        except Exception as e:
            log.error(f"Error while parsing org file: {e}\n{traceback.format_exc()}")

    def parse_files(self):
        """Parse all org files, returns a list of OrgFile objects with tree structure."""
        files = []
        
        for file_path in self.get_files(get_full=True):
            try:
                log.info(f"parsing: {file_path}")
                root = load(file_path)
                title, nodes = self.parse_org(root)
                org_id = self.get_id(root)
                parsed_org = OrgFile(
                    file_path=file_path,
                    body=nodes,
                    title=title
                )
                if org_id:
                    parsed_org.id = org_id
                files.append(parsed_org)
            except Exception as e:
                log.error(f"Error processing file {file_path}: {e}")
                
        return files
