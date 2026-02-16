#!/usr/bin/env python3
"""
Debug script to identify the duplicate ID issue in org parsing
"""

import org_vector.parse_org_files as o
from collections import Counter

# Create roam parser
roam = o.OrgRoam("~/Documents/Notes/org/roam/")

# Parse files
print("Parsing files...")
df = roam.parse_files()
print(f"Found {len(df)} unique OrgFile objects")

# Check for duplicate IDs across all nodes
all_node_ids = []
for org_file in df:
    for node in org_file.body:
        all_node_ids.append(node.id)

print(f"Total nodes: {len(all_node_ids)}")

# Count duplicates
id_counts = Counter(all_node_ids)
duplicates = {id_val: count for id_val, count in id_counts.items() if count > 1}

if duplicates:
    print(f"Found {len(duplicates)} duplicate IDs:")
    for dup_id, count in duplicates.items():
        print(f"  ID {dup_id} appears {count} times")
else:
    print("No duplicate IDs found!")

# Let's also check if the same node appears in multiple files
print("\nChecking for nodes appearing in multiple files...")
node_to_files = {}
for org_file in df:
    for node in org_file.body:
        if node.id not in node_to_files:
            node_to_files[node.id] = []
        node_to_files[node.id].append(org_file.title)

cross_file_duplicates = {node_id: files for node_id, files in node_to_files.items() if len(files) > 1}
if cross_file_duplicates:
    print(f"Found {len(cross_file_duplicates)} nodes appearing in multiple files:")
    for node_id, files in cross_file_duplicates.items():
        print(f"  Node {node_id} in files: {files}")
