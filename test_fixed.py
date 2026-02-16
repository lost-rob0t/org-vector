#!/usr/bin/env python3
"""
Test script using the fixed parser to verify no duplicate IDs
"""

import org_vector.parse_org_files_fixed as o_fixed
import org_vector.embeddings as e
from collections import Counter

print("Testing fixed parser...")

# Create roam parser with fixed version
roam = o_fixed.OrgRoam("~/Documents/Notes/org/roam/")

# Parse files
print("Parsing files...")
parsed_files = roam.parse_files()
print(f"Found {len(parsed_files)} unique OrgFile objects")

# Collect all node IDs
all_node_ids = []
total_nodes = 0
for org_file in parsed_files:
    total_nodes += len(org_file.body)
    for node in org_file.body:
        all_node_ids.append(node.id)

print(f"Total nodes: {total_nodes}")
print(f"Total IDs collected: {len(all_node_ids)}")

# Check for duplicates
id_counts = Counter(all_node_ids)
duplicates = {id_val: count for id_val, count in id_counts.items() if count > 1}

if duplicates:
    print(f"❌ Found {len(duplicates)} duplicate IDs:")
    for dup_id, count in duplicates.items():
        print(f"  ID {dup_id} appears {count} times")
else:
    print("✅ No duplicate IDs found!")

# Test embedding if no duplicates
if not duplicates and parsed_files:
    print("\nTesting vector embedding...")
    try:
        v = e.VectorClient(api_url="http://localhost:11434", db_path="org-vector-test")
        
        # Test with first file only
        test_file = parsed_files[0]
        print(f"Testing with file: {test_file.title} ({len(test_file.body)} nodes)")
        
        # Check IDs are unique in this file
        file_ids = [node.id for node in test_file.body]
        if len(file_ids) == len(set(file_ids)):
            print("✅ File has unique IDs")
            v.embed_file(test_file)
            print("✅ Successfully embedded file!")
        else:
            print("❌ File has duplicate IDs")
            
    except Exception as e:
        print(f"❌ Embedding failed: {e}")
else:
    print("Skipping embedding test due to duplicate IDs")