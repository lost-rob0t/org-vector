import org_vector.embeddings as e
import org_vector.parse_org_files as o
roam = o.OrgRoam("~/Documents/Notes/org/roam/")
v = e.VectorClient(api_url="http://localhost:11434", db_path="test-org")

resp = v.query("Vector search")

for r in resp:
    print(r.metadata['id'])
    print(f"{r.metadata['title']}:\n{r.page_content[:200]}")
