# AGENTS.md
Operational guide for coding agents in `vectored-notes`.
Last audited: 2026-02-15

## Project priorities
1. Keep indexing and retrieval quality high.
2. Preserve CLI compatibility for existing users.
3. Keep indexing logic extensible for future background workers.
4. Do not implement a background indexing service unless explicitly requested.

## Cursor/Copilot rules audit
Checked and currently not present:
- `.cursor/rules/`
- `.cursorrules`
- `.github/copilot-instructions.md`
If these files appear later, treat them as higher-priority repo rules.

## Repository map
- `main.py`: CLI entrypoint and mode dispatch.
- `org_vector/embeddings.py`: embeddings, Chroma access, incremental sync.
- `org_vector/parse_org_files.py`: org discovery and parser.
- `org_vector/background_service.py`: inotify-driven background index sync loop.
- `org_vector/service_config.py`: TOML config loader for service mode.
- `org_vector/logger.py`: env-based logging setup.
- `org-vector.el`: Emacs integration.
- `README.org`: user-visible behavior and examples.
- `flake.nix`: package/app wrappers and dev shell.

## Environment
- Preferred workflow uses Nix (`nix develop`, `nix run`, `nix build`).
- Dev shell uses Python 3.12.
- `setup.py` metadata still targets Python `>=3.8`.
- Core deps: `chromadb`, `sentence-transformers`, `orgparse`, `langchain`.

## Build, lint, and test commands
Run commands from repository root.

### Enter dev shell
```bash
nix develop
```

### Build
```bash
nix build .#
```

### Run CLI via Nix app
```bash
nix run .# -- search --path ~/.cache/org-vector/ --query "Python code"
nix run .# -- query --path ~/.cache/org-vector/ --query "Python code"
nix run .# -- embed --dir ~/Documents/Notes/ --path ~/.cache/org-vector/
nix run .# -- update --dir ~/Documents/Notes/ --path ~/.cache/org-vector/
```

### Run CLI via Python
```bash
python main.py search --path ~/.cache/org-vector/ --query "Python code"
python main.py embed --dir ~/Documents/Notes/ --path ~/.cache/org-vector/
```

### Fast syntax check
```bash
python -m py_compile main.py org_vector/embeddings.py org_vector/parse_org_files.py org_vector/logger.py org_vector/background_service.py org_vector/service_config.py
```

### Type check
```bash
nix develop -c pyright main.py org_vector
```

### Optional format/lint checks
```bash
nix develop -c black --check main.py org_vector
nix develop -c flake8 main.py org_vector
```

### Test scripts (current test style)
Tests are script-oriented and integration-heavy, not a strict pytest suite.
```bash
nix develop -c python test_context_fix.py
nix develop -c python test_fixed.py
nix develop -c python test.py
```

### Single test execution (important)
Preferred single-test command:
```bash
nix develop -c python test_context_fix.py
```
Single function invocation:
```bash
nix develop -c python -c "from test_context_fix import test_nested_content_embedding as t; raise SystemExit(0 if t() else 1)"
```

### CLI smoke checks
```bash
nix run .# -- query --path ~/.cache/org-vector/ --query "Python code"
nix run .# -- embed --path ~/.cache/org-vector/
```
Second command should fail with `--dir is required for embed mode`.

## Code style guide
Follow existing repository conventions (PEP 8 plus pragmatic typing).

### Imports
1. Order imports as stdlib, third-party, then local modules.
2. Avoid wildcard imports.
3. Prefer explicit imports (`from org_vector.logger import get_logger`).
4. Keep import-time side effects minimal.

### Formatting
1. Use 4-space indentation.
2. Keep lines readable; Black-compatible formatting is acceptable.
3. Use trailing commas in multiline structures where practical.
4. Add comments only for non-obvious behavior.
5. Keep functions focused and composable.

### Types
1. Add type hints for public functions and methods.
2. Use `Optional`, `List`, `Dict`, `Tuple` consistently.
3. Keep return shapes explicit for planning/status APIs.
4. Use dataclasses for structured parse data (`OrgNode`, `OrgFile`).
5. Preserve metadata field stability when extending documents.

### Naming
1. Use `snake_case` for variables, functions, and methods.
2. Use `PascalCase` for classes.
3. Use `UPPER_SNAKE_CASE` for constants.
4. Use descriptive indexing names (`to_index`, `unchanged`, `to_remove`).

### Errors and logging
1. In library code, prefer logger calls over `print`.
2. Use `print` only for CLI output and standalone test scripts.
3. Catch broad exceptions only at I/O boundaries.
4. Log recoverable failures and continue with other files.
5. Validate CLI inputs with `argparse` and `parser.error(...)`.
6. Include file path/context in warnings and errors.

## Behavioral invariants
1. `query` remains an alias of `search`.
2. `update` remains an alias of incremental `embed` sync.
3. Incremental sync compares source metadata (mtime + size).
4. Unchanged files are skipped during embed/update.
5. Deleted files are removed from the vector collection.
6. Re-indexing a file replaces prior chunks for that file.
7. Missing Org IDs get deterministic fallback storage IDs.
8. Retrieval should avoid repetitive same-file top results.
9. `serve` uses incremental embed/update semantics and reacts to file changes via inotify.

## Agent workflow checklist
1. Read local context before editing.
2. Keep patches small and targeted.
3. Update docs when behavior changes (`README.org`, this file).
4. Run syntax checks plus one relevant runtime check.
5. Do not add background indexing services unless asked.
