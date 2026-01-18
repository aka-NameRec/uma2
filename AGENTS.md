# UMA2 — Codex project instructions

## Language & communication
- Chat with the user: Russian.
- Code: comments, docstrings, user-facing string literals, and git commit messages MUST be in English.

## ConPort (project memory) — mandatory
If MCP server "conport" is available:
- At the start of each new task/session: load relevant project context from ConPort (decisions, progress, architecture, glossary, open tasks).
- Do not assume missing facts. If ConPort lacks data, read `docs/` and the codebase.
- After finishing work: write back important updates to ConPort:
  - decisions (why we did X),
  - progress (what changed / what’s done),
  - tasks (next steps),
  - architecture notes (interfaces/contracts, constraints).

## Git workflow — mandatory
- NEVER create commits on branches: main, master, develop, staging.
  If currently on one of these, warn the user and suggest creating a new branch.
- ALWAYS ask the user to approve the commit message text before committing.
- Commit message MUST start with SCM task id extracted from branch name:
  branch name format: <purpose>/<task_id>-<description>
  task_id may contain: latin letters, digits, '-'
- Commit message format:
  task_id. (commit_type) commit_message.
  Examples: feat, fix, refactor, chore, docs, test.

## Architecture & layering — mandatory
- Service layer MUST contain business logic only.
- Service layer MUST NOT use ORM directly and MUST NOT use external protocols (HTTP, etc).
- DB access MUST be behind a Repository (or similar) abstraction.
- If you detect a potential backward compatibility break, report it to the user,
  but do NOT decide compatibility strategy on your own.

## DRY — mandatory
- Do not duplicate code.
- If duplication is suspected or detected, explicitly tell the user and propose refactoring options.

## Error handling — mandatory
Functions must NEVER silently swallow errors.

A function MUST either:
1) succeed and return a valid result, OR
2) raise an exception with actionable context.

Forbidden:
- returning None on error
- returning empty collections ([], {}) instead of raising
- returning magic values (-1, "", etc)
- logging an error and continuing as if nothing happened
- swallowing exceptions

Allowed:
- Optional[T] only for legitimate "no value" (NOT an error)
- custom exceptions with context
- exception chaining: `raise ... from e`

## Python preferences
- Use type hints everywhere (params + returns).
- Avoid magic numbers/strings. Prefer constants/enums/config.
- Prefer dataclasses (or structured models) over dicts for structured return values.
- Replace long if/elif chains with `match` when it improves readability.
- For CLI scripts: prefer Typer.
- For executing OS commands from Python: prefer plumbum.

## Code quality workflow
- Verify information before stating it. Do not speculate without evidence.
- Preserve existing code/behavior unless explicitly requested to change it.
- Make minimal, focused edits; avoid unrelated cleanup.
- Avoid "apologies" and avoid "I understand" feedback.
- Do not propose whitespace-only changes.
- Do not invent additional changes beyond the request.
- When referencing files, use real file paths in the repo (not placeholders like x.md).
- Prefer providing edits for a file in a single coherent patch/chunk.

## Local project workflow (UMA2)
- Use the project’s existing tooling (uv/ruff/mypy/pytest if present in the repo).
- When you changed code, prefer running (as applicable):
  - `uv run ruff check`
  - `uv run mypy`
  - `uv run pytest`
