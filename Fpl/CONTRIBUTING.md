# Contributing Guidelines

## Purpose

This document defines contribution guidelines and project-wide policies for the repository, including the policy for licensing headers in source files.

## License Policy

This project uses the MIT License. Maintain a single canonical `LICENSE` file at the repository root containing the full text of the MIT license.

Source files should not duplicate the full license text. Instead, each source file should include a short header that references the canonical `LICENSE` file and the copyright owner. This keeps files concise while preserving legal attribution.

### Recommended short header (F#)

Place this header at the top of each `.fs` file (and analogous short headers for other file types):

```fsharp
(*
Copyright (c) 2026+ bookofproofs
See LICENSE in the project root for license terms.
*)
```

For files that already contain the full license block (historical files), it is acceptable to leave those unchanged. New files and file updates should use the short header format.

## Why this approach

- Single source of truth: keeps the full license text in one place making updates straightforward.
- Reduces noise in individual source files, improving readability.
- Many tools (license scanners, package managers) expect a top-level `LICENSE` file for automated detection.
- Maintains legal attribution via the short header while reducing duplication.

## Migration guidance

Optionally, you can create a small script or use an existing tool to replace full license blocks with the short header for files where maintainers agree this is safe. Keep a commit that documents the mass update.

## Automation

When adding files, include the short header automatically via project templates, add-file hooks, or IDE file templates. Example Git hook or CI step can verify that new files contain the short header.

## Examples

F# file header example (recommended):

```fsharp
(*
Copyright (c) 2026+ bookofproofs
See LICENSE in the project root for license terms.
*)
```

Markdown README or other documentation files should include a short note pointing to the LICENSE file, or include the license only where necessary.

## Contact

If you need to change the license text or adopt a different header policy, open an issue or a PR and document the reasons in the PR description.
