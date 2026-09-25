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

## Contact

If you need to change the license text or adopt a different header policy, open an issue or a PR and document the reasons in the PR description.

## Changelog Policy

This project maintains a [`CHANGELOG.md`](./CHANGELOG.md) at the repository root, following the [Keep a Changelog](https://keepachangelog.com/en/1.1.0/) format.

### Rules for updating the changelog

* Every pull request that changes user-observable behavior (new features, bug fixes, breaking changes, removals) **must** add an entry under the `## [Unreleased]` section at the top of `CHANGELOG.md`.
* Entries are grouped by change type, using the standard Keep a Changelog headings:
  * `### Added` - for new features.
  * `### Changed` - for changes in existing functionality.
  * `### Deprecated` - for soon-to-be removed features.
  * `### Removed` - for now removed features.
  * `### Fixed` - for any bug fixes.
  * `### Security` - in case of vulnerabilities.
* Each bullet point must be tagged with one or more of the following, depending on which part of the solution it affects:
  * **[vscode]** - the VS Code extension
  * **[language-server]** - the FPL Language Server
  * **[interpreter]** - the FPL interpreter
  * **[parser]** - the FPL parser / grammar
  * **[other]** - documentation, test suite, tooling, .NET version, etc.
* Pull requests that are purely internal (refactors with no observable effect, test-only changes, CI tweaks) are not required to add a changelog entry, but may do so under `[other]` if useful for traceability.
* Maintainers move entries from `[Unreleased]` into a new dated, versioned section (e.g. `## [v5.2.0] - 2026-10-01`) as part of cutting a release, following Semantic Versioning (MAJOR.MINOR.PATCH).

### Example entry

```markdown
## [Unreleased]
### Fixed
- **[interpreter]** Corrected false positive in `SIG04` diagnostics for nested corollaries.
```

## How to contribute?
It is important to coordinate the project FPL Interpreter among the team.
* In the beginning, get in touch with the team via [Discussions](https://github.com/bookofproofs/fpl/discussions). 
* Please propose the work items you would like to focus on in the Discussions section. Please also describe your anticipated solution, and be sufficiently specific. 
* Next, agree with the team upon the work items you will get assigned to cover.
* Implement the work items.
* If necessary, create unit tests related to the new code. 
* Test your repository against your new and the existing unit tests.
* Before creating a pull request, verify if the number of failed unit test got greater than the number you got before you implemented the change. Ideally, no unit tests should fail before creating the pull request. 
* Update `CHANGELOG.md` as described in the Changelog Policy above, if applicable.
* Create a pull request.

## Conventions for your pull-requests
* Never push your repository directly into the `main` branch.
* Instead, create pull requests using the following naming conventions:
* ```fix/<descriptive_repository_name>``` - focus on bug fixes
* ```feat/<descriptive_repository_name>``` - focus on a new feature
* ```refactor/<descriptive_repository_name>``` - focus on refactoring
* ```test/<descriptive_repository_name>``` - focus on test coverage
* ```doc/<descriptive_repository_name>``` - focus on documentation