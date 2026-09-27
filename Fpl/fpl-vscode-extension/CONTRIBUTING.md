# Contributing Guidelines

## Purpose

This document defines contribution guidelines and project-wide policies for the repository, including the policy for licensing headers in source files.

## License Policy

This project uses the MIT License. Maintain a single canonical `LICENSE` file at the repository root containing the full text of the MIT license.

Source files should not duplicate the full license text. Instead, each source file should include a short header that references the canonical `LICENSE` file and the copyright owner. This keeps files concise while preserving legal attribution.

### Recommended approach

For your own contribution, attribute yourself in our [CONTRIBUTORS.md](./CONTRIBUTORS.md) file.

If you use third party code, attribute it in our [THIRD_PARTY_NOTICE.md](./THIRD_PARTY_NOTICE.md) file.

Why this approach?

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

## Conventions for your pull-requests
* Create pull requests using the following naming conventions:
* ```fix/<descriptive_repository_name>``` - focus on bug fixes
* ```feat/<descriptive_repository_name>``` - focus on a new feature
* ```refactor/<descriptive_repository_name>``` - focus on refactoring
* ```test/<descriptive_repository_name>``` - focus on test coverage
* ```doc/<descriptive_repository_name>``` - focus on documentation

## Getting Started and How to Test Your Code

- For trying out the solution: 
  - It is convenient to use Visual Studio.
  - Open the main solution file located at fpl.net/Fpl/Fpl.sln. 
  - You can run .NET releated unit tests there.
- For trying out the VS Code extension: 
  - Install `Node.js` (if not already available on your system)
  - Run `npm install` from the folder `fpl.net/Fpl/fpl-vscode-extension`
  - Use Visual Studio Code.
  - Open fpl.net/Fpl/fpl-vscode-extension as folder
  - Press F5 to start the debugging session.
