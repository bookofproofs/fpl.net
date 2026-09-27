# Change Log

All notable changes to the VS Code extension **FPL (Formal Proving Language)**
are documented in this file. For a full list of changes of the **FPL solution**
(including parser, interpreter, language server, ...),
see [CHANGELOG.md](https://github.com/bookofproofs/fpl.net/blob/main/CHANGELOG.md).

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to Semantic Versioning (MAJOR.MINOR.PATCH).

## [v5.1.1] - 2026-09-25
### Added
- **[vscode]** Automate syncing and checking vscode-extension specific version number, CHANGELOG and RELEASE NOTES with the corresponding centralized repository files

## [v5.1.0] - 2026-09-21
### Changed
- **[vscode]** Converted the `fpl-vscode-extension` folder into a proper `.esproj` project within the solution; migrated `extension.js`/`webViewPanel.js` to TypeScript; adopted `@vscode/dotnet-runtime` (Microsoft's official acquisition API) instead of a custom dotnet download mechanism.
### Added
- **[vscode]** Automated pre-publish step to copy language server DLLs into the VS Code extension; `launch.json` for extension debugging.
### Fixed
- **[vscode]** `.gitignore` and `.vscodeignore` corrected for the new TypeScript-based extension; extension compile errors resolved (`tsconfig.json` `moduleResolution: bundler`); removed undefined command from `package.json`.

## [v5.0.2] - 2026-08-22
### Fixed
- **[vscode]** Replaced deprecated `url.resolve` with the WHATWG URL API to avoid activation race conditions.

## [v5.0.1] - 2026-07-06
### Added
- **[vscode]** New webview panel showing valid statements as a sortable, KaTeX-rendered table (including rules of inference), replacing the earlier JSON-based tree view for this feature.
- **[vscode]** Double-click navigation from the valid-statement table to the corresponding source location; window layout persistence for the panel; a dedicated "FPL" command submenu.
### Changed
- **[vscode]** Tree view now refreshes without arbitrarily collapsing nodes, preserving user-expanded state where possible.

## [v4.6.0] - 2026-04-21
### Added
- **[vscode]** Tree provider and request handler for retrieving valid statements from the language server; markdown tooltips for expression previews.

## [v3.0.0] - 2025-08-17
### Fixed
- **[vscode]** Fixed downloading of the redirected dotnet runtime library in the extension.

## [v1.8.0] - 2024-11-21
### Fixed
- **[vscode]** Fixed disappearing symbol table while navigating the tree view; added tooltips to the tree view.

## [v1.7.0] - 2024-11-18
### Added
- **[vscode]** Navigation tree view with colors and icons, backed by the new symbol table.
### Changed
- **[vscode]** Removed `Scope` sub-nodes from the tree view for a cleaner presentation.
### Fixed
- **[vscode]** Tree view rendering bug; colors interfering with debug console logs.

## [v1.6.3] - 2024-10-03
### Added
- **[vscode]** Type info shown in the VS Code object explorer; main theory marked in the explorer.

## [v1.6.2] - 2024-09-29
### Fixed
- **[vscode]** Missing syntax-error diagnostics display; README updated.

## [v1.6.0] - 2024-09-09
### Added
- **[vscode]** Tree view for symbol table navigation; configuration properties (`vsfplconfig.json`).

## [v1.5.4] - 2024-03-21
### Fixed
- **[vscode]** `spawn UNKNOWN` error in the extension.

## [v1.5.1] - 2024-03-03
### Fixed
- **[vscode]** Duplicate items in auto-completion.

## [v1.5.0] - 2024-02-03
### Added
- **[vscode]** Auto-completion for infix, prefix, postfix, and user-defined mathematical/object symbols.

## [v1.4.1] - 2023-11-17
### Added
- **[vscode]** Equality snippet.
### Fixed
- **[vscode]** "Exists n-times" snippet; `getLineOffset` bugfix.

## [v1.4.0] - 2023-11-17
### Added
- **[vscode]** Syntactical autocompletion service.
- **[vscode]** Syntax highlighting for argument identifiers and code markdown; code-example highlighting and insertions in comments.
### Fixed
- **[vscode]** Numerous bugfixes across autocompletion for axioms, definitions, theorems, lemmas, conjectures, properties, proofs, corollaries, quantors, and delegates.

## [v1.2.9] - 2023-09-30
### Fixed
- **[vscode]** Improved display of fatal errors.

## [v1.2.7] - 2023-09-08
### Added
- **[vscode]** Support for standard color themes.
### Fixed
- **[vscode]** Extension version-number display bug.

## [v1.2.5] - 2023-09-06
### Fixed
- **[vscode]** Syntax highlighting for comments.

## [v1.2.4] - 2023-09-06
### Fixed
- **[vscode]** Syntax highlighting for comments.

## [v1.2.3] - 2023-09-06
### Fixed
- **[vscode]** Syntax highlighting for comments.

## [v1.2.2] - 2023-09-05
### Fixed
- **[vscode]** Synchronization of install and start processes.

## [v1.2.1] - 2023-09-04
### Fixed
- **[vscode]** Logging and installation issues, including excessive `node_modules` exclusions.

## [v1.2.0] - 2023-09-03
### Added
- **[vscode]** Dotnet runtimes for Windows, Linux, and macOS x64, downloaded on demand rather than bundled with the extension.
### Changed
- **[vscode]** `activationEvents` added to suppress `vsce` packaging errors; release notes updated.

## [v1.1.1] - 2023-09-03
### Added
- **[vscode]** Bundled dotnet runtime for Windows x64 and compiled DLL for the initial FPL Language Server.

## [v1.0.1] - 2023-08-31
### Added
- **[vscode]** Extension icon, description, categories, and keywords.

## [v1.0.0] - 2023-08-29
### Added
- **[vscode]** Initial release with syntax highlighting.
