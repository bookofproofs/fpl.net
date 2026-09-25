# Release Notes

This page summarizes the notable releases of the **FPL solution** (parser, interpreter, language server, and VS Code extension).

*For detailed, version-by-version entries (including every bugfix), see the full [CHANGELOG.md](https://github.com/bookofproofs/fpl.net/blob/main/CHANGELOG.md).*

___

## [5.1.1] - 2026-09-25
A major leap since v1.7.0: a complete interpreter overhaul, a vastly expanded diagnostics system, a much richer FPL syntax, and the VS Code extension's transformation into a full mathematical IDE.

### Parser
- User-defined prefix/postfix/infix operators and mathematical symbols, infix expressions without mandatory parentheses, and Unicode math notation (`∃`, `∀`, `¬`, `∃!`, dotted logical connectives).
- Numerous syntax simplifications (optional definition blocks/inherited types, simplified constructors/quantifiers) and new keywords (`base`, `bydef`, `val`/`validity`).
- A major grammar rewrite with stricter whitespace control and a much-improved regex-based error-recovery/diagnostics engine.

### Interpreter
- Complete architectural overhaul: the `FplBlockType` enum was replaced by a polymorphic `FplValue`/`FplGenericNode` class hierarchy, powering a real **`Run`** execution engine for actual proof evaluation.
- Diagnostics expanded from a handful of codes to well over a hundred across `ID`, `VAR`, `SIG`, `PR`, `LG`, `SY`, `ST`, `NSP`, and `NG` families.
- New core abstractions for inference/validity, inheritance, array types, and reference resolution; solution split into `Fpl0Base`, `Fpl1Parser`, `Fpl2Interpreter` with consolidated tests.

### Language Server
- Migrated to a new F#-based **`Fpl3LanguageServer`** project; improved symbol-table navigation powering the VS Code tree view; multiple `OmniSharp` upgrades and a dedicated garbage collector.

### VS Code Extension
- New navigation tree view and a KaTeX-rendered valid-statements webview panel with source navigation.
- Syntactical autocompletion, syntax highlighting, and color-theme support.
- Adopted Microsoft's official `@VS Code Extension/dotnet-runtime` API for on-demand runtime downloads; converted the extension into a proper `.esproj`/TypeScript project.
- New automated pre-publish pipeline syncing DLLs, version, changelog, and release notes.

### Other
- Added an `fsdocs`/DocFX documentation site on GitHub Pages; numerous CI/CD improvements.

## [1.7.0] - 2024-11-18
Introduced the first navigation-oriented tooling for the VS Code extension, backed by a new symbol table, alongside substantial diagnostics and signature-matching refinements.

### Parser
- New `LanguageCode` parser and distinction between brackets and parentheses.

### Interpreter
- `SIG04` diagnostics extended to infix/prefix/postfix operations; new `PR004`/`PR005` and `ID014`/`ID015` diagnostics; initial `return` statement evaluation.
- Simplified quantifier syntax; major refactoring of `FplValue` naming/signatures and signature matching.
- Numerous false-positive/negative fixes across `ID`, `VAR`, `LG`, and `SIG` diagnostics, plus naming/precedence bugfixes.

### Language Server
- `DocuSymbolTable` navigation and document symbol improvements.

### VS Code Extension
- New navigation tree view with colors and icons; removed `Scope` sub-nodes for cleaner presentation.

### Other
- Upgraded to OmniSharp 0.17.0 and refreshed NuGet packages.

## [1.6.0] - 2024-09-09
Introduced the first alpha version of the FPL interpreter, a substantially expanded diagnostics system, the initial VS Code tree view, and migration to .NET 8.0.

### Interpreter
- Initial FPL interpreter (alpha): evaluation of conjunction, disjunction, xor, implication, and equivalence expressions.
- Many new diagnostics (`ID`, `VAR`, `PR`, `LG`, `GEN`, `SIG`, `NSP` families); new garbage collector, tokenizer, and circular-import detection.
- Simplified inheritance syntax; centralized evaluation-stack model; extensive bugfixes across block-type representation and `uses`-clause loading.

### Language Server
- Fixed clearing of published diagnostics and completion-handler URI handling.

### VS Code Extension
- New tree view for symbol navigation and configuration properties; fixed a `spawn UNKNOWN` error.

### Other
- Migrated to .NET 8.0; renamed registry/library projects; simplified diagnostics code; removed obsolete `.runsettings`.

## [1.5.0] - 2024-02-03
Brought a much richer FPL grammar for user-defined mathematical notation and infix operators, with corresponding autocompletion and highlighting improvements.

### Parser
- Newest FPL parser with inbuilt math-operator parsers; support for user-defined prefix/postfix/infix notation; new keywords `base` and `bydef`.
- Grammar simplifications for namespaces, theorem-like statements, and corollaries; removed indexed predicates.
- Numerous bugfixes for `xor`, proof/corollary references, predicate qualification, and statement syntax.

### VS Code Extension
- Auto-completion for infix/prefix/postfix and user-defined symbols; new equality snippet and snippet bugfixes.

### Language Server
- Minor bugfixes alongside the parser update.

### Other
- Reorganized localization strings; added a README/extension-listing GIF; updated CHANGELOG/README.

## [1.4.0] - 2023-11-17
Introduced the VS Code extension's syntactical autocompletion service, along with new syntax-highlighting features.

### Parser
- Fixed grammar for argument identifiers and `assume` arguments.

### VS Code Extension
- New syntactical autocompletion service and syntax highlighting for argument identifiers, markdown, and code examples.
- Numerous autocompletion bugfixes across most FPL block types.

## [1.3.0] - 2023-11-04
Brought a major rewrite of the parser's error-recovery and diagnostics engine, alongside grammar updates and several extension polish fixes.

### Parser
- Rewrote error recovery and diagnostics using a regex-based approach; new `Interval`/`Positions` types.
- FPL syntax updated to version 2.4.2; unified grammar types; added alias/dotted-identifier/signature error recovery.
- Fixed numerous false positives, infinite loops, and positioning issues in error recovery and diagnostics.

### Language Server
- Diagnostics now come directly from the FPL parser instead of being mocked.

### VS Code Extension 
- New support for standard color themes.
- Fixed fatal-error display, version-number display, comment highlighting, install/start synchronization, and logging issues.

## [1.2.0] - 2023-09-03
Marked the extension's earliest foundational milestones: the first syntax-highlighting release and FPL Language Client/Server integration.

### Parser
- Initial parser migrated from the python-based **tatsu** version in the old, [python-bassed FPL Repository](https://github.com/bookofproofs/fpl) to .NET F#/FParsec-based in this repository.

### Language Server
- FPL Language Client and Server added, starting automatically on `.fpl` file open; diagnostics wired into the extension.

### VS Code Extension
- Initial release with syntax highlighting; icon, description, categories, and keywords added.
- Replaced the bundled Windows-only dotnet runtime with on-demand downloads for Windows, Linux, and macOS.

### Other
- Refactored logging to console.
