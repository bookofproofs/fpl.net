# Release Notes

This page summarizes the notable releases of the **FPL solution** (parser, interpreter, language server, and VS Code extension).

*For detailed, version-by-version entries (including every bugfix), see the full [CHANGELOG.md](https://github.com/bookofproofs/fpl.net/blob/main/CHANGELOG.md).*

___

## [5.1.1] - 2026-09-25
This release marks a major leap for the FPL solution since v1.7.0, spanning nearly two years of continuous development: a complete architectural overhaul of the interpreter, a vastly expanded diagnostics system, a much richer FPL syntax, and the transformation of the VS Code extension into a fully-fledged mathematical IDE.

### [parser]
- **Infix expressions without mandatory parentheses**, dedicated `InfixOp`/`PrefixOp`/`PostfixOp` parsers, and support for **user-defined prefix, postfix, and infix mathematical/object symbols**.
- **Unicode math notation**: `∃`, `∀`, `¬`, `∃!` for quantifiers/negation, and dotted infix notation for conjunction, disjunction, implication, equivalence, xor, and the FPL's `is` operator.
- Numerous **syntax simplifications**: optional definition blocks, optional inherited types, removal of template parameters, removal of the `~` variable-declaration marker, removal of `self`/`parent` as usable variable names, simplified constructor and quantifier syntax.
- New keywords `base`, `bydef`, `val`/`validity`; `LanguageCode` parser; `Interval`/`Positions` types.
- A major grammar rewrite with stricter whitespace control, splitting the monolithic evaluator into focused modules, and a much-improved error-recovery/diagnostics engine (regex-based recovery replacing older ad hoc logic).

### [interpreter]
- **Complete architectural overhaul**: the `FplBlockType` enum (~30 cases) was replaced by a polymorphic `FplValue`/`FplGenericNode` class hierarchy (`FplClass`, `FplPredicate`, `FplFunctionalTerm`, `FplConstructor`, etc.), each implementing its own `Run`/`Represent` — the foundation of true FPL program execution.
- Introduction of the **`Run` execution engine**, enabling actual interpretation of conjunctions, disjunctions, implications, equivalences, negations, cases, assignments, and array/variable operations — evolving from a purely structural representation into a real interpreter.
- **Massive expansion of the diagnostics system**: from a handful of codes in v1.7.0 to well over a hundred distinct diagnostics across `ID`, `VAR`, `SIG`, `PR`, `LG`, `SY`, `ST`, `NSP`, and `NG` families, covering signature mismatches, proof-justification errors, syntax errors, and localization/statement issues — plus multiple large-scale rewording passes to improve clarity.
- New core abstractions: `IInferrable`/`IValid`/`ValidStmtStore` for rules of inference and proof validity, `IInherit`/`FplGenericInheriting` for class/functional-term inheritance, `IHasDimensions`/`FplVariableArray` for array types, `IRefersTo` for reference resolution, and `FplUndetermined` for type-safe placeholder values.
- Solution-wide modularization: the monolithic `Fpl` project was split into `Fpl0Base`, `Fpl1Parser`, and `Fpl2Interpreter`, with consolidated, better-organized test suites.
- Renamed "quantor" → "quantifier" and "Proceeding" → "Preceding" throughout, for more idiomatic, accurate terminology.

### [language-server]
- Migrated from a C# implementation into a new **F#-based `Fpl3LanguageServer`** project, including `TextDocumentSyncHandler`, `DiagnosticHandler`, `TextPositions`, and completion handling.
- **`SymbolTable`** navigation and document-symbol improvements powering the VS Code tree view.
- Multiple `OmniSharp.Extensions.LanguageServer` upgrade cycles, refactored diagnostics storage/logging, and a dedicated garbage collector to keep symbol resolution fast and correct across large theory files.

### [vscode]
- **Navigation tree view** with icons and colors, backed by the symbol table, plus a dedicated **valid-statements webview panel** — a sortable, KaTeX-rendered table (including rules of inference) with double-click navigation to source locations and persisted window layout.
- **Syntactical autocompletion**, including infix/prefix/postfix/user-defined mathematical and object symbols, syntax highlighting for argument identifiers and embedded code examples, and standard color-theme support.
- Adoption of Microsoft's official **`@vscode/dotnet-runtime`** acquisition API, replacing a custom dotnet-download mechanism, and on-demand runtime downloads for Windows/Linux/macOS instead of bundling.
- Converted the extension into a proper **`.esproj` project** integrated into the .NET solution, with the JavaScript codebase **fully migrated to TypeScript**.
- New **automated pre-publish pipeline**: syncing language-server DLLs, the extension's version number, and a dedicated extension-specific `CHANGELOG.md`/`RELEASE_NOTES.md`, all generated from the centralized repository files.

### [other]
- Migrated the solution to **.NET 8.0**.
- Added an **`fsdocs`/DocFX-based documentation site** published to GitHub Pages, separate from the `README.md` landing page.
- Numerous CI/CD improvements for build, test, and documentation-deployment workflows.

## [1.7.0] - 2024-11-18
This release introduced the first navigation-oriented tooling for the VS Code extension, backed by a new symbol table, alongside a substantial round of diagnostics and signature-matching refinements in the interpreter.

### [parser]
- New **`LanguageCode`** parser and distinction between brackets and parentheses.

### [interpreter]
- **`SIG04`** diagnostics extended to infix, prefix, postfix operations and types.
- New **`PR004`/`PR005`** and **`ID014`/`ID015`** diagnostics; justified-argument `BlockType` inference.
- Initial evaluation of **`return`** statements.
- Simplified quantifier syntax (removed the `in` keyword).
- Major refactoring of `FplValue` naming, signatures, and representation (`ReprId`; removal of `FplRepresentation.Pointer`, `AdjustNameAndSignature`, and `NameIsFinal` logic).
- Refactored signature matching (`matchArgumentsWithParameters`, `matchParamsWithArguments`, `findClassInheritanceChain`), including for derived class types.
- Numerous false-positive/negative fixes across `ID001`–`ID013`, `VAR00`–`VAR03`, `LG001`, `SIG00`–`SIG04` diagnostics involving classes, corollaries, quantifiers, references, delegates, and constructor calls.
- Corrected naming/type-signature propagation for arguments, coordinates, delegates, extensions, functional terms, dotted/indexed predicates, and variables.
- Fixed infix notation precedence and position bugs for infix/postfix/prefix operations; fixed a crash in `matchArgumentsWithParameters` when there are no arguments at all, and an empty-stack crash in `PopEvalStack`.

### [language-server]
- **`DocuSymbolTable`** navigation and document symbol improvements.

### [vscode]
- New **navigation tree view** with colors and icons, backed by the new symbol table.
- Removed `Scope` sub-nodes from the tree view for a cleaner presentation.

### [other]
- Upgraded to **OmniSharp 0.17.0** and refreshed NuGet package versions.

## [1.6.0] - 2024-09-09
This release introduced the first alpha version of the FPL interpreter, alongside a substantially expanded diagnostics system, the initial tree view for the VS Code extension, and the solution's migration to .NET 8.0.

### [interpreter]
- **Initial FPL interpreter (alpha version)**: evaluation of conjunction, disjunction, xor, implication, and equivalence expressions; an initial delegate module.
- New diagnostics: **`ID002`–`ID013`**, **`VAR00`–`VAR03`**, **`PR000`–`PR002`**, **`LG000`/`LG001`**, **`GEN00`**, **`SIG00`–`SIG04`**, **`NSP003`–`NSP005`**.
- New **garbage collector**, tokenizer, and circular theory-usage/import detection.
- Simplified syntax for class inheritance (templates and extensions no longer allowed).
- Replaced the `EvaluationType`/`EvalContext` approach with a centralized evaluation-stack model in the symbol table.
- Removed `FplValueType.Expression`; replaced string-based `FplRepresentation` with a custom type; removed class instance properties from the syntax (replaced by zero-arity functional terms).
- Extensive bugfixes to `FplId`, `FplRepresentation`, `TypeSignature`, `QualifiedName`/`QualifiedStartPos`, and `NameIsFinal` across all FPL block types (theorems, corollaries, proofs, functional terms, delegates, properties, constructors).
- Fixed multiple loading of the same `uses` clause from different locations.
- **`loadAllUsesClauses`** now supports dynamic loading/replacement of edited FPL files in the symbol table.
- Corrected the error position for **`NSP003`/`NSP004`** circular-import detection.

### [language-server]
- Fixed clearing of published diagnostics and completion-handler URI handling.

### [vscode]
- New **tree view** for symbol table navigation; new configuration properties (`vsfplconfig.json`).
- Fixed the `spawn UNKNOWN` error in the extension.

### [other]
- Renamed registry/library projects; updated `.gitignore` and current dotnet runtimes.
- Simplified `DiagnosticCode`; added `acquireSources` for locating FPL library sources.
- Added **.NET 8.0** libraries; **migrated the solution to .NET 8.0**.
- New **`EvalAliasedNamespaceIdentifier`** evaluation function and tests; new **`tryFindAndParseUsesClauses`**.
- Refactored diagnostics record types; added `lib`/`libmap.txt` for FPL.

## [1.5.0] - 2024-02-03
This release brought a much richer FPL grammar for user-defined mathematical notation and infix operators, along with corresponding autocompletion, syntax-highlighting, and snippet improvements in the VS Code extension.

### [parser]
- **Newest FPL parser**, with inbuilt parsers for math operators replacing regex-based matching, and improved error reporting for infix/postfix/prefix symbols.
- **Support for user-defined prefix, postfix, and infix notation** for mathematical operators in expressions; parenthesized expressions and an infix operation grammar extension.
- New keywords **`base`** (disambiguating parent-class calls from indexed predicates) and **`bydef`**.
- Grammar simplifications: namespaces, theorem-like statements, conjectures, and corollaries (removed premise/conclusion from predicates; discontinued double-lines in snippets).
- Removed indexed predicates; refactored `predicateList1`.
- Numerous bugfixes: syntax for `xor` and alternating variable declarations/arguments in proofs; referencing identifiers in proofs and corollaries; indexed/dotted predicates and mixed qualification; dollar-qualifier and `for`-statement (`in type`) syntax; corollary references in proofs.

### [language-server]
- Minor bugfixes alongside the parser.

### [vscode]
- **Auto-completion for infix, prefix, postfix, and user-defined mathematical/object symbols**.
- New **equality snippet**; fixes to the "Exists n-times" snippet and a `getLineOffset` bugfix.

### [other]
- Localization strings reorganized for infix/postfix/prefix/symbol keywords.
- Animated GIF added to the README/extension listing.
- CHANGELOG and README updates for the FPL extension.

## [1.4.0] - 2023-11-17
This release introduced the VS Code extension's syntactical autocompletion service, along with new syntax-highlighting features and a corresponding grammar fix in the parser.

### [parser]
- Fixed grammar for argument identifiers and `assume` arguments.

### [vscode]
- New **syntactical autocompletion service**.
- New **syntax highlighting** for argument identifiers and code markdown; code-example highlighting and insertions in comments.
- Numerous bugfixes across autocompletion for axioms, definitions, theorems, lemmas, conjectures, properties, proofs, corollaries, quantors, and delegates. 

## [1.3.0] - 2023-11-04
This release brought a major rewrite of the parser's error-recovery and diagnostics engine to a regex-based approach, alongside grammar updates, an early language-server diagnostics milestone, and several VS Code extension polish fixes.

### [parser]
- **Rewrote error recovery and diagnostics using a regex-based approach**; refactored error codes, messages, and the recovery map.
- New **`Interval`** and **`Positions`** types.
- FPL syntax unified grammar types into the `ast` type.
- Alias recovery; error recovery for dotted identifiers, signatures, and inference blocks; alternative `PascalCaseId` diagnostics.
- Fixed false positives between declarations and translations; consecutive/duplicate diagnostics; infinite loops in error recovery; string-manipulation and namespace-related error positions; class identifiers, ranges/coordinates, standard error recovery, and template syntax highlighting.

### [language-server]
- Diagnostics now come directly from the FPL parser instead of being mocked (no error recovery yet, so at most one syntax error per file is reported).

### [vscode]
- New support for **standard color themes**.
- Fixed improved display of fatal errors; extension version-number display bug; syntax highlighting for comments; synchronization of install and start processes; logging and installation issues, including excessive `node_modules` exclusions.

## [1.2.0] - 2023-09-03
This release marks the extension's earliest foundational milestones: the initial syntax-highlighting release, the first FPL Language Client/Server integration, and the transition from a bundled dotnet runtime to on-demand downloads.

### [parser]
- Initial parser migrated from the python-based **tatsu** version to .NET FParsec-Based version

### [interpreter]
- no interpreter functionality yet

### [language-server]
- **FPL Language Client and Server added**; starts automatically when an `.fpl` file is opened.
- **Diagnostics from the FPL parser wired into the extension.**
- *Known issue:* server failed to start due to an incorrect path to the DLL (fixed in a later release).

### [vscode]
- **Initial release with syntax highlighting.**
- Extension icon, description, categories, and keywords added.
- Bundled dotnet runtime for Windows x64 and compiled DLL for the initial FPL Language Server; later replaced with **dotnet runtimes for Windows, Linux, and macOS x64, downloaded on demand** rather than bundled with the extension.
- `activationEvents` added to suppress `vsce` packaging errors; release notes updated.
