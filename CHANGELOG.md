# Changelog

All notable changes to the Fpl solution (parser, interpreter, language server, and VS Code extension) are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to Semantic Versioning (MAJOR.MINOR.PATCH).

Each entry is tagged with the part(s) of the solution it affects:

- **[vscode]** - the change focusses on the vs code extension
- **[language-server]** - the FPL Language Server was changed
- **[interpreter]** - the FPL interpreter was changed
- **[parser]** - the FPL parser / grammar was changed
- **[other]** - something else was changed, like documentation, test suite, .net version, etc.

## [Unreleased]
- (to come)

## [v5.1.0] - 2026-09-21
### Changed
- **[interpreter]** Split the monolithic `Fpl` project into separate libraries: `Fpl0Base`, `Fpl1Parser`, `Fpl2Interpreter`.
- **[language-server]** Moved `FplLSLib` into a new `Fpl3LanguageServer` project; replaced the C# language server implementation with an F#-based one (migrated `TextDocumentSyncHandler`, `FplLsTraceLogger`, `DiagnosticHandler`, `TextPositions`, and the completion handler to F#).
- **[interpreter]** Consolidated test projects (`FplParser.Tests`, `FplInterpreter.Tests`, `FplLSTests`, `TestFplLsLib`, `TestConfig`) into `TestFpl1Parser`, `TestFpl2Interpreter`, and `TestFpl3LanguageServer`.
- **[vscode]** Converted the `fpl-vscode-extension` folder into a proper `.esproj` project within the solution; migrated `extension.js`/`webViewPanel.js` to TypeScript; adopted `@vscode/dotnet-runtime` (Microsoft's official acquisition API) instead of a custom dotnet download mechanism.
- **[other]** Added an `fsdocs`/DocFX-based documentation site published to GitHub Pages as a subfolder, separate from the repository `README.md` landing page.
### Added
- **[vscode]** Automated pre-publish step to copy language server DLLs into the VS Code extension; `launch.json` for extension debugging.
### Fixed
- **[other]** Numerous CI/CD fixes for the DocFX/`fsdocs` GitHub Pages workflow (deploy environment, working directory, multiline YAML parameters, artifact-only builds to avoid races with the standard Pages workflow).
- **[vscode]** `.gitignore` and `.vscodeignore` corrected for the new TypeScript-based extension; extension compile errors resolved (`tsconfig.json` `moduleResolution: bundler`); removed undefined command from `package.json`.

## [v5.0.3] - 2026-09-08
### Changed
- **[interpreter]** Continued documentation pass adding XML-style doc comments across `SymbolTable.*`, `Diagnostics.fs`, `Emitter.fs`, `BasicTypes.fs`, and the grammar/parser modules; renamed the `FplInterpreter` namespace to the more consistent `Fpl.Interpreter`.
- **[interpreter]** Renamed the global diagnostics accumulator to `diagnosticsContainer` for clarity.
### Fixed
- **[interpreter]** Improved `PR008` diagnostic messages (better context when matching plain variables against premise patterns); reduced duplicated code for producing and formatting expression-matching error messages.
- **[interpreter]** Corrected `ID013` diagnostic position and removed a false positive.

## [v5.0.2] - 2026-08-22
### Added
- **[interpreter]** New `SIG06` diagnostics for array assignments with mismatched value types; `PR022` diagnostics for non-inferable proof elements; `SY013`/`SY014` diagnostics for unnecessary/conflicting parentheses in infix operations based on precedence.
### Changed
- **[interpreter]** Large-scale rewording pass improving clarity and test coverage of diagnostics `SIG00`–`SIG13`, `ST*`, `VAR00`–`VAR11`, `ID001`–`ID027`, `LG001`–`LG005`, `NG000`–`NG005`, and `PR001`–`PR022`.
- **[interpreter]** Removed `PR018` diagnostics (did not reflect real proof-based mathematics — a "trivial" argument does not require exactly one justification).
- **[interpreter]** Renamed "quantor" to "quantifier" throughout the codebase for more idiomatic terminology; renamed `Proceeding` to `Preceding` where it was semantically incorrect.
- **[interpreter]** Removed the `Alternative` field from the `Diagnostic` record, folding alternative text into the main message for a cleaner diagnostic structure.
### Fixed
- **[parser]** Fixed a grammar bug where negation didn't correctly consume a prefix expression (`¬f⇒a` was parsed as `¬(f⇒a)`); corrected matching of the `is` operator and infix operator precedence.
- **[interpreter]** Corrected `NSP05` to scope emission to the specific alias being resolved instead of rescanning everything.
- **[vscode]** Replaced deprecated `url.resolve` with the WHATWG URL API to avoid activation race conditions.

## [v5.0.1] - 2026-07-06
### Added
- **[vscode]** New webview panel showing valid statements as a sortable, KaTeX-rendered table (including rules of inference), replacing the earlier JSON-based tree view for this feature.
- **[vscode]** Double-click navigation from the valid-statement table to the corresponding source location; window layout persistence for the panel; a dedicated "FPL" command submenu.
- **[interpreter]** New bare-JSON-array export of valid statements to simplify VS Code extension consumption.
### Changed
- **[vscode]** Tree view now refreshes without arbitrarily collapsing nodes, preserving user-expanded state where possible.

## [v5.0.0] - 2026-06-28
### Changed
- **[parser]** Major parser rewrite: hooked up dedicated `InfixOp`/`PrefixOp`/`PostfixOp` parsers, removed most parsing optionality in favor of stricter whitespace control to improve error messages; split the large monolithic evaluator function into focused modules (14-part refactor); reorganized namespaces and folders to mirror solution structure.
- **[interpreter]** Renamed "quantor" internals continued; regrouped symbol-table modules (rules of inference, quantifiers, proofs, predicates) into dedicated files.
- **[other]** Multiple rounds of `OmniSharp.Extensions.LanguageServer` upgrades (0.17.0 → 18.0 → 18.3 → 19.0 → 19.9), ultimately settling back on 0.17.4 after compatibility issues; adapted `CompletionItem` usage to the new immutable record-based API.
### Added
- **[parser]** Support for infix expressions without mandatory parentheses; `Ast.Parens` node; faster dedicated parsers for prefix/postfix/infix/object symbols.
### Fixed
- **[interpreter]** Numerous unit-test corrections following the OmniSharp and namespace-reorganization changes.

## [v4.8.0] - 2026-05-31
### Added
- **[parser]** Dotted infix notation for conjunction, disjunction, implication, equivalence, xor, and the `is` operator; Unicode math symbols `∃`, `∀`, `¬`, and `∃!` for quantifiers and negation; `SY001`–`SY014` diagnostics covering missing braces, parentheses, brackets, semicolons, and identifiers across nearly all FPL block types.
- **[interpreter]** `PR020`/`PR021` diagnostics for mismatched premises and inferred expressions in rules of inference.
### Changed
- **[parser]** Simplified variable declaration syntax (removed the `~` marker); reworked whitespace handling for commas, parentheses, and braces to make syntax errors more precise.

## [v4.7.0] - 2026-05-12
### Added
- **[interpreter]** `PceedingExprCandidates`/`ProceedingExprCandidates` implemented across all justification item kinds (`by def`, `by def var`, `by cor`, `by conj`, `by theorem-like stmt`, `by inf`, `by proof argument`, `trivial`, `assume`, `revoke`), enabling inference-based checking of proof conclusions.
- **[interpreter]** `PR017`–`PR021` diagnostics for mismatched expressions and mixed justification items.
### Fixed
- **[interpreter]** Multiple false positives in `PR003`, `PR008`, `PR009`, `PR019`, `PR020`, and matching-parameters-with-arguments (`mpwa`) logic, particularly around quantifier variable matching and open-formula comparisons.

## [v4.6.0] - 2026-04-21
### Added
- **[interpreter]** `IInferrable`/`IValid` interface implemented for axioms, assertions, and rules of inference, feeding a centralized `ValidStmtStore`.
- **[vscode]** Tree provider and request handler for retrieving valid statements from the language server; markdown tooltips for expression previews.
### Changed
- **[interpreter]** Reorganized `FplInterpreterGlobals`, moving symbol table, heap, and validity-storage types into dedicated modules to reduce coupling and centralize global singletons.

## [v4.5.0] - 2026-04-03
### Changed
- **[interpreter]** Split `FplGenericPredicate`/related classes to properly represent `Value` semantics; formalized `IVariable.IsBound`.
- **[interpreter]** Continued modularization of `FplInterpreterTypes` (moved compound predicates, delegates, extensions, quantifiers, and proof-related nodes into separate modules).
### Fixed
- **[interpreter]** `SIG12`/`SIG13` positioning fixes; `LG001`/`LG003` diagnostics correctness for conjectures and definition properties.

## [v4.4.0] - 2026-03-23
### Added
- **[interpreter]** `FplUndetermined` class representing default values for consumers whose value cannot be determined but remains type-compatible.
### Changed
- **[interpreter]** Split `FplGenericNode` into `FplGenericHasValue`/`FplGenericIsValue` to separate nodes with values from those without; renamed `FplValue` to `FplGenericNode` to reflect this abstraction.
### Fixed
- **[interpreter]** Extension-object evaluation skipped inside their own definition blocks; corrected reference resolution for `FplVariableArray`.

## [v4.3.0] - 2026-03-01
### Fixed
- **[interpreter]** Corrected embedding of `FplEquality`/`FplDecrement` delegates in the symbol table; fixed matching of extension objects against object types, functional terms, and predicates (`mpwa`).
- **[interpreter]** Value setting for reference nodes moved from symbol-resolution (`eval`) to run-time (`Run`) to prevent incorrect targets.

## [v4.2.0] - 2026-02-08
### Added
- **[interpreter]** `Run` implemented for `FplCases`/`FplMapCases`; `SY000` diagnostics for missing infix operands; `ST004`/`ST005` diagnostics for localization and `for`-statement domain handling.
### Changed
- **[interpreter]** `FplVariableStack` now stores variable values on a stack instead of cloning variables, simplifying restoration of state across calls.
### Fixed
- **[interpreter]** `LG001`/`VAR09` false positives for localizations and `mcases` statements eliminated.

## [v4.1.0] - 2026-01-24
### Added
- **[interpreter]** `SIG12`–`SIG14` diagnostics for array/variable-assignment type mismatches and unreachable `cases` branches; `val`/`validity` keywords introducing validity typing for the `is` operator.
### Changed
- **[interpreter]** Large-scale refactor of `FplReference.RefersTo` and `DottedChild` resolution logic (20-part series) to make reference resolution more robust and centralized.

## [v4.0.0] - 2026-01-05
### Changed
- **[interpreter]** Major architectural change: introduced `IRefersTo` as a standard interface member (replacing ad hoc `RefersTo` implementations); reversed and refined the `Type`/`Value`/`ValueList` property model on `FplValue`.
- **[parser]** Draft release notes prepared for FPL parser v4.0.0, reflecting the accumulated syntax simplifications (predicate-definition inheritance, optional properties, removal of `self`/`parent` as usable variable names).
### Added
- **[interpreter]** `IInherit`/`FplGenericInheriting` abstraction for class and functional-term inheritance; `SIG08`–`SIG11` diagnostics.
### Fixed
- **[interpreter]** Removed `ID004` diagnostics (superseded by `SIG04`); numerous `mpwa` false positives for uninstantiated classes and variadic arguments.

## [v3.7.0] - 2025-12-17
### Added
- **[interpreter]** `IHasDimensions` interface and array-type support (`FplVariableArray`, replacing `FplVariableMany`/`FplVariableMany1`); removed extension types from the syntax to simplify the grammar.
- **[interpreter]** `SIG08`–`SIG11` diagnostics for signature/argument mismatches.
### Fixed
- **[interpreter]** `SIG03`, `SIG06`, `VAR06` diagnostics corrected for extensions and array assignments.

## [v3.6.0] - 2025-11-08
### Added
- **[interpreter]** `ID020`–`ID027` diagnostics (duplicate detection, invalid references, missing implementations).
### Fixed
- **[interpreter]** `LG001` diagnostics refined for reference nodes; `SIG04` false positives across constructors, properties, and variables resolved.

## [v3.5.0] - 2025-10-22
### Added
- **[interpreter]** `SIG06`/`SIG07` diagnostics; `FplBaseConstructorCall` and `FplDefaultConstructor` support, including automatic default constructors for intrinsic classes.
### Changed
- **[interpreter]** Syntax simplification: definition `{}` blocks made optional; `obj` no longer required for necessary inheritance types; inherited classes made optional.
### Fixed
- **[interpreter]** `ID007`, `ID009`–`ID011` diagnostics refined for inheritance-chain lookups.

## [v3.4.0] - 2025-10-03
### Added
- **[interpreter]** `ST001`/`ST002` diagnostics; `FplSelf`/`FplParent` as first-class reference nodes; `FplJustificationItemByInf`.
### Fixed
- **[interpreter]** `PR001` diagnostics and multiple `FplReference` signature bugs corrected.

## [v3.3.0] - 2025-09-21
### Changed
- **[interpreter]** Constructor syntax simplified, removing the requirement to reference `self` at the end; argument identifiers now accept `\w+` regex; syntax simplification removing template parameters and `bracketedCoordsInType`/`paramTuple` from class types.
### Added
- **[interpreter]** `IHasSignature` interface; `PR015`/`PR016` diagnostics; PL0 rules of inference added to `Fpl.Commons`.

## [v3.2.0] - 2025-09-08
### Added
- **[interpreter]** `PR000`–`PR008` diagnostics for proof justification mismatches; `Ast.JustificationItem`/`FplJustificationItem` and its `bydef`/`ArgIdentifier` matching modes; `FplPremiseList` (renamed from `FplPredicateList`).
### Removed
- **[interpreter]** `ID000` diagnostics (superseded).

## [v3.1.0] - 2025-08-24
### Added
- **[interpreter]** `Run` implemented for `FplProof`, `FplCorollary`, `FplFunctionalTerm`, and `ICanBeCalledRecursively`; `LG002`–`LG005` diagnostics.
### Fixed
- **[interpreter]** Infinite loop when resolving `self` references fixed.

## [v3.0.0] - 2025-08-17
### Changed
- **[interpreter]** **Breaking architectural change:** removed the `FplBlockType` enum entirely (all ~30 cases), replacing it with an abstract `FplValue` base class and polymorphic derived classes (`FplClass`, `FplPredicate`, `FplFunctionalTerm`, `FplConstructor`, `FplExtensionObj`, etc.), each implementing its own `Run`/`Represent`.
- **[interpreter]** Introduced the `Run` abstract method as the foundation of FPL program execution; added `FplNegation`, `FplConjunction`, `FplDisjunction`, `FplExclusiveOr`, `FplImplication`, `FplEquivalence`, `FplIsOperator`, `FplDelegate`, `FplEquality`, `FplDecrement`, `FplReturn`, `FplExtensionObj`, and `FplAssignment` as concrete executable node types.
### Fixed
- **[vscode]** Fixed downloading of the redirected dotnet runtime library in the extension.

## [v2.2.0] - 2025-07-29
### Changed
- **[interpreter]** New representation approach for symbol-table values; renamed `ValueList` to `ArgList`; added `FplBlockType.Bool`/`FplBlockType.Undefined` as precursors to the later class-based value model.
### Fixed
- **[interpreter]** Duplicate values in predicates and functional terms; `SIG04` false-positive reduction pass.

## [v2.1.0] - 2025-07-15
### Added
- **[interpreter]** `Instance` representation for intrinsic classes; renamed `StartPos`/`EndPos` members for clarity; markdown tooltips in the object explorer.
### Fixed
- **[interpreter]** Instance representation bugs for intrinsic classes and object creation.

## [v2.0.0] - 2025-03-07
### Added
- **[interpreter]** `SIG05` diagnostics.
### Changed
- **[interpreter]** **Breaking:** stabilized expression-execution semantics (conjunction, disjunction, xor, implication, equivalence, negation now evaluated via `Run` rather than only structurally represented), following the v1.9.0 execution-engine groundwork.
### Fixed
- **[interpreter]** `SIG04`/`ID013` diagnostics refined; representation bugs for dollar-digits, variadic variables, and predicates.

## [v1.9.0] - 2024-12-20
### Added
- **[interpreter]** Initial `FplInterpreterRunner`/`Run` infrastructure — the first execution engine capable of evaluating conjunction, disjunction, implication, equivalence, and negation expressions at runtime; `FplValue.Clone()`.
### Changed
- **[interpreter]** `ObjectType`/`PredicateType`/index-type evaluation refactored; residual `eval_units` function replaced by `setUnitType`.
### Removed
- **[interpreter]** Redundant `ID020` diagnostics (already covered by `ID001`).

## [v1.8.0] - 2024-11-21
### Added
- **[interpreter]** `SIG03` diagnostics; `ID015`–`ID019` diagnostics for extension and mapping signature checks.
### Changed
- **[interpreter]** Syntax change for extensions so they behave like functional terms; `@self` literal usage adjusted to free `@` for other purposes.
### Fixed
- **[interpreter]** `SIG04` false positives for variable declarations, constructors, self-references, and property names; `VAR01`/`VAR03` diagnostics for extensions and nested variables.
- **[vscode]** Fixed disappearing symbol table while navigating the tree view; added tooltips to the tree view.

## [v1.7.0] - 2024-11-18
### Added
- **[vscode]** Navigation tree view with colors and icons, backed by the new symbol table.
- **[language-server]** `DocuSymbolTable` navigation and document symbol improvements.
- **[interpreter]** `SIG04` diagnostics extended to infix, prefix, postfix operations and types.
- **[interpreter]** `PR004`/`PR005` diagnostics, `ID014`/`ID015` diagnostics, and justified-argument `BlockType` inference.
- **[interpreter]** Initial evaluation of `return` statements.
- **[parser]** `LanguageCode` parser and distinction between brackets and parentheses.
### Changed
- **[interpreter]** Simplified syntax of quantors (removed the `in` keyword).
- **[interpreter]** Major refactoring of `FplValue` naming, signatures, and representation (`ReprId`, removal of `FplRepresentation.Pointer`, removal of `AdjustNameAndSignature`/`NameIsFinal` logic).
- **[interpreter]** Refactored signature matching (`matchArgumentsWithParameters`, `matchParamsWithArguments`, `findClassInheritanceChain`) including for derived class types.
- **[vscode]** Removed `Scope` sub-nodes from the tree view for a cleaner presentation.
- **[other]** Upgraded to OmniSharp 0.17.0 and refreshed NuGet package versions.
### Fixed
- **[interpreter]** Numerous false positives/negatives across `ID001–ID013`, `VAR00–VAR03`, `LG001`, `SIG00–SIG04` diagnostics involving classes, corollaries, quantors, references, delegates, and constructor calls.
- **[interpreter]** Correct naming/type-signature propagation for arguments, coordinates, delegates, extensions, functional terms, dotted/indexed predicates, and variables.
- **[interpreter]** Infix notation precedence and position bugs for infix/postfix/prefix operations.
- **[interpreter]** `matchArgumentsWithParameters` failure when there are no arguments at all; empty-stack crash in `PopEvalStack`.
- **[vscode]** Tree view rendering bug; colors interfering with debug console logs.

## [v1.6.3] - 2024-10-03
### Added
- **[vscode]** Type info shown in the VS Code object explorer; main theory marked in the explorer.
### Fixed
- **[interpreter]** Improved error message for `SIG04` diagnostics.
- **[language-server]** Missing refresh of the current theory's scope/diagnostics and object explorer.

## [v1.6.2] - 2024-09-29
### Changed
- **[interpreter]** Replaced `Uri` usage with `PathEquivalentUri` throughout the interpreter and language server.
- **[language-server]** Refactored diagnostics storage and logging.
### Fixed
- **[interpreter]** False-positive `LG001` diagnostics for functional-term types; missing handling of `InIsOperatorCreation` context in `Ast.Var`/`Ast.PredicateIdentifier`.
- **[interpreter]** `FplId` for variables; `SIG04`/`PredicateIdentifier` diagnostics for in-block variable declarations; missing `NSP05` diagnostics.
- **[interpreter]** Distinguishing `SIG04` from `ID010` diagnostics; `ID010` diagnostics in variable declarations.
- **[language-server]** Garbage collector now runs from the main file only; missing syntax-error diagnostics and diagnostics updates when double-clicking across buffers; stream-name and text-position bugs.
- **[vscode]** Missing syntax-error diagnostics display; README updated.
### Removed
- **[other]** Removed obsolete `.runsettings` file.

## [v1.6.0] - 2024-09-09
### Added
- **[interpreter]** FPL interpreter (alpha version): evaluation of conjunction, disjunction, xor, implication, and equivalence expressions; initial delegate module.
- **[interpreter]** Diagnostics `ID002`–`ID013`, `VAR00`–`VAR03`, `PR000`–`PR002`, `LG000`/`LG001`, `GEN00`, `SIG00`–`SIG04`, `NSP003`–`NSP005`.
- **[interpreter]** Garbage collector, tokenizer, circular theory-usage/import detection.
- **[vscode]** Tree view for symbol navigation; configuration properties (`vsfplconfig.json`).
### Changed
- **[interpreter]** Syntax simplification for class inheritance (templates and extensions no longer allowed).
- **[interpreter]** Replaced `EvaluationType`/`EvalContext` approach with a centralized evaluation-stack model in the symbol table.
- **[interpreter]** Removed `FplValueType.Expression`; replaced string-based `FplRepresentation` with a custom type.
- **[interpreter]** Removed class instance properties from the syntax (replaced by zero-arity functional terms).
### Fixed
- **[interpreter]** Extensive bugfixes to `FplId`, `FplRepresentation`, `TypeSignature`, `QualifiedName`/`QualifiedStartPos`, and `NameIsFinal` across all FPL block types (theorems, corollaries, proofs, functional terms, delegates, properties, constructors).
- **[interpreter]** Multiple loading of the same `uses` clause from different locations.
- **[language-server]** Clearing of published diagnostics and completion-handler URI handling.

## [v1.5.4] - 2024-03-21
### Fixed
- **[vscode]** `spawn UNKNOWN` error in the extension.
- **[interpreter]** `loadAllUsesClauses` now supports dynamic loading/replacement of edited FPL files in the symbol table.
- **[interpreter]** `NSP003`/`NSP004` circular-import detection error position.

## [v1.5.3] - 2024-03-13
### Changed
- **[other]** Renamed registry/library projects; updated `.gitignore` and current dotnet runtimes.
- **[interpreter]** Simplified `DiagnosticCode`.
- **[interpreter]** Added `acquireSources` for locating FPL library sources.

## [v1.5.2] - 2024-03-12
### Added
- **[other]** .NET 8.0 libraries.
- **[interpreter]** `EvalAliasedNamespaceIdentifier` evaluation function and tests; `tryFindAndParseUsesClauses`.
### Changed
- **[interpreter]** Refactored diagnostics record types; added `lib`/`libmap.txt` for FPL.

## [v1.5.1] - 2024-03-03
### Changed
- **[other]** Migrated to .NET 8.0.
### Fixed
- **[vscode]** Duplicate items in auto-completion.

## [v1.5.0] - 2024-02-03
### Added
- **[parser]** Newest FPL parser (v3.4.0).
- **[vscode]** Auto-completion for infix, prefix, postfix, and user-defined mathematical/object symbols.
- **[parser]** Inbuilt parsers for math operators, replacing regex-based matching.
### Changed
- **[other]** Localization strings reorganized for infix/postfix/prefix/symbol keywords.
### Fixed
- **[parser]** Improved error reporting for infix/postfix/prefix symbols.

## [v1.4.9] - 2023-12-16
### Added
- **[other]** Animated GIF added to the README/extension listing.

## [v1.4.8] - 2023-12-15
### Fixed
- **[parser]** **[language-server]** Minor bugfixes in the parser and language server.

## [v1.4.7] - 2023-12-13
### Changed
- **[other]** CHANGELOG and README for the FPL extension updated.
- **[parser]** Refactored `predicateList1`.
### Removed
- **[parser]** Removed indexed predicates.

## [v1.4.6] - 2023-12-03
### Added
- **[parser]** Support for user-defined prefix, postfix, and infix notation for mathematical operators in expressions.
- **[parser]** Parenthesized expressions; infix operation grammar extension.
### Fixed
- **[parser]** Syntax for `xor` and alternating variable declarations/arguments in proofs.

## [v1.4.5] - 2023-11-26
### Fixed
- **[parser]** Syntax of referencing identifiers in proofs and corollaries; proof/corollary snippet bugfixes.

## [v1.4.4] - 2023-11-25
### Added
- **[parser]** New keyword `base`, disambiguating parent-class calls from indexed predicates.
### Fixed
- **[parser]** Syntax of indexed and dotted predicates; mixed qualification of predicates; dotted/indexed `PascalCaseId`s; dollar-qualifier and `for`-statement (`in type`) syntax.

## [v1.4.3] - 2023-11-19
### Added
- **[parser]** Additional keyword `bydef`.
### Changed
- **[parser]** Grammar simplification for namespaces.

## [v1.4.2] - 2023-11-18
### Changed
- **[parser]** Simplified grammar for theorem-like statements, conjectures, and corollaries; removed premise/conclusion from predicates; discontinued double-lines in snippets.
### Fixed
- **[parser]** References to corollaries in proofs; corollary snippet bugfix.

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
- **[parser]** Grammar for argument identifiers and `assume` arguments.
- **[vscode]** Numerous bugfixes across autocompletion for axioms, definitions, theorems, lemmas, conjectures, properties, proofs, corollaries, quantors, and delegates.

## [v1.3.0] - 2023-11-04
### Changed
- **[parser]** Rewrote error recovery and diagnostics using a regex-based approach; refactored error codes, messages, and the recovery map.
### Added
- **[parser]** `Interval` type.
### Fixed
- **[parser]** False positives between declarations and translations; consecutive/duplicate diagnostics; infinite loops in error recovery; string-manipulation and namespace-related error positions.

## [v1.2.10] - 2023-10-01
### Fixed
- **[parser]** Class identifiers, ranges/coordinates, standard error recovery, and template syntax highlighting.

## [v1.2.9] - 2023-09-30
### Changed
- **[parser]** Reverted to standard error recovery; added error recovery for signatures and inference blocks.
### Added
- **[parser]** `Positions` type.
### Fixed
- **[vscode]** Improved display of fatal errors.

## [v1.2.8] - 2023-09-27
### Added
- **[parser]** Alias recovery; error recovery for dotted identifiers; alternative `PascalCaseId` diagnostics.
### Changed
- **[parser]** Unified grammar types into the `ast` type.

## [v1.2.7] - 2023-09-08
### Added
- **[vscode]** Support for standard color themes.
### Fixed
- **[vscode]** Extension version-number display bug.
- **[parser]** **[language-server]** Language Server updated to reflect FPL grammar 2.4.2.

## [v1.2.6] - 2023-09-08
### Changed
- **[parser]** FPL syntax updated to version 2.4.2.

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
### Changed
- **[language-server]** Diagnostics now come directly from the FPL parser instead of being mocked (no error recovery yet, so at most one syntax error per file is reported).
### Fixed
- **[vscode]** Logging and installation issues, including excessive `node_modules` exclusions.

## [v1.2.0] - 2023-09-03
### Added
- **[vscode]** Dotnet runtimes for Windows, Linux, and macOS x64, downloaded on demand rather than bundled with the extension.
- **[language-server]** Diagnostics from the FPL parser wired into the extension.
### Changed
- **[vscode]** `activationEvents` added to suppress `vsce` packaging errors; release notes updated.

## [v1.1.1] - 2023-09-03
### Added
- **[vscode]** Bundled dotnet runtime for Windows x64 and compiled DLL for the initial FPL Language Server.
### Changed
- **[other]** Refactored logging to console.

## [v1.1.0] - 2023-09-02
### Added
- **[language-server]** FPL Language Client and Server added; starts automatically when an `.fpl` file is opened.
### Known Issues
- **[language-server]** Server fails to start due to an incorrect path to the DLL (fixed in a later release).

## [v1.0.1] - 2023-08-31
### Added
- **[vscode]** Extension icon, description, categories, and keywords.

## [v1.0.0] - 2023-08-29
### Added
- **[vscode]** Initial release with syntax highlighting.
- **[parser]** Initial complete FPL parser (versions 2.1.0–2.4.0): namespaces, definitions, proofs, theorem-like statements, statements, axioms, constructors, types, entities, coordinates, signatures, and variable types.
- **[other]** Initial project documentation (`README.md`, `CONTRIBUTING.md`, high-level design docs, FPL grammar/syntax change logs). 

## Before this repository
This is the initial, starting point of the history of this .NET-based repository. However, the FPL project started with 
a [python-based repository that was deprecated but is kept in here](https://github.com/bookofproofs/fpl) for reference.

There are separate CHANGES.md files in this old repository with separate versioning for 
- the [grammar](https://github.com/bookofproofs/fpl/blob/master/grammar/CHANGES.md), 
- the [interpreter](https://github.com/bookofproofs/fpl/blob/master/poc/CHANGES.md), 
- the [ide](https://github.com/bookofproofs/fpl/blob/master/ide/CHANGES.md) (there were no VS Code extension yet then), 
- and the [example theories](https://github.com/bookofproofs/fpl/blob/master/poc/theories/CHANGES.md). 
