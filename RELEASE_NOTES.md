# Release Notes

This page summarizes the notable releases of the Fpl solution (parser, interpreter, language server, and VS Code extension).

*For detailed, version-by-version entries (including every bugfix), see the full [CHANGELOG.md](https://github.com/bookofproofs/fpl.net/Fpl/fpl-vscode-extension/blob/main/CHANGELOG.md).*

___

## v5.1.0 (2026-09-21) — Solution split & F#-based language server
The `Fpl` project was split into `Fpl0Base`, `Fpl1Parser`, and `Fpl2Interpreter`; the language server was rewritten in F# as `Fpl3LanguageServer`. Test projects were consolidated accordingly. The VS Code extension was migrated to TypeScript as a proper `.esproj` project and now uses Microsoft's official `@vscode/dotnet-runtime` acquisition API. An `fsdocs`/DocFX-based documentation site (this site) was added and published to GitHub Pages.
___

## v5.0.x (2026-07 – 2026-09) — Diagnostics polish & valid-statement viewer
A broad pass improved wording and test coverage across nearly all diagnostic families (`SIG*`, `ST*`, `VAR*`, `ID*`, `LG*`, `NG*`, `PR*`). New `SY013`/`SY014` diagnostics detect unnecessary or conflicting parentheses in infix expressions. "Quantor" was renamed to "quantifier" throughout. The VS Code extension gained a new webview panel presenting valid statements as a sortable, KaTeX-rendered table with double-click navigation.
___

## v5.0.0 (2026-06-28) — Parser rewrite
Dedicated `InfixOp`/`PrefixOp`/`PostfixOp` parsers were introduced, removing most parsing optionality in favor of stricter whitespace control — producing much more precise syntax error messages. The evaluator was split from a single monolithic function into focused modules.
___

## v4.7.0 – v4.8.0 (2026-05) — Inference-aware proof checking
`ProceedingExprCandidates`/inference-based checking was implemented across all justification item kinds (`by def`, `by cor`, `by inf`, `trivial`, etc.), enabling FPL to verify that proof conclusions are actually derivable from their justifications. Unicode math symbols (`∃`, `∀`, `¬`, `∃!`) and dotted infix notation for logical connectives were added.
___

## v4.0.0 – v4.6.0 (2026-01 – 2026-04) — `IRefersTo` / validity architecture
A major internal overhaul introduced `IRefersTo` as a standard reference-resolution interface and `IInferrable`/`IValid` for validity tracking, backed by a centralized `ValidStmtStore`. `IInherit`/`FplGenericInheriting` formalized class and functional-term inheritance.
___

## v3.0.0 (2025-08-17) — Removal of `FplBlockType`
**Breaking change:** the `FplBlockType` enumeration (~30 cases) was removed entirely and replaced by an abstract `FplValue` base class with polymorphic derived classes (`FplClass`, `FplPredicate`, `FplConstructor`, etc.), each implementing its own `Run`/`Represent` behavior. This introduced the `Run` abstract method as the foundation of FPL program execution.
___

## v1.9.0 – v3.7.0 (2024-12 – 2025-12) — Execution engine bring-up
The first `FplInterpreterRunner`/`Run` infrastructure was introduced, giving FPL its execution engine: conjunction, disjunction, implication, equivalence, and negation could now be evaluated at runtime, followed by proofs, corollaries, and array types (`FplVariableArray`).
___

## v1.6.0 – v1.8.0 (2024-09 – 2024-11) — Interpreter alpha
The FPL interpreter's alpha version was released, along with the first diagnostics families (`ID`, `VAR`, `PR`, `LG`, `GEN`, `NSP`, `SIG`), a garbage collector, circular-import detection, and the VS Code symbol-navigation tree view.
___

## v1.0.0 – v1.5.x (2023-08 – 2024-03) — Parser, error recovery & VS Code foundation
The initial FPL parser was built up through versions 2.1.0–3.4.0, along with regex-based error recovery, syntax highlighting, auto-completion, and the first FPL Language Client/Server integration in VS Code — laying the foundation for everything that followed.



