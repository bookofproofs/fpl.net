# FPL (Formal Proving Language)

**FPL**, the *Formal Proving Language*, is a language to formulate mathematical definitions, theorems, and proofs independently of individual natural languages.

FPL aims to be human-readable and catchy first, and only then formal enough to be compiled and processed by computers. It is intended to facilitate automated parsing, consistency checking, proof search, and theory generation, while remaining accessible even to non-programmers and suitable for mathematical education.

This site hosts the **API documentation** for the FPL solution, generated with `fsdocs`/DocFX. For a general introduction, tutorials, and the VS Code extension, see the links below.

## Solution Overview

The FPL solution consists of:

- **Parser** — an F#/FParsec-based parser translating FPL source code into an abstract syntax tree, with a regex-based error-recovery and diagnostics engine.
- **Interpreter** — a real execution engine (`Fpl2Interpreter`) built around a polymorphic `FplValue`/`FplGenericNode` class hierarchy, capable of evaluating logical expressions and verifying proof correctness. It powers an extensive, fine-grained diagnostics system with well over a hundred codes across the `ID`, `VAR`, `SIG`, `PR`, `LG`, `SY`, `ST`, `NSP`, and `NG` families.
- **Language Server** — an F#-based Language Server Protocol implementation (`Fpl3LanguageServer`) powering real-time diagnostics, completion, and document/symbol navigation.
- **VS Code Extension** — a full-featured IDE experience with syntax highlighting, autocompletion, a symbol navigation tree view, and a KaTeX-rendered webview for browsing valid statements and rules of inference. It uses Microsoft's official `@vscode/dotnet-runtime` API for on-demand, cross-platform .NET runtime acquisition (Windows, Linux, macOS).

The .NET solution is organized into `Fpl0Base`, `Fpl1Parser`, and `Fpl2Interpreter`, and `Fpl3LanguageServer` libraries, alongside consolidated test projects. 

Note: 
*This is an auto-generated fdocs API documentation. It does not cover the whole solution.*
- *Not covered*: 
  - a type-script project for the VS Code extension 
  - test projects in the solution
