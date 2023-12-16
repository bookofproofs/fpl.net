# Change Log

All notable changes to the "fpl-vscode-extension" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## 1.4.8
* Minor Bugfixes in Parser and Language Server 

## 1.4.7
* CHANGELOG and README for FPL extension updated

## 1.4.6
* FPL support for user-defined prefix, postfix, and infix notation for mathematical operators in expressions.

## 1.4.5
* Bugfixes in the syntax of referencing identifiers in proofs and corollaries

## 1.4.4
* Bugfixes in the syntax of indexed and dotted predicates
* Disambiguation of parent-class calls and indexed predicates by introducing the new keyword base.

## 1.4.3
* Additional keyword bydef

## 1.4.2
* Minor bugfixes

## 1.4.1
* Minor bugfixes

## 1.4.0
* Syntactical autocompletion service added
* Syntax-Highlighting for argument identifiers and code markdown added
* Minor bugfixes in the FPL grammar for argument identifiers and 'assume' arguments  

## 1.3.0
* Rewrite error recovery / diagnostics


## 1.2.9, 1.2.10
* Update DLL library for the Language Server 

## 1.2.8
- Support for standard color themes

## 1.2.7
- Update FPL Language Server to reflect FPL grammar 2.4.2

## 1.2.6
- Correcting syntax highlighting issues for comments

## 1.2.2, 1.2.3, 1.2.4, 1.2.5
- Logging and installation issues fixed

## 1.2.1
- Diagnostics are now not mocked but come directly from the FPL parser.
- However, the parser has still no error recovery added and you will see at most one syntax error per .fpl file.

## 1.2.0
- Dotnet runtimes for windows, linux and macOS x64 
- Extension does not include a dotnet runtime, instead, it will be downloaded on demand on a particular platform

## 1.1.1
- inclusion of a dotnet runtime for windows x64 and a compiled dll lib for initial FPL Language Server 

## 1.1.0

- FPL Language Client and Server added and try to start when an fpl file is open. 
- However, FPL Server will fail, since the path to dll is not correct

## 1.0.1

- Icon addes
- Description added
- Categories amended
- Keywords added
## 1.0.0

- Initial release with syntax highlighting