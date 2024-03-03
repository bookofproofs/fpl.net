# FPL (Formal Proving Language)

FPL, the Formal Proving Language, is a language to formulate mathematical definitions, theorems, and proofs independently of local natural languages.

This extension enables you to make use of advantages of an IDE like VS Code while writing code in FPL.

This is a Proof of Concept of a private research project to develop such a language. For more information or code examples, see https://github.com/bookofproofs/fpl.net. In this repository, you will also find the source code for this extension.

## Features

* Syntax highlighting for `.fpl` files.
* (Syntax-driven) Error diagnostics service (experimental) 
* (Syntax-driven) Code-completion service with various code snippets 
* FPL support for user-defined prefix, postfix, and infix notation for mathematical operators in expressions.


<img src="https://github.com/bookofproofs/fpl.net/blob/main/Fpl/fpl-vscode-extension/images/FplExtension.gif?raw=true" width="550">

There is also an FPL Channel on YouTube devoted to the language.

[![FPL Channel](https://img.youtube.com/vi/0yiR_3S3OJA/0.jpg)](http://www.youtube.com/watch?v=0yiR_3S3OJA "FPL Channel").

## Requirements

VSCode version ">1.84.2"

## Extension Settings

None

## Known Issues

* None

## Release Notes

### 1.5.0
* Newest FPL parser including many mathematical symbols further simplifying the syntax

### >=1.4
* Syntax-driven auto-completion service 

### 1.3.0
* Syntax error diagnostics (experimental) 

### 1.2.10
* Improved syntax error messages from the FPL parser
* Bugfix in syntax highlighting of generics in FPL

### 1.2.9

* Emitting error diagnostics directly from FplParser

### 1.2.8

* Support for standard color themes

### 1.2.7

* Update FPL Language Server to reflect FPL grammar 2.4.2

### 1.2.6

* Bugfix syntax highlighting for comments

### 1.2.2, 1.2.3, 1.2.4, 1.2.5

* Correcting logging and installation issues
### 1.2.1

* Hooking up Fpl Parser to VSCode diagnostics
### 1.2.0

* Shipping dotnet runtime dependency for linux, windows, and macOS x64 systems via download on demand

### 1.1.1

* Shipping initial diagnostics for windows / 64 runtime 
* Fake diagnostics only! You can test them by opening and editing a fake .fpl file containing some nuspec code 
* The Language Server will show diagnostics for if your nuspec contains the text '<summary>__REPLACE__</summary>'
* Proof of Concept of a running Language Server thanks to Gary Ewan Park, Martin Björkström, and Bing AI-powered copilot :-)

### 1.1.0 

* Initial Language Server with diagnostics added

### 1.0.0 

Syntax highlighting
