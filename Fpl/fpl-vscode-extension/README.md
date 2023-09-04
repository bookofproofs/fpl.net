# FPL (Formal Provling Language)

FPL, the Formal Proving Language, is a language to formulate mathematical definitions, theorems, and proofs independently of local natural languages.

This extension enables you to make use of advantages of an IDE like VS Code while writing code in FPL.

This is a Proof of Concept of a private reasearch project to develop such a language. For more information or code examples, see https://github.com/bookofproofs/fpl.net. In this repository, you will also find the source code for this extension.

## Features

Currently, the extension provides a basic syntax highlighting for `.fpl` files in the dark scheme.

## Requirements

* VSCode version ">1.81.0"
* .NET Cli on your system

## Extension Settings

None

## Known Issues

* Syntax highlighting of comments does not work properly.
* FPL Server will fail since the path to dll is not working 

## Release Notes


### 1.2.2, 1.2.3, 1.2.4

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
