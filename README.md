FPL, the Formal Proving Language, is a language to formulate
mathematical definitions, theorems, and proofs independently of 
local natural languages. 

# Proof of Concept (Work-in-Progress)

This repository is a Proof of Concept of FPL (PoC) based on the requirements specified in the [INTRO.md](https://github.com/bookofproofs/fpl.net/blob/main/INTRO.md).
It is a private research project with the goal to show that FPL is capable to formulate and interpret complex mathematics in a standardized format. 

<img src="https://github.com/bookofproofs/fpl.net/blob/main/Fpl/fpl-vscode-extension/images/FplExtension.gif" width="650">

Contributions are welcome. 

There is also an FPL Channel on YouTube devoted to the language.

[![FPL Channel](https://img.youtube.com/vi/0yiR_3S3OJA/0.jpg)](http://www.youtube.com/watch?v=0yiR_3S3OJA "FPL Channel").

## In progress: 
* The FPL grammar and parser based on F# / FParsec library 

## Open tasks (among others)
* Writing even more unit tests for the new F# parser
* IDE extension for FPL (e.g. based on Visual Studio)
* FPL interpreter

# Getting Started

## Software dependencies
* Use F# (.NET 6.0)
* Install the following Nuget packages:
  * FSharp.Core by Microsoft (latest) 
  * FParsec by Stephan Tolksdorf (latest)
  * Microsoft.NET.Test.Sdk (latest)
  * MSTest.TestAdapter (latest)
## Testing
* Open the solution in Fpl/Fpl.sln with Visual Studio (eg. Microsoft Visual Studio Community 2022)
* Build and run the tests

