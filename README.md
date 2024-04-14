FPL, the Formal Proving Language, is a language to formulate
mathematical definitions, theorems, and proofs independently of 
local natural languages. 

# Proof of Concept (Work-in-Progress)

This repository is a Proof of Concept of FPL (PoC) based on the requirements specified in the [INTRO.md](https://github.com/bookofproofs/fpl.net/blob/main/INTRO.md).
It is a private research project with the goal to show that FPL is capable to formulate and interpret complex mathematics in a standardized format. 

<img src="https://github.com/bookofproofs/fpl.net/blob/main/Fpl/fpl-vscode-extension/images/FplExtension.gif" width="650">
 

There is also an FPL Channel on YouTube devoted to the language.

[![FPL Channel](https://img.youtube.com/vi/0yiR_3S3OJA/0.jpg)](http://www.youtube.com/watch?v=0yiR_3S3OJA "FPL Channel").

## Closed Tasks
* The FPL grammar and parser based on F# / FParsec library 
	
## Open tasks (among others)
* Fpl Interpreter (See [project](https://github.com/users/bookofproofs/projects/1)).

# Contributions are welcome 

It is convenient to use Visual Studio for programming the .NET solution `Fpl/Fpl.sln` and Visual Studio Code for programming the `Fpl/fpl-vscode-extension`.
If you are working with Visual Studio Code only, you will still need two opened sessions at once: The first for the .NET solution with the Language Server and the second for the folder containing the fpl-vscode-extension.
We still recommend to use Visual Studio for `Fpl/Fpl.sln` because Visual Studio Code proved to cause problems with correctly finding and running all .NET unit tests in the solution `Fpl/Fpl.sln`.

## Getting Started with Testing the FPL Language Server

Follow these steps to set up the project on your local machine:

1. **Clone the repository:** Use `git clone` to clone the repository to your local machine.

2. **Open the .NET Solution:** Navigate to the subdirectory `Fpl` and open the solution in `Fpl.sln`. We recommend to use Visual Studio for this purpose (see above).
The solution contains the following projects:
  * `Fpl` - contains the F# modules for the FPL parser and the FPL interpreter 
  * `FplLS` - contains the C# classes for the FPL Language Server
  * Projects with unit tests
      * `FplLSTests` - contains unit tests for FplLS
	  * `FplParserTests` - contains unit tests for the FPL parser
	  * `FplInterpreter.Tests` - contains unit tests for the FPL Interpreter

3. **Install the dependencies:** Install the following Nuget packages:
  * FSharp.Core by Microsoft (latest) 
  * FParsec by Stephan Tolksdorf (latest)
  * Microsoft.NET.Test.Sdk (latest)
  * MSTest.TestAdapter (latest)
  * Use .NET 8.0 for F# and C# code.

4. **Build the solution** Your test suite should discover the unit tests in all three test projects (`FplLSTests`, `FplParserTests`, `FplInterpreter.Tests`)

4. **Run the Tests** Run the unit tests of the solution. They provide also many useful examples of how to use the code.

## Getting Started with Testing the Visual Studio Code Extension

Follow these steps to run and test the FPL extension with your current version of the FPL parser and FPL interpreter:

1. **Get Started with Testing the FPL Language Server** (see above)

2. **Install Node.js and npm:** If you don't have Node.js and npm installed, you will need to install them. You can download Node.js and npm from the official Node.js website. npm is included in the Node.js installation.

3. **Install the dependencies:** Navigate to the subdirectory `Fpl/fpl-vscode-extension`. This is the root directory of the VS code extension (the directory that contains the `package.json` file) in your terminal or command prompt and run `npm install`. This command installs all the dependencies listed in the `package.json` file.

4. **Build the FPL Language Server:** Like described above in "Getting Started with Testing the FPL Language Server", open the solution in `Fpl/Fpl.sln` and build it as **Release**. 
It is important that you build the solution as Release because the vscode-extension requires the `.dll` files of the FPL language server from the `bin/Release/net8.0` subdirectory of your build. 

5. **Make your FPL VSCode Extension use your fresh new build of the FPL Language Server:** There are batchfiles to simplify this step: 
* run copyFilesWindows.bat on Windows
* run copyFilesLunux.sh on Linux or MacOS
Depending on your operating system, the batch file simply deletes all old files in the subfolder `./Fpl/fpl-vscode-extension/dotnet-runtimes/FplLsDll` and replaces them by the build files from the folder `./Fpl/FplLS/bin/Release/net8.0`.

4. **Debug the FPL VSCode extension:** Now, switch to your second VSCode session (the one with the opened `./Fpl/fpl-vscode-extension` folder) and
and press F5 to start debugging the extension. It will use your fresh build of the FPL Language Server.
