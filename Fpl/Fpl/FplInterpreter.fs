/// This module contains the main program of the FPL interpreter
/// It makes sure that all uses clauses are loaded and parsed before the SymbolTable can be created out of the AST.
/// It also catches any unexpected runtime errors to make sure that the FPL Language Server never crashes. 
/// In case an expected runtime error is produced, the Language Server will emit a GEN00 diagnostics.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter
open System
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks
open FplInterpreterDiagnosticsEmitterPre

let fplInterpreter (st:SymbolTable) input (uri:PathEquivalentUri) fplLibUrl = 
    try
        if st.MainTheory = String.Empty then
            st.MainTheory <- uri.TheoryName
        loadAllUsesClauses st input uri fplLibUrl 
        evaluateSymbolTable st
        st.Root.Run variableStack
    with ex -> 
        emitUnexpectedErrorDiagnostics (ex.Message + Environment.NewLine + ex.StackTrace)
    