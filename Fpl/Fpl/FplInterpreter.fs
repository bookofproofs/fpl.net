module FplInterpreter
open System
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks
open FplInterpreterDiagnosticsEmitter

let fplInterpreter (st:SymbolTable) input (uri:PathEquivalentUri) fplLibUrl = 
    //try
        if st.MainTheory = String.Empty then
            st.MainTheory <- uri.TheoryName
        loadAllUsesClauses st input uri fplLibUrl 
        evaluateSymbolTable st
    //with ex -> 
    //    emitUnexpectedErrorDiagnostics (ex.Message + Environment.NewLine + ex.StackTrace)
    