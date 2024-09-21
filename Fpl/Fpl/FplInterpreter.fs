module FplInterpreter
open System
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks
open FplInterpreterDiagnosticsEmitter

let fplInterpreter (st:SymbolTable) input (uri:PathEquivalentUri) fplLibUrl = 
    try
        loadAllUsesClauses st input uri fplLibUrl 
        evaluateSymbolTable uri st
    with ex -> 
        emitUnexpectedErrorDiagnostics (ex.Message + Environment.NewLine + ex.StackTrace)
    