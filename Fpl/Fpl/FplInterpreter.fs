module FplInterpreter
open System
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks
open FplInterpreterDiagnosticsEmitter

let fplInterpreter (st:SymbolTable) input (uri:Uri) fplLibUrl = 
    try
        let escapedUri = Uri(Uri.UnescapeDataString(uri.AbsoluteUri))
        loadAllUsesClauses st input escapedUri fplLibUrl 
        evaluateSymbolTable uri st
    with ex -> 
        emitUnexpectedErrorDiagnostics (uri.AbsolutePath) (ex.Message + Environment.NewLine + ex.StackTrace)
    