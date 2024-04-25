module FplInterpreter
open System
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks

let private emitUnexpectedErrorDiagnostics uri errMsg = 
        let diagnostic = { 
            Diagnostic.Uri = uri
            Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
            Diagnostic.Severity = DiagnosticSeverity.Error
            Diagnostic.StartPos = Position("",0,1,1)
            Diagnostic.EndPos = Position("",0,1,1)
            Diagnostic.Code = GEN00 errMsg
            Diagnostic.Alternatives = None 
        }
        FplParser.parserDiagnostics.AddDiagnostic(diagnostic)

let fplInterpreter input (uri:Uri) fplLibUrl (parsedAsts:ParsedAstList) debug = 
    let st = SymbolTable(parsedAsts, debug)
    try
        let escapedUri = Uri(Uri.UnescapeDataString(uri.AbsoluteUri))
        loadAllUsesClauses input escapedUri fplLibUrl parsedAsts 
        evaluateSymbolTable uri st
    with ex -> 
        emitUnexpectedErrorDiagnostics uri ex.Message
    st
 