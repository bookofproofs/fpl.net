module FplInterpreter
open System
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks

let fplInterpreter input (uri:Uri) fplLibUrl (parsedAsts:ParsedAstList) debug = 
    let escapedUri = Uri(Uri.UnescapeDataString(uri.AbsoluteUri))
    let st = SymbolTable(parsedAsts, debug)
    
    loadAllUsesClauses input escapedUri fplLibUrl parsedAsts 
    evaluateSymbolTable uri st
    st
