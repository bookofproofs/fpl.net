module FplInterpreter
open System.Collections.Generic
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks

let fplInterpreter input uri fplLibUrl (parsedAsts:ParsedAstList) debug = 
    let st = SymbolTable(parsedAsts, debug)
    
    loadAllUsesClauses input uri fplLibUrl parsedAsts 
    evaluateSymbolTable st
    st
