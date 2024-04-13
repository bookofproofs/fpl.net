module FplInterpreter
open System.Collections.Generic
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks

let fplInterpreter input uri fplLibUrl (parsedAsts:System.Collections.Generic.List<ParsedAst>) = 
    let st = SymbolTable(parsedAsts, true)
    
    loadAllUsesClauses input uri fplLibUrl parsedAsts 
    evaluateSymbolTable st
    st
