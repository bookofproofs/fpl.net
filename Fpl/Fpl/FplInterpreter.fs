module FplInterpreter
open System.Collections.Generic
open FplInterpreterTypes
open FplInterpreterUsesClause
open FplInterpreterBuildingBlocks

let fplInterpreter input uri fplLibUrl (parsedAsts:System.Collections.Generic.List<ParsedAst>) = 
    let st = { 
        SymbolTable.ParsedAsts = parsedAsts
        SymbolTable.CurrentContext = EvalContext.ContextNone
        SymbolTable.Root = FplValue.CreateRoot()
        }
    
    loadAllUsesClauses input uri fplLibUrl parsedAsts 
    evaluateSymbolTable st

