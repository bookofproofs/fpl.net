/// This module creates the SymbolTable
/// and deals with types and type-related constructs

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module FplInterpreter.ST.Types
open FplPrimitives
open FplGrammarTypes
open FplInterpreterGlobals
open FplInterpreterST
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables


let setKeywordType keywordType pos1 pos2 = 
    let fv = variableStack.PeekEvalStack()
    match fv with
    | :? FplVariableArray as arr -> arr.SetType keywordType None pos1 pos2 
    | :? FplMapping as map -> map.SetType keywordType None pos1 pos2
    | _ ->  fv.TypeId <- keywordType

let eval (st: SymbolTable) (ast: Ast) =
    match ast with


    | Ast.IndexType((pos1, pos2),()) -> 
        setKeywordType LiteralInd pos1 pos2
    | Ast.FunctionalTermType((pos1, pos2),()) -> 
        setKeywordType LiteralFunc pos1 pos2
    | Ast.ObjectType((pos1, pos2),()) -> 
        setKeywordType LiteralObj pos1 pos2
    | Ast.PredicateType((pos1, pos2),()) -> 
        setKeywordType LiteralPred pos1 pos2
    | Ast.TemplateType((pos1, pos2), s) -> 
        let fv = variableStack.PeekEvalStack()
        setKeywordType s pos1 pos2
        let templateNode = new FplIntrinsicTpl(s, (pos1, pos2), fv)
        match fv with 
        | :? FplGenericVariable as var -> 
            // attach template type to declared variable 
            var.RefersTo <- Some templateNode
        | _ -> () // RefersTo's semantics in other FplValues is different, do not interfere with it
        variableStack.PushEvalStack(templateNode)
        variableStack.PopEvalStack()

    | _ ->
        // Not a leaf lexical node — caller should route to another focused module.
        failwith "FplInterpreter.ST.Types:eval: unexpected AST node"
