/// This module contains classes used to store valid statements globally by the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.Globals.Validity
open System.Collections.Generic
open FplPrimitives
open FplInterpreterBasicTypes

type ValidityReason =
    | IsAxiom of FplGenericNode
    | IsAsserted of FplGenericNode
    | IsAssumed of FplGenericNode
    | IsInferred of FplGenericNode
    | Error 

type ValidStatement =
    { Node: FplGenericNode
      ValidityReason: ValidityReason
      StatementExpression: string}

type ValidStmtStore() =
    let _theoremStore = Dictionary<string, ValidStatement>()

    member this.RegisterValidStmt (fv: FplGenericHasValue) =
        let validityReason = 
            match fv.Name with
            | LiteralAxL ->
                let exprOpt = fv.ArgList |> Seq.tryLast
                match exprOpt with
                | Some expr -> ValidityReason.IsAxiom expr
                | _ -> ValidityReason.Error // fallback if axiom node is empty
            | _ -> ValidityReason.Error // TODO handle all other cases correctly

        match validityReason with
        | ValidityReason.IsAxiom expr ->
            let validStmt = 
                { ValidStatement.Node = fv
                  ValidStatement.ValidityReason = validityReason
                  ValidStatement.StatementExpression = expr.Type SignatureType.Mixed }
            _theoremStore.TryAdd(validStmt.StatementExpression, validStmt) |> ignore
        | _ ->
            ()

    member this.Clear() = _theoremStore.Clear()

    member this.Count = _theoremStore.Count
