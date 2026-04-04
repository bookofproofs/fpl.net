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
open FplInterpreterCompoundPredicates

type ValidityReason =
    | IsAxiom of FplGenericNode
    | IsAsserted of FplGenericNode
    | IsAssumed of FplGenericNode
    | IsInferred of FplGenericNode
    | IsInferredFromRevocation of FplGenericNode
    | Error 

type ValidStatement =
    { Node: FplGenericNode
      ValidityReason: ValidityReason
      StatementExpression: string}

type ValidStmtStore() =
    let _theoremStore = Dictionary<string, ValidStatement>()
    let _assumedArguments = Stack<FplGenericHasValue>()

    /// Adds an axiom to the theorem store, returns true if success, false otherwise.
    member this.AddAxiom (axiom: FplGenericHasValue) =
        let validityReason = 
            match axiom.Name with
            | LiteralAxL ->
                let exprOpt = axiom.ArgList |> Seq.tryLast
                match exprOpt with
                | Some expr -> ValidityReason.IsAxiom expr
                | _ -> ValidityReason.Error // fallback if axiom node is empty
            | _ -> ValidityReason.Error // TODO handle all other cases correctly

        match validityReason with
        | ValidityReason.IsAxiom expr ->
            let validStmt = 
                { ValidStatement.Node = axiom
                  ValidStatement.ValidityReason = validityReason
                  ValidStatement.StatementExpression = expr.Type SignatureType.Mixed }
            _theoremStore.TryAdd(validStmt.StatementExpression, validStmt)
        | _ -> false

    /// Assumes an argument and puts it to the theorem store
    member this.AssumeArgument assumption =
        let validStmt = 
            { ValidStatement.Node = assumption
              ValidStatement.ValidityReason = ValidityReason.IsAssumed assumption
              ValidStatement.StatementExpression = assumption.Type SignatureType.Mixed }
        _theoremStore.TryAdd(validStmt.StatementExpression, validStmt) |> ignore
        _assumedArguments.Push assumption

    /// Revokes an argument and puts its negation to the theorem store
    member this.RevokeLastArgument() =
        if _assumedArguments.Count > 0 then
            let revocation = _assumedArguments.Pop()
            let negatedRevocation = new FplNegation((revocation.StartPos, revocation.EndPos),revocation.Parent.Value)
            negatedRevocation.ArgList.Add revocation
            let validStmt = 
                { ValidStatement.Node = negatedRevocation
                  ValidStatement.ValidityReason = ValidityReason.IsInferredFromRevocation negatedRevocation
                  ValidStatement.StatementExpression = negatedRevocation.Type SignatureType.Mixed }
            _theoremStore.TryAdd(validStmt.StatementExpression, validStmt) |> ignore

    member this.LastAssumedArgument =
        if _assumedArguments.Count > 0 then
            Some (_assumedArguments.Peek())
        else 
            None

    member this.Clear() =
        _theoremStore.Clear() // TODO unify assumed arguments with theoremStore
        _assumedArguments.Clear()

    member this.Count = _theoremStore.Count


    

