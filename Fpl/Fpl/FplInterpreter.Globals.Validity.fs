/// This module contains classes used to store valid statements globally by the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.Globals.Validity
open System.Collections.Generic
open System.Text
open FplInterpreterBasicTypes
open FplInterpreter.Globals.HelpersBasic
open FplInterpreterCompoundPredicates

type ValidStmtStore() =
    let _theoremStore = Dictionary<string, ValidStatement>()
    let _assumedArguments = Stack<FplGenericNode>()

    /// Registers an expression in the theorem store
    member this.RegisterExpression (st:FplGenericNode) =
        match box st with
        | :? IInferrable as infer ->
            let validStmt = infer.InferrableExpression
            match validStmt.ValidityReason with
            | ValidityReason.Error -> false // do nothing if error was flagged
            | ValidityReason.IsAssumed assumption ->
                _assumedArguments.Push assumption
                _theoremStore.TryAdd(validStmt.StatementExpression, validStmt) |> ignore
                true
            | _ ->
                _theoremStore.TryAdd(validStmt.StatementExpression, validStmt) |> ignore
                true
        | _ -> false

    /// Assumes an argument and puts it to the theorem store
    member this.AssumeArgument assumption =
        if this.RegisterExpression assumption then 
            _assumedArguments.Push assumption
            true
        else
            false

    /// Revokes an argument and puts its negation to the theorem store
    member this.RevokeLastArgument() =
        if _assumedArguments.Count > 0 then
            let revocation = _assumedArguments.Pop()
            let assumptionId = revocation.Type SignatureType.Mixed
            // remove assumption proved wrong from valid stmts store
            _theoremStore.Remove assumptionId |> ignore 
            // and replace it with its negated version
            let negatedRevocation = new FplNegation((revocation.StartPos, revocation.EndPos),revocation.Parent.Value)
            negatedRevocation.ArgList.Add revocation
            let validStmt = 
                { ValidStatement.Node = negatedRevocation
                  ValidStatement.ValidityReason = ValidityReason.IsInferredFromRevocation negatedRevocation
                  ValidStatement.StatementExpression = negatedRevocation.Type SignatureType.Mixed }
            _theoremStore.TryAdd(validStmt.StatementExpression, validStmt) |> ignore
            true
        else
            false

    member this.LastAssumedArgument =
        if _assumedArguments.Count > 0 then
            Some (_assumedArguments.Peek())
        else 
            None

    member this.AddInferredArgument inferredArg = 
        let validStmt = 
            { ValidStatement.Node = inferredArg
              ValidStatement.ValidityReason = ValidityReason.IsInferred inferredArg
              ValidStatement.StatementExpression = inferredArg.Type SignatureType.Mixed }
        _theoremStore.TryAdd(validStmt.StatementExpression, validStmt) |> ignore

    /// Produces a JSON string grouping stored valid statements by their ValidityReason.
    /// The resulting JSON maps each reason name to an array of StatementExpression strings.
    /// NOTE: This implementation builds JSON with a StringBuilder and performs necessary escaping.
    member this.ToJson() =
        // group statements by human-friendly category label
        let groups = Dictionary<string, ResizeArray<string>>()

        let getKey reason =
            match reason with
            | IsAxiom _ -> "Axioms"
            | IsAsserted _ -> "Assertions"
            | IsAssumed _ -> "Assumptions"
            | IsInferred _ -> "Inferred Arguments"
            | IsInferredFromRevocation _ -> "Inferred from Revocations"
            | Error -> "Error"

        for kvp in _theoremStore do
            let stmt = kvp.Value
            let key = getKey stmt.ValidityReason
            let list =
                match groups.TryGetValue key with
                | true, v -> v
                | _ ->
                    let v = ResizeArray<string>()
                    groups.Add(key, v)
                    v
            // keep the original marker used previously
            list.Add $"∀Ǝ{stmt.StatementExpression}"

        // build JSON with StringBuilder to avoid System.Text.Json allocations
        let sb = StringBuilder()
        sb.Append('{') |> ignore
        let mutable firstGroup = true
        for kv in groups do
            if not firstGroup then sb.Append(',') |> ignore
            firstGroup <- false
            sb.Append('"') |> ignore
            sb.Append(escape kv.Key) |> ignore
            sb.Append("\":[") |> ignore
            let arr = kv.Value
            for i = 0 to arr.Count - 1 do
                if i > 0 then sb.Append(',') |> ignore
                sb.Append('"') |> ignore
                sb.Append(escape arr.[i]) |> ignore
                sb.Append('"') |> ignore
            sb.Append(']') |> ignore
        sb.Append('}') |> ignore
        sb.ToString()

    member this.ClearValidityStore() =
        _theoremStore.Clear() 
        _assumedArguments.Clear()

    member this.Count = _theoremStore.Count

