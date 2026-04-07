/// This module contains classes used to store valid statements globally by the FplInterpreter

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreter.Globals.Validity
open System.Collections.Generic
open System.Text.Json
open FplPrimitives
open FplInterpreterBasicTypes
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
            | ValidityReason.IsDerivedAssumed assumption ->
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
                  ValidStatement.ValidityReason = ValidityReason.IsDerivedRevoke negatedRevocation
                  ValidStatement.StatementExpression = negatedRevocation.Type SignatureType.Name }
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
              ValidStatement.ValidityReason = ValidityReason.IsRuleOfInference inferredArg
              ValidStatement.StatementExpression = inferredArg.Type SignatureType.Mixed }
        _theoremStore.TryAdd(validStmt.StatementExpression, validStmt) |> ignore

    /// Produces a JSON string grouping stored valid statements by their ValidityReason.
    /// Each group's value is an array of JSON objects. Objects may contain multiple key/value pairs.
    member this.ToJson() =
        // group statements by human-friendly category label
        let groups = Dictionary<string, ResizeArray<Dictionary<string,string>>>()

        let getKey reason =
            match reason with
            | ValidityReason.IsAxiom _ -> PrimTitleAxioms
            | ValidityReason.IsAxiomAssertion _ -> PrimTitleAxioms
            | ValidityReason.IsRuleOfInference _ -> PrimTitleRuleOfInference
            | ValidityReason.IsTheorem _ -> PrimTitleTheorems
            | ValidityReason.IsDerived _ -> PrimTitleDerived
            | ValidityReason.IsDerivedAssumed _ -> PrimTitleDerived
            | ValidityReason.IsDerivedRevoke _ -> PrimTitleDerived
            | ValidityReason.Error -> "Error"
         
        let getReason reason =
            match reason with
            | ValidityReason.IsAxiom _ -> LiteralAxL
            | ValidityReason.IsAxiomAssertion _ -> PrimAssertion
            | ValidityReason.IsRuleOfInference _ -> PrimRuleOfInference
            | ValidityReason.IsTheorem _ -> PrimTitleTheorems
            | ValidityReason.IsDerived _ -> PrimArgInfDerive
            | ValidityReason.IsDerivedAssumed _ -> PrimArgInfAssume
            | ValidityReason.IsDerivedRevoke _ -> PrimArgInfRevoke
            | ValidityReason.Error -> "Error"

        // Build the groups storing per-statement object dictionaries
        for kvp in _theoremStore do
            let stmt = kvp.Value
            let key = getKey stmt.ValidityReason
            let list =
                match groups.TryGetValue key with
                | true, v -> v
                | _ ->
                    let v = ResizeArray<Dictionary<string,string>>()
                    groups.Add(key, v)
                    v
            // create a small object for the statement; first pair is statementExpression
            let obj = Dictionary<string,string>()
            obj.Add("statementExpression", $"{stmt.StatementExpression}")
            let parentNodeOpt = stmt.Node.UltimateBlockNode
            match parentNodeOpt with
            | Some parent ->
                obj.Add("nodeName", $"{parent.Name} {parent.Type SignatureType.Mixed}")
                obj.Add("Line", $"{stmt.Node.StartPos.Line}")
                obj.Add("Column", $"{stmt.Node.StartPos.Column}")
                match parent.Parent with
                | Some theory ->
                    match theory.FilePath with
                    | Some filePath ->
                        obj.Add("FilePath", filePath)
                    | _ -> ()
                | _ -> ()
            | _ -> ()
            // additional key/value pairs can be added to `obj` here in future
            list.Add(obj)

        JsonSerializer.Serialize(groups)

    member this.ClearValidityStore() =
        _theoremStore.Clear() 
        _assumedArguments.Clear()

    member this.Count = _theoremStore.Count

