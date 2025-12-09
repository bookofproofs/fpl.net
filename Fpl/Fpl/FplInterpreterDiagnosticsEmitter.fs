/// This modulde contains all side-effect functions necessary to emit diagnostics for the FPL language server.
module FplInterpreterDiagnosticsEmitter

open System.Collections.Generic
open System.Text.RegularExpressions
open FParsec
open FplPrimitives
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterDiagnosticsEmitterPre
(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

let emitSIG01Diagnostics (st: SymbolTable) (fv: FplValue) pos1 pos2 =
    match fv with
    | :? FplReference ->
        // collect candidates to match this reference from all theories and
        // add them to fplValues's scope
        let expressionId = fv.FplId

        st.Root.Scope
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun theory ->
            theory.Scope
            |> Seq.map (fun kv -> kv.Value)
            |> Seq.iter (fun block ->
                if expressionId = block.FplId then
                    let blockType = block.Type(SignatureType.Mixed)
                    fv.Scope.Add(blockType, block)
                    fv.TypeId <- block.TypeId
                else
                    let blockType = block.Type(SignatureType.Mixed)
                    match block.ExpressionType with
                    | FixType.Prefix symbol
                    | FixType.Symbol symbol
                    | FixType.Postfix symbol ->
                        if expressionId = symbol then
                            fv.Scope.Add(blockType, block)
                            fv.TypeId <- block.TypeId
                    | FixType.Infix(symbol, precedence) ->
                        if expressionId = symbol then
                            fv.Scope.Add(blockType, block)
                            fv.TypeId <- block.TypeId
                    | _ -> ()))

        if fv.Scope.Count = 0 then
            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Error
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = SIG01 expressionId
                    Diagnostic.Alternatives = Some "Declare a functional term, predicate, or class with this symbol." 
                }
            ad.AddDiagnostic diagnostic
    | _ -> ()

let emitSIG02Diagnostics (st: SymbolTable) (fplValue: FplValue) pos1 pos2 =
    let precedences = Dictionary<int, FplValue>()

    match fplValue.ExpressionType with
    | FixType.Infix(symbol, precedence) ->
        let precedenceWasAlreadyThere precedence fv =
            if not (precedences.ContainsKey(precedence)) then
                precedences.Add(precedence, fv)
                false
            else
                true

        st.Root.Scope
        |> Seq.map (fun kv -> kv.Value)
        |> Seq.iter (fun theory ->
            theory.Scope
            |> Seq.map (fun kv1 -> kv1.Value)
            |> Seq.iter (fun block ->
                match block.ExpressionType with
                | FixType.Infix(_, precedence) -> precedenceWasAlreadyThere precedence block |> ignore
                | _ -> ()))

        if precedences.ContainsKey(precedence) then
            let conflict = precedences[precedence].QualifiedStartPos

            let diagnostic =
                { 
                    Diagnostic.Uri = ad.CurrentUri
                    Diagnostic.Emitter = DiagnosticEmitter.FplInterpreter
                    Diagnostic.Severity = DiagnosticSeverity.Information
                    Diagnostic.StartPos = pos1
                    Diagnostic.EndPos = pos2
                    Diagnostic.Code = SIG02(symbol, precedence, conflict)
                    Diagnostic.Alternatives = Some "Consider disambiguating the precedence to avoid unexpected results." 
                }
            ad.AddDiagnostic diagnostic
    | _ -> ()


