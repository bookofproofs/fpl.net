/// This module contains the symbol table type of the FPL interpreter.
(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterST

open System
open System.Collections.Generic
open System.Text
open FplPrimitives
open ErrDiagnostics
open FplInterpreterAstPreprocessing
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Root

type SymbolTable(parsedAsts: ParsedAstList, debug: bool, offlineMode: bool) =
    let _parsedAsts = parsedAsts
    let mutable _mainTheory = ""
    let _evalLog = List<string>()
    let _root = new FplRoot()
    let _debug = debug
    let _offlineMode = offlineMode

    /// Returns the current OfflineMode, with which the SymbolTable was created. 
    /// OfflineMode=True should not be used in production. If true, the unit tests will try to 
    /// get a local copy of Fpl libraries instead of trying to download them from the Internet.
    member this.OfflineMode
        with get () = _offlineMode

    /// Returns the current main theory.
    member this.MainTheory
        with get () = _mainTheory
        and set (value) = _mainTheory <- value

    /// Returns the evaluation root node of the symbol table.
    member this.Root = _root

    /// Returns the list of parsed asts
    member this.ParsedAsts = _parsedAsts

    /// Returns the string representation of all asts .
    member this.AstsToString =
        let res =
            _parsedAsts
            |> Seq.map (fun pa -> pa.Parsing.Ast.ToString())
            |> String.concat Environment.NewLine

        res

    /// If there is a valid topological sorting, order the list descending by this ordering.
    member this.OrderAsts() =
        _parsedAsts.Sort(
            Comparer<ParsedAst>.Create(fun b a -> compare a.Sorting.TopologicalSorting b.Sorting.TopologicalSorting)
        )

    /// Serializes the symbol table as json
    member this.ToJson() =
        let sb = StringBuilder()
        let mutable currentPath = ""

        let rec createJson (root: FplGenericNode) (sb: StringBuilder) level isLast preventInfinite =
            match root.FilePath with
            | Some path -> currentPath <- path
            | _ -> ()

            let indent, indentMinusOne =
                if _debug then
                    String(' ', level), String(' ', level - 1)
                else
                    String.Empty, String.Empty

            sb.AppendLine(indentMinusOne + "{") |> ignore
            let name = $"{root.Type(SignatureType.Name)}".Replace(@"\", @"\\")
            let fplTypeName = $"{root.Type(SignatureType.Type)}".Replace(@"\", @"\\")
            let mutable fplValueRepr = $"{root.Represent()}".Replace("\\", "\\\\")   // escape backslashes first
                                                            .Replace("\"", "\\\"")   // then escape double quotes

            if name = this.MainTheory then
                sb.AppendLine($"{indent}\"Name\": \"(Main) {name}\",") |> ignore
            else
                sb.AppendLine($"{indent}\"Name\": \"{name}\",") |> ignore

            sb.AppendLine($"{indent}\"Type\": \"{root.ShortName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueType\": \"{fplTypeName}\",") |> ignore
            sb.AppendLine($"{indent}\"FplValueRepr\": \"{fplValueRepr}\",") |> ignore
            sb.AppendLine($"{indent}\"Line\": \"{root.StartPos.Line}\",") |> ignore
            sb.AppendLine($"{indent}\"Column\": \"{root.StartPos.Column}\",") |> ignore
            sb.AppendLine($"{indent}\"FilePath\": \"{currentPath}\",") |> ignore

            if preventInfinite then
                sb.AppendLine($"{indent}\"Scope\": [],") |> ignore
                sb.AppendLine($"{indent}\"ArgList\": [],") |> ignore
                sb.AppendLine($"{indent}\"ValueList\": []") |> ignore
            else
                sb.AppendLine($"{indent}\"Scope\": [") |> ignore
                let mutable counterScope = 0
                root.Scope
                |> Seq.iter (fun child ->
                    counterScope <- counterScope + 1
                    createJson
                        child.Value
                        sb
                        (level + 1)
                        (counterScope = root.Scope.Count)
                        (root.FplId = LiteralSelf || root.FplId = LiteralParent))
                sb.AppendLine($"{indent}],") |> ignore

                sb.AppendLine($"{indent}\"ArgList\": [") |> ignore
                let mutable argList = 0
                root.ArgList
                |> Seq.iter (fun child ->
                    argList <- argList + 1
                    createJson child sb (level + 1) (argList = root.ArgList.Count) false)
                sb.AppendLine($"{indent}],") |> ignore

                sb.AppendLine($"{indent}\"ValueList\": [") |> ignore
                let mutable valueList = 0
                match root with 
                | :? FplGenericHasValue as rootWithValue ->
                    match rootWithValue.Value with
                    | Some ref -> createJson ref sb (level + 1) true false
                    | None -> ()
                | _ -> ()
                sb.AppendLine($"{indent}]") |> ignore

            if isLast then
                sb.AppendLine(indentMinusOne + "}") |> ignore
            else
                sb.AppendLine(indentMinusOne + "},") |> ignore

        createJson this.Root sb 1 false false
        let res = sb.ToString().TrimEnd()

        if res.EndsWith(',') then
            res.Substring(0, res.Length - 1)
        else
            res

    /// Returns the uses dependencies of this symbol table needed e.g. for debugging purposes in the FPL language server.
    member this.UsesDependencies() =
        let sb = StringBuilder()
        sb.AppendLine() |> ignore
        sb.AppendLine("SymbolTable: ") |> ignore

        this.Root.Scope
        |> Seq.map (fun theory -> $"{theory.Value.Type(SignatureType.Mixed)} ({theory.Value.Scope.Count})")
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore

        sb.AppendLine("ParsedAsts: ") |> ignore

        this.ParsedAsts
        |> Seq.map (fun pa ->
            $"[{pa.Id}, {pa.Sorting.TopologicalSorting}, {pa.Sorting.ReferencedAsts}, {pa.Sorting.ReferencingAsts}]")
        |> String.concat Environment.NewLine
        |> sb.AppendLine
        |> ignore

        sb.ToString()

    /// Creates trace statistics needed e.g. for debugging purposes in the FPL language server.
    member this.TraceStatistics =
        let sb = StringBuilder()

        this.ParsedAsts
        |> Seq.iter (fun pa ->
            let paDiagnostics = ad.GetStreamDiagnostics(pa.Parsing.Uri)

            let statsDiags =
                paDiagnostics.Values
                |> Seq.groupBy (fun d -> $"{d.Emitter}({d.Code.Code})")
                |> Seq.map (fun (groupId, group) -> $"{groupId}:{Seq.length group}")
                |> String.concat ", "

            sb.AppendLine $"{pa.Id}(chksm {pa.Parsing.Checksum}): #total diags {paDiagnostics.Count}, {statsDiags}"
            |> ignore)

        sb.ToString()



