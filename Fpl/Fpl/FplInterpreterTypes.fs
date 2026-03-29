/// This module contains all types necessary to interpret FPL code (semantics)
(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterTypes

open System
open System.Collections.Generic
open System.Text
open FplPrimitives
open FplGrammarTypes
open ErrDiagnostics
open FplInterpreterDiagnosticsEmitter
open FplInterpreterAstPreprocessing
open FplInterpreterBasicTypes
open FplInterpreterGlobals
open FplInterpreterChecks
open FplInterpreterSTEmbedding
open FplInterpreterReferences
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables
open FplInterpreterDefinitions
open FplInterpreterFplTypeMatching
open FplInterpreterReferencesSelfParent

[<AbstractClass>]
type FplGenericStmt(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericIsAction(positions, parent)

    override this.ShortName = PrimStmt

    override this.Type signatureType = this.FplId

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

type FplAssertion(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericStmt(positions, parent)

    override this.Name = PrimAssertion
    override this.ShortName = LiteralAss

    override this.Clone () =
        let ret = new FplAssertion((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Run() = 
        // TODO implement run
        debug this Debug.Start
        debug this Debug.Stop

    override this.RunOrder = None


/// Implements the semantics of the FPL is operator.
type FplIsOperator(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralIs

    override this.Name = PrimIsOperator
    override this.ShortName = LiteralIs

    override this.Clone () =
        let ret = new FplIsOperator((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let head = getFplHead this signatureType
        let args = signatureSep ", " this.ArgList signatureType
        sprintf "%s(%s)" head args
        
    override this.Run() = 
        debug this Debug.Start
        let operand = this.ArgList[0]
        let typeOfOperand = this.ArgList[1]
        // FPL truth-table
        match operand with 
        | :? FplReference as op ->
            match mpwa [operand] [typeOfOperand] with
            | Some errMsg -> 
                let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
                newValue.FplId <- LiteralFalse
                this.SetValue newValue
            | None -> 
                let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
                newValue.FplId <- LiteralTrue
                this.SetValue newValue
        | _ -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralFalse
            this.SetValue newValue
        
        debug this Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this


type FplCaseSingle(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimCaseSingle

    override this.Name = PrimCaseSingleL

    override this.Clone () =
        let ret = new FplCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetCondition() = this.ArgList[0]
    member this.StmtsAfterCondition() = this.ArgList |> Seq.tail

    override this.CheckConsistency() = 
        base.CheckConsistency()
        checkArgPred this (this.GetCondition())

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.Run() = 
        debug this Debug.Start
        this.StmtsAfterCondition()
        |> Seq.iter (fun stmt -> stmt.Run())
        debug this Debug.Stop

type FplCaseElse(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimCaseElse

    override this.Name = PrimCaseElseL

    override this.Clone () =
        let ret = new FplCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    override this.Run() = 
        debug this Debug.Start
        this.ArgList 
        |> Seq.iter (fun stmt -> stmt.Run())
        debug this Debug.Stop

type FplCases(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    let _reachableCases = new HashSet<string>()
    do 
        this.FplId <- LiteralCases

    override this.Name = PrimCasesL

    override this.Clone () =
        let ret = new FplCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplCaseSingle as condRes -> Some condRes
            | _ -> None)
        |> Seq.toList

    member this.GetElseStmt() = this.ArgList |> Seq.last

    member private this.CheckAllCasesForBeingReachable() =
        _reachableCases.Clear()
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetCondition())
        |> Seq.iter (fun condition -> 
            let conditionSignature = condition.Type SignatureType.Name
            if _reachableCases.Add(conditionSignature) then 
                () // signature added
            else
                // signature was already added
                this.ErrorOccurred <- emitSIG14diagnostics condition.StartPos condition.EndPos
                
        )

    override this.CheckConsistency() = 
        base.CheckConsistency()
        this.CheckAllCasesForBeingReachable()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.Run() = 
        debug this Debug.Start
        let resultLst = this.GetConditionResultList()
        let elseStmt = this.GetElseStmt()
        let firstCaseWithTrueConditionOpt = 
            resultLst
            |> Seq.tryFind(fun caseSingle -> 
                let condition = caseSingle.GetCondition()
                condition.Run()
                condition.Represent() = LiteralTrue
            )
        match firstCaseWithTrueConditionOpt with
        | Some firstCaseWithTrueCondition -> 
            firstCaseWithTrueCondition.Run()
        | None -> 
            elseStmt.Run()
        debug this Debug.Stop

type FplForEnumeratorType = 
    | ArrayElements
    | Predicative
    | Error

type FplForInStmt(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- LiteralFor

    override this.Name = PrimForInStmtL

    override this.Clone () =
        let ret = new FplForInStmt((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.Entity =
        if this.ArgList.Count > 0 then 
            this.ArgList[0].RefersTo
        else 
            None

    member this.Domain =
        if this.ArgList.Count > 1 then 
            this.ArgList[1].RefersTo 
        else 
            None

    member this.Body =
        // the body of the for statement starts after the entity and after the domain
        if this.ArgList.Count > 2 then 
            this.ArgList |> Seq.tail |> Seq.tail |> Seq.toList
        else
            []

    member this.GetEnumerator() =
        match this.Domain with
        | Some (:? FplVariableArray as domain) ->
            (FplForEnumeratorType.ArrayElements, domain.ValueList |> Seq.toList)
        | Some domain ->
            this.ErrorOccurred <- emitST005diagnostics (domain.Type SignatureType.Name) domain.Name this.ArgList[1].StartPos this.ArgList[1].EndPos
            (FplForEnumeratorType.Error, [])
        | _ ->
            this.ErrorOccurred <- emitST005diagnostics "missing" PrimNone this.StartPos this.StartPos
            (FplForEnumeratorType.Error, [])
            
    override this.Run() = 
        debug this Debug.Start
        match this.Entity, this.GetEnumerator() with
        | Some (:? FplGenericHasValue as entity), (FplForEnumeratorType.ArrayElements, lst) ->
            lst
            |> List.iter (fun lstElement ->
                // TODO: check type compatibility of entity accepting lstElement
                entity.Value <- Some lstElement
                this.Body
                |> List.iter (fun stmt ->
                    stmt.Run()
                )
            )
        | _, _ -> ()
        debug this Debug.Stop

type FplForInStmtEntity(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimForInStmtEntity

    override this.Name = PrimForInStmtEntityL

    override this.Clone () =
        let ret = new FplForInStmtEntity((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let entityOpt = referencedNodeOpt this
        match entityOpt with 
        | Some entity -> entity.Type signatureType
        | _ -> getFplHead this signatureType

    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    override this.Run() = 
        // TODO implement run
        debug this Debug.Start
        debug this Debug.Stop

type FplForInStmtDomain(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)
    do 
        this.FplId <- PrimForInStmtDomain

    override this.Name = PrimForInStmtDomainL

    override this.Clone () =
        let ret = new FplForInStmtDomain((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let domainOpt = referencedNodeOpt this
        match domainOpt with 
        | Some domain -> domain.Type signatureType
        | _ -> getFplHead this signatureType
    override this.EmbedInSymbolTable _ = tryAddToParentForInStmt this

    override this.Run() = 
        // TODO implement run
        debug this Debug.Start
        debug this Debug.Stop

/// Implements the assignment statement in FPL.
type FplAssignment(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericStmt(positions, parent)

    do
        this.FplId <- PrimAssignment
        this.TypeId <- LiteralUndef

    override this.Name = PrimAssignmentL

    override this.Clone () =
        let ret = new FplAssignment((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member private this.GetAssignmentArg no =
        if this.ArgList.Count > 1 then 
            let candidate = this.ArgList[no]
            match candidate with 
            | :? FplReference as ref ->
                match ref.DottedChild with 
                | Some dc -> dc.RefersTo
                | None when ref.RefersTo.IsSome -> ref.RefersTo
                | _ -> Some candidate
            | _ ->
                Some candidate
        else
            None

    member this.Assignee:FplGenericNode option = this.GetAssignmentArg 0

    member this.AssignedValue = 
        let assignedValueOpt = this.GetAssignmentArg 1
        match assignedValueOpt with 
        | Some (:? FplVariableArray as targetArray) when this.ArgList[1].ArgType = ArgType.Brackets ->
            let targetCoords = representationSep "|" (this.ArgList[1].ArgList) 
            let valueAtTargetCoordinates = targetArray.GetValueByCoordinates targetCoords
            Some valueAtTargetCoordinates
        | Some _ -> assignedValueOpt
        | None -> Some (new FplIntrinsicUndef((this.StartPos, this.EndPos), this))

    override this.CheckConsistency () = 
        base.CheckConsistency()
        let checkTypes (assignee:FplGenericNode) (assignedValue:FplGenericNode) =
            let nameAssignee = assignee.Type SignatureType.Name
            let nameAssignedValue = assignedValue.Type SignatureType.Name
            if nameAssignee = nameAssignedValue then
                this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue assignedValue.StartPos assignedValue.EndPos
            else
                // assignee is to be treated as parameter, the assignedValue as argument
                match mpwa [assignedValue] [assignee] with
                | Some errMsg ->
                    this.ErrorOccurred <- emitSIG05Diagnostics errMsg this.ArgList[1].StartPos this.ArgList[1].EndPos
                | _ -> ()
                
        let checkErrorOccuredInReference (fv:FplGenericNode) = 
            match fv with
            | :? FplReference as ref -> 
                this.ErrorOccurred <- ref.ErrorOccurred 
            | _ -> ()

        match this.ArgList[0], this.Assignee with
        | :? FplReference as ref, Some assignee when ref.ArgType = ArgType.Parentheses ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "an expression" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | :? FplReference as ref, Some (:? FplGenericIsValue as assignee) ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "a value" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | :? FplReference as ref, Some (:? FplReference as assignee) when assignee.RefersTo.IsNone ->
            this.ErrorOccurred <- emitSIG07iagnostic (ref.Type SignatureType.Name) "undefined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

        let nameAssignee = this.ArgList[0].Type SignatureType.Name
        let nameAssignedValue = this.ArgList[1].Type SignatureType.Name
        if nameAssignee = nameAssignedValue then
            // something has been assigned to itself
            this.ErrorOccurred <- emitLG005Diagnostics nameAssignedValue this.ArgList[1].StartPos this.ArgList[1].EndPos

        // remember proceeding errors of references used in the assignment (if any)
        checkErrorOccuredInReference this.ArgList[0]
        checkErrorOccuredInReference this.ArgList[1]
        match this.ErrorOccurred, this.Assignee, this.AssignedValue with
        | None, Some (:? FplVariable as assignee), Some (assignedValue:FplGenericNode) when assignedValue.Name = PrimClassL ->
            assignee.IsInitialized <- true
            checkTypes assignee assignedValue
        | None, Some (:? FplVariable as assignee), Some (assignedValue:FplGenericNode) when (assignedValue.Name = PrimFunctionalTermL || assignedValue.Name = PrimMandatoryFunctionalTermL) && isCallByValue this.ArgList[1] ->
            let mapOpt = getMapping assignedValue
            match mapOpt with 
            | Some map -> checkTypes this.ArgList[0] map
            | _ -> checkTypes assignee assignedValue
        | None, Some (:? FplVariable as assignee), Some _ -> 
            checkTypes assignee this.ArgList[1] 
        | None, Some (:? FplVariableArray as assignee), Some assignedValue ->
           checkTypes this.ArgList[0] this.ArgList[1] 
        | None, Some (:? FplSelf as assignee), _ ->
            match assignee.RefersTo with 
            | Some ref -> 
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Name false) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            | None ->
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of self could not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | None, Some (:? FplParent as assignee), _ ->
            match assignee.RefersTo with 
            | Some ref -> 
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) (getEnglishName ref.Name false) assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
            | None ->
                this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) "the type of parent could not be determined" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | None, Some (assignee), Some assignedValue ->
            this.ErrorOccurred <- emitSIG07iagnostic (assignee.Type SignatureType.Name) $"type `{assignee.Type SignatureType.Type}`" assignee.Name (this.ArgList[0].StartPos) (this.ArgList[0].EndPos)
        | _ -> ()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    member private this.SetAssignee (fv:FplGenericNode) = 
        match this.Assignee with
        | Some (:? FplVariable as assignee) ->
            assignee.SetValue fv
        | Some (:? FplVariableArray as assignee) ->
            let coordinatesKey = representationSep "|" (this.ArgList[0].ArgList) 
            assignee.AssignValueToCoordinates coordinatesKey fv // set value of array
        | _ -> ()

    override this.Run() =
        debug this Debug.Start

        match this.ErrorOccurred, this.ArgList[1], this.AssignedValue with 
        | Some _, _, _ ->
            () // skip assignment, if any proceeding errors occured
        | None, (:? FplGenericHasValue as ref), Some (:? FplVariableArray as assignedValue) ->
            this.SetAssignee assignedValue
        | None, (:? FplGenericHasValue as ref), Some (:? FplGenericIsValue as assignedValue) ->
            this.SetAssignee assignedValue
        | None, (:? FplGenericHasValue as ref), _ ->
            ref.Run()
            this.SetAssignee (ref.Value.Value)
        | None, (:? FplGenericIsValue as ref), _ ->
            this.SetAssignee ref
        | _ -> ()

        debug this Debug.Stop



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



