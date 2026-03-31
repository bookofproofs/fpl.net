/// This module contains all types used by the FplInterpreter
/// to model / interpret the mcases statement

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 
*)
module FplInterpreterMapCases
open System.Collections.Generic
open FplPrimitives
open FplGrammarTypes
open ErrDiagnostics
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Main
open FplInterpreterChecks
open FplInterpreterSTEmbedding
open FplInterpreterIntrinsicTypes

type FplMapCaseSingle(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    do 
        this.FplId <- PrimMapCaseSingle

    override this.Name = PrimMapCaseSingleL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCaseSingle((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetCondition() = this.ArgList[0]
    member this.GetResult() = this.ArgList[1] :?> FplGenericHasValue

    override this.CheckConsistency() = 
        base.CheckConsistency()
        checkArgPred this (this.GetCondition())

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run() = 
        debug this Debug.Start
        let result = this.GetResult()
        result.Run()
        this.SetValueOf result
        debug this Debug.Stop

type FplMapCaseElse(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    do 
        this.FplId <- PrimMapCaseElse

    override this.Name = PrimMapCaseElseL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCaseElse((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        let argOpt = this.ArgList |> Seq.tryHead 
        match argOpt with 
        | Some arg -> arg.Type signatureType // delegate type to the argument of MapCaseElse case
        | _ -> getFplHead this signatureType // fallback (should never occur due to FPL syntax)

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run() = 
        debug this Debug.Start
        let first = this.ArgList |> Seq.head
        let contentOfElsResult = first :?> FplGenericHasValue
        contentOfElsResult.Run()
        this.SetValueOf contentOfElsResult
        debug this Debug.Stop

type FplMapCases(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericHasValue(positions, parent)
    let _consistentCaseType = new FplIntrinsicTpl("", positions, parent)
    let _reachableCases = new HashSet<string>()

    do 
        this.FplId <- LiteralMapCases

    override this.Name = PrimMapCasesL
    override this.ShortName = PrimStmt

    override this.Clone () =
        let ret = new FplMapCases((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = 
        getFplHead this signatureType

    member this.GetConditionResultList() = 
        this.ArgList
        |> Seq.choose (fun item ->
            match item with
            | :? FplMapCaseSingle as condRes -> Some condRes
            | _ -> None)
        |> Seq.toList

    member this.GetMapElse() = 
        let last = this.ArgList |> Seq.last
        last :?> FplGenericHasValue

    member private this.CheckAllResultsForEqualType() =
        // check if all results have the same type
        this.GetConditionResultList()
        |> Seq.map (fun conditionResultPair -> conditionResultPair.GetResult())
        |> Seq.iter (fun result -> _consistentCaseType.TrySetTemplateUsage result (SIG13("", "", "", "").Code))
        // check also else result
        _consistentCaseType.TrySetTemplateUsage (this.GetMapElse()) (SIG13("", "", "", "").Code)
        match _consistentCaseType.ErrorOccurred with
        | Some errMsg -> 
            // Since there were proceeding errors regarding inconsistent Type Ids of some branches
            // set the TypeId of this FplMapCases to undefined
            this.TypeId <- LiteralUndef  
         | _ ->
            // Set the TypeId of this FplMapCases to the consistent TypeId found for all of its branches
            let typeOfAllBranches = _consistentCaseType.RefersTo.Value
            let mapOpt = getMapping typeOfAllBranches
            this.TypeId <- 
                match mapOpt with 
                | Some map -> map.TypeId // if the type of all branches is a mapping, use the mapping's TypeId
                | None -> typeOfAllBranches.TypeId

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
        this.CheckAllResultsForEqualType()
        this.CheckAllCasesForBeingReachable()


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

    override this.RunOrder = None

    override this.Run() = 
        debug this Debug.Start
        let resultLst = this.GetConditionResultList()
        let mapElse = this.GetMapElse()
        let firstMapCaseWithTrueConditionOpt = 
            resultLst
            |> Seq.tryFind(fun mapCaseSingle -> 
                let condition = mapCaseSingle.GetCondition()
                condition.Run()
                condition.Represent() = LiteralTrue
            )
        match firstMapCaseWithTrueConditionOpt with
        | Some firstMapCaseWithTrueCondition -> 
            firstMapCaseWithTrueCondition.Run()
            let resOfFound = firstMapCaseWithTrueCondition.GetResult()
            this.SetValueOf resOfFound
        | None -> 
            mapElse.Run()
            this.SetValueOf mapElse
        debug this Debug.Stop
