/// This module contains all symbol table nodes used by the FplInterpreter
/// to model the cases statement.

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterCasesStmt
open System.Collections.Generic
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.HelpersBasic
open FplInterpreterChecks
open FplInterpreterIntrinsicTypes



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
