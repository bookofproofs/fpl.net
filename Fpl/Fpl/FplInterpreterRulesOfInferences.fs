/// This module contains all types used by the FplInterpreter
/// to model / interpret rules of inferences

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterRulesOfInferences
open FParsec
open FplPrimitives
open FplGrammarTypes
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.HelpersBasic
open FplInterpreter.Globals.HelpersComplex
open FplInterpreterVariables



type FplPredicateList(positions: Positions, parent: FplGenericNode, runOrder) = 
    inherit FplGenericIsAction(positions, parent)
    let _runOrder = runOrder
    override this.Name = LiteralPreL
    override this.ShortName = LiteralInf

    override this.Clone () =
        let ret = new FplPredicateList((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = signatureSep ", " this.ArgList signatureType

    override this.Run() = 
        debug this Debug.Start
        // this line only makes sure that all Run is called recursively
        // FplPredicateList has no value its own
        this.ArgList |> Seq.map (fun fv -> fv.Run()) |> ignore
        debug this Debug.Stop

    override this.EmbedInSymbolTable _ = addExpressionToParentArgList this

    override this.RunOrder = Some _runOrder

type FplRuleOfInference(positions: Positions, parent: FplGenericNode, runOrder) as this =
    inherit FplGenericIsAction(positions, parent)
    let _runOrder = runOrder
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

    do
        this.FplId <- LiteralUndef
        this.TypeId <- LiteralUndef

    member this.SignStartPos
        with get() = _signStartPos
        and set(value) = _signStartPos <- value

    member this.SignEndPos
        with get() = _signEndPos
        and set(value) = _signEndPos <- value

    interface IHasSignature with
        member this.SignStartPos 
            with get () = this.SignStartPos
            and set (value) = this.SignStartPos <- value
        member this.SignEndPos 
            with get () = this.SignEndPos
            and set (value) = this.SignEndPos <- value

    override this.Name = PrimRuleOfInference
    override this.ShortName = LiteralInf

    override this.Clone () =
        let ret = new FplRuleOfInference((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getFplHead this signatureType
    
    override this.IsFplBlock () = true
    override this.IsBlock () = true    

    override this.Run() = 
        // FplRuleOfReference does not have any Value and doesn't need run
        ()

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        checkVAR04Diagnostics this
        tryAddToParentUsingFplId this

    override this.RunOrder = Some _runOrder

