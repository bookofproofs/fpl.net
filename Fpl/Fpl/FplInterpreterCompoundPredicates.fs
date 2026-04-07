/// This module contains all nodes of the symbol table used by the FplInterpreter
/// to interpret compound predicates

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module FplInterpreterCompoundPredicates
open FplPrimitives
open FplGrammarTypes
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Debug
open FplInterpreterChecks
open FplInterpreter.Globals.HelpersBasic
open FplInterpreterIntrinsicTypes


/// Implements the semantics of an FPL conjunction compound predicate.
type FplConjunction(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralAnd

    override this.Name = PrimConjunction
    override this.ShortName = LiteralAnd

    override this.Clone () =
        let ret = new FplConjunction((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getNotationTwoArgs this "∧" signatureType LiteralPred

    override this.Run() =
        debug this Debug.Start
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run()
        arg2.Run()
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        // FPL truth-table
        match (arg1Repr, arg2Repr) with
        | (LiteralFalse, _) 
        | (_, LiteralFalse)  ->
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralFalse
            this.SetValue newValue
        | (LiteralTrue, LiteralTrue) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralTrue
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()
        debug this Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


/// Implements the semantics of an FPL disjunction compound predicate.
type FplDisjunction(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralOr

    override this.Name = PrimDisjunction
    override this.ShortName = LiteralOr

    override this.Clone () =
        let ret = new FplDisjunction((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getNotationTwoArgs this "∨" signatureType LiteralPred

    override this.Run() =
        debug this Debug.Start
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run()
        arg2.Run()
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        // FPL truth-table
        match (arg1Repr, arg2Repr) with
        | (LiteralTrue, _) 
        | (_, LiteralTrue) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralTrue
            this.SetValue newValue
        | (LiteralFalse, LiteralFalse) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralFalse
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()
        debug this Debug.Stop
        
    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// Implements the semantics of an FPL xor compound predicate.
type FplExclusiveOr(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralXor

    override this.Name = PrimExclusiveOr
    override this.ShortName = LiteralXor

    override this.Clone () =
        let ret = new FplExclusiveOr((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getNotationTwoArgs this "⩡" signatureType LiteralPred

    override this.Run() = 
        debug this Debug.Start
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run()
        arg2.Run()
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        // FPL truth-table
        match (arg1Repr, arg2Repr) with
        | (LiteralTrue, LiteralFalse) 
        | (LiteralFalse, LiteralTrue) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralTrue
            this.SetValue newValue
        | (LiteralTrue, LiteralTrue) 
        | (LiteralFalse, LiteralFalse) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralFalse
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()

        debug this Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this


    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this


/// Implements the semantics of an FPL negation compound predicate.
type FplNegation(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralNot

    override this.Name = PrimNegation
    override this.ShortName = LiteralNot

    override this.Clone () =
        let ret = new FplNegation((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType =
        match signatureType with
        | SignatureType.Type -> LiteralPred
        | _ ->
            let argRepr =
                if this.ArgList.Count>0 then
                    let arg = this.ArgList[0]
                    if isSimpleExpression arg then
                        $"{arg.Type signatureType}"
                    else
                        $"({arg.Type signatureType})"
                else
                    $"({PrimUndetermined})"
            $"¬{argRepr}"

    override this.Run() =
        debug this Debug.Start
        let arg = this.ArgList[0]
        arg.Run()
        let argRepr = arg.Represent()
        match argRepr with 
        // FPL truth-table
        | LiteralFalse -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralTrue
            this.SetValue newValue
        | LiteralTrue -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralFalse
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()

        debug this Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency()
        let arg = this.ArgList[0]
        checkArgPred this arg
        checkFreeVar arg
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// Implements the semantics of an FPL implication compound predicate.
type FplImplication(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralImpl

    override this.Name = PrimImplication
    override this.ShortName = LiteralImpl

    override this.Clone () =
        let ret = new FplImplication((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getNotationTwoArgs this "⇒" signatureType LiteralPred

    override this.Run() = 
        debug this Debug.Start
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        match (arg1Repr, arg2Repr) with
        // FPL truth-table
        | (LiteralTrue, LiteralFalse) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralFalse
            this.SetValue newValue
        | (LiteralFalse, LiteralTrue) 
        | (LiteralFalse, LiteralFalse) 
        | (LiteralTrue, LiteralTrue) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralTrue
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()
        
        debug this Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this

/// Implements the semantics of an FPL equivalence compound predicate.
type FplEquivalence(positions: Positions, parent: FplGenericNode) as this =
    inherit FplGenericPredicate(positions, parent)

    do 
        this.FplId <- LiteralIif

    override this.Name = PrimEquivalence
    override this.ShortName = LiteralIif

    override this.Clone () =
        let ret = new FplEquivalence((this.StartPos, this.EndPos), this.Parent.Value)
        this.AssignParts(ret)
        ret

    override this.Type signatureType = getNotationTwoArgs this "⇔" signatureType LiteralPred

    override this.Run() = 
        debug this Debug.Start
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        arg1.Run()
        arg2.Run()
        let arg1Repr = arg1.Represent()
        let arg2Repr = arg2.Represent()
        match (arg1Repr, arg2Repr) with
        // FPL truth-table
        | (LiteralTrue, LiteralTrue) 
        | (LiteralFalse, LiteralFalse) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralTrue
            this.SetValue newValue
        | (LiteralFalse, LiteralTrue) 
        | (LiteralTrue, LiteralFalse) -> 
            let newValue =  new FplIntrinsicPred((this.StartPos, this.EndPos), this)
            newValue.FplId <- LiteralFalse
            this.SetValue newValue
        | _ -> 
            this.SetDefaultValue()

        debug this Debug.Stop

    override this.CheckConsistency() = 
        base.CheckConsistency() 
        let arg1 = this.ArgList[0]
        let arg2 = this.ArgList[1]
        checkArgPred this arg1
        checkArgPred this arg2
        checkFreeVar arg1
        checkFreeVar arg2
        checkCleanedUpFormula this

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        addExpressionToParentArgList this
