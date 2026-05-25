/// This module contains all classes used by the FplInterpreter
/// to model predicative nodes in the symbol table, like axioms, and theorem-like statments

(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)
module FplInterpreterPredicativeBlocks
open FParsec
open FplPrimitives
open FplGrammarTypes
open FplInterpreterDiagnosticsEmitter
open FplInterpreterBasicTypes
open FplInterpreterChecks
open FplInterpreter.Globals.Debug
open FplInterpreter.Globals.HelpersBasic
open FplInterpreter.Globals.Heap
open FplInterpreter.Globals.HelpersComplex
open FplInterpreterIntrinsicTypes
open FplInterpreterVariables

let runArgumentsOfGenericPredicateWithExpression (fv:FplGenericHasValue) = 
    fv.ArgList
    |> Seq.iter (fun fv1 -> 
        fv1.Run()
        match fv1 with 
        | :? FplGenericHasValue as fv1WithValue -> 
            fv.SetValueOf fv1WithValue
        | _ ->
            // warning stmt inside predicate might cause side-effects
            fv.ErrorOccurred <- emitLG004diagnostic fv.Name fv1.StartPos fv1.EndPos
            
    )

[<AbstractClass>]
type FplGenericPredicateWithExpression(positions: Positions, parent: FplGenericNode) =
    inherit FplGenericPredicate(positions, parent)
    let mutable _signStartPos = Position("", 0L, 0L, 0L)
    let mutable _signEndPos = Position("", 0L, 0L, 0L)

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

    override this.Type signatureType = getFplHead this signatureType

    override this.CheckConsistency () = 
        base.CheckConsistency()
        if not this.IsIntrinsic then
            checkVAR04Diagnostics this
        checkPredicateExpressionReturnsPredicate this

type FplAxiom(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false

    interface IReady with
        member _.IsReady = _isReady

    override this.Name = LiteralAxL
    override this.ShortName = LiteralAx

    member this.InferrableExpression =
        let validityReason = 
            let exprOpt = this.ArgList |> Seq.tryLast
            match exprOpt with
            | Some expr -> ValidityReason.IsAxiom (expr.Type SignatureType.Name)
            | _ -> ValidityReason.Error // fallback if axiom node is empty

        {
            ValidStatement.Node = this
            ValidStatement.ValidityReason = validityReason
        }

    interface IInferrable with
        member this.InferrableExpression
            with get () = this.InferrableExpression


    override this.Clone () =
        let ret = new FplAxiom((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this

    override this.Run() = 
        debug this Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this 
            _isReady <- true
            checkLG003Diagnostics this

            // register the axiom in the valid statement store
            if heap.ValidStmtStore.RegisterExpression this then
                // evaluate all corollaries of the axiom
                this.Scope.Values
                |> Seq.filter (fun fv -> fv.Name = LiteralCorL) 
                |> Seq.sortBy (fun block -> block.RunOrder.Value) 
                |> Seq.iter (fun fv -> 
                    fv.Run()
                )

        debug this Debug.Stop

    override this.RunOrder = Some _runOrder

[<AbstractClass>]
type FplGenericTheoremLikeStmt(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false
    let mutable _hasProof = false

    override this.Name = LiteralThmL
    override this.ShortName = LiteralThm

    interface IReady with
        member _.IsReady = _isReady

    interface IHasProof with
        member this.HasProof
            with get (): bool = _hasProof
            and set (value) = _hasProof <- value

    member this.InferrableExpression =
        let validityReason = 
            let exprOpt = this.ArgList |> Seq.tryLast
            match exprOpt with
            | Some expr -> ValidityReason.IsDerived (expr.Type SignatureType.Name)
            | _ -> ValidityReason.Error // fallback if theorem-like statement node is empty

        {
            ValidStatement.Node = this
            ValidStatement.ValidityReason = validityReason
        }

    interface IInferrable with
        member this.InferrableExpression
            with get () = this.InferrableExpression

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.Run() = 
        debug this Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this 
            _isReady <- true
            checkLG003Diagnostics this

            // register the theorem-like stmtmt in the valid statement store
            if heap.ValidStmtStore.RegisterExpression this then
            // evaluate all corollaries and proofs of the theorem-like statement
                this.Scope.Values
                |> Seq.filter (fun fv -> fv.Name = LiteralPrfL || fv.Name = LiteralCorL) 
                |> Seq.sortBy (fun block -> block.RunOrder.Value) 
                |> Seq.iter (fun fv -> 
                    fv.Run()
                )

        if not _hasProof then 
           this.ErrorOccurred <- emitPR007Diagnostics (this.Type(SignatureType.Name)) this.Name this.SignStartPos this.SignEndPos
        debug this Debug.Stop

    override this.RunOrder = Some _runOrder

type FplTheorem(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralThmL
    override this.ShortName = LiteralThm

    override this.Clone () =
        let ret = new FplTheorem((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplLemma(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralLemL
    override this.ShortName = LiteralLem

    override this.Clone () =
        let ret = new FplLemma((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplProposition(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralPropL
    override this.ShortName = LiteralProp

    override this.Clone () =
        let ret = new FplProposition((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

type FplCorollary(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericTheoremLikeStmt(positions, parent, runOrder)

    override this.Name = LiteralCorL
    override this.ShortName = LiteralCor

    override this.Clone () =
        let ret = new FplCorollary((this.StartPos, this.EndPos), this.Parent.Value, this.RunOrder.Value)
        this.AssignParts(ret)
        ret

    override this.EmbedInSymbolTable _ =
        match tryFindAssociatedBlockForCorollary this with
        | ScopeSearchResult.FoundAssociate potentialParent -> 
            // everything is OK, change the parent of the provable from theory to the found parent 
            this.Parent <- Some potentialParent
        | ScopeSearchResult.FoundIncorrectBlock incorrectBlock ->
            this.ErrorOccurred <- emitID005diagnostics this.FplId (qualifiedName incorrectBlock false) this.StartPos this.EndPos
        | ScopeSearchResult.NotFound ->
            this.ErrorOccurred <- emitID006diagnostics this.FplId this.SignStartPos this.SignEndPos
        | _ -> ()
        tryAddToParentUsingFplId this

type FplConjecture(positions: Positions, parent: FplGenericNode, runOrder) =
    inherit FplGenericPredicateWithExpression(positions, parent)
    let _runOrder = runOrder
    let mutable _isReady = false

    interface IReady with
        member _.IsReady = _isReady

    override this.Name = LiteralConjL
    override this.ShortName = LiteralConj

    override this.Clone () =
        let ret = new FplConjecture((this.StartPos, this.EndPos), this.Parent.Value, _runOrder)
        this.AssignParts(ret)
        ret

    override this.IsFplBlock () = true
    override this.IsBlock () = true

    override this.EmbedInSymbolTable _ = 
        this.CheckConsistency()
        tryAddToParentUsingFplId this 

    override this.Run() = 
        debug this Debug.Start
        if not _isReady then
            runArgumentsOfGenericPredicateWithExpression this 
            _isReady <- true
            checkLG003Diagnostics this

            // evaluate all corollaries of the conjecture
            this.Scope.Values
            |> Seq.filter (fun fv -> fv.Name = LiteralCorL) 
            |> Seq.sortBy (fun block -> block.RunOrder.Value) 
            |> Seq.iter (fun fv -> 
                fv.Run()
            )
        debug this Debug.Stop


    override this.RunOrder = Some _runOrder

(*
    TODO: 1) implement a function ToPL0Form transforming a predicative expression into a PL0 formula by replacing predicates with free pred variables
             possible applications: see 1a) 
    TODO: 1a) implement a function ToTrueTable generating a true table out of ToPL0Form
             possible applications: see 2), 2a) 3) 4)
    TODO: 2) implement a satisfiability check to the Output of ToTrueTable
             possible applications: 
                issue error, if a formula of a theorem / axiom / conjecture is not satisfiable
                issue warning, if a sub formula is not satisfiable to replace it by false
    TODO: 2a) implement a tautology check to the output of ToTrueTable
             possible applications: 
                issue warning, if a formula of a theorem / axiom / conjecture is a tautology, because it could be replaced by a trivial true
                issue warning, if a sub formula is a tautology to replace it by true
    TODO: 3) implement a CanonicalDNF (disjunctive normal form) based on ToTrueTable with a sorted representation.
             possible applications:
                issue error, if in a proof there are two consecutive arguments aprev, anext whose outputs have the same ToTrueTables 
                    in terms of variables (its columns) that are not equivalent (have different rows)
    TODO: 4) implement unit tests for all inference rules defined in Fpl.Commons checking if the respective premises and conclusions produce the same outputs of ToTrueTable.
             In this case, it is ensured that each inference rule in this library is a tautology. This is a required for 
             FPL to use inference rules as a Hilbert Calculus (see definition D.Hoffmann "Theoretische Informatik" 3rd. ed., p. 98)
             respectively inference rules with a premise being a predicate list: Here it is sufficient to check, if each rule 
             conserves the tautology property: If each predicate in a list is a tautology, so is the conclusion (see D.Hoffmann, "Theoretische Informatik", p. 104)
    TODO: 5) ensure cleaned-up expressions by renaming variable with the same names in independent parts of the same formula expression.
             (see D.H. "Theoretische Informatik", 3rd. p. 119) 
             Implementation idea: This can be accomplished by moving the scope of variables declared in quantors to the containing FPL block, forcing renaming the variables by the end-user at coding-time of the formula. 
    TODO: 6) issue error if arity-0 predicates are intrinsically defined, enforcing true or false (see D.H. "Theoretische Informatik", 3rd. p. 120) 
    TODO: 7) write functions for normalizing predicative formulas (see D.H. "Th. Inf", 3rd. p. 122-123):
                NormalizeNeg - (uses cleaned-up expressions - see 5) replace impl, iif, xor, by and/or/not and move all negations from the beginning of non-atomic formula to its atomic sub formulas 
                NormalizePrenex - (uses the output of NormalizeNeg): move quantors from all sub formulas the most outer quantor using fixed rules (see figure 3.35, p. 122)
                NormalizeSkolem - (uses the output of NormalizePrenex): eliminated all exists-quantors from the formula
                    there are two use cases: 
                        exists quantor is not proceeded by all quantors: - then just remove the ex quantor by replacing x <- u() with some intrinsic 0-ary function u()->tpl {intr} (some constant u fulfilling p)
                            for instance, 
                                ex x:tpl { p(x) } 
                            will be transformed to 
                                p(u())
                        exists quantor is proceeded by some all quantors: then remove the ex quantor by replacing x<-g_p(x1,x2) with some intrinsic n-ary function g(tpl1,tpl2)->tpl {intr} (some function fulfilling p, depending on proceeding variables bound by all quantors) 
                            for instance, 
                                all x1:tpl1, x2:tpl2, x:tpl {p(x1,x2,x)} 
                            will be transformed to  
                                all x1:tpl1, x2:tpl2 {p(x1,x2, g(x1, x2))} 
    TODO: 8) write unit-test checking if FplValue.Type(SignatureType.Type) of expressions like p(u()) or all x1:tpl1, x2:tpl2 {p(x1,x2, g(x1, x2))} 
        includes full signatures of the functions u() and g(,), .i.e., including their mappings. This will later become necessary 
        to be able to recognize the satisfiability-equivalence of two NormalizeSkolem outputs (see 7)
        For the term "satisfiability-equivalence" see D.H. "Th. Inf", 3rd. p. 124
*)
