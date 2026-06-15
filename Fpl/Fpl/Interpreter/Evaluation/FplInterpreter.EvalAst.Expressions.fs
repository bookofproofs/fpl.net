/// This module provides specialized evaluators for the AST nodes related to FPL expressions.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module FplInterpreter.EvalAst.Expressions
open System
open Fpl.Primitives
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open FplInterpreterDefinitionProperties
open FplInterpreterFplTypeMatching
open FplInterpreter.EvalAst.Forward
open Fpl.Errors.Emitter

/// Simplify trivially nested expressions by removing from the stack FplValue nodes that were created due to too long parsing tree and replacing them by their sub nodes 
let rec private simplifyTriviallyNestedExpressions (rb1:FplGenericNode) = 
    match rb1 with 
    | :? FplReference as rb when rb.ArgList.Count = 1 && rb.FplId = "" && not rb.ExpressionType.IsParen ->
        // removable reference blocks are those with only a single argument and unset FplId 
        let subNode = rb.ArgList[0] 
        heap.Eval.Pop() |> ignore // pop the removable reference block and ignored it
        heap.Eval.PushEvalStack(subNode) // push its subNode instead
        // adjust subNode's Parent, EndPos, Scope
        subNode.Parent <- rb.Parent 
        subNode.EndPos <- rb.EndPos
        // prevent recursive loops
        rb.ArgList.Clear() 
        rb.Value <- None
        rb.Scope.Clear()
        simplifyTriviallyNestedExpressions subNode
    | _ -> ()


/// Returns the precedence of fv1 if its ExpressionType is Infix
/// or Int32.MinValue otherwise
let private getSymbolWithPrecedence (fv1:FplGenericNode) =
    match fv1.RefersTo with
    | None ->
        match fv1.ExpressionType with
        | FixType.Infix (symb, prec) -> (symb, prec)
        | _ -> ("", Int32.MinValue)
    | Some x ->
        match x.ExpressionType with
        | FixType.Infix (symb, prec) -> (symb, prec)
        | _ -> ("", Int32.MinValue)

/// If node has an argument, and it is an infix operation, the function will return the argument's symbol with precedence
/// and None otherwise
let private getArgumentsSymbolWithPrecedence (node:FplGenericNode) =
    match (node.ArgList |> Seq.tryHead) with
    | Some arg ->
        let (infixSymbol, precedence) = getSymbolWithPrecedence arg
        if infixSymbol = String.Empty then
            None
        else
            Some (infixSymbol, precedence)
    | _ -> None

/// Checks if the operand of some outerInfixOperator is itself an parenthesized infix operator and, if so, compares the precedences
/// of both infix operations with each other. If the inner precedence is higher than the outer,
/// a diagnostics will be issued that the parentheses of the inner infix operation can be omitted.
let private checkSY013ForOperand (operand:FplGenericNode) (outerInfixOperator:FplGenericNode) =
    match outerInfixOperator.ExpressionType with
    | FixType.Infix(outerInfixSymbol, outerPrecedence) ->
        match operand.ExpressionType with
        | FixType.Paren ->
            match getArgumentsSymbolWithPrecedence(operand) with 
            | Some (innerInfixSymbol, innerPrecedence) when innerPrecedence > outerPrecedence ->
                operand.ErrorOccurred <- emitSY013diagnostics innerInfixSymbol innerPrecedence outerInfixSymbol outerPrecedence operand.StartPos operand.EndPos
            | _ -> ()
        | _ -> ()
    | _ -> ()

/// Checks if two infix operations with the same precedence are involved and issues a diagnostic.
let private checkSY014ForOperand (operatorIndices:(int * (string * int)) list) (operandOperatorList: FplGenericNode list) =
    /// detect two different symbols that share the same precedence
    let tryPickConflictingSymbols =
        operatorIndices
        |> List.groupBy (fun (_, (_, prec)) -> prec)
        |> List.tryPick (fun (prec, group) ->
            match group with
            | (_, (s1, _)) :: (i2, (s2, _)) :: _ when s1 <> s2 ->
                // i2 is the index of the second symbol with conflicting precedence
                // we need only one index to reduce the number of
                // issued SY014 diagnostics to the significant ones
                Some (i2, s1, s2, prec) 
            | _ ->
                None)
    match tryPickConflictingSymbols with
    | Some (i, firstSymbol, secondSymbol, precedence) ->
        let operand = operandOperatorList[i]
        operand.ErrorOccurred <- emitSY014diagnostics firstSymbol secondSymbol precedence operand.StartPos operand.EndPos
    | None -> ()

// This function will transform a list of [operand; op; operand; ...; op ; operand] 
// by grouping them into binary operations op(operand, operand)
// and sort all ops according to their precedence, starting with the highest and ending with the lowest
let rec private reduce (operandOperatorList: FplGenericNode list) =
    match operandOperatorList with
    | []
    | [_] -> operandOperatorList // items with zero or one 
    | _ ->
        // operators are stored at odd indices (1,3,...). collect (index, precedence)
        let operatorIndices =
            operandOperatorList
            |> List.mapi (fun i it -> (i, getSymbolWithPrecedence it))
            |> List.filter (fun (i, _) -> i % 2 = 1)
        checkSY014ForOperand operatorIndices operandOperatorList

        // pick the operator with maximal precedence
        let (maxIdx, _) = operatorIndices |> List.maxBy (fun (_, (_, prec)) -> prec)

        let left = operandOperatorList.[maxIdx - 1]
        let op = operandOperatorList.[maxIdx]
        let right = operandOperatorList.[maxIdx + 1]

        // mutate the operator node to attach its operand children (this is domain-necessary;
        // the control flow itself is immutable/functional)
        op.ArgList.Add(left)
        op.ArgList.Add(right)
        checkSY013ForOperand left op
        checkSY013ForOperand right op
        checkSIG04DiagnosticsForInfixOperator op left right

        let before = operandOperatorList |> List.take (maxIdx - 1)
        let after = operandOperatorList |> List.skip (maxIdx + 2)
        let newItems = before @ [op] @ after
        reduce newItems

let private gatherOperandOperatorListFromInfixExpression operandOperatorOptList (fv:FplGenericNode) pos1 pos2 =
    operandOperatorOptList
    |> List.map (fun (predAst, opAstOpt) ->
        // operand
        let operand = new FplReference((pos1,pos2), fv)
        heap.Eval.PushEvalStack(operand)
        evalRef.Value predAst
        simplifyTriviallyNestedExpressions operand
        fv.ArgList.Add(heap.Eval.Pop()) // pop the stack element (same reference as pred) and store it in a list

        match opAstOpt with
        | Some opAst ->
            // followed by the operator
            // evaluate the operator by trying to find a definition for the operator
            let infixOperator = new FplReference((pos1,pos2), fv)
            heap.Eval.PushEvalStack(infixOperator)
            evalRef.Value opAst
            // store the index of the infix operator, so we still know it after sorting the list by precedence later
            fv.ArgList.Add(heap.Eval.Pop()) // pop the stack element (same reference as infixOperator) and store it in a list
        | None -> ()
    )
    |> ignore


let evalExpressions ast =
    match ast with
    | Ast.PrefixOp(operatorAst, operandAst) 
    | Ast.PostfixOp(operatorAst, operandAst) -> 
        let fv = heap.Eval.PeekEvalStack()
        let operator = new FplReference((fv.StartPos, fv.EndPos), fv) 
        heap.Eval.PushEvalStack(operator)
        evalRef.Value operatorAst
        let operand = new FplReference((operator.StartPos, operator.EndPos), operator) 
        heap.Eval.PushEvalStack(operand)
        evalRef.Value operandAst
        heap.Eval.PopEvalStack()
        heap.Eval.PopEvalStack()
        simplifyTriviallyNestedExpressions fv
    | Ast.Parens ((pos1, pos2), expressionAst) ->
        let fv = heap.Eval.PeekEvalStack()
        let refBlock = new FplReference ((pos1,pos2), fv)
        refBlock.ExpressionType <- FixType.Paren
        heap.Eval.PushEvalStack(refBlock)
        match expressionAst with
        | Ast.InfixOp((_,_), listInsideInfix) when listInsideInfix.Length = 1 ->
            // if an InfixOp is inside the Parens and it contains only one element
            // (i.e. the infix Operation is actually not an infix operation having at least two operands but a single one).
            // then flag that the parentheses can be safely removed.
            refBlock.ErrorOccurred <- emitSY010diagnostics pos1 pos2
        | _ -> ()
        evalRef.Value expressionAst
        heap.Eval.PopEvalStack()
        // parens in parens 
        refBlock.ArgList |> Seq.iter checkSY010
    | Ast.InfixOp ((pos1, pos2), operandOperatorOptList) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplReference((pos1, pos2), parent)
        heap.Eval.PushEvalStack(fv)
        gatherOperandOperatorListFromInfixExpression operandOperatorOptList fv pos1 pos2

        // run reduction on a snapshot list, then replace fv.ArgList with the reduced result
        let snapshot = fv.ArgList |> Seq.toList
        let reducedTree = reduce snapshot // the tree has the binary op with the lowest precedence at the root
        // replace contents of fv.ArgList with reduced result (typically a single binary op with the lowest precedence)
        fv.ArgList.Clear()
        reducedTree |> List.iter fv.ArgList.Add
        simplifyTriviallyNestedExpressions fv
        heap.Eval.PopEvalStack()
        match parent with
        | :? FplReference ->
            simplifyTriviallyNestedExpressions parent
        | _ when parent.IsBlock() ->
            // parens top level in { block }
            parent.ArgList |> Seq.iter checkSY010
        | _ when isCompoundPredicate parent  ->
            // parens in compound predicate top level
            parent.ArgList |> Seq.iter checkSY010
        | _ -> ()
    | Ast.PredicateWithQualification(predicateWithOptSpecificationAst, qualificationListAst) ->
        evalRef.Value predicateWithOptSpecificationAst
        evalRef.Value qualificationListAst
    | Ast.PredicateWithOptSpecification((pos1, pos2), (fplIdentifierAst, optionalSpecificationAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let searchForCandidatesOfReferenceBlock (refBlock:FplGenericNode) = 
            let candidatesFromTheory = findCandidatesByName refBlock.FplId true false
            let candidatesFromPropertyScope = findPropertyCandidatesByNameInBlock refBlock refBlock.FplId
            let candidatesFromDottedQualification = findCandidatesByNameInDotted refBlock refBlock.FplId
            candidatesFromTheory  
            @ candidatesFromPropertyScope 
            @ candidatesFromDottedQualification

        /// parentFv is a dotted reference 
        let getCandidatesBasedOnDottedParent (parentFv: FplGenericNode) = 
            let referencedNodeOpt, typeRefNode, typeNameRefNode =
                match parentFv.RefersTo with 
                | Some parentFvRefersTo ->
                    match parentFvRefersTo with 
                    | :? FplFunctionalTerm 
                    | :? FplPredicate 
                    | :? FplClass -> Some parentFvRefersTo, parentFvRefersTo.Type SignatureType.Mixed, parentFvRefersTo.Name
                    | _ -> 
                        let refNodeOpt = parentFvRefersTo.RefersTo
                        match refNodeOpt with 
                        | Some refNode -> refNodeOpt, refNode.Type SignatureType.Mixed, refNode.Name
                        | None -> None, $"{parentFv.FplId}:{LiteralUndef}", parentFv.Name
                | None ->
                    None, $"{parentFv.FplId}:{LiteralUndef}", parentFv.Name
            let candidatesPre = 
                match referencedNodeOpt with 
                | Some referencedNode ->
                    referencedNode.GetVariables() @ referencedNode.GetProperties() 
                | _ -> []
            match box parentFv with
            | :? IHasDotted as pDotted when pDotted.DottedChild.IsSome -> 
                let dottedChild = pDotted.DottedChild.Value
                typeRefNode, typeNameRefNode, filterCandidates candidatesPre dottedChild.FplId false
            | _ -> typeRefNode, typeNameRefNode, ([], "") // empty candidates list and name

        let parentFv = fv.Parent.Value
        match optionalSpecificationAst, box parentFv with
        | Some specificationAst, (:? IHasDotted as pDotted) when pDotted.DottedChild.IsSome -> 
            evalRef.Value fplIdentifierAst
            evalRef.Value specificationAst |> ignore
            let typeRefNode, typeNameRefNode, (candidates, candidatesNames) = getCandidatesBasedOnDottedParent parentFv 
            if candidates.Length = 0 then 
                fv.ErrorOccurred <- emitID012Diagnostics (fv.Type SignatureType.Mixed) typeNameRefNode typeRefNode candidatesNames pos1 pos2
            else
                match checkSIG04Diagnostics fv candidates with
                | Some matchedCandidate -> fv.RefersTo <- Some matchedCandidate
                | _ -> ()

        | Some specificationAst, _ -> 
            let node = new FplReference((pos1, pos2), fv) 
            heap.Eval.PushEvalStack(node)
            evalRef.Value fplIdentifierAst
            evalRef.Value specificationAst |> ignore
            
            let candidates = 
                if checkStartsWithLowerCase node.FplId then
                    // match the signatures of small-letter entities (like the self or parent entity, or variables with arguments) 
                    // with their declared types 
                    match node.RefersTo with
                    | Some ref ->
                        match ref.Name, ref.RefersTo with
                        // the candidate from FplSelf is the block it points to (if any)
                        | LiteralSelf, Some fplBlock -> [fplBlock]
                        | LiteralSelf, None -> []
                        // the candidate from FplParent is the block it points to (if any)
                        | LiteralParent, Some fplBlock -> [fplBlock]
                        | LiteralParent, None -> []
                        // the candidate from FplVariable is the block it points to (if any)
                        | PrimVariableL, Some fplBlock -> [fplBlock]
                        | _, _ -> [ref]
                    | None -> []
                else
                    searchForCandidatesOfReferenceBlock node
            if candidates.Length = 1 && candidates.Head.Name = PrimVariableArrayL then
                let candidate = candidates.Head
                node.RefersTo <- Some candidate 
                checkSIG08_SIG10Diagnostics node
            else
                match checkSIG04Diagnostics node candidates with
                | Some matchedCandidate -> 
                    match node.RefersTo with
                    | Some self when self.Name = LiteralSelf && self.RefersTo.IsSome && Object.ReferenceEquals(self.RefersTo.Value, matchedCandidate) ->
                        () // omit replacing node.RefersTo if it refers to FplSelf and FplSelf already refers to the matchedCandidate
                    | Some parent when parent.Name = LiteralParent && parent.RefersTo.IsSome && Object.ReferenceEquals(parent.RefersTo.Value, matchedCandidate) ->
                        () // omit replacing node.RefersTo if it refers to FplParent and FplParent already refers to the matchedCandidate
                    | _ ->
                        node.RefersTo <- Some matchedCandidate
                | _ -> ()

            heap.Eval.PopEvalStack()
        | None, (:? IHasDotted as pDotted) when pDotted.DottedChild.IsSome -> 
            evalRef.Value fplIdentifierAst
            let typeRefNode, typeNameRefNode, (candidates, candidatesNames) = getCandidatesBasedOnDottedParent parentFv
            if candidates.Length = 0 then 
                fv.ErrorOccurred <- emitID012Diagnostics (fv.Type SignatureType.Mixed) typeNameRefNode typeRefNode candidatesNames pos1 pos2
            else
                fv.RefersTo <- Some candidates.Head 
        | None, _ -> 
            // if no specification was found then simply continue in the same context
            evalRef.Value fplIdentifierAst
            let node = fv.UltimateBlockNode.Value
            // make sure, we still add a referenced node candidate to the scope of a reference
            let candidates = searchForCandidatesOfReferenceBlock fv
            let classes = candidates |> List.filter (fun c -> c.Name = PrimClassL)
            let constructors = candidates |> List.filter (fun c -> c.Name = LiteralCtorL) 
            if constructors.Length > 0 then
                // if among the candidates are class constructors (that due to the FPL syntax always have a signature with 0 or more parameters)
                // we check if to issue a SIG04 diagnostic. At this AST case, a class was referred with a PascalCaseIdentifier 
                // without parentheses. This will only be accepted by the interpreter (without SIG04), if there is
                // a parameterless constructor. In other words, referring a class without parentheses is only allowed
                // if the class is intrinsic (has no constructors) or has a parameterless constructor.
                match checkSIG04Diagnostics fv constructors with
                | Some matchedCandidate -> 
                    // add a parameterless constructor (if such exists)
                     fv.RefersTo <- Some matchedCandidate 
                | _ -> ()
            elif classes.Length > 0 && constructors.Length = 0 then
                // add the class (intrinsic case, no constructors at all)
                let candidate = classes.Head
                fv.RefersTo <- Some candidate
                fv.ErrorOccurred <- checkID025Diagnostics (qualifiedName candidate false) node.Name fv.StartPos fv.EndPos
            elif candidates.Length > 0 then
                // not a class was referred, add the candidate (e.g., referenced variable)
                let candidate = candidates.Head
                fv.FplId <- candidate.FplId 
                fv.RefersTo <- Some candidate
                fv.ErrorOccurred <- checkID025Diagnostics (qualifiedName candidate false) node.Name fv.StartPos fv.EndPos
            else
                ()
        simplifyTriviallyNestedExpressions fv
    | _ ->
        failwith (sprintf "{%O} is not an expression node" ast) 
