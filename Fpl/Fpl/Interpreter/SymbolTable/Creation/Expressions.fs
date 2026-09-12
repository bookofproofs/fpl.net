(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Evaluators and helpers for AST nodes that represent expressions (infix, prefix/postfix, parentheses,
/// qualified predicates and more).
/// </summary>
/// <remarks>
/// The module provides private helpers to inspect operator precedence, to transform flat operand/operator
/// sequences into a binary op tree ordered by precedence, and the public entry point <c>evalExpressions</c>
/// which interprets expression AST nodes by mutating the interpreter evaluation stack (<c>heap.Eval</c>).
/// Diagnostics (SY010..SY014, SIG04 etc.) are emitted where parentheses or operator usage can be improved or are invalid.
/// </remarks>
module Fpl.Interpreter.SymbolTable.Creation.Expressions
open System
open Fpl.Primitives
open Fpl.Errors.Emitter
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.Helpers.Basic
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Storage.Util
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.Types3.DefinitionProperties
open Fpl.Interpreter.SymbolTable.TypeMatching
open Fpl.Interpreter.SymbolTable.Creation.Forward

/// <summary>
/// Return the infix symbol and its precedence for the supplied node, if available.
/// </summary>
/// <param name="fv1">The node to inspect. The function checks both the node itself and any referenced definition.</param>
/// <returns>
/// Tuple of (<c>symbol</c>, <c>precedence</c>) when node represents an infix operator; otherwise <c>("", Int32.MinValue)</c>.
/// </returns>
/// <remarks>
/// This helper centralizes precedence lookup: it first prefers <c>RefersTo</c> target's expression type, then the node's own type.
/// </remarks>
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

/// <summary>
/// If the supplied node has an argument and that argument is an infix operation, return the inner infix symbol and precedence.
/// </summary>
/// <param name="node">The node whose first argument (if any) will be inspected.</param>
/// <returns>
/// <c>Some (symbol, precedence)</c> when the first argument is an infix operation; otherwise <c>None</c>.
/// </returns>
/// <remarks>
/// Used to detect redundant parentheses when an inner infix with higher precedence is parenthesized.
/// </remarks>
let private getArgumentsSymbolWithPrecedence (node:FplGenericNode) =
    match (node.ArgList |> Seq.tryHead) with
    | Some arg ->
        let (infixSymbol, precedence) = getSymbolWithPrecedence arg
        if infixSymbol = String.Empty then
            None
        else
            Some (infixSymbol, precedence)
    | _ -> None

/// <summary>
/// Issue diagnostics if a parenthesized operand contains an infix with strictly higher precedence than an outer operator.
/// </summary>
/// <param name="operand">The operand to inspect (possibly a parenthesized infix).</param>
/// <param name="outerInfixOperator">The outer infix operator node.</param>
/// <returns>Unit. Emits SY013 diagnostics when applicable.</returns>
/// <remarks>
/// This improves feedback about unnecessary parentheses: when inner precedence > outer precedence,
/// the inner parentheses can be omitted safely and a suggestion diagnostic is emitted.
/// </remarks>
let private checkSY013ForOperand (operand:FplGenericNode) (outerInfixOperator:FplGenericNode) =
    match outerInfixOperator.ExpressionType with
    | FixType.Infix(outerInfixSymbol, outerPrecedence) ->
        match operand.ExpressionType with
        | FixType.Paren ->
            match getArgumentsSymbolWithPrecedence(operand) with 
            | Some (innerInfixSymbol, innerPrecedence) when innerPrecedence > outerPrecedence ->
                operand.ErrorOccurred <- emitSY013Diagnostics innerInfixSymbol innerPrecedence outerInfixSymbol outerPrecedence operand.StartPos operand.EndPos
            | _ -> ()
        | _ -> ()
    | _ -> ()

/// <summary>
/// Detect conflicting infix symbols sharing the same precedence and emit a diagnostic for the operand where conflict occurs.
/// </summary>
/// <param name="operatorIndices">List of (index, (symbol, precedence)) pairs for operator positions in the operand/operator list.</param>
/// <param name="operandOperatorList">Original flat operand/operator list used to locate the offending operand node.</param>
/// <returns>Unit. Emits SY014 diagnostics when two different symbols share the same precedence in a single expression.</returns>
/// <remarks>
/// The helper finds the first precedence group containing at least two different symbols and emits a single diagnostic
/// to avoid overwhelming the user with redundant messages.
/// </remarks>
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
        operand.ErrorOccurred <- emitSY014Diagnostics firstSymbol secondSymbol precedence operand.StartPos operand.EndPos
    | None -> ()

/// <summary>
/// Reduce a flat list of alternating operands and operators into a binary operation tree ordered by operator precedence.
/// </summary>
/// <param name="operandOperatorList">A list of <c>FplGenericNode</c> representing an alternating sequence:
/// [operand; operator; operand; operator; operand; ...].</param>
/// <returns>
/// A (smaller) list where operator nodes have their <c>ArgList</c> populated with their left and right operands.
/// The result typically collapses to a single node representing the full infix expression.
/// </returns>
/// <remarks>
/// The algorithm:
/// - collects operator precedences,
/// - issues SY014 diagnostics for conflicting precedence definitions,
/// - selects the operator with maximal precedence, attaches its left and right operands,
/// - repeats until the list is fully reduced.
/// Side effects: operator nodes receive their argument children via mutation of <c>ArgList</c>.
/// </remarks>
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

/// <summary>
/// Evaluate each (operand, optional-operator) pair in an infix expression and push the corresponding
/// operand and operator reference nodes into the parent's argument list.
/// </summary>
/// <param name="operandOperatorOptList">List of tuples (<c>operandAst</c>, <c>opAstOpt</c>) describing the infix expression elements.</param>
/// <param name="fv">The parent <c>FplGenericNode</c> that will receive built operand/operator nodes in its <c>ArgList</c>.</param>
/// <param name="pos1">Start position used to create temporary reference nodes.</param>
/// <param name="pos2">End position used to create temporary reference nodes.</param>
/// <returns>Unit. The function mutates the global evaluation stack and appends created nodes to <c>fv.ArgList</c>.</returns>
/// <remarks>
/// For each operand the function:
/// - creates a temporary <c>FplReference</c>, pushes it, evaluates the operand AST via <c>evalRef.Value</c>,
///   simplifies trivial nesting and pops the resulting node into <c>fv.ArgList</c>.
/// - if an operator AST exists, evaluates it into another temporary reference and appends it as well.
/// </remarks>
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


/// <summary>
/// Evaluate an expression AST node and apply its semantics to the interpreter evaluation stack.
/// </summary>
/// <param name="ast">AST node expected to represent an expression (prefix/postfix operators, parentheses, infix, qualified predicates, etc.).</param>
/// <returns>Unit. The function mutates <c>heap.Eval</c> and constructs appropriate <c>FplGenericNode</c> instances.</returns>
/// <remarks>
/// The implementation covers:
/// - prefix/postfix operator evaluation and operand handling,
/// - parentheses handling including SY010 diagnostics for redundant parens,
/// - infix expression flattening, precedence-based reduction and simplification,
/// - predicate qualification and signature-based candidate resolution for referenced identifiers.
/// The function delegates nested AST evaluation to the global evaluator via <c>evalRef.Value</c>.
/// </remarks>
/// <exception cref="System.Exception">
/// Thrown via <c>failwith</c> when <paramref name="ast"/> is not a recognized expression node.
/// </exception>
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
        simplifyTriviallyNestedExpressions operand
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
            refBlock.ErrorOccurred <- emitSY010Diagnostics pos1 pos2
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
                fv.ErrorOccurred <- checkID025Diagnostics (qualifiedNameSimple candidate) node.Name fv.StartPos fv.EndPos
            elif candidates.Length > 0 then
                // not a class was referred, add the candidate (e.g., referenced variable)
                let candidate = candidates.Head
                fv.FplId <- candidate.FplId 
                fv.RefersTo <- Some candidate
                fv.ErrorOccurred <- checkID025Diagnostics (qualifiedNameSimple candidate) node.Name fv.StartPos fv.EndPos
            else
                ()
        simplifyTriviallyNestedExpressions fv
    | _ ->
        failwith (sprintf "{%O} is not an expression node" ast) 
