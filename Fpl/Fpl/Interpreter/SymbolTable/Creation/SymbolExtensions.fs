(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

/// <summary>
/// Provides specialized evaluators for AST nodes related to symbol extensions of the FPL
/// language. Evaluators update interpreter state on the shared <c>heap</c>, set expression
/// and auxiliary information on evaluation frames, and create extension blocks when needed.
/// </summary>
/// <remarks>
/// This module handles parser-produced AST nodes that declare or define symbol-related
/// constructs such as prefix/postfix/infix operators, precedences, and definition extensions.
/// The primary side effects are performed on <c>heap.Eval</c> (evaluation stack frames)
/// and <c>heap.Helper</c> (helper flags and run-order allocation).
/// </remarks>
module Fpl.Interpreter.SymbolTable.Creation.SymbolExtensions
open Fpl.Parser.Types
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Creation.Forward

/// <summary>
/// Evaluate an AST node that pertains to symbol extension declarations or definitions.
/// </summary>
/// <param name="ast">AST node to evaluate. Expected node shapes include:
/// <c>Ast.SymbolDecl</c>, <c>Ast.PrefixDecl</c>, <c>Ast.PostfixDecl</c>,
/// <c>Ast.InfixDeclWithPrecedence</c>, <c>Ast.Precedence</c>,
/// <c>Ast.DefinitionExtension</c>, <c>Ast.ExtensionSignature</c>,
/// <c>Ast.ExtensionRegex</c>, and <c>Ast.ExtensionAssignment</c>.</param>
/// <returns>Unit. The function performs side effects on interpreter state (heap and evaluation frames).</returns>
/// <remarks>
/// Behavior by AST shape:
/// - <c>Ast.SymbolDecl</c>, <c>Ast.PrefixDecl</c>, <c>Ast.PostfixDecl</c>:
///   Set the <c>ExpressionType</c> of the current evaluation frame to the corresponding <c>FixType</c>.
/// - <c>Ast.InfixDeclWithPrecedence</c>:
///   Evaluates the precedence AST child(s), then stores the symbol and evaluated auxiliary
///   precedence information as <c>FixType.Infix</c> on the current frame.
/// - <c>Ast.Precedence</c>:
///   Stores precedence information in the current frame's <c>AuxiliaryInfo</c>.
/// - <c>Ast.DefinitionExtension</c>:
///   Creates a new <c>FplExtension</c> block with the current parent frame, pushes it onto the
///   eval stack, evaluates name, signature and term children, and then pops the block.
///   The block receives a run-order id from <c>heap.Helper.GetNextAvailableFplBlockRunOrder</c>.
/// - <c>Ast.ExtensionSignature</c>, <c>Ast.ExtensionAssignment</c>:
///   Evaluate signature and assignment children. During <c>ExtensionAssignment</c> evaluation,
///   <c>heap.Helper.InSignatureEvaluation</c> is toggled to true to signal signature-context evaluation.
/// - <c>Ast.ExtensionRegex</c>:
///   Stores the regex string into the current frame's <c>TypeId</c>.
/// </remarks>
/// <exception cref="System.Exception">
/// Raised via <c>failwith</c> when <paramref name="ast"/> is not recognized as a node relevant
/// to symbol extensions.
/// </exception>
let evalExtendSymbols ast =
    match ast with
    | Ast.SymbolDecl((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Symbol symbol
    | Ast.PrefixDecl((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Prefix symbol
    | Ast.PostfixDecl((pos1, pos2), symbol) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.ExpressionType <- FixType.Postfix symbol
    | Ast.InfixDeclWithPrecedence((pos1, pos2), (symbol, precedenceAsts)) -> 
        let fv = heap.Eval.PeekEvalStack()
        evalRef.Value precedenceAsts
        fv.ExpressionType <- FixType.Infix (symbol, fv.AuxiliaryInfo)
    | Ast.Precedence((pos1, pos2), precedence) ->
        let fv = heap.Eval.PeekEvalStack()
        fv.AuxiliaryInfo <- precedence
    // Definitions of extensions
    | Ast.DefinitionExtension((pos1, pos2), ((extensionNameAst, extensionSignatureAst), extensionTermAst)) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplExtension((pos1,pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        heap.Eval.PushEvalStack(fv)
        evalRef.Value extensionNameAst
        evalRef.Value extensionSignatureAst
        evalRef.Value extensionTermAst
        heap.Eval.PopEvalStack()
    | Ast.ExtensionSignature((pos1, pos2), (extensionAssignmentAst, extensionMappingAst)) ->
        evalRef.Value extensionAssignmentAst
        evalRef.Value extensionMappingAst
    | Ast.ExtensionRegex s -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.TypeId <- s
    | Ast.ExtensionAssignment((pos1, pos2), (varAst, extensionRegexAst)) ->
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value varAst
        evalRef.Value extensionRegexAst
        heap.Helper.InSignatureEvaluation <- false
    | _ ->
        failwith (sprintf "{%O} is not a node needed for symbol extensions" ast)
