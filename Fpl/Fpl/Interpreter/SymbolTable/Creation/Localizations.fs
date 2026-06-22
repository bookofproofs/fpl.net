/// This module provides specialized evaluators for the AST nodes related to FPL localizations of mathematical language.


(* MIT License

Copyright (c) 2024+ bookofproofs

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. 

*)

module Fpl.Interpreter.SymbolTable.Creation.Localizations
open System
open System.Collections.Generic
open Fpl.Parser.Types
open Fpl.Errors.Emitter
open Fpl.Interpreter.Helpers.Checks
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types3.Localization
open Fpl.Interpreter.SymbolTable.Creation.Forward

let private chooseRandomMember (lst: Ast list) =
    let rnd = Random()
    let index = rnd.Next(lst.Length)
    lst.[index]

let evalLocalizations ast =
    match ast with
    | Ast.Localization(((pos1, pos2), predicateAst), translationListAsts) ->
        let parent = heap.Eval.PeekEvalStack()
        let fv = new FplLocalization((pos1, pos2), parent, heap.Helper.GetNextAvailableFplBlockRunOrder)
        let var04List = List<KeyValuePair<string, Positions>>()
        heap.Eval.PushEvalStack(fv)
        heap.Helper.InSignatureEvaluation <- true
        evalRef.Value predicateAst
        heap.Helper.InSignatureEvaluation <- false
        translationListAsts |> List.map (fun subAst -> 
            evalRef.Value subAst
            let vars = fv.GetVariables()
            vars
            |> List.map (fun var -> var :?> FplGenericVariable)
            |> List.filter (fun var -> not var.IsUsed)
            |> List.map (fun var ->
                let loc = heap.Eval.PeekEvalStack()
                let languageList = 
                    loc.Scope 
                    |> Seq.filter (fun kvp -> isLanguage kvp.Value) 
                    |> Seq.map (fun kvp -> kvp.Value) 
                    |> Seq.toList 
                    |> List.rev
                if not languageList.IsEmpty then
                    let lan = languageList.Head
                    let kvp = KeyValuePair(var.FplId,(lan.StartPos, lan.EndPos))
                    var04List.Add kvp
            )
        ) |> ignore
        let identifier = fv.ArgList |> Seq.map (fun arg -> arg.FplId) |> String.concat ""
        fv.FplId <- identifier
        fv.TypeId <- identifier
        heap.Eval.PopEvalStack()
        var04List
        |> Seq.iter (fun kvp -> 
            fv.ErrorOccurred <- emitVAR04Diagnostics kvp.Key (fst kvp.Value) (snd kvp.Value)
        )
    | Ast.TranslationTerm((pos1, pos2), asts) ->
        let fv = heap.Eval.PeekEvalStack()
        asts |> List.map (fun ebnfTerm ->
            let trsl = new FplTranslation((pos1, pos2), fv)
            heap.Eval.PushEvalStack(trsl)
            evalRef.Value ebnfTerm
            heap.Eval.PopEvalStack()
        ) |> ignore
    | Ast.TranslationTermList((pos1, pos2), ebnfTermAsts) ->
        evalRef.Value (chooseRandomMember ebnfTermAsts)
    | Ast.Language((pos1, pos2),(langCode, ebnfAst)) ->
        let fv = heap.Eval.PeekEvalStack()
        let lang = new FplLanguage((pos1, pos2), fv) 
        heap.Eval.PushEvalStack(lang)
        evalRef.Value langCode
        evalRef.Value ebnfAst
        heap.Eval.PopEvalStack() // remove language
    | Ast.LanguageCode((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
        fv.StartPos <- pos1
        fv.EndPos <- pos2
    | Ast.LocalizationString((pos1, pos2), s) -> 
        let fv = heap.Eval.PeekEvalStack()
        fv.FplId <- s
        fv.TypeId <- s
    | _ ->
        failwith (sprintf "{%O} is not a localization or a related node" ast) 
