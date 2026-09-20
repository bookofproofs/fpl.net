namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Predicate
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestPredicate () =

    [<DataRow(LiteralTrue, 1)>]
    [<DataRow(LiteralFalse, 1)>]
    [<DataRow(LiteralUndef, 1)>]
    [<DataRow(LiteralUndefL, 1)>]
    [<DataRow(LiteralNot, 2)>]
    [<DataRow(LiteralXor, 2)>]
    [<DataRow(LiteralIif, 2)>]
    [<DataRow(LiteralImpl, 2)>]
    [<DataRow(LiteralAnd, 2)>]
    [<DataRow(LiteralOr, 2)>]
    [<DataRow("(", 1)>]
    [<TestMethod>]
    member this.TestAddPredicateChoicesNumber(choice:string, expected:int) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPredicate()).GetChoices(detailCi)
        Assert.AreEqual<int>(expected, actual.Count)


    [<DataRow(LiteralTrue)>]
    [<DataRow(LiteralFalse)>]
    [<DataRow(LiteralUndef)>]
    [<DataRow(LiteralUndefL)>]
    [<DataRow(LiteralNot)>]
    [<DataRow(LiteralXor)>]
    [<DataRow(LiteralIif)>]
    [<DataRow(LiteralImpl)>]
    [<DataRow(LiteralAnd)>]
    [<DataRow(LiteralOr)>]
    [<DataRow("(")>]
    [<TestMethod>]
    member this.TestAddPredicateKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPredicate()).GetChoices(detailCi)
        let mutable count = 0
        let mutable expected = 1
        actual
        |>Seq.iter (fun item ->
            if choice = "(" then
                expected <- 0;
        
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(expected, count)


    [<DataRow(LiteralTrue, CompletionItemKind.Keyword, "zzztrue")>]
    [<DataRow(LiteralFalse, CompletionItemKind.Keyword, "zzzfalse")>]
    [<DataRow(LiteralUndefL, CompletionItemKind.Keyword, "zzzundefined01")>]
    [<DataRow(LiteralUndef, CompletionItemKind.Keyword, "zzzundefined02")>]
    [<DataRow(LiteralNot, CompletionItemKind.Operator, LiteralNot)>]
    [<DataRow(LiteralNot, CompletionItemKind.Keyword, "zzznot")>]
    [<DataRow(LiteralXor, CompletionItemKind.Operator, LiteralXor)>]
    [<DataRow(LiteralXor, CompletionItemKind.Keyword, "zzzxor")>]
    [<DataRow(LiteralIif, CompletionItemKind.Operator, LiteralIif)>]
    [<DataRow(LiteralIif, CompletionItemKind.Keyword, "zzziif")>]
    [<DataRow(LiteralImpl, CompletionItemKind.Operator, LiteralImpl)>]
    [<DataRow(LiteralImpl, CompletionItemKind.Keyword, "zzzimpl")>]
    [<DataRow(LiteralAnd, CompletionItemKind.Operator, LiteralAnd)>]
    [<DataRow(LiteralAnd, CompletionItemKind.Keyword, "zzzand")>]
    [<DataRow(LiteralOr, CompletionItemKind.Operator, LiteralOr)>]
    [<DataRow(LiteralOr, CompletionItemKind.Keyword, "zzzor")>]
    [<DataRow("(", CompletionItemKind.Operator, "(")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, isKeyword:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPredicate()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Label.Contains("_ " + choice) && item.Kind = isKeyword then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralTrue)>]
    [<DataRow(LiteralFalse)>]
    [<DataRow(LiteralUndef)>]
    [<DataRow(LiteralUndefL)>]
    [<DataRow(LiteralNot)>]
    [<DataRow(LiteralXor)>]
    [<DataRow(LiteralIif)>]
    [<DataRow(LiteralImpl)>]
    [<DataRow(LiteralAnd)>]
    [<DataRow(LiteralOr)>]
    [<DataRow("(")>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPredicate()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
        
                Assert.IsTrue(item.InsertText.EndsWith(' '))
        )
    


    [<DataRow(LiteralTrue)>]
    [<DataRow(LiteralFalse)>]
    [<DataRow(LiteralUndef)>]
    [<DataRow(LiteralUndefL)>]
    [<DataRow(LiteralNot)>]
    [<DataRow(LiteralXor)>]
    [<DataRow(LiteralIif)>]
    [<DataRow(LiteralImpl)>]
    [<DataRow(LiteralAnd)>]
    [<DataRow(LiteralOr)>]
    [<DataRow("(")>]
    [<TestMethod>]
    member this.TestAddPredicateChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPredicate()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralTrue, "predicate (true)")>]
    [<DataRow(LiteralFalse, "predicate (false)")>]
    [<DataRow(LiteralUndef, "undefined (short form)")>]
    [<DataRow(LiteralUndefL, LiteralUndefL)>]
    [<DataRow(LiteralNot, "predicate (negation)")>]
    [<DataRow(LiteralXor, "predicate (exclusive or)")>]
    [<DataRow(LiteralIif, "predicate (equivalence, <=>)")>]
    [<DataRow(LiteralImpl, "predicate (implication, =>)")>]
    [<DataRow(LiteralAnd, "predicate (conjunction)")>]
    [<DataRow(LiteralOr, "predicate (disjunction)")>]
    [<DataRow("(", PrimDelegateEqual)>]
    [<TestMethod>]
    member this.TestAddPredicateChoicesDetail(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPredicate()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind <> CompletionItemKind.Keyword then
        
                Assert.AreEqual<string>(l, item.Detail)
        )
    


    [<DataRow(LiteralTrue)>]
    [<DataRow(LiteralFalse)>]
    [<DataRow(LiteralUndef)>]
    [<DataRow(LiteralUndefL)>]
    [<DataRow(LiteralNot)>]
    [<DataRow(LiteralXor)>]
    [<DataRow(LiteralIif)>]
    [<DataRow(LiteralImpl)>]
    [<DataRow(LiteralAnd)>]
    [<DataRow(LiteralOr)>]
    [<DataRow("(")>]
    [<TestMethod>]
    member this.TestAddPredicateChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPredicate()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser PrimPredicate item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

