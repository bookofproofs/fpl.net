namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.TheoremLikeStmt
open Fpl0Base.Primitives
open Fpl.Parser.Main

[<TestClass>]
type TestTheoremLikeStmt () =

    [<DataRow(LiteralInf, "Inference")>]
    [<DataRow(LiteralInfL, "Inference")>]
    [<DataRow(LiteralThm, "Theorem")>]
    [<DataRow(LiteralThmL, "Theorem")>]
    [<DataRow(LiteralLem, "Lemma")>]
    [<DataRow(LiteralLemL, "Lemma")>]
    [<DataRow(LiteralConj, "Conjecture")>]
    [<DataRow(LiteralConjL, "Conjecture")>]
    [<DataRow(LiteralProp, "Proposition")>]
    [<DataRow(LiteralPropL, "Proposition")>]
    [<TestMethod>]
    member this.TestAddChoicesNumber(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesTheoremLikeStmt(l)).GetChoices(detailCi)

        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralInf, "Inference")>]
    [<DataRow(LiteralInfL, "Inference")>]
    [<DataRow(LiteralThm, "Theorem")>]
    [<DataRow(LiteralThmL, "Theorem")>]
    [<DataRow(LiteralLem, "Lemma")>]
    [<DataRow(LiteralLemL, "Lemma")>]
    [<DataRow(LiteralConj, "Conjecture")>]
    [<DataRow(LiteralConjL, "Conjecture")>]
    [<DataRow(LiteralProp, "Proposition")>]
    [<DataRow(LiteralPropL, "Proposition")>]
    [<TestMethod>]
    member this.TestAddKeywordCounts(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesTheoremLikeStmt(l)).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralInfL, "Inference", CompletionItemKind.Class, "inference01")>]
    [<DataRow(LiteralInf, "Inference", CompletionItemKind.Class, "inference02")>]
    [<DataRow(LiteralInfL, "Inference", CompletionItemKind.Keyword, "zzzinference01")>]
    [<DataRow(LiteralInf, "Inference", CompletionItemKind.Keyword, "zzzzinference02")>]
    [<DataRow(LiteralThmL, "Theorem", CompletionItemKind.Class, "theorem01")>]
    [<DataRow(LiteralThm, "Theorem", CompletionItemKind.Class, "theorem02")>]
    [<DataRow(LiteralThmL, "Theorem", CompletionItemKind.Keyword, "zzztheorem01")>]
    [<DataRow(LiteralThm, "Theorem", CompletionItemKind.Keyword, "zzzztheorem02")>]
    [<DataRow(LiteralLemL, "Lemma", CompletionItemKind.Class, "lemma01")>]
    [<DataRow(LiteralLem, "Lemma", CompletionItemKind.Class, "lemma02")>]
    [<DataRow(LiteralLemL, "Lemma", CompletionItemKind.Keyword, "zzzlemma01")>]
    [<DataRow(LiteralLem, "Lemma", CompletionItemKind.Keyword, "zzzzlemma02")>]
    [<DataRow(LiteralConjL, "Conjecture", CompletionItemKind.Class, "conjecture01")>]
    [<DataRow(LiteralConj, "Conjecture", CompletionItemKind.Class, "conjecture02")>]
    [<DataRow(LiteralConjL, "Conjecture", CompletionItemKind.Keyword, "zzzconjecture01")>]
    [<DataRow(LiteralConj, "Conjecture", CompletionItemKind.Keyword, "zzzzconjecture02")>]
    [<DataRow(LiteralPropL, "Proposition", CompletionItemKind.Class, "proposition01")>]
    [<DataRow(LiteralProp, "Proposition", CompletionItemKind.Class, "proposition02")>]
    [<DataRow(LiteralPropL, "Proposition", CompletionItemKind.Keyword, "zzzproposition01")>]
    [<DataRow(LiteralProp, "Proposition", CompletionItemKind.Keyword, "zzzzproposition02")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, l:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesTheoremLikeStmt(l)).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralInf, "Inference")>]
    [<DataRow(LiteralInfL, "Inference")>]
    [<DataRow(LiteralThm, "Theorem")>]
    [<DataRow(LiteralThmL, "Theorem")>]
    [<DataRow(LiteralLem, "Lemma")>]
    [<DataRow(LiteralLemL, "Lemma")>]
    [<DataRow(LiteralConj, "Conjecture")>]
    [<DataRow(LiteralConjL, "Conjecture")>]
    [<DataRow(LiteralProp, "Proposition")>]
    [<DataRow(LiteralPropL, "Proposition")>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesTheoremLikeStmt(l)).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )
    


    [<DataRow(LiteralInf, "Inference")>]
    [<DataRow(LiteralInfL, "Inference")>]
    [<DataRow(LiteralThm, "Theorem")>]
    [<DataRow(LiteralThmL, "Theorem")>]
    [<DataRow(LiteralLem, "Lemma")>]
    [<DataRow(LiteralLemL, "Lemma")>]
    [<DataRow(LiteralConj, "Conjecture")>]
    [<DataRow(LiteralConjL, "Conjecture")>]
    [<DataRow(LiteralProp, "Proposition")>]
    [<DataRow(LiteralPropL, "Proposition")>]
    [<TestMethod>]
    member this.TestAddChoicesLabel(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesTheoremLikeStmt(l)).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralInf, "Inference")>]
    [<DataRow(LiteralInfL, "Inference")>]
    [<DataRow(LiteralThm, "Theorem")>]
    [<DataRow(LiteralThmL, "Theorem")>]
    [<DataRow(LiteralLem, "Lemma")>]
    [<DataRow(LiteralLemL, "Lemma")>]
    [<DataRow(LiteralConj, "Conjecture")>]
    [<DataRow(LiteralConjL, "Conjecture")>]
    [<DataRow(LiteralProp, "Proposition")>]
    [<DataRow(LiteralPropL, "Proposition")>]
    [<TestMethod>]
    member this.TestAddChoicesDetail(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesTheoremLikeStmt(l)).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then
        
                Assert.IsTrue(not (String.IsNullOrEmpty(item.InsertText)) && not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(choice))
        
            else
        
                Assert.IsTrue(not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(l, StringComparison.CurrentCultureIgnoreCase))
        )
    


    [<DataRow(LiteralInf, "Inference")>]
    [<DataRow(LiteralInfL, "Inference")>]
    [<DataRow(LiteralThm, "Theorem")>]
    [<DataRow(LiteralThmL, "Theorem")>]
    [<DataRow(LiteralLem, "Lemma")>]
    [<DataRow(LiteralLemL, "Lemma")>]
    [<DataRow(LiteralConj, "Conjecture")>]
    [<DataRow(LiteralConjL, "Conjecture")>]
    [<DataRow(LiteralProp, "Proposition")>]
    [<DataRow(LiteralPropL, "Proposition")>]
    [<TestMethod>]
    member this.TestAddChoicesInsertText(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesTheoremLikeStmt(l)).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1 
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains('{') then
        
                let res = testParser PrimTheoremLike item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

