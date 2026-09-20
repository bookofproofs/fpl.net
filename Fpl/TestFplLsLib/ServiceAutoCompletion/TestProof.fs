namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Proof
open Fpl0Base.Primitives
open Fpl.Parser.Main

[<TestClass>]
type TestProof () =

    [<DataRow(LiteralPrf)>]
    [<DataRow(LiteralPrfL)>]
    [<TestMethod>]
    member this.TestAddProofChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesProof())).GetChoices(detailCi)
        Assert.AreEqual<int>(15, actual.Count)


    [<DataRow(LiteralPrf)>]
    [<DataRow(LiteralPrfL)>]
    [<TestMethod>]
    member this.TestAddProofKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesProof())).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralPrfL, CompletionItemKind.Property, "proof01")>]
    [<DataRow(LiteralPrf, CompletionItemKind.Property, "proof02")>]
    [<DataRow(LiteralPrfL, CompletionItemKind.Keyword, "zzzproof01")>]
    [<DataRow(LiteralPrf, CompletionItemKind.Keyword, "zzzzproof02")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesProof())).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            if item.Label.Contains(choice) && item.Kind = kind then
                Assert.AreEqual<string>(expected, item.SortText)
        )
    
        
    


    [<DataRow(LiteralPrfL)>]
    [<DataRow(LiteralPrf)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesProof())).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )
    


    [<DataRow(LiteralPrf)>]
    [<DataRow(LiteralPrfL)>]
    [<TestMethod>]
    member this.TestAddProofChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesProof())).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralPrf, LiteralPrfL)>]
    [<DataRow(LiteralPrfL, LiteralPrfL)>]
    [<TestMethod>]
    member this.TestAddProofChoicesDetail(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesProof())).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind <> CompletionItemKind.Keyword then
        
                Assert.IsTrue(not (String.IsNullOrEmpty(item.InsertText)) && not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(l))
                if item.IsShort then
                    Assert.IsTrue(item.Detail.Contains("(short)"))
        )    
        
    


    [<DataRow(LiteralPrf)>]
    [<DataRow(LiteralPrfL)>]
    [<TestMethod>]
    member this.TestAddProofChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesProof())).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralPrf item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

