namespace TestFpl3LanguageServer.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.For
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestFor () =

    [<DataRow(LiteralFor)>]
    [<TestMethod>]
    member this.TestAddForChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesFor()).GetChoices(detailCi)
        Assert.AreEqual<int>(4, actual.Count)


    [<DataRow(LiteralFor)>]
    [<TestMethod>]
    member this.TestAddForKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesFor()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralFor, "range", CompletionItemKind.Property, "for01")>]
    [<DataRow(LiteralFor, "list", CompletionItemKind.Property, "for02")>]
    [<DataRow(LiteralFor, "", CompletionItemKind.Keyword, "zzzfor03")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, l:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesFor()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Label.Contains(choice) && not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(l) && item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralFor)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesFor()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind <> CompletionItemKind.Keyword && not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )
    


    [<DataRow(LiteralFor)>]
    [<TestMethod>]
    member this.TestAddForChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesFor()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralFor)>]
    [<TestMethod>]
    member this.TestAddForChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesFor()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
            Assert.IsTrue(item.Detail.Contains(choice))
        )


    [<DataRow(LiteralFor)>]
    [<TestMethod>]
    member this.TestAddForChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesFor()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralFor item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

