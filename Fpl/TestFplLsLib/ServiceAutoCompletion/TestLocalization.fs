namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Localization
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestLocalization () =

    [<DataRow(LiteralLocL)>]
    [<DataRow(LiteralLoc)>]
    [<TestMethod>]
    member this.TestAddLocalizationChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesLocalization()).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralLocL)>]
    [<DataRow(LiteralLoc)>]
    [<TestMethod>]
    member this.TestAddLocalizationKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesLocalization()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralLocL, CompletionItemKind.Property, "localization01")>]
    [<DataRow(LiteralLoc, CompletionItemKind.Property, "localization02")>]
    [<DataRow(LiteralLocL, CompletionItemKind.Keyword, "zzzlocalization01")>]
    [<DataRow(LiteralLoc, CompletionItemKind.Keyword, "zzzzlocalization02")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesLocalization()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Label.Contains(choice) && item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralLocL)>]
    [<DataRow(LiteralLoc)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesLocalization()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind <> CompletionItemKind.Keyword && not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )
    


    [<DataRow(LiteralLocL)>]
    [<DataRow(LiteralLoc)>]
    [<TestMethod>]
    member this.TestAddLocalizationChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesLocalization()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralLocL)>]
    [<DataRow(LiteralLoc)>]
    [<TestMethod>]
    member this.TestAddLocalizationChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesLocalization()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Detail.Contains(choice))
        )


    [<DataRow(LiteralLocL)>]
    [<DataRow(LiteralLoc)>]
    [<TestMethod>]
    member this.TestAddLocalizationChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesLocalization()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1

            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralLoc item.InsertText
                if not (res.StartsWith("Success:")) then
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

