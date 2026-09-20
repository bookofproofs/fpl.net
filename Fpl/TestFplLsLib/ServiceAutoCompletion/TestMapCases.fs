namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.MapCases
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestMapCases () =

    [<DataRow(LiteralMapCases)>]
    [<TestMethod>]
    member this.TestAddMapCasesChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesMapCases()).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralMapCases)>]
    [<TestMethod>]
    member this.TestAddMapCasesKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesMapCases()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralMapCases)>]
    [<TestMethod>]
    member this.TestAddMapChoicesSortText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesMapCases()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.SortText.Contains(LiteralCases))
        )


    [<DataRow(LiteralMapCases)>]
    [<TestMethod>]
    member this.TestAddMapCasesChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesMapCases()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralMapCases)>]
    [<TestMethod>]
    member this.TestAddMapCasesChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesMapCases()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Detail.Contains(choice))
        )


    [<DataRow(LiteralMapCases)>]
    [<TestMethod>]
    member this.TestAddMapCasesChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesMapCases()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralMapCases item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

