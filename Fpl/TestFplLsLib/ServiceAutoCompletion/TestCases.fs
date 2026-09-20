namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Cases
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestCases () =

    [<DataRow(LiteralCases)>]
    [<TestMethod>]
    member this.TestAddCasesChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesCases())).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralCases)>]
    [<TestMethod>]
    member this.TestAddCasesKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesCases())).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |> Seq.iter (fun item ->
            if item.Kind = CompletionItemKind.Keyword then
                count <- count + 1;
        )
    
    
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralCases)>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesCases())).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.SortText.Contains(LiteralCases))
        )    


    [<DataRow(LiteralCases)>]
    [<TestMethod>]
    member this.TestAddCasesChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesCases())).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )    


    [<DataRow(LiteralCases)>]
    [<TestMethod>]
    member this.TestAddCasesChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesCases())).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Detail.Contains(choice))
        )    

    [<DataRow(LiteralCases)>]
    [<TestMethod>]
    member this.TestAddCasesChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesCases())).GetChoices(detailCi)
        let mutable counterSnippets = 0
        actual
        |> Seq.iter (fun item ->

            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1

            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
                let res = testParser LiteralCases item.InsertText
                if not (res.StartsWith("Success:")) then
                    Assert.Fail(res)

        )

        Assert.AreEqual<int>(actual.Count, counterSnippets)

