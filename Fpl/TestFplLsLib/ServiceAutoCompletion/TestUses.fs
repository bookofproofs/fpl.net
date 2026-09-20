namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Uses
open Fpl.Primitives

[<TestClass>]
type TestUses () =

    [<DataRow(LiteralUses)>]
    [<TestMethod>]
    member this.TestAddUsesChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesUses()).GetChoices(detailCi)
        Assert.AreEqual<int>(4, actual.Count)


    [<DataRow(LiteralUses)>]
    [<TestMethod>]
    member this.TestAddUsesKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesUses()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralUses, CompletionItemKind.Property, "uses01")>]
    [<DataRow(LiteralUses, CompletionItemKind.Property, "uses02")>]
    [<DataRow(LiteralUses, CompletionItemKind.Keyword, "uses03")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesUses()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralUses)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesUses()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )
    


    [<DataRow(LiteralUses)>]
    [<TestMethod>]
    member this.TestAddUsesChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesUses()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralUses)>]

    [<TestMethod>]
    member this.TestAddUsesChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesUses()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Detail.Contains(choice))
        )


    [<DataRow(LiteralUses)>]

    [<TestMethod>]
    member this.TestAddUsesChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesUses()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
        )
        Assert.AreEqual<int>(actual.Count, counterSnippets)

