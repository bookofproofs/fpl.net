namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Regex
open Fpl0Base.Primitives

[<TestClass>]
type TestRegex () =

    [<DataRow("extension regex")>]
    [<TestMethod>]
    member this.TestAddRegexChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesRegex()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow("extension regex")>]
    [<TestMethod>]
    member this.TestAddRegexTextCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesRegex()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Text then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow("extension regex", CompletionItemKind.Value, "word")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesRegex()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow("extension regex")>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithSpace(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesRegex()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice)) && not (choice.EndsWith('!')) then
        
                Assert.IsTrue(item.InsertText.EndsWith(" "))
        )
    


    [<DataRow("extension regex", "some regex")>]
    [<TestMethod>]
    member this.TestAddRegexChoicesLabel(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesRegex()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(l) && item.Label.StartsWith("_ "))
        )


    [<DataRow("extension regex")>]
    [<TestMethod>]
    member this.TestAddRegexChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesRegex()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
            Assert.IsTrue(not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(choice))
        )


    [<DataRow("extension regex", "/+\\d/ ")>]
    [<TestMethod>]
    member this.TestAddRegexChoicesInsertText(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesRegex()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l, item.InsertText)
        )

