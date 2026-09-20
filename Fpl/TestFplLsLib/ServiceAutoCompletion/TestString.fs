namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.String
open Fpl0Base.Primitives

[<TestClass>]
type TestString () =

    [<DataRow("language-specific string")>]
    [<TestMethod>]
    member this.TestAddStringChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesString()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow("language-specific string")>]
    [<TestMethod>]
    member this.TestAddStringValueCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesString()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Value then count <- count + 1
        )
        Assert.AreEqual<int>(actual.Count, count)


    [<DataRow("language-specific string", "\"\"")>]
    [<TestMethod>]
    member this.TestAddStringChoicesSortText(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesString()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l, item.SortText)
        )


    [<DataRow("language-specific string", "\"...\"")>]
    [<TestMethod>]
    member this.TestAddStringChoicesLabel(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesString()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(l) && item.Label.StartsWith("_ "))
        )


    [<DataRow("language-specific string")>]
    [<TestMethod>]
    member this.TestAddStringChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesString()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(choice))
        )


    [<DataRow("language-specific string", "\"...\" ")>]
    [<TestMethod>]
    member this.TestAddStringChoicesInsertText(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesString()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l, item.InsertText)
        )

