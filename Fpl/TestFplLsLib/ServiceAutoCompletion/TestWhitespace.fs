namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Whitespace
open Fpl0Base.Primitives

[<TestClass>]
type TestWhitespace () =

    [<DataRow("whitespace")>]
    [<DataRow("significant whitespace")>]
    [<TestMethod>]
    member this.TestAddWhitespaceChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWhitespace()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow("whitespace")>]
    [<DataRow("significant whitespace")>]
    [<TestMethod>]
    member this.TestAddWhitespaceTextCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWhitespace()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |> Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Text then
                count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow("whitespace", CompletionItemKind.Text, "zzzzz")>]
    [<DataRow("significant whitespace", CompletionItemKind.Text, "zzzzz")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWhitespace()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow("whitespace")>]
    [<DataRow("significant whitespace")>]
    [<TestMethod>]
    member this.TestAddWhitespaceChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWhitespace()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains("' '") && item.Label.StartsWith("_ "))
        )


    [<DataRow("whitespace", "(whitespace)")>]
    [<DataRow("significant whitespace", "(whitespace)")>]
    [<TestMethod>]
    member this.TestAddWhitespaceChoicesDetail(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWhitespace()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l, item.Detail)
        )


    [<DataRow("whitespace")>]
    [<DataRow("significant whitespace")>]
    [<TestMethod>]
    member this.TestAddWhitespaceChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWhitespace()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(" ", item.InsertText)
        )

