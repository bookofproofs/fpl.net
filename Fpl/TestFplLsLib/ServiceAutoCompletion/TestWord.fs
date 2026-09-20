namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Word
open Fpl0Base.Primitives

[<TestClass>]
type TestWord () =

    [<DataRow("word")>]
    [<TestMethod>]
    member this.TestAddWordChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWord()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow("word")>]
    [<TestMethod>]
    member this.TestAddWordValueCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWord()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Value then count <- count + 1
        )
        Assert.AreEqual<int>(actual.Count, count)


    [<DataRow("word", CompletionItemKind.Value, "word")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWord()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow("word")>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithSpace(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWord()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind <> CompletionItemKind.Keyword && not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) && not (choice.EndsWith('!')) then
        
                Assert.IsTrue(item.InsertText.EndsWith(' '))
        )
    



    [<DataRow("word", "someIdentifier")>]
    [<TestMethod>]
    member this.TestAddWordChoicesLabel(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWord()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(l) && item.Label.StartsWith("_ "))
        )


    [<DataRow("word", "regex \\w+")>]
    [<TestMethod>]
    member this.TestAddWordChoicesDetail(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWord()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l, item.Detail)
        )


    [<DataRow("word", "someIdentifier ")>]
    [<TestMethod>]
    member this.TestAddWordChoicesInsertText(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesWord()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l, item.InsertText)
        )

