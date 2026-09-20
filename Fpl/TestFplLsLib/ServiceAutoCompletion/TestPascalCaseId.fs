namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.PascalCaseId
open Fpl.Primitives
open Fpl.Parser.Main

[<TestClass>]
type TestPascalCaseId () =

    [<DataRow(PrimPascalCaseId)>]
    [<TestMethod>]
    member this.TestAddPascalCaseIdChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPascalCaseId()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow(PrimPascalCaseId)>]
    [<TestMethod>]
    member this.TestAddPascalCaseIdReferenceCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPascalCaseId()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Reference then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(PrimPascalCaseId, CompletionItemKind.Reference, PrimPascalCaseId)>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPascalCaseId()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Label.Contains(choice) && item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(PrimPascalCaseId)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithSpace(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPascalCaseId()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
                Assert.IsTrue(item.InsertText.EndsWith(" "))
        )
    


    [<DataRow(PrimPascalCaseId)>]
    [<TestMethod>]
    member this.TestAddPascalCaseIdChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPascalCaseId()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(PrimPascalCaseId, "user-defined id")>]
    [<TestMethod>]
    member this.TestAddPascalCaseIdChoicesDetail(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPascalCaseId()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l, item.Detail)
        )


    [<DataRow(PrimPascalCaseId)>]
    [<TestMethod>]
    member this.TestAddPascalCaseIdChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesPascalCaseId()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser PrimPascalCaseId item.InsertText
                if not (res.StartsWith("Success:")) then
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

