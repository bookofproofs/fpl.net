namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Self
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl0Base.Primitives

[<TestClass>]
type TestSelf () =

    [<DataRow(LiteralSelf)>]
    [<DataRow(LiteralBase)>]
    [<DataRow(LiteralParent)>]
    [<TestMethod>]
    member this.TestAddSelfChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesSelf()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow(LiteralSelf)>]
    [<DataRow(LiteralBase)>]
    [<DataRow(LiteralParent)>]
    [<TestMethod>]
    member this.TestAddSelfReferenceCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesSelf()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Reference then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralSelf, CompletionItemKind.Reference, "self01")>]
    [<DataRow(LiteralParent, CompletionItemKind.Reference, "parent02")>]
    [<DataRow(LiteralBase, CompletionItemKind.Reference, "self03")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesSelf()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralSelf)>]
    [<DataRow(LiteralBase)>]
    [<DataRow(LiteralParent)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithSpace(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesSelf()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice)) && choice<>LiteralBase then
        
                Assert.IsTrue(item.InsertText.EndsWith(' '))
        )
    


    [<DataRow(LiteralSelf)>]
    [<DataRow(LiteralBase)>]
    [<DataRow(LiteralParent)>]
    [<TestMethod>]
    member this.TestAddSelfChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesSelf()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralSelf, "self reference")>]
    [<DataRow(LiteralBase, "ctor call (parent class)")>]
    [<DataRow(LiteralParent, "parent self reference")>]
    [<TestMethod>]
    member this.TestAddSelfChoicesDetail(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesSelf()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l, item.Detail)
        )


    [<DataRow(LiteralSelf)>]
    [<DataRow(LiteralBase)>]
    [<DataRow(LiteralParent)>]
    [<TestMethod>]
    member this.TestAddSelfChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesSelf()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
        )
        Assert.AreEqual<int>(actual.Count, counterSnippets)

