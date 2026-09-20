namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Extension
open Fpl.Primitives
open Fpl.Parser.Main

[<TestClass>]
type TestExtension () =

    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralExt)>]
    [<TestMethod>]
    member this.TestAddExtensionChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesExtension()).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralExt)>]
    [<TestMethod>]
    member this.TestAddExtensionKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesExtension()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralExtL, CompletionItemKind.Property, "extension01")>]
    [<DataRow(LiteralExt, CompletionItemKind.Property, "extension02")>]
    [<DataRow(LiteralExtL, CompletionItemKind.Keyword, "zzzextension01")>]
    [<DataRow(LiteralExt, CompletionItemKind.Keyword, "zzzzextension02")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesExtension()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Label.Contains(choice) && item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralExt)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesExtension()).GetChoices(detailCi)
        actual |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
        
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        
        )


    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralExt)>]
    [<TestMethod>]
    member this.TestAddExtensionChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesExtension()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralExt)>]
    [<TestMethod>]
    member this.TestAddExtensionChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesExtension()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Detail.Contains(choice))
        )


    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralExt)>]
    [<TestMethod>]
    member this.TestAddExtensionChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesExtension()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralExt item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

