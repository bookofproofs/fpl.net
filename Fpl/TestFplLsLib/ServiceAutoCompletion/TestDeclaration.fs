namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Declaration
open Fpl.Primitives
open Fpl.Parser.Main

[<TestClass>]
type TestDeclaration () =

    [<DataRow(LiteralDec)>]
    [<DataRow(LiteralDecL)>]
    [<TestMethod>]
    member this.TestAddDeclarationChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDeclaration()).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralDec)>]
    [<DataRow(LiteralDecL)>]
    [<TestMethod>]
    member this.TestAddDeclarationKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDeclaration()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |> Seq.iter (fun item ->
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )    
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralDecL, CompletionItemKind.Property, "declaration01")>]
    [<DataRow(LiteralDec, CompletionItemKind.Property, "declaration02")>]
    [<DataRow(LiteralDecL, CompletionItemKind.Keyword, "zzzdeclaration01")>]
    [<DataRow(LiteralDec, CompletionItemKind.Keyword, "zzzzdeclaration02")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDeclaration()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            if item.Kind = kind then
                Assert.AreEqual<string>(expected, item.SortText)
        )        
        
    [<DataRow(LiteralDecL)>]
    [<DataRow(LiteralDec)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDeclaration()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )        
    


    [<DataRow(LiteralDec)>]
    [<DataRow(LiteralDecL)>]
    [<TestMethod>]
    member this.TestAddDeclarationChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDeclaration()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )    


    [<DataRow(LiteralDec)>]
    [<DataRow(LiteralDecL)>]
    [<TestMethod>]
    member this.TestAddDeclarationChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDeclaration()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            Assert.IsTrue(item.Detail.Contains(choice))
        )    


    [<DataRow(LiteralDec)>]
    [<DataRow(LiteralDecL)>]
    [<TestMethod>]
    member this.TestAddDeclarationChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDeclaration()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1

            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
                let res = testParser LiteralDec item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )            
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

