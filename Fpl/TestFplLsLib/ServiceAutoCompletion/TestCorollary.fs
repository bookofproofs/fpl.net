namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Corollary
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestCorollary () =

    [<DataRow(LiteralCorL)>]
    [<DataRow(LiteralCor)>]
    [<TestMethod>]
    member this.TestAddCorollaryChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesCorollary()).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralCorL)>]
    [<DataRow(LiteralCor)>]
    [<TestMethod>]
    member this.TestAddCorollaryKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesCorollary()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |> Seq.iter (fun item ->
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )    
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralCorL, CompletionItemKind.Property, "corollary01")>]
    [<DataRow(LiteralCor, CompletionItemKind.Property, "corollary02")>]
    [<DataRow(LiteralCorL, CompletionItemKind.Keyword, "zzzcorollary01")>]
    [<DataRow(LiteralCor, CompletionItemKind.Keyword, "zzzzcorollary02")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesCorollary()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            if item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )        
    


    [<DataRow(LiteralCorL)>]
    [<DataRow(LiteralCor)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesCorollary()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
        
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )        
    


    [<DataRow(LiteralCorL)>]
    [<DataRow(LiteralCor)>]
    [<TestMethod>]
    member this.TestAddCorollaryChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesCorollary()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )    


    [<DataRow(LiteralCorL)>]
    [<DataRow(LiteralCor)>]
    [<TestMethod>]
    member this.TestAddCorollaryChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesCorollary()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(not (String.IsNullOrEmpty(item.InsertText)) && not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(choice))
        )    


    [<DataRow(LiteralCorL)>]
    [<DataRow(LiteralCor)>]
    [<TestMethod>]
    member this.TestAddCorollaryChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesCorollary()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralCor item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )            
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

