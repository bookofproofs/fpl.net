namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Delegate
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestDelegate () =

    [<DataRow(LiteralDel)>]
    [<DataRow(LiteralDelL)>]
    [<TestMethod>]
    member this.TestAddDelegateChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDelegate()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow(LiteralDel)>]
    [<DataRow(LiteralDelL)>]
    [<TestMethod>]
    member this.TestAddDelegateEventCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDelegate()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |> Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Event then count <- count + 1
        )    
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralDelL, CompletionItemKind.Property, "delegate01")>]
    [<DataRow(LiteralDel, CompletionItemKind.Property, "delegate02")>]
    [<DataRow(LiteralDelL, CompletionItemKind.Keyword, "zzzdelegate01")>]
    [<DataRow(LiteralDel, CompletionItemKind.Keyword, "zzzzdelegate02")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDelegate()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            if item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )        
    


    [<DataRow(LiteralDel)>]
    [<DataRow(LiteralDelL)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithSpace(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDelegate()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
        
                Assert.IsTrue(item.InsertText.EndsWith(' '))
        )        
    


    [<DataRow(LiteralDel)>]
    [<DataRow(LiteralDelL)>]
    [<TestMethod>]
    member this.TestAddDelegateChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDelegate()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )    


    [<DataRow(LiteralDel)>]
    [<DataRow(LiteralDelL)>]
    [<TestMethod>]
    member this.TestAddDelegateChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDelegate()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Detail.Contains(choice))
        )    


    [<DataRow(LiteralDel)>]
    [<DataRow(LiteralDelL)>]
    [<TestMethod>]
    member this.TestAddDelegateChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDelegate()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->

            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralDel item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)


