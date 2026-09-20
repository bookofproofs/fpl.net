namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.IsOperator
open Fpl0Base.Primitives
open Fpl.Parser.Main

[<TestClass>]
type TestIsOperator () =

    [<DataRow(LiteralIs)>]
    [<TestMethod>]
    member this.TestAddIsOperatorChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesIsOperator())).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralIs)>]
    [<TestMethod>]
    member this.TestAddIsOperatorKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesIsOperator())).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralIs, CompletionItemKind.Property, LiteralIs)>]
    [<DataRow(LiteralIs, CompletionItemKind.Keyword, "zzzis")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesIsOperator())).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Label.Contains(choice) && item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralIs)>]
    [<TestMethod>]
    member this.TestAddIsOperatorChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesIsOperator())).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralIs)>]
    [<TestMethod>]
    member this.TestAddIsOperatorChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesIsOperator()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(not (String.IsNullOrEmpty(item.InsertText)) && not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(choice))
        )


    [<DataRow(LiteralIs)>]
    [<TestMethod>]
    member this.TestAddIsOperatorChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesIsOperator()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1 
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralIs item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)


