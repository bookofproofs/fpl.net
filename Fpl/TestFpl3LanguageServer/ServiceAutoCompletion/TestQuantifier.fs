namespace TestFpl3LanguageServer.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Quantifier
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestQuantifier () =


    [<DataRow(LiteralAll, 2)>]
    [<DataRow(LiteralEx, 2)>]
    [<DataRow(LiteralExN, 2)>]
    [<TestMethod>]
    member this.TestAddQuantifierChoicesNumber(choice:string, number:int) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesQuantifier()).GetChoices(detailCi)
        Assert.AreEqual<int>(number, actual.Count)


    [<DataRow(LiteralAll, 1)>]
    [<DataRow(LiteralEx, 1)>]
    [<DataRow(LiteralExN, 1)>]
    [<TestMethod>]
    member this.TestAddQuantifierKeywordCounts(choice:string, number:int) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesQuantifier()).GetChoices(detailCi)

        let mutable count = 0;
        actual
        |> Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(number, count)


    [<DataRow(LiteralAll, "all ...", CompletionItemKind.Operator, "all01")>]
    [<DataRow(LiteralAll, "type ...", CompletionItemKind.Operator, "all02")>]
    [<DataRow(LiteralAll, "list ...", CompletionItemKind.Operator, "all03")>]
    [<DataRow(LiteralAll, "range ...", CompletionItemKind.Operator, "all04")>]
    [<DataRow(LiteralAll, "combined ...", CompletionItemKind.Operator, "all05")>]
    [<DataRow(LiteralAll, "", CompletionItemKind.Keyword, "zzzall")>]
    [<DataRow(LiteralEx, "ex ...", CompletionItemKind.Operator, "ex01")>]
    [<DataRow(LiteralEx, "type ...", CompletionItemKind.Operator, "ex02")>]
    [<DataRow(LiteralEx, "list ...", CompletionItemKind.Operator, "ex03")>]
    [<DataRow(LiteralEx, "range ...", CompletionItemKind.Operator, "ex04")>]
    [<DataRow(LiteralEx, "combined ...", CompletionItemKind.Operator, "ex05")>]
    [<DataRow(LiteralEx, "", CompletionItemKind.Keyword, "zzzex")>]
    [<DataRow(LiteralExN, "exn!1 ...", CompletionItemKind.Operator, "exn01")>]
    [<DataRow(LiteralExN, "type ...", CompletionItemKind.Operator, "exn02")>]
    [<DataRow(LiteralExN, "list ...", CompletionItemKind.Operator, "exn03")>]
    [<DataRow(LiteralExN, "range ...", CompletionItemKind.Operator, "exn04")>]
    [<DataRow(LiteralExN, "", CompletionItemKind.Keyword, "zzzexn")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, subType:string, isKeyword:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesQuantifier()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind = isKeyword then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralAll)>]
    [<DataRow(LiteralEx)>]
    [<DataRow(LiteralExN)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithSpace(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesQuantifier()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
        
                Assert.IsTrue(item.InsertText.EndsWith(' '))
        )
    


    [<DataRow(LiteralAll)>]
    [<DataRow(LiteralEx)>]
    [<DataRow(LiteralExN)>]
    [<TestMethod>]
    member this.TestAddQuantifierChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesQuantifier()).GetChoices(detailCi)

        actual
        |>Seq.iter (fun item ->
            Assert.IsTrue(item.Label.Contains(choice))
        )


    [<DataRow(LiteralAll, LiteralAll)>]
    [<DataRow(LiteralEx, "exists")>]
    [<DataRow(LiteralExN, "n-times")>]
    [<TestMethod>]
    member this.TestAddQuantifierChoicesDetail(choice:string, s:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesQuantifier()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind <> CompletionItemKind.Keyword then
        
                Assert.IsTrue(item.Detail.Contains(s))
        )
    


    [<DataRow(LiteralAll)>]
    [<DataRow(LiteralEx)>]
    [<DataRow(LiteralExN)>]
    [<TestMethod>]
    member this.TestAddQuantifierChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesQuantifier()).GetChoices(detailCi)

        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice))
            if item.InsertText.Contains('{') then
        
                let res = testParser PrimQuantifier item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )    
        
    

