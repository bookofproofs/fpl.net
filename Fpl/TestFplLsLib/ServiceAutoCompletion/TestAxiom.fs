namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Axiom
open Fpl0Base.Primitives
open Fpl1Parser.Main


[<TestClass>]
type TestAxiom () =

    [<DataRow(LiteralAx)>]
    [<DataRow(LiteralAxL)>]
    [<DataRow(LiteralPost)>]
    [<DataRow(LiteralPostL)>]
    [<TestMethod>]
    member this.TestAddAxiomChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesAxiom()).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralAx)>]
    [<DataRow(LiteralAxL)>]
    [<DataRow(LiteralPost)>]
    [<DataRow(LiteralPostL)>]
    [<TestMethod>]
    member this.TestAddAxiomKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesAxiom()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |> Seq.iter (fun item ->
            if item.Kind = CompletionItemKind.Keyword then
                count <- count + 1;
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralAx)>]
    [<DataRow(LiteralAxL)>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesAxiom()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.SortText.Contains(LiteralAxL))
        )
    
    [<DataRow(LiteralPost)>]
    [<DataRow(LiteralPostL)>]
    [<TestMethod>]
    member this.TestAddPostulateChoicesSortText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesAxiom()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.SortText.Contains(LiteralPostL))
        )
    
    


    [<DataRow(LiteralAx)>]
    [<DataRow(LiteralAxL)>]
    [<DataRow(LiteralPost)>]
    [<DataRow(LiteralPostL)>]
    [<TestMethod>]
    member this.TestAddAxiomChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesAxiom()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )
    
    


    [<DataRow(LiteralAx)>]
    [<DataRow(LiteralAxL)>]
    [<DataRow(LiteralPost)>]
    [<DataRow(LiteralPostL)>]
    [<TestMethod>]
    member this.TestAddAxiomChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesAxiom()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.SortText.Contains(choice))
        )

    


    [<DataRow(LiteralAx)>]
    [<DataRow(LiteralAxL)>]
    [<DataRow(LiteralPost)>]
    [<DataRow(LiteralPostL)>]
    [<TestMethod>]
    member this.TestAddAxiomChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesAxiom()).GetChoices(detailCi)
        let mutable counterSnippets = 0
        actual
        |> Seq.iter (fun item ->

            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1

            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
                let res = testParser LiteralAx item.InsertText
                if not (res.StartsWith("Success:")) then
                    Assert.Fail(res)
        )
        Assert.AreEqual<int>(actual.Count, counterSnippets)


    [<DataRow(LiteralAx)>]
    [<DataRow(LiteralAxL)>]
    [<DataRow(LiteralPost)>]
    [<DataRow(LiteralPostL)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesAxiom()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind<>CompletionItemKind.Keyword && item.InsertText.Contains(choice) then 
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )
    
        
    

