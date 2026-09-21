namespace TestFpl3LanguageServer.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Variable
open Fpl0Base.Primitives

[<TestClass>]
type TestVariable () =

    [<DataRow(PrimVariableL)>]
    [<DataRow("variable (got keyword)")>]
    [<DataRow("variable (got template)")>]
    [<TestMethod>]
    member this.TestAddVariableChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesVariable()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow(PrimVariableL)>]
    [<DataRow("variable (got keyword)")>]
    [<DataRow("variable (got template)")>]
    [<TestMethod>]
    member this.TestAddVariableVariableCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesVariable()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Variable then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(PrimVariableL, PrimVariableL)>]
    [<DataRow("variable (got keyword)", PrimVariableL)>]
    [<DataRow("variable (got template)", PrimVariableL)>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesVariable()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(expected, item.SortText)
        )


    [<DataRow(PrimVariableL)>]
    [<DataRow("variable (got keyword)")>]
    [<DataRow("variable (got template)")>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithSpace(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesVariable()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
            if item.Kind <> CompletionItemKind.Keyword && not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                Assert.IsTrue(item.InsertText.EndsWith(" "))
        )
    


    [<DataRow(PrimVariableL, "someVar")>]
    [<DataRow("variable (got keyword)", "someVar")>]
    [<DataRow("variable (got template)", "someVar")>]
    [<TestMethod>]
    member this.TestAddVariableChoicesLabel(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesVariable()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(l) && item.Label.StartsWith("_ "))
        )


    [<DataRow(PrimVariableL)>]
    [<DataRow("variable (got keyword)")>]
    [<DataRow("variable (got template)")>]
    [<TestMethod>]
    member this.TestAddVariableChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesVariable()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(not (String.IsNullOrEmpty(item.InsertText)) && not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(PrimVariableL))
        )


    [<DataRow(PrimVariableL, "someVar")>]
    [<DataRow("variable (got keyword)", "someVar")>]
    [<DataRow("variable (got template)", "someVar")>]
    [<TestMethod>]
    member this.TestAddVariableChoicesInsertText(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesVariable()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreEqual<string>(l + " ", item.InsertText)
        )

