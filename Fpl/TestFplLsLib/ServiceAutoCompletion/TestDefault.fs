namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Default
open Fpl0Base.Primitives

[<TestClass>]
type TestDefault () =

    [<DataRow("?")>]
    [<DataRow("|")>]
    [<DataRow("@")>]
    [<DataRow(PrimDelegateEqual)>]
    [<DataRow(":=")>]
    [<DataRow(":*")>]
    [<DataRow(":")>]
    [<DataRow(".")>]
    [<DataRow(",")>]
    [<DataRow("~")>]
    [<DataRow("|-")>]
    [<DataRow("->")>]
    [<DataRow(";")>]
    [<DataRow("!")>]
    [<DataRow("{")>]
    [<DataRow("}")>]
    [<DataRow("(")>]
    [<DataRow(")")>]
    [<DataRow("<")>]
    [<DataRow(">")>]
    [<DataRow("blabla")>]
    [<TestMethod>]
    member this.TestAddDefaultChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDefault()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow("?")>]
    [<DataRow("|")>]
    [<DataRow("@")>]
    [<DataRow(PrimDelegateEqual)>]
    [<DataRow(":=")>]
    [<DataRow(":*")>]
    [<DataRow(":")>]
    [<DataRow(".")>]
    [<DataRow(",")>]
    [<DataRow("~")>]
    [<DataRow("|-")>]
    [<DataRow("->")>]
    [<DataRow(";")>]
    [<DataRow("!")>]
    [<DataRow("{")>]
    [<DataRow("}")>]
    [<DataRow("(")>]
    [<DataRow(")")>]
    [<DataRow("<")>]
    [<DataRow(">")>]
    [<DataRow("blabla")>]
    [<TestMethod>]
    member this.TestAddDefaultTextCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDefault()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
            if item.Kind = CompletionItemKind.Text then count <- count + 1
        )

        Assert.AreEqual<int>(1, count)


    [<DataRow("?")>]
    [<DataRow("|")>]
    [<DataRow("@")>]
    [<DataRow(PrimDelegateEqual)>]
    [<DataRow(":=")>]
    [<DataRow(":*")>]
    [<DataRow(":")>]
    [<DataRow(".")>]
    [<DataRow(",")>]
    [<DataRow("~")>]
    [<DataRow("|-")>]
    [<DataRow("->")>]
    [<DataRow(";")>]
    [<DataRow("!")>]
    [<DataRow("{")>]
    [<DataRow("}")>]
    [<DataRow("(")>]
    [<DataRow(")")>]
    [<DataRow("<")>]
    [<DataRow(">")>]
    [<DataRow("blabla")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDefault()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.SortText.Contains(choice))
        )    


    [<DataRow("?")>]
    [<DataRow("|")>]
    [<DataRow("@")>]
    [<DataRow(PrimDelegateEqual)>]
    [<DataRow(":=")>]
    [<DataRow(":*")>]
    [<DataRow(":")>]
    [<DataRow(".")>]
    [<DataRow(",")>]
    [<DataRow("~")>]
    [<DataRow("|-")>]
    [<DataRow("->")>]
    [<DataRow(";")>]
    [<DataRow("!")>]
    [<DataRow("{")>]
    [<DataRow("}")>]
    [<DataRow("(")>]
    [<DataRow(")")>]
    [<DataRow("<")>]
    [<DataRow(">")>]
    [<DataRow("blabla")>]
    [<TestMethod>]
    member this.TestAddDefaultChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDefault()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )    


    [<DataRow("?", "else case '?'")>]
    [<DataRow("|", "new case '|'")>]
    [<DataRow(PrimDelegateEqual, "equal sign '='")>]
    [<DataRow(":=", "assignment sign ':='")>]
    [<DataRow(":*", "zero or more '*'")>]
    [<DataRow(":", "colon ':'")>]
    [<DataRow(".", "dot '.'")>]
    [<DataRow(",", "enumeration ','")>]
    [<DataRow("|-", "follows logically '|-'")>]
    [<DataRow("->", "map '->'")>]
    [<DataRow("{", "opening '{'")>]
    [<DataRow("}", "closing '}'")>]
    [<DataRow("(", "opening '('")>]
    [<DataRow(")", "closing '('")>]
    [<DataRow("[", "opening '['")>]
    [<DataRow("]", "closing ']'")>]
    [<DataRow("blabla", "unknown")>]
    [<TestMethod>]
    member this.TestAddDefaultChoicesDetail(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDefault()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.AreEqual<string>(l, item.Detail)
        )    


    [<DataRow("?")>]
    [<DataRow("|")>]
    [<DataRow("@")>]
    [<DataRow(PrimDelegateEqual)>]
    [<DataRow(":=")>]
    [<DataRow(":*")>]
    [<DataRow(":")>]
    [<DataRow(".")>]
    [<DataRow(",")>]
    [<DataRow("~")>]
    [<DataRow("|-")>]
    [<DataRow("->")>]
    [<DataRow(";")>]
    [<DataRow("{")>]
    [<DataRow("}")>]
    [<DataRow("(")>]
    [<DataRow(")")>]
    [<DataRow("blabla")>]
    [<TestMethod>]
    member this.TestAddDefaultChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDefault()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
        )    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

