namespace TestFpl3LanguageServer.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Digits
open Fpl0Base.Primitives

[<TestClass>]
type TestDigits () =

    [<DataRow("digits")>]
    [<TestMethod>]
    member this.TestAddDigitsChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDigits()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow("digits")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDigits()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            Assert.IsTrue(item.SortText.Contains("123"))
        )
        

    [<DataRow("digits")>]
    [<TestMethod>]
    member this.TestAddDigitsChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDigits()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains("123") && item.Label.StartsWith("_ "))
        )    


    [<DataRow("digits")>]
    [<TestMethod>]
    member this.TestAddDigitsChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesDigits()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Detail.Contains(choice))
        )    

