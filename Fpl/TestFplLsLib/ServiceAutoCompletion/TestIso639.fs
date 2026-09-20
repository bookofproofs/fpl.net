namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Iso639
open Fpl0Base.Primitives

[<TestClass>]
type TestIso639 () =

    [<DataRow("ISO 639 language code")>]
    [<TestMethod>]
    member this.TestAddIso639ChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesIso639()).GetChoices(detailCi)
        Assert.AreEqual<int>(490, actual.Count)


    [<DataRow("ISO 639 language code")>]
    [<TestMethod>]
    member this.TestAddIso639KeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesIso639()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Value then
                count <- count + 1
        )
        Assert.AreEqual<int>(actual.Count, count)


    [<DataRow("ISO 639 language code")>]
    [<TestMethod>]
    member this.TestAddIso639ChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesIso639()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.StartsWith("_ "))
        )


    [<DataRow("ISO 639 language code")>]
    [<TestMethod>]
    member this.TestAddIso639ChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesIso639()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.AreNotEqual<string>("", item.Detail)
        )

