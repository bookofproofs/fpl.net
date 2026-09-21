namespace TestFpl3LanguageServer.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Constructor
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestConstructor () =

    [<DataRow(LiteralCtor)>]
    [<DataRow(LiteralCtorL)>]
    [<TestMethod>]
    member this.TestAddConstructorChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesConstructor()).GetChoices(detailCi)
        Assert.AreEqual<int>(2, actual.Count)


    [<DataRow(LiteralCtor)>]
    [<DataRow(LiteralCtorL)>]
    [<TestMethod>]
    member this.TestAddConstructorKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesConstructor()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |> Seq.iter (fun item ->
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )    
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralCtorL, CompletionItemKind.Property, "constructor01")>]
    [<DataRow(LiteralCtor, CompletionItemKind.Property, "constructor02")>]
    [<DataRow(LiteralCtorL, CompletionItemKind.Keyword, "zzzconstructor01")>]
    [<DataRow(LiteralCtor, CompletionItemKind.Keyword, "zzzzconstructor02")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, kind:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesConstructor()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            if item.Label.Contains(choice) && item.Kind = kind then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )        
    


    [<DataRow(LiteralCtorL)>]
    [<DataRow(LiteralCtor)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesConstructor()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) then
        
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )        
    


    [<DataRow(LiteralCtor)>]
    [<DataRow(LiteralCtorL)>]
    [<TestMethod>]
    member this.TestAddConstructorChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesConstructor()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )    


    [<DataRow(LiteralCtor)>]
    [<DataRow(LiteralCtorL)>]
    [<TestMethod>]
    member this.TestAddConstructorChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesConstructor()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Detail.Contains(choice))
        )    


    [<DataRow(LiteralCtor)>]
    [<DataRow(LiteralCtorL)>]
    [<TestMethod>]
    member this.TestAddConstructorChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesConstructor()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(' ') then
        
                let res = testParser LiteralCtor item.InsertText
                if not (res.StartsWith("Success:")) then
                    Assert.Fail(res)
        )            
        
    
        Assert.AreEqual<int>(actual.Count, counterSnippets)

