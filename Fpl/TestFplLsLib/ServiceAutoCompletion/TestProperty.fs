namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Property
open Fpl0Base.Primitives
open Fpl.Parser.Main

[<TestClass>]
type TestProperty () =

    [<DataRow(LiteralPrty)>]
    [<DataRow(LiteralPrtyL)>]
    [<TestMethod>]
    member this.TestAddPropertyChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesProperty()).GetChoices(detailCi)
        Assert.AreEqual<int>(4, actual.Count)


    [<DataRow(LiteralPrty)>]
    [<DataRow(LiteralPrtyL)>]
    [<TestMethod>]
    member this.TestAddPropertyKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesProperty()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(2, count)


    [<DataRow(LiteralPrtyL, LiteralPredL, CompletionItemKind.Value, "property02")>]
    [<DataRow(LiteralPrtyL, LiteralFuncL, CompletionItemKind.Value, "property03")>]
    [<DataRow(LiteralPrty, LiteralPred, CompletionItemKind.Value, "zproperty02")>]
    [<DataRow(LiteralPrty, LiteralFunc, CompletionItemKind.Value, "zproperty03")>]

    [<DataRow(LiteralPrtyL, LiteralPredL, CompletionItemKind.Keyword, "zzzproperty02")>]
    [<DataRow(LiteralPrtyL, LiteralFuncL, CompletionItemKind.Keyword, "zzzproperty03")>]
    [<DataRow(LiteralPrty, LiteralPred, CompletionItemKind.Keyword, "zzzzproperty02")>]
    [<DataRow(LiteralPrty, LiteralFunc, CompletionItemKind.Keyword, "zzzzproperty03")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, subType:string, isKeyword:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesProperty()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind = isKeyword then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralPrty, "pred ")>]
    [<DataRow(LiteralPrtyL, LiteralPredL)>]
    [<DataRow(LiteralPrty, "func ")>]
    [<DataRow(LiteralPrtyL, LiteralFuncL)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesProperty()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) && item.InsertText.Contains(l) then
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )
    



    [<DataRow(LiteralPrty, LiteralPred)>]
    [<DataRow(LiteralPrtyL, LiteralPredL)>]
    [<DataRow(LiteralPrty, LiteralFunc)>]
    [<DataRow(LiteralPrtyL, LiteralFuncL)>]
    [<TestMethod>]
    member this.TestAddPropertyChoicesLabel(choice:string, subType:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesProperty()).GetChoices(detailCi)
        let mutable counterRelated = 0;
        actual
        |>Seq.iter (fun item ->
    
            let mutable postfix = "";
            if item.Kind <> CompletionItemKind.Keyword then
        
                postfix <- " ...";
        

            if item.Label.Contains(subType) then
                Assert.AreEqual<string>("_ " + choice + " " + subType + postfix, item.Label)
                counterRelated <- counterRelated + 1
        )
    
        Assert.AreEqual<int>(2, counterRelated)


    [<DataRow(LiteralPrty, LiteralPrty)>]
    [<DataRow(LiteralPrtyL, LiteralPrtyL)>]
    [<TestMethod>]
    member this.TestAddPropertyChoicesDetailKeyword(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesProperty()).GetChoices(detailCi)
        let mutable countPredicative = 0;
        let mutable countFunctional = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then
        
                Assert.IsTrue(not (String.IsNullOrEmpty(item.Detail)) && not (String.IsNullOrEmpty(item.InsertText)) && item.Detail.Contains(l))
                if item.Detail.Contains(LiteralPred) then countPredicative <- countPredicative + 1
                if item.Detail.Contains(LiteralFunc) then countFunctional <- countFunctional + 1
        )
    
        Assert.AreEqual<int>(1, countPredicative)
        Assert.AreEqual<int>(1, countFunctional)



    [<DataRow(LiteralPrty, "pr")>]
    [<DataRow(LiteralPrtyL, LiteralPrtyL)>]
    [<TestMethod>]
    member this.TestAddPropertyChoicesDetailNonKeyword(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesProperty()).GetChoices(detailCi)
        let mutable countPredicative = 0;
        let mutable countFunctional = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind <> CompletionItemKind.Keyword then
        
                Assert.IsTrue(not (String.IsNullOrEmpty(item.InsertText)) && not (String.IsNullOrEmpty(item.Detail)) && item.Detail.Contains(l))
                if item.Detail.Contains(LiteralPred) then countPredicative <- countPredicative + 1
                if item.Detail.Contains(LiteralFunc) then countFunctional <- countFunctional + 1
        )
    
        Assert.AreEqual<int>(1, countPredicative)
        Assert.AreEqual<int>(1, countFunctional)



    [<DataRow(LiteralPrty, LiteralPred)>]
    [<DataRow(LiteralPrtyL, LiteralPredL)>]
    [<DataRow(LiteralPrty, LiteralFunc)>]
    [<DataRow(LiteralPrtyL, LiteralFuncL)>]
    [<TestMethod>]
    member this.TestAddPropertyChoicesInsertText(choice:string, subType:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesProperty()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |>Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(choice)) && item.InsertText.Contains(subType) then
                counterSnippets <- counterSnippets + 1
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains('{') then
        
                let res = testParser LiteralPrty item.InsertText
                if not (res.StartsWith("Success:")) then
            
                    Assert.Fail(res)
        )    
        
    
        Assert.AreEqual<int>(2, counterSnippets)

