namespace TestFpl3LanguageServer.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl3LanguageServer.ServiceAutoCompletion.Item
open Fpl3LanguageServer.ServiceAutoCompletion.Definition
open Fpl0Base.Primitives
open Fpl1Parser.Main

[<TestClass>]
type TestDefinition () =

    [<DataRow(LiteralDef)>]
    [<DataRow(LiteralDefL)>]
    [<TestMethod>]
    member this.TestAddDefinitionChoicesNumber(choice: string) =
        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesDefinition())).GetChoices(detailCi)
        Assert.AreEqual<int>(6, actual.Count)


    [<DataRow(LiteralDef)>]
    [<DataRow(LiteralDefL)>]
    [<TestMethod>]
    member this.TestAddDefinitionKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesDefinition())).GetChoices(detailCi)
        let mutable count = 0
        actual
        |> Seq.iter (fun item ->
            if item.Kind = CompletionItemKind.Keyword then
                count <- count + 1
        )    
        Assert.AreEqual<int>(3, count)


    [<DataRow(LiteralDefL, LiteralClL, CompletionItemKind.Class, "definition01")>]
    [<DataRow(LiteralDefL, LiteralPredL, CompletionItemKind.Class, "definition02")>]
    [<DataRow(LiteralDefL, LiteralFuncL, CompletionItemKind.Class, "definition03")>]
    [<DataRow(LiteralDef, LiteralCl, CompletionItemKind.Class, "zdefinition01")>]
    [<DataRow(LiteralDef, LiteralPred, CompletionItemKind.Class, "zdefinition02")>]
    [<DataRow(LiteralDef, LiteralFunc, CompletionItemKind.Class, "zdefinition03")>]
    [<DataRow(LiteralDefL, LiteralClL, CompletionItemKind.Keyword, "zzzdefinition01")>]
    [<DataRow(LiteralDefL, LiteralPredL, CompletionItemKind.Keyword, "zzzdefinition02")>]
    [<DataRow(LiteralDefL, LiteralFuncL, CompletionItemKind.Keyword, "zzzdefinition03")>]
    [<DataRow(LiteralDef, LiteralCl, CompletionItemKind.Keyword, "zzzzdefinition01")>]
    [<DataRow(LiteralDef, LiteralPred, CompletionItemKind.Keyword, "zzzzdefinition02")>]
    [<DataRow(LiteralDef, LiteralFunc, CompletionItemKind.Keyword, "zzzzdefinition03")>]
    [<TestMethod>]
    member this.TestAddChoicesSortText(choice:string, subType:string, isKeyword:CompletionItemKind, expected:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesDefinition())).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->

            if item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind = isKeyword then
        
                Assert.AreEqual<string>(expected, item.SortText)
        )
    


    [<DataRow(LiteralDef, LiteralCl)>]
    [<DataRow(LiteralDefL, LiteralClL)>]
    [<DataRow(LiteralDef, LiteralPred)>]
    [<DataRow(LiteralDefL, LiteralPredL)>]
    [<DataRow(LiteralDef, LiteralFunc)>]
    [<DataRow(LiteralDefL, LiteralFuncL)>]
    [<TestMethod>]
    member this.TestInsertTextEndsWithTwoNewLines(choice:string, l:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesDefinition())).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.Kind <> CompletionItemKind.Keyword && item.InsertText.Contains(choice) && item.InsertText.Contains(l) then
                Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine))
        )        
    


    [<DataRow(LiteralDef, LiteralCl)>]
    [<DataRow(LiteralDefL, LiteralClL)>]
    [<DataRow(LiteralDef, LiteralPred)>]
    [<DataRow(LiteralDefL, LiteralPredL)>]
    [<DataRow(LiteralDef, LiteralFunc)>]
    [<DataRow(LiteralDefL, LiteralFuncL)>]
    [<TestMethod>]
    member this.TestAddDefinitionChoicesLabel(choice:string, subType:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesDefinition())).GetChoices(detailCi)
        let mutable counterRelated = 0;
        actual
        |> Seq.iter (fun item ->
    
            let mutable postfix = "";
            if item.Kind <> CompletionItemKind.Keyword then
        
                postfix <- " ...";
        
            if item.Label.Contains(subType) then
        
                Assert.AreEqual<string>("_ " + choice + " " + subType + postfix, item.Label)
                counterRelated <- counterRelated + 1
        )        
    
        Assert.AreEqual<int>(2, counterRelated)


    [<DataRow(LiteralDef)>]
    [<DataRow(LiteralDefL)>]
    [<TestMethod>]
    member this.TestAddDefinitionChoicesDetail(choice: string) =
        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesDefinition())).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
            Assert.IsTrue(item.Detail.Contains(choice))
        )    


    [<DataRow(LiteralDef, LiteralCl)>]
    [<DataRow(LiteralDefL, LiteralClL)>]
    [<DataRow(LiteralDef, LiteralPred)>]
    [<DataRow(LiteralDefL, LiteralPredL)>]
    [<DataRow(LiteralDef, LiteralFunc)>]
    [<DataRow(LiteralDefL, LiteralFuncL)>]
    [<TestMethod>]
    member this.TestAddDefinitionChoicesInsertText(choice:string, subType:string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = ((new FplCompletionItemChoicesDefinition())).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) && item.InsertText.Contains(subType) then
                counterSnippets <- counterSnippets + 1

            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains('{') then
        
                let res = testParser LiteralDef item.InsertText
                if not (res.StartsWith("Success:")) then
                    Assert.Fail(res)
        )            
        
    
        Assert.AreEqual<int>(2, counterSnippets)

