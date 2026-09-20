namespace TestFplLsLib.ServiceAutoCompletion

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open FplLsLib.ServiceAutoCompletion.Item
open FplLsLib.ServiceAutoCompletion.Keyword
open Fpl0Base.Primitives

[<TestClass>]
type TestKeyword () =

    [<DataRow(LiteralAlias)>]
    [<DataRow(LiteralAssL)>]
    [<DataRow(LiteralAss)>]
    [<DataRow(LiteralAssert)>]
    [<DataRow(LiteralByDef)>]
    [<DataRow(LiteralCl)>]
    [<DataRow(LiteralClL)>]
    [<DataRow(LiteralCon)>]
    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralExt)>]
    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralFunc)>]
    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralInd)>]
    [<DataRow(LiteralIndL)>]
    [<DataRow(LiteralIntr)>]
    [<DataRow(LiteralIntrL)>]
    [<DataRow(LiteralIn)>]
    [<DataRow(LiteralObj)>]
    [<DataRow(LiteralObjL)>]
    [<DataRow(LiteralPred)>]
    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPre)>]
    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralQed)>]
    [<DataRow(LiteralRet)>]
    [<DataRow(LiteralRetL)>]
    [<DataRow(LiteralRev)>]
    [<DataRow(LiteralRevL)>]
    [<DataRow(LiteralTrivial)>]
    [<TestMethod>]
    member this.TestAddKeywordChoicesNumber(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesKeyword()).GetChoices(detailCi)
        Assert.AreEqual<int>(1, actual.Count)


    [<DataRow(LiteralAlias)>]
    [<DataRow(LiteralAssL)>]
    [<DataRow(LiteralAss)>]
    [<DataRow(LiteralAssert)>]
    [<DataRow(LiteralByDef)>]
    [<DataRow(LiteralCl)>]
    [<DataRow(LiteralClL)>]
    [<DataRow(LiteralCon)>]
    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralExt)>]
    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralFunc)>]
    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralInd)>]
    [<DataRow(LiteralIndL)>]
    [<DataRow(LiteralIntr)>]
    [<DataRow(LiteralIntrL)>]
    [<DataRow(LiteralIn)>]
    [<DataRow(LiteralObj)>]
    [<DataRow(LiteralObjL)>]
    [<DataRow(LiteralPred)>]
    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPre)>]
    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralQed)>]
    [<DataRow(LiteralRet)>]
    [<DataRow(LiteralRetL)>]
    [<DataRow(LiteralRev)>]
    [<DataRow(LiteralRevL)>]
    [<DataRow(LiteralTrivial)>]

    [<TestMethod>]
    member this.TestAddKeywordKeywordCounts(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesKeyword()).GetChoices(detailCi)
        let mutable count = 0;
        actual
        |>Seq.iter (fun item ->
    
            if item.Kind = CompletionItemKind.Keyword then count <- count + 1
        )
        Assert.AreEqual<int>(1, count)


    [<DataRow(LiteralAlias)>]
    [<DataRow(LiteralAssL)>]
    [<DataRow(LiteralAss)>]
    [<DataRow(LiteralAssert)>]
    [<DataRow(LiteralByDef)>]
    [<DataRow(LiteralCl)>]
    [<DataRow(LiteralClL)>]
    [<DataRow(LiteralCon)>]
    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralExt)>]
    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralFunc)>]
    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralInd)>]
    [<DataRow(LiteralIndL)>]
    [<DataRow(LiteralIntr)>]
    [<DataRow(LiteralIntrL)>]
    [<DataRow(LiteralIn)>]
    [<DataRow(LiteralObj)>]
    [<DataRow(LiteralObjL)>]
    [<DataRow(LiteralPred)>]
    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPre)>]
    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralQed)>]
    [<DataRow(LiteralRet)>]
    [<DataRow(LiteralRetL)>]
    [<DataRow(LiteralRev)>]
    [<DataRow(LiteralRevL)>]
    [<DataRow(LiteralTrivial)>]

    [<TestMethod>]
    member this.TestAddChoicesSortText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesKeyword()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.SortText.Contains(choice))
        )


    [<DataRow(LiteralAlias)>]
    [<DataRow(LiteralAssL)>]
    [<DataRow(LiteralAss)>]
    [<DataRow(LiteralAssert)>]
    [<DataRow(LiteralByDef)>]
    [<DataRow(LiteralCl)>]
    [<DataRow(LiteralClL)>]
    [<DataRow(LiteralCon)>]
    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralExt)>]
    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralFunc)>]
    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralInd)>]
    [<DataRow(LiteralIndL)>]
    [<DataRow(LiteralIntr)>]
    [<DataRow(LiteralIntrL)>]
    [<DataRow(LiteralIn)>]
    [<DataRow(LiteralObj)>]
    [<DataRow(LiteralObjL)>]
    [<DataRow(LiteralPred)>]
    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPre)>]
    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralQed)>]
    [<DataRow(LiteralRet)>]
    [<DataRow(LiteralRetL)>]
    [<DataRow(LiteralRev)>]
    [<DataRow(LiteralRevL)>]
    [<DataRow(LiteralTrivial)>]

    [<TestMethod>]
    member this.TestAddKeywordChoicesLabel(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesKeyword()).GetChoices(detailCi)
        actual
        |>Seq.iter (fun item ->
    
            Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "))
        )


    [<DataRow(LiteralAlias)>]
    [<DataRow(LiteralAssL)>]
    [<DataRow(LiteralAss)>]
    [<DataRow(LiteralAssert)>]
    [<DataRow(LiteralByDef)>]
    [<DataRow(LiteralCl)>]
    [<DataRow(LiteralClL)>]
    [<DataRow(LiteralCon)>]
    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralExt)>]
    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralFunc)>]
    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralInd)>]
    [<DataRow(LiteralIndL)>]
    [<DataRow(LiteralIntr)>]
    [<DataRow(LiteralIntrL)>]
    [<DataRow(LiteralIn)>]
    [<DataRow(LiteralObj)>]
    [<DataRow(LiteralObjL)>]
    [<DataRow(LiteralPred)>]
    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPre)>]
    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralQed)>]
    [<DataRow(LiteralRet)>]
    [<DataRow(LiteralRetL)>]
    [<DataRow(LiteralRev)>]
    [<DataRow(LiteralRevL)>]
    [<DataRow(LiteralTrivial)>]

    [<TestMethod>]
    member this.TestAddKeywordChoicesDetail(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesKeyword()).GetChoices(detailCi)
        actual
        |> Seq.iter (fun item ->
    
            Assert.IsTrue(item.Detail.Contains(choice))
        )


    [<DataRow(LiteralAlias)>]
    [<DataRow(LiteralAssL)>]
    [<DataRow(LiteralAss)>]
    [<DataRow(LiteralAssert)>]
    [<DataRow(LiteralByDef)>]
    [<DataRow(LiteralCl)>]
    [<DataRow(LiteralClL)>]
    [<DataRow(LiteralCon)>]
    [<DataRow(LiteralConL)>]
    [<DataRow(LiteralExt)>]
    [<DataRow(LiteralExtL)>]
    [<DataRow(LiteralFunc)>]
    [<DataRow(LiteralFuncL)>]
    [<DataRow(LiteralInd)>]
    [<DataRow(LiteralIndL)>]
    [<DataRow(LiteralIntr)>]
    [<DataRow(LiteralIntrL)>]
    [<DataRow(LiteralIn)>]
    [<DataRow(LiteralObj)>]
    [<DataRow(LiteralObjL)>]
    [<DataRow(LiteralPred)>]
    [<DataRow(LiteralPredL)>]
    [<DataRow(LiteralPre)>]
    [<DataRow(LiteralPreL)>]
    [<DataRow(LiteralQed)>]
    [<DataRow(LiteralRet)>]
    [<DataRow(LiteralRetL)>]
    [<DataRow(LiteralRev)>]
    [<DataRow(LiteralRevL)>]
    [<DataRow(LiteralTrivial)>]
    [<TestMethod>]
    member this.TestAddKeywordChoicesInsertText(choice: string) =

        let detailCi = new FplCompletionItem(choice, "")
        let actual = (new FplCompletionItemChoicesKeyword()).GetChoices(detailCi)
        let mutable counterSnippets = 0;
        actual
        |> Seq.iter (fun item ->
    
            if not (String.IsNullOrEmpty(item.InsertText)) && item.InsertText.Contains(choice) then
                counterSnippets <- counterSnippets + 1
        )
        Assert.AreEqual<int>(actual.Count, counterSnippets)

