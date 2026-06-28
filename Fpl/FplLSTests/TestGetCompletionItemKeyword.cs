// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace TestFplLS
{
    [TestClass]
    public class TestGetCompletionItemKeyword
    {
        [DataRow(LiteralAlias)]
        [DataRow(LiteralAssL)]
        [DataRow(LiteralAss)]
        [DataRow(LiteralAssert)]
        [DataRow(LiteralByDef)]
        [DataRow(LiteralCl)]
        [DataRow(LiteralClL)]
        [DataRow(LiteralCon)]
        [DataRow(LiteralConL)]
        [DataRow(LiteralExt)]
        [DataRow(LiteralExtL)]
        [DataRow(LiteralFunc)]
        [DataRow(LiteralFuncL)]
        [DataRow(LiteralInd)]
        [DataRow(LiteralIndL)]
        [DataRow(LiteralIntr)]
        [DataRow(LiteralIntrL)]
        [DataRow(LiteralIn)]
        [DataRow(LiteralObj)]
        [DataRow(LiteralObjL)]
        [DataRow(LiteralPred)]
        [DataRow(LiteralPredL)]
        [DataRow(LiteralPre)]
        [DataRow(LiteralPreL)]
        [DataRow(LiteralQed)]
        [DataRow(LiteralRet)]
        [DataRow(LiteralRetL)]
        [DataRow(LiteralRev)]
        [DataRow(LiteralRevL)]
        [DataRow(LiteralTrivial)]
        [TestMethod]
        public void TestAddKeywordChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            Assert.HasCount(1, actual);
        }

        [DataRow(LiteralAlias)]
        [DataRow(LiteralAssL)]
        [DataRow(LiteralAss)]
        [DataRow(LiteralAssert)]
        [DataRow(LiteralByDef)]
        [DataRow(LiteralCl)]
        [DataRow(LiteralClL)]
        [DataRow(LiteralCon)]
        [DataRow(LiteralConL)]
        [DataRow(LiteralExt)]
        [DataRow(LiteralExtL)]
        [DataRow(LiteralFunc)]
        [DataRow(LiteralFuncL)]
        [DataRow(LiteralInd)]
        [DataRow(LiteralIndL)]
        [DataRow(LiteralIntr)]
        [DataRow(LiteralIntrL)]
        [DataRow(LiteralIn)]
        [DataRow(LiteralObj)]
        [DataRow(LiteralObjL)]
        [DataRow(LiteralPred)]
        [DataRow(LiteralPredL)]
        [DataRow(LiteralPre)]
        [DataRow(LiteralPreL)]
        [DataRow(LiteralQed)]
        [DataRow(LiteralRet)]
        [DataRow(LiteralRetL)]
        [DataRow(LiteralRev)]
        [DataRow(LiteralRevL)]
        [DataRow(LiteralTrivial)]

        [TestMethod]
        public void TestAddKeywordKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralAlias)]
        [DataRow(LiteralAssL)]
        [DataRow(LiteralAss)]
        [DataRow(LiteralAssert)]
        [DataRow(LiteralByDef)]
        [DataRow(LiteralCl)]
        [DataRow(LiteralClL)]
        [DataRow(LiteralCon)]
        [DataRow(LiteralConL)]
        [DataRow(LiteralExt)]
        [DataRow(LiteralExtL)]
        [DataRow(LiteralFunc)]
        [DataRow(LiteralFuncL)]
        [DataRow(LiteralInd)]
        [DataRow(LiteralIndL)]
        [DataRow(LiteralIntr)]
        [DataRow(LiteralIntrL)]
        [DataRow(LiteralIn)]
        [DataRow(LiteralObj)]
        [DataRow(LiteralObjL)]
        [DataRow(LiteralPred)]
        [DataRow(LiteralPredL)]
        [DataRow(LiteralPre)]
        [DataRow(LiteralPreL)]
        [DataRow(LiteralQed)]
        [DataRow(LiteralRet)]
        [DataRow(LiteralRetL)]
        [DataRow(LiteralRev)]
        [DataRow(LiteralRevL)]
        [DataRow(LiteralTrivial)]

        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.Contains(choice, item.SortText ?? string.Empty);
            }
        }

        [DataRow(LiteralAlias)]
        [DataRow(LiteralAssL)]
        [DataRow(LiteralAss)]
        [DataRow(LiteralAssert)]
        [DataRow(LiteralByDef)]
        [DataRow(LiteralCl)]
        [DataRow(LiteralClL)]
        [DataRow(LiteralCon)]
        [DataRow(LiteralConL)]
        [DataRow(LiteralExt)]
        [DataRow(LiteralExtL)]
        [DataRow(LiteralFunc)]
        [DataRow(LiteralFuncL)]
        [DataRow(LiteralInd)]
        [DataRow(LiteralIndL)]
        [DataRow(LiteralIntr)]
        [DataRow(LiteralIntrL)]
        [DataRow(LiteralIn)]
        [DataRow(LiteralObj)]
        [DataRow(LiteralObjL)]
        [DataRow(LiteralPred)]
        [DataRow(LiteralPredL)]
        [DataRow(LiteralPre)]
        [DataRow(LiteralPreL)]
        [DataRow(LiteralQed)]
        [DataRow(LiteralRet)]
        [DataRow(LiteralRetL)]
        [DataRow(LiteralRev)]
        [DataRow(LiteralRevL)]
        [DataRow(LiteralTrivial)]

        [TestMethod]
        public void TestAddKeywordChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(LiteralAlias)]
        [DataRow(LiteralAssL)]
        [DataRow(LiteralAss)]
        [DataRow(LiteralAssert)]
        [DataRow(LiteralByDef)]
        [DataRow(LiteralCl)]
        [DataRow(LiteralClL)]
        [DataRow(LiteralCon)]
        [DataRow(LiteralConL)]
        [DataRow(LiteralExt)]
        [DataRow(LiteralExtL)]
        [DataRow(LiteralFunc)]
        [DataRow(LiteralFuncL)]
        [DataRow(LiteralInd)]
        [DataRow(LiteralIndL)]
        [DataRow(LiteralIntr)]
        [DataRow(LiteralIntrL)]
        [DataRow(LiteralIn)]
        [DataRow(LiteralObj)]
        [DataRow(LiteralObjL)]
        [DataRow(LiteralPred)]
        [DataRow(LiteralPredL)]
        [DataRow(LiteralPre)]
        [DataRow(LiteralPreL)]
        [DataRow(LiteralQed)]
        [DataRow(LiteralRet)]
        [DataRow(LiteralRetL)]
        [DataRow(LiteralRev)]
        [DataRow(LiteralRevL)]
        [DataRow(LiteralTrivial)]

        [TestMethod]
        public void TestAddKeywordChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.Contains(choice, item.Detail ?? string.Empty);
            }
        }

        [DataRow(LiteralAlias)]
        [DataRow(LiteralAssL)]
        [DataRow(LiteralAss)]
        [DataRow(LiteralAssert)]
        [DataRow(LiteralByDef)]
        [DataRow(LiteralCl)]
        [DataRow(LiteralClL)]
        [DataRow(LiteralCon)]
        [DataRow(LiteralConL)]
        [DataRow(LiteralExt)]
        [DataRow(LiteralExtL)]
        [DataRow(LiteralFunc)]
        [DataRow(LiteralFuncL)]
        [DataRow(LiteralInd)]
        [DataRow(LiteralIndL)]
        [DataRow(LiteralIntr)]
        [DataRow(LiteralIntrL)]
        [DataRow(LiteralIn)]
        [DataRow(LiteralObj)]
        [DataRow(LiteralObjL)]
        [DataRow(LiteralPred)]
        [DataRow(LiteralPredL)]
        [DataRow(LiteralPre)]
        [DataRow(LiteralPreL)]
        [DataRow(LiteralQed)]
        [DataRow(LiteralRet)]
        [DataRow(LiteralRetL)]
        [DataRow(LiteralRev)]
        [DataRow(LiteralRevL)]
        [DataRow(LiteralTrivial)]

        [TestMethod]
        public void TestAddKeywordChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
