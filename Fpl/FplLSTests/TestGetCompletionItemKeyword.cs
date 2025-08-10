// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemKeyword
    {
        [DataRow(literalAlias)]
        [DataRow(literalAssL)]
        [DataRow(literalAss)]
        [DataRow(literalAssume)]
        [DataRow(literalByDef)]
        [DataRow(literalCl)]
        [DataRow(literalClL)]
        [DataRow(literalCon)]
        [DataRow(literalConL)]
        [DataRow(literalExt)]
        [DataRow(literalExtL)]
        [DataRow(literalFunc)]
        [DataRow(literalFuncL)]
        [DataRow(literalInd)]
        [DataRow(literalIndL)]
        [DataRow(literalIntr)]
        [DataRow(literalIntrL)]
        [DataRow(literalIn)]
        [DataRow("obj")]
        [DataRow("object")]
        [DataRow("opt")]
        [DataRow("optional")]
        [DataRow("pred")]
        [DataRow("predicate")]
        [DataRow("pre")]
        [DataRow("premise")]
        [DataRow("qed")]
        [DataRow("ret")]
        [DataRow("return")]
        [DataRow("rev")]
        [DataRow("revoke")]
        [DataRow("trivial")]
        [TestMethod]
        public void TestAddKeywordChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            Assert.AreEqual<int>(1, actual.Count);
        }

        [DataRow(literalAlias)]
        [DataRow(literalAssL)]
        [DataRow(literalAss)]
        [DataRow(literalAssume)]
        [DataRow(literalByDef)]
        [DataRow(literalCl)]
        [DataRow(literalClL)]
        [DataRow(literalCon)]
        [DataRow(literalConL)]
        [DataRow(literalExt)]
        [DataRow(literalExtL)]
        [DataRow(literalFunc)]
        [DataRow(literalFuncL)]
        [DataRow(literalInd)]
        [DataRow(literalIndL)]
        [DataRow(literalIntr)]
        [DataRow(literalIntrL)]
        [DataRow(literalIn)]
        [DataRow("obj")]
        [DataRow("object")]
        [DataRow("opt")]
        [DataRow("optional")]
        [DataRow("pred")]
        [DataRow("predicate")]
        [DataRow("pre")]
        [DataRow("premise")]
        [DataRow("qed")]
        [DataRow("ret")]
        [DataRow("return")]
        [DataRow("rev")]
        [DataRow("revoke")]
        [DataRow("trivial")]

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

        [DataRow(literalAlias)]
        [DataRow(literalAssL)]
        [DataRow(literalAss)]
        [DataRow(literalAssume)]
        [DataRow(literalByDef)]
        [DataRow(literalCl)]
        [DataRow(literalClL)]
        [DataRow(literalCon)]
        [DataRow(literalConL)]
        [DataRow(literalExt)]
        [DataRow(literalExtL)]
        [DataRow(literalFunc)]
        [DataRow(literalFuncL)]
        [DataRow(literalInd)]
        [DataRow(literalIndL)]
        [DataRow(literalIntr)]
        [DataRow(literalIntrL)]
        [DataRow(literalIn)]
        [DataRow("obj")]
        [DataRow("object")]
        [DataRow("opt")]
        [DataRow("optional")]
        [DataRow("pred")]
        [DataRow("predicate")]
        [DataRow("pre")]
        [DataRow("premise")]
        [DataRow("qed")]
        [DataRow("ret")]
        [DataRow("return")]
        [DataRow("rev")]
        [DataRow("revoke")]
        [DataRow("trivial")]

        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(choice));
            }
        }

        [DataRow(literalAlias)]
        [DataRow(literalAssL)]
        [DataRow(literalAss)]
        [DataRow(literalAssume)]
        [DataRow(literalByDef)]
        [DataRow(literalCl)]
        [DataRow(literalClL)]
        [DataRow(literalCon)]
        [DataRow(literalConL)]
        [DataRow(literalExt)]
        [DataRow(literalExtL)]
        [DataRow(literalFunc)]
        [DataRow(literalFuncL)]
        [DataRow(literalInd)]
        [DataRow(literalIndL)]
        [DataRow(literalIntr)]
        [DataRow(literalIntrL)]
        [DataRow(literalIn)]
        [DataRow("obj")]
        [DataRow("object")]
        [DataRow("opt")]
        [DataRow("optional")]
        [DataRow("pred")]
        [DataRow("predicate")]
        [DataRow("pre")]
        [DataRow("premise")]
        [DataRow("qed")]
        [DataRow("ret")]
        [DataRow("return")]
        [DataRow("rev")]
        [DataRow("revoke")]
        [DataRow("trivial")]

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

        [DataRow(literalAlias)]
        [DataRow(literalAssL)]
        [DataRow(literalAss)]
        [DataRow(literalAssume)]
        [DataRow(literalByDef)]
        [DataRow(literalCl)]
        [DataRow(literalClL)]
        [DataRow(literalCon)]
        [DataRow(literalConL)]
        [DataRow(literalExt)]
        [DataRow(literalExtL)]
        [DataRow(literalFunc)]
        [DataRow(literalFuncL)]
        [DataRow(literalInd)]
        [DataRow(literalIndL)]
        [DataRow(literalIntr)]
        [DataRow(literalIntrL)]
        [DataRow(literalIn)]
        [DataRow("obj")]
        [DataRow("object")]
        [DataRow("opt")]
        [DataRow("optional")]
        [DataRow("pred")]
        [DataRow("predicate")]
        [DataRow("pre")]
        [DataRow("premise")]
        [DataRow("qed")]
        [DataRow("ret")]
        [DataRow("return")]
        [DataRow("rev")]
        [DataRow("revoke")]
        [DataRow("trivial")]

        [TestMethod]
        public void TestAddKeywordChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow(literalAlias)]
        [DataRow(literalAssL)]
        [DataRow(literalAss)]
        [DataRow(literalAssume)]
        [DataRow(literalByDef)]
        [DataRow(literalCl)]
        [DataRow(literalClL)]
        [DataRow(literalCon)]
        [DataRow(literalConL)]
        [DataRow(literalExt)]
        [DataRow(literalExtL)]
        [DataRow(literalFunc)]
        [DataRow(literalFuncL)]
        [DataRow(literalInd)]
        [DataRow(literalIndL)]
        [DataRow(literalIntr)]
        [DataRow(literalIntrL)]
        [DataRow(literalIn)]
        [DataRow("obj")]
        [DataRow("object")]
        [DataRow("opt")]
        [DataRow("optional")]
        [DataRow("pred")]
        [DataRow("predicate")]
        [DataRow("pre")]
        [DataRow("premise")]
        [DataRow("qed")]
        [DataRow("ret")]
        [DataRow("return")]
        [DataRow("rev")]
        [DataRow("revoke")]
        [DataRow("trivial")]

        [TestMethod]
        public void TestAddKeywordChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
