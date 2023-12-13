using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemKeyword
    {
        [DataRow("alias")]
        [DataRow("assert")]
        [DataRow("ass")]
        [DataRow("assume")]
        [DataRow("bydef")]
        [DataRow("cl")]
        [DataRow("class")]
        [DataRow("con")]
        [DataRow("conclusion")]
        [DataRow("end")]
        [DataRow("ext")]
        [DataRow("func")]
        [DataRow("function")]
        [DataRow("ind")]
        [DataRow("index")]
        [DataRow("intr")]
        [DataRow("intrinsic")]
        [DataRow("in")]
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
            Assert.AreEqual(1, actual.Count);
        }

        [DataRow("alias")]
        [DataRow("assert")]
        [DataRow("ass")]
        [DataRow("assume")]
        [DataRow("bydef")]
        [DataRow("cl")]
        [DataRow("class")]
        [DataRow("con")]
        [DataRow("conclusion")]
        [DataRow("end")]
        [DataRow("ext")]
        [DataRow("func")]
        [DataRow("function")]
        [DataRow("ind")]
        [DataRow("index")]
        [DataRow("intr")]
        [DataRow("intrinsic")]
        [DataRow("in")]
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
            Assert.AreEqual(1, count);
        }

        [DataRow("alias")]
        [DataRow("assert")]
        [DataRow("ass")]
        [DataRow("assume")]
        [DataRow("bydef")]
        [DataRow("cl")]
        [DataRow("class")]
        [DataRow("con")]
        [DataRow("conclusion")]
        [DataRow("end")]
        [DataRow("ext")]
        [DataRow("func")]
        [DataRow("function")]
        [DataRow("ind")]
        [DataRow("index")]
        [DataRow("intr")]
        [DataRow("intrinsic")]
        [DataRow("in")]
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

        [DataRow("alias")]
        [DataRow("assert")]
        [DataRow("ass")]
        [DataRow("assume")]
        [DataRow("bydef")]
        [DataRow("cl")]
        [DataRow("class")]
        [DataRow("con")]
        [DataRow("conclusion")]
        [DataRow("end")]
        [DataRow("ext")]
        [DataRow("func")]
        [DataRow("function")]
        [DataRow("ind")]
        [DataRow("index")]
        [DataRow("intr")]
        [DataRow("intrinsic")]
        [DataRow("in")]
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

        [DataRow("alias")]
        [DataRow("assert")]
        [DataRow("ass")]
        [DataRow("assume")]
        [DataRow("bydef")]
        [DataRow("cl")]
        [DataRow("class")]
        [DataRow("con")]
        [DataRow("conclusion")]
        [DataRow("end")]
        [DataRow("ext")]
        [DataRow("func")]
        [DataRow("function")]
        [DataRow("ind")]
        [DataRow("index")]
        [DataRow("intr")]
        [DataRow("intrinsic")]
        [DataRow("in")]
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

        [DataRow("alias")]
        [DataRow("assert")]
        [DataRow("ass")]
        [DataRow("assume")]
        [DataRow("bydef")]
        [DataRow("cl")]
        [DataRow("class")]
        [DataRow("con")]
        [DataRow("conclusion")]
        [DataRow("end")]
        [DataRow("ext")]
        [DataRow("func")]
        [DataRow("function")]
        [DataRow("ind")]
        [DataRow("index")]
        [DataRow("intr")]
        [DataRow("intrinsic")]
        [DataRow("in")]
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
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
