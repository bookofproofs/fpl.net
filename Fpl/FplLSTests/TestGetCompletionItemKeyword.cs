using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemKeyword
    {

        [DataRow("ctor")]
        [DataRow("Keyword")]
        [TestMethod]
        public void TestAddKeywordChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("ctor")]
        [DataRow("Keyword")]
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

        [DataRow("ctor")]
        [DataRow("Keyword")]
        [TestMethod]
        public void TestAddKeywordChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesKeyword().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("Keyword"));
            }
        }

        [DataRow("ctor")]
        [DataRow("Keyword")]
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

        [DataRow("ctor")]
        [DataRow("Keyword")]
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

        [DataRow("ctor")]
        [DataRow("Keyword")]
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
