using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemCorollary
    {

        [DataRow("ctor")]
        [DataRow("Corollary")]
        [TestMethod]
        public void TestAddCorollaryChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("ctor")]
        [DataRow("Corollary")]
        [TestMethod]
        public void TestAddCorollaryKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("ctor")]
        [DataRow("Corollary")]
        [TestMethod]
        public void TestAddCorollaryChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("Corollary"));
            }
        }

        [DataRow("ctor")]
        [DataRow("Corollary")]
        [TestMethod]
        public void TestAddCorollaryChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("ctor")]
        [DataRow("Corollary")]
        [TestMethod]
        public void TestAddCorollaryChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow("ctor")]
        [DataRow("Corollary")]
        [TestMethod]
        public void TestAddCorollaryChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
