using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemUses
    {

        [DataRow("ctor")]
        [DataRow("Uses")]
        [TestMethod]
        public void TestAddUsesChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("ctor")]
        [DataRow("Uses")]
        [TestMethod]
        public void TestAddUsesKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("ctor")]
        [DataRow("Uses")]
        [TestMethod]
        public void TestAddUsesChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("Uses"));
            }
        }

        [DataRow("ctor")]
        [DataRow("Uses")]
        [TestMethod]
        public void TestAddUsesChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("ctor")]
        [DataRow("Uses")]
        [TestMethod]
        public void TestAddUsesChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow("ctor")]
        [DataRow("Uses")]
        [TestMethod]
        public void TestAddUsesChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
