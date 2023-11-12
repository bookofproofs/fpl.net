using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemSelf
    {

        [DataRow("ctor")]
        [DataRow("Self")]
        [TestMethod]
        public void TestAddSelfChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("ctor")]
        [DataRow("Self")]
        [TestMethod]
        public void TestAddSelfKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("ctor")]
        [DataRow("Self")]
        [TestMethod]
        public void TestAddSelfChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("Self"));
            }
        }

        [DataRow("ctor")]
        [DataRow("Self")]
        [TestMethod]
        public void TestAddSelfChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("ctor")]
        [DataRow("Self")]
        [TestMethod]
        public void TestAddSelfChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow("ctor")]
        [DataRow("Self")]
        [TestMethod]
        public void TestAddSelfChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
