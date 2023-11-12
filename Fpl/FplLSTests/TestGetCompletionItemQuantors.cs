using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemQuantors
    {

        [DataRow("all", 6)]
        [DataRow("ex", 6)]
        [DataRow("exn", 5)]
        [TestMethod]
        public void TestAddQuantorChoicesNumber(string choice, int number)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);
            Assert.AreEqual(number, actual.Count);
        }

        [DataRow("all", 1)]
        [DataRow("ex", 1)]
        [DataRow("exn", 1)]
        [TestMethod]
        public void TestAddQuantorKeywordCounts(string choice, int number)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);

            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(number, count);
        }

        [DataRow("all")]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddQuantorChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);

            var lastSortText = "";
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(choice));
                Assert.IsTrue(string.Compare(lastSortText,item.SortText)<0);
                lastSortText = item.SortText;
            }
        }

        [DataRow("all")]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddQuantorChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);

            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice));
            }
        }

        [DataRow("all", "all")]
        [DataRow("ex", "exists")]
        [DataRow("exn", "n-times")]
        [TestMethod]
        public void TestAddQuantorChoicesDetail(string choice, string s)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(s));
                }
            }
        }

        [DataRow("all")]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddQuantorChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);

            foreach (var item in actual)
            {
                Assert.IsTrue(item.InsertText.Contains(choice));
            }
        }
    }
}
