using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemBound
    {

        [DataRow("(closed) left bound")]
        [DataRow("(open) left bound")]
        [DataRow("(open) right bound")]
        [DataRow("(closed) right bound")]
        [TestMethod]
        public void TestAddBoundChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesBound().GetChoices(detailCi);
            Assert.AreEqual(1, actual.Count);
        }

        [DataRow("(closed) left bound")]
        [DataRow("(open) left bound")]
        [DataRow("(open) right bound")]
        [DataRow("(closed) right bound")]
        [TestMethod]
        public void TestAddBoundTextCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesBound().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Text) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("(closed) left bound")]
        [DataRow("(open) left bound")]
        [DataRow("(open) right bound")]
        [DataRow("(closed) right bound")]
        [TestMethod]
        public void TestAddBoundChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesBound().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(choice));
            }
        }

        [DataRow("(closed) left bound", "[")]
        [DataRow("(open) left bound", "[(")]
        [DataRow("(open) right bound", ")]")]
        [DataRow("(closed) right bound", "]")]
        [TestMethod]
        public void TestAddBoundChoicesLabel(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesBound().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(l) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("(closed) left bound")]
        [DataRow("(open) left bound")]
        [DataRow("(open) right bound")]
        [DataRow("(closed) right bound")]
        [TestMethod]
        public void TestAddBoundChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesBound().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow("(closed) left bound", "[")]
        [DataRow("(open) left bound", "[(")]
        [DataRow("(open) right bound", ")]")]
        [DataRow("(closed) right bound", "]")]
        [TestMethod]
        public void TestAddBoundChoicesInsertText(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesBound().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual(l, item.InsertText);
            }
        }
    }
}
