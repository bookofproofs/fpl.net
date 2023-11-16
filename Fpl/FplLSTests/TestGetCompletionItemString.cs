using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemString
    {

        [DataRow("language-specific string")]
        [TestMethod]
        public void TestAddStringChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesString().GetChoices(detailCi);
            Assert.AreEqual(1, actual.Count);
        }

        [DataRow("language-specific string")]
        [TestMethod]
        public void TestAddStringValueCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesString().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Value) count++;
            }
            Assert.AreEqual(actual.Count, count);
        }

        [DataRow("language-specific string", "\"\"")]
        [TestMethod]
        public void TestAddStringChoicesSortText(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesString().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual(l, item.SortText);
            }
        }

        [DataRow("language-specific string", "\"...\"")]
        [TestMethod]
        public void TestAddStringChoicesLabel(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesString().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(l) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("language-specific string")]
        [TestMethod]
        public void TestAddStringChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesString().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow("language-specific string", "\"...\" ")]
        [TestMethod]
        public void TestAddStringChoicesInsertText(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesString().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual(l, item.InsertText);
            }
        }
    }
}
