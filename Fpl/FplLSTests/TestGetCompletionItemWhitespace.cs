using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemWhitespace
    {

        [DataRow("whitespace")]
        [DataRow("significant whitespace")]
        [TestMethod]
        public void TestAddWhitespaceChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWhitespace().GetChoices(detailCi);
            Assert.AreEqual<int>(1, actual.Count);
        }

        [DataRow("whitespace")]
        [DataRow("significant whitespace")]
        [TestMethod]
        public void TestAddWhitespaceTextCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWhitespace().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Text) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow("whitespace", CompletionItemKind.Text, "zzzzz")]
        [DataRow("significant whitespace", CompletionItemKind.Text, "zzzzz")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWhitespace().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow("whitespace")]
        [DataRow("significant whitespace")]
        [TestMethod]
        public void TestAddWhitespaceChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWhitespace().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains("' '") && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("whitespace", "(whitespace)")]
        [DataRow("significant whitespace", "(whitespace)")]
        [TestMethod]
        public void TestAddWhitespaceChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWhitespace().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual<string>(l, item.Detail);
            }
        }

        [DataRow("whitespace")]
        [DataRow("significant whitespace")]
        [TestMethod]
        public void TestAddWhitespaceChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWhitespace().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual<string>(" ", item.InsertText);
            }
        }
    }
}
