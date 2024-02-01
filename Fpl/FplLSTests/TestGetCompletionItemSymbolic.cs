using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemSymbolic
    {

        [DataRow("infix")]
        [DataRow("postfix")]
        [DataRow("prefix")]
        [DataRow("symbol")]
        [TestMethod]
        public void TestAddSymbolicChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbolic().GetChoices(detailCi);
            Assert.AreEqual(1, actual.Count);
        }

        [DataRow("infix")]
        [DataRow("postfix")]
        [DataRow("prefix")]
        [DataRow("symbol")]
        [TestMethod]
        public void TestAddSymbolicKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbolic().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(0, count);
        }

        [DataRow("infix")]
        [DataRow("postfix")]
        [DataRow("prefix")]
        [DataRow("symbol")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbolic().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(choice));
            }
        }

        [DataRow("infix")]
        [DataRow("postfix")]
        [DataRow("prefix")]
        [DataRow("symbol")]
        [TestMethod]
        public void TestAddSymbolicChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbolic().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("infix")]
        [DataRow("postfix")]
        [DataRow("prefix")]
        [DataRow("symbol")]
        [TestMethod]
        public void TestAddSymbolicChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbolic().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow("infix")]
        [DataRow("postfix")]
        [DataRow("prefix")]
        [DataRow("symbol")]
        [TestMethod]
        public void TestAddSymbolicChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbolic().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.StartsWith(choice) && item.InsertText.EndsWith("\"...\"")) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }

        [DataRow("infix")]
        [DataRow("postfix")]
        [DataRow("prefix")]
        [DataRow("symbol")]
        [TestMethod]
        public void TestInsertTextDoesNotEndWithNewLine(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbolic().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind!=CompletionItemKind.Keyword && item.InsertText.Contains(choice)) 
                { 
                    Assert.IsTrue(!item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }
    }
}
