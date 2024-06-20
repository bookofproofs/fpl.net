using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDelegate
    {

        [DataRow("del")]
        [DataRow("delegate")]
        [TestMethod]
        public void TestAddDelegateChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDelegate().GetChoices(detailCi);
            Assert.AreEqual<int>(1, actual.Count);
        }

        [DataRow("del")]
        [DataRow("delegate")]
        [TestMethod]
        public void TestAddDelegateEventCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDelegate().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Event) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow("delegate", CompletionItemKind.Property, "delegate01")]
        [DataRow("del", CompletionItemKind.Property, "delegate02")]
        [DataRow("delegate", CompletionItemKind.Keyword, "zzzdelegate01")]
        [DataRow("del", CompletionItemKind.Keyword, "zzzzdelegate02")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDelegate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow("del")]
        [DataRow("delegate")]
        [TestMethod]
        public void TestInsertTextEndsWithSpace(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDelegate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }

        [DataRow("del")]
        [DataRow("delegate")]
        [TestMethod]
        public void TestAddDelegateChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDelegate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("del")]
        [DataRow("delegate")]
        [TestMethod]
        public void TestAddDelegateChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDelegate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow("del")]
        [DataRow("delegate")]
        [TestMethod]
        public void TestAddDelegateChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDelegate().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
