using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;
namespace TestFplLS
{
    [TestClass]
    public class TestGetCompletionItemUses
    {

        [DataRow(LiteralUses)]

        [TestMethod]
        public void TestAddUsesChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            Assert.HasCount(4, actual);
        }

        [DataRow(LiteralUses)]
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
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralUses, CompletionItemKind.Property, "uses01")]
        [DataRow(LiteralUses, CompletionItemKind.Property, "uses02")]
        [DataRow(LiteralUses, CompletionItemKind.Keyword, "uses03")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(LiteralUses)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (!string.IsNullOrEmpty(item.InsertText) && item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.EndsWith(Environment.NewLine, item.InsertText);
                }
            }
        }

        [DataRow(LiteralUses)]
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

        [DataRow(LiteralUses)]

        [TestMethod]
        public void TestAddUsesChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.Contains(choice, item.Detail ?? string.Empty);
            }
        }

        [DataRow(LiteralUses)]

        [TestMethod]
        public void TestAddUsesChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesUses().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
