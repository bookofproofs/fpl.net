using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;
namespace FplLSTests
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
            Assert.AreEqual<int>(4, actual.Count);
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

        [DataRow(LiteralUses, "...", CompletionItemKind.Property, "uses01")]
        [DataRow(LiteralUses, "... alias", CompletionItemKind.Property, "uses02")]
        [DataRow(LiteralUses, "", CompletionItemKind.Keyword, "uses03")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string l, CompletionItemKind kind, string expected)
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
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
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
                Assert.IsTrue(item.Detail.Contains(choice));
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
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
