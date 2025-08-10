using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;
namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemIsOperator
    {

        [DataRow(literalIs)]
        [TestMethod]
        public void TestAddIsOperatorChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            Assert.AreEqual<int>(2, actual.Count);
        }

        [DataRow(literalIs)]
        [TestMethod]
        public void TestAddIsOperatorKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(literalIs, CompletionItemKind.Property, literalIs)]
        [DataRow(literalIs, CompletionItemKind.Keyword, "zzzis")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(literalIs)]
        [TestMethod]
        public void TestAddIsOperatorChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(literalIs)]
        [TestMethod]
        public void TestAddIsOperatorChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow(literalIs)]
        [TestMethod]
        public void TestAddIsOperatorChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
