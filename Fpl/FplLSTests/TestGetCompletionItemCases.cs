// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;
using static Fpl.Parser.Main;
namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemCases
    {

        [DataRow(LiteralCases)]
        [TestMethod]
        public void TestAddCasesChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCases().GetChoices(detailCi);
            Assert.AreEqual<int>(2, actual.Count);
        }

        [DataRow(LiteralCases)]
        [TestMethod]
        public void TestAddCasesKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCases().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralCases)]
        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCases().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText?.Contains(LiteralCases));
            }
        }

        [DataRow(LiteralCases)]
        [TestMethod]
        public void TestAddCasesChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCases().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(LiteralCases)]
        [TestMethod]
        public void TestAddCasesChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCases().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.Contains(choice, item.Detail ?? string.Empty);
            }
        }

        [DataRow(LiteralCases)]
        [TestMethod]
        public void TestAddCasesChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCases().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(choice)) { counterSnippets++; }
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(' '))
                {
                    var res = testParser(LiteralCases, item.InsertText);
                    if (!res.StartsWith("Success:"))
                    {
                        Assert.Fail(res);
                    }
                }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }

    }
}
