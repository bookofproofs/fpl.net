// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;
using static Fpl.Parser.Main;

namespace TestFplLS
{
    [TestClass]
    public class TestGetCompletionItemMapCases
    {

        [DataRow(LiteralMapCases)]
        [TestMethod]
        public void TestAddMapCasesChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesMapCases().GetChoices(detailCi);
            Assert.HasCount(2, actual);
        }

        [DataRow(LiteralMapCases)]
        [TestMethod]
        public void TestAddMapCasesKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesMapCases().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralMapCases)]
        [TestMethod]
        public void TestAddMapChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesMapCases().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.Contains(LiteralCases, item.SortText ?? string.Empty);
            }
        }

        [DataRow(LiteralMapCases)]
        [TestMethod]
        public void TestAddMapCasesChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesMapCases().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(LiteralMapCases)]
        [TestMethod]
        public void TestAddMapCasesChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesMapCases().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.Contains(choice, item.Detail ?? string.Empty);
            }
        }

        [DataRow(LiteralMapCases)]
        [TestMethod]
        public void TestAddMapCasesChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesMapCases().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(choice)) { counterSnippets++; }
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(' '))
                {
                    var res = testParser(LiteralMapCases, item.InsertText);
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
