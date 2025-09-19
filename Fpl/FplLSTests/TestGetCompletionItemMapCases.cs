// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
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
            Assert.AreEqual<int>(2, actual.Count);
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
                Assert.IsTrue(item.SortText.Contains(LiteralCases));
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
                Assert.IsTrue(item.Detail.Contains(choice));
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
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
                if (item.InsertText.Contains(" "))
                {
                    var res = FplParser.testParser(LiteralMapCases, item.InsertText);
                    if (!res.StartsWith("Success:"))
                    {
                        Assert.IsTrue(false, res);
                    }
                }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
