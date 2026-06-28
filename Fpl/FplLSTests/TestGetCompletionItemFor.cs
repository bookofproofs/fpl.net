using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;
using static Fpl.Parser.Main;
namespace TestFplLS
{
    [TestClass]
    public class TestGetCompletionItemFor
    {

        [DataRow(LiteralFor)]
        [TestMethod]
        public void TestAddForChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesFor().GetChoices(detailCi);
            Assert.HasCount(4, actual);
        }

        [DataRow(LiteralFor)]
        [TestMethod]
        public void TestAddForKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesFor().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralFor, "range", CompletionItemKind.Property, "for01")]
        [DataRow(LiteralFor, "list", CompletionItemKind.Property, "for02")]
        [DataRow(LiteralFor, "", CompletionItemKind.Keyword, "zzzfor03")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string l, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesFor().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (!string.IsNullOrEmpty(item.InsertText) && item.Label.Contains(choice) && !string.IsNullOrEmpty(item.Detail) && item.Detail.Contains(l) && item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(LiteralFor)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesFor().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && !string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(choice))
                {
                    Assert.EndsWith(Environment.NewLine, item.InsertText);
                }
            }
        }

        [DataRow(LiteralFor)]
        [TestMethod]
        public void TestAddForChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesFor().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(LiteralFor)]
        [TestMethod]
        public void TestAddForChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesFor().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.Contains(choice, item.Detail ?? string.Empty);
            }
        }

        [DataRow(LiteralFor)]
        [TestMethod]
        public void TestAddForChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesFor().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(choice)) { counterSnippets++; }
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(' '))
                {
                    var res = testParser(LiteralFor, item.InsertText);
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
