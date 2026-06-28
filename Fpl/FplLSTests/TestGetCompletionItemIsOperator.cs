using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;
using static Fpl.Parser.Main;
namespace TestFplLS
{
    [TestClass]
    public class TestGetCompletionItemIsOperator
    {

        [DataRow(LiteralIs)]
        [TestMethod]
        public void TestAddIsOperatorChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            Assert.HasCount(2, actual);
        }

        [DataRow(LiteralIs)]
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

        [DataRow(LiteralIs, CompletionItemKind.Property, LiteralIs)]
        [DataRow(LiteralIs, CompletionItemKind.Keyword, "zzzis")]
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

        [DataRow(LiteralIs)]
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

        [DataRow(LiteralIs)]
        [TestMethod]
        public void TestAddIsOperatorChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(!string.IsNullOrEmpty(item.InsertText) && !string.IsNullOrEmpty(item.Detail) && item.Detail.Contains(choice));
            }
        }

        [DataRow(LiteralIs)]
        [TestMethod]
        public void TestAddIsOperatorChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIsOperator().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(choice)) { counterSnippets++; }
                if (!string.IsNullOrEmpty(item.InsertText) && item.InsertText.Contains(' '))
                {
                    var res = testParser(LiteralIs, item.InsertText);
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
