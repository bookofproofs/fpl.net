using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;


namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemQuantors
    {

        [DataRow(LiteralAll, 2)]
        [DataRow(LiteralEx, 2)]
        [DataRow(LiteralExN, 2)]
        [TestMethod]
        public void TestAddQuantorChoicesNumber(string choice, int number)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);
            Assert.AreEqual<int>(number, actual.Count);
        }

        [DataRow(LiteralAll, 1)]
        [DataRow(LiteralEx, 1)]
        [DataRow(LiteralExN, 1)]
        [TestMethod]
        public void TestAddQuantorKeywordCounts(string choice, int number)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);

            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(number, count);
        }

        [DataRow(LiteralAll, "all ...", CompletionItemKind.Operator, "all01")]
        [DataRow(LiteralAll, "type ...", CompletionItemKind.Operator, "all02")]
        [DataRow(LiteralAll, "list ...", CompletionItemKind.Operator, "all03")]
        [DataRow(LiteralAll, "range ...", CompletionItemKind.Operator, "all04")]
        [DataRow(LiteralAll, "combined ...", CompletionItemKind.Operator, "all05")]
        [DataRow(LiteralAll, "", CompletionItemKind.Keyword, "zzzall")]
        [DataRow(LiteralEx, "ex ...", CompletionItemKind.Operator, "ex01")]
        [DataRow(LiteralEx, "type ...", CompletionItemKind.Operator, "ex02")]
        [DataRow(LiteralEx, "list ...", CompletionItemKind.Operator, "ex03")]
        [DataRow(LiteralEx, "range ...", CompletionItemKind.Operator, "ex04")]
        [DataRow(LiteralEx, "combined ...", CompletionItemKind.Operator, "ex05")]
        [DataRow(LiteralEx, "", CompletionItemKind.Keyword, "zzzex")]
        [DataRow(LiteralExN, "exn!1 ...", CompletionItemKind.Operator, "exn01")]
        [DataRow(LiteralExN, "type ...", CompletionItemKind.Operator, "exn02")]
        [DataRow(LiteralExN, "list ...", CompletionItemKind.Operator, "exn03")]
        [DataRow(LiteralExN, "range ...", CompletionItemKind.Operator, "exn04")]
        [DataRow(LiteralExN, "", CompletionItemKind.Keyword, "zzzexn")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string subType, CompletionItemKind isKeyword, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind == isKeyword)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(LiteralAll)]
        [DataRow(LiteralEx)]
        [DataRow(LiteralExN)]
        [TestMethod]
        public void TestInsertTextEndsWithSpace(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }

        [DataRow(LiteralAll)]
        [DataRow(LiteralEx)]
        [DataRow(LiteralExN)]
        [TestMethod]
        public void TestAddQuantorChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);

            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice));
            }
        }

        [DataRow(LiteralAll, LiteralAll)]
        [DataRow(LiteralEx, "exists")]
        [DataRow(LiteralExN, "n-times")]
        [TestMethod]
        public void TestAddQuantorChoicesDetail(string choice, string s)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(s));
                }
            }
        }

        [DataRow(LiteralAll)]
        [DataRow(LiteralEx)]
        [DataRow(LiteralExN)]
        [TestMethod]
        public void TestAddQuantorChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);

            foreach (var item in actual)
            {
                Assert.IsTrue(item.InsertText.Contains(choice));
                if (item.InsertText.Contains("{"))
                {
                    var res = FplParser.testParser(PrimQuantor, item.InsertText);
                    if (!res.StartsWith("Success:"))
                    {
                        Assert.IsTrue(false, res);
                    }
                }
            }
        }
    }
}
