using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;


namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemQuantors
    {

        [DataRow(literalAll, 2)]
        [DataRow("ex", 2)]
        [DataRow("exn", 2)]
        [TestMethod]
        public void TestAddQuantorChoicesNumber(string choice, int number)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);
            Assert.AreEqual<int>(number, actual.Count);
        }

        [DataRow(literalAll, 1)]
        [DataRow("ex", 1)]
        [DataRow("exn", 1)]
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

        [DataRow(literalAll, "all ...", CompletionItemKind.Operator, "all01")]
        [DataRow(literalAll, "type ...", CompletionItemKind.Operator, "all02")]
        [DataRow(literalAll, "list ...", CompletionItemKind.Operator, "all03")]
        [DataRow(literalAll, "range ...", CompletionItemKind.Operator, "all04")]
        [DataRow(literalAll, "combined ...", CompletionItemKind.Operator, "all05")]
        [DataRow(literalAll, "", CompletionItemKind.Keyword, "zzzall")]
        [DataRow("ex", "ex ...", CompletionItemKind.Operator, "ex01")]
        [DataRow("ex", "type ...", CompletionItemKind.Operator, "ex02")]
        [DataRow("ex", "list ...", CompletionItemKind.Operator, "ex03")]
        [DataRow("ex", "range ...", CompletionItemKind.Operator, "ex04")]
        [DataRow("ex", "combined ...", CompletionItemKind.Operator, "ex05")]
        [DataRow("ex", "", CompletionItemKind.Keyword, "zzzex")]
        [DataRow("exn", "exn!1 ...", CompletionItemKind.Operator, "exn01")]
        [DataRow("exn", "type ...", CompletionItemKind.Operator, "exn02")]
        [DataRow("exn", "list ...", CompletionItemKind.Operator, "exn03")]
        [DataRow("exn", "range ...", CompletionItemKind.Operator, "exn04")]
        [DataRow("exn", "", CompletionItemKind.Keyword, "zzzexn")]
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

        [DataRow(literalAll)]
        [DataRow("ex")]
        [DataRow("exn")]
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

        [DataRow(literalAll)]
        [DataRow("ex")]
        [DataRow("exn")]
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

        [DataRow(literalAll, literalAll)]
        [DataRow("ex", "exists")]
        [DataRow("exn", "n-times")]
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

        [DataRow(literalAll)]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddQuantorChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesQuantor().GetChoices(detailCi);

            foreach (var item in actual)
            {
                Assert.IsTrue(item.InsertText.Contains(choice));
            }
        }
    }
}
