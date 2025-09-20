using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemCorollary
    {

        [DataRow(LiteralCorL)]
        [DataRow(LiteralCor)]
        [TestMethod]
        public void TestAddCorollaryChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            Assert.AreEqual<int>(2, actual.Count);
        }

        [DataRow(LiteralCorL)]
        [DataRow(LiteralCor)]
        [TestMethod]
        public void TestAddCorollaryKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralCorL, CompletionItemKind.Property, "corollary01")]
        [DataRow(LiteralCor, CompletionItemKind.Property, "corollary02")]
        [DataRow(LiteralCorL, CompletionItemKind.Keyword, "zzzcorollary01")]
        [DataRow(LiteralCor, CompletionItemKind.Keyword, "zzzzcorollary02")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(LiteralCorL)]
        [DataRow(LiteralCor)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }

        [DataRow(LiteralCorL)]
        [DataRow(LiteralCor)]
        [TestMethod]
        public void TestAddCorollaryChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(LiteralCorL)]
        [DataRow(LiteralCor)]
        [TestMethod]
        public void TestAddCorollaryChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow(LiteralCorL)]
        [DataRow(LiteralCor)]
        [TestMethod]
        public void TestAddCorollaryChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesCorollary().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
                if (item.InsertText.Contains(" "))
                {
                    var res = FplParser.testParser(LiteralCor, item.InsertText);
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
