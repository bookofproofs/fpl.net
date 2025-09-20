using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Data;
using static FplPrimitives;
namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDeclaration
    {

        [DataRow(LiteralDec)]
        [DataRow(LiteralDecL)]
        [TestMethod]
        public void TestAddDeclarationChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            Assert.AreEqual<int>(2, actual.Count);
        }

        [DataRow(LiteralDec)]
        [DataRow(LiteralDecL)]
        [TestMethod]
        public void TestAddDeclarationKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralDecL, CompletionItemKind.Property, "declaration01")]
        [DataRow(LiteralDec, CompletionItemKind.Property, "declaration02")]
        [DataRow(LiteralDecL, CompletionItemKind.Keyword, "zzzdeclaration01")]
        [DataRow(LiteralDec, CompletionItemKind.Keyword, "zzzzdeclaration02")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(LiteralDecL)]
        [DataRow(LiteralDec)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }

        [DataRow(LiteralDec)]
        [DataRow(LiteralDecL)]
        [TestMethod]
        public void TestAddDeclarationChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(LiteralDec)]
        [DataRow(LiteralDecL)]
        [TestMethod]
        public void TestAddDeclarationChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow(LiteralDec)]
        [DataRow(LiteralDecL)]
        [TestMethod]
        public void TestAddDeclarationChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
                if (item.InsertText.Contains(" "))
                {
                    var res = FplParser.testParser(LiteralDec, item.InsertText);
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
