using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;
namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemProofs
    {

        [DataRow(LiteralPrf)]
        [DataRow(LiteralPrfL)]
        [TestMethod]
        public void TestAddProofChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            Assert.AreEqual<int>(15, actual.Count);
        }

        [DataRow(LiteralPrf)]
        [DataRow(LiteralPrfL)]
        [TestMethod]
        public void TestAddProofKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralPrfL, CompletionItemKind.Property, "proof01")]
        [DataRow(LiteralPrf, CompletionItemKind.Property, "proof02")]
        [DataRow(LiteralPrfL, CompletionItemKind.Keyword, "zzzproof01")]
        [DataRow(LiteralPrf, CompletionItemKind.Keyword, "zzzzproof02")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(LiteralPrfL)]
        [DataRow(LiteralPrf)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }

        [DataRow(LiteralPrf)]
        [DataRow(LiteralPrfL)]
        [TestMethod]
        public void TestAddProofChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(LiteralPrf, LiteralPrfL)]
        [DataRow(LiteralPrfL, LiteralPrfL)]
        [TestMethod]
        public void TestAddProofChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(l));
                    if (item.IsShort)
                    {
                        Assert.IsTrue(item.Detail.Contains("(short)"));
                    }
                }
            }
        }

        [DataRow(LiteralPrf)]
        [DataRow(LiteralPrfL)]
        [TestMethod]
        public void TestAddProofChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
                if (item.InsertText.Contains(" "))
                {
                    var res = FplParser.testParser(LiteralPrf, item.InsertText);
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
