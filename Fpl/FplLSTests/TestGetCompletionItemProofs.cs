using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemProofs
    {

        [DataRow("prf")]
        [DataRow("proof")]
        [TestMethod]
        public void TestAddProofChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            Assert.AreEqual<int>(15, actual.Count);
        }

        [DataRow("prf")]
        [DataRow("proof")]
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

        [DataRow("proof", CompletionItemKind.Property, "proof01")]
        [DataRow("prf", CompletionItemKind.Property, "proof02")]
        [DataRow("proof", CompletionItemKind.Keyword, "zzzproof01")]
        [DataRow("prf", CompletionItemKind.Keyword, "zzzzproof02")]
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

        [DataRow("proof")]
        [DataRow("prf")]
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

        [DataRow("prf")]
        [DataRow("proof")]
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

        [DataRow("prf", "proof")]
        [DataRow("proof", "proof")]
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

        [DataRow("prf")]
        [DataRow("proof")]
        [TestMethod]
        public void TestAddProofChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProof().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
