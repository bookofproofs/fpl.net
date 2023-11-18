using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Data;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDeclaration
    {

        [DataRow("dec")]
        [DataRow("declaration")]
        [TestMethod]
        public void TestAddDeclarationChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("dec")]
        [DataRow("declaration")]
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
            Assert.AreEqual(1, count);
        }

        [DataRow("declaration", CompletionItemKind.Property, "declaration01")]
        [DataRow("dec", CompletionItemKind.Property, "declaration02")]
        [DataRow("declaration", CompletionItemKind.Keyword, "zzzdeclaration01")]
        [DataRow("dec", CompletionItemKind.Keyword, "zzzzdeclaration02")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual(expected, item.SortText);
                }
            }
        }

        [DataRow("declaration")]
        [DataRow("dec")]
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

        [DataRow("dec")]
        [DataRow("declaration")]
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

        [DataRow("dec")]
        [DataRow("declaration")]
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

        [DataRow("dec")]
        [DataRow("declaration")]
        [TestMethod]
        public void TestAddDeclarationChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDeclaration().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
