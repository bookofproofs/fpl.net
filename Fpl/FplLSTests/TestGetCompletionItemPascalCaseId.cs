using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemPascalCaseId
    {

        [DataRow("PascalCaseId")]
        [TestMethod]
        public void TestAddPascalCaseIdChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            Assert.AreEqual(1, actual.Count);
        }

        [DataRow("PascalCaseId")]
        [TestMethod]
        public void TestAddPascalCaseIdReferenceCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Reference) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("PascalCaseId", CompletionItemKind.Reference, "PascalCaseId")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Kind == kind)
                {
                    Assert.AreEqual(expected, item.SortText);
                }
            }
        }

        [DataRow("PascalCaseId")]
        [TestMethod]
        public void TestInsertTextEndsWithSpace(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }

        [DataRow("PascalCaseId")]
        [TestMethod]
        public void TestAddPascalCaseIdChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("PascalCaseId", "user-defined id")]
        [TestMethod]
        public void TestAddPascalCaseIdChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual(l, item.Detail);
            }
        }

        [DataRow("PascalCaseId")]
        [TestMethod]
        public void TestAddPascalCaseIdChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
