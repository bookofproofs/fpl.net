using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemSelf
    {

        [DataRow("self")]
        [DataRow(literalBase)]
        [DataRow(literalParent)]
        [TestMethod]
        public void TestAddSelfChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            Assert.AreEqual<int>(1, actual.Count);
        }

        [DataRow("self")]
        [DataRow(literalBase)]
        [DataRow(literalParent)]
        [TestMethod]
        public void TestAddSelfReferenceCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Reference) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow("self", CompletionItemKind.Reference, "self01")]
        [DataRow(literalParent, CompletionItemKind.Reference, "parent02")]
        [DataRow(literalBase, CompletionItemKind.Reference, "self03")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow("self")]
        [DataRow(literalBase)]
        [DataRow(literalParent)]
        [TestMethod]
        public void TestInsertTextEndsWithSpace(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice) && choice!=literalBase)
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }

        [DataRow("self")]
        [DataRow(literalBase)]
        [DataRow(literalParent)]
        [TestMethod]
        public void TestAddSelfChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("self", "self reference")]
        [DataRow(literalBase, "ctor call (parent class)")]
        [DataRow(literalParent, "parent self reference")]
        [TestMethod]
        public void TestAddSelfChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual<string>(l, item.Detail);
            }
        }

        [DataRow("self")]
        [DataRow(literalBase)]
        [DataRow(literalParent)]
        [TestMethod]
        public void TestAddSelfChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
