using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemSelf
    {

        [DataRow(LiteralSelf)]
        [DataRow(LiteralBase)]
        [DataRow(LiteralParent)]
        [TestMethod]
        public void TestAddSelfChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            Assert.AreEqual<int>(1, actual.Count);
        }

        [DataRow(LiteralSelf)]
        [DataRow(LiteralBase)]
        [DataRow(LiteralParent)]
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

        [DataRow(LiteralSelf, CompletionItemKind.Reference, "self01")]
        [DataRow(LiteralParent, CompletionItemKind.Reference, "parent02")]
        [DataRow(LiteralBase, CompletionItemKind.Reference, "self03")]
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

        [DataRow(LiteralSelf)]
        [DataRow(LiteralBase)]
        [DataRow(LiteralParent)]
        [TestMethod]
        public void TestInsertTextEndsWithSpace(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSelf().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice) && choice!=LiteralBase)
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }

        [DataRow(LiteralSelf)]
        [DataRow(LiteralBase)]
        [DataRow(LiteralParent)]
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

        [DataRow(LiteralSelf, "self reference")]
        [DataRow(LiteralBase, "ctor call (parent class)")]
        [DataRow(LiteralParent, "parent self reference")]
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

        [DataRow(LiteralSelf)]
        [DataRow(LiteralBase)]
        [DataRow(LiteralParent)]
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
