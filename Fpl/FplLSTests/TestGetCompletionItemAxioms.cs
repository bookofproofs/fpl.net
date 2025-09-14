// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemAxioms
    {

        [DataRow(literalAx)]
        [DataRow(literalAxL)]
        [DataRow(literalPost)]
        [DataRow(literalPostL)]
        [TestMethod]
        public void TestAddAxiomChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            Assert.AreEqual<int>(2, actual.Count);
        }

        [DataRow(literalAx)]
        [DataRow(literalAxL)]
        [DataRow(literalPost)]
        [DataRow(literalPostL)]
        [TestMethod]
        public void TestAddAxiomKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(literalAx)]
        [DataRow(literalAxL)]
        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(literalAxL));
            }
        }

        [DataRow(literalPost)]
        [DataRow(literalPostL)]
        [TestMethod]
        public void TestAddPostulateChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(literalPostL));
            }
        }

        [DataRow(literalAx)]
        [DataRow(literalAxL)]
        [DataRow(literalPost)]
        [DataRow(literalPostL)]
        [TestMethod]
        public void TestAddAxiomChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(literalAx)]
        [DataRow(literalAxL)]
        [DataRow(literalPost)]
        [DataRow(literalPostL)]
        [TestMethod]
        public void TestAddAxiomChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow(literalAx)]
        [DataRow(literalAxL)]
        [DataRow(literalPost)]
        [DataRow(literalPostL)]
        [TestMethod]
        public void TestAddAxiomChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }

        [DataRow(literalAx)]
        [DataRow(literalAxL)]
        [DataRow(literalPost)]
        [DataRow(literalPostL)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind!=CompletionItemKind.Keyword && item.InsertText.Contains(choice)) 
                { 
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }
    }
}
