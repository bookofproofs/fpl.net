// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemAxioms
    {

        [DataRow(LiteralAx)]
        [DataRow(LiteralAxL)]
        [DataRow(LiteralPost)]
        [DataRow(LiteralPostL)]
        [TestMethod]
        public void TestAddAxiomChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            Assert.AreEqual<int>(2, actual.Count);
        }

        [DataRow(LiteralAx)]
        [DataRow(LiteralAxL)]
        [DataRow(LiteralPost)]
        [DataRow(LiteralPostL)]
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

        [DataRow(LiteralAx)]
        [DataRow(LiteralAxL)]
        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(LiteralAxL));
            }
        }

        [DataRow(LiteralPost)]
        [DataRow(LiteralPostL)]
        [TestMethod]
        public void TestAddPostulateChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(LiteralPostL));
            }
        }

        [DataRow(LiteralAx)]
        [DataRow(LiteralAxL)]
        [DataRow(LiteralPost)]
        [DataRow(LiteralPostL)]
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

        [DataRow(LiteralAx)]
        [DataRow(LiteralAxL)]
        [DataRow(LiteralPost)]
        [DataRow(LiteralPostL)]
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

        [DataRow(LiteralAx)]
        [DataRow(LiteralAxL)]
        [DataRow(LiteralPost)]
        [DataRow(LiteralPostL)]
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

        [DataRow(LiteralAx)]
        [DataRow(LiteralAxL)]
        [DataRow(LiteralPost)]
        [DataRow(LiteralPostL)]
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
