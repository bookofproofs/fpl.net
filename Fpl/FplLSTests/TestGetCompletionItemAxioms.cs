using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemAxioms
    {

        [DataRow("ax")]
        [DataRow("axiom")]
        [DataRow("post")]
        [DataRow("postulate")]
        [TestMethod]
        public void TestAddAxiomChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("ax")]
        [DataRow("axiom")]
        [DataRow("post")]
        [DataRow("postulate")]
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
            Assert.AreEqual(1, count);
        }

        [DataRow("ax")]
        [DataRow("axiom")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("axiom"));
            }
        }

        [DataRow("post")]
        [DataRow("postulate")]
        [TestMethod]
        public void TestAddPostulateChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesAxiom().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("postulate"));
            }
        }

        [DataRow("ax")]
        [DataRow("axiom")]
        [DataRow("post")]
        [DataRow("postulate")]
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

        [DataRow("ax")]
        [DataRow("axiom")]
        [DataRow("post")]
        [DataRow("postulate")]
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

        [DataRow("ax")]
        [DataRow("axiom")]
        [DataRow("post")]
        [DataRow("postulate")]
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
            Assert.AreEqual(actual.Count, counterSnippets);
        }

        [DataRow("ax")]
        [DataRow("axiom")]
        [DataRow("post")]
        [DataRow("postulate")]
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
