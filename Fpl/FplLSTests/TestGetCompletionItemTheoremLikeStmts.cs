using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemTheoremLikeStmts
    {

        [DataRow("inf", "Inference")]
        [DataRow("inference", "Inference")]
        [DataRow("thm", "Theorem")]
        [DataRow("theorem", "Theorem")]
        [DataRow("lem", "Lemma")]
        [DataRow("lemma", "Lemma")]
        [DataRow("conj", "Conjecture")]
        [DataRow("conjecture", "Conjecture")]
        [DataRow("prop", "Proposition")]
        [DataRow("proposition", "Proposition")]
        [TestMethod]
        public void TestAddChoicesNumber(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);

            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("inf", "Inference")]
        [DataRow("inference", "Inference")]
        [DataRow("thm", "Theorem")]
        [DataRow("theorem", "Theorem")]
        [DataRow("lem", "Lemma")]
        [DataRow("lemma", "Lemma")]
        [DataRow("conj", "Conjecture")]
        [DataRow("conjecture", "Conjecture")]
        [DataRow("prop", "Proposition")]
        [DataRow("proposition", "Proposition")]
        [TestMethod]
        public void TestAddKeywordCounts(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("inf", "Inference")]
        [DataRow("inference", "Inference")]
        [DataRow("thm", "Theorem")]
        [DataRow("theorem", "Theorem")]
        [DataRow("lem", "Lemma")]
        [DataRow("lemma", "Lemma")]
        [DataRow("conj", "Conjecture")]
        [DataRow("conjecture", "Conjecture")]
        [DataRow("prop", "Proposition")]
        [DataRow("proposition", "Proposition")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(l.ToLower()));
            }
        }

        [DataRow("inf", "Inference")]
        [DataRow("inference", "Inference")]
        [DataRow("thm", "Theorem")]
        [DataRow("theorem", "Theorem")]
        [DataRow("lem", "Lemma")]
        [DataRow("lemma", "Lemma")]
        [DataRow("conj", "Conjecture")]
        [DataRow("conjecture", "Conjecture")]
        [DataRow("prop", "Proposition")]
        [DataRow("proposition", "Proposition")]
        [TestMethod]
        public void TestAddChoicesLabel(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("inf", "Inference")]
        [DataRow("inference", "Inference")]
        [DataRow("thm", "Theorem")]
        [DataRow("theorem", "Theorem")]
        [DataRow("lem", "Lemma")]
        [DataRow("lemma", "Lemma")]
        [DataRow("conj", "Conjecture")]
        [DataRow("conjecture", "Conjecture")]
        [DataRow("prop", "Proposition")]
        [DataRow("proposition", "Proposition")]
        [TestMethod]
        public void TestAddChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(choice));
                }
                else
                {
                    Assert.IsTrue(item.Detail.Contains(l.ToLower()));
                }
            }
        }

        [DataRow("inf", "Inference")]
        [DataRow("inference", "Inference")]
        [DataRow("thm", "Theorem")]
        [DataRow("theorem", "Theorem")]
        [DataRow("lem", "Lemma")]
        [DataRow("lemma", "Lemma")]
        [DataRow("conj", "Conjecture")]
        [DataRow("conjecture", "Conjecture")]
        [DataRow("prop", "Proposition")]
        [DataRow("proposition", "Proposition")]
        [TestMethod]
        public void TestAddChoicesInsertText(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
