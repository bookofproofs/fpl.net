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

            Assert.AreEqual<int>(2, actual.Count);
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
            Assert.AreEqual<int>(1, count);
        }

        [DataRow("inference", "Inference", CompletionItemKind.Class, "inference01")]
        [DataRow("inf", "Inference", CompletionItemKind.Class, "inference02")]
        [DataRow("inference", "Inference", CompletionItemKind.Keyword, "zzzinference01")]
        [DataRow("inf", "Inference", CompletionItemKind.Keyword, "zzzzinference02")]
        [DataRow("theorem", "Theorem", CompletionItemKind.Class, "theorem01")]
        [DataRow("thm", "Theorem", CompletionItemKind.Class, "theorem02")]
        [DataRow("theorem", "Theorem", CompletionItemKind.Keyword, "zzztheorem01")]
        [DataRow("thm", "Theorem", CompletionItemKind.Keyword, "zzzztheorem02")]
        [DataRow("lemma", "Lemma", CompletionItemKind.Class, "lemma01")]
        [DataRow("lem", "Lemma", CompletionItemKind.Class, "lemma02")]
        [DataRow("lemma", "Lemma", CompletionItemKind.Keyword, "zzzlemma01")]
        [DataRow("lem", "Lemma", CompletionItemKind.Keyword, "zzzzlemma02")]
        [DataRow("conjecture", "Conjecture", CompletionItemKind.Class, "conjecture01")]
        [DataRow("conj", "Conjecture", CompletionItemKind.Class, "conjecture02")]
        [DataRow("conjecture", "Conjecture", CompletionItemKind.Keyword, "zzzconjecture01")]
        [DataRow("conj", "Conjecture", CompletionItemKind.Keyword, "zzzzconjecture02")]
        [DataRow("proposition", "Proposition", CompletionItemKind.Class, "proposition01")]
        [DataRow("prop", "Proposition", CompletionItemKind.Class, "proposition02")]
        [DataRow("proposition", "Proposition", CompletionItemKind.Keyword, "zzzproposition01")]
        [DataRow("prop", "Proposition", CompletionItemKind.Keyword, "zzzzproposition02")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string l, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
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
        public void TestInsertTextEndsWithTwoNewLines(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
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
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
