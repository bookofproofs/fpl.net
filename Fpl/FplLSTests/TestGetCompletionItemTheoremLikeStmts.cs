using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;
namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemTheoremLikeStmts
    {

        [DataRow(LiteralInf, "Inference")]
        [DataRow(LiteralInfL, "Inference")]
        [DataRow(LiteralThm, "Theorem")]
        [DataRow(LiteralThmL, "Theorem")]
        [DataRow(LiteralLem, "Lemma")]
        [DataRow(LiteralLemL, "Lemma")]
        [DataRow(LiteralConj, "Conjecture")]
        [DataRow(LiteralConjL, "Conjecture")]
        [DataRow(LiteralProp, "Proposition")]
        [DataRow(LiteralPropL, "Proposition")]
        [TestMethod]
        public void TestAddChoicesNumber(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);

            Assert.AreEqual<int>(2, actual.Count);
        }

        [DataRow(LiteralInf, "Inference")]
        [DataRow(LiteralInfL, "Inference")]
        [DataRow(LiteralThm, "Theorem")]
        [DataRow(LiteralThmL, "Theorem")]
        [DataRow(LiteralLem, "Lemma")]
        [DataRow(LiteralLemL, "Lemma")]
        [DataRow(LiteralConj, "Conjecture")]
        [DataRow(LiteralConjL, "Conjecture")]
        [DataRow(LiteralProp, "Proposition")]
        [DataRow(LiteralPropL, "Proposition")]
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

        [DataRow(LiteralInfL, "Inference", CompletionItemKind.Class, "inference01")]
        [DataRow(LiteralInf, "Inference", CompletionItemKind.Class, "inference02")]
        [DataRow(LiteralInfL, "Inference", CompletionItemKind.Keyword, "zzzinference01")]
        [DataRow(LiteralInf, "Inference", CompletionItemKind.Keyword, "zzzzinference02")]
        [DataRow(LiteralThmL, "Theorem", CompletionItemKind.Class, "theorem01")]
        [DataRow(LiteralThm, "Theorem", CompletionItemKind.Class, "theorem02")]
        [DataRow(LiteralThmL, "Theorem", CompletionItemKind.Keyword, "zzztheorem01")]
        [DataRow(LiteralThm, "Theorem", CompletionItemKind.Keyword, "zzzztheorem02")]
        [DataRow(LiteralLemL, "Lemma", CompletionItemKind.Class, "lemma01")]
        [DataRow(LiteralLem, "Lemma", CompletionItemKind.Class, "lemma02")]
        [DataRow(LiteralLemL, "Lemma", CompletionItemKind.Keyword, "zzzlemma01")]
        [DataRow(LiteralLem, "Lemma", CompletionItemKind.Keyword, "zzzzlemma02")]
        [DataRow(LiteralConjL, "Conjecture", CompletionItemKind.Class, "conjecture01")]
        [DataRow(LiteralConj, "Conjecture", CompletionItemKind.Class, "conjecture02")]
        [DataRow(LiteralConjL, "Conjecture", CompletionItemKind.Keyword, "zzzconjecture01")]
        [DataRow(LiteralConj, "Conjecture", CompletionItemKind.Keyword, "zzzzconjecture02")]
        [DataRow(LiteralPropL, "Proposition", CompletionItemKind.Class, "proposition01")]
        [DataRow(LiteralProp, "Proposition", CompletionItemKind.Class, "proposition02")]
        [DataRow(LiteralPropL, "Proposition", CompletionItemKind.Keyword, "zzzproposition01")]
        [DataRow(LiteralProp, "Proposition", CompletionItemKind.Keyword, "zzzzproposition02")]
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

        [DataRow(LiteralInf, "Inference")]
        [DataRow(LiteralInfL, "Inference")]
        [DataRow(LiteralThm, "Theorem")]
        [DataRow(LiteralThmL, "Theorem")]
        [DataRow(LiteralLem, "Lemma")]
        [DataRow(LiteralLemL, "Lemma")]
        [DataRow(LiteralConj, "Conjecture")]
        [DataRow(LiteralConjL, "Conjecture")]
        [DataRow(LiteralProp, "Proposition")]
        [DataRow(LiteralPropL, "Proposition")]
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

        [DataRow(LiteralInf, "Inference")]
        [DataRow(LiteralInfL, "Inference")]
        [DataRow(LiteralThm, "Theorem")]
        [DataRow(LiteralThmL, "Theorem")]
        [DataRow(LiteralLem, "Lemma")]
        [DataRow(LiteralLemL, "Lemma")]
        [DataRow(LiteralConj, "Conjecture")]
        [DataRow(LiteralConjL, "Conjecture")]
        [DataRow(LiteralProp, "Proposition")]
        [DataRow(LiteralPropL, "Proposition")]
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

        [DataRow(LiteralInf, "Inference")]
        [DataRow(LiteralInfL, "Inference")]
        [DataRow(LiteralThm, "Theorem")]
        [DataRow(LiteralThmL, "Theorem")]
        [DataRow(LiteralLem, "Lemma")]
        [DataRow(LiteralLemL, "Lemma")]
        [DataRow(LiteralConj, "Conjecture")]
        [DataRow(LiteralConjL, "Conjecture")]
        [DataRow(LiteralProp, "Proposition")]
        [DataRow(LiteralPropL, "Proposition")]
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

        [DataRow(LiteralInf, "Inference")]
        [DataRow(LiteralInfL, "Inference")]
        [DataRow(LiteralThm, "Theorem")]
        [DataRow(LiteralThmL, "Theorem")]
        [DataRow(LiteralLem, "Lemma")]
        [DataRow(LiteralLemL, "Lemma")]
        [DataRow(LiteralConj, "Conjecture")]
        [DataRow(LiteralConjL, "Conjecture")]
        [DataRow(LiteralProp, "Proposition")]
        [DataRow(LiteralPropL, "Proposition")]
        [TestMethod]
        public void TestAddChoicesInsertText(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesTheoremLikeStmt(l).GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
                if (item.InsertText.Contains("{"))
                {
                    var res = FplParser.testParser(PrimTheoremLike, item.InsertText);
                    if (!res.StartsWith("Success:"))
                    {
                        Assert.IsTrue(false, res);
                    }
                }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
