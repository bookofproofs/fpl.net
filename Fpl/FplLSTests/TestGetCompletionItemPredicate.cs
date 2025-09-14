// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemPredicate
    {
        [DataRow(literalTrue, 1)]
        [DataRow(literalFalse, 1)]
        [DataRow(literalUndef, 1)]
        [DataRow(literalUndefL, 1)]
        [DataRow(literalNot, 2)]
        [DataRow(literalXor, 2)]
        [DataRow(literalIif, 2)]
        [DataRow(literalImpl, 2)]
        [DataRow(literalAnd, 2)]
        [DataRow(literalOr, 2)]
        [DataRow("(", 1)]
        [TestMethod]
        public void TestAddPredicateChoicesNumber(string choice, int expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            Assert.AreEqual<int>(expected, actual.Count);
        }

        [DataRow(literalTrue)]
        [DataRow(literalFalse)]
        [DataRow(literalUndef)]
        [DataRow(literalUndefL)]
        [DataRow(literalNot)]
        [DataRow(literalXor)]
        [DataRow(literalIif)]
        [DataRow(literalImpl)]
        [DataRow(literalAnd)]
        [DataRow(literalOr)]
        [DataRow("(")]
        [TestMethod]
        public void TestAddPredicateKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            var count = 0;
            int expected = 1;
            foreach (var item in actual)
            {
                if (choice == "(")
                {
                    expected = 0;
                }
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(expected, count);
        }

        [DataRow(literalTrue, CompletionItemKind.Keyword, "zzztrue")]
        [DataRow(literalFalse, CompletionItemKind.Keyword, "zzzfalse")]
        [DataRow(literalUndefL, CompletionItemKind.Keyword, "zzzundefined01")]
        [DataRow(literalUndef, CompletionItemKind.Keyword, "zzzzundefined02")]
        [DataRow(literalNot, CompletionItemKind.Operator, literalNot)]
        [DataRow(literalNot, CompletionItemKind.Keyword, "zzznot")]
        [DataRow(literalXor, CompletionItemKind.Operator, literalXor)]
        [DataRow(literalXor, CompletionItemKind.Keyword, "zzzxor")]
        [DataRow(literalIif, CompletionItemKind.Operator, literalIif)]
        [DataRow(literalIif, CompletionItemKind.Keyword, "zzziif")]
        [DataRow(literalImpl, CompletionItemKind.Operator, literalImpl)]
        [DataRow(literalImpl, CompletionItemKind.Keyword, "zzzimpl")]
        [DataRow(literalAnd, CompletionItemKind.Operator, literalAnd)]
        [DataRow(literalAnd, CompletionItemKind.Keyword, "zzzand")]
        [DataRow(literalOr, CompletionItemKind.Operator, literalOr)]
        [DataRow(literalOr, CompletionItemKind.Keyword, "zzzor")]
        [DataRow("(", CompletionItemKind.Operator, "(")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind isKeyword, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains("_ " + choice) && item.Kind == isKeyword)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(literalTrue)]
        [DataRow(literalFalse)]
        [DataRow(literalUndef)]
        [DataRow(literalUndefL)]
        [DataRow(literalNot)]
        [DataRow(literalXor)]
        [DataRow(literalIif)]
        [DataRow(literalImpl)]
        [DataRow(literalAnd)]
        [DataRow(literalOr)]
        [DataRow("(")]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }

        [DataRow(literalTrue)]
        [DataRow(literalFalse)]
        [DataRow(literalUndef)]
        [DataRow(literalUndefL)]
        [DataRow(literalNot)]
        [DataRow(literalXor)]
        [DataRow(literalIif)]
        [DataRow(literalImpl)]
        [DataRow(literalAnd)]
        [DataRow(literalOr)]
        [DataRow("(")]
        [TestMethod]
        public void TestAddPredicateChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(literalTrue, "predicate (true)")]
        [DataRow(literalFalse, "predicate (false)")]
        [DataRow(literalUndef, "undefined (short form)")]
        [DataRow(literalUndefL, literalUndefL)]
        [DataRow(literalNot, "predicate (negation)")]
        [DataRow(literalXor, "predicate (exclusive or)")]
        [DataRow(literalIif, "predicate (equivalence, <=>)")]
        [DataRow(literalImpl, "predicate (implication, =>)")]
        [DataRow(literalAnd, "predicate (conjunction)")]
        [DataRow(literalOr, "predicate (disjunction)")]
        [DataRow("(", PrimEqualityL)]
        [TestMethod]
        public void TestAddPredicateChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.AreEqual<string>(l, item.Detail);
                }
            }
        }

        [DataRow(literalTrue)]
        [DataRow(literalFalse)]
        [DataRow(literalUndef)]
        [DataRow(literalUndefL)]
        [DataRow(literalNot)]
        [DataRow(literalXor)]
        [DataRow(literalIif)]
        [DataRow(literalImpl)]
        [DataRow(literalAnd)]
        [DataRow(literalOr)]
        [DataRow("(")]
        [TestMethod]
        public void TestAddPredicateChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
