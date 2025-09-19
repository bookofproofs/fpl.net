// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemPredicate
    {
        [DataRow(LiteralTrue, 1)]
        [DataRow(LiteralFalse, 1)]
        [DataRow(LiteralUndef, 1)]
        [DataRow(LiteralUndefL, 1)]
        [DataRow(LiteralNot, 2)]
        [DataRow(LiteralXor, 2)]
        [DataRow(LiteralIif, 2)]
        [DataRow(LiteralImpl, 2)]
        [DataRow(LiteralAnd, 2)]
        [DataRow(LiteralOr, 2)]
        [DataRow("(", 1)]
        [TestMethod]
        public void TestAddPredicateChoicesNumber(string choice, int expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            Assert.AreEqual<int>(expected, actual.Count);
        }

        [DataRow(LiteralTrue)]
        [DataRow(LiteralFalse)]
        [DataRow(LiteralUndef)]
        [DataRow(LiteralUndefL)]
        [DataRow(LiteralNot)]
        [DataRow(LiteralXor)]
        [DataRow(LiteralIif)]
        [DataRow(LiteralImpl)]
        [DataRow(LiteralAnd)]
        [DataRow(LiteralOr)]
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

        [DataRow(LiteralTrue, CompletionItemKind.Keyword, "zzztrue")]
        [DataRow(LiteralFalse, CompletionItemKind.Keyword, "zzzfalse")]
        [DataRow(LiteralUndefL, CompletionItemKind.Keyword, "zzzundefined01")]
        [DataRow(LiteralUndef, CompletionItemKind.Keyword, "zzzzundefined02")]
        [DataRow(LiteralNot, CompletionItemKind.Operator, LiteralNot)]
        [DataRow(LiteralNot, CompletionItemKind.Keyword, "zzznot")]
        [DataRow(LiteralXor, CompletionItemKind.Operator, LiteralXor)]
        [DataRow(LiteralXor, CompletionItemKind.Keyword, "zzzxor")]
        [DataRow(LiteralIif, CompletionItemKind.Operator, LiteralIif)]
        [DataRow(LiteralIif, CompletionItemKind.Keyword, "zzziif")]
        [DataRow(LiteralImpl, CompletionItemKind.Operator, LiteralImpl)]
        [DataRow(LiteralImpl, CompletionItemKind.Keyword, "zzzimpl")]
        [DataRow(LiteralAnd, CompletionItemKind.Operator, LiteralAnd)]
        [DataRow(LiteralAnd, CompletionItemKind.Keyword, "zzzand")]
        [DataRow(LiteralOr, CompletionItemKind.Operator, LiteralOr)]
        [DataRow(LiteralOr, CompletionItemKind.Keyword, "zzzor")]
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

        [DataRow(LiteralTrue)]
        [DataRow(LiteralFalse)]
        [DataRow(LiteralUndef)]
        [DataRow(LiteralUndefL)]
        [DataRow(LiteralNot)]
        [DataRow(LiteralXor)]
        [DataRow(LiteralIif)]
        [DataRow(LiteralImpl)]
        [DataRow(LiteralAnd)]
        [DataRow(LiteralOr)]
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

        [DataRow(LiteralTrue)]
        [DataRow(LiteralFalse)]
        [DataRow(LiteralUndef)]
        [DataRow(LiteralUndefL)]
        [DataRow(LiteralNot)]
        [DataRow(LiteralXor)]
        [DataRow(LiteralIif)]
        [DataRow(LiteralImpl)]
        [DataRow(LiteralAnd)]
        [DataRow(LiteralOr)]
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

        [DataRow(LiteralTrue, "predicate (true)")]
        [DataRow(LiteralFalse, "predicate (false)")]
        [DataRow(LiteralUndef, "undefined (short form)")]
        [DataRow(LiteralUndefL, LiteralUndefL)]
        [DataRow(LiteralNot, "predicate (negation)")]
        [DataRow(LiteralXor, "predicate (exclusive or)")]
        [DataRow(LiteralIif, "predicate (equivalence, <=>)")]
        [DataRow(LiteralImpl, "predicate (implication, =>)")]
        [DataRow(LiteralAnd, "predicate (conjunction)")]
        [DataRow(LiteralOr, "predicate (disjunction)")]
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

        [DataRow(LiteralTrue)]
        [DataRow(LiteralFalse)]
        [DataRow(LiteralUndef)]
        [DataRow(LiteralUndefL)]
        [DataRow(LiteralNot)]
        [DataRow(LiteralXor)]
        [DataRow(LiteralIif)]
        [DataRow(LiteralImpl)]
        [DataRow(LiteralAnd)]
        [DataRow(LiteralOr)]
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
