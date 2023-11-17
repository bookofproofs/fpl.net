using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FParsec.ErrorMessage;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemPredicate
    {
        [DataRow("true", 1)]
        [DataRow("false", 1)]
        [DataRow("undef", 1)]
        [DataRow("undefined", 1)]
        [DataRow("not", 2)]
        [DataRow("xor", 2)]
        [DataRow("iif", 2)]
        [DataRow("impl", 2)]
        [DataRow("and", 2)]
        [DataRow("or", 2)]
        [DataRow("<", 1)]
        [TestMethod]
        public void TestAddPredicateChoicesNumber(string choice, int expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            Assert.AreEqual(expected, actual.Count);
        }

        [DataRow("true")]
        [DataRow("false")]
        [DataRow("undef")]
        [DataRow("undefined")]
        [DataRow("not")]
        [DataRow("xor")]
        [DataRow("iif")]
        [DataRow("impl")]
        [DataRow("and")]
        [DataRow("or")]
        [DataRow("<")]
        [TestMethod]
        public void TestAddPredicateKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            var count = 0;
            int expected = 1;
            foreach (var item in actual)
            {
                if (choice == "<")
                {
                    expected = 0;
                }
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(expected, count);
        }

        [DataRow("true", CompletionItemKind.Keyword, "zzztrue")]
        [DataRow("false", CompletionItemKind.Keyword, "zzzfalse")]
        [DataRow("undefined", CompletionItemKind.Keyword, "zzzundefined01")]
        [DataRow("undef", CompletionItemKind.Keyword, "zzzzundefined02")]
        [DataRow("not", CompletionItemKind.Operator, "not")]
        [DataRow("not", CompletionItemKind.Keyword, "zzznot")]
        [DataRow("xor", CompletionItemKind.Operator, "xor")]
        [DataRow("xor", CompletionItemKind.Keyword, "zzzxor")]
        [DataRow("iif", CompletionItemKind.Operator, "iif")]
        [DataRow("iif", CompletionItemKind.Keyword, "zzziif")]
        [DataRow("impl", CompletionItemKind.Operator, "impl")]
        [DataRow("impl", CompletionItemKind.Keyword, "zzzimpl")]
        [DataRow("and", CompletionItemKind.Operator, "and")]
        [DataRow("and", CompletionItemKind.Keyword, "zzzand")]
        [DataRow("or", CompletionItemKind.Operator, "or")]
        [DataRow("or", CompletionItemKind.Keyword, "zzzor")]
        [DataRow("<", CompletionItemKind.Operator, "<")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind isKeyword, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains("_ " + choice) && item.Kind == isKeyword)
                {
                    Assert.AreEqual(expected, item.SortText);
                }
            }
        }

        [DataRow("true")]
        [DataRow("false")]
        [DataRow("undef")]
        [DataRow("undefined")]
        [DataRow("not")]
        [DataRow("xor")]
        [DataRow("iif")]
        [DataRow("impl")]
        [DataRow("and")]
        [DataRow("or")]
        [DataRow("<")]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    if (choice == "<")
                    {
                        Assert.IsTrue(item.InsertText.EndsWith(" "));
                    }
                    else
                    {
                        Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine + Environment.NewLine));
                    }
                }
            }
        }

        [DataRow("true")]
        [DataRow("false")]
        [DataRow("undef")]
        [DataRow("undefined")]
        [DataRow("not")]
        [DataRow("xor")]
        [DataRow("iif")]
        [DataRow("impl")]
        [DataRow("and")]
        [DataRow("or")]
        [DataRow("<")]
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

        [DataRow("true", "predicate (true)")]
        [DataRow("false", "predicate (false)")]
        [DataRow("undef", "undefined (short form)")]
        [DataRow("undefined", "undefined")]
        [DataRow("not", "predicate (negation)")]
        [DataRow("xor", "predicate (exclusive or)")]
        [DataRow("iif", "predicate (equivalence, <=>)")]
        [DataRow("impl", "predicate (implication, =>)")]
        [DataRow("and", "predicate (conjunction)")]
        [DataRow("or", "predicate (disjunction)")]
        [DataRow("<", "equality")]
        [TestMethod]
        public void TestAddPredicateChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPredicate().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.AreEqual(l, item.Detail);
                }
            }
        }

        [DataRow("true")]
        [DataRow("false")]
        [DataRow("undef")]
        [DataRow("undefined")]
        [DataRow("not")]
        [DataRow("xor")]
        [DataRow("iif")]
        [DataRow("impl")]
        [DataRow("and")]
        [DataRow("or")]
        [DataRow("<")]
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
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
