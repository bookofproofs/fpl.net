// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemProperties
    {

        [DataRow(LiteralPrty)]
        [DataRow(LiteralPrtyL)]
        [TestMethod]
        public void TestAddPropertyChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            Assert.AreEqual<int>(4, actual.Count);
        }

        [DataRow(LiteralPrty)]
        [DataRow(LiteralPrtyL)]
        [TestMethod]
        public void TestAddPropertyKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(2, count);
        }

        [DataRow(LiteralPrtyL, LiteralPredL, CompletionItemKind.Value, "property02")]
        [DataRow(LiteralPrtyL, LiteralFuncL, CompletionItemKind.Value, "property03")]
        [DataRow(LiteralPrty, LiteralPred, CompletionItemKind.Value, "zproperty02")]
        [DataRow(LiteralPrty, LiteralFunc, CompletionItemKind.Value, "zproperty03")]

        [DataRow(LiteralPrtyL, LiteralPredL, CompletionItemKind.Keyword, "zzzproperty02")]
        [DataRow(LiteralPrtyL, LiteralFuncL, CompletionItemKind.Keyword, "zzzproperty03")]
        [DataRow(LiteralPrty, LiteralPred, CompletionItemKind.Keyword, "zzzzproperty02")]
        [DataRow(LiteralPrty, LiteralFunc, CompletionItemKind.Keyword, "zzzzproperty03")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string subType, CompletionItemKind isKeyword, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind == isKeyword)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(LiteralPrty, "pred ")]
        [DataRow(LiteralPrtyL, LiteralPredL)]
        [DataRow(LiteralPrty, "func ")]
        [DataRow(LiteralPrtyL, LiteralFuncL)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice) && item.InsertText.Contains(l))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }


        [DataRow(LiteralPrty, LiteralPred)]
        [DataRow(LiteralPrtyL, LiteralPredL)]
        [DataRow(LiteralPrty, LiteralFunc)]
        [DataRow(LiteralPrtyL, LiteralFuncL)]
        [TestMethod]
        public void TestAddPropertyChoicesLabel(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var counterRelated = 0;
            foreach (var item in actual)
            {
                string postfix = "";
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    postfix = " ...";
                }

                if (item.Label.Contains(subType))
                {
                    Assert.AreEqual<string>("_ " + choice + " " + subType + postfix, item.Label);
                    counterRelated++;
                }
            }
            Assert.AreEqual<int>(2, counterRelated);
        }

        [DataRow(LiteralPrty, LiteralPrty)]
        [DataRow(LiteralPrtyL, LiteralPrtyL)]
        [TestMethod]
        public void TestAddPropertyChoicesDetailKeyword(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var countPredicative = 0;
            var countFunctional = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(l));
                    if (item.Detail.Contains(LiteralPred)) countPredicative++;
                    if (item.Detail.Contains(LiteralFunc)) countFunctional++;
                }
            }
            Assert.AreEqual<int>(1, countPredicative);
            Assert.AreEqual<int>(1, countFunctional);

        }

        [DataRow(LiteralPrty, "pr")]
        [DataRow(LiteralPrtyL, LiteralPrtyL)]
        [TestMethod]
        public void TestAddPropertyChoicesDetailNonKeyword(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var countPredicative = 0;
            var countFunctional = 0;
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(l));
                    if (item.Detail.Contains(LiteralPred)) countPredicative++;
                    if (item.Detail.Contains(LiteralFunc)) countFunctional++;
                }
            }
            Assert.AreEqual<int>(1, countPredicative);
            Assert.AreEqual<int>(1, countFunctional);

        }

        [DataRow(LiteralPrty, LiteralPred)]
        [DataRow(LiteralPrtyL, LiteralPredL)]
        [DataRow(LiteralPrty, LiteralFunc)]
        [DataRow(LiteralPrtyL, LiteralFuncL)]
        [TestMethod]
        public void TestAddPropertyChoicesInsertText(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice) && item.InsertText.Contains(subType)) { counterSnippets++; }
                if (item.InsertText.Contains("{"))
                {
                    var res = FplParser.testParser(LiteralPrty, item.InsertText);
                    if (!res.StartsWith("Success:"))
                    {
                        Assert.IsTrue(false, res);
                    }
                }
            }
            Assert.AreEqual<int>(2, counterSnippets);
        }

    }
}
