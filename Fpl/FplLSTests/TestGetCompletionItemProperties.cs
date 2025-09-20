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
            Assert.AreEqual<int>(8, actual.Count);
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
            Assert.AreEqual<int>(4, count);
        }

        [DataRow(LiteralPrtyL, LiteralPredL, CompletionItemKind.Value, false, "property02")]
        [DataRow(LiteralPrtyL, LiteralFuncL, CompletionItemKind.Value, false, "property03")]
        [DataRow(LiteralPrtyL, LiteralPredL, CompletionItemKind.Value, true, "property05")]
        [DataRow(LiteralPrtyL, LiteralFuncL, CompletionItemKind.Value, true, "property06")]
        [DataRow(LiteralPrty, LiteralPred, CompletionItemKind.Value, false, "zproperty02")]
        [DataRow(LiteralPrty, LiteralFunc, CompletionItemKind.Value, false, "zproperty03")]
        [DataRow(LiteralPrty, LiteralPred, CompletionItemKind.Value, true, "zproperty05")]
        [DataRow(LiteralPrty, LiteralFunc, CompletionItemKind.Value, true, "zproperty06")]

        [DataRow(LiteralPrtyL, LiteralPredL, CompletionItemKind.Keyword, false, "zzzproperty02")]
        [DataRow(LiteralPrtyL, LiteralFuncL, CompletionItemKind.Keyword, false, "zzzproperty03")]
        [DataRow(LiteralPrtyL, LiteralPredL, CompletionItemKind.Keyword, true, "zzzproperty05")]
        [DataRow(LiteralPrtyL, LiteralFuncL, CompletionItemKind.Keyword, true, "zzzproperty06")]
        [DataRow(LiteralPrty, LiteralPred, CompletionItemKind.Keyword, false, "zzzzproperty02")]
        [DataRow(LiteralPrty, LiteralFunc, CompletionItemKind.Keyword, false, "zzzzproperty03")]
        [DataRow(LiteralPrty, LiteralPred, CompletionItemKind.Keyword, true, "zzzzproperty05")]
        [DataRow(LiteralPrty, LiteralFunc, CompletionItemKind.Keyword, true, "zzzzproperty06")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string subType, CompletionItemKind isKeyword, bool isOptional, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind == isKeyword && isOptional == (item.Label.Contains(LiteralOpt)))
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
            var counterRelatedOpt = 0;
            foreach (var item in actual)
            {
                string postfix = "";
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    postfix = " ...";
                }

                if (item.Label.Contains(subType) && item.Label.Contains(LiteralOpt))
                {
                    if (choice == LiteralPrty)
                    {
                        Assert.AreEqual<string>("_ opt " + choice + " " + subType + postfix, item.Label);
                    }
                    else
                    {
                        Assert.AreEqual<string>("_ optional " + choice + " " + subType + postfix, item.Label);
                    }
                    counterRelatedOpt++;
                }
                else if (item.Label.Contains(subType))
                {
                    Assert.AreEqual<string>("_ " + choice + " " + subType + postfix, item.Label);
                    counterRelated++;
                }
            }
            Assert.AreEqual<int>(2, counterRelated);
            Assert.AreEqual<int>(2, counterRelatedOpt);
        }

        [DataRow(LiteralPrty, "pr")]
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
            Assert.AreEqual<int>(2, countPredicative);
            Assert.AreEqual<int>(2, countFunctional);

        }

        [DataRow(LiteralPrty, "pr")]
        [DataRow(LiteralPrtyL, LiteralPrtyL)]
        [TestMethod]
        public void TestAddPropertyChoicesDetailNonKeyword(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var countOptional = 0;
            var countPredicative = 0;
            var countFunctional = 0;
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(l));
                    if (item.Detail.Contains(LiteralOpt)) countOptional++;
                    if (item.Detail.Contains(LiteralPred)) countPredicative++;
                    if (item.Detail.Contains(LiteralFunc)) countFunctional++;
                }
            }
            Assert.AreEqual<int>(2, countOptional);
            Assert.AreEqual<int>(2, countPredicative);
            Assert.AreEqual<int>(2, countFunctional);

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
            Assert.AreEqual<int>(4, counterSnippets);
        }

    }
}
