// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemProperties
    {

        [DataRow(literalPrty)]
        [DataRow(literalPrtyL)]
        [TestMethod]
        public void TestAddPropertyChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            Assert.AreEqual<int>(8, actual.Count);
        }

        [DataRow(literalPrty)]
        [DataRow(literalPrtyL)]
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

        [DataRow(literalPrtyL, literalPredL, CompletionItemKind.Value, false, "property02")]
        [DataRow(literalPrtyL, literalFuncL, CompletionItemKind.Value, false, "property03")]
        [DataRow(literalPrtyL, literalPredL, CompletionItemKind.Value, true, "property05")]
        [DataRow(literalPrtyL, literalFuncL, CompletionItemKind.Value, true, "property06")]
        [DataRow(literalPrty, literalPred, CompletionItemKind.Value, false, "zproperty02")]
        [DataRow(literalPrty, literalFunc, CompletionItemKind.Value, false, "zproperty03")]
        [DataRow(literalPrty, literalPred, CompletionItemKind.Value, true, "zproperty05")]
        [DataRow(literalPrty, literalFunc, CompletionItemKind.Value, true, "zproperty06")]

        [DataRow(literalPrtyL, literalPredL, CompletionItemKind.Keyword, false, "zzzproperty02")]
        [DataRow(literalPrtyL, literalFuncL, CompletionItemKind.Keyword, false, "zzzproperty03")]
        [DataRow(literalPrtyL, literalPredL, CompletionItemKind.Keyword, true, "zzzproperty05")]
        [DataRow(literalPrtyL, literalFuncL, CompletionItemKind.Keyword, true, "zzzproperty06")]
        [DataRow(literalPrty, literalPred, CompletionItemKind.Keyword, false, "zzzzproperty02")]
        [DataRow(literalPrty, literalFunc, CompletionItemKind.Keyword, false, "zzzzproperty03")]
        [DataRow(literalPrty, literalPred, CompletionItemKind.Keyword, true, "zzzzproperty05")]
        [DataRow(literalPrty, literalFunc, CompletionItemKind.Keyword, true, "zzzzproperty06")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string subType, CompletionItemKind isKeyword, bool isOptional, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind == isKeyword && isOptional == (item.Label.Contains(literalOpt)))
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(literalPrty, "pred ")]
        [DataRow(literalPrtyL, literalPredL)]
        [DataRow(literalPrty, "func ")]
        [DataRow(literalPrtyL, literalFuncL)]
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


        [DataRow(literalPrty, literalPred)]
        [DataRow(literalPrtyL, literalPredL)]
        [DataRow(literalPrty, literalFunc)]
        [DataRow(literalPrtyL, literalFuncL)]
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

                if (item.Label.Contains(subType) && item.Label.Contains(literalOpt))
                {
                    if (choice == literalPrty)
                    {
                        Assert.AreEqual<string>("_ " + choice + " " + subType + " opt" + postfix, item.Label);
                    }
                    else
                    {
                        Assert.AreEqual<string>("_ " + choice + " " + subType + " optional" + postfix, item.Label);
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

        [DataRow(literalPrty, "pr")]
        [DataRow(literalPrtyL, literalPrtyL)]
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
                    if (item.Detail.Contains(literalPred)) countPredicative++;
                    if (item.Detail.Contains(literalFunc)) countFunctional++;
                }
            }
            Assert.AreEqual<int>(2, countPredicative);
            Assert.AreEqual<int>(2, countFunctional);

        }

        [DataRow(literalPrty, "pr")]
        [DataRow(literalPrtyL, literalPrtyL)]
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
                    if (item.Detail.Contains(literalOpt)) countOptional++;
                    if (item.Detail.Contains(literalPred)) countPredicative++;
                    if (item.Detail.Contains(literalFunc)) countFunctional++;
                }
            }
            Assert.AreEqual<int>(2, countOptional);
            Assert.AreEqual<int>(2, countPredicative);
            Assert.AreEqual<int>(2, countFunctional);

        }

        [DataRow(literalPrty, literalPred)]
        [DataRow(literalPrtyL, literalPredL)]
        [DataRow(literalPrty, literalFunc)]
        [DataRow(literalPrtyL, literalFuncL)]
        [TestMethod]
        public void TestAddPropertyChoicesInsertText(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice) && item.InsertText.Contains(subType+" ")) { counterSnippets++; }
            }
            Assert.AreEqual<int>(3, counterSnippets);
        }

    }
}
