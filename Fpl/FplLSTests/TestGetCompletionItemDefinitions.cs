// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;


namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDefinitions
    {

        [DataRow(literalDef)]
        [DataRow(literalDefL)]
        [TestMethod]
        public void TestAddDefinitionChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            Assert.AreEqual<int>(6, actual.Count);
        }

        [DataRow(literalDef)]
        [DataRow(literalDefL)]
        [TestMethod]
        public void TestAddDefinitionKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(3, count);
        }

        [DataRow(literalDefL, literalClL, CompletionItemKind.Class, "definition01")]
        [DataRow(literalDefL, literalPredL, CompletionItemKind.Class, "definition02")]
        [DataRow(literalDefL, literalFuncL, CompletionItemKind.Class, "definition03")]
        [DataRow(literalDef, literalCl, CompletionItemKind.Class, "zdefinition01")]
        [DataRow(literalDef, literalPred, CompletionItemKind.Class, "zdefinition02")]
        [DataRow(literalDef, literalFunc, CompletionItemKind.Class, "zdefinition03")]
        [DataRow(literalDefL, literalClL, CompletionItemKind.Keyword, "zzzdefinition01")]
        [DataRow(literalDefL, literalPredL, CompletionItemKind.Keyword, "zzzdefinition02")]
        [DataRow(literalDefL, literalFuncL, CompletionItemKind.Keyword, "zzzdefinition03")]
        [DataRow(literalDef, literalCl, CompletionItemKind.Keyword, "zzzzdefinition01")]
        [DataRow(literalDef, literalPred, CompletionItemKind.Keyword, "zzzzdefinition02")]
        [DataRow(literalDef, literalFunc, CompletionItemKind.Keyword, "zzzzdefinition03")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string subType, CompletionItemKind isKeyword, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind == isKeyword)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(literalDef, literalCl)]
        [DataRow(literalDefL, literalClL)]
        [DataRow(literalDef, literalPred)]
        [DataRow(literalDefL, literalPredL)]
        [DataRow(literalDef, literalFunc)]
        [DataRow(literalDefL, literalFuncL)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice) && item.InsertText.Contains(l))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }

        [DataRow(literalDef, literalCl)]
        [DataRow(literalDefL, literalClL)]
        [DataRow(literalDef, literalPred)]
        [DataRow(literalDefL, literalPredL)]
        [DataRow(literalDef, literalFunc)]
        [DataRow(literalDefL, literalFuncL)]
        [TestMethod]
        public void TestAddDefinitionChoicesLabel(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
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

        [DataRow(literalDef)]
        [DataRow(literalDefL)]
        [TestMethod]
        public void TestAddDefinitionChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow(literalDef, literalCl)]
        [DataRow(literalDefL, literalClL)]
        [DataRow(literalDef, literalPred)]
        [DataRow(literalDefL, literalPredL)]
        [DataRow(literalDef, literalFunc)]
        [DataRow(literalDefL, literalFuncL)]
        [TestMethod]
        public void TestAddDefinitionChoicesInsertText(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice) && item.InsertText.Contains(subType)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(2, counterSnippets);
        }

    }
}
