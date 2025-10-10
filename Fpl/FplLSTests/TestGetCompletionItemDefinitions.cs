// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;


namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDefinitions
    {

        [DataRow(LiteralDef)]
        [DataRow(LiteralDefL)]
        [TestMethod]
        public void TestAddDefinitionChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            Assert.AreEqual<int>(6, actual.Count);
        }

        [DataRow(LiteralDef)]
        [DataRow(LiteralDefL)]
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

        [DataRow(LiteralDefL, LiteralClL, CompletionItemKind.Class, "definition01")]
        [DataRow(LiteralDefL, LiteralPredL, CompletionItemKind.Class, "definition02")]
        [DataRow(LiteralDefL, LiteralFuncL, CompletionItemKind.Class, "definition03")]
        [DataRow(LiteralDef, LiteralCl, CompletionItemKind.Class, "zdefinition01")]
        [DataRow(LiteralDef, LiteralPred, CompletionItemKind.Class, "zdefinition02")]
        [DataRow(LiteralDef, LiteralFunc, CompletionItemKind.Class, "zdefinition03")]
        [DataRow(LiteralDefL, LiteralClL, CompletionItemKind.Keyword, "zzzdefinition01")]
        [DataRow(LiteralDefL, LiteralPredL, CompletionItemKind.Keyword, "zzzdefinition02")]
        [DataRow(LiteralDefL, LiteralFuncL, CompletionItemKind.Keyword, "zzzdefinition03")]
        [DataRow(LiteralDef, LiteralCl, CompletionItemKind.Keyword, "zzzzdefinition01")]
        [DataRow(LiteralDef, LiteralPred, CompletionItemKind.Keyword, "zzzzdefinition02")]
        [DataRow(LiteralDef, LiteralFunc, CompletionItemKind.Keyword, "zzzzdefinition03")]
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

        [DataRow(LiteralDef, LiteralCl)]
        [DataRow(LiteralDefL, LiteralClL)]
        [DataRow(LiteralDef, LiteralPred)]
        [DataRow(LiteralDefL, LiteralPredL)]
        [DataRow(LiteralDef, LiteralFunc)]
        [DataRow(LiteralDefL, LiteralFuncL)]
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

        [DataRow(LiteralDef, LiteralCl)]
        [DataRow(LiteralDefL, LiteralClL)]
        [DataRow(LiteralDef, LiteralPred)]
        [DataRow(LiteralDefL, LiteralPredL)]
        [DataRow(LiteralDef, LiteralFunc)]
        [DataRow(LiteralDefL, LiteralFuncL)]
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

        [DataRow(LiteralDef)]
        [DataRow(LiteralDefL)]
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

        [DataRow(LiteralDef, LiteralCl)]
        [DataRow(LiteralDefL, LiteralClL)]
        [DataRow(LiteralDef, LiteralPred)]
        [DataRow(LiteralDefL, LiteralPredL)]
        [DataRow(LiteralDef, LiteralFunc)]
        [DataRow(LiteralDefL, LiteralFuncL)]
        [TestMethod]
        public void TestAddDefinitionChoicesInsertText(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice) && item.InsertText.Contains(subType)) { counterSnippets++; }
                if (item.InsertText.Contains("{"))
                {
                    var res = FplParser.testParser(LiteralDef, item.InsertText);
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
