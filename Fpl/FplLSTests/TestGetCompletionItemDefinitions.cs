using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Diagnostics.Metrics;
using System.Reflection;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDefinitions
    {

        [DataRow("def")]
        [DataRow("definition")]
        [TestMethod]
        public void TestAddDefinitionChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            Assert.AreEqual(6, actual.Count);
        }

        [DataRow("def")]
        [DataRow("definition")]
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
            Assert.AreEqual(3, count);
        }

        [DataRow("definition", "class", CompletionItemKind.Class, "definition01")]
        [DataRow("definition", "predicate", CompletionItemKind.Class, "definition02")]
        [DataRow("definition", "function", CompletionItemKind.Class, "definition03")]
        [DataRow("def", "cl", CompletionItemKind.Class, "zdefinition01")]
        [DataRow("def", "pred", CompletionItemKind.Class, "zdefinition02")]
        [DataRow("def", "func", CompletionItemKind.Class, "zdefinition03")]
        [DataRow("definition", "class", CompletionItemKind.Keyword, "zzzdefinition01")]
        [DataRow("definition", "predicate", CompletionItemKind.Keyword, "zzzdefinition02")]
        [DataRow("definition", "function", CompletionItemKind.Keyword, "zzzdefinition03")]
        [DataRow("def", "cl", CompletionItemKind.Keyword, "zzzzdefinition01")]
        [DataRow("def", "pred", CompletionItemKind.Keyword, "zzzzdefinition02")]
        [DataRow("def", "func", CompletionItemKind.Keyword, "zzzzdefinition03")]
        [TestMethod]
        public void TestAddPropertyChoicesSortText(string choice, string subType, CompletionItemKind isKeyword, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind == isKeyword)
                {
                    Assert.AreEqual(expected, item.SortText);
                }
            }
        }

        [DataRow("def", "cl")]
        [DataRow("definition", "class")]
        [DataRow("def", "pred")]
        [DataRow("definition", "predicate")]
        [DataRow("def", "func")]
        [DataRow("definition", "function")]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefinition().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice) && item.InsertText.Contains(l))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine + Environment.NewLine));
                }
            }
        }

        [DataRow("def", "cl")]
        [DataRow("definition", "class")]
        [DataRow("def", "pred")]
        [DataRow("definition", "predicate")]
        [DataRow("def", "func")]
        [DataRow("definition", "function")]
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
                    Assert.AreEqual("_ " + choice + " " + subType + postfix, item.Label);
                    counterRelated++;
                }
            }
            Assert.AreEqual(2, counterRelated);
        }

        [DataRow("def")]
        [DataRow("definition")]
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

        [DataRow("def", "cl")]
        [DataRow("definition", "class")]
        [DataRow("def", "pred")]
        [DataRow("definition", "predicate")]
        [DataRow("def", "func")]
        [DataRow("definition", "function")]
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
            Assert.AreEqual(2, counterSnippets);
        }

    }
}
