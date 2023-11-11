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
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            Assert.AreEqual(6, actual.Count);
        }

        [DataRow("def")]
        [DataRow("definition")]
        [TestMethod]
        public void TestAddDefinitionKeywordCounts(string choice)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(3, count);
        }

        [DataRow("def", "class")]
        [DataRow("definition", "class")]
        [DataRow("def", "predicate")]
        [DataRow("definition", "predicate")]
        [DataRow("def", "function")]
        [DataRow("definition", "function")]
        [TestMethod]
        public void TestAddDefinitionChoicesSortText(string choice, string subType)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            var countSubType = 0;
            foreach (var item in actual)
            {
                if (item.SortText.Contains(subType)) countSubType++;
                Assert.IsTrue(item.SortText.Contains(choice));
            }
            Assert.AreEqual(2, countSubType);
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
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            var counterRelated = 0;
            foreach (var item in actual)
            {
                if (item.Label.Contains(subType))
                {
                    Assert.AreEqual("_ " + choice + " " + subType, item.Label);
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
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
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
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice) && item.InsertText.Contains(subType)) { counterSnippets++; }
            }
            Assert.AreEqual(2, counterSnippets);
        }

    }
}
