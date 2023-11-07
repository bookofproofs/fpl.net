using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDefinitions
    {

        [TestMethod]
        public void TestAddDefinitionChoicesNumber()
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices();
            Assert.AreEqual(0, actual.Count);
        }

        [TestMethod]
        public void TestAddDefinitionSnippetCounts()
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices();
            var count = 0;
            foreach(var item in actual)
            {
                if (item.Kind == CompletionItemKind.Snippet) count++;
            }
            Assert.AreEqual(0, count);
        }

        [TestMethod]
        public void TestAddDefinitionKeywordCounts()
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices();
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(0, count);
        }

        [TestMethod]
        public void TestAddDefinitionChoicesSortText()
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices();
            var lastSortText = "";
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("xxx"));
                Assert.IsTrue(string.Compare(lastSortText,item.SortText)<0);
                lastSortText = item.SortText;
            }
        }

        [TestMethod]
        public void TestAddDefinitionChoicesLabel()
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices();
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains("xxx"));
            }
        }
        [TestMethod]
        public void TestAddDefinitionChoicesDetail()
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices();
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains("xxx"));
            }
        }

        [TestMethod]
        public void TestAddDefinitionChoicesInsertText()
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices();
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Snippet)
                {
                    Assert.IsTrue(item.InsertText.Contains("xxx"));
                }
                else
                {
                    Assert.IsTrue(item.InsertText == null);
                }
            }
        }

    }
}
