using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDefinitions
    {

        [DataRow("all", 6)]
        [DataRow("ex", 6)]
        [DataRow("exn", 5)]
        [TestMethod]
        public void TestAddDefinitionChoicesNumber(string choice, int number)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            Assert.AreEqual(number, actual.Count);
        }

        [DataRow("all", 5)]
        [DataRow("ex", 5)]
        [DataRow("exn", 4)]
        [TestMethod]
        public void TestAddDefinitionSnippetCounts(string choice, int number)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            var count = 0;
            foreach(var item in actual)
            {
                if (item.Kind == CompletionItemKind.Snippet) count++;
            }
            Assert.AreEqual(number, count);
        }

        [DataRow("all", 1)]
        [DataRow("ex", 1)]
        [DataRow("exn", 1)]
        [TestMethod]
        public void TestAddDefinitionKeywordCounts(string choice, int number)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(number, count);
        }

        [DataRow("all")]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddDefinitionChoicesSortText(string choice)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            var lastSortText = "";
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(choice));
                Assert.IsTrue(string.Compare(lastSortText,item.SortText)<0);
                lastSortText = item.SortText;
            }
        }

        [DataRow("all")]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddDefinitionChoicesLabel(string choice)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice));
            }
        }

        [DataRow("all", "all")]
        [DataRow("ex", "exists")]
        [DataRow("exn", "n-times")]
        [TestMethod]
        public void TestAddDefinitionChoicesDetail(string choice, string s)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(s));
            }
        }

        [DataRow("all")]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddDefinitionChoicesInsertText(string choice)
        {
            var actual = FplAutoCompleteService.AddDefinitionChoices(choice);
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Snippet)
                {
                    Assert.IsTrue(item.InsertText.Contains(choice));
                }
                else
                {
                    Assert.IsTrue(item.InsertText == null);
                }
            }
        }

    }
}
