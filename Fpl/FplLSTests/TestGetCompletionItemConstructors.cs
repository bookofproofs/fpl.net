using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemConstructors
    {

        [DataRow("ctor")]
        [DataRow("constructor")]
        [TestMethod]
        public void TestAddConstructorChoicesNumber(string choice)
        {
            var actual = FplAutoCompleteService.AddConstructorChoices(choice);
            Assert.AreEqual(6, actual.Count);
        }

        [DataRow("ctor")]
        [DataRow("constructor")]
        [TestMethod]
        public void TestAddConstructorSnippetCounts(string choice)
        {
            var actual = FplAutoCompleteService.AddConstructorChoices(choice);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Snippet) count++;
            }
            Assert.AreEqual(3, count);
        }

        [DataRow("ctor")]
        [DataRow("constructor")]
        [TestMethod]
        public void TestAddConstructorKeywordCounts(string choice)
        {
            var actual = FplAutoCompleteService.AddConstructorChoices(choice);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(3, count);
        }

        [DataRow("ctor")]
        [DataRow("constructor")]
        [TestMethod]
        public void TestAddConstructorChoicesSortText(string choice)
        {
            var actual = FplAutoCompleteService.AddConstructorChoices(choice);
            var countSubType = 0;
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(choice));
            }
            Assert.AreEqual(2, countSubType);
        }

        [DataRow("ctor")]
        [DataRow("constructor")]
        [TestMethod]
        public void TestAddConstructorChoicesLabel(string choice)
        {
            var actual = FplAutoCompleteService.AddConstructorChoices(choice);
            var counterRelated = 0;
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice));
            }
            Assert.AreEqual(1, counterRelated);
        }

        [DataRow("ctor")]
        [DataRow("constructor")]
        [TestMethod]
        public void TestAddConstructorChoicesDetail(string choice)
        {
            var actual = FplAutoCompleteService.AddConstructorChoices(choice);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains("constructor"));
            }
        }

        [DataRow("ctor")]
        [DataRow("constructor")]
        [TestMethod]
        public void TestAddConstructorChoicesInsertText(string choice)
        {
            var actual = FplAutoCompleteService.AddConstructorChoices(choice);
            var counterSnippets = 0;
            var counterKeywords = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Snippet)
                {
                    if (item.InsertText.Contains(choice)) { counterSnippets++; }
                }
                else
                {
                    if (item.InsertText == null) { counterKeywords++; }
                }
            }
            Assert.AreEqual(3, counterKeywords);
            Assert.AreEqual(1, counterSnippets);
        }
    }
}
