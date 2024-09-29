using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemWord
    {

        [DataRow("word")]
        [TestMethod]
        public void TestAddWordChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWord().GetChoices(detailCi);
            Assert.AreEqual<int>(1, actual.Count);
        }

        [DataRow("word")]
        [TestMethod]
        public void TestAddWordValueCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWord().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Value) count++;
            }
            Assert.AreEqual<int>(actual.Count, count);
        }

        [DataRow("word", CompletionItemKind.Value, "word")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWord().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow("word")]
        [TestMethod]
        public void TestInsertTextEndsWithSpace(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWord().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice) && !choice.EndsWith("!"))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }


        [DataRow("word", "someIdentifier")]
        [TestMethod]
        public void TestAddWordChoicesLabel(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWord().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(l) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("word", "regex \\w+")]
        [TestMethod]
        public void TestAddWordChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWord().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual<string>(l, item.Detail);
            }
        }

        [DataRow("word", "someIdentifier ")]
        [TestMethod]
        public void TestAddWordChoicesInsertText(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesWord().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual<string>(l, item.InsertText);
            }
        }
    }
}
