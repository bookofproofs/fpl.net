using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemVariable
    {
        [DataRow("variable")]
        [DataRow("variable (got keyword)")]
        [DataRow("variable (got template)")]
        [TestMethod]
        public void TestAddVariableChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesVariable().GetChoices(detailCi);
            Assert.AreEqual(1, actual.Count);
        }

        [DataRow("variable")]
        [DataRow("variable (got keyword)")]
        [DataRow("variable (got template)")]
        [TestMethod]
        public void TestAddVariableVariableCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesVariable().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Variable) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("variable", CompletionItemKind.Reference, "variable")]
        [DataRow("variable (got keyword)", CompletionItemKind.Reference, "variable")]
        [DataRow("variable (got template)", CompletionItemKind.Reference, "variable")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesVariable().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual(expected, item.SortText);
            }
        }

        [DataRow("variable")]
        [DataRow("variable (got keyword)")]
        [DataRow("variable (got template)")]
        [TestMethod]
        public void TestInsertTextEndsWithSpace(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesVariable().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }

        [DataRow("variable", "someVar")]
        [DataRow("variable (got keyword)", "someVar")]
        [DataRow("variable (got template)", "someVar")]
        [TestMethod]
        public void TestAddVariableChoicesLabel(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesVariable().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(l) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("variable")]
        [DataRow("variable (got keyword)")]
        [DataRow("variable (got template)")]
        [TestMethod]
        public void TestAddVariableChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesVariable().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains("variable"));
            }
        }

        [DataRow("variable", "someVar")]
        [DataRow("variable (got keyword)", "someVar")]
        [DataRow("variable (got template)", "someVar")]
        [TestMethod]
        public void TestAddVariableChoicesInsertText(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesVariable().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual(l + " ", item.InsertText);
            }
        }
    }
}
