using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDigits
    {

        [DataRow("digits")]
        [TestMethod]
        public void TestAddDigitsChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDigits().GetChoices(detailCi);
            Assert.AreEqual(1, actual.Count);
        }

        [DataRow("digits")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDigits().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("123"));
            }
        }

        [DataRow("digits")]
        [TestMethod]
        public void TestAddDigitsChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDigits().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains("123") && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("digits")]
        [TestMethod]
        public void TestAddDigitsChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDigits().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

    }
}
