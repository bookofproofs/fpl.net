using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemIso639
    {

        [DataRow("ISO 639 language code")]
        [TestMethod]
        public void TestAddIso639ChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIso639().GetChoices(detailCi);
            Assert.AreEqual<int>(490, actual.Count);
        }

        [DataRow("ISO 639 language code")]
        [TestMethod]
        public void TestAddIso639KeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIso639().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Value) count++;
            }
            Assert.AreEqual<int>(actual.Count, count);
        }

        [DataRow("ISO 639 language code")]
        [TestMethod]
        public void TestAddIso639ChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIso639().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.StartsWith("_ "));
            }
        }

        [DataRow("ISO 639 language code")]
        [TestMethod]
        public void TestAddIso639ChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesIso639().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail != "");
            }
        }

    }
}
