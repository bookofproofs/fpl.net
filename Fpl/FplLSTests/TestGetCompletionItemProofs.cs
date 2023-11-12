using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemProofs
    {

        [DataRow("prf")]
        [DataRow("proof")]
        [TestMethod]
        public void TestAddProofChoicesNumber(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddProofChoices(choice, detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("prf")]
        [DataRow("proof")]
        [TestMethod]
        public void TestAddProofKeywordCounts(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddProofChoices(choice, detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("prf")]
        [DataRow("proof")]
        [TestMethod]
        public void TestAddProofChoicesSortText(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddProofChoices(choice, detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("proof"));
            }
        }

        [DataRow("prf")]
        [DataRow("proof")]
        [TestMethod]
        public void TestAddProofChoicesLabel(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddProofChoices(choice, detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("prf", "proof")]
        [DataRow("proof", "proof")]
        [TestMethod]
        public void TestAddProofChoicesDetail(string choice, string l)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddProofChoices(choice, detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("prf")]
        [DataRow("proof")]
        [TestMethod]
        public void TestAddProofChoicesInsertText(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddProofChoices(choice, detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
