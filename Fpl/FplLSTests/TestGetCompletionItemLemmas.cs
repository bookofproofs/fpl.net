using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemLemmas
    {

        [DataRow("lem")]
        [DataRow("lemma")]
        [TestMethod]
        public void TestAddInferenceChoicesNumber(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Lemma", detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("lem")]
        [DataRow("lemma")]
        [TestMethod]
        public void TestAddInferenceKeywordCounts(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Lemma", detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("lem")]
        [DataRow("lemma")]
        [TestMethod]
        public void TestAddInferenceChoicesSortText(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Lemma", detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("lemma"));
            }
        }

        [DataRow("lem")]
        [DataRow("lemma")]
        [TestMethod]
        public void TestAddInferenceChoicesLabel(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Lemma", detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("lem")]
        [DataRow("lemma")]
        [TestMethod]
        public void TestAddInferenceChoicesDetail(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Lemma", detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(choice));
                }
                else
                {
                    Assert.IsTrue(item.Detail.Contains("lemma"));
                }
            }
        }

        [DataRow("lem")]
        [DataRow("lemma")]
        [TestMethod]
        public void TestAddInferenceChoicesInsertText(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Lemma", detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
