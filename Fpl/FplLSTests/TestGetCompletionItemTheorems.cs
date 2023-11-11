using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemTheorems
    {

        [DataRow("thm")]
        [DataRow("theorem")]
        [TestMethod]
        public void TestAddInferenceChoicesNumber(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Theorem", detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("thm")]
        [DataRow("theorem")]
        [TestMethod]
        public void TestAddInferenceKeywordCounts(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Theorem", detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("thm")]
        [DataRow("theorem")]
        [TestMethod]
        public void TestAddInferenceChoicesSortText(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Theorem", detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("theorem"));
            }
        }

        [DataRow("thm")]
        [DataRow("theorem")]
        [TestMethod]
        public void TestAddInferenceChoicesLabel(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Theorem", detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("thm")]
        [DataRow("theorem")]
        [TestMethod]
        public void TestAddInferenceChoicesDetail(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Theorem", detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(choice));
                }
                else
                {
                    Assert.IsTrue(item.Detail.Contains("theorem"));
                }
            }
        }

        [DataRow("thm")]
        [DataRow("theorem")]
        [TestMethod]
        public void TestAddInferenceChoicesInsertText(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Theorem", detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
