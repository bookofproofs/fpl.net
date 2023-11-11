using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemInferences
    {

        [DataRow("inf")]
        [DataRow("inference")]
        [TestMethod]
        public void TestAddInferenceChoicesNumber(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Inference", detailCi);
            Assert.AreEqual(2, actual.Count);
        }

        [DataRow("inf")]
        [DataRow("inference")]
        [TestMethod]
        public void TestAddInferenceKeywordCounts(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Inference", detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(1, count);
        }

        [DataRow("inf")]
        [DataRow("inference")]
        [TestMethod]
        public void TestAddInferenceChoicesSortText(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Inference", detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("inference"));
            }
        }

        [DataRow("inf")]
        [DataRow("inference")]
        [TestMethod]
        public void TestAddInferenceChoicesLabel(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Inference", detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("inf")]
        [DataRow("inference")]
        [TestMethod]
        public void TestAddInferenceChoicesDetail(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Inference", detailCi);
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(choice));
                }
                else
                {
                    Assert.IsTrue(item.Detail.Contains("inference"));
                }
            }
        }

        [DataRow("inf")]
        [DataRow("inference")]

        [TestMethod]
        public void TestAddInferenceChoicesInsertText(string choice)
        {
            var detailCi = FplAutoCompleteService.GetDetail(choice);
            var actual = FplAutoCompleteService.AddTheoremLikeStatementChoices(choice, "Inference", detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual(actual.Count, counterSnippets);
        }
    }
}
