using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;
namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemPascalCaseId
    {

        [DataRow(PrimPascalCaseId)]
        [TestMethod]
        public void TestAddPascalCaseIdChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            Assert.AreEqual<int>(1, actual.Count);
        }

        [DataRow(PrimPascalCaseId)]
        [TestMethod]
        public void TestAddPascalCaseIdReferenceCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Reference) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(PrimPascalCaseId, CompletionItemKind.Reference, PrimPascalCaseId)]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(PrimPascalCaseId)]
        [TestMethod]
        public void TestInsertTextEndsWithSpace(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(" "));
                }
            }
        }

        [DataRow(PrimPascalCaseId)]
        [TestMethod]
        public void TestAddPascalCaseIdChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(PrimPascalCaseId, "user-defined id")]
        [TestMethod]
        public void TestAddPascalCaseIdChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual<string>(l, item.Detail);
            }
        }

        [DataRow(PrimPascalCaseId)]
        [TestMethod]
        public void TestAddPascalCaseIdChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesPascalCaseId().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
                if (item.InsertText.Contains(" "))
                {
                    var res = FplParser.testParser(PrimPascalCaseId, item.InsertText);
                    if (!res.StartsWith("Success:"))
                    {
                        Assert.IsTrue(false, res);
                    }
                }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
