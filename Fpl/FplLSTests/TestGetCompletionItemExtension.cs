using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;
using static FplParser;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemExtension
    {

        [DataRow(LiteralExtL)]
        [DataRow(LiteralExt)]
        [TestMethod]
        public void TestAddExtensionChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesExtension().GetChoices(detailCi);
            Assert.AreEqual<int>(2, actual.Count);
        }

        [DataRow(LiteralExtL)]
        [DataRow(LiteralExt)]
        [TestMethod]
        public void TestAddExtensionKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesExtension().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow(LiteralExtL, CompletionItemKind.Property, "extension01")]
        [DataRow(LiteralExt, CompletionItemKind.Property, "extension02")]
        [DataRow(LiteralExtL, CompletionItemKind.Keyword, "zzzextension01")]
        [DataRow(LiteralExt, CompletionItemKind.Keyword, "zzzzextension02")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, CompletionItemKind kind, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesExtension().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Kind == kind)
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow(LiteralExtL)]
        [DataRow(LiteralExt)]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesExtension().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }

        [DataRow(LiteralExtL)]
        [DataRow(LiteralExt)]
        [TestMethod]
        public void TestAddExtensionChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesExtension().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow(LiteralExtL)]
        [DataRow(LiteralExt)]
        [TestMethod]
        public void TestAddExtensionChoicesDetail(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesExtension().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(choice));
            }
        }

        [DataRow(LiteralExtL)]
        [DataRow(LiteralExt)]
        [TestMethod]
        public void TestAddExtensionChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesExtension().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
                if (item.InsertText.Contains(" "))
                {
                    var res = FplParser.testParser(LiteralExt, item.InsertText);
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
