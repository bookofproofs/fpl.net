using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplGrammarCommons;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemSymbol
    {

        [DataRow("infix symbol", false, 2)]
        [DataRow("prefix symbol", false, 4)]
        [DataRow("postfix symbol", false, 1)]
        [DataRow("object symbol", false, 8)]
        [DataRow("infix", true, 2)]
        [DataRow("prefix", true, 4)]
        [DataRow("postfix", true, 1)]
        [DataRow("symbol", true, 8)]
        [TestMethod]
        public void TestAddSymbolChoicesNumber(string choice, bool declarative, int bitMask)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbol(choice, declarative).GetChoices(detailCi);
            var counter = 0;

            foreach (KeyValuePair<char, System.Tuple<int, string, string>> item in FplGrammarCommons.mathSymbols)
            {
                if ((item.Value.Item1 & bitMask) == bitMask)
                {
                    counter++;
                }
            }
            Assert.AreEqual<int>(counter, actual.Count);
        }

        [DataRow("infix symbol", false)]
        [DataRow("prefix symbol", false)]
        [DataRow("postfix symbol", false)]
        [DataRow("object symbol", false)]
        [DataRow("infix", true)]
        [DataRow("prefix", true)]
        [DataRow("postfix", true)]
        [DataRow("symbol", true)]
        [TestMethod]
        public void TestAddSymbolOperatorCounts(string choice, bool declarative)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbol(choice, declarative).GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Operator) count++;
            }
            Assert.AreEqual<int>(actual.Count, count);
        }

        [DataRow("infix symbol", false)]
        [DataRow("prefix symbol", false)]
        [DataRow("postfix symbol", false)]
        [DataRow("object symbol", false)]
        [DataRow("infix", true)]
        [DataRow("prefix", true)]
        [DataRow("postfix", true)]
        [DataRow("symbol", true)]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, bool declarative)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbol(choice, declarative).GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.StartsWith($"symbol {item.InsertText}"));
            }
        }

        [DataRow("infix symbol", false)]
        [DataRow("prefix symbol", false)]
        [DataRow("postfix symbol", false)]
        [DataRow("object symbol", false)]
        [DataRow("infix", true)]
        [DataRow("prefix", true)]
        [DataRow("postfix", true)]
        [DataRow("symbol", true)]
        [TestMethod]
        public void TestInsertTextDoesNotEndWithSpace(string choice, bool declarative)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbol(choice, declarative).GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(!item.InsertText.EndsWith(' '));
            }
        }

        [DataRow("infix symbol", false)]
        [DataRow("prefix symbol", false)]
        [DataRow("postfix symbol", false)]
        [DataRow("object symbol", false)]
        [DataRow("infix", true)]
        [DataRow("prefix", true)]
        [DataRow("postfix", true)]
        [DataRow("symbol", true)]
        [TestMethod]
        public void TestAddSymbolChoicesLabel(string choice, bool declarative)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbol(choice, declarative).GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (declarative)
                {
                    Assert.IsTrue(item.Label.Contains(choice) && item.Label.Contains("U+") && item.Label.StartsWith("_ "));
                }
                else
                {
                    Assert.IsTrue(item.Label.Contains(item.InsertText) && item.Label.StartsWith("_ "));
                }

            }
        }

        [DataRow("infix symbol", false)]
        [DataRow("prefix symbol", false)]
        [DataRow("postfix symbol", false)]
        [DataRow("object symbol", false)]
        [DataRow("infix", true)]
        [DataRow("prefix", true)]
        [DataRow("postfix", true)]
        [DataRow("symbol", true)]
        [TestMethod]
        public void TestAddSymbolChoicesDetail(string choice, bool declarative)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbol(choice, declarative).GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains("usable"));
            }
        }

        [DataRow("infix symbol", false)]
        [DataRow("prefix symbol", false)]
        [DataRow("postfix symbol", false)]
        [DataRow("object symbol", false)]
        [DataRow("infix", true)]
        [DataRow("prefix", true)]
        [DataRow("postfix", true)]
        [DataRow("symbol", true)]
        [TestMethod]
        public void TestAddSymbolChoicesInsertText(string choice, bool declarative)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesSymbol(choice, declarative).GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (declarative)
                {
                    var symbol = item.Label.Substring(4 + choice.Length, 1);
                    Assert.IsTrue(item.InsertText.Contains(choice) && item.InsertText.Contains(symbol));
                }
                else
                {
                    var symbol = item.Label.Substring(3, 1);
                    Assert.IsTrue(item.InsertText.Contains(symbol));
                }
            }
        }
    }
}
