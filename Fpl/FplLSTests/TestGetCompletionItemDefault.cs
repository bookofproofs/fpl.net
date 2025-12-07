using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemDefault
    {
        [DataRow("?")]
        [DataRow("|")]
        [DataRow("@")]
        [DataRow(PrimDelegateEqual)]
        [DataRow(":=")]
        [DataRow(":*")]
        [DataRow(":")]
        [DataRow(".")]
        [DataRow(",")]
        [DataRow("~")]
        [DataRow("|-")]
        [DataRow("->")]
        [DataRow(";")]
        [DataRow("!")]
        [DataRow("{")]
        [DataRow("}")]
        [DataRow("(")]
        [DataRow(")")]
        [DataRow("<")]
        [DataRow(">")]
        [DataRow(":ext")]
        [DataRow(":end")]
        [DataRow("blabla")]
        [TestMethod]
        public void TestAddDefaultChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefault().GetChoices(detailCi);
            Assert.AreEqual<int>(1, actual.Count);
        }

        [DataRow("?")]
        [DataRow("|")]
        [DataRow("@")]
        [DataRow(PrimDelegateEqual)]
        [DataRow(":=")]
        [DataRow(":*")]
        [DataRow(":")]
        [DataRow(".")]
        [DataRow(",")]
        [DataRow("~")]
        [DataRow("|-")]
        [DataRow("->")]
        [DataRow(";")]
        [DataRow("!")]
        [DataRow("{")]
        [DataRow("}")]
        [DataRow("(")]
        [DataRow(")")]
        [DataRow("<")]
        [DataRow(">")]
        [DataRow(":ext")]
        [DataRow(":end")]
        [DataRow("blabla")]
        [TestMethod]
        public void TestAddDefaultTextCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefault().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Text) count++;
            }
            Assert.AreEqual<int>(1, count);
        }

        [DataRow("?")]
        [DataRow("|")]
        [DataRow("@")]
        [DataRow(PrimDelegateEqual)]
        [DataRow(":=")]
        [DataRow(":*")]
        [DataRow(":")]
        [DataRow(".")]
        [DataRow(",")]
        [DataRow("~")]
        [DataRow("|-")]
        [DataRow("->")]
        [DataRow(";")]
        [DataRow("!")]
        [DataRow("{")]
        [DataRow("}")]
        [DataRow("(")]
        [DataRow(")")]
        [DataRow("<")]
        [DataRow(">")]
        [DataRow(":ext")]
        [DataRow(":end")]
        [DataRow("blabla")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefault().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(choice));
            }
        }

        [DataRow("?")]
        [DataRow("|")]
        [DataRow("@")]
        [DataRow(PrimDelegateEqual)]
        [DataRow(":=")]
        [DataRow(":*")]
        [DataRow(":")]
        [DataRow(".")]
        [DataRow(",")]
        [DataRow("~")]
        [DataRow("|-")]
        [DataRow("->")]
        [DataRow(";")]
        [DataRow("!")]
        [DataRow("{")]
        [DataRow("}")]
        [DataRow("(")]
        [DataRow(")")]
        [DataRow("<")]
        [DataRow(">")]
        [DataRow(":ext")]
        [DataRow(":end")]
        [DataRow("blabla")]
        [TestMethod]
        public void TestAddDefaultChoicesLabel(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefault().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice) && item.Label.StartsWith("_ "));
            }
        }

        [DataRow("?", "else case '?'")]
        [DataRow("|", "new case '|'")]
        [DataRow(PrimDelegateEqual, "equal sign '='")]
        [DataRow(":=", "assignment sign ':='")]
        [DataRow(":*", "zero or more '*'")]
        [DataRow(":", "colon ':'")]
        [DataRow(".", "dot '.'")]
        [DataRow(",", "enumeration ','")]
        [DataRow("~", "type declaration '~'")]
        [DataRow("|-", "follows logically '|-'")]
        [DataRow("->", "map '->'")]
        [DataRow(";", "closing ';'")]
        [DataRow("{", "opening '{'")]
        [DataRow("}", "closing '}'")]
        [DataRow("(", "opening '('")]
        [DataRow(")", "closing '('")]
        [DataRow("[", "opening '['")]
        [DataRow("]", "closing ']'")]
        [DataRow("blabla", "unknown")]
        [TestMethod]
        public void TestAddDefaultChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefault().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.AreEqual<string>(l, item.Detail);
            }
        }

        [DataRow("?")]
        [DataRow("|")]
        [DataRow("@")]
        [DataRow(PrimDelegateEqual)]
        [DataRow(":=")]
        [DataRow(":*")]
        [DataRow(":")]
        [DataRow(".")]
        [DataRow(",")]
        [DataRow("~")]
        [DataRow("|-")]
        [DataRow("->")]
        [DataRow(";")]
        [DataRow("{")]
        [DataRow("}")]
        [DataRow("(")]
        [DataRow(")")]
        [DataRow("blabla")]
        [TestMethod]
        public void TestAddDefaultChoicesInsertText(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesDefault().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice)) { counterSnippets++; }
            }
            Assert.AreEqual<int>(actual.Count, counterSnippets);
        }
    }
}
