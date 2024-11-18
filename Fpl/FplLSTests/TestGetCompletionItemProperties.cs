using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.Diagnostics.Metrics;
using System.Reflection;

namespace FplLSTests
{
    [TestClass]
    public class TestGetCompletionItemProperties
    {

        [DataRow("prty")]
        [DataRow("property")]
        [TestMethod]
        public void TestAddPropertyChoicesNumber(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            Assert.AreEqual<int>(8, actual.Count);
        }

        [DataRow("prty")]
        [DataRow("property")]
        [TestMethod]
        public void TestAddPropertyKeywordCounts(string choice)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual<int>(4, count);
        }

        [DataRow("property", "predicate", CompletionItemKind.Value, false, "property02")]
        [DataRow("property", "function", CompletionItemKind.Value, false, "property03")]
        [DataRow("property", "predicate", CompletionItemKind.Value, true, "property05")]
        [DataRow("property", "function", CompletionItemKind.Value, true, "property06")]
        [DataRow("prty", "pred", CompletionItemKind.Value, false, "zproperty02")]
        [DataRow("prty", "func", CompletionItemKind.Value, false, "zproperty03")]
        [DataRow("prty", "pred", CompletionItemKind.Value, true, "zproperty05")]
        [DataRow("prty", "func", CompletionItemKind.Value, true, "zproperty06")]

        [DataRow("property", "predicate", CompletionItemKind.Keyword, false, "zzzproperty02")]
        [DataRow("property", "function", CompletionItemKind.Keyword, false, "zzzproperty03")]
        [DataRow("property", "predicate", CompletionItemKind.Keyword, true, "zzzproperty05")]
        [DataRow("property", "function", CompletionItemKind.Keyword, true, "zzzproperty06")]
        [DataRow("prty", "pred", CompletionItemKind.Keyword, false, "zzzzproperty02")]
        [DataRow("prty", "func", CompletionItemKind.Keyword, false, "zzzzproperty03")]
        [DataRow("prty", "pred", CompletionItemKind.Keyword, true, "zzzzproperty05")]
        [DataRow("prty", "func", CompletionItemKind.Keyword, true, "zzzzproperty06")]
        [TestMethod]
        public void TestAddChoicesSortText(string choice, string subType, CompletionItemKind isKeyword, bool isOptional, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind == isKeyword && isOptional == (item.Label.Contains("opt")))
                {
                    Assert.AreEqual<string>(expected, item.SortText);
                }
            }
        }

        [DataRow("prty", "pred ")]
        [DataRow("property", "predicate")]
        [DataRow("prty", "func ")]
        [DataRow("property", "function")]
        [TestMethod]
        public void TestInsertTextEndsWithTwoNewLines(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword && item.InsertText.Contains(choice) && item.InsertText.Contains(l))
                {
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine));
                }
            }
        }


        [DataRow("prty", "pred")]
        [DataRow("property", "predicate")]
        [DataRow("prty", "func")]
        [DataRow("property", "function")]
        [TestMethod]
        public void TestAddPropertyChoicesLabel(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var counterRelated = 0;
            var counterRelatedOpt = 0;
            foreach (var item in actual)
            {
                string postfix = "";
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    postfix = " ...";
                }

                if (item.Label.Contains(subType) && item.Label.Contains("opt"))
                {
                    if (choice == "prty")
                    {
                        Assert.AreEqual<string>("_ " + choice + " " + subType + " opt" + postfix, item.Label);
                    }
                    else
                    {
                        Assert.AreEqual<string>("_ " + choice + " " + subType + " optional" + postfix, item.Label);
                    }
                    counterRelatedOpt++;
                }
                else if (item.Label.Contains(subType))
                {
                    Assert.AreEqual<string>("_ " + choice + " " + subType + postfix, item.Label);
                    counterRelated++;
                }
            }
            Assert.AreEqual<int>(2, counterRelated);
            Assert.AreEqual<int>(2, counterRelatedOpt);
        }

        [DataRow("prty", "pr")]
        [DataRow("property", "property")]
        [TestMethod]
        public void TestAddPropertyChoicesDetailKeyword(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var countPredicative = 0;
            var countFunctional = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(l));
                    if (item.Detail.Contains("pred")) countPredicative++;
                    if (item.Detail.Contains("func")) countFunctional++;
                }
            }
            Assert.AreEqual<int>(2, countPredicative);
            Assert.AreEqual<int>(2, countFunctional);

        }

        [DataRow("prty", "pr")]
        [DataRow("property", "property")]
        [TestMethod]
        public void TestAddPropertyChoicesDetailNonKeyword(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var countOptional = 0;
            var countPredicative = 0;
            var countFunctional = 0;
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(l));
                    if (item.Detail.Contains("opt")) countOptional++;
                    if (item.Detail.Contains("pred")) countPredicative++;
                    if (item.Detail.Contains("func")) countFunctional++;
                }
            }
            Assert.AreEqual<int>(2, countOptional);
            Assert.AreEqual<int>(2, countPredicative);
            Assert.AreEqual<int>(2, countFunctional);

        }

        [DataRow("prty", "pred")]
        [DataRow("property", "predicate")]
        [DataRow("prty", "func")]
        [DataRow("property", "function")]
        [TestMethod]
        public void TestAddPropertyChoicesInsertText(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var counterSnippets = 0;
            foreach (var item in actual)
            {
                if (item.InsertText.Contains(choice) && item.InsertText.Contains(subType+" ")) { counterSnippets++; }
            }
            Assert.AreEqual<int>(3, counterSnippets);
        }

    }
}
