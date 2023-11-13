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
            Assert.AreEqual(12, actual.Count);
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
            Assert.AreEqual(6, count);
        }

        [DataRow("property", "object", CompletionItemKind.Value, false, "property01")]
        [DataRow("property", "predicate", CompletionItemKind.Value, false, "property02")]
        [DataRow("property", "function", CompletionItemKind.Value, false, "property03")]
        [DataRow("property", "object", CompletionItemKind.Value, true, "property04")]
        [DataRow("property", "predicate", CompletionItemKind.Value, true, "property05")]
        [DataRow("property", "function", CompletionItemKind.Value, true, "property06")]
        [DataRow("prty", "obj", CompletionItemKind.Value, false, "zproperty01")]
        [DataRow("prty", "pred", CompletionItemKind.Value, false, "zproperty02")]
        [DataRow("prty", "func", CompletionItemKind.Value, false, "zproperty03")]
        [DataRow("prty", "obj", CompletionItemKind.Value, true, "zproperty04")]
        [DataRow("prty", "pred", CompletionItemKind.Value, true, "zproperty05")]
        [DataRow("prty", "func", CompletionItemKind.Value, true, "zproperty06")]

        [DataRow("property", "object", CompletionItemKind.Keyword, false, "zzzproperty01")]
        [DataRow("property", "predicate", CompletionItemKind.Keyword, false, "zzzproperty02")]
        [DataRow("property", "function", CompletionItemKind.Keyword, false, "zzzproperty03")]
        [DataRow("property", "object", CompletionItemKind.Keyword, true, "zzzproperty04")]
        [DataRow("property", "predicate", CompletionItemKind.Keyword, true, "zzzproperty05")]
        [DataRow("property", "function", CompletionItemKind.Keyword, true, "zzzproperty06")]
        [DataRow("prty", "obj", CompletionItemKind.Keyword, false, "zzzzproperty01")]
        [DataRow("prty", "pred", CompletionItemKind.Keyword, false, "zzzzproperty02")]
        [DataRow("prty", "func", CompletionItemKind.Keyword, false, "zzzzproperty03")]
        [DataRow("prty", "obj", CompletionItemKind.Keyword, true, "zzzzproperty04")]
        [DataRow("prty", "pred", CompletionItemKind.Keyword, true, "zzzzproperty05")]
        [DataRow("prty", "func", CompletionItemKind.Keyword, true, "zzzzproperty06")]
        [TestMethod]
        public void TestAddPropertyChoicesSortText(string choice, string subType, CompletionItemKind isKeyword, bool isOptional, string expected)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            foreach (var item in actual)
            {
                if (item.Label.Contains(choice) && item.Label.Contains(subType) && item.Kind == isKeyword && isOptional == (item.Label.Contains("opt")))
                {
                    Assert.AreEqual(expected, item.SortText);
                }
            }
        }

        [DataRow("prty", "obj ")]
        [DataRow("property", "object")]
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
                    Assert.IsTrue(item.InsertText.EndsWith(Environment.NewLine + Environment.NewLine));
                }
            }
        }


        [DataRow("prty", "obj")]
        [DataRow("property", "object")]
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
                        Assert.AreEqual("_ " + choice + " opt " + subType + postfix, item.Label);
                    }
                    else
                    {
                        Assert.AreEqual("_ " + choice + " optional " + subType + postfix, item.Label);
                    }
                    counterRelatedOpt++;
                }
                else if (item.Label.Contains(subType))
                {
                    Assert.AreEqual("_ " + choice + " " + subType + postfix, item.Label);
                    counterRelated++;
                }
            }
            Assert.AreEqual(2, counterRelated);
            Assert.AreEqual(2, counterRelatedOpt);
        }

        [DataRow("prty", "pr")]
        [DataRow("property", "property")]
        [TestMethod]
        public void TestAddPropertyChoicesDetailKeyword(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var countOptional = 0;
            var countObject = 0;
            var countPredicative = 0;
            var countFunctional = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(l));
                    if (item.Detail.Contains("opt")) countOptional++;
                    if (item.Detail.Contains("obj")) countObject++;
                    if (item.Detail.Contains("pred")) countPredicative++;
                    if (item.Detail.Contains("func")) countFunctional++;
                }
            }
            Assert.AreEqual(3, countOptional);
            Assert.AreEqual(2, countObject);
            Assert.AreEqual(2, countPredicative);
            Assert.AreEqual(2, countFunctional);

        }

        [DataRow("prty", "pr")]
        [DataRow("property", "property")]
        [TestMethod]
        public void TestAddPropertyChoicesDetailNonKeyword(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var countOptional = 0;
            var countObject = 0;
            var countPredicative = 0;
            var countFunctional = 0;
            foreach (var item in actual)
            {
                if (item.Kind != CompletionItemKind.Keyword)
                {
                    Assert.IsTrue(item.Detail.Contains(l));
                    if (item.Detail.Contains("opt")) countOptional++;
                    if (item.Detail.Contains("cl")) countObject++;
                    if (item.Detail.Contains("pred")) countPredicative++;
                    if (item.Detail.Contains("func")) countFunctional++;
                }
            }
            Assert.AreEqual(3, countOptional);
            Assert.AreEqual(2, countObject);
            Assert.AreEqual(2, countPredicative);
            Assert.AreEqual(2, countFunctional);

        }

        [DataRow("prty", "obj")]
        [DataRow("property", "object")]
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
            Assert.AreEqual(2, counterSnippets);
        }

    }
}
