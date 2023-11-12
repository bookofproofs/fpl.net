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

        [DataRow("prty", "obj")]
        [DataRow("property", "object")]
        [DataRow("prty", "predicate")]
        [DataRow("property", "predicate")]
        [DataRow("prty", "function")]
        [DataRow("property", "function")]
        [TestMethod]
        public void TestAddPropertyChoicesSortText(string choice, string subType)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains("property"));
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
                if (item.Label.Contains(subType) && item.Label.Contains("opt"))
                {
                    if (choice == "prty")
                    {
                        Assert.AreEqual("_ " + choice + " opt " + subType, item.Label);
                    }
                    else
                    {
                        Assert.AreEqual("_ " + choice + " optional " + subType, item.Label);
                    }
                    counterRelatedOpt++;
                }
                else if (item.Label.Contains(subType))
                {
                    Assert.AreEqual("_ " + choice + " " + subType, item.Label);
                    counterRelated++;
                }
            }
            Assert.AreEqual(2, counterRelated);
            Assert.AreEqual(2, counterRelatedOpt);
        }

        [DataRow("prty", "pr")]
        [DataRow("property", "property")]
        [TestMethod]
        public void TestAddPropertyChoicesDetail(string choice, string l)
        {
            var detailCi = new FplCompletionItem(choice);
            var actual = new FplCompletionItemChoicesProperty().GetChoices(detailCi);
            var countMandatory = 0;
            var countOptional = 0;
            var countObject = 0;
            var countPredicative = 0;
            var countFunctional = 0;
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Detail.Contains(l));
                if (item.Detail.Contains("mandatory")) countMandatory++;
                if (item.Detail.Contains("opt")) countOptional++;
                if (item.Detail.Contains("obj")) countObject++;
                if (item.Detail.Contains("pred")) countPredicative++;
                if (item.Detail.Contains("func")) countFunctional++;
            }
            Assert.AreEqual(3, countMandatory);
            Assert.AreEqual(6, countOptional);
            Assert.AreEqual(4, countObject);
            Assert.AreEqual(4, countPredicative);
            Assert.AreEqual(4, countFunctional);

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
