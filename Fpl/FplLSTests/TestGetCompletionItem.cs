using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLSTests
{
    [TestClass]
    public class TestFplAutoCompleteService
    {
        [TestMethod]
        [DataRow("<some token>", "some token")]
        [DataRow("'token'", "token")]
        [DataRow("some token", "some token")]
        [DataRow("token", "token")]
        [DataRow("'token", "'token")]
        [DataRow("token'", "token'")]
        [DataRow("<token", "<token")]
        [DataRow("token>", "token>")]
        public void TestGetCompletionItemStrippingBracketsAndQuotes(string input, string expected)
        {
            var actual = FplAutoCompleteService.GetCompletionItem(input);
            Assert.AreEqual(expected, actual.Label);
        }

        [TestMethod]
        [DataRow("ax")]
        [DataRow("axiom")]
        [DataRow("post")]
        [DataRow("postulate")]
        [DataRow("thm")]
        [DataRow("theorem")]
        [DataRow("lem")]
        [DataRow("lemma")]
        [DataRow("prop")]
        [DataRow("proposition")]
        [DataRow("cor")]
        [DataRow("corollary")]
        [DataRow("ass")]
        [DataRow("assume")]
        [DataRow("cl")]
        [DataRow("class")]
        [DataRow("conj")]
        [DataRow("conjecture")]
        [DataRow("con")]
        [DataRow("conclusion")]
        [DataRow("ctr")]
        [DataRow("constructor")]
        [DataRow("dec")]
        [DataRow("declaration")]
        [DataRow("def")]
        [DataRow("definition")]
        [DataRow("del")]
        [DataRow("delegate")]
        [DataRow("func")]
        [DataRow("function")]
        [DataRow("ind")]
        [DataRow("index")]
        [DataRow("intr")]
        [DataRow("intrinsic")]
        [DataRow("inf")]
        [DataRow("inference")]
        [DataRow("loc")]
        [DataRow("localization")]
        [DataRow("mand")]
        [DataRow("mandatory")]
        [DataRow("obj")]
        [DataRow("object")]
        [DataRow("opt")]
        [DataRow("optional")]
        [DataRow("pred")]
        [DataRow("predicate")]
        [DataRow("pre")]
        [DataRow("premise")]
        [DataRow("prf")]
        [DataRow("proof")]
        [DataRow("ret")]
        [DataRow("return")]
        [DataRow("rev")]
        [DataRow("revoke")]
        [DataRow("undef")]
        [DataRow("undefined")]
        public void TestGetCompletionItemLabels(string input)
        {
            var actual = FplAutoCompleteService.GetCompletionItem(input);
            Assert.AreEqual(input, actual.Label);
        }

        [TestMethod]
        [DataRow("ax", "axiom (short form)")]
        [DataRow("post", "postulate (short form)")]
        [DataRow("thm", "theorem (short form)")]
        [DataRow("lem", "lemma (short form)")]
        [DataRow("prop", "proposition (short form)")]
        [DataRow("cor", "corollary (short form)")]
        [DataRow("ass", "argument (assume, short form)")]
        [DataRow("cl", "class (short form)")]
        [DataRow("conj", "conjecture (short form)")]
        [DataRow("con", "conclusion (short form)")]
        [DataRow("ctr", "constructor (short form)")]
        [DataRow("dec", "declaration (short form)")]
        [DataRow("def", "definition (short form)")]
        [DataRow("del", "delegate (short form)")]
        [DataRow("func", "type (functional term, short form)")]
        [DataRow("ind", "type (index, short form)")]
        [DataRow("intr", "intrinsic (short form)")]
        [DataRow("inf", "rule of inference (short form)")]
        [DataRow("loc", "localization (short form)")]
        [DataRow("mand", "mandatory (short form)")]
        [DataRow("obj", "type (object, short form)")]
        [DataRow("opt", "optional (short form)")]
        [DataRow("pred", "type (predicate, short form)")]
        [DataRow("pre", "premise (short form)")]
        [DataRow("prf", "proof (short form)")]
        [DataRow("ret", "statement (return, short form)")]
        [DataRow("rev", "argument (revoke, short form)")]
        [DataRow("undef", "undefined (short form)")]
        public void TestGetDetailShortForms(string input, string expected)
        {
            var actual = FplAutoCompleteService.GetDetail(input, out string sortText);
            Assert.AreEqual(expected, actual);
        }

        [TestMethod]
        [DataRow("axiom", "axiom")]
        [DataRow("postulate", "postulate")]
        [DataRow("theorem", "theorem")]
        [DataRow("lemma", "lemma")]
        [DataRow("proposition", "proposition")]
        [DataRow("corollary", "corollary")]
        [DataRow("assume", "argument (assume)")]
        [DataRow("class", "class")]
        [DataRow("conjecture", "conjecture")]
        [DataRow("conclusion", "conclusion")]
        [DataRow("constructor", "constructor")]
        [DataRow("declaration", "declaration")]
        [DataRow("definition", "definition")]
        [DataRow("delegate", "delegate")]
        [DataRow("function", "type (functional term)")]
        [DataRow("index", "type (index)")]
        [DataRow("intrinsic", "intrinsic")]
        [DataRow("inference", "rule of inference")]
        [DataRow("localization", "localization")]
        [DataRow("mandatory", "mandatory")]
        [DataRow("object", "type (object)")]
        [DataRow("optional", "optional")]
        [DataRow("predicate", "type (predicate)")]
        [DataRow("premise", "premise")]
        [DataRow("proof", "proof")]
        [DataRow("return", "statement (return)")]
        [DataRow("revoke", "argument (revoke)")]
        [DataRow("undefined", "undefined")]
        // keywords without a short form
        [DataRow("alias", "alias")]
        [DataRow("all", "predicate (all quantor)")]
        [DataRow("and", "predicate (conjunction)")]
        [DataRow("assert", "statement (assert)")]
        [DataRow("cases", "statement (cases)")]
        [DataRow("end", "extension (end of)")]
        [DataRow("ext", "extension (beginning of)")]
        [DataRow("ex", "predicate (exists quantor)")]
        [DataRow("exn", "predicate (exists n-times quantor)")]
        [DataRow("false", "predicate (false)")]
        [DataRow("for", "statement (for loop)")]
        [DataRow("iif", "predicate (equivalence, <=>)")]
        [DataRow("impl", "predicate (implication, =>)")]
        [DataRow("in", "clause (in type or in range)")]
        [DataRow("is", "predicate (is of type)")]
        [DataRow("not", "predicate (negation)")]
        [DataRow("or", "predicate (disjunction)")]
        [DataRow("qed", "conclusion (quod erat demonstrandum)")]
        [DataRow("self", "reference (to self)")]
        [DataRow("trivial", "argument (trivial)")]
        [DataRow("true", "predicate (true)")]
        [DataRow("uses", "clause (uses)")]
        [DataRow("xor", "predicate (exclusive or)")]
        public void TestGetDetailLongForms(string input, string expected)
        {
            var actual = FplAutoCompleteService.GetDetail(input, out string sortText);
            Assert.AreEqual(expected, actual);
        }


        [TestMethod]
        [DataRow("ax", "axiom")]
        [DataRow("axiom", "axiom")]
        [DataRow("post", "postulate")]
        [DataRow("postulate", "postulate")]
        [DataRow("thm", "theorem")]
        [DataRow("theorem", "theorem")]
        [DataRow("lem", "lemma")]
        [DataRow("lemma", "lemma")]
        [DataRow("prop", "proposition")]
        [DataRow("proposition", "proposition")]
        [DataRow("cor", "corollary")]
        [DataRow("corollary", "corollary")]
        [DataRow("ass", "assume")]
        [DataRow("assume", "assume")]
        [DataRow("cl", "class")]
        [DataRow("class", "class")]
        [DataRow("conj", "conjecture")]
        [DataRow("conjecture", "conjecture")]
        [DataRow("con", "conclusion")]
        [DataRow("conclusion", "conclusion")]
        [DataRow("ctr", "constructor")]
        [DataRow("constructor", "constructor")]
        [DataRow("dec", "declaration")]
        [DataRow("declaration", "declaration")]
        [DataRow("def", "definition")]
        [DataRow("definition", "definition")]
        [DataRow("del", "delegate")]
        [DataRow("delegate", "delegate")]
        [DataRow("func", "function")]
        [DataRow("function", "function")]
        [DataRow("ind", "index")]
        [DataRow("index", "index")]
        [DataRow("intr", "intrinsic")]
        [DataRow("intrinsic", "intrinsic")]
        [DataRow("inf", "inference")]
        [DataRow("inference", "inference")]
        [DataRow("loc", "localization")]
        [DataRow("localization", "localization")]
        [DataRow("mand", "mandatory")]
        [DataRow("mandatory", "mandatory")]
        [DataRow("obj", "object")]
        [DataRow("object", "object")]
        [DataRow("opt", "optional")]
        [DataRow("optional", "optional")]
        [DataRow("pred", "predicate")]
        [DataRow("predicate", "predicate")]
        [DataRow("pre", "premise")]
        [DataRow("premise", "premise")]
        [DataRow("prf", "proof")]
        [DataRow("proof", "proof")]
        [DataRow("ret", "return")]
        [DataRow("return", "return")]
        [DataRow("rev", "revoke")]
        [DataRow("revoke", "revoke")]
        [DataRow("undef", "undefined")]
        [DataRow("undefined", "undefined")]
        // keywords without a short form
        [DataRow("alias", "alias")]
        [DataRow("all", "all")]
        [DataRow("and", "and")]
        [DataRow("assert", "assert")]
        [DataRow("cases", "cases")]
        [DataRow("end", "end")]
        [DataRow("ext", "ext")]
        [DataRow("ex", "ex")]
        [DataRow("exn", "exn")]
        [DataRow("false", "false")]
        [DataRow("for", "for")]
        [DataRow("iif", "iif")]
        [DataRow("impl", "impl")]
        [DataRow("in", "in")]
        [DataRow("is", "is")]
        [DataRow("not", "not")]
        [DataRow("or", "or")]
        [DataRow("qed", "qed")]
        [DataRow("self", "self")]
        [DataRow("trivial", "trivial")]
        [DataRow("true", "true")]
        [DataRow("uses", "uses")]
        [DataRow("xor", "xor")]
        public void TestGetDetailSortTexts(string input, string expected)
        {
            var actual = FplAutoCompleteService.GetDetail(input, out string sortText);
            Assert.AreEqual(expected, sortText);
        }

        [TestMethod]
        [DataRow("ax", "axiom04")]
        [DataRow("axiom", "axiom02")]
        [DataRow("post", "postulate04")]
        [DataRow("postulate", "postulate02")]
        [DataRow("thm", "theorem04")]
        [DataRow("theorem", "theorem02")]
        [DataRow("lem", "lemma04")]
        [DataRow("lemma", "lemma02")]
        [DataRow("prop", "proposition04")]
        [DataRow("proposition", "proposition02")]
        [DataRow("cor", "corollary04")]
        [DataRow("corollary", "corollary02")]
        [DataRow("ass", "assume04")]
        [DataRow("assume", "assume02")]
        [DataRow("cl", "class04")]
        [DataRow("class", "class02")]
        [DataRow("conj", "conjecture04")]
        [DataRow("conjecture", "conjecture02")]
        [DataRow("con", "conclusion04")]
        [DataRow("conclusion", "conclusion02")]
        [DataRow("ctr", "constructor04")]
        [DataRow("constructor", "constructor02")]
        [DataRow("dec", "declaration04")]
        [DataRow("declaration", "declaration02")]
        [DataRow("def", "definition04")]
        [DataRow("definition", "definition02")]
        [DataRow("del", "delegate04")]
        [DataRow("delegate", "delegate02")]
        [DataRow("func", "function04")]
        [DataRow("function", "function02")]
        [DataRow("ind", "index04")]
        [DataRow("index", "index02")]
        [DataRow("intr", "intrinsic04")]
        [DataRow("intrinsic", "intrinsic02")]
        [DataRow("inf", "inference04")]
        [DataRow("inference", "inference02")]
        [DataRow("loc", "localization04")]
        [DataRow("localization", "localization02")]
        [DataRow("mand", "mandatory04")]
        [DataRow("mandatory", "mandatory02")]
        [DataRow("obj", "object04")]
        [DataRow("object", "object02")]
        [DataRow("opt", "optional04")]
        [DataRow("optional", "optional02")]
        [DataRow("pred", "predicate04")]
        [DataRow("predicate", "predicate02")]
        [DataRow("pre", "premise04")]
        [DataRow("premise", "premise02")]
        [DataRow("prf", "proof04")]
        [DataRow("proof", "proof02")]
        [DataRow("ret", "return04")]
        [DataRow("return", "return02")]
        [DataRow("rev", "revoke04")]
        [DataRow("revoke", "revoke02")]
        [DataRow("undef", "undefined04")]
        [DataRow("undefined", "undefined02")]
        // keywords without a short form
        [DataRow("alias", "alias02")]
        [DataRow("all", "all02")]
        [DataRow("and", "and02")]
        [DataRow("assert", "assert02")]
        [DataRow("cases", "cases02")]
        [DataRow("end", "end02")]
        [DataRow("ext", "ext02")]
        [DataRow("ex", "ex02")]
        [DataRow("exn", "exn02")]
        [DataRow("false", "false02")]
        [DataRow("for", "for02")]
        [DataRow("iif", "iif02")]
        [DataRow("impl", "impl02")]
        [DataRow("in", "in02")]
        [DataRow("is", "is02")]
        [DataRow("not", "not02")]
        [DataRow("or", "or02")]
        [DataRow("qed", "qed02")]
        [DataRow("self", "self02")]
        [DataRow("trivial", "trivial02")]
        [DataRow("true", "true02")]
        [DataRow("uses", "uses02")]
        [DataRow("xor", "xor02")]
        public void TestGetCompletionItemSortTextsForKeywords(string input, string expected)
        {
            var actual = FplAutoCompleteService.GetCompletionItem(input);
            Console.WriteLine(input);
            Assert.AreEqual(expected, actual.SortText);
        }

        [TestMethod]
        [DataRow("ax", "axiom03")]
        [DataRow("axiom", "axiom01")]
        [DataRow("post", "postulate03")]
        [DataRow("postulate", "postulate01")]
        [DataRow("thm", "theorem03")]
        [DataRow("theorem", "theorem01")]
        [DataRow("lem", "lemma03")]
        [DataRow("lemma", "lemma01")]
        [DataRow("prop", "proposition03")]
        [DataRow("proposition", "proposition01")]
        [DataRow("cor", "corollary03")]
        [DataRow("corollary", "corollary01")]
        [DataRow("ass", "assume03")]
        [DataRow("assume", "assume01")]
        [DataRow("cl", "class03")]
        [DataRow("class", "class01")]
        [DataRow("conj", "conjecture03")]
        [DataRow("conjecture", "conjecture01")]
        [DataRow("con", "conclusion03")]
        [DataRow("conclusion", "conclusion01")]
        [DataRow("ctr", "constructor03")]
        [DataRow("constructor", "constructor01")]
        [DataRow("dec", "declaration03")]
        [DataRow("declaration", "declaration01")]
        [DataRow("def", "definition03")]
        [DataRow("definition", "definition01")]
        [DataRow("del", "delegate03")]
        [DataRow("delegate", "delegate01")]
        [DataRow("func", "function03")]
        [DataRow("function", "function01")]
        [DataRow("ind", "index03")]
        [DataRow("index", "index01")]
        [DataRow("intr", "intrinsic03")]
        [DataRow("intrinsic", "intrinsic01")]
        [DataRow("inf", "inference03")]
        [DataRow("inference", "inference01")]
        [DataRow("loc", "localization03")]
        [DataRow("localization", "localization01")]
        [DataRow("mand", "mandatory03")]
        [DataRow("mandatory", "mandatory01")]
        [DataRow("obj", "object03")]
        [DataRow("object", "object01")]
        [DataRow("opt", "optional03")]
        [DataRow("optional", "optional01")]
        [DataRow("pred", "predicate03")]
        [DataRow("predicate", "predicate01")]
        [DataRow("pre", "premise03")]
        [DataRow("premise", "premise01")]
        [DataRow("prf", "proof03")]
        [DataRow("proof", "proof01")]
        [DataRow("ret", "return03")]
        [DataRow("return", "return01")]
        [DataRow("rev", "revoke03")]
        [DataRow("revoke", "revoke01")]
        [DataRow("undef", "undefined03")]
        [DataRow("undefined", "undefined01")]
        // keywords without a short form
        [DataRow("alias", "alias01")]
        [DataRow("all", "all01")]
        [DataRow("and", "and01")]
        [DataRow("assert", "assert01")]
        [DataRow("cases", "cases01")]
        [DataRow("end", "end01")]
        [DataRow("ext", "ext01")]
        [DataRow("ex", "ex01")]
        [DataRow("exn", "exn01")]
        [DataRow("false", "false01")]
        [DataRow("for", "for01")]
        [DataRow("iif", "iif01")]
        [DataRow("impl", "impl01")]
        [DataRow("in", "in01")]
        [DataRow("is", "is01")]
        [DataRow("not", "not01")]
        [DataRow("or", "or01")]
        [DataRow("qed", "qed01")]
        [DataRow("self", "self01")]
        [DataRow("trivial", "trivial01")]
        [DataRow("true", "true01")]
        [DataRow("uses", "uses01")]
        [DataRow("xor", "xor01")]
        public void TestGetCompletionItemSortTextsForSnippets(string input, string expected)
        {
            var actual = FplAutoCompleteService.GetCompletionItem(input, "xxx");
            Console.WriteLine(input);
            Assert.AreEqual(expected, actual.SortText);
        }


        [DataRow("all", 6)]
        [DataRow("ex", 6)]
        [DataRow("exn", 5)]
        [TestMethod]
        public void TestAddQuantorChoicesNumber(string choice, int number)
        {
            var actual = FplAutoCompleteService.AddQuantorChoices(choice);
            Assert.AreEqual(number, actual.Count);
        }

        [DataRow("all", 5)]
        [DataRow("ex", 5)]
        [DataRow("exn", 4)]
        [TestMethod]
        public void TestAddQuantorSnippetCounts(string choice, int number)
        {
            var actual = FplAutoCompleteService.AddQuantorChoices(choice);
            var count = 0;
            foreach(var item in actual)
            {
                if (item.Kind == CompletionItemKind.Snippet) count++;
            }
            Assert.AreEqual(number, count);
        }

        [DataRow("all", 1)]
        [DataRow("ex", 1)]
        [DataRow("exn", 1)]
        [TestMethod]
        public void TestAddQuantorKeywordCounts(string choice, int number)
        {
            var actual = FplAutoCompleteService.AddQuantorChoices(choice);
            var count = 0;
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Keyword) count++;
            }
            Assert.AreEqual(number, count);
        }

        [DataRow("all")]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddQuantorChoicesSortText(string choice)
        {
            var actual = FplAutoCompleteService.AddQuantorChoices(choice);
            var lastSortText = "";
            foreach (var item in actual)
            {
                Assert.IsTrue(item.SortText.Contains(choice));
                Assert.IsTrue(string.Compare(lastSortText,item.SortText)<0);
                lastSortText = item.SortText;
            }
        }

        [DataRow("all")]
        [DataRow("ex")]
        [DataRow("exn")]
        [TestMethod]
        public void TestAddQuantorChoicesLabel(string choice)
        {
            var actual = FplAutoCompleteService.AddQuantorChoices(choice);
            foreach (var item in actual)
            {
                Assert.IsTrue(item.Label.Contains(choice));
            }
        }

        [DataRow("all", "all")]
        [DataRow("ex", "exists")]
        [DataRow("exn", "n-times")]
        [TestMethod]
        public void TestAddQuantorChoicesDetail(string choice, string s)
        {
            var actual = FplAutoCompleteService.AddQuantorChoices(choice);
            foreach (var item in actual)
            {
                if (item.Kind == CompletionItemKind.Snippet)
                {
                    Assert.IsTrue(item.Detail.Contains(s));
                }
                else
                {
                    Assert.IsTrue(item.Detail=="");
                }
            }
        }

    }
}
