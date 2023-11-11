using FParsec;
using Microsoft.FSharp.Core;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.ComponentModel.Design;
using System.Data;
using System.Diagnostics.Tracing;
using System.Text;


namespace FplLS
{
    public enum KeywordKind
    {
        Short,
        Long,
        EitherNor
    }

    public class FplAutoCompleteService
    {
        const string prefix = "_ ";

        public async Task<CompletionList> GetParserChoices(StringBuilder builder, int index)
        {
            // make sure we get the parser choices from the position before the typed character, not after it
            string s;
            if (index > 0)
            {
                s = builder.ToString().Substring(0, index - 1) + "§";
            }
            else
            {
                s = builder.ToString().Substring(0, index);
            }
            var choicesTuple = FplParser.getParserChoicesAtPosition(s, index);
            var choices = choicesTuple.Item1;
            var modChoices = new List<CompletionItem>();
            foreach (var choice in choices)
            {
                var word = StripQuotesOrBrackets(choice);
                CompletionItem defaultCi = GetDetail(word);
                switch (word)
                {
                    case "ISO 639 language code":
                        modChoices.AddRange(AddIso639Choices());
                        break;
                    case "whitespace":
                    case "significant whitespace":
                        modChoices.AddRange(AddWhitespaceChoices());
                        break;
                    case "(closed) left bound":
                    case "(open) left bound":
                    case "(open) right bound":
                    case "(closed) right bound":
                        modChoices.AddRange(AddBoundChoices(word));
                        break;
                    case "digits":
                        modChoices.AddRange(AddDigitsChoices(word));
                        break;
                    case "argument identifier":
                        modChoices.AddRange(AddArgumentIdentifierChoices(word));
                        break;
                    case "language-specific string":
                        modChoices.AddRange(AddLanguageSpecificStringChoices(word));
                        break;
                    case "extension regex":
                        modChoices.AddRange(AddExtensionRegexChoices(word));
                        break;
                    case "word":
                        modChoices.AddRange(AddWordChoices(word));
                        break;
                    case "variable":
                    case "variable (got keyword)":
                    case "variable (got template)":
                        modChoices.AddRange(AddVariableChoices(word));
                        break;
                    case "PascalCaseId":
                        modChoices.AddRange(AddPascalCaseIdChoices(word));
                        break;
                    case "del":
                    case "delegate":
                        modChoices.AddRange(AddDelegateChoices(word, defaultCi));
                        break;
                    case "is":
                        modChoices.AddRange(AddIsOperatorChoices(word, defaultCi));
                        break;
                    case "alias":
                    case "assert":
                    case "ass":
                    case "assume":
                    case "cl":
                    case "class":
                    case "con":
                    case "conclusion":
                    case "end":
                    case "ext":
                    case "func":
                    case "function":
                    case "ind":
                    case "index":
                    case "intr":
                    case "intrinsic":
                    case "in":
                    case "obj":
                    case "object":
                    case "opt":
                    case "optional":
                    case "pred":
                    case "predicate":
                    case "pre":
                    case "premise":
                    case "qed":
                    case "ret":
                    case "return":
                    case "rev":
                    case "revoke":
                    case "trivial":
                        modChoices.AddRange(AddKeywordChoices(word, defaultCi));
                        break;
                    case "self":
                    case "@":
                        modChoices.AddRange(AddSelfChoices(word));
                        break;
                    case "true":
                    case "false":
                    case "undef":
                    case "undefined":
                        modChoices.AddRange(AddPredicateChoices(word, 0, defaultCi));
                        break;
                    case "all":
                    case "ex":
                    case "exn":
                        modChoices.AddRange(AddQuantorChoices(word, defaultCi));
                        break;
                    case "not":
                        modChoices.AddRange(AddPredicateChoices(word, 1, defaultCi));
                        break;
                    case "xor":
                    case "iif":
                    case "impl":
                        modChoices.AddRange(AddPredicateChoices(word, 2, defaultCi));
                        break;
                    case "and":
                    case "or":
                        modChoices.AddRange(AddPredicateChoices(word, 3, defaultCi));
                        break;
                    case "ctor":
                    case "constructor":
                        modChoices.AddRange(AddConstructorChoices(word, defaultCi));
                        break;
                    case "dec":
                    case "declaration":
                        modChoices.AddRange(AddDeclarationChoices(word, defaultCi));
                        break;
                    case "cases":
                        modChoices.AddRange(AddCasesChoices(word, defaultCi));
                        break;
                    case "for":
                        modChoices.AddRange(AddForChoices(word, defaultCi));
                        break;
                    case "prty":
                    case "property":
                        modChoices.AddRange(AddPropertyChoices(word));
                        break;
                    case "ax":
                    case "axiom":
                    case "post":
                    case "postulate":
                        modChoices.AddRange(AddAxiomChoices(word, defaultCi));
                        break;
                    case "def":
                    case "definition":
                        modChoices.AddRange(AddDefinitionChoices(word));
                        break;
                    case "thm":
                    case "theorem":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Theorem", defaultCi));
                        break;
                    case "lem":
                    case "lemma":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Lemma", defaultCi));
                        break;
                    case "prop":
                    case "proposition":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Proposition", defaultCi));
                        break;
                    case "inf":
                    case "inference":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Inference", defaultCi));
                        break;
                    case "conj":
                    case "conjecture":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Conjecture", defaultCi));
                        break;
                    case "cor":
                    case "corollary":
                        modChoices.AddRange(AddCorollaryChoices(word, defaultCi));
                        break;
                    case "prf":
                    case "proof":
                        modChoices.AddRange(AddProofChoices(word, defaultCi));
                        break;
                    case "loc":
                    case "localization":
                        modChoices.AddRange(AddLocalizationChoices(word, defaultCi));
                        break;
                    case "uses":
                        modChoices.AddRange(AddUsesChoices(word, defaultCi));
                        break;
                    default:
                        modChoices.AddRange(AddDefaultChoices(word));
                        break;
                }
            }
            return new CompletionList(modChoices);
        }

        public static List<CompletionItem> AddAxiomChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            // snippet
            defaultCi.InsertText = $"{word} SomeFplIdentifier (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\ttrue{Environment.NewLine}" + "}" + Environment.NewLine;
            modChoices.Add(defaultCi);
            // keyword
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        public static List<CompletionItem> AddPredicateChoices(string word, int numbOfArgs, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            switch (numbOfArgs)
            {
                case 0:
                    // no snippets for null-ary predicates (treat them as keywords only - see below)
                    break;
                case 1:
                    var ci = GetCompletionItem(word, $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine})" + Environment.NewLine);
                    ci.Detail = defaultCi.Detail;
                    ci.SortText = defaultCi.SortText;
                    ci.Label = defaultCi.Label;
                    ci.Kind = defaultCi.Kind;
                    modChoices.Add(ci);
                    break;
                case 2:
                    var ci1 = GetCompletionItem(word, $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\tfalse{Environment.NewLine})" + Environment.NewLine);
                    ci1.Detail = defaultCi.Detail;
                    ci1.SortText = defaultCi.SortText;
                    ci1.Label = defaultCi.Label;
                    ci1.Kind = defaultCi.Kind;
                    modChoices.Add(ci1);
                    break;
                default:
                    var ci2 = GetCompletionItem(word, $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\tfalse{Environment.NewLine})" + Environment.NewLine);
                    ci2.Detail = defaultCi.Detail;
                    ci2.SortText = defaultCi.SortText;
                    ci2.Label = defaultCi.Label;
                    ci2.Kind = defaultCi.Kind;
                    modChoices.Add(ci2);
                    break;
            }
            // keywords
            modChoices.Add(defaultCi);
            return modChoices;
        }

        public static List<CompletionItem> AddQuantorChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            var postfix = "";
            var isExn = word.Contains("exn");
            if (isExn)
            {
                postfix = "!1";
            }
            // snippets
            var ci = GetCompletionItem(word, $"<replace>{postfix} x ({Environment.NewLine}" + $"\tp(x){Environment.NewLine})" + Environment.NewLine);
            ci.Label = $"{prefix}ex (..)"; ReplaceLabel(word, ci);
            ci.Detail = "predicate (exists quantor)"; ReplaceDetails(word, ci);
            ci.SortText = "ex00"; ReplaceSortText(word, ci);
            ci.Kind = defaultCi.Kind;
            modChoices.Add(ci);
            var ci1 = GetCompletionItem(word, $"<replace>{postfix} x in someVariadicVar ({Environment.NewLine}" + $"\tp(x){Environment.NewLine})" + Environment.NewLine);
            ci1.Label = $"{prefix}ex x in variable (..)"; ReplaceLabel(word, ci1);
            ci1.Detail = "predicate (exists quantor)"; ReplaceDetails(word, ci1);
            ci1.SortText = "ex01"; ReplaceSortText(word, ci1);
            ci1.Kind = defaultCi.Kind;
            modChoices.Add(ci1);
            var ci2 = GetCompletionItem(word, $"<replace>{postfix} x in [a,b] ({Environment.NewLine}" + $"\tp(x){Environment.NewLine})" + Environment.NewLine);
            ci2.Label = $"{prefix}ex x in range (..)"; ReplaceLabel(word, ci2);
            ci2.Detail = "predicate (exists quantor)"; ReplaceDetails(word, ci2);
            ci2.SortText = "ex02"; ReplaceSortText(word, ci2);
            ci2.Kind = defaultCi.Kind;

            modChoices.Add(ci2);
            var ci3 = GetCompletionItem(word, $"<replace>{postfix} x in object ({Environment.NewLine}" + $"\tp(x){Environment.NewLine})" + Environment.NewLine);
            ci3.Label = $"{prefix}ex x in type (..)"; ReplaceLabel(word, ci3);
            ci3.Detail = "predicate (exists quantor)"; ReplaceDetails(word, ci3);
            ci3.SortText = "ex03"; ReplaceSortText(word, ci3);
            ci3.Kind = defaultCi.Kind;
            modChoices.Add(ci3);

            if (isExn)
            {
                // keywords
                var ci4 = GetCompletionItem("exn!");
                ci4.SortText = "exn!04";
                ci4.Kind = CompletionItemKind.Keyword;
                modChoices.Add(ci4);
            }
            else
            {
                // this combined snippet only available for 'ex' and 'all' but not for exn!
                var ci5 = GetCompletionItem(word, $"<replace> x in [a,b], y in c, z ({Environment.NewLine}" + $"\tp(x,y,z){Environment.NewLine})" + Environment.NewLine);
                ci5.Label = $"{prefix}ex <combined> (..)"; ReplaceLabel(word, ci5);
                ci5.Detail = "predicate (exists quantor in <combined>)"; ReplaceDetails(word, ci5);
                ci5.SortText = "ex04"; ReplaceSortText(word, ci5);
                ci5.Kind = defaultCi.Kind;
                modChoices.Add(ci5);
                // keywords
                var ci6 = GetCompletionItem(word);
                ci6.SortText = "ex05"; ReplaceSortText(word, ci6);
                ci6.Kind = CompletionItemKind.Keyword;
                modChoices.Add(ci6);
            }
            return modChoices;
        }

        private static void ReplaceLabel(string word, CompletionItem ci)
        {
            if (word.Contains("all"))
                ci.Label = ci.Label.Replace("ex", "all");
            else if (word.Contains("exn"))
                ci.Label = ci.Label.Replace("ex", "exn!");
        }

        private static void ReplaceSortText(string word, CompletionItem ci)
        {
            if (word.Contains("all"))
                ci.SortText = ci.SortText.Replace("ex", "all");
            else if (word.Contains("exn"))
                ci.SortText = ci.SortText.Replace("ex", "exn!");
        }

        private static void ReplaceDetails(string word, CompletionItem ci)
        {
            if (word.Contains("all"))
                ci.Detail = ci.Detail.Replace("exists", "all");
            else if (word.Contains("exn"))
                ci.Detail = ci.Detail.Replace("exists", "exists n-times");
        }

        public static List<CompletionItem> AddConstructorChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            // snippet
            defaultCi.InsertText = GetConstructorSnippet(word);

            modChoices.Add(defaultCi);
            // keyword
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        public static List<CompletionItem> AddPropertyChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippet
            var ciMandCl = GetCompletionItem(word, GetClassInstanceSnippet(word, false, out string optMand, out string objMand));
            ciMandCl.Label = $"_ property object";
            ciMandCl.Detail = $"mandatory object property";
            ciMandCl.SortText = "property01";
            if (word == "prty") { ciMandCl.SortText = "z" + ciMandCl.SortText; ciMandCl.Detail += " (short)"; ciMandCl.Label = ciMandCl.Label.Replace("property", "prty").Replace("object", "obj"); }
            modChoices.Add(ciMandCl);
            var ciMandPr = GetCompletionItem(word, GetPredicateInstanceSnippet(word, false));
            ciMandPr.Label = $"_ property predicate";
            ciMandPr.Detail = $"mandatory predicative property";
            ciMandPr.SortText = "property02";
            if (word == "prty") { ciMandPr.SortText = "z" + ciMandPr.SortText; ciMandPr.Detail += " (short)"; ciMandPr.Label = ciMandPr.Label.Replace("property", "prty").Replace("predicate", "pred"); }
            modChoices.Add(ciMandPr);
            var ciMandFu = GetCompletionItem(word, GetFunctionalTermInstanceSnippet(word, false));
            ciMandFu.Label = $"_ property function";
            ciMandFu.Detail = $"mandatory functional property";
            ciMandFu.SortText = "property03";
            if (word == "prty") { ciMandFu.SortText = "z" + ciMandFu.SortText; ciMandFu.Detail += " (short)"; ciMandFu.Label = ciMandFu.Label.Replace("property", "prty").Replace("function", "func"); }
            modChoices.Add(ciMandFu);
            var ciOptCl = GetCompletionItem(word, GetClassInstanceSnippet(word, true, out string optOpt, out string objOpt));
            ciOptCl.Label = $"_ property optional object";
            ciOptCl.Detail = $"optional object property";
            ciOptCl.SortText = "property04";
            if (word == "prty") { ciOptCl.SortText = "z" + ciOptCl.SortText; ciOptCl.Detail += " (short)"; ciOptCl.Label = ciOptCl.Label.Replace("property", "prty").Replace("object", "obj").Replace("optional", "opt"); }
            modChoices.Add(ciOptCl);
            var ciOptPr = GetCompletionItem(word, GetPredicateInstanceSnippet(word, true));
            ciOptPr.Label = $"_ property optional predicate";
            ciOptPr.Detail = $"optional predicative property";
            ciOptPr.SortText = "property05";
            if (word == "prty") { ciOptPr.SortText = "z" + ciOptPr.SortText; ciOptPr.Detail += " (short)"; ciOptPr.Label = ciOptPr.Label.Replace("property", "prty").Replace("predicate", "pred").Replace("optional", "opt"); }
            modChoices.Add(ciOptPr);
            var ciOptFu = GetCompletionItem(word, GetFunctionalTermInstanceSnippet(word, true));
            ciOptFu.Label = $"_ property optional function";
            ciOptFu.Detail = $"optional functional property";
            ciOptFu.SortText = "property06";
            if (word == "prty") { ciOptFu.SortText = "z" + ciOptFu.SortText; ciOptFu.Detail += " (short)"; ciOptFu.Label = ciOptFu.Label.Replace("property", "prty").Replace("function", "func").Replace("optional", "opt"); }
            modChoices.Add(ciOptFu);
            // keyword
            var ciMandClKw = GetCompletionItem(word);
            ciMandClKw.InsertText = ciMandCl.Label.Substring(2);
            ciMandClKw.Label = prefix + ciMandClKw.InsertText;
            ciMandClKw.Detail = $"keywords '{ciMandClKw.InsertText}'";
            ciMandClKw.SortText = "zzz" + ciMandCl.SortText;
            modChoices.Add(ciMandClKw);
            var ciMandPrKw = GetCompletionItem(word);
            ciMandPrKw.InsertText = ciMandPr.Label.Substring(2);
            ciMandPrKw.Label = prefix + ciMandPrKw.InsertText;
            ciMandPrKw.Detail = $"keywords '{ciMandPrKw.InsertText}'";
            ciMandPrKw.SortText = "zzz" + ciMandPr.SortText;
            modChoices.Add(ciMandPrKw);
            var ciMandFuKw = GetCompletionItem(word);
            ciMandFuKw.InsertText = ciMandFu.Label.Substring(2);
            ciMandFuKw.Label = prefix + ciMandFuKw.InsertText;
            ciMandFuKw.Detail = $"keywords '{ciMandFuKw.InsertText}'";
            ciMandFuKw.SortText = "zzz" + ciMandFu.SortText;
            modChoices.Add(ciMandFuKw);
            var ciOptClKw = GetCompletionItem(word);
            ciOptClKw.InsertText = ciOptCl.Label.Substring(2);
            ciOptClKw.Label = prefix + ciOptClKw.InsertText;
            ciOptClKw.Detail = $"keywords '{ciOptClKw.InsertText}'";
            ciOptClKw.SortText = "zzz" + ciOptCl.SortText;
            modChoices.Add(ciOptClKw);
            var ciOptPrKw = GetCompletionItem(word);
            ciOptPrKw.InsertText = ciOptPr.Label.Substring(2);
            ciOptPrKw.Label = prefix + ciOptPrKw.InsertText;
            ciOptPrKw.Detail = $"keywords '{ciOptPrKw.InsertText}'";
            ciOptPrKw.SortText = "zzz" + ciOptPr.SortText;
            modChoices.Add(ciOptPrKw);
            var ciOptFuKw = GetCompletionItem(word);
            ciOptFuKw.InsertText = ciOptFu.Label.Substring(2);
            ciOptFuKw.Label = prefix + ciOptFuKw.InsertText;
            ciOptFuKw.Detail = $"keywords '{ciOptFuKw.InsertText}'";
            ciOptFuKw.SortText = "zzz" + ciOptFu.SortText;
            modChoices.Add(ciOptFuKw);
            return modChoices;
        }

        public static List<CompletionItem> AddDeclarationChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();

            // snippet
            defaultCi.InsertText = GetDeclarationSnippet(word);
            modChoices.Add(defaultCi);
            // keyword
            var ci1 = GetCompletionItem(word);
            modChoices.Add(ci1);
            return modChoices;
        }


        public static List<CompletionItem> AddCasesChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();

            // snippet
            defaultCi.InsertText = GetCasesStatement();
            modChoices.Add(defaultCi);
            // keyword
            var ci1 = GetCompletionItem(word);
            modChoices.Add(ci1);
            return modChoices;
        }

        public static List<CompletionItem> AddForChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();

            // snippet
            var ci = GetCompletionItem(word, GetForStatement(true));
            ci.Label = $"{prefix}for .. []";
            ci.Detail = "for statement (range)";
            ci.Kind = defaultCi.Kind;
            modChoices.Add(ci);
            var ci1 = GetCompletionItem(word, GetForStatement(false));
            ci1.Label = $"{prefix}for .. list";
            ci1.Detail = "for statement (list)";
            ci1.Kind = defaultCi.Kind;
            modChoices.Add(ci1);
            // keyword
            var ci2 = GetCompletionItem(word);
            modChoices.Add(ci2);
            return modChoices;
        }


        public static List<CompletionItem> AddDefinitionChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            // class definition
            var ciClass = GetCompletionItem(word, GetDefinitionSnippet(word, "class"));
            SetIntrinsicDefinitionProperty(word, "class", ciClass);
            modChoices.Add(ciClass);
            // predicate definition
            var ciPred = GetCompletionItem(word, GetDefinitionSnippet(word, "predicate"));
            SetIntrinsicDefinitionProperty(word, "predicate", ciPred);
            modChoices.Add(ciPred);
            // functional term definition
            var ciFunc = GetCompletionItem(word, GetDefinitionSnippet(word, "function"));
            SetIntrinsicDefinitionProperty(word, "function", ciFunc);
            modChoices.Add(ciFunc);

            // keyword
            // class definition           
            var ciClassKw = GetCompletionItem(word);
            ciClassKw.SortText += "class";
            SetDefinitionLabel(word, "class", ciClassKw);
            modChoices.Add(ciClassKw);
            // predicate definition           
            var ciPredKw = GetCompletionItem(word);
            ciPredKw.SortText += "predicate";
            SetDefinitionLabel(word, "predicate", ciPredKw);
            modChoices.Add(ciPredKw);
            // functional term definition           
            var ciFuncKw = GetCompletionItem(word);
            ciFuncKw.SortText += "function";
            SetDefinitionLabel(word, "function", ciFuncKw);
            modChoices.Add(ciFuncKw);

            return modChoices;
        }
        private static void SetDefinitionLabel(string word, string subType, CompletionItem ci)
        {
            var newSubType = GetDefinitionSubtypeDependingOnLengthChoice(word, subType, out bool isShort, out string intrinsic, out string objtype);
            ci.Label += $" {newSubType}";
            ci.InsertText = $"{word} {newSubType}";
        }

        private static void SetIntrinsicDefinitionProperty(string word, string subType, CompletionItem ci)
        {

            ci.SortText += subType;
            var newSubType = GetDefinitionSubtypeDependingOnLengthChoice(word, subType, out bool isShort, out string intrinsic, out string objtype);
            ci.Label += $" {newSubType}";
            if (isShort)
            {
                ci.Detail = ci.Detail.Replace("(", $"{subType} (");
            }
            else
            {
                ci.Detail += $" {subType}";
            }
            ci.Kind = CompletionItemKind.Class;

        }

        private static string GetDefinitionSubtypeDependingOnLengthChoice(string word, string subType, out bool isShort, out string intrinsic, out string objtype)
        {
            isShort = !word.Contains("definition");
            if (isShort)
            {
                if (subType == "class")
                {
                    objtype = "obj";
                    intrinsic = "intr";
                    return "cl";
                }
                else if (subType == "predicate" || subType == "function")
                {
                    objtype = "obj";
                    intrinsic = "intr";
                    return subType.Substring(0, 4);
                }
                else
                {
                    objtype = "object";
                    intrinsic = "intrinsic";
                    return subType;
                }
            }
            else
            {
                objtype = "object";
                intrinsic = "intrinsic";
                return subType;
            }
        }

        private static string GetObjectTypeDependingOnLengthChoice(string word)
        {
            var isShort = !word.Contains("declaration");
            if (isShort)
            {
                return "obj";
            }
            else
            {
                return "object";
            }
        }

        private static void GetPropertySubtypeDependingOnLengthChoice(string word, string subType, out bool isShort, out string optStr, out string objType, out string intrisic)
        {
            isShort = !word.Contains("property");
            if (isShort)
            {

                if (subType == "class")
                {
                    objType = "obj";
                    optStr = "opt";
                    intrisic = "intr";
                }
                else if (subType == "predicate")
                {
                    objType = "pred";
                    optStr = "opt";
                    intrisic = "intr";
                }
                else if (subType == "function")
                {
                    objType = "func";
                    optStr = "opt";
                    intrisic = "intr";
                }
                else
                {
                    objType = "obj";
                    optStr = "opt";
                    intrisic = "intr";
                }
            }
            else
            {
                if (subType == "class")
                {
                    objType = "object";
                    optStr = "optional";
                    intrisic = "intrinsic";
                }
                else if (subType == "predicate")
                {
                    objType = subType;
                    optStr = "optional";
                    intrisic = "intrinsic";
                }
                else if (subType == "function")
                {
                    objType = subType;
                    optStr = "optional";
                    intrisic = "intrinsic";
                }
                else
                {
                    objType = "object";
                    optStr = "optional";
                    intrisic = "intrinsic";
                }
            }
        }

        private static string GetDefinitionSnippet(string word, string subType)
        {
            var leftBrace = "{";
            var rightBrace = "}";

            var newSubType = GetDefinitionSubtypeDependingOnLengthChoice(word, subType, out bool isShort, out string intrinsic, out string objtype);
            switch (subType)
            {
                case "class":
                    return
                        $"{Environment.NewLine}{word} {newSubType} SomeFplClass: {objtype}" +
                        $"{Environment.NewLine}{leftBrace}" +
                        $"{Environment.NewLine}\t{intrinsic}" +
                        $"{Environment.NewLine}{rightBrace}" +
                        $"{Environment.NewLine}";
                case "function":
                    return
                        $"{Environment.NewLine}{word} {newSubType} SomeFplFunction() -> {objtype}" +
                        $"{Environment.NewLine}{leftBrace}" +
                        $"{Environment.NewLine}\t{intrinsic}" +
                        $"{Environment.NewLine}{rightBrace}" +
                        $"{Environment.NewLine}";
                case "predicate":
                    return
                        $"{Environment.NewLine}{word} {newSubType} SomeFplPredicate()" +
                        $"{Environment.NewLine}{leftBrace}" +
                        $"{Environment.NewLine}\t{intrinsic}" +
                        $"{Environment.NewLine}{rightBrace}" +
                        $"{Environment.NewLine}";
            }
            return "";
        }

        private static string GetConstructorSnippet(string word)
        {
            var leftBrace = "{";
            var rightBrace = "}";

            var isShort = (word == "ctor");
            if (isShort)
            {
                return
                    $"{Environment.NewLine}ctor SomeFplClass()" +
                    $"{Environment.NewLine}{leftBrace}" +
                    $"{Environment.NewLine}\tdec" +
                    $"{Environment.NewLine}\t\tself!obj()" +
                    $"{Environment.NewLine}\t;" +
                    $"{Environment.NewLine}\tself" +
                    $"{Environment.NewLine}{rightBrace}" +
                    $"{Environment.NewLine}";
            }
            else
            {
                return
                    $"{Environment.NewLine}constructor SomeFplClass()" +
                    $"{Environment.NewLine}{leftBrace}" +
                    $"{Environment.NewLine}\tdeclaration" +
                    $"{Environment.NewLine}\t\tself!object()" +
                    $"{Environment.NewLine}\t;" +
                    $"{Environment.NewLine}\tself" +
                    $"{Environment.NewLine}{rightBrace}" +
                    $"{Environment.NewLine}";
            }
        }

        private static string GetCasesStatement()
        {
            return
                $"{Environment.NewLine}cases" +
                $"{Environment.NewLine}(" +
                $"{Environment.NewLine}\t| p(x) : y := a" +
                $"{Environment.NewLine}\t| q(x) : y := b" +
                $"{Environment.NewLine}\t? y := c" +
                $"{Environment.NewLine})" +
                $"{Environment.NewLine}";
        }

        private static string GetForStatement(bool withRange)
        {
            if (withRange)
            {
                return
                    $"{Environment.NewLine}for i in [a,b]" +
                    $"{Environment.NewLine}(" +
                    $"{Environment.NewLine}\tx<i> := 1" +
                    $"{Environment.NewLine}\ty<i> := 0" +
                    $"{Environment.NewLine})" +
                    $"{Environment.NewLine}";
            }
            else
            {
                return
                    $"{Environment.NewLine}for i in someList" +
                    $"{Environment.NewLine}(" +
                    $"{Environment.NewLine}\tx<i> := 1" +
                    $"{Environment.NewLine}\ty<i> := 0" +
                    $"{Environment.NewLine})" +
                    $"{Environment.NewLine}";
            }
        }

        private static string GetDeclarationSnippet(string word)
        {

            var objStr = GetObjectTypeDependingOnLengthChoice(word);

            return
                $"{Environment.NewLine}{word}" +
                $"{Environment.NewLine}\t~x: {objStr}" +
                $"{Environment.NewLine}\t~y: {objStr}" +
                $"{Environment.NewLine}\tx := 0" +
                $"{Environment.NewLine}\ty := 1" +
                $"{Environment.NewLine};" +
                $"{Environment.NewLine}";
        }

        private static string GetClassInstanceSnippet(string word, bool optional, out string optionalStr, out string objTypeStr)
        {
            var leftBrace = "{";
            var rightBrace = "}";

            GetPropertySubtypeDependingOnLengthChoice(word, "class", out bool isShort, out string optStr, out string objType, out string intrinsic);

            optionalStr = optStr;
            objTypeStr = objType;

            string firstLine;
            if (optional)
            {
                firstLine = $"{word} {optStr} {objType} SomeObjectProperty()";
            }
            else
            {
                firstLine = $"{word} {objType} SomeObjectProperty()";
            }

            return
                firstLine +
                $"{Environment.NewLine}{leftBrace}" +
                $"{Environment.NewLine}\t{intrinsic}" +
                $"{Environment.NewLine}{rightBrace}" +
                $"{Environment.NewLine}";
        }

        private static string GetFunctionalTermInstanceSnippet(string word, bool optional)
        {
            var leftBrace = "{";
            var rightBrace = "}";
            GetPropertySubtypeDependingOnLengthChoice(word, "function", out bool isShort, out string optStr, out string objType, out string intrinsic);

            string objStr;
            if (isShort)
            {
                objStr = "obj";
            }
            else
            {
                objStr = "object";
            }
            string firstLine;
            if (optional)
            {
                firstLine = $"{word} {optStr} {objType} SomeFunctionProperty() -> {objStr}";
            }
            else
            {
                firstLine = $"{word} {objType} SomeFunctionProperty() -> {objStr}";
            }

            return
                firstLine +
                $"{Environment.NewLine}{leftBrace}" +
                $"{Environment.NewLine}\t{intrinsic}" +
                $"{Environment.NewLine}{rightBrace}" +
                $"{Environment.NewLine}";
        }

        private static string GetPredicateInstanceSnippet(string word, bool optional)
        {
            var leftBrace = "{";
            var rightBrace = "}";
            GetPropertySubtypeDependingOnLengthChoice(word, "predicate", out bool isShort, out string optStr, out string objType, out string intrinsic);

            string firstLine;
            if (optional)
            {
                firstLine = $"{word} {optStr} {objType} SomePredicateProperty()";
            }
            else
            {
                firstLine = $"{word} {objType} SomePredicateProperty()";
            }

            return
                firstLine +
                $"{Environment.NewLine}{leftBrace}" +
                $"{Environment.NewLine}\t{intrinsic}" +
                $"{Environment.NewLine}{rightBrace}" +
                $"{Environment.NewLine}";
        }

        public static List<CompletionItem> AddTheoremLikeStatementChoices(string word, string example, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            defaultCi.InsertText = $"{word} SomeFpl{example} (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tpre: true{Environment.NewLine}\tcon: true{Environment.NewLine}" + "}" + Environment.NewLine;
            modChoices.Add(defaultCi);
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }


        private List<CompletionItem> AddCorollaryChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            defaultCi.InsertText = $"<replace> SomeFplTheorem!1 (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tpre: true{Environment.NewLine}\tcon: true{Environment.NewLine}" + "}" + Environment.NewLine;
            modChoices.Add(defaultCi);
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        private List<CompletionItem> AddProofChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            defaultCi.InsertText = $"<replace> SomeFplTheorem!1{Environment.NewLine}" + "{" + $"{Environment.NewLine}\t1. |- qed{Environment.NewLine}" + "}" + Environment.NewLine;
            modChoices.Add(defaultCi);
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        private List<CompletionItem> AddLocalizationChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            defaultCi.InsertText = $"<replace> iif(x,y) :={Environment.NewLine}!tex: x \"\\Leftrightarrow\" y{Environment.NewLine}!eng: x \" if and only if \" y{Environment.NewLine}!eng: x \" dann und nur dann \" y{Environment.NewLine}" + ";" + Environment.NewLine;
            modChoices.Add(defaultCi);
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        private List<CompletionItem> AddUsesChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            var ci = GetCompletionItem(word, $"<replace> SomeFplNamespace{Environment.NewLine}");
            ci.Detail = "uses namespace";
            ci.SortText = defaultCi.SortText;
            ci.Label = defaultCi.Label;
            ci.Kind = defaultCi.Kind;
            var ci1 = GetCompletionItem(word, $"<replace> SomeFplNamespace alias Sfn{Environment.NewLine}");
            ci1.Detail = "uses namespace with alias";
            ci1.SortText = defaultCi.SortText;
            ci1.Label = defaultCi.Label + " .. alias";
            ci1.Kind = defaultCi.Kind;
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        private List<CompletionItem> AddDefaultChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.InsertText = word;
            ci.Label = prefix + ci.InsertText;
            switch (word)
            {
                case "{":
                    ci.Detail = "opening '{'";
                    break;
                case "}":
                    ci.Detail = "closing '{'";
                    break;
                case "(":
                    ci.Detail = "opening '('";
                    break;
                case ")":
                    ci.Detail = "closing '('";
                    break;
                case "<":
                    ci.Detail = "coordinates";
                    ci.InsertText = "<1,2>";
                    ci.Label += "1,2>";
                    break;
                case ":ext":
                    ci.Detail = "extension header";
                    break;
                case ":end":
                    ci.Detail = "extension tail";
                    break;
                default:
                    ci.Detail = "unknown";
                    break;
            }

            ci.Kind = CompletionItemKind.Text;
            modChoices.Add(ci);
            return modChoices;
        }


        private List<CompletionItem> AddBoundChoices(string word)
        {
            var isLeftBound = word.Contains("left bound");
            var isClosed = word.Contains("closed");

            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = word;
            if (isClosed && isLeftBound)
            {
                ci.InsertText = "[";
            }
            else if (!isClosed && isLeftBound)
            {
                ci.InsertText = "[(";
            }
            else if (isClosed && !isLeftBound)
            {
                ci.InsertText = "]";
            }
            else
            {
                ci.InsertText = ")]";
            }
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Text;
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddDigitsChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = word;
            ci.InsertText = "123";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Text;
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddArgumentIdentifierChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = word;
            ci.InsertText = "10.";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Unit;
            modChoices.Add(ci);
            return modChoices;
        }
        private List<CompletionItem> AddLanguageSpecificStringChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = word;
            ci.InsertText = "\"...\"";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Value;
            modChoices.Add(ci);
            return modChoices;
        }
        private List<CompletionItem> AddExtensionRegexChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = word;
            ci.InsertText = "/+\\d/ ";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Text;
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddWordChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = "word pattern [a-z0-9_]+";
            ci.InsertText = "someIdentifier";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Value;
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddVariableChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = word;
            ci.InsertText = "someVar";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Variable;
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddPascalCaseIdChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = "user-defined identifier";
            ci.InsertText = "SomeFplIdentifier";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Reference;
            modChoices.Add(ci);
            return modChoices;
        }


        private List<CompletionItem> AddKeywordChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            defaultCi.InsertText = word;
            defaultCi.Label = prefix + defaultCi.InsertText;
            defaultCi.Kind = CompletionItemKind.Keyword;
            modChoices.Add(defaultCi);
            return modChoices;
        }

        private List<CompletionItem> AddSelfChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            if (word == "self")
            {
                var ci = new CompletionItem();
                ci.Detail = "self reference";
                ci.InsertText = "self";
                ci.Label = prefix + ci.InsertText;
                ci.Kind = CompletionItemKind.Reference;
                ci.SortText = "self01";
                modChoices.Add(ci);
            }
            if (word == "@")
            {
                var ci = new CompletionItem();
                ci.Detail = "parent self reference";
                ci.InsertText = "@self";
                ci.Label = prefix + ci.InsertText;
                ci.Kind = CompletionItemKind.Reference;
                ci.SortText = "self02";
                modChoices.Add(ci);
            }
            return modChoices;
        }

        private List<CompletionItem> AddWhitespaceChoices()
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.InsertText = " ";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Text;
            ci.Detail = "(whitespace)";
            ci.SortText = "zzzz"; // make sure whitespaces appear at the end of any list.
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddDelegateChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            defaultCi.InsertText = word + ".SomeExternalMethod(x,1)";
            defaultCi.Label = prefix + defaultCi.InsertText;
            defaultCi.Kind = CompletionItemKind.Reference;
            modChoices.Add(defaultCi);
            return modChoices;
        }

        private List<CompletionItem> AddIsOperatorChoices(string word, CompletionItem defaultCi)
        {
            var modChoices = new List<CompletionItem>();
            defaultCi.Detail = "is operator";
            defaultCi.InsertText = word + "(x, SomeFplType)";
            defaultCi.Label = prefix + defaultCi.InsertText;
            defaultCi.Kind = CompletionItemKind.Interface;
            modChoices.Add(defaultCi);

            var ci1 = new CompletionItem();
            ci1.Detail = "keyword 'is'";
            ci1.InsertText = word;
            ci1.Label = prefix + ci1.InsertText;
            ci1.Kind = CompletionItemKind.Keyword;
            ci1.SortText = "zzz" + defaultCi.SortText;
            modChoices.Add(ci1);
            return modChoices;
        }


        private List<CompletionItem> AddIso639Choices()
        {
            var modChoices = new List<CompletionItem>();
            var iso632_2 = new Dictionary<string, string>()
            {
                { "aar","Afar" }
                ,{ "abk","Abkhazian" }
                ,{ "ace","Achinese" }
                ,{ "ach","Acoli" }
                ,{ "ada","Adangme" }
                ,{ "ady","Adyghe; Adygei" }
                ,{ "afa","Afro-Asiatic languages" }
                ,{ "afh","Afrihili" }
                ,{ "afr","Afrikaans" }
                ,{ "ain","Ainu" }
                ,{ "aka","Akan" }
                ,{ "akk","Akkadian" }
                ,{ "alb","Albanian" }
                ,{ "ale","Aleut" }
                ,{ "alg","Algonquian languages" }
                ,{ "alt","Southern Altai" }
                ,{ "amh","Amharic" }
                ,{ "anp","Angika" }
                ,{ "apa","Apache languages" }
                ,{ "ara","Arabic" }
                ,{ "arg","Aragonese" }
                ,{ "arm","Armenian" }
                ,{ "arn","Mapudungun; Mapuche" }
                ,{ "arp","Arapaho" }
                ,{ "art","Artificial languages" }
                ,{ "arw","Arawak" }
                ,{ "asm","Assamese" }
                ,{ "ast","Asturian; Bable; Leonese; Asturleonese" }
                ,{ "ath","Athapascan languages" }
                ,{ "aus","Australian languages" }
                ,{ "ava","Avaric" }
                ,{ "ave","Avestan" }
                ,{ "awa","Awadhi" }
                ,{ "aym","Aymara" }
                ,{ "aze","Azerbaijani" }
                ,{ "bad","Banda languages" }
                ,{ "bai","Bamileke languages" }
                ,{ "bak","Bashkir" }
                ,{ "bal","Baluchi" }
                ,{ "bam","Bambara" }
                ,{ "ban","Balinese" }
                ,{ "baq","Basque" }
                ,{ "bas","Basa" }
                ,{ "bat","Baltic languages" }
                ,{ "bej","Beja; Bedawiyet" }
                ,{ "bel","Belarusian" }
                ,{ "bem","Bemba" }
                ,{ "ben","Bengali" }
                ,{ "ber","Berber languages" }
                ,{ "bho","Bhojpuri" }
                ,{ "bih","Bihari languages" }
                ,{ "bik","Bikol" }
                ,{ "bin","Bini; Edo" }
                ,{ "bis","Bislama" }
                ,{ "bla","Siksika" }
                ,{ "bnt","Bantu languages" }
                ,{ "bod","Tibetan" }
                ,{ "bos","Bosnian" }
                ,{ "bra","Braj" }
                ,{ "bre","Breton" }
                ,{ "btk","Batak languages" }
                ,{ "bua","Buriat" }
                ,{ "bug","Buginese" }
                ,{ "bul","Bulgarian" }
                ,{ "bur","Burmese" }
                ,{ "byn","Blin; Bilin" }
                ,{ "cad","Caddo" }
                ,{ "cai","Central American Indian languages" }
                ,{ "car","Galibi Carib" }
                ,{ "cat","Catalan; Valencian" }
                ,{ "cau","Caucasian languages" }
                ,{ "ceb","Cebuano" }
                ,{ "cel","Celtic languages" }
                ,{ "ces","Czech" }
                ,{ "cha","Chamorro" }
                ,{ "chb","Chibcha" }
                ,{ "che","Chechen" }
                ,{ "chg","Chagatai" }
                ,{ "chi ","Chinese" }
                ,{ "chk","Chuukese" }
                ,{ "chm","Mari" }
                ,{ "chn","Chinook jargon" }
                ,{ "cho","Choctaw" }
                ,{ "chp","Chipewyan; Dene Suline" }
                ,{ "chr","Cherokee" }
                ,{ "chv","Chuvash" }
                ,{ "chy","Cheyenne" }
                ,{ "cmc","Chamic languages" }
                ,{ "cnr","Montenegrin" }
                ,{ "cop","Coptic" }
                ,{ "cor","Cornish" }
                ,{ "cos","Corsican" }
                ,{ "cpe","Creoles and pidgins, English based" }
                ,{ "cpf","Creoles and pidgins, French-based" }
                ,{ "cpp","Creoles and pidgins, Portuguese-based" }
                ,{ "cre","Cree" }
                ,{ "crh","Crimean Tatar; Crimean Turkish" }
                ,{ "crp","Creoles and pidgins" }
                ,{ "csb","Kashubian" }
                ,{ "cus","Cushitic languages" }
                ,{ "cym","Welsh" }
                ,{ "cze","Czech" }
                ,{ "dak","Dakota" }
                ,{ "dan","Danish" }
                ,{ "dar","Dargwa" }
                ,{ "day","Land Dayak languages" }
                ,{ "del","Delaware" }
                ,{ "den","Slave (Athapascan)" }
                ,{ "deu","German" }
                ,{ "dgr","Dogrib" }
                ,{ "din","Dinka" }
                ,{ "div","Divehi; Dhivehi; Maldivian" }
                ,{ "doi","Dogri" }
                ,{ "dra","Dravidian languages" }
                ,{ "dsb","Lower Sorbian" }
                ,{ "dua","Duala" }
                ,{ "dut","Dutch; Flemish" }
                ,{ "dyu","Dyula" }
                ,{ "dzo","Dzongkha" }
                ,{ "efi","Efik" }
                ,{ "eka","Ekajuk" }
                ,{ "ell","Greek" }
                ,{ "elx","Elamite" }
                ,{ "eng","English" }
                ,{ "epo","Esperanto" }
                ,{ "est","Estonian" }
                ,{ "eus","Basque" }
                ,{ "ewe","Ewe" }
                ,{ "ewo","Ewondo" }
                ,{ "fan","Fang" }
                ,{ "fao","Faroese" }
                ,{ "fas","Persian" }
                ,{ "fat","Fanti" }
                ,{ "fij","Fijian" }
                ,{ "fil","Filipino; Pilipino" }
                ,{ "fin","Finnish" }
                ,{ "fiu","Finno-Ugrian languages" }
                ,{ "fon","Fon" }
                ,{ "fra","French" }
                ,{ "fre","French" }
                ,{ "frr","Northern Frisian" }
                ,{ "frs","Eastern Frisian" }
                ,{ "fry","Western Frisian" }
                ,{ "ful","Fulah" }
                ,{ "fur","Friulian" }
                ,{ "gaa","Ga" }
                ,{ "gay","Gayo" }
                ,{ "gba","Gbaya" }
                ,{ "gem","Germanic languages" }
                ,{ "geo","Georgian" }
                ,{ "ger","German" }
                ,{ "gez","Geez" }
                ,{ "gil","Gilbertese" }
                ,{ "gla","Gaelic; Scottish Gaelic" }
                ,{ "gle","Irish" }
                ,{ "glg","Galician" }
                ,{ "glv","Manx" }
                ,{ "gon","Gondi" }
                ,{ "gor","Gorontalo" }
                ,{ "got","Gothic" }
                ,{ "grb","Grebo" }
                ,{ "grn","Guarani" }
                ,{ "gsw","Swiss German; Alemannic; Alsatian" }
                ,{ "guj","Gujarati" }
                ,{ "gwi","Gwich'in" }
                ,{ "hai","Haida" }
                ,{ "hat","Haitian; Haitian Creole" }
                ,{ "hau","Hausa" }
                ,{ "haw","Hawaiian" }
                ,{ "heb","Hebrew" }
                ,{ "her","Herero" }
                ,{ "hil","Hiligaynon" }
                ,{ "him","Himachali languages; Western Pahari languages" }
                ,{ "hin","Hindi" }
                ,{ "hit","Hittite" }
                ,{ "hmn","Hmong; Mong" }
                ,{ "hmo","Hiri Motu" }
                ,{ "hrv","Croatian" }
                ,{ "hsb","Upper Sorbian" }
                ,{ "hun","Hungarian" }
                ,{ "hup","Hupa" }
                ,{ "hye","Armenian" }
                ,{ "iba","Iban" }
                ,{ "ibo","Igbo" }
                ,{ "ice","Icelandic" }
                ,{ "ido","Ido" }
                ,{ "iii","Sichuan Yi; Nuosu" }
                ,{ "ijo","Ijo languages" }
                ,{ "iku","Inuktitut" }
                ,{ "ile","Interlingue; Occidental" }
                ,{ "ilo","Iloko" }
                ,{ "ina","Interlingua (International Auxiliary Language Association)" }
                ,{ "inc","Indic languages" }
                ,{ "ind","Indonesian" }
                ,{ "ine","Indo-European languages" }
                ,{ "inh","Ingush" }
                ,{ "ipk","Inupiaq" }
                ,{ "ira","Iranian languages" }
                ,{ "iro","Iroquoian languages" }
                ,{ "isl","Icelandic" }
                ,{ "ita","Italian" }
                ,{ "jav","Javanese" }
                ,{ "jbo","Lojban" }
                ,{ "jpn","Japanese" }
                ,{ "jpr","Judeo-Persian" }
                ,{ "jrb","Judeo-Arabic" }
                ,{ "kaa","Kara-Kalpak" }
                ,{ "kab","Kabyle" }
                ,{ "kac","Kachin; Jingpho" }
                ,{ "kal","Kalaallisut; Greenlandic" }
                ,{ "kam","Kamba" }
                ,{ "kan","Kannada" }
                ,{ "kar","Karen languages" }
                ,{ "kas","Kashmiri" }
                ,{ "kat","Georgian" }
                ,{ "kau","Kanuri" }
                ,{ "kaw","Kawi" }
                ,{ "kaz","Kazakh" }
                ,{ "kbd","Kabardian" }
                ,{ "kha","Khasi" }
                ,{ "khi","Khoisan languages" }
                ,{ "khm","Central Khmer" }
                ,{ "kho","Khotanese; Sakan" }
                ,{ "kik","Kikuyu; Gikuyu" }
                ,{ "kin","Kinyarwanda" }
                ,{ "kir","Kirghiz; Kyrgyz" }
                ,{ "kmb","Kimbundu" }
                ,{ "kok","Konkani" }
                ,{ "kom","Komi" }
                ,{ "kon","Kongo" }
                ,{ "kor","Korean" }
                ,{ "kos","Kosraean" }
                ,{ "kpe","Kpelle" }
                ,{ "krc","Karachay-Balkar" }
                ,{ "krl","Karelian" }
                ,{ "kro","Kru languages" }
                ,{ "kru","Kurukh" }
                ,{ "kua","Kuanyama; Kwanyama" }
                ,{ "kum","Kumyk" }
                ,{ "kur","Kurdish" }
                ,{ "kut","Kutenai" }
                ,{ "lad","Ladino" }
                ,{ "lah","Lahnda" }
                ,{ "lam","Lamba" }
                ,{ "lao","Lao" }
                ,{ "lat","Latin" }
                ,{ "lav","Latvian" }
                ,{ "lez","Lezghian" }
                ,{ "lim","Limburgan; Limburger; Limburgish" }
                ,{ "lin","Lingala" }
                ,{ "lit","Lithuanian" }
                ,{ "lol","Mongo" }
                ,{ "loz","Lozi" }
                ,{ "ltz","Luxembourgish; Letzeburgesch" }
                ,{ "lua","Luba-Lulua" }
                ,{ "lub","Luba-Katanga" }
                ,{ "lug","Ganda" }
                ,{ "lui","Luiseno" }
                ,{ "lun","Lunda" }
                ,{ "luo","Luo (Kenya and Tanzania)" }
                ,{ "lus","Lushai" }
                ,{ "mac","Macedonian" }
                ,{ "mad","Madurese" }
                ,{ "mag","Magahi" }
                ,{ "mah","Marshallese" }
                ,{ "mai","Maithili" }
                ,{ "mak","Makasar" }
                ,{ "mal","Malayalam" }
                ,{ "man","Mandingo" }
                ,{ "mao","Maori" }
                ,{ "map","Austronesian languages" }
                ,{ "mar","Marathi" }
                ,{ "mas","Masai" }
                ,{ "may","Malay" }
                ,{ "mdf","Moksha" }
                ,{ "mdr","Mandar" }
                ,{ "men","Mende" }
                ,{ "mic","Mi'kmaq; Micmac" }
                ,{ "min","Minangkabau" }
                ,{ "mis","Uncoded languages" }
                ,{ "mkd","Macedonian" }
                ,{ "mkh","Mon-Khmer languages" }
                ,{ "mlg","Malagasy" }
                ,{ "mlt","Maltese" }
                ,{ "mnc","Manchu" }
                ,{ "mni","Manipuri" }
                ,{ "mno","Manobo languages" }
                ,{ "moh","Mohawk" }
                ,{ "mon","Mongolian" }
                ,{ "mos","Mossi" }
                ,{ "mri","Maori" }
                ,{ "msa","Malay" }
                ,{ "mul","Multiple languages" }
                ,{ "mun","Munda languages" }
                ,{ "mus","Creek" }
                ,{ "mwl","Mirandese" }
                ,{ "mwr","Marwari" }
                ,{ "mya","Burmese" }
                ,{ "myn","Mayan languages" }
                ,{ "myv","Erzya" }
                ,{ "nah","Nahuatl languages" }
                ,{ "nai","North American Indian languages" }
                ,{ "nap","Neapolitan" }
                ,{ "nau","Nauru" }
                ,{ "nav","Navajo; Navaho" }
                ,{ "nbl","Ndebele, South; South Ndebele" }
                ,{ "nde","Ndebele, North; North Ndebele" }
                ,{ "ndo","Ndonga" }
                ,{ "nds","Low German; Low Saxon; German, Low; Saxon, Low" }
                ,{ "nep","Nepali" }
                ,{ "new","Nepal Bhasa; Newari" }
                ,{ "nia","Nias" }
                ,{ "nic","Niger-Kordofanian languages" }
                ,{ "niu","Niuean" }
                ,{ "nld","Dutch; Flemish" }
                ,{ "nno","Norwegian Nynorsk; Nynorsk, Norwegian" }
                ,{ "nob","Bokmål, Norwegian; Norwegian Bokmål" }
                ,{ "nog","Nogai" }
                ,{ "non","Norse, Old" }
                ,{ "nor","Norwegian" }
                ,{ "nqo","N'Ko" }
                ,{ "nso","Pedi; Sepedi; Northern Sotho" }
                ,{ "nub","Nubian languages" }
                ,{ "nwc","Classical Newari; Old Newari; Classical Nepal Bhasa" }
                ,{ "nya","Chichewa; Chewa; Nyanja" }
                ,{ "nym","Nyamwezi" }
                ,{ "nyn","Nyankole" }
                ,{ "nyo","Nyoro" }
                ,{ "nzi","Nzima" }
                ,{ "oji","Ojibwa" }
                ,{ "ori","Oriya" }
                ,{ "orm","Oromo" }
                ,{ "osa","Osage" }
                ,{ "oss","Ossetian; Ossetic" }
                ,{ "oto","Otomian languages" }
                ,{ "paa","Papuan languages" }
                ,{ "pag","Pangasinan" }
                ,{ "pal","Pahlavi" }
                ,{ "pam","Pampanga; Kapampangan" }
                ,{ "pan","Panjabi; Punjabi" }
                ,{ "pap","Papiamento" }
                ,{ "pau","Palauan" }
                ,{ "per","Persian" }
                ,{ "phi","Philippine languages" }
                ,{ "phn","Phoenician" }
                ,{ "pli","Pali" }
                ,{ "pol","Polish" }
                ,{ "pon","Pohnpeian" }
                ,{ "por","Portuguese" }
                ,{ "pra","Prakrit languages" }
                ,{ "pus","Pushto; Pashto" }
                ,{ "qaa-qtz","Reserved for local use" }
                ,{ "que","Quechua" }
                ,{ "raj","Rajasthani" }
                ,{ "rap","Rapanui" }
                ,{ "rar","Rarotongan; Cook Islands Maori" }
                ,{ "roa","Romance languages" }
                ,{ "roh","Romansh" }
                ,{ "rom","Romany" }
                ,{ "ron","Romanian; Moldavian; Moldovan" }
                ,{ "rum","Romanian; Moldavian; Moldovan" }
                ,{ "run","Rundi" }
                ,{ "rup","Aromanian; Arumanian; Macedo-Romanian" }
                ,{ "rus","Russian" }
                ,{ "sad","Sandawe" }
                ,{ "sag","Sango" }
                ,{ "sah","Yakut" }
                ,{ "sai","South American Indian languages" }
                ,{ "sal","Salishan languages" }
                ,{ "sam","Samaritan Aramaic" }
                ,{ "san","Sanskrit" }
                ,{ "sas","Sasak" }
                ,{ "sat","Santali" }
                ,{ "scn","Sicilian" }
                ,{ "sco","Scots" }
                ,{ "sel","Selkup" }
                ,{ "sem","Semitic languages" }
                ,{ "sga","Irish, Old (to 900)" }
                ,{ "sgn","Sign Languages" }
                ,{ "shn","Shan" }
                ,{ "sid","Sidamo" }
                ,{ "sin","Sinhala; Sinhalese" }
                ,{ "sio","Siouan languages" }
                ,{ "sit","Sino-Tibetan languages" }
                ,{ "sla","Slavic languages" }
                ,{ "slk","Slovak" }
                ,{ "slo","Slovak" }
                ,{ "slv","Slovenian" }
                ,{ "sma","Southern Sami" }
                ,{ "sme","Northern Sami" }
                ,{ "smi","Sami languages" }
                ,{ "smj","Lule Sami" }
                ,{ "smn","Inari Sami" }
                ,{ "smo","Samoan" }
                ,{ "sms","Skolt Sami" }
                ,{ "sna","Shona" }
                ,{ "snd","Sindhi" }
                ,{ "snk","Soninke" }
                ,{ "sog","Sogdian" }
                ,{ "som","Somali" }
                ,{ "son","Songhai languages" }
                ,{ "sot","Sotho, Southern" }
                ,{ "spa","Spanish; Castilian" }
                ,{ "sqi","Albanian" }
                ,{ "srd","Sardinian" }
                ,{ "srn","Sranan Tongo" }
                ,{ "srp","Serbian" }
                ,{ "srr","Serer" }
                ,{ "ssa","Nilo-Saharan languages" }
                ,{ "ssw","Swati" }
                ,{ "suk","Sukuma" }
                ,{ "sun","Sundanese" }
                ,{ "sus","Susu" }
                ,{ "sux","Sumerian" }
                ,{ "swa","Swahili" }
                ,{ "swe","Swedish" }
                ,{ "syc","Classical Syriac" }
                ,{ "syr","Syriac" }
                ,{ "tah","Tahitian" }
                ,{ "tai","Tai languages" }
                ,{ "tam","Tamil" }
                ,{ "tat","Tatar" }
                ,{ "tel","Telugu" }
                ,{ "tem","Timne" }
                ,{ "ter","Tereno" }
                ,{ "tet","Tetum" }
                ,{ "tex","LaTeX" }
                ,{ "tgk","Tajik" }
                ,{ "tgl","Tagalog" }
                ,{ "tha","Thai" }
                ,{ "tib","Tibetan" }
                ,{ "tig","Tigre" }
                ,{ "tir","Tigrinya" }
                ,{ "tiv","Tiv" }
                ,{ "tkl","Tokelau" }
                ,{ "tlh","Klingon; tlhIngan-Hol" }
                ,{ "tli","Tlingit" }
                ,{ "tmh","Tamashek" }
                ,{ "tog","Tonga (Nyasa)" }
                ,{ "ton","Tonga (Tonga Islands)" }
                ,{ "tpi","Tok Pisin" }
                ,{ "tsi","Tsimshian" }
                ,{ "tsn","Tswana" }
                ,{ "tso","Tsonga" }
                ,{ "tuk","Turkmen" }
                ,{ "tum","Tumbuka" }
                ,{ "tup","Tupi languages" }
                ,{ "tur","Turkish" }
                ,{ "tut","Altaic languages" }
                ,{ "tvl","Tuvalu" }
                ,{ "twi","Twi" }
                ,{ "tyv","Tuvinian" }
                ,{ "udm","Udmurt" }
                ,{ "uga","Ugaritic" }
                ,{ "uig","Uighur; Uyghur" }
                ,{ "ukr","Ukrainian" }
                ,{ "umb","Umbundu" }
                ,{ "und","Undetermined" }
                ,{ "urd","Urdu" }
                ,{ "uzb","Uzbek" }
                ,{ "vai","Vai" }
                ,{ "ven","Venda" }
                ,{ "vie","Vietnamese" }
                ,{ "vol","Volapük" }
                ,{ "vot","Votic" }
                ,{ "wak","Wakashan languages" }
                ,{ "wal","Wolaitta; Wolaytta" }
                ,{ "war","Waray" }
                ,{ "was","Washo" }
                ,{ "wel","Welsh" }
                ,{ "wen","Sorbian languages" }
                ,{ "wln","Walloon" }
                ,{ "wol","Wolof" }
                ,{ "xal","Kalmyk; Oirat" }
                ,{ "xho","Xhosa" }
                ,{ "yao","Yao" }
                ,{ "yap","Yapese" }
                ,{ "yid","Yiddish" }
                ,{ "yor","Yoruba" }
                ,{ "ypk","Yupik languages" }
                ,{ "zap","Zapotec" }
                ,{ "zbl","Blissymbols; Blissymbolics; Bliss" }
                ,{ "zen","Zenaga" }
                ,{ "zgh","Standard Moroccan Tamazight" }
                ,{ "zha","Zhuang; Chuang" }
                ,{ "zho","Chinese" }
                ,{ "znd","Zande languages" }
                ,{ "zul","Zulu" }
                ,{ "zun","Zuni" }
                ,{ "zza","Zaza; Dimili; Dimli; Kirdki; Kirmanjki; Zazaki" }
            };
            foreach (var kvp in iso632_2)

            {
                var ci = new CompletionItem();
                ci.InsertText = kvp.Key + ":";
                ci.Label = prefix + ci.InsertText;
                ci.Detail = kvp.Value;
                ci.Kind = CompletionItemKind.Value;
                modChoices.Add(ci);
            }
            return modChoices;
        }

        public static CompletionItem GetDetail(string word)
        {
            CompletionItem ret = new CompletionItem();
            ret.Label = prefix + word;
            ret.InsertText = word;
            switch (word)
            {
                case "alias":
                    ret.Detail = "alias";
                    ret.SortText = "alias";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "all":
                    ret.Detail = "predicate (all quantor)";
                    ret.SortText = "all";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "and":
                    ret.Detail = "predicate (conjunction)";
                    ret.SortText = "and";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "ass":
                    ret.Detail = "argument (assume, short form)";
                    ret.SortText = "assume02";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "assert":
                    ret.Detail = "statement (assert)";
                    ret.SortText = "assert";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "assume":
                    ret.Detail = "argument (assume)";
                    ret.SortText = "assume01";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "ax":
                    ret.Detail = "axiom (short form)";
                    ret.SortText = "axiom02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "axiom":
                    ret.Detail = "axiom";
                    ret.SortText = "axiom01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "cases":
                    ret.Detail = "statement (cases)";
                    ret.SortText = "cases";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "cl":
                    ret.Detail = "class (short form)";
                    ret.SortText = "class02";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "class":
                    ret.Detail = "class";
                    ret.SortText = "class01";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "con":
                    ret.Detail = "conclusion (short form)";
                    ret.SortText = "conclusion02";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "conclusion":
                    ret.Detail = "conclusion";
                    ret.SortText = "conclusion01";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "cor":
                    ret.Detail = "corollary (short form)";
                    ret.SortText = "corollary02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "corollary":
                    ret.Detail = "corollary";
                    ret.SortText = "corollary01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "conj":
                    ret.Detail = "conjecture (short form)";
                    ret.SortText = "conjecture02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "conjecture":
                    ret.Detail = "conjecture";
                    ret.SortText = "conjecture01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "ctor":
                    ret.Detail = "constructor (short form)";
                    ret.SortText = "constructor02";
                    ret.Kind = CompletionItemKind.Constructor;
                    break;
                case "constructor":
                    ret.Detail = "constructor";
                    ret.SortText = "constructor01";
                    ret.Kind = CompletionItemKind.Constructor;
                    break;
                case "dec":
                    ret.Detail = "declaration (short form)";
                    ret.SortText = "declaration02";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "declaration":
                    ret.Detail = "declaration";
                    ret.SortText = "declaration01";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "del":
                    ret.Detail = "delegate (short form)";
                    ret.SortText = "delegate02";
                    ret.Kind = CompletionItemKind.Event;
                    break;
                case "delegate":
                    ret.Detail = "delegate";
                    ret.SortText = "delegate01";
                    ret.Kind = CompletionItemKind.Event;
                    break;
                case "def":
                    ret.Detail = "definition (short form)";
                    ret.SortText = "definition02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "definition":
                    ret.Detail = "definition";
                    ret.SortText = "definition01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "end":
                    ret.Detail = "extension (end of)";
                    ret.SortText = "end";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "ex":
                    ret.Detail = "predicate (exists quantor)";
                    ret.SortText = "ex";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "exn":
                    ret.Detail = "predicate (exists n-times quantor)";
                    ret.SortText = "exn";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "exn!":
                    ret.Detail = "predicate (exists n-times quantor)";
                    ret.SortText = "exn!";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "ext":
                    ret.Detail = "extension (beginning of)";
                    ret.SortText = "ext";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "false":
                    ret.Detail = "predicate (false)";
                    ret.SortText = "false";
                    ret.Kind = CompletionItemKind.Constant;
                    break;
                case "for":
                    ret.Detail = "statement (for loop)";
                    ret.SortText = "for";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "func":
                    ret.Detail = "type (functional term, short form)";
                    ret.SortText = "function02";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "function":
                    ret.Detail = "type (functional term)";
                    ret.SortText = "function01";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "iif":
                    ret.Detail = "predicate (equivalence, <=>)";
                    ret.SortText = "iif";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "impl":
                    ret.Detail = "predicate (implication, =>)";
                    ret.SortText = "impl";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "in":
                    ret.Detail = "clause (in type or in range)";
                    ret.SortText = "in";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "ind":
                    ret.Detail = "type (index, short form)";
                    ret.SortText = "index02";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "index":
                    ret.Detail = "type (index)";
                    ret.SortText = "index01";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "inf":
                    ret.Detail = "rule of inference (short form)";
                    ret.SortText = "inference02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "inference":
                    ret.Detail = "rule of inference";
                    ret.SortText = "inference01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "intr":
                    ret.Detail = "intrinsic (short form)";
                    ret.SortText = "intrinsic02";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "intrinsic":
                    ret.Detail = "intrinsic";
                    ret.SortText = "intrinsic01";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "is":
                    ret.Detail = "predicate (is of type)";
                    ret.SortText = "is";
                    ret.Kind = CompletionItemKind.Interface;
                    break;
                case "lem":
                    ret.Detail = "lemma (short form)";
                    ret.SortText = "lemma02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "lemma":
                    ret.Detail = "lemma";
                    ret.SortText = "lemma01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "loc":
                    ret.Detail = "localization (short form)";
                    ret.SortText = "localization02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "localization":
                    ret.Detail = "localization";
                    ret.SortText = "localization01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "not":
                    ret.Detail = "predicate (negation)";
                    ret.SortText = "not";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "obj":
                    ret.Detail = "type (object, short form)";
                    ret.SortText = "object02";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "object":
                    ret.Detail = "type (object)";
                    ret.SortText = "object01";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "opt":
                    ret.Detail = "optional (short form)";
                    ret.SortText = "optional02";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "optional":
                    ret.Detail = "optional";
                    ret.SortText = "optional01";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "or":
                    ret.Detail = "predicate (disjunction)";
                    ret.SortText = "or";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                case "post":
                    ret.Detail = "postulate (short form)";
                    ret.SortText = "postulate02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "postulate":
                    ret.Detail = "postulate";
                    ret.SortText = "postulate01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "pre":
                    ret.Detail = "premise (short form)";
                    ret.SortText = "premise02";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "pred":
                    ret.Detail = "type (predicate, short form)";
                    ret.SortText = "predicate02";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "predicate":
                    ret.Detail = "type (predicate)";
                    ret.SortText = "predicate01";
                    ret.Kind = CompletionItemKind.TypeParameter;
                    break;
                case "premise":
                    ret.Detail = "premise";
                    ret.SortText = "premise01";
                    ret.Kind = CompletionItemKind.Struct;
                    break;
                case "prop":
                    ret.Detail = "proposition (short form)";
                    ret.SortText = "proposition02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "proposition":
                    ret.Detail = "proposition";
                    ret.SortText = "proposition01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "prty":
                    ret.Detail = "property (short form)";
                    ret.SortText = "property02";
                    ret.Kind = CompletionItemKind.Value;
                    break;
                case "property":
                    ret.Detail = "property";
                    ret.SortText = "property01";
                    ret.Kind = CompletionItemKind.Value;
                    break;
                case "prf":
                    ret.Detail = "proof (short form)";
                    ret.SortText = "proof02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "proof":
                    ret.Detail = "proof";
                    ret.SortText = "proof01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "qed":
                    ret.Detail = "(quod erat demonstrandum)";
                    ret.SortText = "qed";
                    ret.Kind = CompletionItemKind.Text;
                    break;
                case "ret":
                    ret.Detail = "statement (return, short form)";
                    ret.SortText = "return02";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "return":
                    ret.Detail = "statement (return)";
                    ret.SortText = "return01";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "rev":
                    ret.Detail = "argument (revoke, short form)";
                    ret.SortText = "revoke02";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "revoke":
                    ret.Detail = "argument (revoke)";
                    ret.SortText = "revoke01";
                    ret.Kind = CompletionItemKind.Property;
                    break;
                case "self":
                    ret.Detail = "reference (to self)";
                    ret.SortText = "self";
                    ret.Kind = CompletionItemKind.Reference;
                    break;
                case "thm":
                    ret.Detail = "theorem (short form)";
                    ret.SortText = "theorem02";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "theorem":
                    ret.Detail = "theorem";
                    ret.SortText = "theorem01";
                    ret.Kind = CompletionItemKind.Class;
                    break;
                case "true":
                    ret.Detail = "predicate (true)";
                    ret.SortText = "true";
                    ret.Kind = CompletionItemKind.Constant;
                    break;
                case "trivial":
                    ret.Detail = "argument (trivial)";
                    ret.SortText = "trivial";
                    ret.Kind = CompletionItemKind.Text;
                    break;
                case "undef":
                    ret.Detail = "undefined (short form)";
                    ret.SortText = "undefined02";
                    ret.Kind = CompletionItemKind.Constant;
                    break;
                case "undefined":
                    ret.Detail = "undefined";
                    ret.SortText = "undefined01";
                    ret.Kind = CompletionItemKind.Constant;
                    break;
                case "uses":
                    ret.Detail = "clause (uses)";
                    ret.SortText = "uses";
                    ret.Kind = CompletionItemKind.Module;
                    break;
                case "xor":
                    ret.Detail = "predicate (exclusive or)";
                    ret.SortText = "xor";
                    ret.Kind = CompletionItemKind.Operator;
                    break;
                default:
                    ret.Detail = "";
                    ret.SortText = "";
                    break;
            }
            return ret;
        }

        public static string StripQuotesOrBrackets(string str)
        {
            if (str.StartsWith("'") && str.EndsWith("'") || str.StartsWith("<") && str.EndsWith(">"))
            {
                // strip quotes or brackets from label
                return str.Substring(1, str.Length - 2);
            }
            else
            {
                return str;
            }
        }

        public static CompletionItem GetCompletionItem(string word, string insertText = "")
        {
            var ret = FplAutoCompleteService.GetDetail(word);
            if (insertText != "")
            {
                ret.InsertText = insertText.Replace("<replace>", word);
            }
            else
            {
                ret.Kind = CompletionItemKind.Keyword;
                ret.Detail = $"keyword '{word}'";
                ret.SortText = "zzz" + ret.SortText; // display keywords later as non-keywords
            }
            return ret;
        }

    }


}
