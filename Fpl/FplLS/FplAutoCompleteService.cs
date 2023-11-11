using FParsec;
using Microsoft.FSharp.Core;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using System.ComponentModel.Design;
using System.Data;
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

        public async Task<CompletionList> GetParserChoices(StringBuilder builder, int index, int line, int col)
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
                    case "<variable>":
                    case "<variable (got keyword)>":
                    case "<variable (got template)>":
                        modChoices.AddRange(AddVariableChoices(word));
                        break;
                    case "<PascalCaseId>":
                        modChoices.AddRange(AddPascalCaseIdChoices(word));
                        break;
                    case "del":
                    case "delegate":
                        modChoices.AddRange(AddDelegateChoices(word));
                        break;
                    case "is":
                        modChoices.AddRange(AddIsOperatorChoices(word));
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
                    case "self":
                    case "trivial":
                        modChoices.AddRange(AddKeywordChoices(word));
                        break;
                    case "true":
                    case "false":
                    case "undef":
                    case "undefined":
                        modChoices.AddRange(AddPredicateChoices(word, 0));
                        break;
                    case "all":
                    case "ex":
                    case "exn":
                        modChoices.AddRange(AddQuantorChoices(word));
                        break;
                    case "not":
                        modChoices.AddRange(AddPredicateChoices(word, 1));
                        break;
                    case "xor":
                    case "iif":
                    case "impl":
                        modChoices.AddRange(AddPredicateChoices(word, 2));
                        break;
                    case "and":
                    case "or":
                        modChoices.AddRange(AddPredicateChoices(word, 3));
                        break;
                    case "ctor":
                    case "constructor":
                        modChoices.AddRange(AddConstructorChoices(word));
                        break;
                    case "dec":
                    case "declaration":
                        modChoices.AddRange(AddDeclarationChoices(word));
                        break;
                    case "cases":
                        modChoices.AddRange(AddCasesChoices(word));
                        break;
                    case "for":
                        modChoices.AddRange(AddForChoices(word));
                        break;
                    case "prty":
                    case "property":
                        modChoices.AddRange(AddPropertyChoices(word));
                        break;
                    case "ax":
                    case "axiom":
                    case "post":
                    case "postulate":
                        modChoices.AddRange(AddAxiomChoices(word));
                        break;
                    case "def":
                    case "definition":
                        modChoices.AddRange(AddDefinitionChoices(word));
                        break;
                    case "thm":
                    case "theorem":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Theorem"));
                        break;
                    case "lem":
                    case "lemma":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Lemma"));
                        break;
                    case "prop":
                    case "proposition":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Proposition"));
                        break;
                    case "inf":
                    case "inference":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Inference"));
                        break;
                    case "conj":
                    case "conjecture":
                        modChoices.AddRange(AddTheoremLikeStatementChoices(word, "Conjecture"));
                        break;
                    case "cor":
                    case "corollary":
                        modChoices.AddRange(AddCorollaryChoices(word));
                        break;
                    case "prf":
                    case "proof":
                        modChoices.AddRange(AddProofChoices(word));
                        break;
                    case "loc":
                    case "localization":
                        modChoices.AddRange(AddLocalizationChoices(word));
                        break;
                    case "uses":
                        modChoices.AddRange(AddUsesChoices(word));
                        break;
                    default:
                        modChoices.AddRange(AddDefaultChoices(word));
                        break;
                }
            }
            return new CompletionList(modChoices);
        }

        public static List<CompletionItem> AddAxiomChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippet
            modChoices.Add(GetCompletionItem(word, $"<replace> SomeFplIdentifier (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\ttrue{Environment.NewLine}" + "}" + Environment.NewLine));
            // keyword
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        public static List<CompletionItem> AddPredicateChoices(string word, int numbOfArgs)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            switch (numbOfArgs)
            {
                case 0:
                    // no snippets for null-ary predicates (treat them as keywords only - see below)
                    break;
                case 1:
                    modChoices.Add(GetCompletionItem(word, $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine})" + Environment.NewLine));
                    break;
                case 2:
                    modChoices.Add(GetCompletionItem(word, $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\tfalse{Environment.NewLine})" + Environment.NewLine));
                    break;
                default:
                    modChoices.Add(GetCompletionItem(word, $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\tfalse{Environment.NewLine})" + Environment.NewLine));
                    break;
            }
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        public static List<CompletionItem> AddQuantorChoices(string word)
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
            modChoices.Add(ci);
            var ci1 = GetCompletionItem(word, $"<replace>{postfix} x in someVariadicVar ({Environment.NewLine}" + $"\tp(x){Environment.NewLine})" + Environment.NewLine);
            ci1.Label = $"{prefix}ex x in variable (..)"; ReplaceLabel(word, ci1);
            ci1.Detail = "predicate (exists quantor)"; ReplaceDetails(word, ci1);
            ci1.SortText = "ex01"; ReplaceSortText(word, ci1);
            modChoices.Add(ci1);
            var ci2 = GetCompletionItem(word, $"<replace>{postfix} x in [a,b] ({Environment.NewLine}" + $"\tp(x){Environment.NewLine})" + Environment.NewLine);
            ci2.Label = $"{prefix}ex x in range (..)"; ReplaceLabel(word, ci2);
            ci2.Detail = "predicate (exists quantor)"; ReplaceDetails(word, ci2);
            ci2.SortText = "ex02"; ReplaceSortText(word, ci2);
            modChoices.Add(ci2);
            var ci3 = GetCompletionItem(word, $"<replace>{postfix} x in object ({Environment.NewLine}" + $"\tp(x){Environment.NewLine})" + Environment.NewLine);
            ci3.Label = $"{prefix}ex x in type (..)"; ReplaceLabel(word, ci3);
            ci3.Detail = "predicate (exists quantor)"; ReplaceDetails(word, ci3);
            ci3.SortText = "ex03"; ReplaceSortText(word, ci3);
            modChoices.Add(ci3);

            if (isExn)
            {
                // keywords
                var ci4 = GetCompletionItem("exn!");
                ci4.SortText = "exn!04";
                modChoices.Add(ci4);
            }
            else
            {
                // this combined snippet only available for 'ex' and 'all' but not for exn!
                var ci5 = GetCompletionItem(word, $"<replace> x in [a,b], y in c, z ({Environment.NewLine}" + $"\tp(x,y,z){Environment.NewLine})" + Environment.NewLine);
                ci5.Label = $"{prefix}ex <combined> (..)"; ReplaceLabel(word, ci5);
                ci5.Detail = "predicate (exists quantor in <combined>)"; ReplaceDetails(word, ci5);
                ci5.SortText = "ex04"; ReplaceSortText(word, ci5);
                modChoices.Add(ci5);
                // keywords
                var ci6 = GetCompletionItem(word);
                ci6.SortText = "ex05"; ReplaceSortText(word, ci6);
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

        public static List<CompletionItem> AddConstructorChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippet
            var ci = GetCompletionItem(word, GetConstructorSnippet(word));
            modChoices.Add(ci);
            // keyword
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        public static List<CompletionItem> AddPropertyChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippet
            var ciMandCl = GetCompletionItem(word, GetClassInstanceSnippet(word, false, out string optMand, out string objMand));
            ciMandCl.Label += $"{prefix}{objMand}";
            ciMandCl.Detail = $"mandatory object property";
            modChoices.Add(ciMandCl);
            var ciMandPr = GetCompletionItem(word, GetPredicateInstanceSnippet(word, false));
            ciMandPr.Label += $"{prefix}{objMand}";
            ciMandPr.Detail = $"mandatory predicative property";
            modChoices.Add(ciMandPr);
            var ciMandFu = GetCompletionItem(word, GetFunctionalTermInstanceSnippet(word, false));
            ciMandFu.Label += $"{prefix}{objMand}";
            ciMandFu.Detail = $"mandatory functional property";
            modChoices.Add(ciMandFu);
            var ciOptCl = GetCompletionItem(word, GetClassInstanceSnippet(word, true, out string optOpt, out string objOpt));
            ciOptCl.Label += $"{prefix}{optOpt} {objOpt}";
            ciOptCl.Detail = $"optional object property";
            modChoices.Add(ciOptCl);
            var ciOptPr = GetCompletionItem(word, GetPredicateInstanceSnippet(word, true));
            ciOptPr.Label += $"{prefix}{optOpt} {objOpt}";
            ciOptPr.Detail = $"optional predicative property";
            modChoices.Add(ciOptPr);
            var ciOptFu = GetCompletionItem(word, GetFunctionalTermInstanceSnippet(word, true));
            ciOptFu.Label += $"{prefix}{optOpt} {objOpt}";
            ciOptFu.Detail = $"optional functional property";
            modChoices.Add(ciOptFu);
            // keyword
            var ciMandClKw = GetCompletionItem(word);
            ciMandClKw.Label = ciMandCl.Label;
            ciMandClKw.Detail = ciMandCl.Detail;
            modChoices.Add(ciMandClKw);
            var ciMandPrKw = GetCompletionItem(word);
            ciMandPrKw.Label = ciMandPr.Label;
            ciMandPrKw.Detail = ciMandPr.Detail;
            modChoices.Add(ciMandPrKw);
            var ciMandFuKw = GetCompletionItem(word);
            ciMandFuKw.Label = ciMandFu.Label;
            ciMandFuKw.Detail = ciMandFu.Detail;
            modChoices.Add(ciMandFuKw);
            var ciOptClKw = GetCompletionItem(word);
            ciOptClKw.Label = ciOptCl.Label;
            ciOptClKw.Detail = ciOptCl.Detail;
            modChoices.Add(ciOptClKw);
            var ciOptPrKw = GetCompletionItem(word);
            ciOptPrKw.Label = ciOptPr.Label;
            ciOptPrKw.Detail = ciOptPr.Detail;
            modChoices.Add(ciOptPrKw);
            var ciOptFuKw = GetCompletionItem(word);
            ciOptFuKw.Label = ciOptFu.Label;
            ciOptFuKw.Detail = ciOptFu.Detail;
            modChoices.Add(ciOptFuKw);
            return modChoices;
        }

        public static List<CompletionItem> AddDeclarationChoices(string word)
        {
            var modChoices = new List<CompletionItem>();

            // snippet
            var ci = GetCompletionItem(word, GetDeclarationSnippet(word));
            modChoices.Add(ci);
            // keyword
            var ci1 = GetCompletionItem(word);
            modChoices.Add(ci1);
            return modChoices;
        }


        public static List<CompletionItem> AddCasesChoices(string word)
        {
            var modChoices = new List<CompletionItem>();

            // snippet
            var ci = GetCompletionItem(word, GetCasesStatement());
            modChoices.Add(ci);
            // keyword
            var ci1 = GetCompletionItem(word);
            modChoices.Add(ci1);
            return modChoices;
        }

        public static List<CompletionItem> AddForChoices(string word)
        {
            var modChoices = new List<CompletionItem>();

            // snippet
            var ci = GetCompletionItem(word, GetForStatement(true));
            ci.Label = $"{prefix}for .. []";
            ci.Detail = "for statement (range)";
            modChoices.Add(ci);
            var ci1 = GetCompletionItem(word, GetForStatement(false));
            ci1.Label = $"{prefix}for .. list";
            ci1.Detail = "for statement (list)";
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
                    objType = "object";
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
                firstLine = $"{Environment.NewLine}{word} {optStr} {objType} SomeObjectProperty()";
            }
            else
            {
                firstLine = $"{Environment.NewLine}{word} {objType} SomeObjectProperty()";
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
                firstLine = $"{Environment.NewLine}{word} {optStr} {objType} SomeFunctionProperty() -> {objStr}";
            }
            else
            {
                firstLine = $"{Environment.NewLine}{word} {objType} SomeFunctionProperty() -> {objStr}";
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
                firstLine = $"{Environment.NewLine}{word} {optStr} {objType} SomePredicateProperty()";
            }
            else
            {
                firstLine = $"{Environment.NewLine}{word} {objType} SomePredicateProperty()";
            }

            return
                firstLine +
                $"{Environment.NewLine}{leftBrace}" +
                $"{Environment.NewLine}\t{intrinsic}" +
                $"{Environment.NewLine}{rightBrace}" +
                $"{Environment.NewLine}";
        }

        private List<CompletionItem> AddTheoremLikeStatementChoices(string word, string example)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            modChoices.Add(GetCompletionItem(word, $"<replace> SomeFpl{example} (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tpre: true{Environment.NewLine}\tcon: true{Environment.NewLine}" + "}" + Environment.NewLine));
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }


        private List<CompletionItem> AddCorollaryChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            modChoices.Add(GetCompletionItem(word, $"<replace> SomeFplTheorem!1 (){Environment.NewLine}" + "{" + $"{Environment.NewLine}\tpre: true{Environment.NewLine}\tcon: true{Environment.NewLine}" + "}" + Environment.NewLine));
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        private List<CompletionItem> AddProofChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            modChoices.Add(GetCompletionItem(word, $"<replace> SomeFplTheorem!1{Environment.NewLine}" + "{" + $"{Environment.NewLine}\t1. |- qed{Environment.NewLine}" + "}" + Environment.NewLine));
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        private List<CompletionItem> AddLocalizationChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            modChoices.Add(GetCompletionItem(word, $"<replace> iif(x,y) :={Environment.NewLine}!tex: x \"\\Leftrightarrow\" y{Environment.NewLine}!eng: x \" if and only if \" y{Environment.NewLine}!eng: x \" dann und nur dann \" y{Environment.NewLine}" + ";" + Environment.NewLine));
            // keywords
            modChoices.Add(GetCompletionItem(word));
            return modChoices;
        }

        private List<CompletionItem> AddUsesChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            // snippets
            var ci = GetCompletionItem(word, $"<replace> SomeFplNamespace{Environment.NewLine}");
            ci.Detail = "uses namespace";
            var ci1 = GetCompletionItem(word, $"<replace> SomeFplNamespace alias Sfn{Environment.NewLine}");
            ci1.Detail = "uses namespace with alias";
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
            ci.Kind = CompletionItemKind.Value;
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


        private List<CompletionItem> AddKeywordChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.InsertText = word;
            ci.Label = prefix + ci.InsertText;
            ci.Detail = $"keyword '{word}'";
            ci.Kind = CompletionItemKind.Keyword;
            modChoices.Add(ci);
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
            ci.SortText = "zzzzzzzzz"; // make sure whitespaces appears at the end of any list.
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddDelegateChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = "delegate";
            ci.InsertText = word + ".SomeExternalMethod(x,1)";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Snippet;
            modChoices.Add(ci);
            return modChoices;
        }

        private List<CompletionItem> AddIsOperatorChoices(string word)
        {
            var modChoices = new List<CompletionItem>();
            var ci = new CompletionItem();
            ci.Detail = "is operator";
            ci.InsertText = word + "(x, SomeFplType)";
            ci.Label = prefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Snippet;
            modChoices.Add(ci);

            var ci1 = new CompletionItem();
            ci1.Detail = "keyword 'is'";
            ci1.InsertText = word;
            ci1.Label = prefix + ci1.InsertText;
            ci1.Kind = CompletionItemKind.Keyword;
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

        public static string GetDetail(string word, out string sortText, out KeywordKind keywordKind)
        {
            string ret;
            switch (word)
            {
                case "alias":
                    ret = "alias";
                    sortText = "alias";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "all":
                    ret = "predicate (all quantor)";
                    sortText = "all";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "and":
                    ret = "predicate (conjunction)";
                    sortText = "and";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "ass":
                    ret = "argument (assume, short form)";
                    sortText = "assume02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "assert":
                    ret = "statement (assert)";
                    sortText = "assert";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "assume":
                    ret = "argument (assume)";
                    sortText = "assume01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "ax":
                    ret = "axiom (short form)";
                    sortText = "axiom02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "axiom":
                    ret = "axiom";
                    sortText = "axiom01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "cases":
                    ret = "statement (cases)";
                    sortText = "cases";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "cl":
                    ret = "class (short form)";
                    sortText = "class02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "class":
                    ret = "class";
                    sortText = "class01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "con":
                    ret = "conclusion (short form)";
                    sortText = "conclusion02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "conclusion":
                    ret = "conclusion";
                    sortText = "conclusion01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "cor":
                    ret = "corollary (short form)";
                    sortText = "corollary02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "corollary":
                    ret = "corollary";
                    sortText = "corollary01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "conj":
                    ret = "conjecture (short form)";
                    sortText = "conjecture02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "conjecture":
                    ret = "conjecture";
                    sortText = "conjecture01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "ctr":
                    ret = "constructor (short form)";
                    sortText = "constructor02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "constructor":
                    ret = "constructor";
                    sortText = "constructor01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "dec":
                    ret = "declaration (short form)";
                    sortText = "declaration02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "declaration":
                    ret = "declaration";
                    sortText = "declaration01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "del":
                    ret = "delegate (short form)";
                    sortText = "delegate02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "delegate":
                    ret = "delegate";
                    sortText = "delegate01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "def":
                    ret = "definition (short form)";
                    sortText = "definition02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "definition":
                    ret = "definition";
                    sortText = "definition01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "end":
                    ret = "extension (end of)";
                    sortText = "end";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "ex":
                    ret = "predicate (exists quantor)";
                    sortText = "ex";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "exn":
                    ret = "predicate (exists n-times quantor)";
                    sortText = "exn";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "exn!":
                    ret = "predicate (exists n-times quantor)";
                    sortText = "exn!";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "ext":
                    ret = "extension (beginning of)";
                    sortText = "ext";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "false":
                    ret = "predicate (false)";
                    sortText = "false";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "for":
                    ret = "statement (for loop)";
                    sortText = "for";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "func":
                    ret = "type (functional term, short form)";
                    sortText = "function02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "function":
                    ret = "type (functional term)";
                    sortText = "function01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "iif":
                    ret = "predicate (equivalence, <=>)";
                    sortText = "iif";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "impl":
                    ret = "predicate (implication, =>)";
                    sortText = "impl";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "in":
                    ret = "clause (in type or in range)";
                    sortText = "in";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "ind":
                    ret = "type (index, short form)";
                    sortText = "index02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "index":
                    ret = "type (index)";
                    sortText = "index01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "inf":
                    ret = "rule of inference (short form)";
                    sortText = "inference02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "inference":
                    ret = "rule of inference";
                    sortText = "inference01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "intr":
                    ret = "intrinsic (short form)";
                    sortText = "intrinsic02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "intrinsic":
                    ret = "intrinsic";
                    sortText = "intrinsic01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "is":
                    ret = "predicate (is of type)";
                    sortText = "is";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "lem":
                    ret = "lemma (short form)";
                    sortText = "lemma02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "lemma":
                    ret = "lemma";
                    sortText = "lemma01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "loc":
                    ret = "localization (short form)";
                    sortText = "localization02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "localization":
                    ret = "localization";
                    sortText = "localization01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "not":
                    ret = "predicate (negation)";
                    sortText = "not";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "obj":
                    ret = "type (object, short form)";
                    sortText = "object02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "object":
                    ret = "type (object)";
                    sortText = "object01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "opt":
                    ret = "optional (short form)";
                    sortText = "optional02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "optional":
                    ret = "optional";
                    sortText = "optional01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "or":
                    ret = "predicate (disjunction)";
                    sortText = "or";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "post":
                    ret = "postulate (short form)";
                    sortText = "postulate02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "postulate":
                    ret = "postulate";
                    sortText = "postulate01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "pre":
                    ret = "premise (short form)";
                    sortText = "premise02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "pred":
                    ret = "type (predicate, short form)";
                    sortText = "predicate02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "predicate":
                    ret = "type (predicate)";
                    sortText = "predicate01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "premise":
                    ret = "premise";
                    sortText = "premise01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "prop":
                    ret = "proposition (short form)";
                    sortText = "proposition02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "proposition":
                    ret = "proposition";
                    sortText = "proposition01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "prty":
                    ret = "property (short form)";
                    sortText = "property02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "property":
                    ret = "property";
                    sortText = "property01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "prf":
                    ret = "proof (short form)";
                    sortText = "proof02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "proof":
                    ret = "proof";
                    sortText = "proof01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "qed":
                    ret = "conclusion (quod erat demonstrandum)";
                    sortText = "qed";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "ret":
                    ret = "statement (return, short form)";
                    sortText = "return02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "return":
                    ret = "statement (return)";
                    sortText = "return01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "rev":
                    ret = "argument (revoke, short form)";
                    sortText = "revoke02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "revoke":
                    ret = "argument (revoke)";
                    sortText = "revoke";
                    keywordKind = KeywordKind.Long;
                    break;
                case "self":
                    ret = "reference (to self)";
                    sortText = "self";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "thm":
                    ret = "theorem (short form)";
                    sortText = "theorem02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "theorem":
                    ret = "theorem";
                    sortText = "theorem01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "true":
                    ret = "predicate (true)";
                    sortText = "true";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "trivial":
                    ret = "argument (trivial)";
                    sortText = "trivial";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "undef":
                    ret = "undefined (short form)";
                    sortText = "undefined02";
                    keywordKind = KeywordKind.Short;
                    break;
                case "undefined":
                    ret = "undefined";
                    sortText = "undefined01";
                    keywordKind = KeywordKind.Long;
                    break;
                case "uses":
                    ret = "clause (uses)";
                    sortText = "uses";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                case "xor":
                    ret = "predicate (exclusive or)";
                    sortText = "xor";
                    keywordKind = KeywordKind.EitherNor;
                    break;
                default:
                    ret = "";
                    sortText = "";
                    keywordKind = KeywordKind.EitherNor;
                    break;
            }
            return ret;
        }

        private static string StripQuotesOrBrackets(string str)
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
            var ret = new CompletionItem();
            ret.Detail = FplAutoCompleteService.GetDetail(word, out string sortText, out KeywordKind keywordKind);
            if (insertText != "")
            {
                ret.InsertText = insertText.Replace("<replace>", word);
                ret.Label = prefix + word;
                ret.Kind = CompletionItemKind.Snippet;
                if (ret.Detail.Contains("short"))
                {
                    ret.SortText = sortText + "03";
                }
                else
                {
                    ret.SortText = sortText + "01";
                }
            }
            else
            {
                ret.Kind = CompletionItemKind.Keyword;
                ret.InsertText = word;
                ret.Label = prefix + ret.InsertText;
                ret.Detail = $"keyword '{word}'";
                if (ret.Detail.Contains("short"))
                {
                    ret.SortText = sortText + "04";
                }
                else
                {
                    ret.SortText = sortText + "02";
                }
            }
            return ret;
        }

    }


}
