using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesPredicate : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetBody(ci, "Class", false); ret.Add(ci);
            var ci1 = defaultCi.Clone(); SetBody(ci1, "Predicate", false); ret.Add(ci1);
            var ci2 = defaultCi.Clone(); SetBody(ci2, "Function", false); ret.Add(ci2);

            // keyword
            var ciK = defaultCi.Clone(); SetBody(ciK, "Class", false); ret.Add(ciK);
            var ciK1 = defaultCi.Clone(); SetBody(ciK1, "Predicate", false); ret.Add(ciK1);
            var ciK2 = defaultCi.Clone(); SetBody(ciK2, "Function", false); ret.Add(ciK2);
            return ret;
        }

        private void SetBody(FplCompletionItem ci, string definitionType, bool forKeyword)
        {
            if (ci.IsShort)
            {
                TokenIntrinsic = "intr";
                TokenObject = "obj";
                TokenFunction = "func";
                TokenPredicate = "pred";
                if (forKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, definitionType);
                    ci.Detail = $"keywords '{ci.Label}'";
                    ci.SortText = "zzz" + ci.SortText;
                    ci.InsertText = ci.Label;
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, definitionType) + " ...";
                    ci.Detail = $"{definitionType.ToLower()} definition (short)";
                    ci.SortText = "z" + ci.SortText;
                    ci.InsertText = GetBody(ci, definitionType);
                }
            }
            else
            {
                if (forKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, definitionType);
                    ci.Detail = $"keywords '{ci.Label}'";
                    ci.SortText = "zzz" + ci.SortText;
                    ci.InsertText = ci.Label;
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, definitionType) + " ...";
                    ci.Detail = $"{definitionType.ToLower()} definition";
                    ci.SortText = "z" + ci.SortText;
                    ci.InsertText = GetBody(ci, definitionType);
                }
            }
        }

        public string GetLabelKeyword(FplCompletionItem ci, string definitionType)
        {
            switch (definitionType)
            {
                case "Class":
                    ci.SortText = "definition01";
                    return $"{TokenPrefix}{ci.Word} {TokenClass}'";
                case "Function":
                    ci.SortText = "definition03";
                    return $"{TokenPrefix}{ci.Word} {TokenFunction}'";
                case "Predicate":
                default:
                    ci.SortText = "definition02";
                    return $"{TokenPrefix}{ci.Word} {TokenPredicate}'";
            }
        }

        public void SetBody(FplCompletionItem ci, int numbOfArgs)
        {
            switch (numbOfArgs)
            {
                case 0:
                    // no snippets for null-ary predicates (treat them as keywords only - see below)
                    break;
                case 1:
                    ci.InsertText = $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine})" + Environment.NewLine;
                    break;
                case 2:
                    ci.InsertText = $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\tfalse{Environment.NewLine})" + Environment.NewLine;
                    break;
                default:
                    ci.InsertText = $"<replace> ({Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\tfalse{Environment.NewLine})" + Environment.NewLine;
                    break;
            }


        }





        public static List<CompletionItem> AddQuantorChoices(CompletionItem defaultCi)
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

    }
}
