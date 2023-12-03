using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesPredicate : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            switch (defaultCi.Word)
            {
                case "true":
                case "false":
                case "undef":
                case "undefined":
                    // keyword
                    var ciK = defaultCi.Clone(); ciK.Kind = CompletionItemKind.Keyword; ciK.AdjustToKeyword(); ret.Add(ciK);
                    break;
                case "not":
                    // snippet
                    var ci = defaultCi.Clone(); SetBody(ci, 1); ret.Add(ci);
                    // keyword
                    var ci1 = defaultCi.Clone(); ci1.Kind = CompletionItemKind.Keyword; ci1.AdjustToKeyword(); ret.Add(ci1);
                    break;
                case "iif":
                case "impl":
                    // snippet
                    var ci2 = defaultCi.Clone(); SetBody(ci2, 2); ret.Add(ci2);
                    // keyword
                    var ci2K = defaultCi.Clone(); ci2K.Kind = CompletionItemKind.Keyword; ci2K.AdjustToKeyword(); ret.Add(ci2K);
                    break;
                case "(":
                    // snippet for equality
                    var ciEquals = defaultCi.Clone(); SetBodyEquality(ciEquals); ret.Add(ciEquals);
                    break;
                case "and":
                case "or":
                case "xor":
                    var ci3 = defaultCi.Clone(); SetBody(ci3, 3); ret.Add(ci3);
                    var ci3K = defaultCi.Clone(); ci3K.Kind = CompletionItemKind.Keyword; ci3K.AdjustToKeyword(); ret.Add(ci3K);
                    break;
            }
            return ret;
        }

        public void SetBody(FplCompletionItem ci, int numbOfArgs)
        {
            switch (numbOfArgs)
            {
                case 0:
                    // no snippets for null-ary predicates (treat them as keywords only - see below)
                    break;
                case 1:
                    ci.InsertText = $"{ci.Word}{Environment.NewLine}({Environment.NewLine}" + $"\ttrue{Environment.NewLine}){Environment.NewLine}";
                    break;
                case 2:
                    ci.InsertText = $"{ci.Word}{Environment.NewLine}({Environment.NewLine}" + $"\tfalse{Environment.NewLine}\t,{Environment.NewLine}" + $"\ttrue{Environment.NewLine}){Environment.NewLine}";
                    break;
                default:
                    ci.InsertText = $"{ci.Word}{Environment.NewLine}({Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\ttrue,{Environment.NewLine}" + $"\tfalse{Environment.NewLine}){Environment.NewLine}";
                    break;
            }
            ci.Label += " ...";

        }

        public void SetBodyEquality(FplCompletionItem ci)
        {
            ci.InsertText = $"( x = y ) ";
            ci.Label = TokenPrefix + ci.InsertText + "...";
            ci.Detail = "equality";
            ci.SortText = "(";
            ci.Kind = CompletionItemKind.Operator;
        }

    }
}
