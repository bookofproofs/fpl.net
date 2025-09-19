using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

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
                case LiteralTrue:
                case LiteralFalse:
                case LiteralUndef:
                case LiteralUndefL:
                    // keyword
                    var ciK = defaultCi.Clone(); ciK.Kind = CompletionItemKind.Keyword; ciK.AdjustToKeyword(); ret.Add(ciK);
                    break;
                case LiteralNot:
                    // snippet
                    var ci = defaultCi.Clone(); SetBody(ci, 1); ret.Add(ci);
                    // keyword
                    var ci1 = defaultCi.Clone(); ci1.Kind = CompletionItemKind.Keyword; ci1.AdjustToKeyword(); ret.Add(ci1);
                    break;
                case LiteralIif:
                case LiteralImpl:
                    // snippet
                    var ci2 = defaultCi.Clone(); SetBody(ci2, 2); ret.Add(ci2);
                    // keyword
                    var ci2K = defaultCi.Clone(); ci2K.Kind = CompletionItemKind.Keyword; ci2K.AdjustToKeyword(); ret.Add(ci2K);
                    break;
                case "(":
                    // snippet for equality
                    var ciEquals = defaultCi.Clone(); SetBodyEquality(ciEquals); ret.Add(ciEquals);
                    break;
                case LiteralAnd:
                case LiteralOr:
                case LiteralXor:
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
                    ci.InsertText = $"{ci.Word} true ";
                    break;
                case 2:
                    ci.InsertText = $"{ci.Word} ( false, true ) ";
                    break;
                default:
                    ci.InsertText = $"{ci.Word} ( true, false ) ";
                    break;
            }
            ci.Label += " ...";

        }

        public void SetBodyEquality(FplCompletionItem ci)
        {
            ci.InsertText = $"( x = y ) ";
            ci.Label = TokenPrefix + ci.InsertText + "...";
            ci.Detail = PrimEqualityL;
            ci.SortText = "(";
            ci.Kind = CompletionItemKind.Operator;
        }

    }
}
