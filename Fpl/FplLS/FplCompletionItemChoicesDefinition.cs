using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLS
{

    public class FplCompletionItemChoicesDefinition : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetBody(ci, "Class", false); ret.Add(ci); ci.Label += " ...";
            var ci1 = defaultCi.Clone(); SetBody(ci1, "Predicate", false); ret.Add(ci1); ci1.Label += " ...";
            var ci2 = defaultCi.Clone(); SetBody(ci2, "Function", false); ret.Add(ci2); ci2.Label += " ...";

            // keyword
            defaultCi.Kind = CompletionItemKind.Keyword;
            var ciK = defaultCi.Clone(); SetBody(ciK, "Class", true); ret.Add(ciK);
            var ciK1 = defaultCi.Clone(); SetBody(ciK1, "Predicate", true); ret.Add(ciK1);
            var ciK2 = defaultCi.Clone(); SetBody(ciK2, "Function", true); ret.Add(ciK2);
            return ret;
        }

        private void SetBody(FplCompletionItem ci, string definitionType, bool forKeyword)
        {
            if (ci.IsShort)
            {
                TokenIntrinsic = LiteralIntr;
                TokenObject = LiteralObj;
                TokenFunction = LiteralFunc;
                TokenPredicate = LiteralPred;
                TokenClass = LiteralCl;
                if (forKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, definitionType);
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
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, definitionType) + " ...";
                    ci.Detail = $"{definitionType.ToLower()} definition";
                    ci.InsertText = GetBody(ci, definitionType);
                }
            }
        }

        public string GetLabelKeyword(FplCompletionItem ci, string definitionType)
        {
            string ret;
            switch (definitionType)
            {
                case "Class":
                    ci.SortText = "definition01";
                    ret = $"{TokenPrefix}{ci.Word} {TokenClass}";
                    break;
                case "Function":
                    ci.SortText = "definition03";
                    ret = $"{TokenPrefix}{ci.Word} {TokenFunction}";
                    break;
                case "Predicate":
                default:
                    ci.SortText = "definition02";
                    ret = $"{TokenPrefix}{ci.Word} {TokenPredicate}";
                    break;
            }
            ci.Label = ret;
            ci.AdjustToKeyword();
            return ret;

        }

        public string GetBody(FplCompletionItem ci, string definitionType)
        {
            string ret;
            switch (definitionType)
            {
                case "Class":
                    ret = $"{GetLabelKeyword(ci, definitionType).Substring(TokenPrefix.Length)} SomeFpl{definitionType}: {TokenObject}"; 
                    break;
                case "Function":
                    ret = $"{GetLabelKeyword(ci, definitionType).Substring(TokenPrefix.Length)} SomeFpl{definitionType}() -> {TokenObject}";
                    break;
                case "Predicate":
                default:
                    ret = $"{ GetLabelKeyword(ci, definitionType).Substring(TokenPrefix.Length)} SomeFpl{definitionType}()";
                    break;

            }
            return ret + Environment.NewLine;
        }

    }
}
