using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;
namespace FplLS
{

    public class FplCompletionItemChoicesProperty : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci1 = defaultCi.Clone(); SetBody(ci1, "Predicate", false); ret.Add(ci1);
            var ci2 = defaultCi.Clone(); SetBody(ci2, "Function", false); ret.Add(ci2);

            // keyword
            var ciK1 = defaultCi.Clone(); SetBody(ciK1, "Predicate", true); ret.Add(ciK1);
            var ciK2 = defaultCi.Clone(); SetBody(ciK2, "Function", true); ret.Add(ciK2);
            return ret;
        }

        private void SetBody(FplCompletionItem ci, string propertyType, bool isKeyword)
        {
            SetSortText(ci,propertyType);
            if (ci.IsShort)
            {
                TokenIntrinsic = LiteralIntr;
                TokenFunction = LiteralFunc;
                TokenPredicate = LiteralPred;
                if (isKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, propertyType);
                    ci.Detail = $"keywords '{ci.Label.Substring(TokenPrefix.Length)}'";
                    ci.SortText = "zzz" + ci.SortText;
                    ci.InsertText = ci.Label.Substring(TokenPrefix.Length);
                    ci.Kind = CompletionItemKind.Keyword;
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, propertyType) + " ...";
                    ci.Detail = $"{propertyType.ToLower()} property (short)";
                    ci.InsertText = GetInsertText(ci, propertyType);
                }
            }
            else
            {
                if (isKeyword)
                {
                    ci.Label = GetLabelKeyword(ci, propertyType);
                    ci.Detail = $"keywords '{ci.Label.Substring(TokenPrefix.Length)}'";
                    ci.SortText = "zzz" + ci.SortText;
                    ci.InsertText = ci.Label.Substring(TokenPrefix.Length);
                    ci.Kind = CompletionItemKind.Keyword;
                }
                else
                {
                    ci.Label = GetLabelKeyword(ci, propertyType) + " ...";
                    ci.Detail = $"{propertyType.ToLower()} property";
                    ci.InsertText = GetInsertText(ci, propertyType);
                }
            }
        }

        public string GetLabelKeyword(FplCompletionItem ci, string propertyType)
        {
            string ret;

            switch (propertyType)
            {
                case "Function":
                    ret = $"{TokenPrefix}{ci.Word} {TokenFunction}";
                    break;
                case "Predicate":
                default:
                    ret = $"{TokenPrefix}{ci.Word} {TokenPredicate}";
                    break;
            }

            return ret;
        }

        public string GetInsertText(FplCompletionItem ci, string propertyType)
        {
            string ret;
            switch (propertyType)
            {
                case "Function":
                    ret = $"{GetLabelKeyword(ci, propertyType).Replace("_ ","")} SomeFpl{propertyType}Property() -> {TokenObject}{Environment.NewLine}";
                    break;
                case "Predicate":
                default:
                    ret = $"{GetLabelKeyword(ci, propertyType).Replace("_ ", "")} SomeFpl{propertyType}Property(){Environment.NewLine}";
                    break;

            }
            return ret +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t{TokenIntrinsic}{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
        }

        private void SetSortText(FplCompletionItem ci, string propertyType)
        {

            switch (propertyType)
            {
                case "Function":
                    ci.SortText = "property03";
                    break;
                case "Predicate":
                default:
                    ci.SortText = "property02";
                    break;

            }

            if (ci.IsShort)
            {
                ci.SortText = "z" + ci.SortText;
            }

        }

    }
}