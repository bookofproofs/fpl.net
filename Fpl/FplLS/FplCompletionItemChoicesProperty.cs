using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;
namespace FplLS
{

    public class FplCompletionItemChoicesProperty : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            ret.Add(BuildProperty(defaultCi, "Predicate", false));
            ret.Add(BuildProperty(defaultCi, "Function", false));

            // keyword variants
            ret.Add(BuildProperty(defaultCi.WithKind(CompletionItemKind.Keyword), "Predicate", true));
            ret.Add(BuildProperty(defaultCi.WithKind(CompletionItemKind.Keyword), "Function", true));
            return ret;
        }

        private FplCompletionItem BuildProperty(FplCompletionItem baseCi, string propertyType, bool isKeyword)
        {
            // compute base sort text
            var baseSort = propertyType == "Function" ? "property03" : "property02";
            if (baseCi.IsShort)
            {
                baseSort = "z" + baseSort;
            }

            if (baseCi.IsShort)
            {
                TokenIntrinsic = LiteralIntr;
                TokenFunction = LiteralFunc;
                TokenPredicate = LiteralPred;
                if (isKeyword)
                {
                    var label = GetLabelKeyword(baseCi, propertyType);
                    var insert = label.Substring(TokenPrefix.Length);
                    var detail = $"keywords '{insert}'";
                    return baseCi.WithLabel(label).WithDetail(detail).WithSortText("zzz" + baseSort).WithInsertText(insert).WithKind(CompletionItemKind.Keyword);
                }
                else
                {
                    var label = GetLabelKeyword(baseCi, propertyType) + " ...";
                    var insert = GetInsertText(baseCi, propertyType);
                    var detail = $"{propertyType.ToLower()} property (short)";
                    return baseCi.WithLabel(label).WithDetail(detail).WithInsertText(insert).WithSortText(baseSort);
                }
            }
            else
            {
                if (isKeyword)
                {
                    var label = GetLabelKeyword(baseCi, propertyType);
                    var insert = label.Substring(TokenPrefix.Length);
                    var detail = $"keywords '{insert}'";
                    return baseCi.WithLabel(label).WithDetail(detail).WithSortText("zzz" + baseSort).WithInsertText(insert).WithKind(CompletionItemKind.Keyword);
                }
                else
                {
                    var label = GetLabelKeyword(baseCi, propertyType) + " ...";
                    var insert = GetInsertText(baseCi, propertyType);
                    var detail = $"{propertyType.ToLower()} property";
                    return baseCi.WithLabel(label).WithDetail(detail).WithInsertText(insert).WithSortText(baseSort);
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
                    ret = $"{GetLabelKeyword(ci, propertyType).Replace("_ ", "")} SomeFpl{propertyType}Property() -> {TokenObject}{Environment.NewLine}";
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

    }
}
