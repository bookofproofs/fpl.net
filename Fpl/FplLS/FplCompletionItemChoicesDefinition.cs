using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{

    public class FplCompletionItemChoicesDefinition : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            ret.Add(BuildDefinition(defaultCi, "Class", false));
            ret.Add(BuildDefinition(defaultCi, "Predicate", false));
            ret.Add(BuildDefinition(defaultCi, "Function", false));

            // keyword variants
            ret.Add(BuildDefinition(defaultCi.WithKind(CompletionItemKind.Keyword), "Class", true).WithKeyword());
            ret.Add(BuildDefinition(defaultCi.WithKind(CompletionItemKind.Keyword), "Predicate", true).WithKeyword());
            ret.Add(BuildDefinition(defaultCi.WithKind(CompletionItemKind.Keyword), "Function", true).WithKeyword());
            return ret;
        }

        private FplCompletionItem BuildDefinition(FplCompletionItem baseCi, string definitionType, bool forKeyword)
        {
            // tokens for short vs long handled below
            if (baseCi.IsShort)
            {
                TokenIntrinsic = LiteralIntr;
                TokenObject = LiteralObj;
                TokenFunction = LiteralFunc;
                TokenPredicate = LiteralPred;
                TokenClass = LiteralCl;

                // compute the base sort for each subtype (short forms include leading 'z')
                var baseSort = "z" + (definitionType == "Class" ? "definition01" : definitionType == "Function" ? "definition03" : "definition02");

                if (forKeyword)
                {
                    var label = GetLabelKeyword(definitionType, baseCi);
                    // ensure keyword variant receives the proper short-marked base sort
                    return baseCi.WithLabel(label).WithSortText(baseSort);
                }
                else
                {
                    var label = GetLabelKeyword(definitionType, baseCi) + " ...";
                    var detail = $"{definitionType.ToLower()} definition (short)";
                    return baseCi.WithLabel(label).WithDetail(detail).WithSortText(baseSort).WithInsertText(GetBody(definitionType, baseCi));
                }
            }
            else
            {
                // non-short / long forms: compute base sort WITHOUT 'z'
                var baseSort = definitionType == "Class" ? "definition01" : definitionType == "Function" ? "definition03" : "definition02";

                if (forKeyword)
                {
                    var label = GetLabelKeyword(definitionType, baseCi);
                    // ensure keyword variant has correct base sort so .WithKeyword() prefixes "zzz" correctly
                    return baseCi.WithLabel(label).WithSortText(baseSort);
                }
                else
                {
                    var label = GetLabelKeyword(definitionType, baseCi) + " ...";
                    var detail = $"{definitionType.ToLower()} definition";
                    return baseCi.WithLabel(label).WithDetail(detail).WithSortText(baseSort).WithInsertText(GetBody(definitionType, baseCi));
                }
            }
        }

        private string GetLabelKeyword(string definitionType, FplCompletionItem ci)
        {
            return definitionType switch
            {
                "Class" => $"{TokenPrefix}{ci.Word} {TokenClass}",
                "Function" => $"{TokenPrefix}{ci.Word} {TokenFunction}",
                _ => $"{TokenPrefix}{ci.Word} {TokenPredicate}"
            };
        }

        public string GetBody(string definitionType, FplCompletionItem ci)
        {
            string ret;
            switch (definitionType)
            {
                case "Class":
                    ret = $"{GetLabelKeyword(definitionType, ci).Substring(TokenPrefix.Length)} SomeFpl{definitionType}: {TokenObject}";
                    break;
                case "Function":
                    ret = $"{GetLabelKeyword(definitionType, ci).Substring(TokenPrefix.Length)} SomeFpl{definitionType}() -> {TokenObject}";
                    break;
                case "Predicate":
                default:
                    ret = $"{GetLabelKeyword(definitionType, ci).Substring(TokenPrefix.Length)} SomeFpl{definitionType}()";
                    break;

            }
            return ret + Environment.NewLine;
        }

    }
}
