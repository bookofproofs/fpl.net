using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{

    public class FplCompletionItemChoicesQuantifier : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var postfix = "";
            if (defaultCi.Word.Contains(LiteralExN))
            {
                postfix = "$1";
            }
            // snippet
            var ci = BuildQuantifierSnippet(defaultCi, postfix);
            ret.Add(ci);
            // keyword
            ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;
        }

        private static FplCompletionItem BuildQuantifierSnippet(FplCompletionItem baseCi, string postfix)
        {
            var sortText = $"{baseCi.Word}02";
            var label = $"{TokenPrefix}{baseCi.Word}{postfix} of type ...";
            var insert = $"{baseCi.Word}{postfix} x:FplType" + GetBody();
            if (baseCi.IsShort)
            {
                sortText = "z" + sortText;
            }
            var detail = baseCi.Word switch
            {
                LiteralAll => $"all quantifier (in type)",
                LiteralEx => $"exists quantifier (in type)",
                LiteralExN => $"exists n-times quantifier (in type)",
                _ => baseCi.Detail
            };
            return baseCi.WithSortText(sortText).WithLabel(label).WithInsertText(insert).WithDetail(detail ?? string.Empty);
        }

        private static string GetBody()
        {
            return " { p(x) } ";
        }
    }
}
