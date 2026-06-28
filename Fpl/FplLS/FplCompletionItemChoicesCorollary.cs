using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesCorollary : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippet
            var ci = defaultCi.WithInsertText(FplCompletionItemChoicesAxiom.GetBody(defaultCi.Word, "Theorem$1"))
                              .WithLabel(defaultCi.Label + " ...");
            if (defaultCi.IsShort)
            {
                ci = ci.WithDetail("corollary (short)").WithSortText("z" + defaultCi.SortText);
            }
            ret.Add(ci);

            // keywords (immutable) — preserve short-marker so keyword sort-texts match expectations
            ret.Add(defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;
        }
    }
}
