using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesString : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();

            var ci = defaultCi
                .WithDetail(defaultCi.Word)
                .WithSortText("\"\"")
                .WithInsertText("\"...\" ")
                .WithLabel(TokenPrefix + "\"...\"")
                .WithKind(CompletionItemKind.Value);

            ret.Add(ci);

            return ret;

        }
    }
}
