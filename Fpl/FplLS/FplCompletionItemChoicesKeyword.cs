using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesKeyword : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // keywords: create a new instance (do not mutate defaultCi)
            ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;

        }
    }
}
