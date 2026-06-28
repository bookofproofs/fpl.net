using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesWord : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var insert = "someIdentifier ";
            var ci = defaultCi
                .WithDetail(@"regex \w+")
                .WithInsertText(insert)
                .WithLabel(TokenPrefix + insert)
                .WithKind(CompletionItemKind.Value);
            ret.Add(ci);
            return ret;

        }
    }
}
