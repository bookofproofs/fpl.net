using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesArgumentIdentifier : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var insert = "100:";
            var label = TokenPrefix + insert;
            var ci = defaultCi.WithDetail(defaultCi.Word)
                              .WithInsertText(insert)
                              .WithLabel(label)
                              .WithKind(CompletionItemKind.Unit);
            ret.Add(ci);
            return ret;

        }
    }
}
