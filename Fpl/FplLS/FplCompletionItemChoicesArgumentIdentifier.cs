using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesArgumentIdentifier: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            defaultCi.Detail = defaultCi.Word;
            defaultCi.InsertText = "100.";
            defaultCi.Label = TokenPrefix + defaultCi.InsertText;
            defaultCi.Kind = CompletionItemKind.Unit;
            ret.Add(defaultCi);
            return ret;

        }
    }
}
