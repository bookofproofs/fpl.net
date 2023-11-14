using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesWord: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            defaultCi.Detail = "regex \\w+";
            defaultCi.InsertText = "someIdentifier ";
            defaultCi.Label = TokenPrefix + defaultCi.InsertText;
            defaultCi.Kind = CompletionItemKind.Value;
            ret.Add(defaultCi);
            return ret;

        }
    }
}
