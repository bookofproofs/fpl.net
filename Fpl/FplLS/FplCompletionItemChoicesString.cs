using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesString: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();

            defaultCi.Detail = defaultCi.Word;
            defaultCi.SortText = "\"\"";
            defaultCi.InsertText = "\"...\" ";
            defaultCi.Label = TokenPrefix + "\"...\"";
            defaultCi.Kind = CompletionItemKind.Value;
            ret.Add(defaultCi);

            return ret;

        }
    }
}
