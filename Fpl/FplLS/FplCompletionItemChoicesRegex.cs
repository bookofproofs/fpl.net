using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesRegex: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();

            defaultCi.Detail = defaultCi.Word;
            defaultCi.InsertText = "/+\\d/ ";
            defaultCi.Label = TokenPrefix + "some regex ...";
            defaultCi.Kind = CompletionItemKind.Text;
            ret.Add(defaultCi);

            return ret;

        }
    }
}


