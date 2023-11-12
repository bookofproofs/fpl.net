using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesVariable: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            defaultCi.Detail = "variable";
            defaultCi.InsertText = "someVar";
            defaultCi.Label = TokenPrefix + defaultCi.InsertText;
            defaultCi.Kind = CompletionItemKind.Variable;
            ret.Add(defaultCi);
            return ret;

        }
    }
}
