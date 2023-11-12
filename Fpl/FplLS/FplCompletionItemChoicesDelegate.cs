using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesDelegate: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            defaultCi.InsertText = defaultCi.Word + ".SomeExternalMethod(x,1)";
            defaultCi.Label = TokenPrefix + defaultCi.InsertText;
            defaultCi.Kind = CompletionItemKind.Reference;
            ret.Add(defaultCi);
            return ret;

        }
    }
}
