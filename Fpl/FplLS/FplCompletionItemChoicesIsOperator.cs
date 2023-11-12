using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesIsOperator: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snipped
            var ci = defaultCi.Clone();
            ci.InsertText = "is (x, SomeFplType)";
            ci.Label = TokenPrefix + defaultCi.InsertText;
            ret.Add(ci);

            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            ret.Add(defaultCi);
            return ret;
        }

    }
}
