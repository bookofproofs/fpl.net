using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesSymbolic: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippet
            var ci = defaultCi.Clone();
            ci.InsertText = $"{defaultCi.Label.Substring(2)} {"\"...\""}";
            ret.Add(ci);
            return ret;

        }
    }
}
