using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesWhitespace : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var ci = new FplCompletionItem(" ")
                .WithInsertText(" ")
                .WithLabel(TokenPrefix + "' '")
                .WithKind(CompletionItemKind.Text)
                .WithDetail("(whitespace)")
                .WithSortText("zzzzz"); // make sure whitespace appears at the end of any list.
            ret.Add(ci);
            return ret;

        }
    }
}
