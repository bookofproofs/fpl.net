using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesDigits : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();

            FplCompletionItem ci;
            if (defaultCi.Word.Contains("dollar"))
            {
                ci = defaultCi
                    .WithDetail("$digits")
                    .WithInsertText("$123")
                    .WithSortText("$123")
                    .WithLabel(TokenPrefix + "$123")
                    .WithKind(CompletionItemKind.Text);
            }
            else
            {
                ci = defaultCi
                    .WithDetail("digits")
                    .WithInsertText("123")
                    .WithSortText("123")
                    .WithLabel(TokenPrefix + "123")
                    .WithKind(CompletionItemKind.Text);
            }

            ret.Add(ci);
            return ret;

        }
    }
}
