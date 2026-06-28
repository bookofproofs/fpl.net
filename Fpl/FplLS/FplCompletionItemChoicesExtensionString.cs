using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesExtensionString : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();

            var ci = defaultCi
                .WithDetail(defaultCi.Word)
                .WithInsertText("/+\\d/ ")
                .WithLabel(TokenPrefix + "some regex ...")
                .WithKind(CompletionItemKind.Text);
            ret.Add(ci);

            return ret;

        }
    }
}
