using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesUses : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi
                .WithInsertText($"uses SomeFplNamespace{Environment.NewLine}")
                .WithDetail("uses namespace")
                .WithSortText("uses01")
                .WithLabel(defaultCi.Label + " ...");
            ret.Add(ci);
            var ci1 = defaultCi
                .WithInsertText($"uses SomeFplNamespace alias Sfn{Environment.NewLine}")
                .WithDetail("uses namespace with alias")
                .WithSortText("uses02")
                .WithLabel(defaultCi.Label + " ... alias");
            ret.Add(ci1);
            var ci2 = defaultCi
                .WithInsertText($"uses SomeFplNamespace *{Environment.NewLine}")
                .WithDetail("uses namespace (all subspaces)")
                .WithSortText("uses02")
                .WithLabel(defaultCi.Label + " (all) ... ");
            ret.Add(ci2);
            // keywords
            var ciKeyword = defaultCi.WithKeyword().WithSortText("uses03");
            ret.Add(ciKeyword);
            return ret;

        }
    }
}
