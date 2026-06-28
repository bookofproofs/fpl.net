using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesMapCases : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var insert =
                $"mcases{Environment.NewLine}" +
                $"({Environment.NewLine}" +
                $"\t| p(x): a{Environment.NewLine}" +
                $"\t| q(x): b{Environment.NewLine}" +
                $"\t? c{Environment.NewLine}" +
                $"){Environment.NewLine}";
            var ci = defaultCi
                .WithInsertText(insert)
                .WithLabel(defaultCi.Label + " ...");
            ret.Add(ci);
            // keywords
            var keyword = defaultCi.WithKeyword();
            ret.Add(keyword);
            return ret;

        }
    }
}
