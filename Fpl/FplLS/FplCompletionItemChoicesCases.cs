using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesCases: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetCasesStatement(ci); ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        private static void SetCasesStatement(FplCompletionItem ci)
        {
            ci.Label += " ...";
            ci.InsertText =
                $"cases{Environment.NewLine}" +
                $"({Environment.NewLine}" +
                $"\t| p(x): y := a{Environment.NewLine}" +
                $"\t| q(x): y := b{Environment.NewLine}" +
                $"\t? y := c{Environment.NewLine}" +
                $"){Environment.NewLine}";
        }
    }
}
