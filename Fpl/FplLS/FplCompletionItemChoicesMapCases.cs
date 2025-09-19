using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesMapCases: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetMapCasesStatement(ci, true); ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        private void SetMapCasesStatement(FplCompletionItem ci, bool withRange)
        {
            ci.Label += " ...";
            ci.InsertText =
                $"mcases{Environment.NewLine}" +
                $"({Environment.NewLine}" +
                $"\t| p(x) : a{Environment.NewLine}" +
                $"\t| q(x) : b{Environment.NewLine}" +
                $"\t? c{Environment.NewLine}" +
                $"){Environment.NewLine}";
        }
    }
}
