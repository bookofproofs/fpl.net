using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesCases: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetCasesStatement(ci, true); ret.Add(ci);
            // keywords
            ret.Add(defaultCi);
            return ret;

        }

        private void SetCasesStatement(FplCompletionItem ci, bool withRange)
        {
            ci.InsertText =
                $"cases{Environment.NewLine}" +
                $"({Environment.NewLine}" +
                $"\t| p(x) : y := a{Environment.NewLine}" +
                $"\t| q(x) : y := b{Environment.NewLine}" +
                $"\t? y := c{Environment.NewLine}" +
                $"){Environment.NewLine}";
        }
    }
}
