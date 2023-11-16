using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesFor: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetForStatement(ci, true); ret.Add(ci);
            var ci1 = defaultCi.Clone(); SetForStatement(ci1, false); ret.Add(ci1);
            // keywords
            defaultCi.SortText = "for03";
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        private void SetForStatement(FplCompletionItem ci, bool withRange)
        {
            string firstLine;
            if (withRange)
            {
                firstLine = $"for i in [a,b]{Environment.NewLine}";
                ci.SortText = "for01";
                ci.Detail = $"for statement (range)";
                ci.Label = $"{TokenPrefix}for ... []";
            }
            else
            {
                firstLine = $"for i in someList{Environment.NewLine}";
                ci.SortText = "for02";
                ci.Detail = $"for statement (list)";
                ci.Label = $"{TokenPrefix}for ... list";
            }
            ci.InsertText = firstLine +
                $"({Environment.NewLine}" +
                $"\tx<i> := 1{Environment.NewLine}" +
                $"\ty<i> := 0{Environment.NewLine}" +
                $"){Environment.NewLine}{Environment.NewLine}";
        }
    }
}
