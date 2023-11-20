using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesFor: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetForStatement(ci, 0); ret.Add(ci);
            var ci1 = defaultCi.Clone(); SetForStatement(ci1, 1); ret.Add(ci1);
            var ci2 = defaultCi.Clone(); SetForStatement(ci2, 2); ret.Add(ci2);
            // keywords
            defaultCi.SortText = "for03";
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        private void SetForStatement(FplCompletionItem ci, int subType)
        {
            string firstLine;
            if (subType == 0)
            {
                firstLine = $"for i in [a,b]{Environment.NewLine}";
                ci.SortText = "for01";
                ci.Detail = $"for statement (range)";
                ci.Label = $"{TokenPrefix}for ... []";
            }
            else if (subType == 1)
            {
                firstLine = $"for i in someList{Environment.NewLine}";
                ci.SortText = "for02";
                ci.Detail = $"for statement (list)";
                ci.Label = $"{TokenPrefix}for ... list";
            }
            else 
            {
                firstLine = $"for i in SomeFplType{Environment.NewLine}";
                ci.SortText = "for03";
                ci.Detail = $"for statement (type)";
                ci.Label = $"{TokenPrefix}for ... type";
            }
            ci.InsertText = firstLine +
                $"({Environment.NewLine}" +
                $"\tx<i> := 1{Environment.NewLine}" +
                $"\ty<i> := 0{Environment.NewLine}" +
                $"){Environment.NewLine}";
        }
    }
}
