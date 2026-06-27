using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesFor : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            ret.Add(BuildForSnippet(defaultCi, 0));
            ret.Add(BuildForSnippet(defaultCi, 1));
            ret.Add(BuildForSnippet(defaultCi, 2));

            // keywords -> do not mutate defaultCi
            ret.Add(defaultCi.WithSortText("for03").WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;

        }

        private FplCompletionItem BuildForSnippet(FplCompletionItem baseCi, int subType)
        {
            string firstLine;
            string detail;
            string label;
            string sortText;
            if (subType == 0)
            {
                firstLine = $"for i in Range(){Environment.NewLine}";
                sortText = "for01";
                detail = $"for statement (range)";
                label = $"{TokenPrefix}for ... []";
            }
            else if (subType == 1)
            {
                firstLine = $"for i in someList{Environment.NewLine}";
                sortText = "for02";
                detail = $"for statement (list)";
                label = $"{TokenPrefix}for ... list";
            }
            else
            {
                firstLine = $"for i in SomeFplType{Environment.NewLine}";
                sortText = "for03";
                detail = $"for statement (type)";
                label = $"{TokenPrefix}for ... type";
            }

            if (baseCi.IsShort)
            {
                sortText = "z" + sortText;
            }

            var insert = firstLine +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\tx[i] := 1{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";

            return baseCi.WithSortText(sortText).WithDetail(detail).WithLabel(label).WithInsertText(insert);
        }
    }
}
