using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesDeclaration : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippet
            ret.Add(BuildDeclarationSnippet(defaultCi));
            // keywords - preserve short-marker so keyword sort-texts match expectations
            ret.Add(defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;

        }

        private FplCompletionItem BuildDeclarationSnippet(FplCompletionItem baseCi)
        {
            var label = baseCi.Label + " ...";
            var insert =
                $"{baseCi.Word}{Environment.NewLine}" +
                $"\tx: {TokenObject}{Environment.NewLine}" +
                $"\ty: {TokenObject}{Environment.NewLine}" +
                $"\tx := 0{Environment.NewLine}" +
                $"\ty := 1{Environment.NewLine}" +
                $";{Environment.NewLine}";
            return baseCi.WithLabel(label).WithInsertText(insert);
        }

    }
}
