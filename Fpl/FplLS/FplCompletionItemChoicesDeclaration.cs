using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesDeclaration: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetDeclarationSnippet(ci); ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        private void SetDeclarationSnippet(FplCompletionItem ci)
        {
            ci.Label += " ...";
            ci.InsertText =
                $"{ci.Word}{Environment.NewLine}" +
                $"\t~x: {TokenObject}{Environment.NewLine}" +
                $"\t~y: {TokenObject}{Environment.NewLine}" +
                $"\tx := 0{Environment.NewLine}" +
                $"\ty := 1{Environment.NewLine}" +
                $";{Environment.NewLine}";
        }

    }
}
