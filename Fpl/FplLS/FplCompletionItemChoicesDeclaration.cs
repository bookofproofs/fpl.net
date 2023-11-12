using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesDeclaration: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone();
            ci.InsertText = GetDeclarationSnippet(ci); ;
            ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        private string GetDeclarationSnippet(FplCompletionItem ci)
        {

            return
                $"{ci.Word}{Environment.NewLine}" +
                $"\t~x: {TokenObject}{Environment.NewLine}" +
                $"\t~y: {TokenObject}{Environment.NewLine}" +
                $"\tx := 0{Environment.NewLine}" +
                $"\ty := 1{Environment.NewLine}" +
                $";{Environment.NewLine}";
        }

    }
}
