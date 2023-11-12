using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesConstructor: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone();
            ci.InsertText = GetConstructorSnippet(ci); ;
            ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        private string GetConstructorSnippet(FplCompletionItem ci)
        {

            ci.Detail = $"variable specification";
            if (ci.IsShort)
            {
                TokenDeclaration = "dec";
                ci.Detail = $"variable specification (short)";
            }
            return
                $"{ci.Word} SomeFplClass(){Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t{TokenDeclaration}{Environment.NewLine}" +
                $"\t\tself!obj(){Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"\tself{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
        }

    }
}
