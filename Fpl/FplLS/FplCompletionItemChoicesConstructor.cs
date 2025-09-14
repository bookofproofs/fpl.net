using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;
namespace FplLS
{
    public class FplCompletionItemChoicesConstructor: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetConstructorSnippet(ci); ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }

        private void SetConstructorSnippet(FplCompletionItem ci)
        {

            if (ci.IsShort)
            {
                TokenDeclaration = literalDec;
            }
            ci.Label += " ...";
            ci.InsertText = 
                $"{ci.Word} SomeFplClass(){Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t{TokenDeclaration}{Environment.NewLine}" +
                $"\t\tbase.obj (){Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"\tself{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}" +
                $"{Environment.NewLine}";
        }

    }
}
