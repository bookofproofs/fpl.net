using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;
namespace FplLS
{
    public class FplCompletionItemChoicesConstructor : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>
            {
                // snippets
                BuildConstructorSnippet(defaultCi),
                // keywords - preserve short-marker when base is short so final sort-text is correct
                defaultCi.WithIsShort(defaultCi.IsShort).WithKind(CompletionItemKind.Keyword).WithKeyword()
            };
            return ret;

        }

        private FplCompletionItem BuildConstructorSnippet(FplCompletionItem baseCi)
        {
            string insert;
            if (baseCi.IsShort)
            {
                TokenDeclaration = LiteralDec;
            }
            insert =
                $"{baseCi.Word} SomeFplClass(){Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t{TokenDeclaration}{Environment.NewLine}" +
                $"\t\tbase.Obj (){Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}" +
                $"{Environment.NewLine}";
            return baseCi.WithLabel(baseCi.Label + " ...").WithInsertText(insert);
        }

    }
}
