using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesPredicate : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets / keywords (create new instances instead of mutating)
            switch (defaultCi.Word)
            {
                case LiteralTrue:
                case LiteralFalse:
                case LiteralUndef:
                case LiteralUndefL:
                    // keyword
                    ret.Add(defaultCi.WithKeyword());
                    break;
                case LiteralNot:
                    // snippet
                    ret.Add(CreateWithBody(defaultCi, 1));
                    // keyword
                    ret.Add(defaultCi.WithKeyword());
                    break;
                case LiteralIif:
                case LiteralImpl:
                    // snippet
                    ret.Add(CreateWithBody(defaultCi, 2));
                    // keyword
                    ret.Add(defaultCi.WithKeyword());
                    break;
                case "(":
                    // snippet for equality
                    ret.Add(CreateEquality(defaultCi));
                    break;
                case LiteralAnd:
                case LiteralOr:
                case LiteralXor:
                    ret.Add(CreateWithBody(defaultCi, 3));
                    ret.Add(defaultCi.WithKeyword());
                    break;
            }
            return ret;
        }

        private static FplCompletionItem CreateWithBody(FplCompletionItem baseCi, int numbOfArgs)
        {
            string insert = numbOfArgs switch
            {
                0 => baseCi.Word + " ",
                1 => $"{baseCi.Word} true ",
                2 => $"{baseCi.Word} ( false, true ) ",
                _ => $"{baseCi.Word} ( true, false ) ",
            };
            return baseCi
                .WithInsertText(insert)
                .WithLabel(baseCi.Label + " ...");
        }

        private static FplCompletionItem CreateEquality(FplCompletionItem baseCi)
        {
            var insert = $" x = y ";
            return baseCi
                .WithInsertText(insert)
                .WithLabel(TokenPrefix + insert + "...")
                .WithDetail(PrimDelegateEqual)
                .WithSortText("(")
                .WithKind(CompletionItemKind.Operator);
        }

    }
}
