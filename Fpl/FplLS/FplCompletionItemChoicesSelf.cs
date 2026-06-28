using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesSelf : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            if (defaultCi.Word == LiteralSelf)
            {
                var insert = "self ";
                var ci = defaultCi
                    .WithInsertText(insert)
                    .WithLabel(TokenPrefix + insert)
                    .WithDetail("self reference")
                    .WithKind(CompletionItemKind.Reference)
                    .WithSortText("self01");
                ret.Add(ci);
            }
            if (defaultCi.Word == LiteralParent)
            {
                var insert = "parent ";
                var ci = defaultCi
                    .WithInsertText(insert)
                    .WithLabel(TokenPrefix + insert)
                    .WithDetail("parent self reference")
                    .WithKind(CompletionItemKind.Reference)
                    .WithSortText("parent02");
                ret.Add(ci);
            }
            if (defaultCi.Word == LiteralBase)
            {
                var insert = LiteralBase;
                var ci = defaultCi
                    .WithInsertText(insert)
                    .WithLabel(TokenPrefix + insert)
                    .WithDetail("ctor call (parent class)")
                    .WithKind(CompletionItemKind.Reference)
                    .WithSortText("self03");
                ret.Add(ci);
            }
            return ret;
        }
    }
}
