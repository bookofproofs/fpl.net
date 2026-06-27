using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesVariable : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var insert = "someVar ";
            var label = TokenPrefix + insert;
            var ci = defaultCi.WithDetail(PrimVariableL)
                              .WithSortText(PrimVariableL)
                              .WithInsertText(insert)
                              .WithLabel(label)
                              .WithKind(CompletionItemKind.Variable);
            ret.Add(ci);
            return ret;

        }
    }
}
