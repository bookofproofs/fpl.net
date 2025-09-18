using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLS
{
    public class FplCompletionItemChoicesVariable: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            defaultCi.Detail = PrimVariableL;
            defaultCi.SortText = PrimVariableL;
            defaultCi.InsertText = "someVar ";
            defaultCi.Label = TokenPrefix + defaultCi.InsertText;
            defaultCi.Kind = CompletionItemKind.Variable;
            ret.Add(defaultCi);
            return ret;

        }
    }
}
