using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesPascalCaseId: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            defaultCi.Detail = "user-defined id";
            defaultCi.InsertText = $"{defaultCi.Word}() ";
            defaultCi.Label = TokenPrefix + defaultCi.InsertText + " ...";
            defaultCi.Kind = CompletionItemKind.Reference;
            ret.Add(defaultCi);
            return ret;

        }
    }
}
