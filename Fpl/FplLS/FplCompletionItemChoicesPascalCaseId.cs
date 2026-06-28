using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesPascalCaseId : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var ci = defaultCi
                .WithDetail("user-defined id")
                .WithInsertText($"{defaultCi.Word} ")
                .WithLabel(TokenPrefix + $"{defaultCi.Word} " + " ...")
                .WithKind(CompletionItemKind.Reference);
            ret.Add(ci);
            return ret;

        }
    }
}
