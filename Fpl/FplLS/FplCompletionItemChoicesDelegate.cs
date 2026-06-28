using OmniSharp.Extensions.LanguageServer.Protocol.Models;


namespace FplLS
{
    public class FplCompletionItemChoicesDelegate : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var insert = defaultCi.Word + ".SomeExternalMethod(x,1) ";
            var ci = defaultCi
                .WithInsertText(insert)
                .WithLabel(TokenPrefix + insert)
                .WithKind(CompletionItemKind.Event);
            ret.Add(ci);
            return ret;

        }
    }
}
