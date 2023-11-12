using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesWhitespace: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            var ci = new FplCompletionItem(" ");
            ci.InsertText = " ";
            ci.Label = TokenPrefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Text;
            ci.Detail = "(whitespace)";
            ci.SortText = "zzzz"; // make sure whitespaces appear at the end of any list.
            ret.Add(ci); 
            return ret;

        }
    }
}
