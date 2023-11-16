using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesBound: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();


            var isLeftBound = defaultCi.Word.Contains("left bound");
            var isClosed = defaultCi.Word.Contains("closed");

            var ci = defaultCi.Clone();
            ci.Detail = ci.Word;
            if (isClosed && isLeftBound)
            {
                ci.InsertText = "[";
            }
            else if (!isClosed && isLeftBound)
            {
                ci.InsertText = "[(";
            }
            else if (isClosed && !isLeftBound)
            {
                ci.InsertText = "]";
            }
            else
            {
                ci.InsertText = ")]";
            }
            ci.Label = TokenPrefix + ci.InsertText;
            ci.Kind = CompletionItemKind.Text;
            ret.Add(ci);
            return ret;

        }
    }
}
