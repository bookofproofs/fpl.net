using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesCorollary : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetBody(ci); ret.Add(ci);

            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;
        }

        private void SetBody(FplCompletionItem ci)
        {
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                ci.Detail = "corollary (short)";
                ci.SortText = "z" + ci.SortText;
            }
            ci.InsertText = FplCompletionItemChoicesAxiom.GetBody(ci.Word, "CorollaryOfXxx!$1");
            ci.Label += " ...";
        }

    }
}
