using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesQuantor : FplCompletionItemChoices
    {

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var postfix = "";
            if (defaultCi.Word.Contains("exn"))
            {
                postfix = "$1";
            }
            // snippets
            var ci = defaultCi.Clone(); SetBody(ci, postfix); ret.Add(ci);
            // keyword
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;
        }


        private void SetBody(FplCompletionItem ci, string postfix)
        {
            ci.SortText = $"{ci.Word}02";
            ci.Label = $"{TokenPrefix}{ci.Word}{postfix} of type ...";
            ci.InsertText = $"{ci.Word}{postfix} x:FplType" + GetBody();
            switch (ci.Word)
            {
                case "all":
                    ci.Detail = $"all quantor (in type)";
                    break;
                case "ex":
                    ci.Detail = $"exists quantor (in type)";
                    break;
                case "exn":
                    ci.Detail = $"exists n-times quantor (in type)";
                    break;
            }
        }
        private string GetBody()
        {
            return " { p(x) } ";
        }
    }
}
