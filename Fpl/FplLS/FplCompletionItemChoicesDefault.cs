using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesDefault: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            var ci = defaultCi.Clone();
            switch (defaultCi.Word)
            {
                case "{":
                    ci.Detail = "opening '{'";
                    break;
                case "}":
                    ci.Detail = "closing '{'";
                    break;
                case "(":
                    ci.Detail = "opening '('";
                    break;
                case ")":
                    ci.Detail = "closing '('";
                    break;
                case "<":
                    ci.Detail = "coordinates";
                    ci.InsertText = "<1,2>";
                    ci.Label += "1,2>";
                    break;
                case ":ext":
                    ci.Detail = "extension header";
                    break;
                case ":end":
                    ci.Detail = "extension tail";
                    break;
                default:
                    ci.Detail = "unknown";
                    break;
            }

            ci.Kind = CompletionItemKind.Text;
            ret.Add(ci);
            return ret;

        }
    }
}
