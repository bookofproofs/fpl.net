using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static FplPrimitives;

namespace FplLS
{
    public class FplCompletionItemChoicesDefault : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var ci = defaultCi.Clone();
            switch (defaultCi.Word)
            {
                case "?":
                    ci.Detail = "else case '?'";
                    break;
                case "|":
                    ci.Detail = "new case '|'";
                    break;
                case PrimEquality:
                    ci.Detail = "equal sign '='";
                    break;
                case ":=":
                    ci.Detail = "assignment sign ':='";
                    break;
                case ":+":
                    ci.Detail = "one or more '+'";
                    break;
                case ":*":
                    ci.Detail = "zero or more '*'";
                    break;
                case ":":
                    ci.Detail = "colon ':'";
                    break;
                case ".":
                    ci.Detail = "dot '.'";
                    break;
                case ",":
                    ci.Detail = "enumeration ','";
                    break;
                case "~":
                    ci.Detail = "type declaration '~'";
                    break;
                case "|-":
                    ci.Detail = "follows logically '|-'";
                    break;
                case "->":
                    ci.Detail = "map '->'";
                    break;
                case ";":
                    ci.Detail = "closing ';'";
                    break;
                case "{":
                    ci.Detail = "opening '{'";
                    break;
                case "}":
                    ci.Detail = "closing '}'";
                    break;
                case "(":
                    ci.Detail = "opening '('";
                    break;
                case ")":
                    ci.Detail = "closing '('";
                    break;
                case "[":
                    ci.Detail = "opening '['";
                    break;
                case "]":
                    ci.Detail = "closing ']'";
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
