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
            ci.Detail = defaultCi.Word switch
            {
                "?" => "else case '?'",
                "|" => "new case '|'",
                PrimDelegateEqual => "equal sign '='",
                ":=" => "assignment sign ':='",
                ":*" => "zero or more '*'",
                ":" => "colon ':'",
                "." => "dot '.'",
                "," => "enumeration ','",
                "~" => "type declaration '~'",
                "|-" => "follows logically '|-'",
                "->" => "map '->'",
                "{" => "opening '{'",
                "}" => "closing '}'",
                "(" => "opening '('",
                ")" => "closing '('",
                "[" => "opening '['",
                "]" => "closing ']'",
                _ => "unknown",
            };
            ci.Kind = CompletionItemKind.Text;
            ret.Add(ci);
            return ret;

        }
    }
}
