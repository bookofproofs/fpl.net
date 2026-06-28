using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{
    public class FplCompletionItemChoicesDefault : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            var detail = defaultCi.Word switch
            {
                "?" => "else case '?'",
                "|" => "new case '|'",
                PrimDelegateEqual => "equal sign '='",
                ":=" => "assignment sign ':='",
                ":*" => "zero or more '*'",
                ":" => "colon ':'",
                "." => "dot '.'",
                "," => "enumeration ','",
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
            var ci = defaultCi.WithDetail(detail).WithKind(CompletionItemKind.Text);
            ret.Add(ci);
            return ret;

        }
    }
}
