using FParsec;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesExtension: FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi) 
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone();
            ci.InsertText = $"{ci.Word} Digits x@/\\d+/ -> SomeType";
            ci.InsertText += $"\t{TokenLeftBrace}{Environment.NewLine}";
            ci.InsertText += $"\t\tmcases{Environment.NewLine}";
            ci.InsertText += $"\t\t({Environment.NewLine}";
            ci.InsertText += $"\t\t\t| (x = @0) : Zero{Environment.NewLine}";
            ci.InsertText += $"\t\t\t| (x = @1) : One{Environment.NewLine}";
            ci.InsertText += $"\t\t\t| (x = @2) : Two{Environment.NewLine}";
            ci.InsertText += $"\t\t\t? undef{Environment.NewLine}";
            ci.InsertText += $"\t\t){Environment.NewLine}";
            ci.InsertText += $"\t{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " ...";
            ret.Add(ci);
            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;

        }
    }
}
