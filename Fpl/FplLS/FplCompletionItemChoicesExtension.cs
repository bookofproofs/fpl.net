using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{
    public class FplCompletionItemChoicesExtension : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var insert = $"{defaultCi.Word} Digits x@/\\d+/ -> SomeType";
            insert += $"\t{TokenLeftBrace}{Environment.NewLine}";
            insert += $"\t\tmcases{Environment.NewLine}";
            insert += $"\t\t({Environment.NewLine}";
            insert += $"\t\t\t| (x = @0): Zero{Environment.NewLine}";
            insert += $"\t\t\t| (x = @1): One{Environment.NewLine}";
            insert += $"\t\t\t| (x = @2): Two{Environment.NewLine}";
            insert += $"\t\t\t? undef{Environment.NewLine}";
            insert += $"\t\t){Environment.NewLine}";
            insert += $"\t{TokenRightBrace}{Environment.NewLine}";
            var ci = defaultCi.WithInsertText(insert).WithLabel(defaultCi.Label + " ...");
            ret.Add(ci);
            // keywords (don't mutate default)
            ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;

        }
    }
}
