// Ignore Spelling: Fpl

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using static Fpl.Primitives;

namespace FplLS
{

    public class FplCompletionItemChoicesRuleOfInference : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.WithLabel(defaultCi.Label + " ...").WithInsertText(
                $"{defaultCi.Word} SomeFplRuleOfInference(){Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t{TokenPremise}:{Environment.NewLine}" +
                $"\t\ttrue{Environment.NewLine}" +
                $"\t{TokenConclusion}:{Environment.NewLine}" +
                $"\t\ttrue{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}"
            );
            if (defaultCi.IsShort)
            {
                ci = ci.WithDetail($"inference (short)");
            }
            ret.Add(ci);

            // keywords
            ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;
        }

    }
}
