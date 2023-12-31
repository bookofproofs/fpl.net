﻿using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesRuleOfInference : FplCompletionItemChoices
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
                TokenPremise = "pre";
                TokenConclusion = "con";
                ci.Detail = $"inference (short)";
            }
            ci.Label += " ...";
            ci.InsertText =
                $"{ci.Word} SomeFplRuleOfInference(){Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t{TokenPremise}:{Environment.NewLine}" +
                $"\t\ttrue{Environment.NewLine}" +
                $"\t{TokenConclusion}:{Environment.NewLine}" +
                $"\t\ttrue{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
        }


    }
}
