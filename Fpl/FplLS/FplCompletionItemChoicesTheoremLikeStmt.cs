using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesTheoremLikeStmt : FplCompletionItemChoices
    {
        private string _statementType;
        public FplCompletionItemChoicesTheoremLikeStmt(string statementType)
        {
            _statementType = statementType.Trim();
        }

        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippet
            var ci = BuildBody(defaultCi);
            ret.Add(ci);

            // keywords
            ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;
        }

        private FplCompletionItem BuildBody(FplCompletionItem baseCi)
        {
            var ci = baseCi.WithLabel(baseCi.Label + " ...")
                           .WithInsertText(FplCompletionItemChoicesAxiom.GetBody(baseCi.Word, _statementType));
            if (baseCi.IsShort)
            {
                ci = ci.WithDetail($"{_statementType.ToLower()} (short)");
            }
            return ci;
        }

    }
}
