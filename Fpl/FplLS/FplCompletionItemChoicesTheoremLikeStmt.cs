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
                ci.Detail = $"{_statementType.ToLower()} (short)";
            }
            ci.Label += " ...";
            ci.InsertText = FplCompletionItemChoicesAxiom.GetBody(ci.Word, _statementType);
        }

    }
}
