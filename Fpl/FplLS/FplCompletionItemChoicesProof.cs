using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesProof : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // snippets
            var ci = defaultCi.Clone(); SetDirectProof(ci); ret.Add(ci);
            var ci1 = defaultCi.Clone(); SetEquivalenceProof(ci1); ret.Add(ci1);
            var ci2 = defaultCi.Clone(); SetContrapositiveProof(ci2); ret.Add(ci2);
            var ci3 = defaultCi.Clone(); SetContradictionProof(ci3); ret.Add(ci3);
            var ci4 = defaultCi.Clone(); SetDisjunktivePremiseProof(ci4); ret.Add(ci4); 

            // keywords
            defaultCi.Kind = CompletionItemKind.Keyword;
            defaultCi.AdjustToKeyword();
            ret.Add(defaultCi);
            return ret;
        }

        private void SetDirectProof(FplCompletionItem ci)
        {
            ci.SortText += "01";
            ci.Detail = "direct proof";
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                TokenPremise = "pre";
                TokenConclusion = "con";
                ci.Detail += " (short)";
            }
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem!1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// direct proof{Environment.NewLine}" +
                $"\t100. {TokenAssume} {TokenPremise}{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- {TokenConclusion}{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t400. |- qed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}" +
                $"{Environment.NewLine}";
            ci.Label += " (direct) ...";
        }

        private void SetEquivalenceProof(FplCompletionItem ci)
        {
            ci.SortText += "02";
            ci.Detail = "equivalence proof";
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                ci.Detail += " (short)";
            }
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem!1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// \"impl (p, q)\"{Environment.NewLine}" +
                $"\t100. {TokenAssume} p{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- q{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"impl (q, p)\"{Environment.NewLine}" +
                $"\t500. {TokenAssume} q{Environment.NewLine}" +
                $"\t600. |- trivial{Environment.NewLine}" +
                $"\t700. |- trivial{Environment.NewLine}" +
                $"\t800. |- p{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t900. |- qed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}" +
                $"{Environment.NewLine}";
            ci.Label += " (equivalence) ...";
        }

        private void SetContrapositiveProof(FplCompletionItem ci)
        {
            ci.SortText += "03";
            ci.Detail = "contrapositive proof";
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                ci.Detail += " (short)";
                ci.AdjustToShort();
            }
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem!1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// contrapositive proof{Environment.NewLine}" +
                $"\t100. {TokenAssume} not({TokenConclusion}){Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- not({TokenPremise}){Environment.NewLine}" +
                $"\t400. |- impl( not({TokenConclusion}), not({TokenPremise}) ){Environment.NewLine}" +
                $"\t500. |- impl( {TokenPremise}, {TokenConclusion} ){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t600. |- qed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}" +
                $"{Environment.NewLine}";
            ci.Label += " (contrapositive) ...";
        }

        private void SetContradictionProof(FplCompletionItem ci)
        {
            ci.SortText += "04";
            ci.Detail = "proof by contradict.";
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                TokenRevoke = "rev";
                ci.Detail += " (short)";
                ci.AdjustToShort();
            }
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem!1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// proof by contradiction{Environment.NewLine}" +
                $"\t100. {TokenAssume} not({TokenPremise}){Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- false{Environment.NewLine}" +
                $"\t400. {TokenRevoke} 100.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t500. |- qed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}" +
                $"{Environment.NewLine}";
            ci.Label += " (by contradict.) ...";
        }

        private void SetDisjunktivePremiseProof(FplCompletionItem ci)
        {
            ci.SortText += "04";
            ci.Detail = "disjunctive premise proof";
            if (ci.IsShort)
            {
                TokenAssume = "ass";
                ci.Detail += " (short)";
                ci.AdjustToShort();
            }
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem!1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// proof (premise is a disjunction){Environment.NewLine}" +
                $"\t100. {TokenAssume} {TokenPremise}{Environment.NewLine}" +
                $"\t200. |- or (p, q){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t\t// \"impl(p, con)\" {Environment.NewLine}" +
                $"\t\t210. {TokenAssume} p{Environment.NewLine}" +
                $"\t\t220. |- trivial{Environment.NewLine}" +
                $"\t\t230. |- {TokenConclusion}{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t\t// \"impl(q, con)\" {Environment.NewLine}" +
                $"\t\t250. {TokenAssume} q{Environment.NewLine}" +
                $"\t\t260. |- trivial{Environment.NewLine}" +
                $"\t\t270. |- {TokenConclusion}{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t400. |- impl ( {TokenPremise}, {TokenConclusion} ){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t500. |- qed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}" +
                $"{Environment.NewLine}";
            ci.Label += " ('or' premise) ...";
        }


    }
}
