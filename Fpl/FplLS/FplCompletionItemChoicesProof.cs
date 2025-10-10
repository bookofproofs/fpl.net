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
            var ci3a = defaultCi.Clone(); SetContradictionProof2(ci3a); ret.Add(ci3a);
            var ci4 = defaultCi.Clone(); SetDisjunktivePremiseProof(ci4); ret.Add(ci4);
            var ci4a = defaultCi.Clone(); SetConjunctivePremiseProof(ci4a); ret.Add(ci4a);
            var ci5 = defaultCi.Clone(); SetDisjunktiveConclusionProof(ci5); ret.Add(ci5);
            var ci5a = defaultCi.Clone(); SetConjunctiveConclusionProof(ci5a); ret.Add(ci5a);
            var ci6 = defaultCi.Clone(); SetCounterexampleAllProof(ci6); ret.Add(ci6);
            var ci7 = defaultCi.Clone(); SetCounterexampleExProof(ci7); ret.Add(ci7);
            var ci8 = defaultCi.Clone(); SetByInductionProof(ci8); ret.Add(ci8);
            var ci8a = defaultCi.Clone(); SetByStrongInductionProof(ci8a); ret.Add(ci8a);
            var ci8b = defaultCi.Clone(); SetBySmallestCounterexample(ci8b); ret.Add(ci8b);

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
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Direct Proof{Environment.NewLine}" +
                $"\t// Strategy: Assume that the premise is true, then prove that the conclusion also must be true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} somePremise{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- someConclusion{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
        }

        private void SetContrapositiveProof(FplCompletionItem ci)
        {
            ci.SortText += "02";
            ci.Detail = "contrapositive proof";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Contrapositive Proof{Environment.NewLine}" +
                $"\t// Strategy: Assume that the conclusion is false, then prove that the premise also must be false.{Environment.NewLine}" +
                $"\t// By an contrapositive argument, the conclusion then follows from the premise.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} not someConclusion{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- not somePremise{Environment.NewLine}" +
                $"\t500. |- impl(not someConclusion, not somePremise){Environment.NewLine}" +
                $"\t600. |- impl(somePremise, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (contrapositive) ...";
        }

        private void SetContradictionProof(FplCompletionItem ci)
        {
            ci.SortText += "03";
            ci.Detail = "proof by contradiction (1)";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Contradiction (Type 1){Environment.NewLine}" +
                $"\t// Strategy: Assume that the premise is false, then derive a contradiction.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} not somePremise{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- false{Environment.NewLine}" +
                $"\t500. |- {TokenRevoke} 100{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (by contradict. 1) ...";
        }

        private void SetContradictionProof2(FplCompletionItem ci)
        {
            ci.SortText += "04";
            ci.Detail = "proof by contradiction (2)";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Contradiction (Type 2){Environment.NewLine}" +
                $"\t// Strategy: Assume that the premise is true and the conclusion is false.{Environment.NewLine}" +
                $"\t// Then derive a contradiction.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} and(somePremise, not someConclusion){Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- false{Environment.NewLine}" +
                $"\t500. |- {TokenRevoke} 100{Environment.NewLine}" +
                $"\t600. |- or (not somePremise, someConclusion){Environment.NewLine}" +
                $"\t700. |- impl (somePremise, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (by contradict. 2) ...";
        }

        private void SetEquivalenceProof(FplCompletionItem ci)
        {
            ci.SortText += "05";
            ci.Detail = "equivalence proof";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof of Equivalence{Environment.NewLine}" +
                $"\t// Strategy: Show both directions separately.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// `impl (q, p)`{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} p{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- q{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// `impl (q, p)`{Environment.NewLine}" +
                $"\t500. |- {TokenAssume} q{Environment.NewLine}" +
                $"\t600. |- trivial{Environment.NewLine}" +
                $"\t700. |- trivial{Environment.NewLine}" +
                $"\t800. |- p{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t900. |- iif (p,q){Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (equivalence) ...";
        }

        private void SetDisjunktivePremiseProof(FplCompletionItem ci)
        {
            ci.SortText += "06";
            ci.Detail = "disjunctive premise proof";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof (Premise is a Disjunction: or(p,q) ){Environment.NewLine}" +
                $"\t// Strategy: Given the premise or(p,q), show first impl(p, conclusion), then show impl(q, conclusion).{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} somePremise{Environment.NewLine}" +
                $"\t150. |- or (p, q){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t\t// `impl(p, someConclusion)` {Environment.NewLine}" +
                $"\t\t200. |- {TokenAssume} p{Environment.NewLine}" +
                $"\t\t210. |- trivial{Environment.NewLine}" +
                $"\t\t220. |- someConclusion{Environment.NewLine}" +
                $"\t\t290. |- impl(p, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t\t// `impl(q, someConclusion)` {Environment.NewLine}" +
                $"\t\t300. |- {TokenAssume} q{Environment.NewLine}" +
                $"\t\t310. |- trivial{Environment.NewLine}" +
                $"\t\t320. |- someConclusion{Environment.NewLine}" +
                $"\t\t290. |- impl(q, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t400. |- impl ( or(p,q), someConclusion ){Environment.NewLine}" +
                $"\t500. |- impl ( somePremise, someConclusion ){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " ('or' premise) ...";
        }

        private void SetConjunctivePremiseProof(FplCompletionItem ci)
        {
            ci.SortText += "07";
            ci.Detail = "conjunctive premise proof";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof (Premise is a Conjunction: and(p,q) ){Environment.NewLine}" +
                $"\t// Strategy: Try a direct proof, or consider a contrapositive.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} not someConclusion{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- or (not p, not q){Environment.NewLine}" +
                $"\t500. |- not somePremise{Environment.NewLine}" +
                $"\t600. 100, 500, byinf ProceedingResults |- impl(not someConclusion, not somePremise){Environment.NewLine}" +
                $"\t700. |- impl(somePremise, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " ('and' premise) ...";
        }

        private void SetDisjunktiveConclusionProof(FplCompletionItem ci)
        {
            ci.SortText += "08";
            ci.Detail = "disjunctive conclusion proof";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof (Conclusion is a Disjunction: or(p,q) ){Environment.NewLine}" +
                $"\t// Strategy: Assume the truth of both, the premise and the negation of one of p or q.{Environment.NewLine}" +
                $"\t// Then try to prove that the other one is true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} and(somePremise, not p){Environment.NewLine}" +
                $"\t200. |- trivial {Environment.NewLine}" +
                $"\t300. |- trivial {Environment.NewLine}" +
                $"\t400. |- q{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t500. |- impl(somePremise, or (p,q)){Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " ('or' conclusion) ...";
        }

        private void SetConjunctiveConclusionProof(FplCompletionItem ci)
        {
            ci.SortText += "09";
            ci.Detail = "conjunctive conclusion proof";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof (Conclusion is a Conjunction: and(p,q) ){Environment.NewLine}" +
                $"\t// Strategy: Proof both implications separately.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// `impl (somePremise, p)`{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} somePremise{Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- p{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// `impl (somePremise, q)`{Environment.NewLine}" +
                $"\t500. |- {TokenAssume} somePremise{Environment.NewLine}" +
                $"\t600. |- trivial{Environment.NewLine}" +
                $"\t700. |- trivial{Environment.NewLine}" +
                $"\t800. |- q{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t900. |- impl (somePremise, and(p,q)){Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " ('and' conclusion) ...";
        }

        private void SetCounterexampleAllProof(FplCompletionItem ci)
        {
            ci.SortText += "10";
            ci.Detail = "proof by counterexample (all quantor)";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Counterexample of `not all x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Assume `all x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}`, then produce an example of x for which p(x) is false.{Environment.NewLine}" +
                $"\t// Revoking the assumption proves the argument.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tdec {Environment.NewLine}" +
                $"\t\t ~c:Obj{Environment.NewLine}" +
                $"\t\t // construct a counterexample c := ... {Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} not somePremise{Environment.NewLine}" +
                $"\t200. |- not all x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}{Environment.NewLine}" +
                $"\t300. |- ex x:tpl {TokenLeftBrace} not p(x) {TokenRightBrace}{Environment.NewLine}" +
                $"\t400. 300, byinf ExistsByExample |- false{Environment.NewLine}" +
                $"\t500. |- {TokenRevoke} 100{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (by counterex. all) ...";
        }

        private void SetCounterexampleExProof(FplCompletionItem ci)
        {
            ci.SortText += "11";
            ci.Detail = "proof by counterexample (exists quantor)";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Counterexample of `not ex x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Prove that for `all x:tpl {TokenLeftBrace} not p(x) {TokenRightBrace}` is true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100. |- {TokenAssume} all x:tpl {TokenLeftBrace} not p(x) {TokenRightBrace}{Environment.NewLine}" +
                $"\t300. |- trivial{Environment.NewLine}" +
                $"\t400. |- trivial{Environment.NewLine}" +
                $"\t500. |- true{Environment.NewLine}" +
                $"\t600. |- not ex x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}{Environment.NewLine}" +
                $"\t700. |- someConclusion{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (by counterex. ex) ...";
        }

        private void SetByInductionProof(FplCompletionItem ci)
        {
            ci.SortText += "12";
            ci.Detail = "proof by induction";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Induction (to prove: `all n:N {TokenLeftBrace} p(n) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Prove the \"base case\": p(1){Environment.NewLine}" +
                $"\t// Then do the \"inductive step\": Prove that if `p(n)` is true, then also `p( (n + 1) )` is true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"base case\" {Environment.NewLine}" +
                $"\t100. |- trivial{Environment.NewLine}" +
                $"\t200. |- p(1){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"inductive step\" {Environment.NewLine}" +
                $"\t300. 200 |- {TokenAssume} ex n:N {TokenLeftBrace} p(n) {TokenRightBrace}{Environment.NewLine}" +
                $"\t400. |- trivial{Environment.NewLine}" +
                $"\t500. |- trivial{Environment.NewLine}" +
                $"\t600. |- p( (n + 1) ){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t700. |- all n:N {TokenLeftBrace} p(n) {TokenRightBrace}{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (by induction) ...";
        }

        private void SetByStrongInductionProof(FplCompletionItem ci)
        {
            ci.SortText += "13";
            ci.Detail = "proof by strong induction";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Strong Induction (to prove: `all n:N {TokenLeftBrace} p(n) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Prove the \"base cases\": `p(1)`,..., `p(m)`{Environment.NewLine}" +
                $"\t// Then do the \"inductive step\": Prove that if exists n for which `p(m)` is true is for all `m <= n`, then also `p( (n + 1) )` is true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tdec {Environment.NewLine}" +
                $"\t\t ~n, m : N{Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"\t// \"base cases\" {Environment.NewLine}" +
                $"\t100. |- trivial{Environment.NewLine}" +
                $"\t150. |- p(1){Environment.NewLine}" +
                $"\t200. |- trivial{Environment.NewLine}" +
                $"\t250. |- p(2){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"inductive step\" {Environment.NewLine}" +
                $"\t300. 150, 250, byinf ProceedingResults |- {TokenAssume} ex n:N {TokenLeftBrace} all m:N {TokenLeftBrace} impl((m <= n), p(m)) {TokenRightBrace}{TokenRightBrace}{Environment.NewLine}" +
                $"\t400. |- trivial{Environment.NewLine}" +
                $"\t500. |- trivial{Environment.NewLine}" +
                $"\t600. |- p( (n + 1) ){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t700. |- all n:N {TokenLeftBrace} p(n) {TokenRightBrace}{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (by strong induction) ...";
        }

        private void SetBySmallestCounterexample(FplCompletionItem ci)
        {
            ci.SortText += "14";
            ci.Detail = "proof by smallest counterexample";
            AdjShort(ci);
            ci.InsertText =
                $"{ci.Word} SomeFplTheorem$1{Environment.NewLine}" +
                $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Induction With Smallest CounterExample (to prove: `all n:N {TokenLeftBrace} p(n) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Prove the \"base case\": `p(1)`{Environment.NewLine}" +
                $"\t// Then assume that it is not true that for `all n:N {TokenLeftBrace} p(n) {TokenRightBrace}`.{Environment.NewLine}" +
                $"\t// Then assume that k is the smallest such k for which `p(k)` fails, in particular, `p( (k - 1) )` is then true.{Environment.NewLine}" +
                $"\t// Then seek the contradiction for the expression `and ( p( (k - 1) ) , not p(k) )`.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tdec {Environment.NewLine}" +
                $"\t\t ~n, k : N{Environment.NewLine}" +
                $"\t\t ~kFails, p : {TokenPredicate}{Environment.NewLine}" +
                $"\t\t // p(n) := ... // set to your predicate about n here and uncomment the line{Environment.NewLine}" +
                $"\t\t pFailsFor_k := ex k:N {TokenLeftBrace} not p(k) {TokenRightBrace}{Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"\t// \"base case\" {Environment.NewLine}" +
                $"\t100. |- trivial{Environment.NewLine}" +
                $"\t200. |- p(1){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"inductive step\" {Environment.NewLine}" +
                $"\t300. |- {TokenAssume} pFailsFor_k{Environment.NewLine}" +
                $"\t// we make use of the `Smallest` predicate defined in the namespace Fpl.Arithmetics{Environment.NewLine}" +
                $"\t350. WellOrderingPrinciple |- {TokenAssume} Smallest(k, pFailsFor_k){Environment.NewLine}" +
                $"\t400. |- p( (k - 1) ){Environment.NewLine}" +
                $"\t450. |- trivial{Environment.NewLine}" +
                $"\t500. |- trivial{Environment.NewLine}" +
                $"\t550. |- p(k){Environment.NewLine}" +
                $"\t600. |- false{Environment.NewLine}" +
                $"\t650. |- {TokenRevoke} 350{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t700. |- all n:N {TokenLeftBrace} p(n) {TokenRightBrace}{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}" +
                $"{TokenRightBrace}{Environment.NewLine}";
            ci.Label += " (smallest counterex.) ...";
        }

        private void AdjShort(FplCompletionItem ci)
        {
            if (ci.IsShort)
            {
                ci.Detail += " (short)";
                AdjustToShort();
                ci.AdjustToShort();
            }
        }
    }
}
