using OmniSharp.Extensions.LanguageServer.Protocol.Models;

namespace FplLS
{

    public class FplCompletionItemChoicesProof : FplCompletionItemChoices
    {
        public override List<FplCompletionItem> GetChoices(FplCompletionItem defaultCi)
        {
            var ret = new List<FplCompletionItem>();
            // create each snippet as a new instance (do not mutate defaultCi)
            ret.Add(BuildProof(defaultCi, "01", "direct proof", BuildDirectProofBody));
            ret.Add(BuildProof(defaultCi, "02", "contrapositive proof", BuildContrapositiveProofBody));
            ret.Add(BuildProof(defaultCi, "03", "proof by contradiction (1)", BuildContradictionProofBody));
            ret.Add(BuildProof(defaultCi, "04", "proof by contradiction (2)", BuildContradictionProof2Body));
            ret.Add(BuildProof(defaultCi, "05", "equivalence proof", BuildEquivalenceProofBody));
            ret.Add(BuildProof(defaultCi, "06", "disjunct. premise proof", BuildDisjPremiseProofBody));
            ret.Add(BuildProof(defaultCi, "07", "conjunctive premise proof", BuildConjPremiseProofBody));
            ret.Add(BuildProof(defaultCi, "08", "disjunctive conclusion proof", BuildDisjunctiveConclusionProofBody));
            ret.Add(BuildProof(defaultCi, "09", "conjunctive conclusion proof", BuildConjunctiveConclusionProofBody));
            ret.Add(BuildProof(defaultCi, "10", "counterexample all", BuildCounterexampleAllBody));
            ret.Add(BuildProof(defaultCi, "11", "counterexample ex", BuildCounterexampleExBody));
            ret.Add(BuildProof(defaultCi, "12", "by induction", BuildByInductionBody));
            ret.Add(BuildProof(defaultCi, "13", "by strong induction", BuildByStrongInductionBody));
            ret.Add(BuildProof(defaultCi, "14", "smallest counterexample", BuildSmallestCounterexampleBody));

            // keywords
            ret.Add(defaultCi.WithKind(CompletionItemKind.Keyword).WithKeyword());
            return ret;
        }

        private FplCompletionItem BuildProof(FplCompletionItem baseCi, string suffix, string detailPrefix, Func<FplCompletionItem, string> bodyProvider)
        {
            var newSort = baseCi.SortText + suffix;
            var detail = detailPrefix;
            if (baseCi.IsShort)
            {
                detail += " (short)";
                newSort = "z" + newSort;
            }
            var insert = $"{baseCi.Word} SomeFplTheorem$1{Environment.NewLine}" + bodyProvider(baseCi) + $"{TokenRightBrace}{Environment.NewLine}";
            return baseCi.WithSortText(newSort).WithDetail(detail).WithInsertText(insert).WithLabel(baseCi.Label + " ...");
        }

        private string BuildDirectProofBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Direct Proof{Environment.NewLine}" +
                $"\t// Strategy: Assume that the premise is true, then prove that the conclusion also must be true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100: {TokenAssume} somePremise{Environment.NewLine}" +
                $"\t200: true{Environment.NewLine}" +
                $"\t300: true{Environment.NewLine}" +
                $"\t400: someConclusion{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildContrapositiveProofBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Contrapositive Proof{Environment.NewLine}" +
                $"\t// Strategy: Assume that the conclusion is false, then prove that the premise also must be false.{Environment.NewLine}" +
                $"\t// By an contrapositive argument, the conclusion then follows from the premise.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100: {TokenAssume} not someConclusion{Environment.NewLine}" +
                $"\t200: true{Environment.NewLine}" +
                $"\t300: true{Environment.NewLine}" +
                $"\t400: not somePremise{Environment.NewLine}" +
                $"\t500: impl(not someConclusion, not somePremise){Environment.NewLine}" +
                $"\t600: impl(somePremise, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildContradictionProofBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Contradiction (Type 1){Environment.NewLine}" +
                $"\t// Strategy: Assume that the premise is false, then derive a contradiction.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100: {TokenAssume} not somePremise{Environment.NewLine}" +
                $"\t200: true{Environment.NewLine}" +
                $"\t300: true{Environment.NewLine}" +
                $"\t400: false{Environment.NewLine}" +
                $"\t500: {TokenRevoke} 100{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildContradictionProof2Body(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Contradiction (Type 2){Environment.NewLine}" +
                $"\t// Strategy: Assume that the premise is true and the conclusion is false.{Environment.NewLine}" +
                $"\t// Then derive a contradiction.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100: {TokenAssume} and(somePremise, not someConclusion){Environment.NewLine}" +
                $"\t200: true{Environment.NewLine}" +
                $"\t300: true{Environment.NewLine}" +
                $"\t400: false{Environment.NewLine}" +
                $"\t500: {TokenRevoke} 100{Environment.NewLine}" +
                $"\t600: or (not somePremise, someConclusion){Environment.NewLine}" +
                $"\t700: impl (somePremise, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildEquivalenceProofBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof of Equivalence{Environment.NewLine}" +
                $"\t// Strategy: Show both directions separately.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// `impl (q, p)`{Environment.NewLine}" +
                $"\t100: {TokenAssume} p{Environment.NewLine}" +
                $"\t200: true{Environment.NewLine}" +
                $"\t300: true{Environment.NewLine}" +
                $"\t400: q{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// `impl (q, p)`{Environment.NewLine}" +
                $"\t500: {TokenAssume} q{Environment.NewLine}" +
                $"\t600: true{Environment.NewLine}" +
                $"\t700: true{Environment.NewLine}" +
                $"\t800: p{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t900: iif (p,q){Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildDisjPremiseProofBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof (Premise is a Disjunction: or(p,q) ){Environment.NewLine}" +
                $"\t// Strategy: Given the premise or(p,q), show first impl(p, conclusion), then show impl(q, conclusion).{Environment.NewLine}" +
                $"\t100: {TokenAssume} somePremise{Environment.NewLine}" +
                $"\t150: or (p, q){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t\t// `impl(p, someConclusion)` {Environment.NewLine}" +
                $"\t\t200: {TokenAssume} p{Environment.NewLine}" +
                $"\t\t210: true{Environment.NewLine}" +
                $"\t\t220: someConclusion{Environment.NewLine}" +
                $"\t\t290: impl(p, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t\t// `impl(q, someConclusion)` {Environment.NewLine}" +
                $"\t\t300: {TokenAssume} q{Environment.NewLine}" +
                $"\t\t310: true{Environment.NewLine}" +
                $"\t\t320: someConclusion{Environment.NewLine}" +
                $"\t\t290: impl(q, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t400: impl ( or(p,q), someConclusion ){Environment.NewLine}" +
                $"\t500: impl ( somePremise, someConclusion ){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildConjPremiseProofBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof (Premise is a Conjunction: and(p,q) ){Environment.NewLine}" +
                $"\t// Strategy: Try a direct proof, or consider a contrapositive.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100: {TokenAssume} not someConclusion{Environment.NewLine}" +
                $"\t200: true{Environment.NewLine}" +
                $"\t300: true{Environment.NewLine}" +
                $"\t400: or (not p, not q){Environment.NewLine}" +
                $"\t500: not somePremise{Environment.NewLine}" +
                $"\t600. 100, 500, byinf ProceedingResults |- impl(not someConclusion, not somePremise){Environment.NewLine}" +
                $"\t700: impl(somePremise, someConclusion){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildDisjunctiveConclusionProofBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof (Conclusion is a Disjunction: or(p,q) ){Environment.NewLine}" +
                $"\t// Strategy: Assume the truth of both, the premise and the negation of one of p or q.{Environment.NewLine}" +
                $"\t// Then try to prove that the other one is true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100: {TokenAssume} and(somePremise, not p){Environment.NewLine}" +
                $"\t200: true {Environment.NewLine}" +
                $"\t300: true {Environment.NewLine}" +
                $"\t400: q{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t500: impl(somePremise, or (p,q)){Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildConjunctiveConclusionProofBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof (Conclusion is a Conjunction: and(p,q) ){Environment.NewLine}" +
                $"\t// Strategy: Proof both implications separately.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// `impl (somePremise, p)`{Environment.NewLine}" +
                $"\t100: {TokenAssume} somePremise{Environment.NewLine}" +
                $"\t200: true{Environment.NewLine}" +
                $"\t300: true{Environment.NewLine}" +
                $"\t400: p{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// `impl (somePremise, q)`{Environment.NewLine}" +
                $"\t500: {TokenAssume} somePremise{Environment.NewLine}" +
                $"\t600: true{Environment.NewLine}" +
                $"\t700: true{Environment.NewLine}" +
                $"\t800: q{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t900: impl (somePremise, and(p,q)){Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildCounterexampleAllBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Counterexample of `not all x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Assume `all x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}`, then produce an example of x for which p(x) is false.{Environment.NewLine}" +
                $"\t// Revoking the assumption proves the argument.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tdec {Environment.NewLine}" +
                $"\t\t c:obj{Environment.NewLine}" +
                $"\t\t // construct a counterexample c := ... {Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"\t100: {TokenAssume} not somePremise{Environment.NewLine}" +
                $"\t200: not all x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}{Environment.NewLine}" +
                $"\t300: ex x:tpl {TokenLeftBrace} not p(x) {TokenRightBrace}{Environment.NewLine}" +
                $"\t400. 300, byinf ExistsByExample |- false{Environment.NewLine}" +
                $"\t500: {TokenRevoke} 100{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildCounterexampleExBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Counterexample of `not ex x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Prove that for `all x:tpl {TokenLeftBrace} not p(x) {TokenRightBrace}` is true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t100: {TokenAssume} all x:tpl {TokenLeftBrace} not p(x) {TokenRightBrace}{Environment.NewLine}" +
                $"\t300: true{Environment.NewLine}" +
                $"\t400: true{Environment.NewLine}" +
                $"\t500: true{Environment.NewLine}" +
                $"\t600: not ex x:tpl {TokenLeftBrace} p(x) {TokenRightBrace}{Environment.NewLine}" +
                $"\t700: someConclusion{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tqed{Environment.NewLine}";
        }

        private string BuildByInductionBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Induction (to prove: `all n:N {TokenLeftBrace} p(n) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Prove the \"base case\": p(1){Environment.NewLine}" +
                $"\t// Then do the \"inductive step\": Prove that if `p(n)` is true, then also `p( (n + 1) )` is true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"base case\" {Environment.NewLine}" +
                $"\t100: true{Environment.NewLine}" +
                $"\t200: p(1){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"inductive step\" {Environment.NewLine}" +
                $"\t300. 200 |- {TokenAssume} ex n:N {TokenLeftBrace} all m:N {TokenLeftBrace} impl((m <= n), p(m)) {TokenRightBrace}{TokenRightBrace}{Environment.NewLine}" +
                $"\t400: true{Environment.NewLine}" +
                $"\t500: true{Environment.NewLine}" +
                $"\t600: p( (n + 1) ){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t700: all n:N {TokenLeftBrace} p(n) {TokenRightBrace}{Environment.NewLine}";
        }

        private string BuildByStrongInductionBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Strong Induction (to prove: `all n:N {TokenLeftBrace} p(n) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Prove the \"base cases\": `p(1)`,..., `p(m)`{Environment.NewLine}" +
                $"\t// Then do the \"inductive step\": Prove that if exists n for which `p(m)` is true is for all `m <= n`, then also `p( (n + 1) )` is true.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tdec {Environment.NewLine}" +
                $"\t\t n, m: N{Environment.NewLine}" +
                $"\t\t kFails, p: {TokenPredicate}{Environment.NewLine}" +
                $"\t\t // p(n) := ... // set to your predicate about n here and uncomment the line{Environment.NewLine}" +
                $"\t\t pFailsFor_k := ex k:N {TokenLeftBrace} not p(k) {TokenRightBrace}{Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"\t// \"base cases\" {Environment.NewLine}" +
                $"\t100: true{Environment.NewLine}" +
                $"\t150: p(1){Environment.NewLine}" +
                $"\t200: true{Environment.NewLine}" +
                $"\t250: p(2){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"inductive step\" {Environment.NewLine}" +
                $"\t300. 150, 250, byinf ProceedingResults |- {TokenAssume} ex n:N {TokenLeftBrace} all m:N {TokenLeftBrace} impl((m <= n), p(m)) {TokenRightBrace}{TokenRightBrace}{Environment.NewLine}" +
                $"\t400: true{Environment.NewLine}" +
                $"\t500: true{Environment.NewLine}" +
                $"\t600: p( (n + 1) ){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t700: all n:N {TokenLeftBrace} p(n) {TokenRightBrace}{Environment.NewLine}";
        }

        private string BuildSmallestCounterexampleBody(FplCompletionItem ci)
        {
            return $"{TokenLeftBrace}{Environment.NewLine}" +
                $"\t// Proof by Induction With Smallest CounterExample (to prove: `all n:N {TokenLeftBrace} p(n) {TokenRightBrace}`){Environment.NewLine}" +
                $"\t// Strategy: Prove the \"base case\": `p(1)`{Environment.NewLine}" +
                $"\t// Then assume that it is not true that for `all n:N {TokenLeftBrace} p(n) {TokenRightBrace}`.{Environment.NewLine}" +
                $"\t// Then assume that k is the smallest such k for which `p(k)` fails, in particular, `p( (k - 1) )` is then true.{Environment.NewLine}" +
                $"\t// Then seek the contradiction for the expression `and ( p( (k - 1) ) , not p(k) )`.{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\tdec {Environment.NewLine}" +
                $"\t\t n, k: N{Environment.NewLine}" +
                $"\t\t kFails, p: {TokenPredicate}{Environment.NewLine}" +
                $"\t\t // p(n) := ... // set to your predicate about n here and uncomment the line{Environment.NewLine}" +
                $"\t\t pFailsFor_k := ex k:N {TokenLeftBrace} not p(k) {TokenRightBrace}{Environment.NewLine}" +
                $"\t;{Environment.NewLine}" +
                $"\t// \"base case\" {Environment.NewLine}" +
                $"\t100: true{Environment.NewLine}" +
                $"\t200: p(1){Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t// \"inductive step\" {Environment.NewLine}" +
                $"\t300: {TokenAssume} pFailsFor_k{Environment.NewLine}" +
                $"\t// we make use of the `Smallest` predicate defined in the namespace Fpl.Arithmetics{Environment.NewLine}" +
                $"\t350. WellOrderingPrinciple |- {TokenAssume} Smallest(k, pFailsFor_k){Environment.NewLine}" +
                $"\t400: p( (k - 1) ){Environment.NewLine}" +
                $"\t450: true{Environment.NewLine}" +
                $"\t500: true{Environment.NewLine}" +
                $"\t550: p(k){Environment.NewLine}" +
                $"\t600: false{Environment.NewLine}" +
                $"\t650: {TokenRevoke} 350{Environment.NewLine}" +
                $"\t{Environment.NewLine}" +
                $"\t700: all n:N {TokenLeftBrace} p(n) {TokenRightBrace}{Environment.NewLine}";
        }
    }
}
