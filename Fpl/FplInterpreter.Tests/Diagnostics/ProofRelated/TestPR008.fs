namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

(* PR008
   Purpose: Report that a `byinf` justification does not match the expected premise structure of the referenced rule of inference.
   What it indicates: The subsequent step expects a specific number/shape of premises, but the provided justification items (or their inferred expressions) do not match that structure. The diagnostic text enumerates the candidate expressions that were tried.
   Use: Pinpoint failing `byinf` justification steps and show which candidate expressions were attempted so the author can see why matching failed (wrong arity, wrong connective/structure, mismatched quantifier shape, or incompatible subformulas).
   Action / Treat: Adjust the proceeding justification items so they produce expressions matching the rule's premises (or choose a different rule), or correct the rule's expected premise signature. PR008 is an error blocking the inference step and must be fixed for the `byinf` justification to succeed. *)

[<TestClass>]
type TestPR008() =

    // ModusPonens and (p, impl (p,q) )
    [<DataRow("MP_01", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(true, impl(true, false)) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01a", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: or(true, impl(true, false)) 2. 1, byinf M |- false }""", 1)>]
    [<DataRow("MP_01b", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(true, xor(true, false)) 2. 1, byinf M |- false }""", 1)>]
    [<DataRow("MP_01c", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(is(x,K), impl(ex x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""", 1)>]
    [<DataRow("MP_01d", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(ex x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01e", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, impl(all x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01f", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(exn$1 x:obj {is(x,N)}, impl(exn$1 x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01g", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(and(exn$1 x:obj {is(x,N)}, true), impl(and(exn$1 x:obj {is(x,N)}, true), false)) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01h", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(not (ex x:obj {is(x,N)}), impl(not (ex x:obj {is(x,N)}), false)) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01i", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(is(x,N), false)) 2. 1, byinf M |- false }""", 1)>]
    [<DataRow("MP_01j", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(ex x:obj {is(x,N)}, xor(true,false))) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01k", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(iif(true, ex x:obj {is(x,N)}), impl(iif(true, ex x:obj {is(x,N)}), false)) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01l", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(ex x:obj {is(x,N)}, impl(all x:obj {is(x,N)}, false)) 2. 1, byinf M |- false }""", 1)>]
    [<DataRow("MP_01m", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(is(A,N), impl(is(A,N), false)) 2. 1, byinf M |- false }""", 0)>]
    [<DataRow("MP_01n", """inf M { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } thm T {true} proof T$1 {1: and(is(A,N), impl(is(N,A), false)) 2. 1, byinf M |- false }""", 1)>]

    // AndCummutative and(p,q) 
    [<DataRow("AndC_01", """inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- and(false,true) }""", 0)>]
    [<DataRow("AndC_02", """inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: or(true,false) 2. 1, byinf AndCummutative |- and(false,true) }""", 1)>]
    [<DataRow("AndC_03", """inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, not(all x:obj {is(x,N)})) 2. 1, byinf AndCummutative |- and(not(all x:obj {is(x,N)}), all x:obj {is(x,N)}) }""", 0)>]

    // OrCummutative or(p,q) 
    [<DataRow("OrC_01", """inf OrCummutative{dec p,q:pred; pre:or(p,q) con:or(q,p)} thm T {true} proof T$1 {1: or(true,false) 2. 1, byinf OrCummutative |- or(false,true) }""", 0)>]
    [<DataRow("OrC_02", """inf OrCummutative{dec p,q:pred; pre:or(p,q) con:or(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf OrCummutative |- or(false,true) }""", 1)>]
    [<DataRow("OrC_03", """inf OrCummutative{dec p,q:pred; pre:or(p,q) con:or(q,p)} thm T {true} proof T$1 {1: or(ex x:obj {is(x,N)}, iif(true,false)) 2. 1, byinf OrCummutative |- or(iif(true,false), ex x:obj {is(x,N)}) }""", 0)>]

    // XorCummutative xor(p,q) 
    [<DataRow("XorC_01", """inf XorCummutative{dec p,q:pred; pre:xor(p,q) con:xor(q,p)} thm T {true} proof T$1 {1: xor(true,false) 2. 1, byinf XorCummutative |- xor(false,true) }""", 0)>]
    [<DataRow("XorC_02", """inf XorCummutative{dec p,q:pred; pre:xor(p,q) con:xor(q,p)} thm T {true} proof T$1 {1: xor(true,true) 2. 1, byinf XorCummutative |- xor(true,true) }""", 0)>]
    [<DataRow("XorC_02a", """inf XorX{dec q,s:pred; pre:xor(q,s) con:true} thm T {true} proof T$1 {1: true 2. 1, byinf XorX |- false }""", 1)>]
    [<DataRow("XorC_03", """inf XorCummutative{dec p,q:pred; pre:xor(p,q) con:xor(q,p)} thm T {true} proof T$1 {1: xor(and(ex x:obj {is(x,N)}, true), impl(true,false)) 2. 1, byinf XorCummutative |- xor(impl(true,false), and(ex x:obj {is(x,N)}, true)) }""", 0)>]

    // IifCummutative iif(p,q)
    [<DataRow("IifC_01", """inf IifCummutative{dec p,q:pred; pre:iif(p,q) con:iif(q,p)} thm T {true} proof T$1 {1: iif(true,false) 2. 1, byinf IifCummutative |- iif(false,true) }""", 0)>]
    [<DataRow("IifC_02", """inf IifCummutative{dec p,q:pred; pre:iif(p,q) con:iif(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf IifCummutative |- iif(false,true) }""", 1)>]
    [<DataRow("IifC_03", """inf IifCummutative{dec p,q:pred; pre:iif(p,q) con:iif(q,p)} thm T {true} proof T$1 {1: iif(iif(true,ex x:obj {is(x,N)}), xor(true,false)) 2. 1, byinf IifCummutative |- iif(xor(true,false), iif(true,ex x:obj {is(x,N)})) }""", 0)>]

    // AndAssociative and(p,and(q,s)) 
    [<DataRow("AndA_01", """inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true, and(false, true)) 2. 1, byinf AndAssociative |- and(and(true,false), true) }""", 0)>]
    [<DataRow("AndA_02", """inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(and(true,false), true) 2. 1, byinf AndAssociative |- and(and(true,false), true) }""", 1)>]
    [<DataRow("AndA_02a", """inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true,and(true,false)) 2. 1, byinf AndAssociative |- and(and(true,false), true) }""", 0)>]
    [<DataRow("AndA_03", """inf AndAssociative{dec p,q,s:pred; pre:and(p,and(q,s)) con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, and(ex y:obj {is(y,M)}, true)) 2. 1, byinf AndAssociative |- and(and(all x:obj {is(x,N)}, ex y:obj {is(y,M)}), true) }""", 0)>]

    // OrAssociative or(p,or(q,s))
    [<DataRow("OrA_01", """inf OrAssociative{dec p,q,s:pred; pre:or(p,or(q,s)) con:or(or(p,q),s)} thm T {true} proof T$1 {1: or(true, or(false, true)) 2. 1, byinf OrAssociative |- or(or(true,false), true) }""", 0)>]
    [<DataRow("OrA_02", """inf OrAssociative{dec p,q,s:pred; pre:or(p,or(q,s)) con:or(or(p,q),s)} thm T {true} proof T$1 {1: or(or(true,false), true) 2. 1, byinf OrAssociative |- or(or(true,false), true) }""", 1)>]
    [<DataRow("OrA_03", """inf OrAssociative{dec p,q,s:pred; pre:or(p,or(q,s)) con:or(or(p,q),s)} thm T {true} proof T$1 {1: or(ex x:obj {is(x,N)}, or(iif(true,false), true)) 2. 1, byinf OrAssociative |- or(or(ex x:obj {is(x,N)}, iif(true,false)), true) }""", 0)>]

    // XorAssociative xor(p,xor(q,s))
    [<DataRow("XorA_01", """inf XorAssociative{dec p,q,s:pred; pre:xor(p,xor(q,s)) con:xor(xor(p,q),s)} thm T {true} proof T$1 {1: xor(true, xor(false, true)) 2. 1, byinf XorAssociative |- xor(xor(true,false), true) }""", 0)>]
    [<DataRow("XorA_02", """inf XorAssociative{dec p,q,s:pred; pre:xor(p,xor(q,s)) con:xor(xor(p,q),s)} thm T {true} proof T$1 {1: xor(xor(true,false), true) 2. 1, byinf XorAssociative |- xor(xor(true,false), true) }""", 1)>]
    [<DataRow("XorA_03", """inf XorAssociative{dec p,q,s:pred; pre:xor(p,xor(q,s)) con:xor(xor(p,q),s)} thm T {true} proof T$1 {1: xor(iif(true,ex x:obj {is(x,N)}), xor(xor(true,false), true)) 2. 1, byinf XorAssociative |- xor(xor(iif(true,ex x:obj {is(x,N)}), xor(true,false)), true) }""", 0)>]

    // IifAssociative iif(p,iif(q,s))
    [<DataRow("IifA_01", """inf IifAssociative{dec p,q,s:pred; pre:iif(p,iif(q,s)) con:iif(iif(p,q),s)} thm T {true} proof T$1 {1: iif(true, iif(false, true)) 2. 1, byinf IifAssociative |- iif(iif(true,false), true) }""", 0)>]
    [<DataRow("IifA_02", """inf IifAssociative{dec p,q,s:pred; pre:iif(p,iif(q,s)) con:iif(iif(p,q),s)} thm T {true} proof T$1 {1: iif(iif(true,false), true) 2. 1, byinf IifAssociative |- iif(iif(true,false), true) }""", 1)>]
    [<DataRow("IifA_03", """inf IifAssociative{dec p,q,s:pred; pre:iif(p,iif(q,s)) con:iif(iif(p,q),s)} thm T {true} proof T$1 {1: iif(ex x:obj {is(x,N)}, iif(iif(true,false), ex y:obj {is(y,M)})) 2. 1, byinf IifAssociative |- iif(iif(ex x:obj {is(x,N)}, iif(true,false)), ex y:obj {is(y,M)}) }""", 0)>]

    // FalseAndAbsorbing and(false,p)
    [<DataRow("FAbs_01", """inf FalseAndAbsorbing{dec p:pred; pre:and(false,p) con:false} thm T {true} proof T$1 {1: and(false,true) 2. 1, byinf FalseAndAbsorbing |- false }""", 0)>]
    [<DataRow("FAbs_02", """inf FalseAndAbsorbing{dec p:pred; pre:and(false,p) con:false} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf FalseAndAbsorbing |- false }""", 1)>]
    [<DataRow("FAbs_03", """inf FalseAndAbsorbing{dec p:pred; pre:and(false,p) con:false} thm T {true} proof T$1 {1: and(false, ex x:obj {is(x,N)}) 2. 1, byinf FalseAndAbsorbing |- false }""", 0)>]

    // TrueOrAbsorbing or(true,p)
    [<DataRow("TOrAbs_01", """inf TrueOrAbsorbing{dec p:pred; pre:or(true,p) con:true} thm T {true} proof T$1 {1: or(true,false) 2. 1, byinf TrueOrAbsorbing |- true }""", 0)>]
    [<DataRow("TOrAbs_02", """inf TrueOrAbsorbing{dec p:pred; pre:or(true,p) con:true} thm T {true} proof T$1 {1: or(false,true) 2. 1, byinf TrueOrAbsorbing |- true }""", 1)>]
    [<DataRow("TOrAbs_03", """inf TrueOrAbsorbing{dec p:pred; pre:or(true,p) con:true} thm T {true} proof T$1 {1: or(true, ex x:obj {is(x,N)}) 2. 1, byinf TrueOrAbsorbing |- true }""", 0)>]

    // OrAndAbsorbing: pre: or(p, and(p, q))
    [<DataRow("OrAndAbsorbing_01", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(and(true, not(false)), and(and(true, not(false)), xor(false, true))) 2. 1, byinf OrAndAbsorbing |- true }", 0)>]
    [<DataRow("OrAndAbsorbing_02", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(iif(true, not(false)), and(iif(true, not(false)), ex n:obj { is(n, N) })) 2. 1, byinf OrAndAbsorbing |- true }", 0)>]
    [<DataRow("OrAndAbsorbing_03", "inf OrAndAbsorbing{dec p,q:pred; pre:or(p,and(p,q)) con:p} thm T {true} proof T$1 {1: or(all x:obj { is(x, N) }, and(all x:obj { is(x, N) }, impl(true, false))) 2. 1, byinf OrAndAbsorbing |- true }", 0)>]

    // AndOrAbsorbing: pre: and(p, or(p, q))
    [<DataRow("AndOrAbsorbing_01", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(xor(true, false), or(xor(true, false), not(false))) 2. 1, byinf AndOrAbsorbing |- true }", 0)>]
    [<DataRow("AndOrAbsorbing_02", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(not(and(true, false)), or(not(and(true, false)), all y:obj { is(y, N) })) 2. 1, byinf AndOrAbsorbing |- true }", 0)>]
    [<DataRow("AndOrAbsorbing_03", "inf AndOrAbsorbing{dec p,q:pred; pre:and(p,or(p,q)) con:p} thm T {true} proof T$1 {1: and(ex n:obj { is(n, N) }, or(ex n:obj { is(n, N) }, iif(true, false))) 2. 1, byinf AndOrAbsorbing |- true }", 0)>]

    // AndTrueNeutral: pre: and(true, p)
    [<DataRow("AndTrueNeutral_01", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, and(xor(true, false), not(false))) 2. 1, byinf AndTrueNeutral |- true }", 0)>]
    [<DataRow("AndTrueNeutral_02", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, ex n:obj { is(n, N) }) 2. 1, byinf AndTrueNeutral |- true }", 0)>]
    [<DataRow("AndTrueNeutral_03", "inf AndTrueNeutral{dec p:pred; pre:and(true,p) con:p} thm T {true} proof T$1 {1: and(true, iif(or(true, false), not(false))) 2. 1, byinf AndTrueNeutral |- true }", 0)>]

    // OrFalseNeutral: pre: or(false, p)
    [<DataRow("OrFalseNeutral_01", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, and(true, not(false))) 2. 1, byinf OrFalseNeutral |- true }", 0)>]
    [<DataRow("OrFalseNeutral_02", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, ex n:obj { is(n, N) }) 2. 1, byinf OrFalseNeutral |- true }", 0)>]
    [<DataRow("OrFalseNeutral_03", "inf OrFalseNeutral{dec p:pred; pre:or(false,p) con:p} thm T {true} proof T$1 {1: or(false, iif(true, xor(false, true))) 2. 1, byinf OrFalseNeutral |- true }", 0)>]

    // AndInversion: pre: and(p, not p)
    [<DataRow("AndInversion_01", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(or(true, false), not(or(true, false))) 2. 1, byinf AndInversion |- true }", 0)>]
    [<DataRow("AndInversion_02", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, not(all x:obj { is(x, N) })) 2. 1, byinf AndInversion |- true }", 0)>]
    [<DataRow("AndInversion_03", "inf AndInversion{dec p:pred; pre:and(p,not p) con:false} thm T {true} proof T$1 {1: and(ex n:obj { is(n, N) }, not(ex n:obj { is(n, N) })) 2. 1, byinf AndInversion |- true }", 0)>]

    // OrInversion: pre: or(p, not p)
    [<DataRow("OrInversion_01", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(impl(true, false), not(impl(true, false))) 2. 1, byinf OrInversion |- true }", 0)>]
    [<DataRow("OrInversion_02", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(xor(true, false), not(xor(true, false))) 2. 1, byinf OrInversion |- true }", 0)>]
    [<DataRow("OrInversion_03", "inf OrInversion{dec p:pred; pre:or(p,not p) con:true} thm T {true} proof T$1 {1: or(all x:obj { is(x, N) }, not(all x:obj { is(x, N) })) 2. 1, byinf OrInversion |- true }", 0)>]

    // AndIdempotence: pre: and(p, p)
    [<DataRow("AndIdempotence_01", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(or(true, false), or(true, false)) 2. 1, byinf AndIdempotence |- true }", 0)>]
    [<DataRow("AndIdempotence_02", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, all x:obj { is(x, N) }) 2. 1, byinf AndIdempotence |- true }", 0)>]
    [<DataRow("AndIdempotence_03", "inf AndIdempotence{dec p:pred; pre:and(p,p) con:p} thm T {true} proof T$1 {1: and(iif(true, false), iif(true, false)) 2. 1, byinf AndIdempotence |- true }", 0)>]

    // OrIdempotence: pre: or(p, p)
    [<DataRow("OrIdempotence_01", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(and(true, false), and(true, false)) 2. 1, byinf OrIdempotence |- true }", 0)>]
    [<DataRow("OrIdempotence_02", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(ex n:obj { is(n, N) }, ex n:obj { is(n, N) }) 2. 1, byinf OrIdempotence |- true }", 0)>]
    [<DataRow("OrIdempotence_03", "inf OrIdempotence{dec p:pred; pre:or(p,p) con:p} thm T {true} proof T$1 {1: or(xor(true, false), xor(true, false)) 2. 1, byinf OrIdempotence |- true }", 0)>]

    // OrAndDistributiveUnpack: pre: or(p, and(q, s))
    [<DataRow("OrAndDistributiveUnpack_01", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(iif(true, false), and(and(true, not(false)), xor(false, true))) 2. 1, byinf OrAndDistributiveUnpack |- true }", 0)>]
    [<DataRow("OrAndDistributiveUnpack_02", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(all x:obj { is(x, N) }, and(ex y:obj { is(y, M) }, or(true, false))) 2. 1, byinf OrAndDistributiveUnpack |- true }", 0)>]
    [<DataRow("OrAndDistributiveUnpack_03", "inf OrAndDistributiveUnpack{dec p,q,s:pred; pre:or(p,and(q,s)) con:and(or(p,q),or(p,s))} thm T {true} proof T$1 {1: or(not(xor(true, false)), and(iif(true, true), all z:obj { is(z, N) })) 2. 1, byinf OrAndDistributiveUnpack |- true }", 0)>]

    // AndOrDistributivePack: pre: and(or(p, q), or(p, s))
    [<DataRow("AndOrDistributivePack_01", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(iif(true, false), xor(true, false)), or(iif(true, false), and(ex n:obj { is(n, N) }, true))) 2. 1, byinf AndOrDistributivePack |- true }", 0)>]
    [<DataRow("AndOrDistributivePack_02", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(all x:obj { is(x, N) }, not(false)), or(all x:obj { is(x, N) }, xor(true, false))) 2. 1, byinf AndOrDistributivePack |- true }", 0)>]
    [<DataRow("AndOrDistributivePack_03", "inf AndOrDistributivePack{dec p,q,s:pred; pre:and(or(p,q),or(p,s)) con:or(p,and(q,s))} thm T {true} proof T$1 {1: and(or(not(true), impl(true, false)), or(not(true), ex y:obj { is(y, N) })) 2. 1, byinf AndOrDistributivePack |- true }", 0)>]

    // AndOrDistributiveUnpack: pre: and(p, or(q, s))
    [<DataRow("AndOrDistributiveUnpack_01", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(iif(true, ex x:obj { is(x, N) }), or(and(true, not(false)), xor(false, true))) 2. 1, byinf AndOrDistributiveUnpack |- true }", 0)>]
    [<DataRow("AndOrDistributiveUnpack_02", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, or(ex y:obj { is(y, M) }, impl(true, false))) 2. 1, byinf AndOrDistributiveUnpack |- true }", 0)>]
    [<DataRow("AndOrDistributiveUnpack_03", "inf AndOrDistributiveUnpack{dec p,q,s:pred; pre:and(p,or(q,s)) con:or(and(p,q),and(p,s))} thm T {true} proof T$1 {1: and(not(xor(true, false)), or(iif(true, false), and(exn$1 z:obj { is(z, N) }, true))) 2. 1, byinf AndOrDistributiveUnpack |- true }", 0)>]

    // OrAndDistributivePack: pre: or(and(p, q), and(p, s))
    [<DataRow("OrAndDistributivePack_01", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(all x:obj { is(x, N) }, not(false)), and(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf OrAndDistributivePack |- true }", 0)>]
    [<DataRow("OrAndDistributivePack_02", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(iif(true, false), and(true, not(false))), and(iif(true, false), impl(true, false))) 2. 1, byinf OrAndDistributivePack |- true }", 0)>]
    [<DataRow("OrAndDistributivePack_03", "inf OrAndDistributivePack{dec p,q,s:pred; pre:or(and(p,q),and(p,s)) con:and(p,or(q,s))} thm T {true} proof T$1 {1: or(and(ex x:obj { is(x, N) }, or(true, false)), and(ex x:obj { is(x, N) }, xor(true, false))) 2. 1, byinf OrAndDistributivePack |- true }", 0)>]

    // DeMorganAndUnpack: pre: not and(p, q)
    [<DataRow("DeMorganAndUnpack_01", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(not(true), xor(true, false)) 2. 1, byinf DeMorganAndUnpack |- true }", 0)>]
    [<DataRow("DeMorganAndUnpack_02", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(all x:obj { is(x, N) }, ex y:obj { not is(y, M) }) 2. 1, byinf DeMorganAndUnpack |- true }", 0)>]
    [<DataRow("DeMorganAndUnpack_03", "inf DeMorganAndUnpack{dec p,q:pred; pre:not and(p,q) con:or(not p,not q)} thm T {true} proof T$1 {1: not and(iif(true, false), impl(true, false)) 2. 1, byinf DeMorganAndUnpack |- true }", 0)>]

    // DeMorganOrPack: pre: or(not p, not q)
    [<DataRow("DeMorganOrPack_01", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (and(true, false)), not (ex x:obj { is(x, N) })) 2. 1, byinf DeMorganOrPack |- true }", 0)>]
    [<DataRow("DeMorganOrPack_02", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (iif(true, false)), not (all x:obj { is(x, N) })) 2. 1, byinf DeMorganOrPack |- true }", 0)>]
    [<DataRow("DeMorganOrPack_03", "inf DeMorganOrPack{dec p,q:pred; pre:or(not p,not q) con:not and(p,q)} thm T {true} proof T$1 {1: or(not (xor(true, false)), not (impl(true, false))) 2. 1, byinf DeMorganOrPack |- true }", 0)>]

    // DeMorganOrUnpack: pre: not or(p, q)
    [<DataRow("DeMorganOrUnpack_01", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(iif(true, false), ex x:obj { is(x, N) }) 2. 1, byinf DeMorganOrUnpack |- true }", 0)>]
    [<DataRow("DeMorganOrUnpack_02", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(all x:obj { is(x, N) }, xor(true, false)) 2. 1, byinf DeMorganOrUnpack |- true }", 0)>]
    [<DataRow("DeMorganOrUnpack_03", "inf DeMorganOrUnpack{dec p,q:pred; pre:not or(p,q) con:and(not p,not q)} thm T {true} proof T$1 {1: not or(impl(true, false), and(true, false)) 2. 1, byinf DeMorganOrUnpack |- true }", 0)>]

    // DeMorganAndPack: pre: and(not p, not q)
    [<DataRow("DeMorganAndPack_01", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (iif(true, false)), not (ex x:obj { is(x, N) })) 2. 1, byinf DeMorganAndPack |- true }", 0)>]
    [<DataRow("DeMorganAndPack_02", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (all x:obj { is(x, N) }), not (xor(true, false))) 2. 1, byinf DeMorganAndPack |- true }", 0)>]
    [<DataRow("DeMorganAndPack_03", "inf DeMorganAndPack{dec p,q:pred; pre:and(not p,not q) con:not or(p,q)} thm T {true} proof T$1 {1: and(not (impl(true, false)), not (and(true, false))) 2. 1, byinf DeMorganAndPack |- true }", 0)>]

    // NotDouble: pre: not not p
    [<DataRow("NotDouble_01", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (and(true, xor(false, true))) 2. 1, byinf NotDouble |- true }", 0)>]
    [<DataRow("NotDouble_02", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (ex x:obj { not is(x, N) }) 2. 1, byinf NotDouble |- true }", 0)>]
    [<DataRow("NotDouble_03", "inf NotDouble{dec p:pred; pre:not not p con:p} thm T {true} proof T$1 {1: not not (iif(true, ex y:obj { is(y, M) })) 2. 1, byinf NotDouble |- true }", 0)>]

    // ImplUnpack2Or: pre: impl(p, q)
    [<DataRow("ImplUnpack2Or_01", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf ImplUnpack2Or |- true }", 0)>]
    [<DataRow("ImplUnpack2Or_02", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(not (xor(true, false)), iif(true, false)) 2. 1, byinf ImplUnpack2Or |- true }", 0)>]
    [<DataRow("ImplUnpack2Or_03", "inf ImplUnpack2Or{dec p,q:pred; pre:impl(p,q) con:or(not p,q)} thm T {true} proof T$1 {1: impl(and(true, not(false)), or(ex x:obj { is(x, N) }, true)) 2. 1, byinf ImplUnpack2Or |- true }", 0)>]

    // OrPack2Impl: pre: or(not p, q)
    [<DataRow("OrPack2Impl_01", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf OrPack2Impl |- true }", 0)>]
    [<DataRow("OrPack2Impl_02", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not (iif(true, false)), and(true, false)) 2. 1, byinf OrPack2Impl |- true }", 0)>]
    [<DataRow("OrPack2Impl_03", "inf OrPack2Impl{dec p,q:pred; pre:or(not p,q) con:impl(p,q)} thm T {true} proof T$1 {1: or(not (xor(true, false)), impl(true, false)) 2. 1, byinf OrPack2Impl |- true }", 0)>]

    // IifUnpack2Or: pre: iif(p, q)
    [<DataRow("IifUnpack2Or_01", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf IifUnpack2Or |- true }", 0)>]
    [<DataRow("IifUnpack2Or_02", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(not (xor(true, false)), and(true, false)) 2. 1, byinf IifUnpack2Or |- true }", 0)>]
    [<DataRow("IifUnpack2Or_03", "inf IifUnpack2Or{dec p,q:pred; pre:iif(p,q) con:or(and(not p,not q),and(p,q))} thm T {true} proof T$1 {1: iif(iif(true, false), xor(true, false)) 2. 1, byinf IifUnpack2Or |- true }", 0)>]

    // IifUnpack2And: pre: iif(p, q)
    [<DataRow("IifUnpack2And_01", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(ex x:obj { is(x, N) }, iif(true, false)) 2. 1, byinf IifUnpack2And |- true }", 0)>]
    [<DataRow("IifUnpack2And_02", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(all x:obj { is(x, N) }, xor(true, false)) 2. 1, byinf IifUnpack2And |- true }", 0)>]
    [<DataRow("IifUnpack2And_03", "inf IifUnpack2And{dec p,q:pred; pre:iif(p,q) con:and(or(not p,q),or(p,not q))} thm T {true} proof T$1 {1: iif(not (and(true, false)), or(true, false)) 2. 1, byinf IifUnpack2And |- true }", 0)>]

    // OrPack2Iif: pre: or(and(not p, not q), and(p, q))
    [<DataRow("OrPack2Iif_01", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not all x:obj { is(x, N) }, not ex y:obj { is(y, M) }), and(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf OrPack2Iif |- true }", 0)>]
    [<DataRow("OrPack2Iif_02", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not iif(true, false), not xor(true, false)), and(iif(true, false), xor(true, false))) 2. 1, byinf OrPack2Iif |- true }", 0)>]
    [<DataRow("OrPack2Iif_03", "inf OrPack2Iif{dec p,q:pred; pre:or(and(not p,not q),and(p,q)) con:iif(p,q)} thm T {true} proof T$1 {1: or(and(not (impl(true, false)), not (and(true, false))), and(impl(true, false), and(true, false))) 2. 1, byinf OrPack2Iif |- true }", 0)>]

    // AndPack2Iif: pre: and(or(not p, q), or(p, not q))
    [<DataRow("AndPack2Iif_01", "inf AndPack2Iif{dec p,q:pred; pre:and(or(not p,q),or(p,not q)) con:iif(p,q)} thm T {true} proof T$1 {1: and(or(not all x:obj { is(x, N) }, ex y:obj { is(y, M) }), or(all x:obj { is(x, N) }, not ex z:obj { is(z, K) })) 2. 1, byinf AndPack2Iif |- true }", 1)>]
    [<DataRow("AndPack2Iif_02", "inf AndPack2Iif{dec p,q:pred; pre:and(or(not p,q),or(p,not q)) con:iif(p,q)} thm T {true} proof T$1 {1: and(or(not iif(true, false), xor(true, false)), or(iif(true, false), not xor(true, false))) 2. 1, byinf AndPack2Iif |- true }", 0)>]
    [<DataRow("AndPack2Iif_03", "inf AndPack2Iif{dec p,q:pred; pre:and(or(not p,q),or(p,not q)) con:iif(p,q)} thm T {true} proof T$1 {1: and(or(not (impl(true, false)), and(true, false)), or(impl(true, false), not and(true, false))) 2. 1, byinf AndPack2Iif |- true }", 0)>]

    // XorUnpack2Or: pre: xor(p, q)
    [<DataRow("XorUnpack2Or_01", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf XorUnpack2Or |- true }", 0)>]
    [<DataRow("XorUnpack2Or_02", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(not iif(true, false), and(true, false)) 2. 1, byinf XorUnpack2Or |- true }", 0)>]
    [<DataRow("XorUnpack2Or_03", "inf XorUnpack2Or{dec p,q:pred; pre:xor(p,q) con:or(and(not p,q),and(p,not q))} thm T {true} proof T$1 {1: xor(iif(true, false), xor(true, false)) 2. 1, byinf XorUnpack2Or |- true }", 0)>]

    // XorUnpack2And: pre: xor(p, q)
    [<DataRow("XorUnpack2And_01", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(and(ex x:obj { is(x, N) }, true), iif(true, false)) 2. 1, byinf XorUnpack2And |- true }", 0)>]
    [<DataRow("XorUnpack2And_02", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(not (all x:obj { is(x, N) }), or(true, false)) 2. 1, byinf XorUnpack2And |- true }", 0)>]
    [<DataRow("XorUnpack2And_03", "inf XorUnpack2And{dec p,q:pred; pre:xor(p,q) con:and(or(not p,not q),or(p,q))} thm T {true} proof T$1 {1: xor(iif(true, true), not (xor(false, true))) 2. 1, byinf XorUnpack2And |- true }", 0)>]

    // AndPack2Xor: pre: and(or(not p, not q), or(p, q))
    [<DataRow("AndPack2Xor_01", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not all x:obj { is(x, N) }, not ex y:obj { is(y, M) }), or(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf AndPack2Xor |- true }", 0)>]
    [<DataRow("AndPack2Xor_02", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not iif(true, false), not xor(true, false)), or(iif(true, false), xor(true, false))) 2. 1, byinf AndPack2Xor |- true }", 0)>]
    [<DataRow("AndPack2Xor_03", "inf AndPack2Xor{dec p,q:pred; pre:and(or(not p,not q),or(p,q)) con:xor(p,q)} thm T {true} proof T$1 {1: and(or(not (impl(true, false)), not (and(true, false))), or(impl(true, false), and(true, false))) 2. 1, byinf AndPack2Xor |- true }", 0)>]

    // OrPack2Xor: pre: or(and(not p, q), and(p, not q))
    [<DataRow("OrPack2Xor_01", "inf OrPack2Xor{dec p,q:pred; pre:or(and(not p,q),and(p,not q)) con:xor(p,q)} thm T {true} proof T$1 {1: or(and(not all x:obj { is(x, N) }, ex y:obj { is(y, M) }), and(all x:obj { is(x, N) }, not ex z:obj { is(z, K) })) 2. 1, byinf OrPack2Xor |- true }", 1)>]
    [<DataRow("OrPack2Xor_02", "inf OrPack2Xor{dec p,q:pred; pre:or(and(not p,q),and(p,not q)) con:xor(p,q)} thm T {true} proof T$1 {1: or(and(not iif(true, false), xor(true, false)), and(iif(true, false), not xor(true, false))) 2. 1, byinf OrPack2Xor |- true }", 0)>]
    [<DataRow("OrPack2Xor_03", "inf OrPack2Xor{dec p,q:pred; pre:or(and(not p,q),and(p,not q)) con:xor(p,q)} thm T {true} proof T$1 {1: or(and(not (impl(true, false)), and(true, false)), and(impl(true, false), not and(true, false))) 2. 1, byinf OrPack2Xor |- true }", 0)>]

    // Proceeding2Results: pre: p, q
    [<DataRow("Proceeding2Results_01", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: all x:obj { is(x, N) } 2: ex y:obj { is(y, M) } 3. 1, 2, byinf Proceeding2Results |- true }", 0)>]
    [<DataRow("Proceeding2Results_02", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: iif(true, false) 2: not (xor(true, false)) 3. 1, 2, byinf Proceeding2Results |- true }", 0)>]
    [<DataRow("Proceeding2Results_03", "inf Proceeding2Results{dec p,q:pred; pre:p,q con:and(p,q)} thm T {true} proof T$1 {1: and(true, false) 2: impl(true, false) 3. 1, 2, byinf Proceeding2Results |- true }", 0)>]

    // Proceeding3Results: pre: p, q, s
    [<DataRow("Proceeding3Results_01", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: all x:obj { is(x, N) } 2: ex y:obj { is(y, M) } 3: iif(true, false) 4. 1, 2, 3, byinf Proceeding3Results |- true }", 0)>]
    [<DataRow("Proceeding3Results_02", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: and(true, not(false)) 2: xor(true, false) 3: all z:obj { is(z, K) } 4. 1, 2, 3, byinf Proceeding3Results |- true }", 0)>]
    [<DataRow("Proceeding3Results_03", "inf Proceeding3Results{dec p,q,s:pred; pre:p,q,s con:and(and(p,q),s)} thm T {true} proof T$1 {1: not (xor(true, false)) 2: impl(true, false) 3: exn$1 u:obj { is(u, L) } 4. 1, 2, 3, byinf Proceeding3Results |- true }", 0)>]

    // AndUnpack2NotOr: pre: and(p, q)
    [<DataRow("AndUnpack2NotOr_01", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, ex y:obj { is(y, M) }) 2. 1, byinf AndUnpack2NotOr |- true }", 0)>]
    [<DataRow("AndUnpack2NotOr_02", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(iif(true, false), xor(true, false)) 2. 1, byinf AndUnpack2NotOr |- true }", 0)>]
    [<DataRow("AndUnpack2NotOr_03", "inf AndUnpack2NotOr{dec p,q:pred; pre:and(p,q) con:not (or(not p,not q))} thm T {true} proof T$1 {1: and(not (ex x:obj { is(x, N) }), impl(true, false)) 2. 1, byinf AndUnpack2NotOr |- true }", 0)>]

    // NotOrPack2And: pre: not (or(not p, not q))
    [<DataRow("NotOrPack2And_01", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not all x:obj { is(x, N) }, not ex y:obj { is(y, M) })) 2. 1, byinf NotOrPack2And |- true }", 0)>]
    [<DataRow("NotOrPack2And_02", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not iif(true, false), not xor(true, false))) 2. 1, byinf NotOrPack2And |- true }", 0)>]
    [<DataRow("NotOrPack2And_03", "inf NotOrPack2And{dec p,q:pred; pre:not (or(not p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (or(not impl(true, false), not and(true, false))) 2. 1, byinf NotOrPack2And |- true }", 0)>]

    // AndUnpack2NotImpl: pre: and(p, q)
    [<DataRow("AndUnpack2NotImpl_01", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, iif(true, false)) 2. 1, byinf AndUnpack2NotImpl |- true }", 0)>]
    [<DataRow("AndUnpack2NotImpl_02", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(ex x:obj { is(x, M) }, not (xor(true, false))) 2. 1, byinf AndUnpack2NotImpl |- true }", 0)>]
    [<DataRow("AndUnpack2NotImpl_03", "inf AndUnpack2NotImpl{dec p,q:pred; pre:and(p,q) con:not (impl(p,not q))} thm T {true} proof T$1 {1: and(not (impl(true, false)), all z:obj { is(z, K) }) 2. 1, byinf AndUnpack2NotImpl |- true }", 0)>]

    // NotImplPack2And: pre: not (impl(p, not q))
    [<DataRow("NotImplPack2And_01", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(all x:obj { is(x, N) }, not ex y:obj { is(y, M) })) 2. 1, byinf NotImplPack2And |- true }", 0)>]
    [<DataRow("NotImplPack2And_02", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(iif(true, false), not xor(true, false))) 2. 1, byinf NotImplPack2And |- true }", 0)>]
    [<DataRow("NotImplPack2And_03", "inf NotImplPack2And{dec p,q:pred; pre:not (impl(p,not q)) con:and(p,q)} thm T {true} proof T$1 {1: not (impl(and(true, false), not iif(true, false))) 2. 1, byinf NotImplPack2And |- true }", 0)>]

    // NotImpl2And: pre: not (impl(p, q))
    [<DataRow("NotImpl2And_01", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf NotImpl2And |- true }", 0)>]
    [<DataRow("NotImpl2And_02", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(iif(true, false), xor(true, false))) 2. 1, byinf NotImpl2And |- true }", 0)>]
    [<DataRow("NotImpl2And_03", "inf NotImpl2And{dec p,q:pred; pre:not (impl(p,q)) con:and(p,not q)} thm T {true} proof T$1 {1: not (impl(and(true, false), not (ex x:obj { is(x, N) }))) 2. 1, byinf NotImpl2And |- true }", 0)>]

    // And2NotImpl: pre: and(p, not q)
    [<DataRow("And2NotImpl_01", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(all x:obj { is(x, N) }, not ex y:obj { is(y, M) }) 2. 1, byinf And2NotImpl |- true }", 0)>]
    [<DataRow("And2NotImpl_02", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(iif(true, false), not xor(true, false)) 2. 1, byinf And2NotImpl |- true }", 0)>]
    [<DataRow("And2NotImpl_03", "inf And2NotImpl{dec p,q:pred; pre:and(p,not q) con:not (impl(p,q))} thm T {true} proof T$1 {1: and(not (impl(true, false)), not all z:obj { is(z, K) }) 2. 1, byinf And2NotImpl |- true }", 0)>]

    // NotIif2Or: pre: not (iif(p, q))
    [<DataRow("NotIif2Or_01", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf NotIif2Or |- true }", 0)>]
    [<DataRow("NotIif2Or_02", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(iif(true, false), xor(true, false))) 2. 1, byinf NotIif2Or |- true }", 0)>]
    [<DataRow("NotIif2Or_03", "inf NotIif2Or{dec p,q:pred; pre:not (iif(p,q)) con:or(and(p,not q),and(not p,q))} thm T {true} proof T$1 {1: not (iif(not (and(true, false)), impl(true, false))) 2. 1, byinf NotIif2Or |- true }", 0)>]

    // Or2NotIif: pre: or(and(p, not q), and(not p, q))
    [<DataRow("Or2NotIif_01", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(all x:obj { is(x, N) }, not ex y:obj { is(y, M) }), and(not all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf Or2NotIif |- true }", 0)>]
    [<DataRow("Or2NotIif_02", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(iif(true, false), not xor(true, false)), and(not iif(true, false), xor(true, false))) 2. 1, byinf Or2NotIif |- true }", 0)>]
    [<DataRow("Or2NotIif_03", "inf Or2NotIif{dec p,q:pred; pre:or(and(p,not q),and(not p,q)) con:not (iif(p,q))} thm T {true} proof T$1 {1: or(and(impl(true, false), not all z:obj { is(z, K) }), and(not (impl(true, false)), all z:obj { is(z, K) })) 2. 1, byinf Or2NotIif |- true }", 0)>]

    // NotXor2Or: pre: not (xor(p, q))
    [<DataRow("NotXor2Or_01", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(all x:obj { is(x, N) }, ex y:obj { is(y, M) })) 2. 1, byinf NotXor2Or |- true }", 0)>]
    [<DataRow("NotXor2Or_02", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(iif(true, false), and(true, false))) 2. 1, byinf NotXor2Or |- true }", 0)>]
    [<DataRow("NotXor2Or_03", "inf NotXor2Or{dec p,q:pred; pre:not (xor(p,q)) con:or(and(p,q),and(not p,not q))} thm T {true} proof T$1 {1: not (xor(not (impl(true, false)), xor(true, false))) 2. 1, byinf NotXor2Or |- true }", 0)>]

    // Or2NotXor: pre: or(and(p, q), and(not p, not q))
    [<DataRow("Or2NotXor_01", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(all x:obj { is(x, N) }, ex y:obj { is(y, M) }), and(not all x:obj { is(x, N) }, not ex y:obj { is(y, M) })) 2. 1, byinf Or2NotXor |- true }", 0)>]
    [<DataRow("Or2NotXor_02", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(iif(true, false), xor(true, false)), and(not iif(true, false), not xor(true, false))) 2. 1, byinf Or2NotXor |- true }", 0)>]
    [<DataRow("Or2NotXor_03", "inf Or2NotXor{dec p,q:pred; pre:or(and(p,q),and(not p,not q)) con:not (xor(p,q))} thm T {true} proof T$1 {1: or(and(impl(true, false), and(true, false)), and(not (impl(true, false)), not (and(true, false)))) 2. 1, byinf Or2NotXor |- true }", 0)>]

    // NotAll2ExNot: pre: not all x1:tpl { p(x1) }
    [<DataRow("NotAll2ExNot_01", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all x1:obj { is(x1, N) } 2. 1, byinf NotAll2ExNot |- true }", 0)>]
    [<DataRow("NotAll2ExNot_02", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all n:Nat { impl(is(n, N), xor(true, false)) } 2. 1, byinf NotAll2ExNot |- true }", 0)>]
    [<DataRow("NotAll2ExNot_03", "inf NotAll2ExNot{dec p:pred(x:tpl); pre:not all x1:tpl{p(x1)} con:ex x1:tpl{not p(x1)}} thm T {true} proof T$1 {1: not all y:obj { not is(y, M) } 2. 1, byinf NotAll2ExNot |- true }", 0)>]

    // ExNot2NotAll: pre: ex x:tpl{not p(x)}
    [<DataRow("ExNot2NotAll_01", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex x:obj { not iif(is(x,N), true) } 2. 1, byinf ExNot2NotAll |- true }", 0)>]
    [<DataRow("ExNot2NotAll_02", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex n:Nat { not and(is(n,N), xor(true,false)) } 2. 1, byinf ExNot2NotAll |- true }", 0)>]
    [<DataRow("ExNot2NotAll_03", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex y:obj { not (all z:obj { impl(is(z,N), false) }) } 2. 1, byinf ExNot2NotAll |- true }", 1)>]
    [<DataRow("ExNot2NotAll_03a", "inf ExNot2NotAll{dec p:pred(y:tpl); pre:ex x:tpl{not p(x)} con:not all x:tpl{p(x)}} thm T {true} proof T$1 {1: ex y:obj { not impl(is(y,N), false) } 2. 1, byinf ExNot2NotAll |- true }", 0)>]

    // NotEx2AllNot: pre: not ex x:tpl{p(x)}
    [<DataRow("NotEx2AllNot_01", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex x:obj { iif(is(x,N), false) } 2. 1, byinf NotEx2AllNot |- true }", 0)>]
    [<DataRow("NotEx2AllNot_02", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex n:Nat { and(not is(n,N), xor(true,false)) } 2. 1, byinf NotEx2AllNot |- true }", 0)>]
    [<DataRow("NotEx2AllNot_03", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex y:obj { not (ex z:obj { is(z,M) }) } 2. 1, byinf NotEx2AllNot |- true }", 1)>]
    [<DataRow("NotEx2AllNot_03a", "inf NotEx2AllNot{dec p:pred(y:tpl); pre:not ex x:tpl{p(x)} con:all x:tpl{not p(x)}} thm T {true} proof T$1 {1: not ex y:obj { not is(y,M) } 2. 1, byinf NotEx2AllNot |- true }", 0)>]

    // AllNot2ExNot: pre: all x:tpl{not p(x)}
    [<DataRow("AllNot2ExNot_01", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all x:obj { not iif(is(x,N), true) } 2. 1, byinf AllNot2ExNot |- true }", 0)>]
    [<DataRow("AllNot2ExNot_02", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all n:Nat { not (and(is(n,N), xor(true,false))) } 2. 1, byinf AllNot2ExNot |- true }", 0)>]
    [<DataRow("AllNot2ExNot_03", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all y:obj { not (ex z:obj { is(z,M) }) } 2. 1, byinf AllNot2ExNot |- true }", 1)>]
    [<DataRow("AllNot2ExNot_03a", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all y:obj { not is(y,M) } 2. 1, byinf AllNot2ExNot |- true }", 0)>]

    // OrUnpack2Impl: pre: or(p,q)
    [<DataRow("OrUnpack2Impl_01", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(iif(is(A,N), true), ex x:obj { is(x,M) }) 2. 1, byinf OrUnpack2Impl |- true }", 0)>]
    [<DataRow("OrUnpack2Impl_02", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(all x:obj { is(x,N) }, and(true, not false)) 2. 1, byinf OrUnpack2Impl |- true }", 0)>]
    [<DataRow("OrUnpack2Impl_03", "inf OrUnpack2Impl{dec p,q:pred; pre:or(p,q) con:impl(not p,q)} thm T {true} proof T$1 {1: or(xor(true,false), impl(all z:obj { is(z,N) }, false)) 2. 1, byinf OrUnpack2Impl |- true }", 0)>]

    // ImplPack2Or: pre: impl(not p,q)
    [<DataRow("ImplPack2Or_01", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (and(is(A,N), true)), ex x:obj { is(x,M) }) 2. 1, byinf ImplPack2Or |- true }", 0)>]
    [<DataRow("ImplPack2Or_02", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (xor(true,false)), iif(all x:obj { is(x,N) }, false)) 2. 1, byinf ImplPack2Or |- true }", 0)>]
    [<DataRow("ImplPack2Or_03", "inf ImplPack2Or{dec p,q:pred; pre:impl(not p,q) con:or(p,q)} thm T {true} proof T$1 {1: impl(not (iif(true,false)), or(ex y:obj { is(y,M) }, true)) 2. 1, byinf ImplPack2Or |- true }", 0)>]

    // ModusTollens: pre: not q, impl(p,q)
    [<DataRow("ModusTollens_01", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not (iif(true,false)) 2: impl(all x:obj { is(x,N) }, iif(true,false)) 3. 1, 2, byinf ModusTollens |- true }", 0)>]
    [<DataRow("ModusTollens_02", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not ex x:obj { is(x,N) } 2: impl(xor(true,false), all y:obj { is(y,M) }) 3. 1, 2, byinf ModusTollens |- true }", 1)>]
    [<DataRow("ModusTollens_02a", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not ex x:obj { is(x,N) } 2: impl(xor(true,false), ex x:obj { is(x,N) }) 3. 1, 2, byinf ModusTollens |- true }", 0)>]
    [<DataRow("ModusTollens_03", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not (and(is(A,N), false)) 2: impl(iif(true,false), xor(true,false)) 3. 1, 2, byinf ModusTollens |- true }", 1)>]
    [<DataRow("ModusTollens_03a", "inf ModusTollens{dec p,q:pred; pre:not q,impl(p,q) con:not (p)} thm T {true} proof T$1 {1: not (and(is(A,N), false)) 2: impl(iif(true,false), and(is(A,N), false)) 3. 1, 2, byinf ModusTollens |- true }", 0)>]

    // HypotheticalSyllogism: pre: impl(p,q), impl(q,s)
    [<DataRow("HypotheticalSyllogism_01", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(all x:obj { is(x,N) }, ex y:obj { is(y,M) }) 2: impl(ex z:obj { is(z,M) }, xor(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }", 0)>]
    [<DataRow("HypotheticalSyllogism_02", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(iif(true,false), iif(iif(true,false), true)) 2: impl(all u:obj { is(u,K) }, iif(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }", 1)>]
    [<DataRow("HypotheticalSyllogism_02a", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(iif(true,false), iif(iif(true,false), true)) 2: impl(iif(iif(true,false), true), all u:obj { is(u,K) }) 3. 1, 2, byinf HypotheticalSyllogism |- true }", 0)>]
    [<DataRow("HypotheticalSyllogism_03", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(iif(true,false), iif(iif(true,false), true)) 2: impl(all u:obj { is(u,K) }, iif(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }", 1)>]
    [<DataRow("HypotheticalSyllogism_03a", "inf HypotheticalSyllogism{dec p,q,s:pred; pre:impl(p,q),impl(q,s) con:impl(p,s)} thm T {true} proof T$1 {1: impl(iif(true,false), all u:obj { is(u,K) }) 2: impl(all u:obj { is(u,K) }, iif(true,false)) 3. 1, 2, byinf HypotheticalSyllogism |- true }", 0)>]


    // DisjunctiveSyllogism: pre: not p, or(p,q)
    [<DataRow("DisjunctiveSyllogism_01", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not (iif(true,false)) 2: or(all x:obj { is(x,N) }, ex y:obj { is(y,M) }) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", 1)>]
    [<DataRow("DisjunctiveSyllogism_01a", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not (iif(true,false)) 2: or(iif(true,false), ex y:obj { is(y,M) }) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", 0)>]
    [<DataRow("DisjunctiveSyllogism_02", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not iif(true,false) 2: or(iif(true,false), xor(true,false)) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", 0)>]
    [<DataRow("DisjunctiveSyllogism_02a", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not ex x:obj { is(x,N) } 2: or(iif(true,false), xor(true,false)) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", 1)>]
    [<DataRow("DisjunctiveSyllogism_03", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not (and(is(A,N), false)) 2: or(not (iif(true,false)), impl(true,false)) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", 1)>]
    [<DataRow("DisjunctiveSyllogism_03a", "inf DisjunctiveSyllogism{dec p,q:pred; pre:not p,or(p,q) con:q} thm T {true} proof T$1 {1: not (and(is(A,N), false)) 2: or(and(is(A,N), false), impl(true,false)) 3. 1, 2, byinf DisjunctiveSyllogism |- true }", 0)>]
    
    // ExistsByExample: pre: p(c)
    [<DataRow("ExistsByExample_01", "inf ExistsByExample{dec p:pred(c:tpl); pre:p(c) con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: iif(is(c,N), true) 2. 1, byinf ExistsByExample |- true }", 0)>]
    [<DataRow("ExistsByExample_02", "inf ExistsByExample{dec p:pred(c:tpl); pre:p(c) con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: and(ex x:obj { is(x,M) }, iif(true,false)) 2. 1, byinf ExistsByExample |- true }", 1)>]
    [<DataRow("ExistsByExample_02a", "inf ExistsByExample{dec p:pred(); pre:p() con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: and(ex x:obj { is(x,M) }, iif(true,false)) 2. 1, byinf ExistsByExample |- true }", 0)>]
    [<DataRow("ExistsByExample_03", "inf ExistsByExample{dec p:pred(c:tpl); pre:p(c) con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: xor(all z:obj { is(z,K) }, not (xor(true,false))) 2. 1, byinf ExistsByExample |- true }", 1)>]
    [<DataRow("ExistsByExample_03a", "inf ExistsByExample{dec p:pred(); pre:p() con:ex x:tpl{p(x)}} thm T {true} proof T$1 {1: xor(all z:obj { is(z,K) }, not (xor(true,false))) 2. 1, byinf ExistsByExample |- true }", 0)>]

    // Contraposition: pre: impl(not p, not q)
    [<DataRow("Contraposition_01", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not all x:obj { is(x,N) }, not (ex y:obj { is(y,M) })) 2. 1, byinf Contraposition |- true }", 0)>]
    [<DataRow("Contraposition_02", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not (iif(true,false)), not (xor(true,false))) 2. 1, byinf Contraposition |- true }", 0)>]
    [<DataRow("Contraposition_03", "inf Contraposition{dec p,q:pred; pre:impl(not p,not q) con:impl(q,p)} thm T {true} proof T$1 {1: impl(not (and(is(A,N), true)), not (iif(true,false))) 2. 1, byinf Contraposition |- true }", 0)>]

    // WeakeningRule: pre: p
    [<DataRow("WeakeningRule_01", "inf WeakeningRule{dec p,q:pred; pre:p con:impl(q,p)} thm T {true} proof T$1 {1: iif(true, ex x:obj { is(x,N) }) 2. 1, byinf WeakeningRule |- true }", 0)>]
    [<DataRow("WeakeningRule_02", "inf WeakeningRule{dec p,q:pred; pre:p con:impl(q,p)} thm T {true} proof T$1 {1: all x:obj { is(x,N) } 2. 1, byinf WeakeningRule |- true }", 0)>]
    [<DataRow("WeakeningRule_03", "inf WeakeningRule{dec p,q:pred; pre:p con:impl(q,p)} thm T {true} proof T$1 {1: and(not (xor(true,false)), iif(true,false)) 2. 1, byinf WeakeningRule |- true }", 0)>]

    // PrenexUnpackAndEx: pre: and(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackAndEx_01", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackAndEx |- true }", 0)>]
    [<DataRow("PrenexUnpackAndEx_02", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(all z:obj { is(z,K) }, ex y:obj { xor(is(y,M), true) }) 2. 1, byinf PrenexUnpackAndEx |- true }", 0)>]
    [<DataRow("PrenexUnpackAndEx_03", "inf PrenexUnpackAndEx{dec p:pred q:pred(z:tpl); pre:and(p,ex x:tpl{q(x)}) con:ex x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackAndEx |- true }", 0)>]

    // PrenexPackExAnd: pre: ex x:tpl{and(p, q(x))}
    [<DataRow("PrenexPackExAnd_01", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { and(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExAnd |- true }", 0)>]
    [<DataRow("PrenexPackExAnd_02", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { and(all y:obj { is(y,K) }, xor(is(n,M), true)) } 2. 1, byinf PrenexPackExAnd |- true }", 0)>]
    [<DataRow("PrenexPackExAnd_03", "inf PrenexPackExAnd{dec p:pred q:pred(z:tpl); pre:ex x:tpl{and(p,q(x))} con:and(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { and(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExAnd |- true }", 0)>]

    // PrenexUnpackAndAll: pre: and(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackAndAll_01", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackAndAll |- true }", 0)>]
    [<DataRow("PrenexUnpackAndAll_02", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(ex y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackAndAll |- true }", 0)>]
    [<DataRow("PrenexUnpackAndAll_03", "inf PrenexUnpackAndAll{dec p:pred q:pred(z:tpl); pre:and(p,all x:tpl{q(x)}) con:all x:tpl{and(p,q(x))}} thm T {true} proof T$1 {1: and(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackAndAll |- true }", 0)>]

    // PrenexPackAllAnd: pre: all x:tpl{and(p, q(x))}
    [<DataRow("PrenexPackAllAnd_01", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj { and(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackAllAnd |- true }", 0)>]
    [<DataRow("PrenexPackAllAnd_02", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat { and(ex y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackAllAnd |- true }", 0)>]
    [<DataRow("PrenexPackAllAnd_03", "inf PrenexPackAllAnd{dec p:pred q:pred(z:tpl); pre:all x:tpl{and(p,q(x))} con:and(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj { and(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackAllAnd |- true }", 0)>]

    // PrenexUnpackOrEx: pre: or(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackOrEx_01", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackOrEx |- true }", 0)>]
    [<DataRow("PrenexUnpackOrEx_02", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(all y:obj { is(y,M) }, ex z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackOrEx |- true }", 0)>]
    [<DataRow("PrenexUnpackOrEx_03", "inf PrenexUnpackOrEx{dec p:pred q:pred(z:tpl); pre:or(p,ex x:tpl{q(x)}) con:ex x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackOrEx |- true }", 0)>]

    // PrenexPackExOr: pre: ex x:tpl{or(p, q(x))}
    [<DataRow("PrenexPackExOr_01", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { or(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExOr |- true }", 0)>]
    [<DataRow("PrenexPackExOr_02", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { or(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackExOr |- true }", 0)>]
    [<DataRow("PrenexPackExOr_03", "inf PrenexPackExOr{dec p:pred q:pred(z:tpl); pre:ex x:tpl{or(p,q(x))} con:or(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { or(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExOr |- true }", 0)>]

    // PrenexUnpackOrAll: pre: or(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackOrAll_01", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackOrAll |- true }", 0)>]
    [<DataRow("PrenexUnpackOrAll_02", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(ex y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackOrAll |- true }", 0)>]
    [<DataRow("PrenexUnpackOrAll_03", "inf PrenexUnpackOrAll{dec p:pred q:pred(z:tpl); pre:or(p,all x:tpl{q(x)}) con:all x:tpl{or(p,q(x))}} thm T {true} proof T$1 {1: or(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackOrAll |- true }", 0)>]

    // PrenexPackAllOr: pre: all x:tpl{or(p, q(x))}
    [<DataRow("PrenexPackAllOr_01", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj { or(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackAllOr |- true }", 0)>]
    [<DataRow("PrenexPackAllOr_02", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat { or(ex y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackAllOr |- true }", 0)>]
    [<DataRow("PrenexPackAllOr_03", "inf PrenexPackAllOr{dec p:pred q:pred(z:tpl); pre:all x:tpl{or(p,q(x))} con:or(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj { or(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackAllOr |- true }", 0)>]

    // PrenexUnpackImplEx: pre: impl(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackImplEx_01", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackImplEx |- true }", 0)>]
    [<DataRow("PrenexUnpackImplEx_02", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(all y:obj { is(y,M) }, ex z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackImplEx |- true }", 0)>]
    [<DataRow("PrenexUnpackImplEx_03", "inf PrenexUnpackImplEx{dec p:pred q:pred(z:tpl); pre:impl(p,ex x:tpl{q(x)}) con:ex x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackImplEx |- true }", 0)>]

    // PrenexPackExImpl: pre: ex x:tpl{impl(p, q(x))}
    [<DataRow("PrenexPackExImpl_01", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { impl(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExImpl |- true }", 0)>]
    [<DataRow("PrenexPackExImpl_02", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { impl(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackExImpl |- true }", 0)>]
    [<DataRow("PrenexPackExImpl_03", "inf PrenexPackExImpl{dec p:pred q:pred(z:tpl); pre:ex x:tpl{impl(p,q(x))} con:impl(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { impl(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExImpl |- true }", 0)>]

    // PrenexUnpackImplAll: pre: impl(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackImplAll_01", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackImplAll |- true }", 0)>]
    [<DataRow("PrenexUnpackImplAll_02", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(all y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackImplAll |- true }", 0)>]
    [<DataRow("PrenexUnpackImplAll_03", "inf PrenexUnpackImplAll{dec p:pred q:pred(z:tpl); pre:impl(p,all x:tpl{q(x)}) con:all x:tpl{impl(p,q(x))}} thm T {true} proof T$1 {1: impl(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackImplAll |- true }", 0)>]

    // PrenexPackAllImpl: pre: all x:tpl{impl(p, q(x))}
    [<DataRow("PrenexPackAllImpl_01", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj { impl(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackAllImpl |- true }", 0)>]
    [<DataRow("PrenexPackAllImpl_02", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat { impl(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackAllImpl |- true }", 0)>]
    [<DataRow("PrenexPackAllImpl_03", "inf PrenexPackAllImpl{dec p:pred q:pred(z:tpl); pre:all x:tpl{impl(p,q(x))} con:impl(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj { impl(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackAllImpl |- true }", 0)>]

    // PrenexUnpackIifEx: pre: iif(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackIifEx_01", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackIifEx |- true }", 0)>]
    [<DataRow("PrenexUnpackIifEx_02", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(all y:obj { is(y,M) }, ex z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackIifEx |- true }", 0)>]
    [<DataRow("PrenexUnpackIifEx_03", "inf PrenexUnpackIifEx{dec p:pred q:pred(z:tpl); pre:iif(p,ex x:tpl{q(x)}) con:ex x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackIifEx |- true }", 0)>]

    // PrenexPackExIif: pre: ex x:tpl{iif(p, q(x))}
    [<DataRow("PrenexPackExIif_01", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { iif(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExIif |- true }", 0)>]
    [<DataRow("PrenexPackExIif_02", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { iif(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackExIif |- true }", 0)>]
    [<DataRow("PrenexPackExIif_03", "inf PrenexPackExIif{dec p:pred q:pred(z:tpl); pre:ex x:tpl{iif(p,q(x))} con:iif(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { iif(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExIif |- true }", 0)>]

    // PrenexUnpackIifAll: pre: iif(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackIifAll_01", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackIifAll |- true }", 0)>]
    [<DataRow("PrenexUnpackIifAll_02", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(all y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackIifAll |- true }", 0)>]
    [<DataRow("PrenexUnpackIifAll_03", "inf PrenexUnpackIifAll{dec p:pred q:pred(z:tpl); pre:iif(p,all x:tpl{q(x)}) con:all x:tpl{iif(p,q(x))}} thm T {true} proof T$1 {1: iif(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackIifAll |- true }", 0)>]

    // PrenexPackAllIif: pre: all x:tpl{iif(p, q(x))}
    [<DataRow("PrenexPackAllIif_01", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all x:obj { iif(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackAllIif |- true }", 0)>]
    [<DataRow("PrenexPackAllIif_02", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all n:Nat { iif(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackAllIif |- true }", 0)>]
    [<DataRow("PrenexPackAllIif_03", "inf PrenexPackAllIif{dec p:pred q:pred(z:tpl); pre:all x:tpl{iif(p,q(x))} con:iif(p,all x:tpl{q(x)})} thm T {true} proof T$1 {1: all z:obj { iif(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackAllIif |- true }", 0)>]

    // PrenexUnpackXorEx: pre: xor(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackXorEx_01", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(iif(true,false), ex n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackXorEx |- true }", 0)>]
    [<DataRow("PrenexUnpackXorEx_02", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(all y:obj { is(y,M) }, ex z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackXorEx |- true }", 0)>]
    [<DataRow("PrenexUnpackXorEx_03", "inf PrenexUnpackXorEx{dec p:pred q:pred(z:tpl); pre:xor(p,ex x:tpl{q(x)}) con:ex x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(not (xor(true,false)), ex x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackXorEx |- true }", 0)>]

    // PrenexPackExXor: pre: ex x:tpl{xor(p, q(x))}
    [<DataRow("PrenexPackExXor_01", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex x:obj { xor(iif(true,false), iif(is(x,N), true)) } 2. 1, byinf PrenexPackExXor |- true }", 0)>]
    [<DataRow("PrenexPackExXor_02", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex n:Nat { xor(all y:obj { is(y,M) }, xor(is(n,K), true)) } 2. 1, byinf PrenexPackExXor |- true }", 0)>]
    [<DataRow("PrenexPackExXor_03", "inf PrenexPackExXor{dec p:pred q:pred(z:tpl); pre:ex x:tpl{xor(p,q(x))} con:xor(p,ex x:tpl{q(x)})} thm T {true} proof T$1 {1: ex z:obj { xor(not (xor(true,false)), impl(is(z,N), false)) } 2. 1, byinf PrenexPackExXor |- true }", 0)>]

    // PrenexUnpackXorAll: pre: xor(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackXorAll_01", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(iif(true,false), all n:Nat { iif(is(n,N), true) }) 2. 1, byinf PrenexUnpackXorAll |- true }", 0)>]
    [<DataRow("PrenexUnpackXorAll_02", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(all y:obj { is(y,M) }, all z:obj { xor(is(z,K), true) }) 2. 1, byinf PrenexUnpackXorAll |- true }", 0)>]
    [<DataRow("PrenexUnpackXorAll_03", "inf PrenexUnpackXorAll{dec p:pred q:pred(z:tpl); pre:xor(p,all x:tpl{q(x)}) con:all x:tpl{xor(p,q(x))}} thm T {true} proof T$1 {1: xor(not (xor(true,false)), all x:obj { impl(is(x,N), false) }) 2. 1, byinf PrenexUnpackXorAll |- true }", 0)>]

    // correct matching of the number of quantor variables
    [<DataRow("numb_ex_2_2", "inf X { pre:ex x,y:pred { false } con:false } thm T {true} proof T$1 {1: ex a,b:pred { false } 2. 1, byinf X |- true }", 0)>] 
    [<DataRow("numb_ex_2_1", "inf X { pre:ex x,y:pred { false } con:false } thm T {true} proof T$1 {1: ex a:pred { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("numb_ex_2_3", "inf X { pre:ex x,y:pred { false } con:false } thm T {true} proof T$1 {1: ex a,b,c:pred { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("numb_all_2_2", "inf X { pre:all x,y:pred { false } con:false } thm T {true} proof T$1 {1: all a,b:pred { false } 2. 1, byinf X |- true }", 0)>] 
    [<DataRow("numb_all_2_1", "inf X { pre:all x,y:pred { false } con:false } thm T {true} proof T$1 {1: all a:pred { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("numb_all_2_3", "inf X { pre:all x,y:pred { false } con:false } thm T {true} proof T$1 {1: all a,b,c:pred { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("numb_exn_2_2", "inf X { pre:exn$2 x:pred { false } con:false } thm T {true} proof T$1 {1: exn$2 a:pred { false } 2. 1, byinf X |- true }", 0)>] 
    [<DataRow("numb_exn_2_1", "inf X { pre:exn$2 x:pred { false } con:false } thm T {true} proof T$1 {1: exn$1 a:pred { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("numb_exn_2_3", "inf X { pre:exn$2 x:pred { false } con:false } thm T {true} proof T$1 {1: exn$3 a:pred { false } 2. 1, byinf X |- true }", 1)>] 

    // correct matching of the types of quantor variables
    [<DataRow("type_ex_2_2", "inf X { pre:ex x,y:pred { false } con:false } thm T {true} proof T$1 {1: ex a,b:pred { false } 2. 1, byinf X |- true }", 0)>] 
    [<DataRow("type_ex_2_1", "inf X { pre:ex x,y:pred { false } con:false } thm T {true} proof T$1 {1: ex a:A,b:pred { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("type_ex_2_3", "inf X { pre:ex x,y:pred { false } con:false } thm T {true} proof T$1 {1: ex a:pred,b:ind { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("type_all_2_2", "inf X { pre:all x,y:pred { false } con:false } thm T {true} proof T$1 {1: all a,b:pred { false } 2. 1, byinf X |- true }", 0)>] 
    [<DataRow("type_all_2_1", "inf X { pre:all x,y:pred { false } con:false } thm T {true} proof T$1 {1: all a:ind,b:pred { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("type_all_2_3", "inf X { pre:all x,y:pred { false } con:false } thm T {true} proof T$1 {1: all a:pred,b:ind { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("type_exn_2_2", "inf X { pre:exn$2 x:pred { false } con:false } thm T {true} proof T$1 {1: exn$2 a:pred { false } 2. 1, byinf X |- true }", 0)>] 
    [<DataRow("type_exn_2_1", "inf X { pre:exn$2 x:pred { false } con:false } thm T {true} proof T$1 {1: exn$2 a:obj { false } 2. 1, byinf X |- true }", 1)>] 
    [<DataRow("type_exn_2_3", "inf X { pre:exn$2 x:pred { false } con:false } thm T {true} proof T$1 {1: exn$2 a:func { false } 2. 1, byinf X |- true }", 1)>] 

    [<DataRow("00", "inf AllNot2ExNot{dec p:pred(y:tpl); pre:all x:tpl{not p(x)} con:not ex x:tpl{p(x)}} thm T {true} proof T$1 {1: all y:obj {not ex a:obj { (y = a)}  } 2. 1, byinf AllNot2ExNot |- true }", 1)>] // specific for the error type with the message defined in errExprMismatchVarNumbDifferent
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR008(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR008 ("", 0, "", "") 
            runTestHelper "TestPR008.fpl" fplCode code expected
