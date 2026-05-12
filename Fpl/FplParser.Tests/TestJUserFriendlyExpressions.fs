namespace FplParser.Tests.UserFriendly
open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestUserFriendlyExpressions() =


    [<DataRow("not00", "true")>]
    [<DataRow("not01", "not true")>]
    [<DataRow("not02", "¬true")>]
    [<DataRow("not03", "¬¬true")>]
    [<DataRow("not04", "¬¬¬true")>]
    [<DataRow("not05", "¬ ¬¬true")>]
    [<DataRow("not06", "¬ ¬ ¬true")>]
    [<DataRow("not07", "¬ ¬ ¬ true")>]
    [<DataRow("not02a", "¬(true)")>]
    [<DataRow("not03a", "¬¬(true)")>]
    [<DataRow("not04a", "¬(¬¬(true))")>]
    [<DataRow("not05a", "¬ (¬¬true)")>]
    [<DataRow("not06a", "¬ ¬ ¬(true)")>]
    [<DataRow("not07a", "¬ ¬ ¬ ( true)")>]
    [<TestMethod>]
    member this.TextExprNot(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        

    [<DataRow("00", "true")>]
    [<DataRow("01", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("02", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<TestMethod>]
    member this.TextExprConj(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<DataRow("00thm", "true")>]
    [<DataRow("01thm", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("02thm", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("00lem", "true")>]
    [<DataRow("01lem", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("02lem", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("00prop", "true")>]
    [<DataRow("01prop", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("02prop", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<TestMethod>]
    member this.TextExprTheoremLikeStmt(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<DataRow("00cor", "true")>]
    [<DataRow("01cor", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("02cor", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("00corthm", "true")>]
    [<DataRow("02corthm", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("00corlem", "true")>]
    [<DataRow("02corlem", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("00corprop", "true")>]
    [<DataRow("02corprop", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("00corax", "true")>]
    [<DataRow("02corax", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("00corconj", "true")>]
    [<DataRow("02corconj", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<TestMethod>]
    member this.TextExprCor(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00cl0", "undet")>]
    [<DataRow("00cl1", "A is A1")>] // one assertion
    [<DataRow("00cl2", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>] // two assertions
    [<DataRow("00cl3", "∃ x:obj {x is Nat}")>] // two assertions + predicative property
    [<DataRow("00pr0", "undet")>]
    [<DataRow("00pr0a", "∃ x:obj {x is Nat}")>]
    [<DataRow("00pr1", "A() is A1")>] // one assertion + predicate itself
    [<DataRow("00pr2", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>] // two assertions + predicate itself
    [<DataRow("00pr3", "∃ x:obj {x is Nat}")>] // two assertions + predicate itself + predicative property
    [<DataRow("00fu0", "undet")>]
    [<DataRow("00fu0a", "∃ x:obj {x is Nat}")>]
    [<DataRow("00fu1", "(A() -> ind) is A1")>] // one assertion 
    [<DataRow("00fu2", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>] // two assertions 
    [<DataRow("00fu3", "∃ x:obj {x is Nat}")>] // two assertions + predicative property
    [<TestMethod>]
    member this.TextExprDef(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00cl0", "undet")>]
    [<DataRow("00cl1", "A is A1")>] // one assertion
    [<DataRow("00cl2", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>] // two assertions
    [<DataRow("00cl3", "∃ x:obj {x is Nat}")>] // two assertions + predicative property
    [<DataRow("00pr0", "undet")>]
    [<DataRow("00pr0a", "∃ x:obj {x is Nat}")>]
    [<DataRow("00pr1", "A() is A1")>] // one assertion + predicate itself
    [<DataRow("00pr2", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>] // two assertions + predicate itself
    [<DataRow("00pr3", "∃ x:obj {x is Nat}")>] // two assertions + predicate itself + predicative property
    [<DataRow("00fu0", "undet")>]
    [<DataRow("00fu0a", "∃ x:obj {x is Nat}")>]
    [<DataRow("00fu1", "(A() -> ind) is A1")>] // one assertion 
    [<DataRow("00fu2", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>] // two assertions 
    [<DataRow("00fu3", "∃ x:obj {x is Nat}")>] // two assertions + predicative property
    [<TestMethod>]
    member this.TextExprDefVar(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00", "∃ x:obj {x is N}")>]
    [<DataRow("01", "∀ x, y:Set {(x is N) ⇒ (x = y)}")>]
    [<TestMethod>]
    member this.TextExprArgRef(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00", "∃ x:obj {x is N}")>]
    [<DataRow("01", "∀ x, y:Set {(x is N) ⇒ (x = y)}")>]
    [<TestMethod>]
    member this.TextExprProofArgument(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    // ModusPonens and (p, impl (p,q) )
    [<DataRow("MP_01", "false")>]
    [<DataRow("MP_01d", "false")>]
    [<DataRow("MP_01e", "false")>]
    [<DataRow("MP_01f", "false")>]
    [<DataRow("MP_01g", "false")>]
    [<DataRow("MP_01h", "false")>]
    [<DataRow("MP_01j", "true ⩡ false")>]
    [<DataRow("MP_01k", "false")>]
    [<DataRow("MP_01m", "false")>]

    // AndCummutative and(p,q) 
    [<DataRow("AndC_01", "false ∧ true")>]
    [<DataRow("AndC_03", "¬∀ x:obj {x is N} ∧ ∀ x:obj {x is N}")>]

    // OrCummutative or(p,q) 
    [<DataRow("OrC_01", "false ∨ true")>]
    [<DataRow("OrC_03", "(true ⇔ false) ∨ ∃ x:obj {x is N}")>]

    // IifCummutative iif(p,q)
    [<DataRow("IifC_01", "false ⇔ true")>]
    [<DataRow("IifC_03", "(true ⩡ false) ⇔ (true ⇔ ∃ x:obj {x is N})")>]

    // AndAssociative and(p,and(q,s)) 
    [<DataRow("AndA_01", "(true ∧ false) ∧ true")>]
    [<DataRow("AndA_02a", "(true ∧ true) ∧ false")>]
    [<DataRow("AndA_03", "(∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∧ true")>]

    // OrAssociative or(p,or(q,s))
    [<DataRow("OrA_01", "(true ∨ false) ∨ true")>]
    [<DataRow("OrA_03", "(∃ x:obj {x is N} ∨ (true ⇔ false)) ∨ true")>]

    // IifAssociative iif(p,iif(q,s))
    [<DataRow("IifA_01", "(true ⇔ false) ⇔ true")>]
    [<DataRow("IifA_03", "(∃ x:obj {x is N} ⇔ (true ⇔ false)) ⇔ ∃ y:obj {y is M}")>]

    // FalseAndAbsorbing and(false,p)
    [<DataRow("FAbs_01", "false")>]
    [<DataRow("FAbs_03", "false")>]

    // OrAndAbsorbing: pre: or(p, and(p, q))
    [<DataRow("OrAndAbsorbing_01", "true ∧ ¬false")>]
    [<DataRow("OrAndAbsorbing_02", "true ⇔ ¬false")>]
    [<DataRow("OrAndAbsorbing_03", "∀ x:obj {x is N}")>]

    // AndOrAbsorbing: pre: and(p, or(p, q))
    [<DataRow("AndOrAbsorbing_01", "true ⩡ false")>]
    [<DataRow("AndOrAbsorbing_02", "¬(true ∧ false)")>]
    [<DataRow("AndOrAbsorbing_03", "∃ n:obj {n is N}")>]

    // AndTrueNeutral: pre: and(true, p)
    [<DataRow("AndTrueNeutral_01", "(true ⩡ false) ∧ ¬false")>]
    [<DataRow("AndTrueNeutral_02", "∃ n:obj {n is N}")>]
    [<DataRow("AndTrueNeutral_03", "(true ∨ false) ⇔ ¬false")>]

    // OrFalseNeutral: pre: or(false, p)
    [<DataRow("OrFalseNeutral_01", "true ∧ ¬false")>]
    [<DataRow("OrFalseNeutral_02", "∃ n:obj {n is N}")>]
    [<DataRow("OrFalseNeutral_03", "true ⇔ (false ⩡ true)")>]

    // AndInversion: pre: and(p, not p)
    [<DataRow("AndInversion_01", "false")>]
    [<DataRow("AndInversion_02", "false")>]
    [<DataRow("AndInversion_03", "false")>]

    // OrInversion: pre: or(p, not p)
    [<DataRow("OrInversion_01", "true")>]
    [<DataRow("OrInversion_02", "true")>]
    [<DataRow("OrInversion_03", "true")>]

    // AndIdempotence: pre: and(p, p)
    [<DataRow("AndIdempotence_01", "true ∨ false")>]
    [<DataRow("AndIdempotence_02",   "∀ x:obj {x is N}")>]
    [<DataRow("AndIdempotence_03", "true ⇔ false")>]

    // OrIdempotence: pre: or(p, p)
    [<DataRow("OrIdempotence_01", "true ∧ false")>]
    [<DataRow("OrIdempotence_02", "∃ n:obj {n is N}")>]
    [<DataRow("OrIdempotence_03", "true ⩡ false")>]

    // OrAndDistributiveUnpack: pre: or(p, and(q, s))
    [<DataRow("OrAndDistributiveUnpack_01", "((true ⇔ false) ∨ (true ∧ ¬false)) ∧ ((true ⇔ false) ∨ (false ⩡ true))")>]
    [<DataRow("OrAndDistributiveUnpack_02", "(∀ x:obj {x is N} ∨ ∃ y:obj {y is M}) ∧ (∀ x:obj {x is N} ∨ (true ∨ false))")>]
    [<DataRow("OrAndDistributiveUnpack_03", "(¬(true ⩡ false) ∨ (true ⇔ true)) ∧ (¬(true ⩡ false) ∨ ∀ z:obj {z is N})")>]

    // AndOrDistributivePack: pre: and(or(p, q), or(p, s))
    [<DataRow("AndOrDistributivePack_01", "(true ⇔ false) ∨ ((true ⩡ false) ∧ (∃ n:obj {n is N} ∧ true))")>]
    [<DataRow("AndOrDistributivePack_02", "∀ x:obj {x is N} ∨ (¬false ∧ (true ⩡ false))")>]
    [<DataRow("AndOrDistributivePack_03", "¬true ∨ ((true ⇒ false) ∧ ∃ y:obj {y is N})")>]

    // AndOrDistributiveUnpack: pre: and(p, or(q, s))
    [<DataRow("AndOrDistributiveUnpack_01", "((true ⇔ ∃ x:obj {x is N}) ∧ (true ∧ ¬false)) ∨ ((true ⇔ ∃ x:obj {x is N}) ∧ (false ⩡ true))")>]
    [<DataRow("AndOrDistributiveUnpack_02", "(∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∨ (∀ x:obj {x is N} ∧ (true ⇒ false))")>]
    [<DataRow("AndOrDistributiveUnpack_03", "(¬(true ⩡ false) ∧ (true ⇔ false)) ∨ (¬(true ⩡ false) ∧ (∃! z:obj {z is N} ∧ true))")>]

    // OrAndDistributivePack: pre: or(and(p, q), and(p, s))
    [<DataRow("OrAndDistributivePack_01", "∀ x:obj {x is N} ∧ (¬false ∨ ∃ y:obj {y is M})")>]
    [<DataRow("OrAndDistributivePack_02", "(true ⇔ false) ∧ ((true ∧ ¬false) ∨ (true ⇒ false))")>]
    [<DataRow("OrAndDistributivePack_03", "∃ x:obj {x is N} ∧ ((true ∨ false) ∨ (true ⩡ false))")>]

    // DeMorganAndUnpack: pre: not and(p, q)
    [<DataRow("DeMorganAndUnpack_01", "¬¬true ∨ ¬(true ⩡ false)")>]
    [<DataRow("DeMorganAndUnpack_02", "¬∀ x:obj {x is N} ∨ ¬∃ y:obj {¬(y is M)}")>]
    [<DataRow("DeMorganAndUnpack_03", "¬(true ⇔ false) ∨ ¬(true ⇒ false)")>]

    // DeMorganOrPack: pre: or(not p, not q)
    [<DataRow("DeMorganOrPack_01", "¬((true ∧ false) ∧ ∃ x:obj {x is N})")>]
    [<DataRow("DeMorganOrPack_02", "¬((true ⇔ false) ∧ ∀ x:obj {x is N})")>]
    [<DataRow("DeMorganOrPack_03", "¬((true ⩡ false) ∧ (true ⇒ false))")>]

    // DeMorganOrUnpack: pre: not or(p, q)
    [<DataRow("DeMorganOrUnpack_01", "¬(true ⇔ false) ∧ ¬∃ x:obj {x is N}")>]
    [<DataRow("DeMorganOrUnpack_02", "¬∀ x:obj {x is N} ∧ ¬(true ⩡ false)")>]
    [<DataRow("DeMorganOrUnpack_03", "¬(true ⇒ false) ∧ ¬(true ∧ false)")>]

    // DeMorganAndPack: pre: and(not p, not q)
    [<DataRow("DeMorganAndPack_01", "¬((true ⇔ false) ∨ ∃ x:obj {x is N})")>]
    [<DataRow("DeMorganAndPack_02", "¬(∀ x:obj {x is N} ∨ (true ⩡ false))")>]
    [<DataRow("DeMorganAndPack_03", "¬((true ⇒ false) ∨ (true ∧ false))")>]

    // NotDouble: pre: not not p
    [<DataRow("NotDouble_01", "true ∧ (false ⩡ true)")>]
    [<DataRow("NotDouble_02", "∃ x:obj {¬(x is N)}")>]
    [<DataRow("NotDouble_03", "true ⇔ ∃ y:obj {y is M}")>]

    // ImplUnpack2Or: pre: impl(p, q)
    [<DataRow("ImplUnpack2Or_01", "¬∀ x:obj {x is N} ∨ ∃ y:obj {y is M}")>]
    [<DataRow("ImplUnpack2Or_02", "¬¬(true ⩡ false) ∨ (true ⇔ false)")>]
    [<DataRow("ImplUnpack2Or_03", "¬(true ∧ ¬false) ∨ (∃ x:obj {x is N} ∨ true)")>]

    // OrPack2Impl: pre: or(not p, q)
    [<DataRow("OrPack2Impl_01", "∀ x:obj {x is N} ⇒ ∃ y:obj {y is M}")>]
    [<DataRow("OrPack2Impl_02", "(true ⇔ false) ⇒ (true ∧ false)")>]
    [<DataRow("OrPack2Impl_03", "(true ⩡ false) ⇒ (true ⇒ false)")>]

    // IifUnpack2Or: pre: iif(p, q)
    [<DataRow("IifUnpack2Or_01", "(¬∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}) ∨ (∀ x:obj {x is N} ∧ ∃ y:obj {y is M})")>]
    [<DataRow("IifUnpack2Or_02", "(¬¬(true ⩡ false) ∧ ¬(true ∧ false)) ∨ (¬(true ⩡ false) ∧ (true ∧ false))")>]
    [<DataRow("IifUnpack2Or_03", "(¬(true ⇔ false) ∧ ¬(true ⩡ false)) ∨ ((true ⇔ false) ∧ (true ⩡ false))")>]

    // IifUnpack2And: pre: iif(p, q)
    [<DataRow("IifUnpack2And_01", "(¬∃ x:obj {x is N} ∨ (true ⇔ false)) ∧ (∃ x:obj {x is N} ∨ ¬(true ⇔ false))")>]
    [<DataRow("IifUnpack2And_02", "(¬∀ x:obj {x is N} ∨ (true ⩡ false)) ∧ (∀ x:obj {x is N} ∨ ¬(true ⩡ false))")>]
    [<DataRow("IifUnpack2And_03", "(¬¬(true ∧ false) ∨ (true ∨ false)) ∧ (¬(true ∧ false) ∨ ¬(true ∨ false))")>]

    // OrPack2Iif: pre: or(and(not p, not q), and(p, q))
    [<DataRow("OrPack2Iif_01", "∀ x:obj {x is N} ⇔ ∃ y:obj {y is M}")>]
    [<DataRow("OrPack2Iif_02", "(true ⇔ false) ⇔ (true ⩡ false)")>]
    [<DataRow("OrPack2Iif_03", "(true ⇒ false) ⇔ (true ∧ false)")>]

    // AndPack2Iif: pre: and(or(not p, q), or(p, not q))
    [<DataRow("AndPack2Iif_02", "(true ⇔ false) ⇔ (true ⩡ false)")>]
    [<DataRow("AndPack2Iif_03", "(true ⇒ false) ⇔ (true ∧ false)")>]

    // AndPack2Xor: pre: and(or(not p, not q), or(p, q))
    [<DataRow("AndPack2Xor_01", "∀ x:obj {x is N} ⩡ ∃ y:obj {y is M}")>]
    [<DataRow("AndPack2Xor_02", "(true ⇔ false) ⩡ (true ⩡ false)")>]
    [<DataRow("AndPack2Xor_03", "(true ⇒ false) ⩡ (true ∧ false)")>]

    // AndUnpack2NotOr: pre: and(p, q)
    [<DataRow("AndUnpack2NotOr_01", "¬(¬∀ x:obj {x is N} ∨ ¬∃ y:obj {y is M})")>]
    [<DataRow("AndUnpack2NotOr_02", "¬(¬(true ⇔ false) ∨ ¬(true ⩡ false))")>]
    [<DataRow("AndUnpack2NotOr_03", "¬(¬¬∃ x:obj {x is N} ∨ ¬(true ⇒ false))")>]

    // NotOrPack2And: pre: not (or(not p, not q))
    [<DataRow("NotOrPack2And_01", "∀ x:obj {x is N} ∧ ∃ y:obj {y is M}")>]
    [<DataRow("NotOrPack2And_02", "(true ⇔ false) ∧ (true ⩡ false)")>]
    [<DataRow("NotOrPack2And_03", "(true ⇒ false) ∧ (true ∧ false)")>]

    // AndUnpack2NotImpl: pre: and(p, q)
    [<DataRow("AndUnpack2NotImpl_01", "¬(∀ x:obj {x is N} ⇒ ¬(true ⇔ false))")>]
    [<DataRow("AndUnpack2NotImpl_02", "¬(∃ x:obj {x is M} ⇒ ¬¬(true ⩡ false))")>]
    [<DataRow("AndUnpack2NotImpl_03", "¬(¬(true ⇒ false) ⇒ ¬∀ z:obj {z is K})")>]

    // NotImplPack2And: pre: not (impl(p, not q))
    [<DataRow("NotImplPack2And_01", "∀ x:obj {x is N} ∧ ∃ y:obj {y is M}")>]
    [<DataRow("NotImplPack2And_02", "(true ⇔ false) ∧ (true ⩡ false)")>]
    [<DataRow("NotImplPack2And_03", "(true ∧ false) ∧ (true ⇔ false)")>]

    // NotImpl2And: pre: not (impl(p, q))
    [<DataRow("NotImpl2And_01", "∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}")>]
    [<DataRow("NotImpl2And_02", "(true ⇔ false) ∧ ¬(true ⩡ false)")>]
    [<DataRow("NotImpl2And_03", "(true ∧ false) ∧ ¬¬∃ x:obj {x is N}")>]

    // And2NotImpl: pre: and(p, not q)
    [<DataRow("And2NotImpl_01", "¬(∀ x:obj {x is N} ⇒ ∃ y:obj {y is M})")>]
    [<DataRow("And2NotImpl_02", "¬((true ⇔ false) ⇒ (true ⩡ false))")>]
    [<DataRow("And2NotImpl_03", "¬(¬(true ⇒ false) ⇒ ∀ z:obj {z is K})")>]

    // NotIif2Or: pre: not (iif(p, q))
    [<DataRow("NotIif2Or_01", "(∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}) ∨ (¬∀ x:obj {x is N} ∧ ∃ y:obj {y is M})")>]
    [<DataRow("NotIif2Or_02", "((true ⇔ false) ∧ ¬(true ⩡ false)) ∨ (¬(true ⇔ false) ∧ (true ⩡ false))")>]
    [<DataRow("NotIif2Or_03", "(¬(true ∧ false) ∧ ¬(true ⇒ false)) ∨ (¬¬(true ∧ false) ∧ (true ⇒ false))")>]

    // Or2NotIif: pre: or(and(p, not q), and(not p, q))
    [<DataRow("Or2NotIif_01", "¬(∀ x:obj {x is N} ⇔ ∃ y:obj {y is M})")>]
    [<DataRow("Or2NotIif_02", "¬((true ⇔ false) ⇔ (true ⩡ false))")>]
    [<DataRow("Or2NotIif_03", "¬((true ⇒ false) ⇔ ∀ z:obj {z is K})")>]

    // NotXor2Or: pre: not (xor(p, q))
    [<DataRow("NotXor2Or_01", "(∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∨ (¬∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M})")>]
    [<DataRow("NotXor2Or_02", "((true ⇔ false) ∧ (true ∧ false)) ∨ (¬(true ⇔ false) ∧ ¬(true ∧ false))")>]
    [<DataRow("NotXor2Or_03", "(¬(true ⇒ false) ∧ (true ⩡ false)) ∨ (¬¬(true ⇒ false) ∧ ¬(true ⩡ false))")>]

    // Or2NotXor: pre: or(and(p, q), and(not p, not q))
    [<DataRow("Or2NotXor_01", "¬(∀ x:obj {x is N} ⩡ ∃ y:obj {y is M})")>]
    [<DataRow("Or2NotXor_02", "¬((true ⇔ false) ⩡ (true ⩡ false))")>]
    [<DataRow("Or2NotXor_03", "¬((true ⇒ false) ⩡ (true ∧ false))")>]

    // NotAll2ExNot: pre: not all x1:tpl {p(x1)}
    [<DataRow("NotAll2ExNot_01", "∃ x1:obj {¬(x1 is N)}")>]
    [<DataRow("NotAll2ExNot_02", "∃ n:Nat {¬((n is N) ⇒ (true ⩡ false))}")>]
    [<DataRow("NotAll2ExNot_03", "∃ y:obj {¬¬(y is M)}")>]

    // ExNot2NotAll: pre: ex x:tpl{not p(x)}
    [<DataRow("ExNot2NotAll_01", "¬∀ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("ExNot2NotAll_02", "¬∀ n:Nat {(n is N) ∧ (true ⩡ false)}")>]

    // NotEx2AllNot: pre: not ex x:tpl{p(x)}
    [<DataRow("NotEx2AllNot_01", "∀ x:obj {¬((x is N) ⇔ false)}")>]
    [<DataRow("NotEx2AllNot_02", "∀ n:Nat {¬(¬(n is N) ∧ (true ⩡ false))}")>]

    // AllNot2ExNot: pre: all x:tpl{not p(x)}
    [<DataRow("AllNot2ExNot_01", "¬∃ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("AllNot2ExNot_02", "¬∃ n:Nat {(n is N) ∧ (true ⩡ false)}")>]

    // ImplPack2Or: pre: impl(not p,q)
    [<DataRow("ImplPack2Or_01", "((A is N) ∧ true) ∨ ∃ x:obj {x is M}")>]
    [<DataRow("ImplPack2Or_02", "(true ⩡ false) ∨ (∀ x:obj {x is N} ⇔ false)")>]
    [<DataRow("ImplPack2Or_03", "(true ⇔ false) ∨ (∃ y:obj {y is M} ∨ true)")>]

    // ModusTollens: pre: not q, impl(p,q)
    [<DataRow("ModusTollens_01", "¬∀ x:obj {x is N}")>]
    [<DataRow("ModusTollens_02a", "¬(true ⩡ false)")>]
    [<DataRow("ModusTollens_03a", "¬(true ⇔ false)")>]

    // HypotheticalSyllogism: pre: impl(p,q), impl(q,s)
    [<DataRow("HypotheticalSyllogism_01", "∀ x:obj {x is N} ⇒ (true ⩡ false)")>]
    [<DataRow("HypotheticalSyllogism_02a", "(true ⇔ false) ⇒ ∀ u:obj {u is K}")>]
    [<DataRow("HypotheticalSyllogism_03a", "(true ⇔ false) ⇒ (true ⇔ false)")>]

    // DisjunctiveSyllogism: pre: not p, or(p,q)
    [<DataRow("DisjunctiveSyllogism_01a", "∃ y:obj {y is M}")>]
    [<DataRow("DisjunctiveSyllogism_02", "true ⩡ false")>]
    [<DataRow("DisjunctiveSyllogism_03a", "true ⇒ false")>]

    // ExistsByExample: pre: p(c)
    [<DataRow("ExistsByExample_01", "∃ a:obj {(a is N) ⇔ true}")>]
    [<DataRow("ExistsByExample_02", "∃ x:tpl {∃ x:obj {x is M} ∧ (true ⇔ false)}")>]
    [<DataRow("ExistsByExample_02a", "∃ x:tpl {(a is M) ∧ =(a, $1)}")>]
    [<DataRow("ExistsByExample_03", "∃ x:tpl {∀ z:obj {z is K} ⩡ ¬(true ⩡ false)}")>]

    // Contraposition: pre: impl(not p, not q)
    [<DataRow("Contraposition_01", "∃ y:obj {y is M} ⇒ ∀ x:obj {x is N}")>]
    [<DataRow("Contraposition_02", "(true ⩡ false) ⇒ (true ⇔ false)")>]
    [<DataRow("Contraposition_03", "(true ⇔ false) ⇒ ((A is N) ∧ true)")>]

    // OrPack2Xor: pre: or(and(not p, q), and(p, not q))
    [<DataRow("OrPack2Xor_02", "(true ⇔ false) ⩡ (true ⩡ false)")>]
    [<DataRow("OrPack2Xor_03", "(true ⇒ false) ⩡ (true ∧ false)")>]

    // OrUnpack2Impl: pre: or(p,q)
    [<DataRow("OrUnpack2Impl_01", "¬((A is N) ⇔ true) ⇒ ∃ x:obj {x is M}")>]
    [<DataRow("OrUnpack2Impl_02", "¬∀ x:obj {x is N} ⇒ (true ∧ ¬false)")>]
    [<DataRow("OrUnpack2Impl_03", "¬(true ⩡ false) ⇒ (∀ z:obj {z is N} ⇒ false)")>]

    // PrenexPackAllAnd: pre: all x:tpl{and(p, q(x))}
    [<DataRow("PrenexPackAllAnd_01", "(true ⇔ false) ∧ ∀ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackAllAnd_02", "∃ y:obj {y is K} ∧ ∀ n:Nat {(n is K) ⩡ true}")>]
    [<DataRow("PrenexPackAllAnd_03", "¬(true ⩡ false) ∧ ∀ z:obj {(z is N) ⇒ false}")>]

    // PrenexPackAllIif: pre: all x:tpl{iif(p, q(x))}
    [<DataRow("PrenexPackAllIif_01", "(true ⇔ false) ⇔ ∀ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackAllIif_02", "∀ y:obj {y is M} ⇔ ∀ n:Nat {(n is K) ⩡ true}")>]
    [<DataRow("PrenexPackAllIif_03", "¬(true ⩡ false) ⇔ ∀ z:obj {(z is N) ⇒ false}")>]

    // PrenexPackAllImpl: pre: all x:tpl{impl(p, q(x))}
    [<DataRow("PrenexPackAllImpl_01", "(true ⇔ false) ⇒ ∀ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackAllImpl_02", "∀ y:obj {y is M} ⇒ ∀ n:Nat {(n is K) ⩡ true}")>]
    [<DataRow("PrenexPackAllImpl_03", "¬(true ⩡ false) ⇒ ∀ z:obj {(z is N) ⇒ false}")>]

    // PrenexPackAllOr: pre: all x:tpl{or(p, q(x))}
    [<DataRow("PrenexPackAllOr_01", "(true ⇔ false) ∨ ∀ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackAllOr_02", "∃ y:obj {y is M} ∨ ∀ n:Nat {(n is K) ⩡ true}")>]
    [<DataRow("PrenexPackAllOr_03", "¬(true ⩡ false) ∨ ∀ z:obj {(z is N) ⇒ false}")>]

    // PrenexPackExAnd: pre: ex x:tpl{and(p, q(x))}
    [<DataRow("PrenexPackExAnd_01", "(true ⇔ false) ∧ ∃ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackExAnd_02", "∀ y:obj {y is K} ∧ ∃ n:Nat {(n is M) ⩡ true}")>]
    [<DataRow("PrenexPackExAnd_03", "¬(true ⩡ false) ∧ ∃ z:obj {(z is N) ⇒ false}")>]

    // PrenexPackExIif: pre: ex x:tpl{iif(p, q(x))}
    [<DataRow("PrenexPackExIif_01", "(true ⇔ false) ⇔ ∃ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackExIif_02", "∀ y:obj {y is M} ⇔ ∃ n:Nat {(n is K) ⩡ true}")>]
    [<DataRow("PrenexPackExIif_03", "¬(true ⩡ false) ⇔ ∃ z:obj {(z is N) ⇒ false}")>]

    // PrenexPackExImpl: pre: ex x:tpl{impl(p, q(x))}
    [<DataRow("PrenexPackExImpl_01", "(true ⇔ false) ⇒ ∃ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackExImpl_02", "∀ y:obj {y is M} ⇒ ∃ n:Nat {(n is K) ⩡ true}")>]
    [<DataRow("PrenexPackExImpl_03", "¬(true ⩡ false) ⇒ ∃ z:obj {(z is N) ⇒ false}")>]

    // PrenexPackExOr: pre: ex x:tpl{or(p, q(x))}
    [<DataRow("PrenexPackExOr_01", "(true ⇔ false) ∨ ∃ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackExOr_02", "∀ y:obj {y is M} ∨ ∃ n:Nat {(n is K) ⩡ true}")>]
    [<DataRow("PrenexPackExOr_03", "¬(true ⩡ false) ∨ ∃ z:obj {(z is N) ⇒ false}")>]

    // PrenexPackExXor: pre: ex x:tpl{xor(p, q(x))}
    [<DataRow("PrenexPackExXor_01", "(true ⇔ false) ⩡ ∃ x:obj {(x is N) ⇔ true}")>]
    [<DataRow("PrenexPackExXor_02", "∀ y:obj {y is M} ⩡ ∃ n:Nat {(n is K) ⩡ true}")>]
    [<DataRow("PrenexPackExXor_03", "¬(true ⩡ false) ⩡ ∃ z:obj {(z is N) ⇒ false}")>]

    // PrenexUnpackAndAll: pre: and(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackAndAll_01", "∀ n:Nat {(true ⇔ false) ∧ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackAndAll_02", "∀ z:obj {∃ y:obj {y is M} ∧ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackAndAll_03", "∀ x:obj {¬(true ⩡ false) ∧ ((x is N) ⇒ false)}")>]

    // PrenexUnpackAndEx: pre: and(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackAndEx_01", "∃ n:Nat {(true ⇔ false) ∧ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackAndEx_02", "∃ y:obj {∀ z:obj {z is K} ∧ ((y is M) ⩡ true)}")>]
    [<DataRow("PrenexUnpackAndEx_03", "∃ x:obj {¬(true ⩡ false) ∧ ((x is N) ⇒ false)}")>]

    // PrenexUnpackIifAll: pre: iif(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackIifAll_01", "∀ n:Nat {(true ⇔ false) ⇔ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackIifAll_02", "∀ z:obj {∀ y:obj {y is M} ⇔ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackIifAll_03", "∀ x:obj {¬(true ⩡ false) ⇔ ((x is N) ⇒ false)}")>]

    // PrenexUnpackIifEx: pre: iif(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackIifEx_01", "∃ n:Nat {(true ⇔ false) ⇔ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackIifEx_02", "∃ z:obj {∀ y:obj {y is M} ⇔ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackIifEx_03", "∃ x:obj {¬(true ⩡ false) ⇔ ((x is N) ⇒ false)}")>]

    // PrenexUnpackImplAll: pre: impl(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackImplAll_01", "∀ n:Nat {(true ⇔ false) ⇒ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackImplAll_02", "∀ z:obj {∀ y:obj {y is M} ⇒ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackImplAll_03", "∀ x:obj {¬(true ⩡ false) ⇒ ((x is N) ⇒ false)}")>]

    // PrenexUnpackImplEx: pre: impl(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackImplEx_01", "∃ n:Nat {(true ⇔ false) ⇒ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackImplEx_02", "∃ z:obj {∀ y:obj {y is M} ⇒ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackImplEx_03", "∃ x:obj {¬(true ⩡ false) ⇒ ((x is N) ⇒ false)}")>]

    // PrenexUnpackOrAll: pre: or(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackOrAll_01", "∀ n:Nat {(true ⇔ false) ∨ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackOrAll_02", "∀ z:obj {∃ y:obj {y is M} ∨ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackOrAll_03", "∀ x:obj {¬(true ⩡ false) ∨ ((x is N) ⇒ false)}")>]

    // PrenexUnpackOrEx: pre: or(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackOrEx_01", "∃ n:Nat {(true ⇔ false) ∨ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackOrEx_02", "∃ z:obj {∀ y:obj {y is M} ∨ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackOrEx_03", "∃ x:obj {¬(true ⩡ false) ∨ ((x is N) ⇒ false)}")>]

    // PrenexUnpackXorAll: pre: xor(p, all x:tpl{q(x)})
    [<DataRow("PrenexUnpackXorAll_01", "∀ n:Nat {(true ⇔ false) ⩡ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackXorAll_02", "∀ z:obj {∀ y:obj {y is M} ⩡ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackXorAll_03", "∀ x:obj {¬(true ⩡ false) ⩡ ((x is N) ⇒ false)}")>]

    // PrenexUnpackXorEx: pre: xor(p, ex x:tpl{q(x)})
    [<DataRow("PrenexUnpackXorEx_01", "∃ n:Nat {(true ⇔ false) ⩡ ((n is N) ⇔ true)}")>]
    [<DataRow("PrenexUnpackXorEx_02", "∃ z:obj {∀ y:obj {y is M} ⩡ ((z is K) ⩡ true)}")>]
    [<DataRow("PrenexUnpackXorEx_03", "∃ x:obj {¬(true ⩡ false) ⩡ ((x is N) ⇒ false)}")>]

    // Proceeding2Results: pre: p, q
    [<DataRow("Proceeding2Results_01", "∀ x:obj {x is N} ∧ ∃ y:obj {y is M}")>]
    [<DataRow("Proceeding2Results_02", "(true ⇔ false) ∧ ¬(true ⩡ false)")>]
    [<DataRow("Proceeding2Results_03", "(true ∧ false) ∧ (true ⇒ false)")>]

    // Proceeding3Results: pre: p, q, s
    [<DataRow("Proceeding3Results_01", "(∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∧ (true ⇔ false)")>]
    [<DataRow("Proceeding3Results_02", "((true ∧ ¬false) ∧ (true ⩡ false)) ∧ ∀ z:obj {z is K}")>]
    [<DataRow("Proceeding3Results_03", "(¬(true ⩡ false) ∧ (true ⇒ false)) ∧ ∃! u:obj {u is L}")>]

    // TrueOrAbsorbing or(true,p)
    [<DataRow("TOrAbs_01", "true")>]
    [<DataRow("TOrAbs_03", "true")>]

    // WeakeningRule: pre: p
    [<DataRow("WeakeningRule_02", "q ⇒ ∀ x:obj {x is N}")>]
    [<DataRow("WeakeningRule_03", "q ⇒ (¬(true ⩡ false) ∧ (true ⇔ false))")>]

    // XorAssociative xor(p,xor(q,s))
    [<DataRow("XorA_01", "(true ⩡ false) ⩡ true")>]
    [<DataRow("XorA_03", "((true ⇔ ∃ x:obj {x is N}) ⩡ (true ⩡ false)) ⩡ true")>]

    // XorCummutative xor(p,q) 
    [<DataRow("XorC_01", "false ⩡ true")>]
    [<DataRow("XorC_03", "(true ⇒ false) ⩡ (∃ x:obj {x is N} ∧ true)")>]

    // XorUnpack2And: pre: xor(p, q)
    [<DataRow("XorUnpack2And_01", "(¬(∃ x:obj {x is N} ∧ true) ∨ ¬(true ⇔ false)) ∧ ((∃ x:obj {x is N} ∧ true) ∨ (true ⇔ false))")>]
    [<DataRow("XorUnpack2And_02", "(¬¬∀ x:obj {x is N} ∨ ¬(true ∨ false)) ∧ (¬∀ x:obj {x is N} ∨ (true ∨ false))")>]
    [<DataRow("XorUnpack2And_03", "(¬(true ⇔ true) ∨ ¬¬(false ⩡ true)) ∧ ((true ⇔ true) ∨ ¬(false ⩡ true))")>]

    // XorUnpack2Or: pre: xor(p, q)
    [<DataRow("XorUnpack2Or_01", "(¬∀ x:obj {x is N} ∧ ∃ y:obj {y is M}) ∨ (∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M})")>]
    [<DataRow("XorUnpack2Or_02", "(¬¬(true ⇔ false) ∧ (true ∧ false)) ∨ (¬(true ⇔ false) ∧ ¬(true ∧ false))")>]
    [<DataRow("XorUnpack2Or_03", "(¬(true ⇔ false) ∧ (true ⩡ false)) ∨ ((true ⇔ false) ∧ ¬(true ⩡ false))")>]
    [<TestMethod>]
    member this.TextExprInf(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))



