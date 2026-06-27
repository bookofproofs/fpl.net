namespace FplParser.Tests.UserFriendly
open FParsec
open Fpl.Parser.Grammar
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
    member this.TestExprNot(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<DataRow("is00", "n ∈ N")>]
    [<DataRow("is01", "¬¬n ∈ N")>]
    [<DataRow("is02", "x ∈ N ∈ X")>]
    [<DataRow("is03", "is(n,N)")>]
    [<DataRow("is04", "¬¬is(n, N)")>]
    [<DataRow("is05", "is(is(x , N), X)")>]
    [<DataRow("is06", "is(x ∈ N, X)")>]
    [<TestMethod>]
    member this.TestIsOperator(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("and00", "x ∧ y")>]
    [<DataRow("and01", "¬¬n ∧ x")>]
    [<DataRow("and02", "x ∧ y ∧ z")>]
    [<DataRow("and03", "∧(x,y)")>]
    [<DataRow("and04", "¬¬∧(x, y)")>]
    [<DataRow("and05", "∧(∧(x , y), z)")>]
    [<DataRow("and06", "∧(x ∧ y, z)")>]
    [<TestMethod>]
    member this.TestConjunction(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("or00", "x ∨ y")>]
    [<DataRow("or01", "¬¬n ∨ x")>]
    [<DataRow("or02", "x ∨ y ∨ z")>]
    [<DataRow("or03", "∨(x,y)")>]
    [<DataRow("or04", "¬¬∨(x, y)")>]
    [<DataRow("or05", "∨(∨(x , y), z)")>]
    [<DataRow("or06", "∨(x ∨ y, z)")>]
    [<TestMethod>]
    member this.TestDisjunction(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("impl00", "x ⇒ y")>]
    [<DataRow("impl01", "¬¬n ⇒ x")>]
    [<DataRow("impl02", "x ⇒ y ⇒ z")>]
    [<DataRow("impl03", "⇒(x,y)")>]
    [<DataRow("impl04", "¬¬⇒(x, y)")>]
    [<DataRow("impl05", "⇒(⇒(x , y), z)")>]
    [<DataRow("impl06", "⇒(x ⇒ y, z)")>]
    [<TestMethod>]
    member this.TestImplication(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("iif00", "x ⩡ y")>]
    [<DataRow("iif01", "¬¬n ⩡ x")>]
    [<DataRow("iif02", "x ⩡ y ⩡ z")>]
    [<DataRow("iif03", "⩡(x,y)")>]
    [<DataRow("iif04", "¬¬⩡(x, y)")>]
    [<DataRow("iif05", "⩡(⩡(x , y), z)")>]
    [<DataRow("iif06", "⩡(x ⩡ y, z)")>]
    [<TestMethod>]
    member this.TestExclusiveOr(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("xor00", "x ⇔ y")>]
    [<DataRow("xor01", "¬¬n ⇔ x")>]
    [<DataRow("xor02", "x ⇔ y ⇔ z")>]
    [<DataRow("xor03", "⇔(x,y)")>]
    [<DataRow("xor04", "¬¬⇔(x, y)")>]
    [<DataRow("xor05", "⇔(⇔(x , y), z)")>]
    [<DataRow("xor06", "⇔(x ⇔ y, z)")>]
    [<TestMethod>]
    member this.TestEquivalence(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("all00", "∀ m, n:Nat {((n') = (m'))}")>]
    [<DataRow("all01", "∀ m, n:Nat { true }")>]
    [<DataRow("all02", "∀ m:Nat { true }")>]
    [<DataRow("all03", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("all04", "∀ m, n:Nat {((n') = (m')) ⇒ (n = m)}")>]
    [<DataRow("all05", "∀ n:Nat {true ⇔ false ⩡ (n ∈ N ⇔ true)}")>]
    [<DataRow("all06", "∀ n:Nat {true ⇔ false ∧ (n ∈ N ⇔ true)}")>]
    [<DataRow("all07", "∀ n:Nat {true ⇔ false ∨ (n ∈ N ⇔ true)}")>]
    [<DataRow("all08", "∀ n:Nat {true ⇔ false ⇒ (n ∈ N ⇔ true)}")>]
    [<DataRow("all09", "∀ n:Nat {true ⇔ false ⇔ (n ∈ N ⇔ true)}")>]
    [<DataRow("all10", "∀ n:Nat {¬(¬n ∈ N ∧ true ⩡ false)}")>]
    [<DataRow("all11", "∀ x, y:Set {x ∈ N ⇒ (x = y)}")>]
    [<DataRow("all12", "∀ x:obj {¬(x ∈ N ⇔ false)}")>]
    [<DataRow("all13", "∀ x:obj {¬true ⩡ false ⩡ (x ∈ N ⇒ false)}")>]
    [<DataRow("all14", "∀ x:obj {¬true ⩡ false ∧ (x ∈ N ⇒ false)}")>]
    [<DataRow("all15", "∀ x:obj {¬true ⩡ false ∨ (x ∈ N ⇒ false)}")>]
    [<DataRow("all16", "∀ x:obj {¬true ⩡ false ⇒ (x ∈ N ⇒ false)}")>]
    [<DataRow("all17", "∀ x:obj {¬true ⩡ false ⇔ (x ∈ N ⇒ false)}")>]
    [<DataRow("all18", "∀ x:obj {x ∈ N} ⩡ ∃ y:obj {y ∈ M}")>]
    [<DataRow("all19", "∀ x:obj {x ∈ N} ∧ (¬false ∨ ∃ y:obj {y ∈ M})")>]
    [<DataRow("all20", "∀ x:obj {x ∈ N} ∧ ∃ y:obj {y ∈ M}")>]
    [<DataRow("all21", "∀ x:obj {x ∈ N} ∧ ¬∃ y:obj {y ∈ M}")>]
    [<DataRow("all22", "∀ x:obj {x ∈ N} ∨ (¬false ∧ true ⩡ false)")>]
    [<DataRow("all23", "∀ x:obj {x ∈ N} ⇒ true ⩡ false")>]
    [<DataRow("all24", "∀ x:obj {x ∈ N} ⇒ ∃ y:obj {y ∈ M}")>]
    [<DataRow("all25", "∀ x:obj {x ∈ N} ⇔ ∃ y:obj {y ∈ M}")>]
    [<DataRow("all26", "∀ x:obj {x ∈ N}")>]
    [<DataRow("all27", "∀ y:obj {y ∈ K} ∧ ∃ n:Nat {n ∈ M ⩡ true}")>]
    [<DataRow("all28", "∀ y:obj {y ∈ M} ⩡ ∃ n:Nat {n ∈ K ⩡ true}")>]
    [<DataRow("all29", "∀ y:obj {y ∈ M} ∨ ∃ n:Nat {n ∈ K ⩡ true}")>]
    [<DataRow("all30", "∀ y:obj {y ∈ M} ⇒ ∀ n:Nat {n ∈ K ⩡ true}")>]
    [<DataRow("all31", "∀ y:obj {y ∈ M} ⇒ ∃ n:Nat {n ∈ K ⩡ true}")>]
    [<DataRow("all32", "∀ y:obj {y ∈ M} ⇔ ∀ n:Nat {n ∈ K ⩡ true}")>]
    [<DataRow("all33", "∀ y:obj {y ∈ M} ⇔ ∃ n:Nat {n ∈ K ⩡ true}")>]
    [<DataRow("all34", "∀ z:obj {∀ y:obj {y ∈ M} ⩡ (z ∈ K ⩡ true)}")>]
    [<DataRow("all35", "∀ z:obj {∀ y:obj {y ∈ M} ⇒ (z ∈ K ⩡ true)}")>]
    [<DataRow("all36", "∀ z:obj {∀ y:obj {y ∈ M} ⇔ (z ∈ K ⩡ true)}")>]
    [<DataRow("all37", "∀ z:obj {∃ y:obj {y ∈ M} ∧ (z ∈ K ⩡ true)}")>]
    [<DataRow("all38", "∀ z:obj {∃ y:obj {y ∈ M} ∨ (z ∈ K ⩡ true)}")>]
    [<TestMethod>]
    member this.TestExprAll(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<DataRow("ex01", "∃ x:obj {(x ∈ Nat)}")>] 
    [<DataRow("ex02", "∃ m, n:Nat { true }")>]
    [<DataRow("ex03", "∃ x:obj {x ∈ N}")>]
    [<DataRow("ex04", "∃ a:obj {a ∈ N ⇔ true}")>]
    [<DataRow("ex05", "∃ n:Nat {true ⇔ false ⩡ (n ∈ N ⇔ true)}")>]
    [<DataRow("ex06", "∃ n:Nat {true ⇔ false ∧ (n ∈ N ⇔ true)}")>]
    [<DataRow("ex07", "∃ n:Nat {true ⇔ false ∨ (n ∈ N ⇔ true)}")>]
    [<DataRow("ex08", "∃ n:Nat {true ⇔ false ⇒ (n ∈ N ⇔ true)}")>]
    [<DataRow("ex09", "∃ n:Nat {true ⇔ false ⇔ (n ∈ N ⇔ true)}")>]
    [<DataRow("ex10", "∃ n:Nat {¬(n ∈ N ⇒ true ⩡ false)}")>]
    [<DataRow("ex11", "∃ n:obj {n ∈ N}")>]
    [<DataRow("ex12", "∃ x:obj {¬true ⩡ false ⩡ (x ∈ N ⇒ false)}")>]
    [<DataRow("ex13", "∃ x:obj {¬true ⩡ false ∧ (x ∈ N ⇒ false)}")>]
    [<DataRow("ex14", "∃ x:obj {¬true ⩡ false ∨ (x ∈ N ⇒ false)}")>]
    [<DataRow("ex15", "∃ x:obj {¬true ⩡ false ⇒ (x ∈ N ⇒ false)}")>]
    [<DataRow("ex16", "∃ x:obj {¬true ⩡ false ⇔ (x ∈ N ⇒ false)}")>]
    [<DataRow("ex17", "∃ x:obj {¬x ∈ N}")>]
    [<DataRow("ex18", "∃ x:obj {x ∈ N} ∧ (true ∨ false ∨ true ⩡ false)")>]
    [<DataRow("ex19", "∃ x:obj {x ∈ Nat}")>]
    [<DataRow("ex20", "∃ x:tpl {a ∈ M ∧ (a = $1)}")>]
    [<DataRow("ex21", "∃ x:tpl {∀ z:obj {z ∈ K} ⩡ ¬true ⩡ false}")>]
    [<DataRow("ex22", "∃ x:tpl {∃ x:obj {x ∈ M} ∧ true ⇔ false}")>]
    [<DataRow("ex23", "∃ x1:obj {¬(x1 ∈ N)}")>]
    [<DataRow("ex24", "∃ y:obj {∀ z:obj {z ∈ K} ∧ (y ∈ M ⩡ true)}")>]
    [<DataRow("ex25", "∃ y:obj {¬¬y ∈ M}")>]
    [<DataRow("ex26", "∃ y:obj {y ∈ K} ∧ ∀ n:Nat {n ∈ K ⩡ true}")>]
    [<DataRow("ex27", "∃ y:obj {y ∈ M} ∨ ∀ n:Nat {n ∈ K ⩡ true}")>]
    [<DataRow("ex28", "∃ y:obj {y ∈ M} ⇒ ∀ x:obj {x ∈ N}")>]
    [<DataRow("ex29", "∃ y:obj {y ∈ M}")>]
    [<DataRow("ex30", "∃ z:obj {∀ y:obj {y ∈ M} ⩡ (z ∈ K ⩡ true)}")>]
    [<DataRow("ex31", "∃ z:obj {∀ y:obj {y ∈ M} ∨ (z ∈ K ⩡ true)}")>]
    [<DataRow("ex32", "∃ z:obj {∀ y:obj {y ∈ M} ⇒ (z ∈ K ⩡ true)}")>]
    [<DataRow("ex33", "∃ z:obj {∀ y:obj {y ∈ M} ⇔ (z ∈ K ⩡ true)}")>]
    [<TestMethod>]
    member this.TestExprExists(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<DataRow("exn00", "∃! z:obj {true}")>]
    [<DataRow("exn01", "∃!0 z:obj {true}")>]
    [<DataRow("exn02", "∃!1 z:obj {true}")>]
    [<DataRow("exn03", "∃!2 z:obj {true}")>]
    [<TestMethod>]
    member this.TestExprExistsN(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("00", "(A ∈ N ∧ true) ∨ ∃ x:obj {x ∈ M}")>]
    [<DataRow("01", "(true ∧ ¬false ∧ true ⩡ false) ∧ ∀ z:obj {z ∈ K}")>]
    [<DataRow("02", "((true ⇔ ∃ x:obj {x ∈ N}) ⩡ true ⩡ false) ⩡ true")>]
    [<DataRow("03", "((true ⇔ ∃ x:obj {x ∈ N}) ∧ (true ∧ ¬false)) ∨ ((true ⇔ ∃ x:obj {x ∈ N}) ∧ (false ⩡ true))")>]
    [<DataRow("04", "(true ⇔ false ∧ true ∧ false) ∨ (¬true ⇔ false ∧ ¬true ∧ false)")>]
    [<DataRow("05", "(true ⇔ false ∧ ¬true ⩡ false) ∨ (¬true ⇔ false ∧ true ⩡ false)")>]
    [<DataRow("06", "(true ⇔ false ∨ (true ∧ ¬false)) ∧ (true ⇔ false ∨ (false ⩡ true))")>]
    [<DataRow("07", "(∀ x:obj {x ∈ N} ∧ ∃ y:obj {y ∈ M}) ∧ true ⇔ false")>]
    [<DataRow("08", "(∀ x:obj {x ∈ N} ∧ ∃ y:obj {y ∈ M}) ∧ true")>]
    [<DataRow("09", "(∀ x:obj {x ∈ N} ∧ ∃ y:obj {y ∈ M}) ∨ (∀ x:obj {x ∈ N} ∧ true ⇒ false)")>]
    [<DataRow("0a", "(∀ x:obj {x ∈ N} ∧ ∃ y:obj {y ∈ M}) ∨ (¬∀ x:obj {x ∈ N} ∧ ¬∃ y:obj {y ∈ M})")>]
    [<DataRow("0b", "(∀ x:obj {x ∈ N} ∧ ¬∃ y:obj {y ∈ M}) ∨ (¬∀ x:obj {x ∈ N} ∧ ∃ y:obj {y ∈ M})")>]
    [<DataRow("0c", "(∀ x:obj {x ∈ N} ∨ ∃ y:obj {y ∈ M}) ∧ (∀ x:obj {x ∈ N} ∨ true ∨ false)")>]
    [<DataRow("0d", "(∃ x:obj {x ∈ N} ∨ true ⇔ false) ∨ true")>]
    [<DataRow("0e", "(∃ x:obj {x ∈ N} ⇔ true ⇔ false) ⇔ ∃ y:obj {y ∈ M}")>]
    [<DataRow("0f", "(¬(∃ x:obj {x ∈ N} ∧ true) ∨ ¬(true ⇔ false)) ∧ ((∃ x:obj {x ∈ N} ∧ true) ∨ true ⇔ false)")>]
    [<DataRow("10", "(¬(true ⩡ false) ∧ true ⇒ false) ∧ ∃!3 u:obj {u ∈ L}")>]
    [<DataRow("11", "(¬(true ⩡ false) ∧ (true ⇔ false)) ∨ (¬true ⩡ false ∧ (∃! z:obj {z ∈ N} ∧ true))")>]
    [<DataRow("12", "(¬(true ⩡ false) ∨ (true ⇔ true)) ∧ (¬true ⩡ false ∨ ∀ z:obj {z ∈ N})")>]
    [<DataRow("13", "(¬(true ∧ false) ∧ ¬(true ⇒ false)) ∨ (¬¬(true ∧ false) ∧ true ⇒ false)")>]
    [<DataRow("14", "(¬(true ⇒ false) ∧ (true ⩡ false)) ∨ (¬¬(true ⇒ false) ∧ ¬true ⩡ false)")>]
    [<DataRow("15", "(¬(true ⇔ false) ∧ (true ⩡ false)) ∨ ((true ⇔ false) ∧ ¬true ⩡ false)")>]
    [<DataRow("16", "(¬(true ⇔ false) ∧ ¬(true ⩡ false)) ∨ ((true ⇔ false) ∧ true ⩡ false)")>]
    [<DataRow("17", "(¬(true ⇔ true) ∨ ¬¬(false ⩡ true)) ∧ ((true ⇔ true) ∨ ¬(false ⩡ true))")>]
    [<DataRow("18", "(¬∀ x:obj {x ∈ N} ∧ ∃ y:obj {y ∈ M}) ∨ (∀ x:obj {x ∈ N} ∧ ¬∃ y:obj {y ∈ M})")>]
    [<DataRow("19", "(¬∀ x:obj {x ∈ N} ∧ ¬∃ y:obj {y ∈ M}) ∨ (∀ x:obj {x ∈ N} ∧ ∃ y:obj {y ∈ M})")>]
    [<DataRow("1a", "(¬∀ x:obj {x ∈ N} ∨ (true ⩡ false)) ∧ (∀ x:obj {x ∈ N} ∨ ¬(true ⩡ false))")>]
    [<DataRow("1b", "(¬∃ x:obj {x ∈ N} ∨ (true ⇔ false)) ∧ (∃ x:obj {x ∈ N} ∨ ¬true ⇔ false)")>]
    [<DataRow("1c", "(¬¬(true ⩡ false) ∧ ¬(true ∧ false)) ∨ (¬(true ⩡ false) ∧ true ∧ false)")>]
    [<DataRow("1d", "(¬¬(true ∧ false) ∨ (true ∨ false)) ∧ (¬(true ∧ false) ∨ ¬true ∨ false)")>]
    [<DataRow("1e", "(¬¬(true ⇔ false) ∧ (true ∧ false)) ∨ (¬(true ⇔ false) ∧ ¬true ∧ false)")>]
    [<DataRow("1f", "(¬¬∀ x:obj {x ∈ N} ∨ ¬(true ∨ false)) ∧ (¬∀ x:obj {x ∈ N} ∨ true ∨ false)")>]
    [<DataRow("20", "A() ∈ A1")>]
    [<DataRow("21", "true ⩡ false ⩡ true")>]
    [<DataRow("22", "true ⩡ false ∧ ¬false")>]
    [<DataRow("23", "(true ⩡ false) ∨ (∀ x:obj {x ∈ N} ⇔ false)")>]
    [<DataRow("24", "true ⩡ false ⇒ true ⇒ false")>]
    [<DataRow("25", "true ⩡ false ⇒ true ⇔ false")>]
    [<DataRow("26", "true ⩡ false ⇔ (true ⇔ ∃ x:obj {x ∈ N})")>]
    [<DataRow("27", "true ∧ false ∧ true ⇒ false")>]
    [<DataRow("28", "true ∧ false ∧ true ⇔ false")>]
    [<DataRow("29", "true ∧ false ∧ ¬¬∃ x:obj {x ∈ N}")>]
    [<DataRow("2a", "true ∧ false ∧ true")>]
    [<DataRow("2b", "(true ∧ true) ∧ false")>]
    [<DataRow("2c", "true ∨ false ∨ true")>]
    [<DataRow("2d", "true ∨ false ⇔ ¬false")>]
    [<DataRow("2e", "true ⇒ false ⩡ (∃ x:obj {x ∈ N} ∧ true)")>]
    [<DataRow("2f", "true ⇒ false ⩡ true ∧ false")>]
    [<DataRow("30", "true ⇒ false ∧ true ∧ false")>]
    [<DataRow("31", "true ⇒ false ⇔ true ∧ false")>]
    [<DataRow("32", "true ⇔ false ⩡ true ⩡ false")>]
    [<DataRow("33", "true ⇔ false ⩡ ∃ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("34", "(true ⇔ false) ∧ ((true ∧ ¬false) ∨ true ⇒ false)")>]
    [<DataRow("35", "true ⇔ false ∧ true ⩡ false")>]
    [<DataRow("36", "true ⇔ false ∧ ∀ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("37", "true ⇔ false ∧ ∃ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("38", "true ⇔ false ∧ ¬true ⩡ false")>]
    [<DataRow("39", "true ⇔ false ∨ (true ⩡ false ∧ (∃ n:obj {n ∈ N} ∧ true))")>]
    [<DataRow("3a", "true ⇔ false ∨ (∃ y:obj {y ∈ M} ∨ true)")>]
    [<DataRow("3b", "true ⇔ false ∨ ∀ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("3c", "true ⇔ false ∨ ∃ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("3d", "true ⇔ false ∨ ∃ x:obj {x ∈ N}")>]
    [<DataRow("3e", "true ⇔ false ⇒ (A ∈ N ∧ true)")>]
    [<DataRow("3f", "true ⇔ false ⇒ true ∧ false")>]
    [<DataRow("40", "true ⇔ false ⇒ true ⇔ false")>]
    [<DataRow("41", "true ⇔ false ⇒ ∀ u:obj {u ∈ K}")>]
    [<DataRow("42", "true ⇔ false ⇒ ∀ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("43", "true ⇔ false ⇒ ∃ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("44", "(true ⇔ false) ⇔ true ⩡ false")>]
    [<DataRow("45", "true ⇔ false ⇔ ∀ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("46", "true ⇔ false ⇔ ∃ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("47", "true ⇔ false ⇔ true")>]
    [<DataRow("48", "¬(A ∈ N ⇔ true) ⇒ ∃ x:obj {x ∈ M}")>]
    [<DataRow("49", "¬((true ⩡ false) ∧ true ⇒ false)")>]
    [<DataRow("4a", "¬(true ∧ false ∧ ∃ x:obj {x ∈ N})")>]
    [<DataRow("4b", "¬((true ⇒ false) ⩡ true ∧ false)")>]
    [<DataRow("4c", "¬((true ⇒ false) ∨ true ∧ false)")>]
    [<DataRow("4d", "¬(true ⇒ false ⇔ ∀ z:obj {z ∈ K})")>]
    [<DataRow("4e", "¬((true ⇔ false) ⩡ true ⩡ false)")>]
    [<DataRow("4f", "¬(true ⇔ false ∧ ∀ x:obj {x ∈ N})")>]
    [<DataRow("50", "¬(true ⇔ false ∨ ∃ x:obj {x ∈ N})")>]
    [<DataRow("51", "¬((true ⇔ false) ⇒ true ⩡ false)")>]
    [<DataRow("52", "¬((true ⇔ false) ⇔ true ⩡ false)")>]
    [<DataRow("53", "¬(∀ x:obj {x ∈ N} ⩡ ∃ y:obj {y ∈ M})")>]
    [<DataRow("54", "¬(∀ x:obj {x ∈ N} ∨ true ⩡ false)")>]
    [<DataRow("54", "¬(∀ x:obj {x ∈ N} ⇒ ∃ y:obj {y ∈ M})")>]
    [<DataRow("56", "¬(∀ x:obj {x ∈ N} ⇒ ¬true ⇔ false)")>]
    [<DataRow("57", "¬(∀ x:obj {x ∈ N} ⇔ ∃ y:obj {y ∈ M})")>]
    [<DataRow("58", "¬(∃ x:obj {x ∈ M} ⇒ ¬¬true ⩡ false)")>]
    [<DataRow("59", "¬(¬true ⇒ false ⇒ ∀ z:obj {z ∈ K})")>]
    [<DataRow("5a", "¬(¬true ⇒ false ⇒ ¬∀ z:obj {z ∈ K})")>]
    [<DataRow("5b", "¬(¬(true ⇔ false) ∨ ¬true ⩡ false)")>]
    [<DataRow("5c", "¬(¬∀ x:obj {x ∈ N} ∨ ¬∃ y:obj {y ∈ M})")>]
    [<DataRow("5d", "¬(¬¬∃ x:obj {x ∈ N} ∨ ¬true ⇒ false)")>]
    [<DataRow("5e", "¬true ⩡ false ⩡ ∃ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("5f", "¬true ⩡ false ∧ ∀ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("60", "¬true ⩡ false ∧ ∃ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("61", "¬true ⩡ false ∨ ∀ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("62", "¬true ⩡ false ∨ ∃ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("63", "¬(true ⩡ false) ⇒ (∀ z:obj {z ∈ N} ⇒ false)")>]
    [<DataRow("64", "¬true ⩡ false ⇒ ∀ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("65", "¬true ⩡ false ⇒ ∃ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("66", "¬true ⩡ false ⇔ ∀ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("67", "¬true ⩡ false ⇔ ∃ z:obj {z ∈ N ⇒ false}")>]
    [<DataRow("68", "¬true ⩡ false")>]
    [<DataRow("69", "¬(true ∧ ¬false) ∨ (∃ x:obj {x ∈ N} ∨ true)")>]
    [<DataRow("6a", "¬true ∧ false")>]
    [<DataRow("6b", "¬(true ⇒ false) ∧ ¬true ∧ false")>]
    [<DataRow("6c", "¬true ⇔ false ∧ ¬∃ x:obj {x ∈ N}")>]
    [<DataRow("6d", "¬(true ⇔ false) ∨ ¬true ⇒ false")>]
    [<DataRow("6e", "¬true ⇔ false")>]
    [<DataRow("6f", "¬∀ n:Nat {n ∈ N ∧ true ⩡ false}")>]
    [<DataRow("70", "¬∀ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("71", "¬∀ x:obj {x ∈ N} ∧ ∀ x:obj {x ∈ N}")>]
    [<DataRow("72", "¬∀ x:obj {x ∈ N} ∧ ¬true ⩡ false")>]
    [<DataRow("73", "¬∀ x:obj {x ∈ N} ∨ ∃ y:obj {y ∈ M}")>]
    [<DataRow("74", "¬∀ x:obj {x ∈ N} ∨ ¬∃ y:obj {¬y ∈ M}")>]
    [<DataRow("75", "¬∀ x:obj {x ∈ N} ⇒ (true ∧ ¬false)")>]
    [<DataRow("76", "¬∀ x:obj {x ∈ N}")>]
    [<DataRow("77", "¬∃ n:Nat {n ∈ N ∧ true ⩡ false}")>]
    [<DataRow("78", "¬∃ x:obj {x ∈ N ⇔ true}")>]
    [<DataRow("79", "¬¬(true ⩡ false) ∨ true ⇔ false")>]
    [<DataRow("7a", "¬¬true ∨ ¬true ⩡ false")>]
    [<DataRow("7b", "¬true ∨ (true ⇒ false ∧ ∃ y:obj {y ∈ N})")>]
    [<DataRow("7c", "A ∈ A1")>]
    [<DataRow("7d", "A() ∈ A1")>]
    [<DataRow("7e", "false ⩡ true")>]
    [<DataRow("7f", "false ∧ true")>]
    [<DataRow("80", "false ∨ true")>]
    [<DataRow("81", "false ⇔ true")>]
    [<DataRow("82", "false")>]
    [<DataRow("83", "q ⇒ (¬(true ⩡ false) ∧ true ⇔ false)")>]
    [<DataRow("84", "q ⇒ ∀ x:obj {x ∈ N}")>]
    [<DataRow("85", "true ⩡ false")>]
    [<DataRow("86", "true ∧ false ⩡ true")>]
    [<DataRow("86_", "true ∧ (false ⩡ true)")>]
    [<DataRow("87", "true ∧ ¬false")>]
    [<DataRow("88", "true ∧ false")>]
    [<DataRow("89", "true ∨ false")>]
    [<DataRow("8a", "true ⇒ false")>]
    [<DataRow("8b", "true ⇔ false ⩡ true")>]
    [<DataRow("8b_", "true ⇔ false ⩡ true")>]
    [<DataRow("8c", "true ⇔ ∃ y:obj {y ∈ M}")>]
    [<DataRow("8d", "true ⇔ ¬false")>]
    [<DataRow("8e", "true ⇔ false")>]
    [<DataRow("8f", "true")>]
    [<TestMethod>]
    member this.TestExprMixed(no:string, fplCode) =
        let result = run (predicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
