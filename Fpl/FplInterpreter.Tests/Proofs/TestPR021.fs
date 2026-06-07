namespace FplInterpreter.Tests.Proofs

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreter.Globals.Debug
open CommonTestHelpers


[<TestClass>]
type TestPR021() =


    [<DataRow("00", """ax A {true} thm T {true} prf T$1 { 1. byax A |- true }""", 0)>]
    [<DataRow("00a", """ax A {true} thm T {true} prf T$1 { 1. byax A |- false }""", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("01a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- true }""",  1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byax A |- true }""", 1)>]
    [<DataRow("03", """ax A {∃!1 x:obj{true}} thm T {true} prf T$1 { 1. byax A |- ∃! x:obj {true} }""", 0)>]
    [<DataRow("03a", """ax A {∃!1 x:obj{true}} thm T {true} prf T$1 { 1. byax A |- true }""", 1)>]
    [<DataRow("04", """ax A {∃! x:obj{true}} thm T {true} prf T$1 { 1. byax A |- ∃! x:obj {true} }""", 0)>]
    [<DataRow("04a", """ax A {∃! x:obj{true}} thm T {true} prf T$1 { 1. byax A |- true }""", 1)>]
    [<DataRow("05", """ax A {∃!3 x:obj{true}} thm T {true} prf T$1 { 1. byax A |- ∃!3 x:obj {true} }""", 0)>]
    [<DataRow("05a", """ax A {∃!3 x:obj{true}} thm T {true} prf T$1 { 1. byax A |- true }""", 1)>]
    [<TestMethod>]
    member this.TestPR021InferenceByAx(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "")
            runTestHelper "TestPR021.fpl" fplCode code expected


    // AndCummutative and(p,q) 
    [<DataRow("AndC_01", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- false ∧ true}", 0)>]
    [<DataRow("AndC_01a", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- false ∨ true}", 1)>]
    [<DataRow("AndC_03", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, not(all x:obj {is(x,N)})) 2. 1, byinf AndCummutative |- ¬(∀ x:obj {x is N}) ∧ ∀ x:obj {x is N}}", 0)>]
    [<DataRow("AndC_03a", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, not(all x:obj {is(x,N)})) 2. 1, byinf AndCummutative |- ¬(∀ x:obj {x is N}) ∧ ∀ x:obj {x is M}}", 1)>]
    [<TestMethod>]
    member this.TestPR021InferenceByInf(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "")
            runTestHelper "TestPR021.fpl" fplCode code expected

    

