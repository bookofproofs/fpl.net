namespace FplInterpreter.Tests.Diagnostics.Proofs

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(*
The PR021 diagnostic indicates that the given argument inference (part of code after the argument justification)
is different from the derived one (i.e., was inferred by the justification).
*)

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
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByAx.fpl" fplCode code expected


    [<DataRow("00", """conj A {true} thm T {true} prf T$1 { 1. byconj A |- true }""", 0)>]
    [<DataRow("00a", """conj A {true} thm T {true} prf T$1 { 1. byconj A |- false }""", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byconj A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("01a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byconj A |- true }""", 1)>]
    [<DataRow("02", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byconj A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""",  0)>]
    [<DataRow("02a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. byconj A |- true }""",  1)>]
    [<TestMethod>]
    member this.TestPR021InferenceByConj(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByConj.fpl" fplCode code expected


    [<DataRow("00thm", """thm A {true} thm T {true} prf T$1 { 1. A |- true }""",  0)>]
    [<DataRow("00thma", """thm A {true} thm T {true} prf T$1 { 1. A |- false }""",  1)>]
    [<DataRow("01thm", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("01thma", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- false }""", 1)>]
    [<DataRow("02thm", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02thma", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- false }""", 1)>]
    [<DataRow("00lem", """lem A {true} thm T {true} prf T$1 { 1. A |- true }""", 0)>]
    [<DataRow("00lema", """lem A {true} thm T {true} prf T$1 { 1. A |- false }""", 1)>]
    [<DataRow("01lem", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("01lema", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- false }""", 1)>]
    [<DataRow("02lem", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02lema", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- false }""", 1)>]
    [<DataRow("00prop", """prop A {true} thm T {true} prf T$1 { 1. A |- true }""", 0)>]
    [<DataRow("00propa", """prop A {true} thm T {true} prf T$1 { 1. A |- false }""",  1)>]
    [<DataRow("01prop", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("01propa", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- false }""", 1)>]
    [<DataRow("02prop", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02propa", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. A |- false }""", 1)>]
    [<TestMethod>]
    member this.TestPR021InferenceByTheoremLikeStmt(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByTheoremLikeStmt.fpl" fplCode code expected


    [<DataRow("00cor", """cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""",  0)>]
    [<DataRow("00cora", """cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- false }""",  1)>]
    [<DataRow("01cor", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" cor A$1 {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("01cora", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" cor A$1 {all n,m:Nat { impl( ( Succ(n) = Succ(m) ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("02cor", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02cora", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("00corthm", """thm A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", 0)>]
    [<DataRow("00corthma", """thm A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("02corthm", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02corthma", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" thm A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} thm T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("00corlem", """lem A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", 0)>]
    [<DataRow("00corlema", """lem A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("02corlem", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} lem T {true} prf T$1 { 1. bycor A$1 |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02corlema", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" lem A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} lem T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("00corprop", """prop A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", 0)>]
    [<DataRow("00corpropa", """prop A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("02corprop", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02corpropa", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prop A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("00corax", """ax A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", 0)>]
    [<DataRow("00coraxa", """ax A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("02corax", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02coraxa", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" ax A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("00corconj", """conj A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""", 0)>]
    [<DataRow("00corconja", """conj A {true} cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<DataRow("02corconj", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>]
    [<DataRow("02corconja", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" conj A {true} cor A$1 {all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }} prop T {true} prf T$1 { 1. bycor A$1 |- false }""", 1)>]
    [<TestMethod>]
    member this.TestPR021InferenceByCor(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByCor.fpl" fplCode code expected

    [<DataRow("cl0_a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- A is A1 }""", 0)>] // two assertions + predicative property
    [<DataRow("cl0_b", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions + predicative property
    [<DataRow("cl0_c", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicative property
    [<DataRow("cl0a", """def cl A thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>]
    [<DataRow("cl1", """def cl A1 def cl A { dec assert is(self, A1); ctor A() {} } thm T {true} prf T$1 { 1. bydef A |- A is A1 }""", 0)>] // one assertion
    [<DataRow("cl1a", """def cl A1 def cl A { dec assert is(self, A1); ctor A() {} } thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // one assertion
    [<DataRow("cl2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} } thm T {true} prf T$1 { 1. bydef A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions
    [<DataRow("cl2a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} } thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // two assertions
    [<DataRow("cl3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicative property
    [<DataRow("cl3a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // two assertions + predicative property

    [<DataRow("pr0_a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- A is A1 }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr0_b", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr9_c", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- true }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr0_d", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr0a", """def pred A() thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>]
    [<DataRow("pr0a", """def cl Nat def pred A() {ex x:obj {is(x,Nat)}} thm T {true} prf T$1 { 1. bydef A |- ∃ x:obj {x is Nat} }""", 0)>]
    [<DataRow("pr0aa", """def cl Nat def pred A() {ex x:obj {is(x,Nat)}} thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>]
    [<DataRow("pr1", """def cl A1 def pred A() { dec assert is(self, A1); true} thm T {true} prf T$1 { 1. bydef A |- A() is A1 }""", 0)>] // one assertion + predicate itself
    [<DataRow("pr1a", """def cl A1 def pred A() { dec assert is(self, A1); true} thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // one assertion + predicate itself
    [<DataRow("pr2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true } thm T {true} prf T$1 { 1. bydef A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions + predicate itself
    [<DataRow("pr2a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true } thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // two assertions + predicate itself
    [<DataRow("pr3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr3a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // two assertions + predicate itself + predicative property

    [<DataRow("fu0_a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- A is A1 }""", 0)>] // two assertions + predicative property
    [<DataRow("fu0_b", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions + predicative property
    [<DataRow("fu0_c", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicative property
    [<DataRow("fu0a", """def cl Nat def func A()->ind {dec assert ex x:obj {is(x,Nat)}; ret $1} thm T {true} prf T$1 { 1. bydef A |- ∃ x:obj {x is Nat} }""", 0)>]
    [<DataRow("fu0aa", """def cl Nat def func A()->ind {dec assert ex x:obj {is(x,Nat)}; ret $1} thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>]
    [<DataRow("fu1", """def cl A1 def func A()->ind { dec assert is(self, A1); ret $1} thm T {true} prf T$1 { 1. bydef A |- is(A(), A1) }""", 1)>] // one assertion 
    [<DataRow("fu1a", """def cl A1 def func A()->ind { dec assert is(self, A1); ret $1} thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // one assertion 
    [<DataRow("fu2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 } thm T {true} prf T$1 { 1. bydef A |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions 
    [<DataRow("fu2a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 } thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // two assertions 
    [<DataRow("fu3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicative property
    [<DataRow("fu3a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // two assertions + predicative property
    [<TestMethod>]
    member this.TestPR021InferenceByDef(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByDef.fpl" fplCode code expected

    [<DataRow("cl0_a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- A is A1 }""", 0)>] // two assertions + predicative property
    [<DataRow("cl0_b", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions + predicative property
    [<DataRow("cl0_c", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicative property
    [<DataRow("cl0a", """def cl A thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>]
    [<DataRow("cl1", """def cl A1 def cl A { dec assert is(self, A1); ctor A() {} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- A is A1 }""", 0)>] // one assertion
    [<DataRow("cl1a", """def cl A1 def cl A { dec assert is(self, A1); ctor A() {} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // one assertion
    [<DataRow("cl2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions
    [<DataRow("cl2a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // two assertions
    [<DataRow("cl3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicative property
    [<DataRow("cl3a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def cl A { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ctor A() {} prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // two assertions + predicative property

    [<DataRow("pr0_a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- A is A1 }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr0_b", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr9_c", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- true }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr0_d", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr0a", """def pred A() thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>]
    [<DataRow("pr0a", """def cl Nat def pred A() {ex x:obj {is(x,Nat)}} thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∃ x:obj {x is Nat} }""", 0)>]
    [<DataRow("pr0aa", """def cl Nat def pred A() {ex x:obj {is(x,Nat)}} thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>]
    [<DataRow("pr1", """def cl A1 def pred A() { dec assert is(self, A1); true} thm T {true} prf T$1 {dec v:A; 1. bydef v |- A() is A1 }""", 0)>] // one assertion + predicate itself
    [<DataRow("pr1a", """def cl A1 def pred A() { dec assert is(self, A1); true} thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // one assertion + predicate itself
    [<DataRow("pr2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions + predicate itself
    [<DataRow("pr2a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true } thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // two assertions + predicate itself
    [<DataRow("pr3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicate itself + predicative property
    [<DataRow("pr3a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def pred A() { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; true prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // two assertions + predicate itself + predicative property

    [<DataRow("fu0_a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- A is A1 }""", 0)>] // two assertions + predicative property
    [<DataRow("fu0_b", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions + predicative property
    [<DataRow("fu0_c", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicative property
    [<DataRow("fu0a", """def cl Nat def func A()->ind {dec assert ex x:obj {is(x,Nat)}; ret $1} thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∃ x:obj {x is Nat} }""", 0)>]
    [<DataRow("fu0aa", """def cl Nat def func A()->ind {dec assert ex x:obj {is(x,Nat)}; ret $1} thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>]
    [<DataRow("fu1", """def cl A1 def func A()->ind { dec assert is(self, A1); ret $1} thm T {true} prf T$1 {dec v:A; 1. bydef v |- is(A(), A1) }""", 1)>] // one assertion 
    [<DataRow("fu1a", """def cl A1 def func A()->ind { dec assert is(self, A1); ret $1} thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // one assertion 
    [<DataRow("fu2", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∀ m, n:Nat {((n') = (m')) ⇒ (n = m)} }""", 0)>] // two assertions 
    [<DataRow("fu2a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 } thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // two assertions 
    [<DataRow("fu3", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- ∃ x:obj {x is Nat} }""", 0)>] // two assertions + predicative property
    [<DataRow("fu3a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" def cl A1 def func A()->obj { dec assert is(self, A1) assert all n,m:Nat { impl( ( n' = m' ), ( n = m ) ) }; ret $1 prty pred J() {ex x:obj {is(x,Nat)}} } thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // two assertions + predicative property
    [<TestMethod>]
    member this.TestPR021InferenceByDefVar(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByDefVar.fpl" fplCode code expected

    [<DataRow("00", """def cl N thm T {true} prf T$1 { 1: ex x:obj {is(x,N)} 2. 1 |- ∃ x:obj {x is N}}""", 0)>]
    [<DataRow("00a", """def cl N thm T {true} prf T$1 { 1: ex x:obj {is(x,N)} 2. 1 |- false}""", 1)>]
    [<DataRow("01", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prf T$1 { 1: all x,y:Set {impl ( is(x, N), ( x = y ))} 2. 1 |- ∀ x, y:Set {(x is N) ⇒ (x = y)} }""", 0)>]
    [<DataRow("01a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def cl Nat def func Succ(n: Nat) -> Nat postfix "'" prf T$1 { 1: all x,y:Set {impl ( is(x, N), ( x = y ))} 2. 1 |- true }""", 1)>]
    [<TestMethod>]
    member this.TestPR021InferenceByArgRef(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByArgRef.fpl" fplCode code expected

    [<DataRow("00", """def cl N thm T {true} prf T$1 { a: ex x:obj {is(x,N)} 2. 1 |- trivial} thm T1 {true} prf T1$1 { 1. T$1:a |- ∃ x:obj {x is N} }""", 0)>]
    [<DataRow("00a", """def cl N thm T {true} prf T$1 { a: ex x:obj {is(x,N)} 2. 1 |- trivial} thm T1 {true} prf T1$1 { 1. T$1:a |- false }""", 1)>]
    [<DataRow("01", """ax A {not false} thm T {true} prf T$1 { a. byax A |- not false 2. 1 |- not false } thm T1 {true} proof T1$1 {1. T$1:a |- not false }""", 0)>]
    [<DataRow("01a", """ax A {not false} thm T {true} prf T$1 { a. byax A |- not false 2. 1 |- not false } thm T1 {true} proof T1$1 {1. T$1:a |- false}""", 1)>]
    // conflicting identifiers in both proofs do not cause the error (would be a false positive)
    [<DataRow("c0", """def cl N thm T {true} prf T$1 { 1: ex x:obj {is(x,N)} 2. 1 |- ∃ x:obj {x is N}} thm T1 {true} prf T1$1 { 1. T$1:1 |- ∃ x:obj {x is N} }""", 0)>]
    [<DataRow("c0a", """def cl N thm T {true} prf T$1 { 1: ex x:obj {is(x,N)} 2. 1 |- ∃ x:obj {x is N}} thm T1 {true} prf T1$1 { 1. T$1:1 |- false }""", 1)>]
    [<DataRow("c1", """ax A {not false} thm T {true} prf T$1 { 1. byax A |- not false 2. 1 |- not false } thm T1 {true} proof T1$1 {1. T$1:1 |- not false }""", 0)>]
    [<DataRow("c1a", """ax A {not false} thm T {true} prf T$1 { 1. byax A |- not false 2. 1 |- not false } thm T1 {true} proof T1$1 {1. T$1:1 |- false}""", 1)>]
    [<TestMethod>]
    member this.TestPR021InferenceByProofArgument(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByProofArgument.fpl" fplCode code expected


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
            let code = PR021 ("", "", "")
            runTestHelper "TestPR021InferenceByInf.fpl" fplCode code expected

    

