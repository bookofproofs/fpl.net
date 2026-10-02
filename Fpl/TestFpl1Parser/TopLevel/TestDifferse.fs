namespace TestFpl1Parser

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestDiverse () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""


    [<DataRow("00", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(true) ) }""")>]
    [<DataRow("01", """def pred A() ext Test x@/\d+/->pred() {return A}""")>]
    [<DataRow("02", """def cl A def pred T(a:obj) {is(a,A)}""")>]
    [<DataRow("03", """def pred Equal(x,y:tpl) infix "=" 0 { delegate.Equal(x,y) } inf ExistsByExample{dec p:pred(d:obj, c:tpl); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {dec a:tpl x:obj; 1: and(is(x,M) , (a = $1)) 2. 1, byinf ExistsByExample |- true }""")>]
    [<DataRow("04", """axiom SomeAxiom2 {true}""")>]
    [<DataRow("05", """def cl TestId {ctor TestId() {} ctor TestId(x:obj) {} ctor TestId(x:pred) {} }""")>]
    [<DataRow("06", """proof SomeFplTheorem$1 {1: trivial}""")>]
    [<DataRow("07", """proof SomeFplTheorem$1 {1. 3 |- trivial}""")>]
    [<DataRow("08", """def pred A() {false ∧ true}""")>]
    [<DataRow("09", """inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- false ∧ true}""")>]
    [<DataRow("10", """def pred A() {f()}""")>]
    [<DataRow("11", """proof T$1 {1: iif (a,b)}""")>]
    [<DataRow("12", """loc not(x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("13", """def pred A() {y is M}""")>]
    [<DataRow("14", """def pred T() { dec x,y,z:pred x:=true y:=true z:=true; and(and(x,y),z) }""")>]
    [<DataRow("15", """loc true := !tex: "1" !eng: "true";""")>]
    [<DataRow("16", """def pred A() {-(y + x' = @2 * x)'}""")>]
    [<DataRow("17", """def pred A() {mcases (|($2 = $1) : $42 ? $1)}""")>]
    [<DataRow("18", """def pred A() {dec n:ind cases (|($2 = $1) : n:=$42 ? n:=$1); true}""")>]
    [<DataRow("19", """def pred T() { undef = undef }""")>]
    [<DataRow("20", """def pred T1() { true } def pred T1a() { not x } def pred T1b() { not (x) } def pred T2() { false } def pred T3() { undef } def pred T4() { 1.x } def pred T5() { del.Test() } def pred T6() { $1 } def pred T7() { true } def pred T8() { Test$2$1 } def pred T9() { Test$1 } def pred T10() { Test } def pred T11() { v } def pred T12() { self } def pred T13() { 1 } def pred T11a() { v.x } def pred T12a() { self.x } def pred T10b() { Test() } def pred T11b() { v() } def pred T12b() { self() } def pred T13b() { @1() } def pred T10c() { Test(x,y) } def pred T11c() { v(x,y) } def pred T12c() { self(x,y) } def pred T13c() { @1(x,y) } def pred T10d() { Test[x,y] } def pred T11d() { v[x,y] } def pred T12d() { self[x,y] } def pred T13d() { @1[x.y] } def pred T10e() { Test(x,y).parent[a,b] } def pred T11e() { v(x,y).x[a,b] } def pred T12e() { self(x,y).@3[a,b] } def pred T13e() { @1(x,y).T[a,b] } def pred T10f() { Test[x,y].x(a,b) } def pred T11f() { v[x,y].x(a,b) } def pred T12f() { self[x,y].self(a,b) } def pred T13f() { @1[x.y].T(a,b) } def pred T14() { ∅ } def pred T15() { -x } def pred T16() { -(y + x = @2 * x) } def pred T17() { (y + x' = @2 * x)' } def pred T18() { ex x:Range, y:C, z:obj {and (and(a,b),c)} } def pred T19() { exn$1 x:obj {all y:N {true}} } def pred T20() { all x:obj {not x} } def pred T21() { and (and(x,y),z) } def pred T21a() { not x } def pred T21b() { not (x) } def pred T22() { xor (xor(x,y),z) } def pred T23() { or (or(x,y),z) } def pred T24() { iif (x,y) } def pred T25() { impl (x,y) } def pred T26() { is (x,Nat) } def cl T27 {ctor T27() {dec base.C(a, b, c, d); } }""")>]
    [<DataRow("21", """def pred T1() { (x = y * z + 1) }""")>]
    [<DataRow("22", """ext Digits x@/\d+/ -> R{return x} def pred T() {@1}""")>]
    [<DataRow("23", """ext Alpha x@/[a-z]+/ -> A {return x} ext Digits x@/\d+/ -> B {return x} def pred T() {@123}""")>]
    [<DataRow("24", """ext Alpha x@/[a-z]+/ -> A {return x} ext Digits x@/\d+/ -> B {return x} def pred T() {@abc}""")>]
    [<DataRow("25", """extension Alpha x@/[a-z]+/ -> A {return x} def pred T() {@123}""")>]
    [<DataRow("26", """extension Digits x@/\d+/ -> D {return x} def pred T() {@abc}""")>]
    [<DataRow("27", """extension Alpha x@/[a-z]+/ -> A {return x} def pred T() {@abc}""")>]
    [<DataRow("28", """extension Alpha x@/\d+/ -> obj {ret x} def pred T() {dec a:obj a:=@1; true}""")>]
    [<TestMethod>]
    member this.TestDiverseSuccess (no:string, fplCode:string) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<DataRow("01", """def cl S                       def cl T {ctor T() {dec base. (); }}""")>]
    [<DataRow("02", """proof SomeFplTheorem$1 {true. assume true}""")>]
    [<DataRow("03", """proof SomeFplTheorem$1 {1. cases |- trivial}""")>]
    [<DataRow("04", """proof SomeFplTheorem$1 {1 3 |- trivial}""")>]
    [<DataRow("05", """proof SomeFplTheorem$1 {1 trivial}""")>]
    [<DataRow("06", """proof SomeFplTheorem$1 {1 }""")>]
    [<DataRow("06a", """proof SomeFplTheorem$1 {tpl. }""")>]
    [<DataRow("06b", """proof SomeFplTheorem$1 {tpl: }""")>]
    [<DataRow("06c", """proof SomeFplTheorem$1 {true. }""")>]
    [<DataRow("06d", """proof SomeFplTheorem$1 {true: }""")>]
    [<TestMethod>]
    member this.TestDiverseFail (no:string, fplCode:string) =
        let result = run (stdParser .>> eof) fplCode
        let actual = sprintf "%O" result 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", """axiom s SomeAxiom2 {true}""")>]
    [<DataRow("02", """def cl T {ctor T() {dec base. (); }}""")>]
    [<DataRow("03", """def pred T() { (∀ x:obj {x is N} ∧ ¬∃ y:obj {y is M}) ∨ (¬∀ x:obj {x  N} ∧ ∃ y:obj {y is M}) }""")>]
    [<DataRow("04", """def pred T() { a * b + (c d) }""")>]
    [<TestMethod>]
    member this.TestDiverseBuildingBlockFail (no:string, fplCode:string) =
        let result = run (buildingBlock .>> eof) fplCode
        let actual = sprintf "%O" result 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
