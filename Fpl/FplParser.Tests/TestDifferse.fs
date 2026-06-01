namespace FplParser.Tests

open FParsec
open FplParsing.Combinators
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
    [<TestMethod>]
    member this.TestDiverseBuildingBlockFail (no:string, fplCode:string) =
        let result = run (buildingBlock .>> eof) fplCode
        let actual = sprintf "%O" result 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
