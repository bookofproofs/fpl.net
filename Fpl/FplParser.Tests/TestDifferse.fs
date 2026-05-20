namespace FplParser.Tests

open FParsec
open FplParser
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
    [<DataRow("03", """def pred Equal(x,y:tpl) infix "=" 0 { delegate.Equal(x,y) } inf ExistsByExample{dec p:pred(d:obj, c:tpl); pre:p con:ex x:tpl{p(x)}} thm T {true} proof T$1 {dec a:tpl x:obj; 1. |- and(is(x,M) , (a = $1)) 2. 1, byinf ExistsByExample |- true }""")>]
    [<DataRow("04", """axiom SomeAxiom2 {true}""")>]
    [<DataRow("05", """def cl TestId {ctor TestId() {} ctor TestId(x:obj) {} ctor TestId(x:pred) {} }""")>]
    [<TestMethod>]
    member this.TestDiverseSuccess (no:string, fplCode:string) =
        let result = run (ast .>> eof) fplCode
        let actual = sprintf "%O" result 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("02", """axiom s SomeAxiom2 {true}""")>]
    [<TestMethod>]
    member this.TestDiverseBuildingBlockFail (no:string, fplCode:string) =
        let result = run (buildingBlock .>> eof) fplCode
        let actual = sprintf "%O" result 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
