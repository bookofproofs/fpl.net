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


    [<DataRow("00", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(true) ) };""")>]
    [<DataRow("00", """def pred A() ext Test x@/\d+/->pred() {return A};""")>]
    [<TestMethod>]
    member this.TestDiverse00 (no:string, fplCode:string) =
        let result = run (ast .>> eof) fplCode
        let actual = sprintf "%O" result 
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

