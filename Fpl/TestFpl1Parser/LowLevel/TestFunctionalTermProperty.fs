namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestFunctionalTermProperty () =

    [<DataRow("01", """property func X() -> Y { dec a:obj ; return x }""")>]
    [<DataRow("02", """prty func X() -> Y { dec a:obj ; return x }""")>]
    [<DataRow("03", """prty function X() -> Y { dec a:obj ; return x }""")>]
    [<TestMethod>]
    member this.TestDefinitionPropertySuccess (no:string, fplCode:string) =
        let result = run (definitionProperty .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """prty func X() -> Y {  }""")>] // a function prty without a return statement is not allowed
    [<DataRow("02", """prty func X() -> Y { dec; }""")>] // a function prty without a return statement is not allowed 
    [<DataRow("03", """prty func X() -> Y { dec a:obj ; }""")>] // a function prty without a return statement is not allowed 
    [<DataRow("04", """prty func X() -> Y { dec:; }""")>] // a function prty without a return statement is not allowed 
    [<DataRow("05", """prty func X() -> Y { dec a:obj ; x }""")>] // a function prty without a return statement is not allowed 
    [<DataRow("06", """prty func X() -> Y { dec:; return x }""")>] // malformed declaration 
    [<DataRow("07", """prty func X() -> Y { x }""")>]
    [<DataRow("08", """property func X() -> Y { return x dec a:obj ; }""")>]
    [<DataRow("09", """func X() -> Y { return x dec a:obj ; }""")>]
    [<DataRow("10", """prty func X() -> Y { return x dec a:ind; }""")>]
    [<DataRow("11", """prty func VecAdd(from,to: Nat, v,w: tplFieldElem[from ~ to]) -> tplFieldElem[from ~ to] { dec a:obj self[from ~ to]:=addInField(v[from ~ to],w[from ~ to]) ; }""")>]
    [<TestMethod>]
    member this.TestDefinitionPropertyFailure (no:string, fplCode:string) =
        let result = run (definitionProperty .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

