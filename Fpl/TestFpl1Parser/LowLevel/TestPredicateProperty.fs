namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestPredicateProperty () =

    [<DataRow("01", """property pred X() { dec a:obj ; true }""")>]
    [<DataRow("02", """prty pred X() { dec a:obj ; true }""")>]
    [<DataRow("03", """property predicate X() { dec a:obj ; true }""")>]
    [<DataRow("04", """prty pred X() { dec a:obj ; true }""")>]
    [<DataRow("05", """property pred X() { true }""")>]
    [<DataRow("06", """property pred X() { intr }""")>]
    [<DataRow("07", """prty pred T() {true}""")>]
    [<TestMethod>]
    member this.TestDefinitionPropertySuccess (no:string, fplCode:string) =
        let result = run (definitionProperty .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """prty pred X() { }""")>] // a predicate instance without a predicate is not allowed 
    [<DataRow("02", """prty pred X() { dec; }""")>] // a predicate instance without a predicate is not allowed 
    [<DataRow("03", """prty pred X() { dec a:obj ; }""")>] // a predicate instance without a predicate is not allowed 
    [<DataRow("04", """prty pred X() { dec a:obj ; return x }""")>] // a predicate instance with return not allowed 
    [<DataRow("05", """prty pred X() { return x }""")>]
    [<DataRow("06", """property pred X() { true dec a:obj ; }""")>]
    [<TestMethod>]
    member this.TestDefinitionPropertyFailure (no:string, fplCode:string) =
        let result = run (definitionProperty .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
