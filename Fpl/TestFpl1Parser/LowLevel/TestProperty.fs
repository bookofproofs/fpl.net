namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestProperty () =

    [<DataRow("func01", """property func X() -> Y { dec a:obj ; return x }""")>]
    [<DataRow("func02", """prty func X() -> Y { dec a:obj ; return x }""")>]
    [<DataRow("func03", """prty function X() -> Y { dec a:obj ; return x }""")>]
    [<DataRow("pred01", """property pred X() { dec a:obj ; true }""")>]
    [<DataRow("pred02", """prty pred X() { dec a:obj ; true }""")>]
    [<DataRow("pred03", """property predicate X() { dec a:obj ; true }""")>]
    [<DataRow("pred04", """prty pred X() { dec a:obj ; true }""")>]
    [<DataRow("pred05", """property pred X() { true }""")>]
    [<DataRow("pred06", """property pred X() { intr }""")>]
    [<DataRow("pred07", """prty pred T() {true}""")>]
    [<TestMethod>]
    member this.TestDefinitionPropertySuccess (no:string, fplCode:string) =
        let result = run (definitionProperty .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("func01", """prty func X() -> Y {  }""")>] // a function prty without a return statement is not allowed
    [<DataRow("func02", """prty func X() -> Y { dec; }""")>] // a function prty without a return statement is not allowed 
    [<DataRow("func03", """prty func X() -> Y { dec a:obj ; }""")>] // a function prty without a return statement is not allowed 
    [<DataRow("func04", """prty func X() -> Y { dec:; }""")>] // a function prty without a return statement is not allowed 
    [<DataRow("func05", """prty func X() -> Y { dec a:obj ; x }""")>] // a function prty without a return statement is not allowed 
    [<DataRow("func06", """prty func X() -> Y { dec:; return x }""")>] // malformed declaration 
    [<DataRow("func07", """prty func X() -> Y { x }""")>]
    [<DataRow("func08", """property func X() -> Y { return x dec a:obj ; }""")>]
    [<DataRow("func09", """func X() -> Y { return x dec a:obj ; }""")>]
    [<DataRow("func10", """prty func X() -> Y { return x dec a:ind; }""")>]
    [<DataRow("func11", """prty func VecAdd(from,to: Nat, v,w: tplFieldElem[from ~ to]) -> tplFieldElem[from ~ to] { dec a:obj self[from ~ to]:=addInField(v[from ~ to],w[from ~ to]) ; }""")>]
    [<DataRow("pred01", """prty pred X() { }""")>] // a predicate instance without a predicate is not allowed 
    [<DataRow("pred02", """prty pred X() { dec; }""")>] // a predicate instance without a predicate is not allowed 
    [<DataRow("pred03", """prty pred X() { dec a:obj ; }""")>] // a predicate instance without a predicate is not allowed 
    [<DataRow("pred04", """prty pred X() { dec a:obj ; return x }""")>] // a predicate instance with return not allowed 
    [<DataRow("pred05", """prty pred X() { return x }""")>]
    [<DataRow("pred06", """property pred X() { true dec a:obj ; }""")>]
    [<TestMethod>]
    member this.TestDefinitionPropertyFailure (no:string, fplCode:string) =
        let result = run (definitionProperty .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

