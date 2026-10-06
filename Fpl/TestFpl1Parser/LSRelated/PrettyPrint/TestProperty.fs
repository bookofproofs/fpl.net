namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

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
    member _.TestDefinitionPropertySyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments definitionProperty fplCode

