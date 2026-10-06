namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestExtension () =

    [<DataRow("01", """ext Digits x@/\d+/ -> A {return x}""")>]
    [<DataRow("02", """ext Alpha y@/[a-z]+/ -> A {return y}""")>]
    [<DataRow("03", """ext T z@/ / -> S {return z}""")>]
    [<DataRow("04", """ext Digits x@/\d+/ -> obj {ret x}""")>]
    [<DataRow("05", """extension Digits x@/\d+/ -> S {return x}""")>]
    [<DataRow("06", """extension Alpha x@/[a-z]+/ -> T {return x}""")>]
    [<DataRow("07", """ext Digits x @/\d+/ ->Nat {return x}""")>]
    [<DataRow("08", """extension Digits x@ /\d+/ -> Nat { ret x}""")>]
    [<DataRow("09", """extension Digits x @ /\d+/ -> Nat { return x}""")>]
    [<DataRow("10", """ext Digits x@/\d+/->Nat{return x}""")>]

    [<TestMethod>]
    member _.TestDefinitionExtensionForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments definitionExtension fplCode

