namespace TestFpl1Parser.BuildingBlocks

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestLocalization () =
    [<DataRow("01", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x ; """)>]
    [<DataRow("02", """loc iif(x,y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y ;""")>]
    [<DataRow("03", """loc NotEqual(x,y) := !tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y ;""")>]
    [<TestMethod>]
    member this.TestLocalizationSuccess (no:string, input:string) =
        let result = run (localization .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
