namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestLocalization () =

    [<DataRow("01", """loc not (x) := !tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x ; """)>]
    [<DataRow("02", """localization iif(x,y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y ;""")>]
    [<DataRow("03", """loc NotEqual(x,y) := !tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y ;""")>]
    [<TestMethod>]
    member _.TestLocalizationForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments localization fplCode

