namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestLocalizationElements () =

    [<DataRow("01", """x "\Leftrightarrow" y """)>]
    [<DataRow("02", """"\neg(" x ")" """)>]
    [<TestMethod>]
    member _.TestEbnfTermSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments ebnfTerm fplCode

    [<DataRow("01", """x "\Leftrightarrow" y """)>]
    [<DataRow("02", """"\neg(" x ")" """)>]
    [<DataRow("03", """x "\Leftrightarrow" y | x "\Rightarrow" y """)>]
    [<DataRow("04", """"\neg(" x ")" | x "\Rightarrow" y """)>]
    [<TestMethod>]
    member _.TestEbnfTranslSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments ebnfTransl fplCode

    [<DataRow("01", """!tex: x "\Leftrightarrow" y """)>]
    [<DataRow("02", """!tex: x "\Leftrightarrow" y | x "\Rightarrow" y """)>]
    [<TestMethod>]
    member _.TestLanguageSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments language fplCode

