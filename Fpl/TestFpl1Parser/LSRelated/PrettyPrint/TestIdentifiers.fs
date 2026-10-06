namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestIdentifiers () =

    [<DataRow("01", """Fpl.Test alias MyAlias""")>]
    [<DataRow("02", """Fpl.Test""")>]
    [<TestMethod>]
    member _.TestTheoryNamespaceForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments theoryNamespace fplCode

    [<DataRow("01", """uses  Fpl.Test alias MyAlias uses Fpl.Test uses Fpl.Test.Test1 """)>]
    [<DataRow("02", """uses Fpl.Commons uses Fpl.SetTheory.ZermeloFraenkel""")>]
    [<DataRow("03", """uses Fpl.Commons uses Fpl.SetTheory.ZermeloFraenkel alias ZF uses  Fpl.Arithmetics.Peano alias A""")>]
    [<DataRow("04", """uses Fpl.Commons *""")>]
    [<TestMethod>]
    member _.TestFplNamespaceForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments fplNamespace fplCode

    [<DataRow("01", """ThisIsMyIdentifier""")>]
    [<TestMethod>]
    member _.TesPredicateIdentifierForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments predicateIdentifier fplCode

    [<DataRow("01", LiteralSelf)>]
    [<DataRow("02", LiteralParent)>]
    [<TestMethod>]
    member _.TestSelfOrParentForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments selfOrParent fplCode

    [<DataRow("01", """xyz""")>]
    [<TestMethod>]
    member _.TestVariableForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments variable fplCode

    [<DataRow("01", """Digits """)>]
    [<TestMethod>]
    member _.TestExtensionNameForSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsForSyntaxErrorFreeInputWithoutComments extensionName fplCode
