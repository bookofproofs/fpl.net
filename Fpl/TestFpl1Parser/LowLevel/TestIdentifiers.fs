namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestIdentifiers () =

    [<DataRow("01", """Fpl.Test alias MyAlias""")>]
    [<DataRow("02", """Fpl.Test""")>]
    [<TestMethod>]
    member this.TestTheoryNamespaceSuccess (no:string, input:string) =
        let result = run (theoryNamespace .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """uses  Fpl.Test alias MyAlias uses Fpl.Test uses Fpl.Test.Test1 """)>]
    [<DataRow("02", """uses Fpl.Commons uses Fpl.SetTheory.ZermeloFraenkel""")>]
    [<DataRow("03", """uses Fpl.Commons uses Fpl.SetTheory.ZermeloFraenkel alias ZF uses  Fpl.Arithmetics.Peano alias A""")>]
    [<DataRow("04", """uses Fpl.Commons *""")>]
    [<TestMethod>]
    member this.TestFplNamespaceSuccess (no:string, input:string) =
        let result = run (fplNamespace .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """ThisIsMyIdentifier""")>]
    [<TestMethod>]
    member this.TestPredicateIdentifierSuccess (no:string, input:string) =
        let result = run (predicateIdentifier .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """This.Is.My.Identifier""")>]
    [<TestMethod>]
    member this.TestPredicateIdentifierFailure (no:string, input:string) =
        let result = run (predicateIdentifier .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", LiteralSelf)>]
    [<DataRow("02", LiteralParent)>]
    [<TestMethod>]
    member this.TestSelfOrParentSuccess (no:string, input:string) =
        let result = run (selfOrParent .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """@self""")>]
    [<TestMethod>]
    member this.TestSelfOrParentFailure (no:string, input:string) =
        let result = run (selfOrParent .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", """xyz""")>]
    [<TestMethod>]
    member this.TestVariableSuccess (no:string, input:string) =
        let result = run (variable .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """Digits """)>]
    [<TestMethod>]
    member this.TestExtensionNameSuccess (no:string, input:string) =
        let result = run (extensionName .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

