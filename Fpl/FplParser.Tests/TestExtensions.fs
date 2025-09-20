namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestExtensions () =

    [<DataRow(@"ext Digits x@/\d+/ -> A {return x}")>]
    [<DataRow(@"ext Alpha y@/[a-z]+/ -> A {return y}")>]
    [<DataRow(@"ext T z@/ / -> S {return z}")>]
    [<TestMethod>]
    member this.TestExtension (ext:string) =
        let result = run (definitionExtension .>> eof) ext
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(@"ext Digits: x:=// {return x}")>]
    [<DataRow(@"ext Alpha: x:=/[a-z]+ {return x}")>]
    [<DataRow(@"ext Alpha: x:=[a-z]+/ {return x}")>]
    [<TestMethod>]
    member this.TestExtensionSyntaxError (ext:string) =
        let result = run (definitionExtension .>> eof) ext
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow(@"ext Digits x@/\d+/ -> R{return x} def pred T() {@1};")>]
    [<DataRow(@"ext Alpha x@/[a-z]+/ -> A {return x} ext Digits x@/\d+/ -> B {return x} def pred T() {@123};")>]
    [<DataRow(@"ext Alpha x@/[a-z]+/ -> A {return x} ext Digits x@/\d+/ -> B {return x} def pred T() {@abc};")>]
    [<DataRow(@"ext Alpha x@/[a-z]+/ -> A {return x} def pred T() {@123};")>]
    [<DataRow(@"ext Digits x@/\d+/ -> D {return x} def pred T() {@abc};")>]
    [<DataRow(@"ext Alpha x@/[a-z]+/ -> A {return x} def pred T() {@abc};")>]
    [<DataRow(@"ext Digits x@/\d+/ -> S {return x};")>]
    [<DataRow(@"ext Alpha x@/[a-z]+/ -> T {return x};")>]
    [<TestMethod>]
    member this.TestExtensionMultiple (ext:string) =
        let result = run (ast .>> eof) ext
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

