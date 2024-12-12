namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestExtensions () =

    [<DataRow(@"ext Digits: /\d+/")>]
    [<DataRow(@"ext Alpha: /[a-z]+/")>]
    [<DataRow(@"ext T: / /")>]
    [<TestMethod>]
    member this.TestExtension (ext:string) =
        let result = run (extensionBlock .>> eof) ext
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow(@"ext Digits: //")>]
    [<DataRow(@"ext Alpha: /[a-z]+")>]
    [<DataRow(@"ext Alpha: [a-z]+/")>]
    [<TestMethod>]
    member this.TestExtensionSyntaxError (ext:string) =
        let result = run (extensionBlock .>> eof) ext
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow(@"ext Digits: /\d+/ def pred T() {@1};")>]
    [<DataRow(@"ext Alpha: /[a-z]+/ ext Digits: /\d+/ def pred T() {@123};")>]
    [<DataRow(@"ext Alpha: /[a-z]+/ ext Digits: /\d+/ def pred T() {@abc};")>]
    [<DataRow(@"ext Alpha: /[a-z]+/ def pred T() {@123};")>]
    [<DataRow(@"ext Digits: /\d+/ def pred T() {@abc};")>]
    [<DataRow(@"ext Alpha: /[a-z]+/ def pred T() {@abc};")>]
    [<DataRow(@"ext Digits: /\d+/;")>]
    [<DataRow(@"ext Alpha: /[a-z]+/;")>]
    [<TestMethod>]
    member this.TestExtensionMultiple (ext:string) =
        let result = run (ast .>> eof) ext
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

