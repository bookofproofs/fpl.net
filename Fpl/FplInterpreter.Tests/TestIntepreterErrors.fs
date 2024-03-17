namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open FParsec
open ErrDiagnostics

[<TestClass>]
type TestInterpreterErrors() =
    let filterByErrorCode (input: Diagnostics) errCode =
        input.Collection
        |> List.filter (fun d -> d.Code = errCode)

    [<TestMethod>]
    member this.TestNSP003() =
        let input = """
        uses Fpl1 alias T1
        uses Fpl2 alias T1
        ;"""
        let ast = FplParser.fplParser input
        let fplLibUri = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let interpreter = FplInterpreter.fplInterpreter ast uri fplLibUri
        let result = filterByErrorCode FplParser.parserDiagnostics (NSP003 "T1")
        Assert.AreEqual(1, result.Length)

