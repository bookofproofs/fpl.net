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
    member this.TestNSP000() =
        let code = NSP000 "Bla.fpl"
        printf "Trying %s" code.Message
        let input = """
        uses Bla 
        ;"""
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        FplInterpreter.fplInterpreter input uri fplLibUrl |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestNSP003() =
        let code = NSP003 "T1"
        printf "Trying %s" code.Message
        let input = """
        uses Fpl1 alias T1
        uses Fpl2 alias T1
        ;"""
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        FplInterpreter.fplInterpreter input uri fplLibUrl |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    member this.PrepareTestNSP004Circular() =
        let input = """
            uses Test
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl")
        let uri = System.Uri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        FplInterpreter.fplInterpreter input uri fplLibUrl

    [<TestMethod>]
    member this.TestNSP004Cicrular() =
        let code = NSP004 "Test"
        printf "Trying %s" code.Message
        this.PrepareTestNSP004Circular() |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
