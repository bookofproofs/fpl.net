namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes

[<TestClass>]
type TestInterpreterErrors() =
    let deleteFilesWithExtension dir extension =
        if Directory.Exists(dir) then
            Directory.GetFiles(dir, "*." + extension)
            |> Array.iter File.Delete
        else
            printfn "Directory %s does not exist." dir

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
        let parsedAsts = System.Collections.Generic.List<ParsedAst>()
        FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts |> ignore
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
        let parsedAsts = System.Collections.Generic.List<ParsedAst>()
        FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts |> ignore 
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    member this.PrepareTestNSP004CircularAA(delete:bool) =
        FplParser.parserDiagnostics.Clear()
        let A = """uses Test1_A;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test1_A.fpl")
        File.WriteAllText(pathToFile, A)
        let uri = System.Uri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            File.Delete(pathToFile)
        else
            let parsedAsts = System.Collections.Generic.List<ParsedAst>()
            FplInterpreter.fplInterpreter A uri fplLibUrl parsedAsts

    [<TestMethod>]
    member this.TestNSP004CircularAA() =
        let code = NSP004 "Test1_A -> Test1_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP004CircularAA(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareTestNSP004CircularAA(true) |> ignore

    member this.PrepareTestNSP004CircularABCA(delete:bool) =
        FplParser.parserDiagnostics.Clear()
        let A = """uses Test2_B;"""
        let B = """uses Test2_C;"""
        let C = """uses Test2_A;"""
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Test2_A.fpl"), A)
        File.WriteAllText(Path.Combine(currDir, "Test2_B.fpl"), B)
        File.WriteAllText(Path.Combine(currDir, "Test2_C.fpl"), C)
        let uri = System.Uri(Path.Combine(currDir, "Test2_A.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
        else
            let parsedAsts = System.Collections.Generic.List<ParsedAst>()
            FplInterpreter.fplInterpreter A uri fplLibUrl parsedAsts

    [<TestMethod>]
    member this.TestNSP004CircularABCA() =
        let code = NSP004 "Test2_A -> Test2_B -> Test2_C -> Test2_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP004CircularABCA(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareTestNSP004CircularABCA(true) |> ignore

    member this.PrepareTestNSP005 (delete:bool) =
        FplParser.parserDiagnostics.Clear()
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Fpl.Commons.fpl"), input)
        let uri = System.Uri(Path.Combine(currDir, "Fpl.Commons.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
        else
            let parsedAsts = System.Collections.Generic.List<ParsedAst>()
            FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts

    [<TestMethod>]
    member this.TestNSP005() =
        let code = NSP005 ("Fpl.Commons", ["./"; "https"])
        printf "Trying %s" code.Message
        this.PrepareTestNSP005(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareTestNSP005(true) |> ignore

    member this.PrepareTestNSP005a (delete:bool) =
        FplParser.parserDiagnostics.Clear()
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Fpl.Commons.fpl"), input)
        File.WriteAllText(Path.Combine(currDir, "lib", "Fpl.Commons.fpl"), input)
        let uri = System.Uri(Path.Combine(currDir, "Fpl.Commons.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            deleteFilesWithExtension (Path.Combine(currDir, "lib")) "fpl"
        else
            let parsedAsts = System.Collections.Generic.List<ParsedAst>()
            FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts

    [<TestMethod>]
    member this.TestNSP005a() =
        let code = NSP005 ("Fpl.Commons", ["./"; "./lib"; "https"])
        printf "Trying %s" code.Message
        this.PrepareTestNSP005a(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareTestNSP005a(true) |> ignore


    member this.PrepareFplCode(fplCode:string, delete:bool) =
        FplParser.parserDiagnostics.Clear()
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Test.fpl"), fplCode)
        let uri = System.Uri(Path.Combine(currDir, "Test.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
        else
            let parsedAsts = System.Collections.Generic.List<ParsedAst>()
            FplInterpreter.fplInterpreter fplCode uri fplLibUrl parsedAsts

    [<TestMethod>]
    member this.TestID001() =
        let code = ID001 "SomeAxiom"
        printf "Trying %s" code.Message
        let fplCode = """axiom SomeAxiom() {true} 
        axiom SomeAxiom() {true}
        ;"""
        this.PrepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareFplCode("", true) |> ignore

    [<TestMethod>]
    member this.TestID001CrossCheck() =
        let code = ID001 "SomeAxiom"
        printf "Trying %s" code.Message
        this.PrepareFplCode("axiom SomeAxiom() {true} ;", false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(0, result.Length)
        this.PrepareFplCode("", true) |> ignore
