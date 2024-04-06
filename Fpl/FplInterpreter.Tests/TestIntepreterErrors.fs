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

    [<DataRow("axiom", "SomeAxiom()", "SomeAxiom()")>]
    [<DataRow("postulate", "SomePostulate()", "SomePostulate()")>]
    [<DataRow("theorem", "SomeTheorem()", "SomeTheorem()")>]
    [<DataRow("lemma", "SomeLemma()", "SomeLemma()")>]
    [<DataRow("proposition", "SomeProposition()", "SomeProposition()")>]
    [<DataRow("conjecture", "SomeConjecture()", "SomeConjecture()")>]
    [<TestMethod>]
    member this.TestID001Predicative(blockType:string, blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """%s %s {true} %s %s {true} ;""" blockType blockName blockType blockName
        this.PrepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("axiom", "SomeAxiom()")>]
    [<DataRow("postulate", "SomePostulate()")>]
    [<DataRow("theorem", "SomeTheorem()")>]
    [<DataRow("lemma", "SomeLemma()")>]
    [<DataRow("proposition", "SomeProposition()")>]
    [<DataRow("conjecture", "SomeConjecture()")>]
    [<TestMethod>]
    member this.TestID001PredicativeCrossCheck(blockType:string, blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """%s %s {true} ;""" blockType blockName 
        this.PrepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeFunctionalTerm() -> obj", "SomeFunctionalTerm()->object")>]
    [<DataRow("SomeFunctionalTerm(x:ind) -> obj", "SomeFunctionalTerm(index)->object")>]
    [<DataRow("SomeFunctionalTerm(x:pred) -> obj", "SomeFunctionalTerm(predicate)->object")>]
    [<DataRow("SomeFunctionalTerm(x:func) -> obj", "SomeFunctionalTerm(function)->object")>]
    [<DataRow("SomeFunctionalTerm(x:obj) -> obj", "SomeFunctionalTerm(object)->object")>]
    [<DataRow("SomeFunctionalTerm(x:index) -> obj", "SomeFunctionalTerm(index)->object")>]
    [<DataRow("SomeFunctionalTerm(x:predicate) -> obj", "SomeFunctionalTerm(predicate)->object")>]
    [<DataRow("SomeFunctionalTerm(x:function) -> obj", "SomeFunctionalTerm(function)->object")>]
    [<DataRow("SomeFunctionalTerm(x:object) -> obj", "SomeFunctionalTerm(object)->object")>]
    [<DataRow("SomeFunctionalTerm(x:Nat) -> obj", "SomeFunctionalTerm(Nat)->object")>]
    [<DataRow("SomeFunctionalTerm(x:@Nat) -> obj", "SomeFunctionalTerm(@Nat)->object")>]
    [<DataRow("SomeFunctionalTerm(x:tpl) -> obj", "SomeFunctionalTerm(tpl)->object")>]
    [<DataRow("SomeFunctionalTerm(x:template) -> obj", "SomeFunctionalTerm(template)->object")>]
    [<DataRow("SomeFunctionalTerm(x:tplTest) -> obj", "SomeFunctionalTerm(tplTest)->object")>]
    [<DataRow("SomeFunctionalTerm(x:templateTest) -> obj", "SomeFunctionalTerm(templateTest)->object")>]
    [<TestMethod>]
    member this.TestID001Function(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """def function %s {intrinsic} def function %s {intrinsic} ;""" blockName blockName
        this.PrepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeFunctionalTerm() -> obj")>]
    [<TestMethod>]
    member this.TestID001FunctionCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """def function %s {intrinsic} ;""" blockName 
        this.PrepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeClass")>]
    [<TestMethod>]
    member this.TestID001Class(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """def class %s: obj {intrinsic} def class %s: obj {intrinsic} ;""" blockName blockName
        this.PrepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeClass")>]
    [<TestMethod>]
    member this.TestID001ClassCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """def class %s: obj {intrinsic} ;""" blockName 
        this.PrepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomePredicate()", "SomePredicate()")>]
    [<TestMethod>]
    member this.TestID001Predicate(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """def predicate %s {intrinsic} def predicate %s {intrinsic} ;""" blockName blockName
        this.PrepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomePredicate()")>]
    [<TestMethod>]
    member this.TestID001PredicateCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """def predicate %s {intrinsic} ;""" blockName 
        this.PrepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeRuleOfInference()", "SomeRuleOfInference()")>]
    [<TestMethod>]
    member this.TestID001RuleOfInference(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """inference %s {pre: true con: true} inference %s {pre: true con: true} ;""" blockName blockName
        this.PrepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeRuleOfInference()")>]
    [<TestMethod>]
    member this.TestID001RuleOfInferenceCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """inference %s {pre: true con: true} ;""" blockName 
        this.PrepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeProof$1", "SomeProof$1")>]
    [<DataRow("SomeProof$1$2", "SomeProof$1$2")>]
    [<DataRow("SomeProof$1$1$2", "SomeProof$1$1$2")>]
    [<TestMethod>]
    member this.TestID001Proof(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """proof %s {1. |- trivial} proof %s {1. |- trivial} ;""" blockName blockName
        this.PrepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeProof$1")>]
    [<DataRow("SomeProof$1$2")>]
    [<DataRow("SomeProof$1$1$2")>]
    [<TestMethod>]
    member this.TestID001ProofCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """proof %s {1. |- trivial} ;""" blockName 
        this.PrepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeCorollary$1()", "SomeCorollary$1()")>]
    [<DataRow("SomeCorollary$1$2()", "SomeCorollary$1$2()")>]
    [<DataRow("SomeCorollary$1$1$2()", "SomeCorollary$1$1$2()")>]
    [<TestMethod>]
    member this.TestID001Corollary(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """corollary %s {true} corollary %s {true} ;""" blockName blockName
        this.PrepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareFplCode("", true) |> ignore

    [<DataRow("SomeCorollary$1()")>]
    [<DataRow("SomeCorollary$1$2()")>]
    [<DataRow("SomeCorollary$1$1$2()")>]
    [<TestMethod>]
    member this.TestID001CorollaryCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """corollary %s {true} ;""" blockName 
        this.PrepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareFplCode("", true) |> ignore
