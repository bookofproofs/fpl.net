namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestInterpreterErrors() =

    [<TestMethod>]
    member this.TestNSP00() =
        let code = NSP00 "Bla.fpl"
        printf "Trying %s" code.Message
        let input = """
        uses Bla 
        ;"""
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let parsedAsts = ParsedAstList()
        FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts true |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    [<TestMethod>]
    member this.TestNSP03() =
        let code = NSP03 "T1"
        printf "Trying %s" code.Message
        let input = """
        uses Fpl1 alias T1
        uses Fpl2 alias T1
        ;"""
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let parsedAsts = ParsedAstList()
        FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts true |> ignore 
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)

    member this.PrepareTestNSP04CircularAA(delete:bool) =
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
            None
        else
            let parsedAsts = ParsedAstList()
            Some (FplInterpreter.fplInterpreter A uri fplLibUrl parsedAsts true)

    [<TestMethod>]
    member this.TestNSP04CircularAA() =
        let code = NSP04 "Test1_A -> Test1_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP04CircularAA(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareTestNSP04CircularAA(true) |> ignore

    member this.PrepareTestNSP04CircularABCA(delete:bool) =
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
            None
        else
            let parsedAsts = ParsedAstList()
            Some(FplInterpreter.fplInterpreter A uri fplLibUrl parsedAsts true)

    [<TestMethod>]
    member this.TestNSP04CircularABCA() =
        let code = NSP04 "Test2_A -> Test2_B -> Test2_C -> Test2_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP04CircularABCA(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareTestNSP04CircularABCA(true) |> ignore


    member this.PrepareTestNSP04NonCircular(delete:bool) =
        FplParser.parserDiagnostics.Clear()
        let input = """
            uses Fpl.Commons
            uses Fpl.SetTheory
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test1_A.fpl")
        File.WriteAllText(pathToFile, input)
        let uri = System.Uri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            File.Delete(pathToFile)
            None
        else
            let parsedAsts = ParsedAstList()
            Some (FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts false)

    [<TestMethod>]
    member this.TestNSP04NonCircular() =
        this.PrepareTestNSP04NonCircular(false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareTestNSP04NonCircular(true) |> ignore


    member this.PrepareTestNSP05 (delete:bool) =
        FplParser.parserDiagnostics.Clear()
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Fpl.Commons.fpl"), input)
        let uri = System.Uri(Path.Combine(currDir, "Fpl.Commons.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            None
        else
            let parsedAsts = ParsedAstList()
            Some(FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts true)

    [<TestMethod>]
    member this.TestNSP05() =
        let code = NSP05 ( ["./"; "https"], "Fpl.Commons", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareTestNSP05(true) |> ignore

    member this.PrepareTestNSP05a (delete:bool) =
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
            None
        else
            let parsedAsts = ParsedAstList()
            Some(FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts true)

    [<TestMethod>]
    member this.TestNSP05a() =
        let code = NSP05 (["./"; "./lib"; "https"], "Fpl.Commons", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05a(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        this.PrepareTestNSP05a(true) |> ignore


    member this.PrepareTestNSP05CrossCheck (delete:bool) =
        FplParser.parserDiagnostics.Clear()
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Test.fpl"), input)
        let uri = System.Uri(Path.Combine(currDir, "Test.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            deleteFilesWithExtension (Path.Combine(currDir, "lib")) "fpl"
            None
        else
            let parsedAsts = ParsedAstList()
            Some(FplInterpreter.fplInterpreter input uri fplLibUrl parsedAsts true)

    [<TestMethod>]
    member this.TestNSP05CrossCheck() =
        let code = NSP05 (["./"], "Test", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05CrossCheck(false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareTestNSP05CrossCheck(true) |> ignore

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
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        prepareFplCode("", true) |> ignore

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
        prepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeFunctionalTerm() -> obj", "SomeFunctionalTerm() -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:ind) -> obj", "SomeFunctionalTerm(ind) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:pred) -> obj", "SomeFunctionalTerm(pred) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:func) -> obj", "SomeFunctionalTerm(func) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:obj) -> obj", "SomeFunctionalTerm(obj) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:index) -> obj", "SomeFunctionalTerm(ind) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:predicate) -> obj", "SomeFunctionalTerm(pred) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:function) -> obj", "SomeFunctionalTerm(func) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:object) -> obj", "SomeFunctionalTerm(obj) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:Nat) -> obj", "SomeFunctionalTerm(Nat) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:@Nat) -> obj", "SomeFunctionalTerm(@Nat) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:tpl) -> obj", "SomeFunctionalTerm(tpl) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:template) -> obj", "SomeFunctionalTerm(template) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:tplTest) -> obj", "SomeFunctionalTerm(tplTest) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x:templateTest) -> obj", "SomeFunctionalTerm(templateTest) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x,y,z:obj) -> obj", "SomeFunctionalTerm(obj, obj, obj) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x,y:pred(z:obj)) -> obj", "SomeFunctionalTerm(pred(obj), pred(obj)) -> obj")>]
    [<DataRow("SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj", "SomeFunctionalTerm(pred(obj, obj, obj), pred(obj, obj, obj)) -> obj")>]
    [<TestMethod>]
    member this.TestID001Function(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """def function %s {intrinsic} def function %s {intrinsic} ;""" blockName blockName
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeFunctionalTerm() -> obj")>]
    [<TestMethod>]
    member this.TestID001FunctionCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """def function %s {intrinsic} ;""" blockName 
        prepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeClass")>]
    [<TestMethod>]
    member this.TestID001Class(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """def class %s: obj {intrinsic} def class %s: obj {intrinsic} ;""" blockName blockName
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeClass")>]
    [<TestMethod>]
    member this.TestID001ClassCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """def class %s: obj {intrinsic} ;""" blockName 
        prepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomePredicate()", "SomePredicate()")>]
    [<TestMethod>]
    member this.TestID001Predicate(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """def predicate %s {intrinsic} def predicate %s {intrinsic} ;""" blockName blockName
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomePredicate()")>]
    [<TestMethod>]
    member this.TestID001PredicateCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """def predicate %s {intrinsic} ;""" blockName 
        prepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeRuleOfInference()", "SomeRuleOfInference()")>]
    [<TestMethod>]
    member this.TestID001RuleOfInference(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """inference %s {pre: true con: true} inference %s {pre: true con: true} ;""" blockName blockName
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeRuleOfInference()")>]
    [<TestMethod>]
    member this.TestID001RuleOfInferenceCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """inference %s {pre: true con: true} ;""" blockName 
        prepareFplCode(fplCode, false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeProof$1", "SomeProof$1")>]
    [<DataRow("SomeProof$1$2", "SomeProof$1$2")>]
    [<DataRow("SomeProof$1$1$2", "SomeProof$1$1$2")>]
    [<TestMethod>]
    member this.TestID001Proof(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """proof %s {1. |- trivial} proof %s {1. |- trivial} ;""" blockName blockName
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeProof$1")>]
    [<DataRow("SomeProof$1$2")>]
    [<DataRow("SomeProof$1$1$2")>]
    [<TestMethod>]
    member this.TestID001ProofCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """proof %s {1. |- trivial} ;""" blockName 
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(0, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeCorollary$1()", "SomeCorollary$1()")>]
    [<DataRow("SomeCorollary$1$2()", "SomeCorollary$1$2()")>]
    [<DataRow("SomeCorollary$1$1$2()", "SomeCorollary$1$1$2()")>]
    [<TestMethod>]
    member this.TestID001Corollary(blockName:string, expected:string) =
        let code = ID001 expected
        printf "Trying %s" code.Message
        let fplCode = sprintf """corollary %s {true} corollary %s {true} ;""" blockName blockName
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(1, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("SomeCorollary$1()")>]
    [<DataRow("SomeCorollary$1$2()")>]
    [<DataRow("SomeCorollary$1$1$2()")>]
    [<TestMethod>]
    member this.TestID001CorollaryCrossCheck(blockName:string) =
        let code = ID001 blockName
        printf "Trying %s" code.Message
        let fplCode = sprintf """corollary %s {true} ;""" blockName 
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(0, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("Test(x,y:* pred)", 1)>]
    [<DataRow("Test(x,y:+ pred)", 1)>]
    [<DataRow("Test(x,y: pred)", 0)>]
    [<DataRow("Test(x:* pred)", 0)>]
    [<DataRow("Test(x:+ pred)", 0)>]
    [<TestMethod>]
    member this.TestVAR00(signature:string, expected) =
        let code = VAR00
        printf "Trying %s" code.Message
        let fplCode = sprintf """def predicate %s {true} ;""" signature 
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(expected, result.Length)
        prepareFplCode("", true) |> ignore

    [<DataRow("def pred Test(x,x:* pred) {true};", 1)>]
    [<DataRow("def pred Test(x,x:+ pred) {true};", 1)>]
    [<DataRow("def pred Test(x,x: pred) {true};", 1)>]
    [<DataRow("def pred Test(x: pred) {true};", 0)>]
    [<DataRow("def pred Test(x:+ pred) {dec ~x:obj; true};", 1)>]

    [<DataRow("def pred Test() {true prty pred X(x,x:* pred) {true} };", 1)>]
    [<DataRow("def pred Test() {true prty pred X(x,x:+ pred) {true} };", 1)>]
    [<DataRow("def pred Test() {true prty pred X(x,x: pred) {true} };", 1)>]
    [<DataRow("def pred Test() {true prty pred X(x: pred) {true} };", 0)>]
    [<DataRow("def pred Test() {true prty pred X(x:+ pred) {dec ~x:obj; true} };", 1)>]
    [<DataRow("def pred Test() {true prty func X(x,x:* pred)->obj {intr} };", 1)>]
    [<DataRow("def pred Test() {true prty func X(x,x:+ pred)->obj {intr} };", 1)>]
    [<DataRow("def pred Test() {true prty func X(x,x: pred)->obj {intr} };", 1)>]
    [<DataRow("def pred Test() {true prty func X(x: pred)->obj {intr} };", 0)>]
    [<DataRow("def pred Test() {true prty func X(x:+ pred)->obj {dec ~x:obj; return x} };", 1)>]

    [<TestMethod>]
    member this.TestVAR01(fplCode:string, expected) =
        let code = VAR01 "x"
        FplParser.parserDiagnostics.Clear()
        printf "Trying %s" code.Message
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(expected, result.Length)
        prepareFplCode("", true) |> ignore


    [<DataRow("inf ModusPonens() {dec ~p,q: pred; pre: and (p, impl (p,q) ) con: q};", "p")>]
    [<DataRow("def pred Test() {true prty pred X(y:+ pred) {dec ~x:obj; ex x {true}} };", "x")>]
    [<DataRow("def pred Test() {true prty func X(y:+ pred)->obj {dec ~x:obj; ex x {true}} };", "x")>]
    [<TestMethod>]
    member this.TestVAR01CrossCheck(fplCode:string, duplicateCandidate:string) =
        let code = VAR01 duplicateCandidate
        FplParser.parserDiagnostics.Clear()
        printf "Trying %s" code.Message
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(0, result.Length)
        prepareFplCode("", true) |> ignore


    [<DataRow("def pred Test(x: ind) {true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty pred X(x:* pred) {true} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty pred X(x:+ pred) {true} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty pred X() {dec ~x: obj; true} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty pred X() {dec ~x:* obj; true} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty pred X() {dec ~x:+ obj; true} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty func X(x:* pred)->obj {intr} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty func X(x:+ pred)->obj {intr} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty func X()->obj {dec ~x: obj; return x} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty func X()->obj {dec ~x:* obj; return x} };", 1)>]
    [<DataRow("def pred Test(x: ind) {true prty func X()->obj {dec ~x:+ obj; return x} };", 1)>]
    [<DataRow("def cl Test:obj {dec ~x:ind; ctor Test(x: pred) {self} };", 1)>]
    [<DataRow("def cl Test:obj {dec ~x:ind; ctor Test(x:* pred) {self} };", 1)>]
    [<DataRow("def cl Test:obj {dec ~x:ind; ctor Test(x:+ pred) {self} };", 1)>]
    [<DataRow("def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x: obj; self} };", 1)>]
    [<DataRow("def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x:* obj; self} };", 1)>]
    [<DataRow("def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x:+ obj; self} };", 1)>]
    [<TestMethod>]
    member this.TestVAR02(fplCode:string, expected) =
        let code = VAR02 "x"
        FplParser.parserDiagnostics.Clear()
        printf "Trying %s" code.Message
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(expected, result.Length)
        prepareFplCode("", true) |> ignore


    [<DataRow("axiom Test() {true} proof Test$1 {1. |- trivial};", 1, "axiom")>]
    [<DataRow("conjecture Test() {true} proof Test$1 {1. |- trivial};", 1, "conjecture")>]
    [<TestMethod>]
    member this.TestID002(fplCode:string, expected, falseType) =
        let code = ID002 ("Test$1",falseType)
        FplParser.parserDiagnostics.Clear()
        printf "Trying %s" code.Message
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(expected, result.Length)
        prepareFplCode("", true) |> ignore


    [<DataRow("theorem Test() {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("theorem TestTypo() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<TestMethod>]
    member this.TestID003(fplCode:string, expected) =
        let code = ID003 "Test$1"
        FplParser.parserDiagnostics.Clear()
        printf "Trying %s" code.Message
        prepareFplCode(fplCode, false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code
        Assert.AreEqual(expected, result.Length)
        prepareFplCode("", true) |> ignore
