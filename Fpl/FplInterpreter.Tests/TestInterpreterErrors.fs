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
    member this.TestGEN00() =
        let code = GEN00 ""
        runTestHelper """x;""" code 1

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
        let st = SymbolTable(parsedAsts, true)
        FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code.Code
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
        let st = SymbolTable(parsedAsts, true)
        FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore 
        let result = filterByErrorCode FplParser.parserDiagnostics code.Code
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
            let st = SymbolTable(parsedAsts, true)
            FplInterpreter.fplInterpreter st A uri fplLibUrl |> ignore
            Some (st)

    [<TestMethod>]
    member this.TestNSP04CircularAA() =
        let code = NSP04 "Test1_A -> Test1_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP04CircularAA(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code.Code
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
            let st = SymbolTable(parsedAsts, true)
            FplInterpreter.fplInterpreter st A uri fplLibUrl |> ignore
            Some (st)

    [<TestMethod>]
    member this.TestNSP04CircularABCA() =
        let code = NSP04 "Test2_A -> Test2_B -> Test2_C -> Test2_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP04CircularABCA(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code.Code
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
            let st = SymbolTable(parsedAsts, true)
            FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
            Some (st)

    [<TestMethod>]
    member this.TestNSP04NonCircular() =
        this.PrepareTestNSP04NonCircular(false) |> ignore
        let code = NSP04 ""
        let result = filterByErrorCode FplParser.parserDiagnostics code.Code
        Assert.AreEqual(0, result.Length)
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
            let st = SymbolTable(parsedAsts, true)
            FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
            Some (st)

    [<TestMethod>]
    member this.TestNSP05() =
        let code = NSP05 ( ["./"; "https"], "Fpl.Commons", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code.Code
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
            let st = SymbolTable(parsedAsts, true)
            FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
            Some (st)

    [<TestMethod>]
    member this.TestNSP05a() =
        let code = NSP05 (["./"; "./lib"; "https"], "Fpl.Commons", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05a(false) |> ignore
        let result = filterByErrorCode FplParser.parserDiagnostics code.Code
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
            let st = SymbolTable(parsedAsts, true)
            FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
            Some (st)

    [<TestMethod>]
    member this.TestNSP05CrossCheck() =
        let code = NSP05 (["./"], "Test", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05CrossCheck(false) |> ignore
        Assert.AreEqual(0, FplParser.parserDiagnostics.CountDiagnostics)
        this.PrepareTestNSP05CrossCheck(true) |> ignore


    [<DataRow("theorem SomeTheorem() {true} theorem SomeTheorem() {true} ;", 1)>]
    [<DataRow("theorem SomeTheorem() {true} ;", 0)>]

    [<DataRow("lemma SomeLemma() {true} lemma SomeLemma() {true} ;", 1)>]
    [<DataRow("lemma SomeLemma() {true} ;", 0)>]

    [<DataRow("conj TestId() {true} conj TestId() {true} ;", 1)>]
    [<DataRow("conj TestId() {true} ;", 0)>]

    [<DataRow("prop TestId() {true} prop TestId() {true} ;", 1)>]
    [<DataRow("prop TestId() {true} ;", 0)>]

    [<DataRow("axiom TestId() {true} axiom TestId() {true} ;", 1)>]
    [<DataRow("axiom TestId() {true} ;", 0)>]

    [<DataRow("axiom TestId() {true} postulate TestId() {true} ;", 1)>]

    [<DataRow("def pred TestId() {true} postulate TestId() {true} ;", 1)>]

    [<DataRow("theorem TestId() {true} postulate TestId() {true} ;", 1)>]

    [<DataRow("theorem TestId() {true} def pred TestId() {true} ;", 1)>]

    [<DataRow("postulate SomePostulate() {true} postulate SomePostulate() {true} ;", 1)>]
    [<DataRow("postulate SomePostulate() {true} ;", 0)>]

    [<DataRow("def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} ;", 1)>]
    [<DataRow("def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} ;", 0)>]

    [<DataRow("def class SomeClass:obj {intrinsic} def class SomeClass:obj {intrinsic} ;", 1)>]
    [<DataRow("def class SomeClass:obj {intrinsic} ;", 0)>]

    [<DataRow("def predicate SomePredicate() {intrinsic} def predicate SomePredicate() {intrinsic} ;", 1)>]
    [<DataRow("def predicate SomePredicate() {intrinsic} ;", 0)>]

    [<DataRow("inf SomeRuleOfInference() {pre: true con: true} inf SomeRuleOfInference() {pre: true con: true};", 1)>]
    [<DataRow("inf SomeRuleOfInference() {pre: true con: true} ;", 0)>]

    [<DataRow("corollary SomeCorollary$1() {true} corollary SomeCorollary$1() {true};", 1)>]
    [<DataRow("corollary SomeCorollary$1$2() {true} corollary SomeCorollary$1$2() {true};", 1)>]
    [<DataRow("corollary SomeCorollary$1$1$2() {true} corollary SomeCorollary$1$1$2() {true};", 1)>]
    [<DataRow("corollary SomeCorollary$1() {true} ;", 0)>]
    [<DataRow("corollary SomeCorollary$1$2() {true} ;", 0)>]
    [<DataRow("corollary SomeCorollary$1$1$2() {true} ;", 0)>]

    [<DataRow("proof TestId$1 {1. |- trivial} proof TestId$1 {1. |- trivial};", 1)>]
    [<DataRow("proof TestId$1$2 {1. |- trivial} proof TestId$1$2 {1. |- trivial};", 1)>]
    [<DataRow("proof TestId$1$2$3 {1. |- trivial} proof TestId$1$2$3 {1. |- trivial};", 1)>]
    [<DataRow("proof TestId$1 {1. |- trivial} ;", 0)>]
    [<DataRow("proof TestId$1$2 {1. |- trivial} ;", 0)>]
    [<DataRow("proof TestId$1$2$3 {1. |- trivial} ;", 0)>]
    [<TestMethod>]
    member this.TestID001(fplCode:string, expected:int) =
        let code = ID001 ("", "")
        runTestHelper fplCode code expected

    [<DataRow("uses Fpl.Commons inf ModusPonens() {pre:true con:true} ;", 1)>]
    [<DataRow("uses Fpl.Commons theorem ModusTollens() {true} ;", 1)>]
    [<DataRow("uses Fpl.Commons def pred HypotheticalSyllogism() {true} ;", 1)>]
    [<DataRow("uses Fpl.Commons axiom DisjunctiveSyllogism() {true} ;", 1)>]
    [<TestMethod>]
    member this.TestID001ConflictWithOtherTheories(fplCode:string, expected:int) =
        let code = ID001 ("", "")
        runTestHelper fplCode code expected

    [<DataRow("def predicate Test(x,y:* pred) {true};", 1)>]
    [<DataRow("def predicate Test(x,y:+ pred) {true};", 1)>]
    [<DataRow("def predicate Test(x,y: pred) {true};", 0)>]
    [<DataRow("def predicate Test(x:* pred) {true};", 0)>]
    [<DataRow("def predicate Test(x:+ pred) {true};", 0)>]
    [<TestMethod>]
    member this.TestVAR00(fplCode:string, expected) =
        let code = VAR00
        runTestHelper fplCode code expected

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
    [<DataRow("inf ModusPonens() {dec ~p,q: pred; pre: and (p, impl (p,q) ) con: q};", 0)>]
    [<DataRow("def pred Test() {true prty pred X(y:+ pred) {dec ~x:obj; ex x {true}} };", 0)>]
    [<DataRow("def pred Test() {true prty func X(y:+ pred)->obj {dec ~x:obj; ex x {true}} };", 0)>]
    [<TestMethod>]
    member this.TestVAR01(fplCode:string, expected) =
        let code = VAR01 ("","")
        runTestHelper fplCode code expected

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
        let code = VAR02 ("", "")
        runTestHelper fplCode code expected

    [<DataRow("theorem TestId(x: ind) {true}       proof TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("theorem TestId() {dec ~x:ind; true} proof TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("theorem TestId(x: ind) {true}       proof TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("theorem TestId() {dec ~x:ind; true} proof TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("theorem TestId(x: ind) {true}       corollary TestId$1() { dec ~x:obj; true };", 1)>]
    [<DataRow("theorem TestId() {dec ~x:ind; true} corollary TestId$1() { dec ~x:obj; true };", 1)>]
    [<DataRow("theorem TestId(x: ind) {true}       corollary TestId$1() { true };", 0)>]
    [<DataRow("theorem TestId() {dec ~x:ind; true} corollary TestId$1() { true };", 0)>]
    [<DataRow("theorem TestId(x: ind) {true}       corollary TestId$1(x:obj) { true };", 1)>]
    [<DataRow("theorem TestId() {dec ~x:ind; true} corollary TestId$1(x:obj) { true };", 1)>]
    [<DataRow("theorem TestId(x: ind) {true}       corollary TestId$1() { true };", 0)>]
    [<DataRow("theorem TestId() {dec ~x:ind; true} corollary TestId$1() { true };", 0)>]
    [<TestMethod>]
    member this.TestVAR03(fplCode:string, expected) =
        let code = VAR03 ("", "")
        runTestHelper fplCode code expected

    [<DataRow("axiom Test() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("postulate Test() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("conjecture Test() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("def pred Test() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("def cl Test:obj {intr} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("def func Test()->obj {intr} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("proof Test$1 {1. |- trivial} proof Test$1$1 {1. |- trivial};", 1)>]
    [<DataRow("theorem Test() {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("lemma Test() {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("proposition Test() {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("corollary Test$1() {true} proof Test$1$1 {1. |- trivial};", 0)>]
    [<TestMethod>]
    member this.TestID002(fplCode:string, expected) =
        let code = ID002 ("","")
        runTestHelper fplCode code expected

    [<DataRow("def pred Test() {true} corollary Test$1() {true};", 1)>]
    [<DataRow("def cl Test:obj {intr} corollary Test$1() {true};", 1)>]
    [<DataRow("def func Test()->obj {intr} corollary Test$1() {true};", 1)>]
    [<DataRow("proof Test$1 {1. |- trivial} corollary Test$1$1() {true};", 1)>] // corollaries of proofs are not allowed
    [<DataRow("theorem Test() {true} corollary Test$1() {true};", 0)>]
    [<DataRow("lemma Test() {true} proof corollary Test$1() {true};", 0)>]
    [<DataRow("proposition Test() {true} corollary Test$1() {true};", 0)>]
    [<DataRow("axiom Test() {true} corollary Test$1() {true};", 0)>] // corollaries of axioms are allowed
    [<DataRow("postulate Test() {true} corollary Test$1() {true};", 0)>] // corollaries of postulates (axioms) not allowed
    [<DataRow("conjecture Test() {true} corollary Test$1() {true};", 0)>] // corollaries of conjectures are not allowed
    [<DataRow("corollary Test$1() {true} corollary Test$1$1() {true};", 0)>] // corollaries of corollaries are allowed
    [<TestMethod>]
    member this.TestID005(fplCode:string, expected) =
        let code = ID005 ("","")
        runTestHelper fplCode code expected

    [<DataRow("theorem Test() {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("theorem TestTypo() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<TestMethod>]
    member this.TestID003(fplCode:string, expected) =
        let code = ID003 ""
        runTestHelper fplCode code expected

    [<DataRow("theorem Test() {true} corollary Test$1() {true};", 0)>]
    [<DataRow("theorem TestTypo() {true} corollary Test$1() {true};", 1)>]
    [<TestMethod>]
    member this.TestID006(fplCode:string, expected) =
        let code = ID006 ""
        runTestHelper fplCode code expected

    [<DataRow("theorem Test() {true} lemma Test(x:obj) {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("theorem Test(x:ind) {true} theorem Test() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<TestMethod>]
    member this.TestID004(fplCode:string, expected) =
        let code = ID004 ("", "") 
        runTestHelper fplCode code expected

    [<DataRow("theorem Test() {true} lemma Test(x:obj) {true} corollary Test$1() {true};", 1)>]
    [<DataRow("theorem Test(x:ind) {true} theorem Test() {true} corollary Test$1() {true};", 1)>]
    [<TestMethod>]
    member this.TestID007(fplCode:string, expected) =
        let code = ID007 ("", "") 
        runTestHelper fplCode code expected

    [<DataRow("def cl Test:obj {ctor TestTypo(x:Nat) {self}};", 1)>]
    [<DataRow("def cl Test:obj {ctor TestTypo1() {self}};", 1)>]
    [<DataRow("def cl Test:obj {ctor Test() {self}};", 0)>]
    [<TestMethod>]
    member this.TestID008(fplCode:string, expected) =
        let code = ID008 ("", "") 
        runTestHelper fplCode code expected

    [<DataRow("def cl Test:obj {intr};", 0)>]
    [<DataRow("def cl Test:Test {intr};", 1)>]
    [<DataRow("def cl Test:Test1, Test2, Test3 {intr};", 0)>]
    [<DataRow("def cl Test:Test1, Test2, Test3, Test {intr};", 1)>]
    [<TestMethod>]
    member this.TestID009(fplCode:string, expected) =
        let code = ID009 ""
        runTestHelper fplCode code expected

    [<DataRow("def cl Test:obj {intr};", 0)>]
    [<DataRow("def cl Test:Set {intr};", 1)>]
    [<DataRow("def class Set: obj {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set {intr};", 0)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:EmptySet {intr};", 0)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:EmptySetTypo {intr};", 1)>]
    [<TestMethod>]
    member this.TestID010(fplCode:string, expected) =
        let code = ID010 ""
        runTestHelper fplCode code expected

    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl C:B,A {intr};", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:EmptySet,Set {intr};", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set, EmptySet {intr};", 1)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl C:A,B {intr};", 1)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl C:B {intr};", 0)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:EmptySet {intr};", 0)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set {intr};", 0)>]
    [<DataRow("uses Fpl.Commons uses Fpl.SetTheory def cl Test:Set {intr};", 0)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl C:A {intr};", 0)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl C:A,A {intr};", 1)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl C:obj,object {intr};", 1)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {intr} def cl C:object,D,E,obj {intr};", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set,obj {intr};", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:EmptySet,obj {intr};", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:obj,Set {intr};", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:obj,EmptySet {intr};", 1)>]
    [<TestMethod>]
    member this.TestID011(fplCode:string, expected) =
        let code = ID011 ("","")
        runTestHelper fplCode code expected

    [<DataRow("def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); self} };", 1)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {ctor B() {dec base.A(); self} };", 0)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {ctor B() {dec base.C(); self} };", 1)>]
    [<DataRow("def cl A:obj { ctor A() {dec base.obj(); self} };", 0)>]
    [<DataRow("def cl A:obj { ctor A() {dec base.B(); self} };", 1)>]
    [<DataRow("def cl A:C { ctor A() {dec base.obj(); self} };", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.obj(); self} };", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); self} };", 0)>]
    [<TestMethod>]
    member this.TestID012(fplCode:string, expected) =
        let code = ID012 ("","")
        runTestHelper fplCode code expected

    [<DataRow("""def pred T infix "+" 0 () {true};""", 1)>]
    [<DataRow("""def pred T infix "+" 0 (x:obj) {true};""", 1)>]
    [<DataRow("""def pred T infix "+" 0 (x,y:obj) {true};""", 0)>]
    [<DataRow("""def pred T infix "+" 0 (x,y,z:obj) {true};""", 1)>]
    [<DataRow("""def func T infix "+" 0 ()->obj {intr};""", 1)>]
    [<DataRow("""def func T infix "+" 0 (x:obj)->obj {intr};""", 1)>]
    [<DataRow("""def func T infix "+" 0 (x,y:obj)->obj {intr};""", 0)>]
    [<DataRow("""def func T infix "+" 0 (x,y,z:obj)->obj {intr};""", 1)>]
    [<DataRow("""def pred T prefix "+" () {true};""", 1)>]
    [<DataRow("""def pred T prefix "+" (x:obj) {true};""", 0)>]
    [<DataRow("""def pred T prefix "+" (x,y:obj) {true};""", 1)>]
    [<DataRow("""def func T prefix "+" ()->obj {intr};""", 1)>]
    [<DataRow("""def func T prefix "+" (x:obj)->obj {intr};""", 0)>]
    [<DataRow("""def func T prefix "+" (x,y:obj)->obj {intr};""", 1)>]
    [<DataRow("""def pred T postfix "+" () {true};""", 1)>]
    [<DataRow("""def pred T postfix "+" (x:obj) {true};""", 0)>]
    [<DataRow("""def pred T postfix "+" (x,y:obj) {true};""", 1)>]
    [<DataRow("""def func T postfix "+" ()->obj {intr};""", 1)>]
    [<DataRow("""def func T postfix "+" (x:obj)->obj {intr};""", 0)>]
    [<DataRow("""def func T postfix "+" (x,y:obj)->obj {intr};""", 1)>]
    [<TestMethod>]
    member this.TestSIG00(fplCode:string, expected) =
        let code = SIG00 ("",0)
        runTestHelper fplCode code expected

    [<DataRow("""def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("""def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {+x};""", 0)>]
    [<DataRow("""def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {x+};""", 0)>]
    [<DataRow("""def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("""def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {-x};""", 1)>]
    [<DataRow("""def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {x-};""", 1)>]
    [<DataRow("""def pred T prefix "+" (x,y:obj) {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("""def pred T prefix "+" (x,y:obj) {true} def pred Test() {+x};""", 0)>]
    [<DataRow("""def pred T prefix "+" (x,y:obj) {true} def pred Test() {x+};""", 0)>]
    [<DataRow("""def pred T prefix "+" (x,y:obj) {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("""def pred T prefix "+" (x,y:obj) {true} def pred Test() {-x};""", 1)>]
    [<DataRow("""def pred T prefix "+" (x,y:obj) {true} def pred Test() {x-};""", 1)>]
    [<DataRow("""def pred T postfix "+" (x,y:obj) {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("""def pred T postfix "+" (x,y:obj) {true} def pred Test() {+x};""", 0)>]
    [<DataRow("""def pred T postfix "+" (x,y:obj) {true} def pred Test() {x+};""", 0)>]
    [<DataRow("""def pred T postfix "+" (x,y:obj) {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("""def pred T postfix "+" (x,y:obj) {true} def pred Test() {-x};""", 1)>]
    [<DataRow("""def pred T postfix "+" (x,y:obj) {true} def pred Test() {x-};""", 1)>]
    [<TestMethod>]
    member this.TestSIG01(fplCode:string, expected) =
        let code = SIG01 ""
        runTestHelper fplCode code expected

    [<DataRow("""def pred T infix "+" 1 (x,y:obj) {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("""def pred T1 infix "+" 1 (x,y:obj) {true} def pred T2 infix "+" 1 (x,y:obj) {true} def pred Test() {(x + y)};""", 1)>]
    [<TestMethod>]
    member this.TestSIG02(fplCode:string, expected) =
        let code = SIG02 ("",0, "")
        runTestHelper fplCode code expected
