namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers
open TestSharedConfig

[<TestClass>]
type TestInterpreterErrors() =

    member this.PrepareTestNSP05a (delete:bool) =
        ad.Clear()
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        deleteFiles currDir "Fpl.Commons.fpl"
        deleteFiles (Path.Combine(currDir, "lib")) "Fpl.Commons.fpl"
        File.WriteAllText(Path.Combine(currDir, "Fpl.Commons.fpl"), input)
        File.WriteAllText(Path.Combine(currDir, "lib", "Fpl.Commons.fpl"), input)
        let uri = PathEquivalentUri(Path.Combine(currDir, "Fpl.Commons.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            deleteFilesWithExtension (Path.Combine(currDir, "lib")) "fpl"
            None
        else
            let parsedAsts = ParsedAstList()
            let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
            FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
            Some (st)

    member this.PrepareTestNSP04CircularABCA(delete:bool) =
        ad.Clear()
        let A = """uses Test2_B;"""
        let B = """uses Test2_C;"""
        let C = """uses Test2_A;"""
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Test2_A.fpl"), A)
        File.WriteAllText(Path.Combine(currDir, "Test2_B.fpl"), B)
        File.WriteAllText(Path.Combine(currDir, "Test2_C.fpl"), C)
        let uri = PathEquivalentUri(Path.Combine(currDir, "Test2_A.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            None
        else
            let parsedAsts = ParsedAstList()
            let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
            FplInterpreter.fplInterpreter st A uri fplLibUrl |> ignore
            Some (st)

    member this.PrepareTestNSP04CircularAA(delete:bool) =
        ad.Clear()
        let A = """uses Test1_A;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test1_A.fpl")
        File.WriteAllText(pathToFile, A)
        let uri = PathEquivalentUri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            File.Delete(pathToFile)
            None
        else
            let parsedAsts = ParsedAstList()
            let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
            FplInterpreter.fplInterpreter st A uri fplLibUrl |> ignore
            Some (st)

    member this.PrepareTestNSP04NonCircular(delete:bool) =
        ad.Clear()
        let input = """
            uses Fpl.Commons
            uses Fpl.SetTheory
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test1_A.fpl")
        File.WriteAllText(pathToFile, input)
        let uri = PathEquivalentUri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            File.Delete(pathToFile)
            None
        else
            let parsedAsts = ParsedAstList()
            let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
            FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
            Some (st)

    member this.PrepareTestNSP05 (delete:bool) =
        ad.Clear()
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        deleteFiles currDir "Fpl.Commons.fpl"
        File.WriteAllText(Path.Combine(currDir, "Fpl.Commons.fpl"), input)
        let uri = PathEquivalentUri(Path.Combine(currDir, "Fpl.Commons.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            None
        else
            let parsedAsts = ParsedAstList()
            let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
            FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
            Some (st)

    member this.PrepareTestNSP05CrossCheck (delete:bool) =
        ad.Clear()
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Test.fpl"), input)
        let uri = PathEquivalentUri(Path.Combine(currDir, "Test.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            deleteFilesWithExtension (Path.Combine(currDir, "lib")) "fpl"
            None
        else
            let parsedAsts = ParsedAstList()
            let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
            FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
            Some (st)

    [<TestMethod>]
    member this.TestGEN00() =
        let code = GEN00 ""
        runTestHelper "TestGEN00.fpl" """x;""" code 1

    [<TestMethod>]
    member this.TestNSP00() =
        let code = NSP00 "Bla.fpl"
        printf "Trying %s" code.Message
        let input = """
        uses Bla 
        ;"""
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = PathEquivalentUri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let parsedAsts = ParsedAstList()
        let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
        FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)

    [<TestMethod>]
    member this.TestNSP03() =
        let code = NSP03 "T1"
        printf "Trying %s" code.Message
        let input = """
        uses Fpl1 alias T1
        uses Fpl2 alias T1
        ;"""
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = PathEquivalentUri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let parsedAsts = ParsedAstList()
        let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
        FplInterpreter.fplInterpreter st input uri fplLibUrl |> ignore 
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)

    [<TestMethod>]
    member this.TestNSP04CircularAA() =
        let code = NSP04 "Test1_A -> Test1_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP04CircularAA(false) |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)
        this.PrepareTestNSP04CircularAA(true) |> ignore

    [<TestMethod>]
    member this.TestNSP04CircularABCA() =
        let code = NSP04 "Test2_A -> Test2_B -> Test2_C -> Test2_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP04CircularABCA(false) |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)
        this.PrepareTestNSP04CircularABCA(true) |> ignore

    [<TestMethod>]
    member this.TestNSP04NonCircular() =
        this.PrepareTestNSP04NonCircular(false) |> ignore
        let code = NSP04 ""
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(0, result.Length)
        this.PrepareTestNSP04NonCircular(true) |> ignore

    [<TestMethod>]
    member this.TestNSP05() =
        let code = NSP05 ( ["./"; "https"], "Fpl.Commons", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05(false) |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)
        this.PrepareTestNSP05(true) |> ignore

    [<TestMethod>]
    member this.TestNSP05a() =
        let code = NSP05 (["./"; "./lib"; "https"], "Fpl.Commons", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05a(false) |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)
        this.PrepareTestNSP05a(true) |> ignore


    [<TestMethod>]
    member this.TestNSP05CrossCheck() =
        let code = NSP05 (["./"], "Test", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05CrossCheck(false) |> ignore
        Assert.AreEqual<int>(0, ad.CountDiagnostics)
        this.PrepareTestNSP05CrossCheck(true) |> ignore

    [<DataRow("theorem SomeTheorem {true} theorem SomeTheorem {true} ;", 1)>]
    [<DataRow("theorem SomeTheorem {true} ;", 0)>]

    [<DataRow("lemma SomeLemma {true} lemma SomeLemma {true} ;", 1)>]
    [<DataRow("lemma SomeLemma {true} ;", 0)>]

    [<DataRow("conj TestId {true} conj TestId {true} ;", 1)>]
    [<DataRow("conj TestId {true} ;", 0)>]

    [<DataRow("prop TestId {true} prop TestId {true} ;", 1)>]
    [<DataRow("prop TestId {true} ;", 0)>]

    [<DataRow("axiom TestId {true} axiom TestId {true} ;", 1)>]
    [<DataRow("axiom TestId {true} ;", 0)>]

    [<DataRow("axiom TestId {true} postulate TestId {true} ;", 1)>]

    [<DataRow("def pred TestId() {true} postulate TestId {true} ;", 0)>]

    [<DataRow("theorem TestId {true} postulate TestId {true} ;", 1)>]

    [<DataRow("theorem TestId {true} def pred TestId() {true} ;", 0)>]

    [<DataRow("postulate SomePostulate {true} postulate SomePostulate {true} ;", 1)>]
    [<DataRow("postulate SomePostulate {true} ;", 0)>]

    [<DataRow("def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} ;", 1)>]
    [<DataRow("def func SomeFunctionalTerm(x,y:pred(u,v,w:obj)) -> obj {intrinsic} ;", 0)>]

    [<DataRow("def class SomeClass def class SomeClass ;", 1)>]
    [<DataRow("def class SomeClass ;", 0)>]

    [<DataRow("def predicate SomePredicate() {intrinsic} def predicate SomePredicate() {intrinsic} ;", 1)>]
    [<DataRow("def predicate SomePredicate() {intrinsic} ;", 0)>]

    [<DataRow("inf SomeRuleOfInference {pre: true con: true} inf SomeRuleOfInference {pre: true con: true};", 1)>]
    [<DataRow("inf SomeRuleOfInference {pre: true con: true} ;", 0)>]

    [<DataRow("corollary SomeCorollary$1 {true} corollary SomeCorollary$1 {true};", 1)>]
    [<DataRow("corollary SomeCorollary$1$2 {true} corollary SomeCorollary$1$2 {true};", 1)>]
    [<DataRow("corollary SomeCorollary$1$1$2 {true} corollary SomeCorollary$1$1$2 {true};", 1)>]
    [<DataRow("corollary SomeCorollary$1 {true} ;", 0)>]
    [<DataRow("corollary SomeCorollary$1$2 {true} ;", 0)>]
    [<DataRow("corollary SomeCorollary$1$1$2 {true} ;", 0)>]

    [<DataRow("proof TestId$1 {1. |- trivial} proof TestId$1 {1. |- trivial};", 1)>]
    [<DataRow("proof TestId$1$2 {1. |- trivial} proof TestId$1$2 {1. |- trivial};", 1)>]
    [<DataRow("proof TestId$1$2$3 {1. |- trivial} proof TestId$1$2$3 {1. |- trivial};", 1)>]
    [<DataRow("proof TestId$1 {1. |- trivial} ;", 0)>]
    [<DataRow("proof TestId$1$2 {1. |- trivial} ;", 0)>]
    [<DataRow("proof TestId$1$2$3 {1. |- trivial} ;", 0)>]

    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits x@/\d+/ -> X {ret x};", 1)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} ;", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} def pred S() {true} ;", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} def pred T() {@1} ;", 1)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T(x:Digits) {true};", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T(x:Typo) {true};", 0)>]

    [<DataRow("def func Sum(list:* Nat[ind])->Nat {dec ~result: Nat; return result} def func Sum2(list:* Nat[ind])->Nat {dec ~result: Nat; return result};", 0)>]
    [<DataRow("""def cl B {intr} def cl A {dec ~x:obj; ctor A(y:B) {} };""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID001(fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID001 ("", "")
            runTestHelper "TestID001.fpl" fplCode code expected

    [<DataRow("""def pred Equal(x,y: tpl) infix "=" 50 {intr} ;""", 0)>]
    [<DataRow("uses Fpl.Commons inf ModusPonens {pre:true con:true} ;", 1)>]
    [<DataRow("uses Fpl.Commons theorem ModusTollens {true} ;", 1)>]
    [<DataRow("uses Fpl.Commons def pred HypotheticalSyllogism() {true} ;", 0)>]
    [<DataRow("uses Fpl.Commons axiom DisjunctiveSyllogism {true} ;", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID001ConflictWithOtherTheories(fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID001 ("", "")
            runTestHelper "TestID001ConflictWithOtherTheories.fpl" fplCode code expected

    [<DataRow("axiom Test {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("postulate Test {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("conjecture Test {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("def pred Test() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("def cl Test {intr} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("def func Test()->obj {intr} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("proof Test$1 {1. |- trivial} proof Test$1$1 {1. |- trivial};", 1)>]
    [<DataRow("theorem Test {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("lemma Test {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("proposition Test {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("corollary Test$1 {true} proof Test$1$1 {1. |- trivial};", 0)>]
    [<DataRow("inf T { pre: true con: true } proof T$1 {1. |- trivial};", 1)>]
    [<DataRow("ext T x@/\d+/ -> obj {ret x} proof T$1 {1. |- trivial};", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID002(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID002 ("","")
            runTestHelper "TestID002.fpl" fplCode code expected

    [<DataRow("theorem Test {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("theorem TestTypo {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("corollary Test$1 {true} proof Test$1$1 {1. |- trivial};", 0)>]
    [<DataRow("theorem Test {true} corollary Test$1 {true} proof Test$1$1 {1. |- trivial};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID003(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID003 ""
            runTestHelper "TestID003.fpl" fplCode code expected

    [<DataRow("def pred Test() {true} corollary Test$1 {true};", 1)>]
    [<DataRow("def cl Test {intr} corollary Test$1 {true};", 1)>]
    [<DataRow("def func Test()->obj {intr} corollary Test$1 {true};", 1)>]
    [<DataRow("proof Test$1 {1. |- trivial} corollary Test$1$1 {true};", 1)>] // corollaries of proofs are not allowed
    [<DataRow("theorem Test {true} corollary Test$1 {true};", 0)>]
    [<DataRow("lemma Test {true} corollary Test$1 {true};", 0)>]
    [<DataRow("proposition Test {true} corollary Test$1 {true};", 0)>]
    [<DataRow("axiom Test {true} corollary Test$1 {true};", 0)>] // corollaries of axioms are allowed
    [<DataRow("postulate Test {true} corollary Test$1 {true};", 0)>] // corollaries of postulates (axioms) not allowed
    [<DataRow("conjecture Test {true} corollary Test$1 {true};", 0)>] // corollaries of conjectures are not allowed
    [<DataRow("corollary Test$1 {true} corollary Test$1$1 {true};", 0)>] // corollaries of corollaries are allowed
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID005(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID005 ("","")
            runTestHelper "TestID005.fpl" fplCode code expected

    [<DataRow("""def cl A {intr} def pred T() {dec ~x:A x:=A; x};""", 1)>]
    [<DataRow("""def cl A {intr} def pred T() {dec ~x:A x:=A(); x};""", 0)>]
    [<DataRow("""def cl A {dec ~myX:obj; ctor A(x:obj) {dec myX:=x;}} def cl B:A { ctor B(x:obj) {dec base.A(del.Decrement(x)); } } def pred T() { dec ~v:B v:=B(@2); false};""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID004(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID004 ""
            runTestHelper "TestID004.fpl" fplCode code expected

    [<DataRow("theorem Test {true} corollary Test$1 {true};", 0)>]
    [<DataRow("theorem TestTypo {true} corollary Test$1 {true};", 1)>]
    [<DataRow("theorem Test {true} corollary Test$1 {true} corollary Test$1$1 {true};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID006(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID006 ""
            runTestHelper "TestID006.fpl" fplCode code expected

    // inheritance class from class
    [<DataRow("00a", "def cl A def cl T:A;", 0)>]
    [<DataRow("00b", "ax A {true} def cl T:A;", 1)>]
    [<DataRow("00c", "thm A {true} def cl T:A;", 1)>]
    [<DataRow("00d", "lem A {true} def cl T:A;", 1)>]
    [<DataRow("00e", "prop A {true} def cl T:A;", 1)>]
    [<DataRow("00f", "cor A$1 {true} def cl T:A;", 1)>]
    [<DataRow("00g", "proof A$1 {1. |- true} def cl T:A;", 1)>]
    [<DataRow("00h", "inf A {pre:true con:true} def cl T:A;", 1)>]
    [<DataRow("00i", "ext A x@/\d+/ -> obj {ret x} def cl T:A;", 1)>]
    [<DataRow("00k", "def pred A() {intr} def cl T:A;", 1)>]
    [<DataRow("00l", "def func A()->obj {intr} def cl T:A;", 1)>]
    [<DataRow("01", "def func T()->obj;", 0)>]
    [<DataRow("01a", "def func A()->obj def func T:A()->obj ;", 0)>]
    [<DataRow("01b", "def func A(x:obj)->obj def func T:A()->obj ;", 1)>]
    [<DataRow("01c", "def func A()->pred def func T:A()->obj ;", 1)>]
    // inheritance func from func
    [<DataRow("02a", "def cl A def func T:A()->obj;", 1)>]
    [<DataRow("02b", "ax A {true} def func T:A()->obj;", 1)>]
    [<DataRow("02c", "thm A {true} def func T:A()->obj;", 1)>]
    [<DataRow("02d", "lem A {true} def func T:A()->obj;", 1)>]
    [<DataRow("02e", "prop A {true} def func T:A()->obj;", 1)>]
    [<DataRow("02f", "cor A$1 {true} def func T:A()->obj;", 1)>]
    [<DataRow("02g", "proof A$1 {1. |- true} def func T:A()->obj;", 1)>]
    [<DataRow("02h", "inf A {pre:true con:true} def func T:A()->obj;", 1)>]
    [<DataRow("02i", "ext A x@/\d+/ -> obj {ret x} def func T:A()->obj;", 1)>]
    [<DataRow("02k", "def pred A() {intr} def func T:A()->obj;", 1)>]
    [<DataRow("02l", "def func A(x:obj)->obj {intr} def func T:A()->obj;", 1)>]
    // inheritance pred from pred
    [<DataRow("02a", "def cl A def pred T:A();", 1)>]
    [<DataRow("02b", "ax A {true} def pred T:A();", 1)>]
    [<DataRow("02c", "thm A {true} def pred T:A();", 1)>]
    [<DataRow("02d", "lem A {true} def pred T:A();", 1)>]
    [<DataRow("02e", "prop A {true} def pred T:A();", 1)>]
    [<DataRow("02f", "cor A$1 {true} def pred T:A();", 1)>]
    [<DataRow("02g", "proof A$1 {1. |- true} def pred T:A();", 1)>]
    [<DataRow("02h", "inf A {pre:true con:true} def pred T:A();", 1)>]
    [<DataRow("02i", "ext A x@/\d+/ -> obj {ret x} def pred T:A();", 1)>]
    [<DataRow("02k", "def pred A() {intr} def pred T:A();", 0)>]
    [<DataRow("02l", "def func A(x:obj)->obj {intr} def pred T:A();", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID007(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID007 ("", "", "", "") 
            runTestHelper "TestID007.fpl" fplCode code expected

    [<DataRow("def cl Test {ctor TestTypo(x:Nat) {}};", 1)>]
    [<DataRow("def cl Test {ctor TestTypo1() {}};", 1)>]
    [<DataRow("def cl Test {ctor Test() {}};", 0)>]
    [<DataRow("def cl Test {dec ~x:obj x := 0; ctor Test() {dec base.Obj(); }};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID008(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID008 ("", "") 
            runTestHelper "TestID008.fpl" fplCode code expected

    [<DataRow("def cl Test {intr};", 0)>]
    [<DataRow("def cl Test:Test {intr};", 1)>]
    [<DataRow("def cl Test:Test1, Test2, Test3 {intr};", 0)>]
    [<DataRow("def cl Test:Test1, Test2, Test3, Test {intr};", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID009(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID009 ""
            runTestHelper "TestID009.fpl" fplCode code expected

    [<DataRow("def cl Test {intr};", 0)>]
    [<DataRow("def cl Test:Set {intr};", 1)>]
    [<DataRow("def class Set def cl Test:Set {intr};", 0)>]
    [<DataRow("def cl Set {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("def cl Set {intr} def cl Test:SetTypo {intr};", 1)>]
    [<DataRow("def cl Set {intr} def pred Test() {dec ~x:Set; true};", 0)>]
    [<DataRow("def cl Set {intr} def pred Test() {dec ~x:object; is(x,Set)};", 0)>]

    [<DataRow("def cl A {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("def cl A {intr} thm T {true} proof T$1 {1. bydef B |- trivial };", 1)>]
    [<DataRow("thm A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("thm B {true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]

    // the following examples should not emit ID010 because this context is covered by the SIG04 diagnostics
    [<DataRow("def pred Test(x:Set) {intr};", 0)>]
    [<DataRow("def class Set def pred IsEmpty(x: Set) {true};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID010(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID010 ""
            runTestHelper "TestID010.fpl" fplCode code expected

    [<DataRow("00", "def cl A {intr};", 0)>]
    [<DataRow("01", "def cl A {intr} def cl B:A {intr};", 0)>]
    [<DataRow("02", "def cl A {intr} def cl B:A {intr} def cl C:B,A {intr};", 1)>]
    [<DataRow("03", "uses Fpl.SetTheory def cl Test:EmptySet,Set {intr};", 1)>]
    [<DataRow("04", "uses Fpl.SetTheory def cl Test:Set, EmptySet {intr};", 1)>]
    [<DataRow("05", "def cl A {intr} def cl B:A {intr} def cl C:A,B {intr};", 1)>]
    [<DataRow("06", "def cl A {intr} def cl B:A {intr} def cl C:B {intr};", 0)>]
    [<DataRow("07", "uses Fpl.SetTheory def cl Test:EmptySet {intr};", 0)>]
    [<DataRow("08", "uses Fpl.SetTheory def cl Test:Set {intr};", 0)>]
    [<DataRow("09", "uses Fpl.Commons uses Fpl.SetTheory def cl Test:Set {intr};", 0)>]
    [<DataRow("10", "def cl A {intr} def cl B:A {intr} def cl C:A {intr};", 0)>]
    [<DataRow("11", "def cl A {intr} def cl B:A {intr} def cl C:A,A {intr};", 1)>]
    [<DataRow("12", "def cl A {intr} def cl B:A {intr} def cl C {intr};", 0)>]
    [<DataRow("13", "def cl A {intr} def cl B:A {intr} def cl C:D,E {intr};", 0)>]
    [<DataRow("14", "uses Fpl.SetTheory def cl Test:Set {intr};", 0)>]
    [<DataRow("15", "uses Fpl.SetTheory def cl Test:EmptySet {intr};", 0)>]
    [<DataRow("16", "uses Fpl.SetTheory def cl Test:Set {intr};", 0)>]
    [<DataRow("17", "uses Fpl.SetTheory def cl Test:EmptySet {intr};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID011(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID011 ("","")
            runTestHelper "TestID011.fpl" fplCode code expected

    // class properties
    [<DataRow("C1", "def cl A {intr prty pred L() } def pred T() {dec ~x:A; x.L()};", 0)>]
    [<DataRow("C1a", "def cl A {intr prty pred L() } def pred T() {dec ~x:A; x.LTypo()};", 1)>]
    [<DataRow("C2", "def cl A {intr prty pred L() } def pred T(x:A) {x.L()};", 0)>]
    [<DataRow("C2a", "def cl A {intr prty pred L() } def pred T(x:A) {x.LTypo()};", 1)>]
    [<DataRow("C3", "def cl A {intr prty func L()->ind } def pred T() {dec ~x:A; x.L()};", 0)>]
    [<DataRow("C3a", "def cl A {intr prty func L()->ind } def pred T() {dec ~x:A; x.LTypo()};", 1)>]
    [<DataRow("C3", "def cl A {intr prty func L()->ind } def pred T(x:A) {x.L()};", 0)>]
    [<DataRow("C3a", "def cl A {intr prty func L()->ind } def pred T(x:A) {x.LTypo()};", 1)>]

    // inherted class properties
    [<DataRow("IC1", "def cl A {intr prty pred L() } def cl B:A def pred T() {dec ~x:B; x.L()};", 0)>]
    [<DataRow("IC1a", "def cl A {intr prty pred L() } def cl B:A def pred T() {dec ~x:B; x.LTypo()};", 1)>]
    [<DataRow("IC2", "def cl A {intr prty pred L() } def cl B:A def pred T(x:B) {x.L()};", 0)>]
    [<DataRow("IC2a", "def cl A {intr prty pred L() } def cl B:A def pred T(x:B) {x.LTypo()};", 1)>]
    [<DataRow("IC3", "def cl A {intr prty func L()->ind } def cl B:A def pred T() {dec ~x:B; x.L()};", 0)>]
    [<DataRow("IC3a", "def cl A {intr prty func L()->ind } def cl B:A def pred T() {dec ~x:B; x.LTypo()};", 1)>]
    [<DataRow("IC4", "def cl A {intr prty func L()->ind } def cl B:A def pred T(x:B) {x.L()};", 0)>]
    [<DataRow("IC4a", "def cl A {intr prty func L()->ind } def cl B:A def pred T(x:B) {x.LTypo()};", 1)>]

    // class instance properties
    [<DataRow("I1", "def cl A {intr prty pred L() } def pred T() {dec ~x:A x:=A(); x.L()};", 0)>]
    [<DataRow("I1a", "def cl A {intr prty pred L() } def pred T() {dec ~x:A x:=A(); x.LTypo()};", 1)>]
    [<DataRow("I2", "def cl A {intr prty pred L() } def pred T(x:A) {dec x:=A(); x.L()};", 0)>]
    [<DataRow("I2a", "def cl A {intr prty pred L() } def pred T(x:A) {dec x:=A(); x.LTypo()};", 1)>]
    [<DataRow("I3", "def cl A {intr prty func L()->ind } def pred T() {dec ~x:A x:=A(); x.L()};", 0)>]
    [<DataRow("I3a", "def cl A {intr prty func L()->ind } def pred T() {dec ~x:A x:=A(); x.LTypo()};", 1)>]
    [<DataRow("I4", "def cl A {intr prty func L()->ind } def pred T(x:A) {dec x:=A(); x.L()};", 0)>]
    [<DataRow("I4a", "def cl A {intr prty func L()->ind } def pred T(x:A) {dec x:=A(); x.LTypo()};", 1)>]

    // inherited class instance properties
    [<DataRow("II1", "def cl A {intr prty pred L() } def cl B:A def pred T() {dec ~x:B x:=B(); x.L()};", 0)>]
    [<DataRow("II1a", "def cl A {intr prty pred L() } def cl B:A def pred T() {dec ~x:A x:=B(); x.LTypo()};", 1)>]
    [<DataRow("II2", "def cl A {intr prty pred L() } def cl B:A def pred T(x:B) {dec x:=B(); x.L()};", 0)>]
    [<DataRow("II2a", "def cl A {intr prty pred L() } def cl B:A def pred T(x:B) {dec x:=B(); x.LTypo()};", 1)>]
    [<DataRow("II3", "def cl A {intr prty func L()->ind } def cl B:A def pred T() {dec ~x:B x:=B(); x.L()};", 0)>]
    [<DataRow("II3a", "def cl A {intr prty func L()->ind } def cl B:A def pred T() {dec ~x:A x:=B(); x.LTypo()};", 1)>]
    [<DataRow("II4", "def cl A {intr prty func L()->ind } def cl B:A def pred T(x:B) {dec x:=B(); x.L()};", 0)>]
    [<DataRow("II4a", "def cl A {intr prty func L()->ind } def cl B:A def pred T(x:B) {dec x:=B(); x.LTypo()};", 1)>]

    // predicate properties
    [<DataRow("P1", "def pred A() {intr prty pred L() } def pred T() {dec ~x:A; x.L()};", 0)>]
    [<DataRow("P1a", "def pred A() {intr prty pred L() } def pred T() {dec ~x:A; x.LTypo()};", 1)>]
    [<DataRow("P2", "def pred A() {intr prty pred L() } def pred T(x:A) {x.L()};", 0)>]
    [<DataRow("P2a", "def pred A() {intr prty pred L() } def pred T(x:A) {x.LTypo()};", 1)>]
    [<DataRow("P3", "def pred A() {intr prty func L()->ind } def pred T() {dec ~x:A; x.L()};", 0)>]
    [<DataRow("P3a", "def pred A() {intr prty func L()->ind } def pred T() {dec ~x:A; x.LTypo()};", 1)>]
    [<DataRow("P4", "def pred A() {intr prty func L()->ind } def pred T(x:A) {x.L()};", 0)>]
    [<DataRow("P4a", "def pred A() {intr prty func L()->ind } def pred T(x:A) {x.LTypo()};", 1)>]

    // inherited predicate properties
    [<DataRow("IP1", "def pred A() {intr prty pred L() } def pred B:A() def pred T() {dec ~x:B; x.L()};", 0)>]
    [<DataRow("IP1a", "def pred A() {intr prty pred L() } def pred B:A() def pred T() {dec ~x:B; x.LTypo()};", 1)>]
    [<DataRow("IP2", "def pred A() {intr prty pred L() } def pred B:A() def pred T(x:B) {x.L()};", 0)>]
    [<DataRow("IP2a", "def pred A() {intr prty pred L() } def pred B:A() def pred T(x:B) {x.LTypo()};", 1)>]
    [<DataRow("IP3", "def pred A() {intr prty func L()->ind } def pred B:A() def pred T() {dec ~x:B; x.L()};", 0)>]
    [<DataRow("IP3a", "def pred A() {intr prty func L()->ind } def pred B:A() def pred T() {dec ~x:B; x.LTypo()};", 1)>]
    [<DataRow("IP4", "def pred A() {intr prty func L()->ind } def pred B:A() def pred T(x:B) {x.L()};", 0)>]
    [<DataRow("IP4a", "def pred A() {intr prty func L()->ind } def pred B:A() def pred T(x:B) {x.LTypo()};", 1)>]

    // functional term properties
    [<DataRow("F1", "def func A()->ind {intr prty pred L() } def pred T() {dec ~x:A; x.L()};", 0)>]
    [<DataRow("F1a", "def func A()->ind {intr prty pred L() } def pred T() {dec ~x:A; x.LTypo()};", 1)>]
    [<DataRow("F2", "def func A()->ind {intr prty pred L() } def pred T(x:A) {x.L()};", 0)>]
    [<DataRow("F2a", "def func A()->ind {intr prty pred L() } def pred T(x:A) {x.LTypo()};", 1)>]
    [<DataRow("F3", "def func A()->ind {intr prty func L()->ind } def pred T() {dec ~x:A; x.L()};", 0)>]
    [<DataRow("F3a", "def func A()->ind {intr prty func L()->ind } def pred T() {dec ~x:A; x.LTypo()};", 1)>]
    [<DataRow("F4", "def func A()->ind {intr prty func L()->ind } def pred T(x:A) {x.L()};", 0)>]
    [<DataRow("F4a", "def func A()->ind {intr prty func L()->ind } def pred T(x:A) {x.LTypo()};", 1)>]

    // inherited functional term properties
    [<DataRow("IF1", "def func A()->ind {intr prty pred L() } def func B:A()->ind def pred T() {dec ~x:B; x.L()};", 0)>]
    [<DataRow("IF1a", "def func A()->ind {intr prty pred L() } def func B:A()->ind def pred T() {dec ~x:B; x.LTypo()};", 1)>]
    [<DataRow("IF2", "def func A()->ind {intr prty pred L() } def func B:A()->ind def pred T(x:B) {x.L()};", 0)>]
    [<DataRow("IF2a", "def func A()->ind {intr prty pred L() } def func B:A()->ind def pred T(x:B) {x.LTypo()};", 1)>]
    [<DataRow("IF3", "def func A()->ind {intr prty func L()->ind } def func B:A()->ind def pred T() {dec ~x:B; x.L()};", 0)>]
    [<DataRow("IF3a", "def func A()->ind {intr prty func L()->ind } def func B:A()->ind def pred T() {dec ~x:B; x.LTypo()};", 1)>]
    [<DataRow("IF4", "def func A()->ind {intr prty func L()->ind } def func B:A()->ind def pred T(x:B) {x.L()};", 0)>]
    [<DataRow("IF4a", "def func A()->ind {intr prty func L()->ind } def func B:A()->ind def pred T(x:B) {x.LTypo()};", 1)>]

    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID012Properties(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID012 ("", "", "", "")
            runTestHelper "TestID012Properties.fpl" fplCode code expected

    // class variables
    [<DataRow("C1", "def cl A {dec ~a:obj; ctor A() {} } def pred T() {dec ~x:A; x.a};", 0)>]
    [<DataRow("C1a", "def cl A {dec ~a:obj; ctor A() {} } def pred T() {dec ~x:A; x.aTypo};", 1)>]
    [<DataRow("C2", "def cl A {dec ~a:obj; ctor A() {} } def pred T(x:A) {x.a};", 0)>]
    [<DataRow("C2a", "def cl A {dec ~a:obj; ctor A() {} } def pred T(x:A) {x.aTypo};", 1)>]

    // inherted class variables
    [<DataRow("IC1", "def cl A {dec ~a:obj; ctor A() {} } def cl B:A def pred T() {dec ~x:B; x.a};", 0)>]
    [<DataRow("IC1a", "def cl A {dec ~a:obj; ctor A() {} } def cl B:A def pred T() {dec ~x:B; x.aTypo};", 1)>]
    [<DataRow("IC2", "def cl A {dec ~a:obj; ctor A() {} } def cl B:A def pred T(x:B) {x.a};", 0)>]
    [<DataRow("IC2a", "def cl A {dec ~a:obj; ctor A() {} } def cl B:A def pred T(x:B) {x.aTypo};", 1)>]

    // class instance variables
    [<DataRow("I1", "def cl A {dec ~a:obj; ctor A() {} } def pred T() {dec ~x:A x:=A(); x.a};", 0)>]
    [<DataRow("I1a", "def cl A {dec ~a:obj; ctor A() {} } def pred T() {dec ~x:A x:=A(); x.aTypo};", 1)>]
    [<DataRow("I2", "def cl A {dec ~a:obj; ctor A() {} } def pred T(x:A) {dec x:=A(); x.a};", 0)>]
    [<DataRow("I2a", "def cl A {dec ~a:obj; ctor A() {} } def pred T(x:A) {dec x:=A(); x.aTypo};", 1)>]

    // inherited class instance variables
    [<DataRow("II1", "def cl A {dec ~a:obj; ctor A() {} } def cl B:A def pred T() {dec ~x:B x:=B(); x.a};", 0)>]
    [<DataRow("II1a", "def cl A {dec ~a:obj; ctor A() {} } def cl B:A def pred T() {dec ~x:A x:=B(); x.aTypo};", 1)>]
    [<DataRow("II2", "def cl A {dec ~a:obj; ctor A() {} } def cl B:A def pred T(x:B) {dec x:=B(); x.a};", 0)>]
    [<DataRow("II2a", "def cl A {dec ~a:obj; ctor A() {} } def cl B:A def pred T(x:B) {dec x:=B(); x.aTypo};", 1)>]

    // predicate variables
    [<DataRow("P1", "def pred A() {dec ~a:obj; true} def pred T() {dec ~x:A; x.a};", 0)>]
    [<DataRow("P1a", "def pred A() {dec ~a:obj; true} def pred T() {dec ~x:A; x.aTypo};", 1)>]
    [<DataRow("P2", "def pred A() {dec ~a:obj; true} def pred T(x:A) {x.a};", 0)>]
    [<DataRow("P2a", "def pred A() {dec ~a:obj; true} def pred T(x:A) {x.aTypo};", 1)>]

    // inherited predicate variables
    [<DataRow("IP1", "def pred A() {dec ~a:obj; true} def pred B:A() def pred T() {dec ~x:B; x.a};", 0)>]
    [<DataRow("IP1a", "def pred A() {dec ~a:obj; true} def pred B:A() def pred T() {dec ~x:B; x.aTypo};", 1)>]
    [<DataRow("IP2", "def pred A() {dec ~a:obj; true} def pred B:A() def pred T(x:B) {x.a};", 0)>]
    [<DataRow("IP2a", "def pred A() {dec ~a:obj; true} def pred B:A() def pred T(x:B) {x.aTypo};", 1)>]

    // functional term variables
    [<DataRow("F1", "def func A()->ind {dec ~a:obj; return $1} def pred T() {dec ~x:A; x.a};", 0)>]
    [<DataRow("F1a", "def func A()->ind {dec ~a:obj; return $1} def pred T() {dec ~x:A; x.aTypo};", 1)>]
    [<DataRow("F2", "def func A()->ind {dec ~a:obj; return $1} def pred T(x:A) {x.a};", 0)>]
    [<DataRow("F2a", "def func A()->ind {dec ~a:obj; return $1} def pred T(x:A) {x.aTypo};", 1)>]

    // inherited functional term variables
    [<DataRow("IF1", "def func A()->ind {dec ~a:obj; return $1} def func B:A()->ind def pred T() {dec ~x:B; x.a};", 0)>]
    [<DataRow("IF1a", "def func A()->ind {dec ~a:obj; return $1} def func B:A()->ind def pred T() {dec ~x:B; x.aTypo};", 1)>]
    [<DataRow("IF2", "def func A()->ind {dec ~a:obj; return $1} def func B:A()->ind def pred T(x:B) {x.a};", 0)>]
    [<DataRow("IF2a", "def func A()->ind {dec ~a:obj; return $1} def func B:A()->ind def pred T(x:B) {x.aTypo};", 1)>]

    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID012Variables(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID012 ("", "", "", "")
            runTestHelper "TestID012Variables.fpl" fplCode code expected

    [<DataRow("00", "def pred T() {del.Test()};", 1, "Unknown delegate `Test`")>]
    [<DataRow("01", "def pred T() {del.Test1(x,y)};", 1, "Unknown delegate `Test1`")>]
    [<DataRow("02", "def pred T() {del.Equal(x,y)};", 1, "Predicate `=` cannot be evaluated because the left argument is undefined.")>]
    [<DataRow("03", "def pred T(x:pred) {del.Equal(x,y)};", 1, "Predicate `=` cannot be evaluated because the right argument is undefined.")>]
    [<DataRow("04", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)};""", 0, "missing error message")>]
    [<DataRow("04a", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} def pred NotEqual(x,y: tpl) infix "<>" 60 {not (x = y)}; """, 0, "missing error message")>] 
    [<DataRow("05", "def pred T(x,y:pred) {del.Equal(true,y)};", 1, "Predicate `=` cannot be evaluated because the right argument is undetermined.")>]
    [<DataRow("06", "ax T {all x,y:obj {del.Equal(x,y)}};", 0, "missing error message")>]
    [<DataRow("06a", "def cl Nat {intr} ax T {all x,y:Nat {del.Equal(x,y)}};", 0, "missing error message")>]
    [<DataRow("06b", "ax T {all x,y:Bla {del.Equal(x,y)}};", 0, "Predicate `=` cannot be evaluated because the left argument is undefined.")>]
    [<DataRow("07", "ax T {exn$1 x:obj {del.Equal(x,@1)}};", 0, "missing error message")>]
    [<DataRow("07a", "def cl Nat {intr} ax T {exn$1 x:Nat {del.Equal(x,@1)}};", 0, "missing error message")>]
    [<DataRow("07b", "ax T {exn$1 x:obj {del.Equal(x,$1)}};", 0, "missing error message")>]
    [<DataRow("07b_", "def cl Nat {intr} ax T {exn$1 x:Nat {del.Equal(x,$1)}};", 0, "missing error message")>]
    [<DataRow("08", """ax T {all n:obj {exn$1 y:obj {del.Equal(y,n)}}};""", 0, "missing error message")>]
    [<DataRow("08a", """def cl Nat {intr} ax T {all n:Nat {exn$1 y:Nat {del.Equal(y,n)}}};""", 0, "missing error message")>]
    [<DataRow("09", """def func Add()->obj {intr} prop AddIsSomething {dec ~anotherAdd, op: Add; all n,m:obj { (add(n,m) = anotherAdd(n,m) )} };""", 0, "missing error message")>]
    [<DataRow("10", """def func Add()->obj {intr} prop AddIsSomething {dec ~anotherAdd: Add; all n,m:obj { (anotherAdd(n,m) = n) } };""", 0, "missing error message")>]
    [<DataRow("11", """def func Add()->obj {intr} prop AddIsSomething {dec ~anotherAdd: Add; all n,m:obj { (anotherAdd(n,@0) = n) } };""", 0, "missing error message")>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0, "missing error message")>]
    [<TestMethod>]
    member this.TestID013(no:string, fplCode:string, expected, expectedErrMsg:string) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID013 ""
            let errMsg = runTestHelperWithText "TestID013.fpl" fplCode code expected
            Assert.AreEqual<string>(expectedErrMsg, errMsg)


    [<DataRow("""loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;;""", 0)>]
    [<DataRow("""loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !tex: x " dann und nur dann wenn " y;;""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID014(fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID014 ("", "")
            runTestHelper "TestID014.fpl" fplCode code expected

    [<DataRow("00a", """def cl A {dec ~x:obj x:=parent; ctor A() {}};""", 1)>]
    [<DataRow("00b", """def cl A {ctor A() {dec ~x:obj x:=parent;}};""", 0)>]
    [<DataRow("00c", """def cl A {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("00d", """def cl A {intr property func T()->obj {dec ~x:obj x:=parent; return x } };""", 0)>]
    [<DataRow("00e", """def cl A {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("00f", """def cl A {intr property func T()->obj {dec ~x:obj x:=parent; return x } };""", 0)>]
    // todo: issure diagnostics when asserting is(parent,...), only asserting is(self,...) inside classes is allowed
    [<DataRow("01a", """def pred A() {dec assert is(parent,A); true };""", 1)>]
    [<DataRow("01b", """def pred A() {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("01c", """def pred A() {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("01d", """def pred A() {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("01e", """def pred A() {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("02a", """def func A()->obj {dec ~x:obj assert is(parent,A); return x};""", 1)>]
    [<DataRow("02b", """def func A()->obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("02c", """def func A()->obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("02d", """def func A()->obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("02e", """def func A()->obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("03", """axiom A {parent};""", 1)>]
    [<DataRow("04", """theorem A {parent};""", 1)>]
    [<DataRow("05", """lemma A {parent};""", 1)>]
    [<DataRow("06", """prop A {parent};""", 1)>]
    [<DataRow("08", """conj A {parent};""", 1)>]
    [<DataRow("09", """cor A$1 {parent};""", 1)>]
    [<DataRow("10", """prf A$1 {1. |- parent qed};""", 1)>]
    [<DataRow("11", """inf A {pre: true con: parent};""", 1)>]
    [<DataRow("12", """inf A {pre: parent con: true};""", 1)>]
    [<DataRow("13", """loc not(parent) := !tex: "\neg(" x ")";;""", 1)>]
    [<DataRow("14", """ext A x@/\d+/ -> obj {dec assert is(parent,A); ret x};""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID015(no:string, fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID015 ""
            runTestHelper "TestID015.fpl" fplCode code expected

    [<DataRow("00a", """def cl A {dec ~x:obj x:=self; ctor A() {}};""", 0)>]
    [<DataRow("00b", """def cl A {ctor A() {dec ~x:obj x:=self;}};""", 1)>]
    [<DataRow("00c", """def cl A {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("00d", """def cl A {intr property func T()->obj {dec ~x:obj x:=self; return x } };""", 0)>]
    [<DataRow("00e", """def cl A {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("00f", """def cl A {intr property func T()->obj {dec ~x:obj x:=self; return x } };""", 0)>]
    // todo: issure diagnostics when asserting is(self,...) outside classes
    [<DataRow("01a", """def pred A() {dec assert is(self,A); true };""", 0)>]
    [<DataRow("01b", """def pred A() {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("01c", """def pred A() {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("01d", """def pred A() {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("01e", """def pred A() {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("02a", """def func A()->obj {dec ~x:obj assert is(self,A); return x};""", 0)>]
    [<DataRow("02b", """def func A()->obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("02c", """def func A()->obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("02d", """def func A()->obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("02e", """def func A()->obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("03", """axiom A {self};""", 1)>]
    [<DataRow("04", """theorem A {self};""", 1)>]
    [<DataRow("05", """lemma A {self};""", 1)>]
    [<DataRow("06", """prop A {self};""", 1)>]
    [<DataRow("08", """conj A {self};""", 1)>]
    [<DataRow("09", """cor A$1 {self};""", 1)>]
    [<DataRow("10", """prf A$1 {1. |- self qed};""", 1)>]
    [<DataRow("11", """inf A {pre: true con: self};""", 1)>]
    [<DataRow("12", """inf A {pre: self con: true};""", 1)>]
    [<DataRow("13", """loc not(self) := !tex: "\neg(" x ")";;""", 1)>]
    [<DataRow("14", """ext A x@/\d+/ -> obj {dec assert is(self,A); ret x};""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID016(no: string, fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID016 ""
            runTestHelper "TestID016.fpl" fplCode code expected

    [<DataRow("00", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); } };", 1)>]
    [<DataRow("01", "def cl A {intr} def cl B:A {ctor B() {dec base.A(); } };", 0)>]
    [<DataRow("01a", "thm A {true} def cl A def cl B:A {ctor B() {dec base.A(); } };", 1)>]
    [<DataRow("02", "def cl A {intr} def cl B:A {ctor B() {dec base.C(); } };", 1)>]
    [<DataRow("03", "def cl A { ctor A() {dec base.Obj(); } };", 1)>]
    [<DataRow("04", "def cl A { ctor A() {dec base.B(); } };", 1)>]
    [<DataRow("05", "def cl A:C { ctor A() {dec base.Obj(); } };", 1)>]
    [<DataRow("07", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Obj(); } };", 1)>]
    [<DataRow("08", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); } };", 0)>]
    [<DataRow("50", "def pred A() {true} def pred A(x:obj) {true} def pred T(x:A) {intr};", 1)>]
    [<DataRow("51", "def pred A() {true} def func A(x:obj)->obj {intr} def pred T(x:A) {intr};", 1)>]
    [<DataRow("52", "def pred A() {true} def func A()->obj {intr} def pred T(x:A) {intr};", 1)>]
    [<DataRow("53", "def pred A() {true} def pred T(x:A) {intr};", 0)>]
    [<DataRow("54", "def func A(x:obj)->obj {intr} def pred T(x:A) {intr};", 0)>]
    [<DataRow("55", "def pred B() {true} def func A()->obj {intr} def pred T(x:A) {intr};", 0)>]
    [<DataRow("56", "def cl Set def cl SetRoster:Set def pred T(x:Set) {intr};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID017(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID017 ("","")
            runTestHelper "TestID017.fpl" fplCode code expected

    [<DataRow("00", @"ext Digits x @ /\d+/ -> N {return x} def pred T() {@1};", 0)>]
    [<DataRow("01", @"ext Alpha x @ /[a-z]+/ -> N {return x} ext Digits x @ /\d+/ -> P {ret x} def pred T() {@123};", 0)>]
    [<DataRow("02", @"ext Alpha x @ /[a-z]+/ -> N {return x} ext Digits x @ /\d+/ -> P {ret x} def pred T() {@abc};", 0)>]
    [<DataRow("03", @"ext Alpha x @ /[a-z]+/ -> N {return x} def pred T() {@123};", 1)>]
    [<DataRow("04", @"ext Digits x @ /\d+/ -> N {return x} def pred T() {@abc};", 1)>]
    [<DataRow("05", @"ext Alpha x @ /[a-z]+/ -> N {return x} def pred T() {@abc};", 0)>]
    [<DataRow("06", @"ext Digits x @ /\d+/ -> N {return (x = @0)};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID018(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID018 ""
            runTestHelper "TestID018.fpl" fplCode code expected

    [<DataRow("00", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.A(); } };", 2)>]
    [<DataRow("00a", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.B(); } };", 2)>]
    [<DataRow("00b", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.C(); } };", 2)>]
    [<DataRow("00c", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.A() base.B(); } };", 1)>]
    [<DataRow("00d", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.A() base.C(); } };", 1)>]
    [<DataRow("00e", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.B() base.C(); } };", 1)>]
    [<DataRow("00f", "def cl A {intr} def cl B {intr} def cl C {intr} def cl D:A,B,C {ctor D() {dec base.A() base.B() base.C(); } };", 0)>]
    [<DataRow("01", "def cl A {intr} def cl B:A {ctor B() {dec base.A(); } };", 0)>]
    [<DataRow("01a", "def cl A {intr} def cl B:A {ctor B() {} };", 1)>]
    [<DataRow("02", "def cl A { ctor A() {dec base.Obj(); } };", 0)>]
    [<DataRow("02a", "def cl A { ctor A() {} };", 0)>]
    [<DataRow("03", "def cl A { ctor A() {dec base.Obj(); } };", 0)>]
    [<DataRow("03a", "def cl A:C { ctor A() {dec base.Obj(); } };", 1)>]
    [<DataRow("03b", "def cl A {intr} def cl B:A {intr} def cl C:B { ctor C() {dec base.Obj(); } };", 1)>]
    [<DataRow("04", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Obj(); } };", 1)>]
    [<DataRow("04a", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); } };", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID020(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID020 ""
            runTestHelper "TestID020.fpl" fplCode code expected

    [<DataRow("01", "def cl A {intr} def cl B:A {ctor B() {dec base.A(); } };", 0)>]
    [<DataRow("01a", "def cl A {intr} def cl B:A {ctor B() {dec base.A() base.A(); } };", 1)>]
    [<DataRow("02", "def cl A { ctor A() {dec base.Obj(); } };", 0)>]
    [<DataRow("02a", "def cl A { ctor A() {dec base.Obj() base.Obj(); } };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID021(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID021 ""
            runTestHelper "TestID021.fpl" fplCode code expected


    [<DataRow("00", "def pred T() {intr};", 0)>]
    [<DataRow("00a", "def pred T() {intr property pred Surjective() {RightTotal()}};", 0)>]
    [<DataRow("01", "def cl T def cl D:T {ctor D() {dec ~x:ind base.T(x);}};", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID022(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID022 "" 
            runTestHelper "TestID022.fpl" fplCode code expected

    [<DataRow("00a", "def cl A {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("00b", "def cl A {intr} def cl B:A {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("01a", "lem A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("01b", "lem A {true} def pred A(x:obj) {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("02a", "cor A$1 {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 0)>]
    [<DataRow("02b", "proof A$1 {1. |-  trivial} cor A$11 {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID023(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID023 ""
            runTestHelper "TestID023.fpl" fplCode code expected

    [<DataRow("00a", """loc not(x) :=  !tex: "\neg(" x ")";;""", 0)>]
    [<DataRow("00b", """loc not(x) :=  !tex: "\neg(" x ")"; loc not(x) :=  !tex: "\neg(" x ")";;""", 1)>]
    [<DataRow("00c", """loc not(y) :=  !tex: "\neg(" x ")"; loc not(x) :=  !tex: "\neg(" x ")";;""", 1)>]
    [<DataRow("01a", """loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y;;""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID024(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID024 ("", "")
            runTestHelper "TestID024.fpl" fplCode code expected

    [<DataRow("00", """loc not(x) := !tex: "\not(x)" ; ax T { n };""", 0)>]
    [<DataRow("01e", """def cl A {intr} ax T { A };""", 1)>]
    [<DataRow("01f", """inf A {pre:true con:true} ax T { A };""", 1)>]
    [<DataRow("01g", """ax A {true} ax T { A };""", 1)>]
    [<DataRow("01h", """thm A {true} ax T { A };""", 1)>]
    [<DataRow("01i", """lem A {true} ax T { A };""", 1)>]
    [<DataRow("01j", """prop A {true} ax T { A };""", 1)>]
    [<DataRow("01k", """conj A {true} ax T { A };""", 1)>]
    [<DataRow("01l", """cor A$1 {true} ax T { A$1 };""", 1)>]
    [<DataRow("01m", """proof A$1 {1. |- trivial} ax T { A$1 };""", 1)>]
    [<DataRow("01n", """ext A x@/\d+/ -> obj {ret x} ax T { A };""", 1)>]
    [<DataRow("01o", """loc A := !tex: "\alpha" ; ax T { A };""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID025(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID025 ("", "")
            runTestHelper "TestID025.fpl" fplCode code expected
            
    [<DataRow("00", "def func T(list:* Nat[ind])->pred { dec ~result:pred for list in list { result:=true }; return result };", 1)>]
    [<DataRow("01", "def func T(list:* Nat[ind])->pred { dec ~result:pred for list in a { result:=true }; return result };", 0)>]
    [<DataRow("02", "def cl Set def cl Nat:Set def pred T(list:*Set[Nat]) { dec ~s:Set for s in list { i:=Succ(i) }; true };", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID027(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID027 ""
            runTestHelper "TestID027.fpl" fplCode code expected

    [<DataRow("00a", """def pred T() { true };""", 0)>]
    [<DataRow("00", """def pred T() { not true };""", 0)>]
    [<DataRow("01", """def pred T() { dec ~x:pred; not x };""", 0)>]
    [<DataRow("02", """def pred T() { dec ~x:pred; not (x) };""", 0)>]
    [<DataRow("03", """def pred T() { dec ~x:pred; not ((x)) };""", 0)>]
    [<DataRow("04", """def pred T() { dec ~x:pred; not (((x))) };""", 0)>]
    [<DataRow("05", """def pred T() { dec ~x:ind; not x };""", 1)>]
    [<DataRow("06", """def pred T() { all x:obj {true} };""", 0)>]
    [<DataRow("07", """inf ModusTollens {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("08", """inf ModusTollens {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("09", """inf ModusTollens {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("10", """inf ModusTollens {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not p};""", 0)>]
    [<DataRow("11", """def pred T() { and(true,and(x,true)) };""", 1)>]
    [<DataRow("12", """def pred T() { dec ~x:pred; and(and(true,x),true) };""", 0)>]
    [<DataRow("13", """def pred T() { or(false,or(x,false)) };""", 1)>]
    [<DataRow("14", """def pred T() { dec ~x:pred; or(or(false,x),false) };""", 0)>]
    [<DataRow("15", """def pred T() { impl(true,x) };""", 1)>]
    [<DataRow("16", """def pred T() { impl(true,true) };""", 0)>]
    [<DataRow("17", """def pred T() { dec ~x:pred; iif(true,x) };""", 0)>]
    [<DataRow("18", """def pred T() { iif(true,x) };""", 1)>]
    [<DataRow("19", """def pred T() { xor(xor(true,true),true) };""", 0)>]
    [<DataRow("20", """def pred T() { dec ~x,y:pred; xor(y,xor(x,z)) };""", 1)>]
    [<DataRow("21", """loc and(p,q) := !tex: p "\wedge" q;;""", 0)>]
    [<DataRow("22", """def class Set def pred In(x,y: Set) def pred IsEmpty(x: Set) { all y:Set { not In(y, x) } };""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestLG001(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("", "", "")
            runTestHelper "TestLG001.fpl" fplCode code expected

    [<DataRow("""axiom A {dec ~x,y:Nat; impl(x,y)};""", 29)>]
    [<TestMethod>]
    member this.TestLG001Position(fplCode:string, (expected:int64)) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("", "", "")
            prepareFplCode ("TestLG001Position.fpl", fplCode, false) |> ignore
            checkForUnexpectedErrors code
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<int64>(expected, result.Head.StartPos.Column)
        
    [<DataRow("00", """axiom A {dec ~x,y:obj; impl(x,y)};""", "Cannot evaluate `implication` because its argument `x` typed `obj` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("01", """def cl T {intr} axiom A {impl(T,true)};""", "Cannot evaluate `implication` because its argument `T` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("02", """axiom A {dec ~x,y:ind; impl(x,y)};""", "Cannot evaluate `implication` because its argument `x` typed `ind` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("03", """axiom A {dec ~x,y:func; impl(x,y)};""", "Cannot evaluate `implication` because its argument `x` typed `func` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("04", """axiom A {impl(x,y)};""", "Cannot evaluate `implication` because its argument `x` typed `undef` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("05", """axiom A {impl(T(),true)};""", "Cannot evaluate `implication` because its argument `T()` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<DataRow("06", """axiom A {impl(T,true)};""", "Cannot evaluate `implication` because its argument `T` could not be evaluated as a predicate. This issue might be subsequent to other errors to be resolved first.")>]
    [<TestMethod>]
    member this.TestLG001MsgSpecificity(no:string, fplCode:string, (expected:string)) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("", "", "")
            prepareFplCode ("TestLG001MsgSpecificity.fpl", fplCode, false) |> ignore
            checkForUnexpectedErrors code
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<string>(expected, result.Head.Message)


    [<DataRow("00", """def pred T() { true };""", 0)>]
    [<DataRow("01a", """def pred T() { self };""", 1)>]
    [<DataRow("01b", """def pred T() { self() };""", 1)>]
    [<DataRow("02", """def pred T(x:obj) { self(x) };""", 1)>]
    [<DataRow("03", """def func T()->obj { intr };""", 0)>]
    [<DataRow("03a", """def func T()->obj { return self };""", 1)>]
    [<DataRow("03b", """def func T()->obj { return self() };""", 1)>]
    [<DataRow("04", """def func T(x:obj)->obj { return self(x) };""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestLG002(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG002 ("",0)
            runTestHelper "TestLG002.fpl" fplCode code expected
            
    [<DataRow("00", """axiom T { true };""", 0)>]
    [<DataRow("01", """axiom T { false };""", 1)>]
    [<DataRow("02", """theorem T { true };""", 0)>]
    [<DataRow("03", """theorem T { false };""", 1)>]
    [<DataRow("04", """proposition T { true };""", 0)>]
    [<DataRow("05", """proposition T { false };""", 1)>]
    [<DataRow("06", """lemma T { true };""", 0)>]
    [<DataRow("07", """lemma T { false };""", 1)>]
    [<DataRow("08", """corollary T$1 { true };""", 0)>]
    [<DataRow("09", """corollary T$1 { false };""", 1)>]
    [<DataRow("10", """conjecture T { true };""", 0)>]
    [<DataRow("11", """conjecture T { false };""", 1)>]
    [<DataRow("12", """postulate T { true };""", 0)>]
    [<DataRow("13", """postulate T { false };""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestLG003(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG003 ("", "")
            runTestHelper "TestLG003.fpl" fplCode code expected

    [<DataRow("00", """axiom T { true };""", 0)>]
    [<DataRow("01", """axiom T {dec ~x:obj; true };""", 1)>]
    [<DataRow("02", """theorem T { true };""", 0)>]
    [<DataRow("03", """theorem T {dec ~x:obj; true };""", 1)>]
    [<DataRow("04", """proposition T { true };""", 0)>]
    [<DataRow("05", """proposition T {dec ~x:obj; true };""", 1)>]
    [<DataRow("06", """lemma T { true };""", 0)>]
    [<DataRow("07", """lemma T {dec ~x:obj; true };""", 1)>]
    [<DataRow("08", """corollary T$1 { true };""", 0)>]
    [<DataRow("09", """corollary T$1 {dec ~x:obj; true };""", 1)>]
    [<DataRow("10", """conjecture T { true };""", 0)>]
    [<DataRow("11", """conjecture T {dec ~x:obj; true };""", 1)>]
    [<DataRow("12", """postulate T { true };""", 0)>]
    [<DataRow("13", """postulate T {dec ~x:obj; true };""", 1)>]
    [<DataRow("14", """def pred T() { true };""", 0)>]
    [<DataRow("15", """def pred T(x:obj) { true };""", 0)>]
    [<DataRow("16", """def func T()->obj { intr };""", 0)>]
    [<DataRow("17", """def func T(x:obj)->obj { intr };""", 0)>]
    [<DataRow("18", """inf T { pre: true con: true };""", 0)>]
    [<DataRow("19", """inf T {dec ~x:obj; pre: true con: true };""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestLG004(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG004 "" 
            runTestHelper "TestLG004.fpl" fplCode code expected

    [<DataRow("00", """def pred T() { dec ~x:obj x:=x; true };""", 1)>]
    [<DataRow("01", """def pred T() { dec ~x,y:pred x(x):=x(x); true };""", 1)>]
    [<DataRow("02", """def pred T() { dec ~x,y:pred x(y):=x(y); true };""", 1)>]
    [<DataRow("03", """def pred T() { dec ~x:obj Succ(x):=Succ(x); true };""", 1)>]
    [<DataRow("04", """def pred T() { dec ~x,y:pred Succ(x,x):=Succ(x,y); true };""", 0)>]
    [<DataRow("05", """def pred T() { dec ~x,y:pred Succ(y,y):=Succ(y,y); true };""", 1)>]
    [<DataRow("arr0", """def pred T() { dec ~i:ind ~x:*pred[ind] x[i]:=x[i]; true };""", 1)>]
    [<DataRow("arr1", """def pred T() { dec ~i,j:ind ~x:*pred[ind] x[i]:=x[j]; true };""", 0)>]
    [<DataRow("arr2", """def pred T() { dec ~i,j:ind ~x:*pred[ind,ind] x[i,j]:=x[i , j]; true };""", 1)>]
    [<DataRow("arr3", """def pred T() { dec ~i,j:ind ~x:*pred[ind,ind] x[i,j]:=x[j , i]; true };""", 0)>]
    [<DataRow("arr4", """def pred T() { dec ~i:ind ~j:obj ~x:*pred[ind,obj] x[i,j]:=x[j , i]; true };""", 0)>]
    [<DataRow("arr5", """def pred T() { dec ~i:ind ~j:obj ~x:*pred[ind,obj] x[i,j]:=x[ i ,j]; true };""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestLG005(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG005 "" 
            runTestHelper "TestLG005.fpl" fplCode code expected

    [<DataRow("00", "def cl A {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("00a", "def cl A {intr} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("00b", "def cl A {intr} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("00c", "def cl A {intr} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("00d", "def cl A {intr} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("00e", "thm A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00f", "prop A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00g", "lem A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00h", "cor A$1 {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00i", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00j", "ax A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00k", "inf A {pre:true con:true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00l", """loc A := !tex: ""; thm T {true} proof T$1 {1. bydef A |- trivial };""", 1)>]
    [<DataRow("00m", "ext A x@/\d+/ -> obj {ret x} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00n", "cor A$1 {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("00o", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("01", "def pred A() {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("01a", "def pred A() {intr} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("01b", "def pred A() {intr} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("01c", "def pred A() {intr} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("01d", "def pred A() {intr} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("02", "def func A()->obj {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("02a", "def func A()->obj {intr} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("02b", "def func A()->obj {intr} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("02c", "def func A()->obj {intr} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("02d", "def func A()->obj {intr} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("03", "ax A {true} thm T {true} proof T$1 {1. byax A |- trivial };", 0)>]
    [<DataRow("03a", "ax A {true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("03b", "ax A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("03c", "ax A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("03d", "ax A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("03e", "thm A {true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03f", "prop A {true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03g", "lem A {true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03h", "cor A$1 {true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03i", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03j", "def pred A() {true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03k", "def func A()->obj {intr} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03l", "def cl A {intr} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03m", "conj A {true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03n", "inf A {pre:true con:true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("03o", """loc A := !tex: ""; thm T {true} proof T$1 {1. byax A |- trivial };""", 1)>]
    [<DataRow("03p", "ext A x@/\d+/ -> obj {ret x} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("04", "conj A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 0)>]
    [<DataRow("04a", "conj A {true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("04b", "conj A {true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("04c", "conj A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("04d", "conj A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("04e", "thm A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04f", "prop A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04g", "lem A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04h", "cor A$1 {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04i", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04j", "def pred A() {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04k", "def func A()->obj {intr} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04l", "def cl A {intr} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04m", "ax A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04n", "inf A {pre:true con:true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("04o", """loc A := !tex: ""; thm T {true} proof T$1 {1. byconj A |- trivial };""", 1)>]
    [<DataRow("04p", "ext A x@/\d+/ -> obj {ret x} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("05", "thm A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("05a", "thm A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("05b", "thm A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("05c", "thm A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("05d", "thm A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("05e", "cor A$1 {true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("05f", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("05g", "def pred A() {true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("05e", "def func A()->obj {intr} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("05f", "def cl A {intr} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("05g", "conj A {true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("05h", "inf A {pre:true con:true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("05i", """loc A := !tex: ""; thm T {true} proof T$1 {1. A |- trivial };""", 1)>]
    [<DataRow("05j", "ext A x@/\d+/ -> obj {ret x} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("06", "lem A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("06a", "lem A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("06b", "lem A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("06c", "lem A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("06d", "lem A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("07", "prop A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("07a", "prop A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("07b", "prop A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("07c", "prop A {true} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("08", "inf A {pre: true con: true} thm T {true} proof T$1 {1. byinf A |- trivial };", 0)>]
    [<DataRow("08a", "inf A {pre: true con: true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("08b", "inf A {pre: true con: true} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("08c", "inf A {pre: true con: true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("08d", "thm A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08e", "prop A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08f", "lem A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08g", "cor A$1 {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08h", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08i", "def pred A() {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08j", "def func A()->obj {intr} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08k", "def cl A {intr} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08l", "ax A {true} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("08m", """loc A := !tex: ""; thm T {true} proof T$1 {1. byinf A |- trivial };""", 1)>]
    [<DataRow("08n", "ext A x@/\d+/ -> obj {ret x} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
    [<DataRow("09", "proof A$1 {1. |- trivial } thm T {true} proof T$1 {1. A$1:1 |- trivial };", 0)>]
    [<DataRow("09a", "cor A$1 {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09b", "thm A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09c", "thm A {true} proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 0)>]
    [<DataRow("09d", "inf A {pre: true con: true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09e", "prop A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09f", "lem A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09g", "cor A$1 {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09h", "def pred A() {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09i", "def func A()->obj {intr} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09j", "def cl A {intr} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09k", "ax A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("09l", """loc A := !tex: ""; thm T {true} proof T$1 {1. A$1:1 |- trivial };""", 1)>]
    [<DataRow("09m", "ext A x@/\d+/ -> obj {ret x} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("10", "cor A$1 {true} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 0)>]
    [<DataRow("10a", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10b", "thm A {true} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10c", "thm A {true} proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 0)>]
    [<DataRow("10d", "inf A {pre: true con: true} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10e", "prop A {true} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10f", "lem A {true} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10g", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10h", "def pred A() {true} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10i", "def func A()->obj {intr} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10j", "def cl A {intr} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10k", "ax A {true} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    [<DataRow("10l", """loc A := !tex: ""; thm T {true} proof T$1 {1. bycor A$1 |- trivial };""", 1)>]
    [<DataRow("10m", "ext A x@/\d+/ -> obj {ret x} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
    
    [<DataRow("1a", "ax A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("1b", "conj A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("1c", "thm A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("1d", "lem A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("1e", "prop A {true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("1f", "cor A$1 {true} thm T {true} proof T$1 {1. bydef A$1 |- trivial };", 1)>]
    [<DataRow("1f_", "thm A {true} cor A$1 {true} thm T {true} proof T$1 {1. bydef A$1 |- trivial };", 1)>]
    
    
    [<DataRow("1g_", "thm A {true} proof A$1 {1. |- trivial } thm T {true} proof T$1 {1. bydef A$1 |- trivial };", 1)>]
    
    [<DataRow("1j", "inf A {pre: true con: true} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("0a", "def cl A {intr} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("0b", "def pred A() {intr} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("0c", "def func A()->obj {intr} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("1a", "ax A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("1b", "conj A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("1c", "thm A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("1d", "lem A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("1e", "prop A {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("1f", "cor A$1 {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("1f_", "thm A {true} cor A$1 {true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("1j", "inf A {pre: true con: true} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
    [<DataRow("2j", "cor A$1 {true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("3k", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("2k", "proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("3a", "def cl A {intr} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3b", "def pred A() {intr} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3c", "def func A()->obj {intr} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3d", "ax A {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3e", "conj A {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3f", "thm A {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3g", "lem A {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3h", "prop A {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3i", "inf A {pre: true con: true} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
    [<DataRow("3k_", "thm A {true} proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
  
    [<DataRow("z2k_", "thm A {true} proof A$1 {1. |- trivial} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("z3j", "cor A$1 {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 0)>]
    [<DataRow("z0a", "def cl A {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("z0b", "def pred A() {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("z0c", "def func A()->obj {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]

    [<DataRow("z1g_", "thm A {true} proof A$1 {1. |- trivial } thm T {true} proof T$1 {1. A$1:1 |- trivial };", 0)>]
    [<DataRow("z1h_", "thm A {true} proof A$1 {1. |- trivial } thm T {true} proof T$1 {1. A$1:2 |- trivial };", 0)>]
    [<DataRow("z2j_", "thm A {true} cor A$1 {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("z3j_", "thm A {true} cor A$1 {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR001(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR001 ("", "")
            runTestHelper "TestPR001.fpl" fplCode code expected

    [<DataRow("""proof T$1 { 100. |- assume and(x,y) 300. |- trivial 100. |- trivial qed};""", 1)>]
    [<DataRow("""proof T$1 { 1. |- trivial 1. |- trivial qed};""", 1)>]
    [<DataRow("""proof T$1 { 1. |- trivial 2. |- trivial qed};""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR003(fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR003 ("", "")
            runTestHelper "TestPR003.fpl" fplCode code expected

    [<DataRow("""proof T$1 { 1. |- trivial qed};""", 0)>]
    [<DataRow("""proof T$1 { 1. 2, 3 |- trivial qed};""", 0)>]
    [<DataRow("""proof T$1 { 1. 1, 1 |- trivial qed};""", 1)>]
    [<DataRow("""proof T$1 { 1. 1, 1, 1 |- trivial qed};""", 2)>]
    [<DataRow("""proof T$1 { 1. 1, 2, 1 |- trivial qed};""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR004(fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR004 ("", "")
            runTestHelper "TestPR004.fpl" fplCode code expected


    [<DataRow("""proof T$1 { 1. |- trivial qed};""", 0)>]
    [<DataRow("""proof T$1 { 1. 2, 3 |- trivial qed};""", 2)>]
    [<DataRow("""proof T$1 { 1. |- trivial 2. 1 |- trivial qed};""", 0)>]
    [<DataRow("""proof T$1 { 1. |- trivial 2. 1, 1a |- trivial qed};""", 1)>]
    [<DataRow("""proof T$1 { 1. 1, 1, 1 |- trivial qed};""", 3)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR005(fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR005 ""
            runTestHelper "TestPR005.fpl" fplCode code expected

    [<DataRow("00", """thm A {true} proof A$1 {1. |- trivial} proof T$1 { 1. |- trivial qed};""", 0)>]
    [<DataRow("01", """thm A {true} proof A$1 {1. |- trivial} proof T$1 { 1. A$1:2, A$1:3 |- trivial qed};""", 2)>]
    [<DataRow("02", """thm A {true} proof A$1 {1. |- trivial} proof T$1 { 1. |- trivial 2. A$1:1 |- trivial qed};""", 0)>]
    [<DataRow("03", """thm A {true} proof A$1 {1. |- trivial} proof T$1 { 1. |- trivial 2. A$1:1, A$1:1a |- trivial qed};""", 1)>]
    [<DataRow("04", """thm A {true} proof A$1 {1. |- trivial} proof T$1 { 1. A$1:2, A$1:2, A$1:1 |- trivial qed};""", 2)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR006(no: string, fplCode:string, expected:int) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR006 ("", "")
            runTestHelper "TestPR005.fpl" fplCode code expected

    [<DataRow("01", """theorem T { true } proof T$1 {1. |- trivial};""", 0)>]
    [<DataRow("01a", """theorem T { true };""", 1)>]
    [<DataRow("02", """proposition T { true } proof T$1 {1. |- trivial};""", 0)>]
    [<DataRow("02a", """proposition T { true };""", 1)>]
    [<DataRow("03", """lemma T { true } proof T$1 {1. |- trivial};""", 0)>]
    [<DataRow("03a", """lemma T { true };""", 1)>]
    [<DataRow("04", """corollary T$1 { true } proof T$1$1 {1. |- trivial};""", 0)>]
    [<DataRow("04a", """corollary T$1 { true };""", 1)>]
    [<DataRow("04b", """corollary T$1 { true } proof T$1$1 {1. bydef A |- trivial};""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR007(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR007 ("", "") 
            runTestHelper "TestPR007.fpl" fplCode code expected

    [<DataRow("01", """inference ModusPonens { dec ~p,q: pred; pre: and (p, impl (p,q) ) con: q } theorem T { true } proof T$1 {1. |- and (x, impl(x,z)) 2. ModusPonens |- z };""", 0)>]
    [<DataRow("01a", """inference ModusPonens { dec ~p,q: pred; pre: and (p, impl (p,q) ) con: q } theorem T { true } proof T$1 {1. |- or (x, z) 2. ModusPonens |- z };""", 1)>]
    [<DataRow("01b", """inference ModusPonens { dec ~p,q: pred; pre: and (p, impl (p,q) ) con: q } theorem T { true } proof T$1 {1. |- and (x, impl(y,z)) 2. ModusPonens |- z };""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR008(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR008 ("", "", "") 
            runTestHelper "TestPR008.fpl" fplCode code expected

    [<DataRow("01", """thm T { true } proof T$1 {1. |- trivial};""", 0)>]
    [<DataRow("02a", """thm T { true } proof T$1 {1. |- true};""", 0)>]
    [<DataRow("02b", """thm T { true } proof T$1 {1. 2 |- false};""", 1)>]
    [<DataRow("02c", """thm T { true } proof T$1 {1. B |- true};""", 1)>]
    [<DataRow("02d", """thm T { true } proof T$1 {1. bydef S |- false};""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR009(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR009 
            runTestHelper "TestPR009.fpl" fplCode code expected

    [<DataRow("00", "proof T$1 {1. byax A |- trivial};", 0)>]
    [<DataRow("01", "proof T$1 {1. byax A$1 |- trivial};", 1)>]
    [<DataRow("00", "proof T$1 {1. byconj A |- trivial};", 0)>]
    [<DataRow("01", "proof T$1 {1. byconj A$1 |- trivial};", 1)>]
    [<DataRow("04", "proof T$1 {1. bydef A |- trivial};", 0)>]
    [<DataRow("05", "proof T$1 {1. bydef A$1 |- trivial};", 1)>]
    [<DataRow("02", "proof T$1 {1. byinf A |- trivial};", 0)>]
    [<DataRow("03", "proof T$1 {1. byinf A$1 |- trivial};", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR010(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR010 ("", "")
            runTestHelper "TestPR010.fpl" fplCode code expected

    [<DataRow("00", "proof T$1 {1. byax A |- trivial};", 0)>]
    [<DataRow("01", "proof T$1 {1. byax A$1:3 |- trivial};", 1)>]
    [<DataRow("00", "proof T$1 {1. byconj A |- trivial};", 0)>]
    [<DataRow("01", "proof T$1 {1. byconj A$1:3 |- trivial};", 1)>]
    [<DataRow("04", "proof T$1 {1. bydef A |- trivial};", 0)>]
    [<DataRow("05", "proof T$1 {1. bydef A$1:3 |- trivial};", 1)>]
    [<DataRow("02", "proof T$1 {1. byinf A |- trivial};", 0)>]
    [<DataRow("03", "proof T$1 {1. byinf A$1:3 |- trivial};", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR011(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR011 ("", "")
            runTestHelper "TestPR011.fpl" fplCode code expected

    [<DataRow("00", "proof T$1 {1. bycor A$1 |- trivial};", 0)>]
    [<DataRow("01", "proof T$1 {1. bycor A |- trivial};", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR012(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR012 
            runTestHelper "TestPR012.fpl" fplCode code expected

    [<DataRow("00", "proof T$1 {1. bycor A$1 |- trivial};", 0)>]
    [<DataRow("01", "proof T$1 {1. A$1 |- trivial};", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR013(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR013 
            runTestHelper "TestPR013.fpl" fplCode code expected

    [<DataRow("00", "proof T$1 {1. A |- trivial};", 0)>]
    [<DataRow("01", "proof T$1 {1. A:2 |- trivial};", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR014(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR014 
            runTestHelper "TestPR014.fpl" fplCode code expected

    [<DataRow("00", "proof T$1 {1. |- assume x 2. |- revoke 1};", 0)>]
    [<DataRow("01", "proof T$1 {1. |- assume x 2. |- revoke 2};", 0)>] // its ok, 2 is not evaluated at this stage but this will issue PR005 anyway, 
    [<DataRow("02", "proof T$1 {1. |- assume x 2. |- trivial 3. |- revoke 2};", 1)>]
    [<DataRow("03", "proof T$1 {1. |- assume x 2. |- trivial 3. |- revoke 1};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR015(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR015 ""
            runTestHelper "TestPR015.fpl" fplCode code expected

    [<DataRow("00", "proof T$1 {1. |- assume x 2. |- trivial 3. |- revoke 2};", 0)>]
    [<DataRow("01", "proof T$1 {1. |- assume x 2. |- trivial 3. |- assume y 4. |- trivial 5. |- revoke 1};", 1)>]
    [<DataRow("02", "proof T$1 {1. |- assume x 2. |- trivial 3. |- assume y 4. |- trivial 5. |- revoke 3};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR016(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR016 ""
            runTestHelper "TestPR016.fpl" fplCode code expected
           
    [<DataRow("""def pred Or (x,y:*pred[obj]) infix "or" 0 {true};""", 0)>]
    [<DataRow("""def pred Or (x:* pred[ind]) infix "or" 0 {true};""", 1)>]
    [<DataRow("""def pred Or (x,y,z:* pred[ind]) infix "or" 0 {true};""", 1)>]
    [<DataRow("""def pred T() {true};""", 0)>]
    [<DataRow("""def pred T(x:obj) infix "+" 0 {true};""", 1)>]
    [<DataRow("""def pred T(x,y:obj) infix "+" 0{true};""", 0)>]
    [<DataRow("""def pred T(x,y,z:obj) infix "+" 0{true};""", 1)>]
    [<DataRow("""def pred T () prefix "+" {true};""", 1)>]
    [<DataRow("""def pred T(x:obj) prefix "+"  {true};""", 0)>]
    [<DataRow("""def pred T(x,y:obj) prefix "+" {true};""", 1)>]
    [<DataRow("""def pred T() postfix "+" {true};""", 1)>]
    [<DataRow("""def pred T(x:obj) postfix "+" {true};""", 0)>]
    [<DataRow("""def pred T(x,y:obj) postfix "+" {true};""", 1)>]
    [<DataRow("""def func T()->obj infix "+" 0{intr};""", 1)>]
    [<DataRow("""def func T(x:obj)->obj infix "+" 0{intr};""", 1)>]
    [<DataRow("""def func T(x,y:obj)->obj infix "+" 0{intr};""", 0)>]
    [<DataRow("""def func T(x,y,z:obj)->obj infix "+" 0{intr};""", 1)>]
    [<DataRow("""def func T()->obj prefix "+" {intr};""", 1)>]
    [<DataRow("""def func T(x:obj)->obj prefix "+" {intr};""", 0)>]
    [<DataRow("""def func T(x,y:obj)->obj prefix "+" {intr};""", 1)>]
    [<DataRow("""def func T()->obj postfix "+" {intr};""", 1)>]
    [<DataRow("""def func T(x:obj)->obj postfix "+" {intr};""", 0)>]
    [<DataRow("""def func T(x,y:obj)->obj postfix "+" {intr};""", 1)>]
    [<DataRow("""def func T(x,y:*pred[obj])->obj  infix "or" 0 {intr};""", 0)>]
    [<DataRow("""def func T(x:* pred[ind])->obj  infix "or" 0 {intr};""", 1)>]
    [<DataRow("""def func T(x,y,z:* pred[ind])->obj infix "or" 0 {intr};""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG00(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG00 ("",0)
            runTestHelper "TestSIG00.fpl" fplCode code expected

    [<DataRow("01", """def pred Equal(x,y:tpl) infix "=" 0 { delegate.Equal(x,y) } def pred NotEqual (x,y: tpl) { not (x = y) };""", 0)>]
    [<DataRow("02", """def pred NotEqual (x,y: tpl) { not (x = y) };""", 1)>]
    [<DataRow("03", """def pred T(x,y:obj) infix "+" 0  {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("04", """def pred T(x,y:obj) infix "+" 0  {true} def pred Test() {+x};""", 0)>]
    [<DataRow("05", """def pred T(x,y:obj) infix "+" 0 {true} def pred Test() {x+};""", 0)>]
    [<DataRow("06", """def pred T(x,y:obj) infix "+" 0 {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("07", """def pred T(x,y:obj) infix "+" 0 {true} def pred Test() {-x};""", 1)>]
    [<DataRow("08", """def pred T(x,y:obj) infix "+" 0 {true} def pred Test() {x-};""", 1)>]
    [<DataRow("09", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("10", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {+x};""", 0)>]
    [<DataRow("11", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {x+};""", 0)>]
    [<DataRow("12", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("13", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {-x};""", 1)>]
    [<DataRow("14", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {x-};""", 1)>]
    [<DataRow("15", """def pred T(x,y:obj) postfix "+"  {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("16", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {+x};""", 0)>]
    [<DataRow("17", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {x+};""", 0)>]
    [<DataRow("18", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("19", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {-x};""", 1)>]
    [<DataRow("20", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {x-};""", 1)>]
    [<DataRow("21", """loc (x + y) := !tex: x "+" y; ;""", 0)>]
    [<DataRow("22", """loc (x + y) := !tex: x "+" y; ;""", 0)>]
    [<DataRow("23", """def cl A symbol "0" {intr} axiom T {0} ;""", 0)>]
    [<DataRow("24", """def cl A symbol "1" {intr} axiom T {0} ;""", 1)>]
    [<DataRow("25", """def cl A {intr} axiom T {0} ;""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG01(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG01 ""
            runTestHelper "TestSIG01.fpl" fplCode code expected

    [<DataRow("""def pred T (x,y:obj) infix "+" 1 {intr};""", 0)>]
    [<DataRow("""def pred T1 (x,y:obj) infix "+" 1 {intr} def pred T2 (x,y:obj) infix "+" 1 {intr};""", 1)>]
    [<DataRow("""def pred T1  (x,y: obj) infix "+" 2 {intr} def pred T2 (x,y: obj) infix "*" 1 {intr};""", 0)>]
    [<DataRow("""def func T (x,y:obj)->obj infix "+" 1 {intr};""", 0)>]
    [<DataRow("""def func T1 (x,y:obj)->obj infix "+" 1 {intr} def pred T2 (x,y:obj) infix "+" 1 {intr};""", 1)>]
    [<DataRow("""def func T1  (x,y: obj)->obj infix "+" 2 {intr} def pred T2 (x,y: obj) infix "*" 1 {intr};""", 0)>]
    [<DataRow("""def pred T1 (x,y:obj) infix "+" 1 {intr} def func T2 (x,y:obj)->obj infix "+" 1 {intr};""", 1)>]
    [<DataRow("""def pred T1  (x,y: obj) infix "+" 2 {intr} def func T2 (x,y: obj)->obj infix "*" 1 {intr};""", 0)>]
    [<DataRow("""def func T1 (x,y:obj)->obj infix "+" 1 {intr} def func T2 (x,y:obj)->obj infix "+" 1 {intr};""", 1)>]
    [<DataRow("""def func T1  (x,y: obj)->obj infix "+" 2 {intr} def func T2 (x,y: obj)->obj infix "*" 1 {intr};""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG02(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG02 ("",0, "")
            runTestHelper "TestSIG02.fpl" fplCode code expected

    // match return with mapping having simple types
    [<DataRow("ST0", "def func Test()->obj {dec ~x:obj; return x};", 0)>]
    [<DataRow("ST1", "def func Test()->ind {dec ~x:ind; return x};", 0)>]
    [<DataRow("ST2", "def func Test()->func {dec ~x:func; return x};", 0)>]
    [<DataRow("ST2a", "def func Test()->func {dec ~x:func()->ind; return x};", 0)>]
    [<DataRow("ST2b", "def func Test()->func {dec ~x:func(y:obj)->ind; return x};", 0)>]
    [<DataRow("ST2c", "def func Test()->func {dec ~x:func(y:obj)->func; return x};", 0)>]
    [<DataRow("ST2d", "def func Test()->func {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 0)>]
    [<DataRow("ST3", "def func Test()->pred {dec ~x:pred; return x};", 0)>]
    [<DataRow("ST3a", "def func Test()->pred {dec ~x:pred(); return x};", 0)>]
    [<DataRow("ST3b", "def func Test()->pred {dec ~x:pred; return x};", 0)>]
    [<DataRow("ST3c", "def func Test()->pred {dec ~x:pred(y:obj); return x};", 0)>]

    // mismatch return with mapping having simple type obj
    [<DataRow("ST0_obj", "def func Test()->obj {dec ~x:obj; return x};", 0)>]
    [<DataRow("ST1_obj", "def func Test()->obj {dec ~x:ind; return x};", 1)>]
    [<DataRow("ST2_obj", "def func Test()->obj {dec ~x:func; return x};", 1)>]
    [<DataRow("ST2a_obj", "def func Test()->obj {dec ~x:func()->ind; return x};", 1)>]
    [<DataRow("ST2b_obj", "def func Test()->obj {dec ~x:func(y:obj)->ind; return x};", 1)>]
    [<DataRow("ST2c_obj", "def func Test()->obj {dec ~x:func(y:obj)->func; return x};", 1)>]
    [<DataRow("ST2d_obj", "def func Test()->obj {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 1)>]
    [<DataRow("ST3_obj", "def func Test()->obj {dec ~x:pred; return x};", 1)>]
    [<DataRow("ST3a_obj", "def func Test()->obj {dec ~x:pred(); return x};", 1)>]
    [<DataRow("ST3b_obj", "def func Test()->obj {dec ~x:pred; return x};", 1)>]
    [<DataRow("ST3c_obj", "def func Test()->obj {dec ~x:pred(y:obj); return x};", 1)>]

    // mismatch return with mapping having simple type ind
    [<DataRow("ST0_ind", "def func Test()->ind {dec ~x:obj; return x};", 1)>]
    [<DataRow("ST1_ind", "def func Test()->ind {dec ~x:ind; return x};", 0)>]
    [<DataRow("ST2_ind", "def func Test()->ind {dec ~x:func; return x};", 1)>]
    [<DataRow("ST2a_ind", "def func Test()->ind {dec ~x:func()->ind; return x};", 1)>]
    [<DataRow("ST2b_ind", "def func Test()->ind {dec ~x:func(y:obj)->ind; return x};", 1)>]
    [<DataRow("ST2c_ind", "def func Test()->ind {dec ~x:func(y:obj)->func; return x};", 1)>]
    [<DataRow("ST2d_ind", "def func Test()->ind {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 1)>]
    [<DataRow("ST3_ind", "def func Test()->ind {dec ~x:pred; return x};", 1)>]
    [<DataRow("ST3a_ind", "def func Test()->ind {dec ~x:pred(); return x};", 1)>]
    [<DataRow("ST3b_ind", "def func Test()->ind {dec ~x:pred; return x};", 1)>]
    [<DataRow("ST3c_ind", "def func Test()->ind {dec ~x:pred(y:obj); return x};", 1)>]

    // mismatch return with mapping having simple type pred
    [<DataRow("ST0_pred", "def func Test()->pred {dec ~x:obj; return x};", 1)>]
    [<DataRow("ST1_pred", "def func Test()->pred {dec ~x:ind; return x};", 1)>]
    [<DataRow("ST2_pred", "def func Test()->pred {dec ~x:func; return x};", 1)>]
    [<DataRow("ST2a_pred", "def func Test()->pred {dec ~x:func()->ind; return x};", 1)>]
    [<DataRow("ST2b_pred", "def func Test()->pred {dec ~x:func(y:obj)->pred; return x};", 1)>]
    [<DataRow("ST2c_pred", "def func Test()->pred {dec ~x:func(y:obj)->func; return x};", 1)>]
    [<DataRow("ST2d_pred", "def func Test()->pred {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 1)>]
    [<DataRow("ST3_pred", "def func Test()->pred {dec ~x:pred; return x};", 0)>]
    [<DataRow("ST3a_pred", "def func Test()->pred {dec ~x:pred(); return x};", 0)>]
    [<DataRow("ST3b_pred", "def func Test()->pred {dec ~x:pred; return x};", 0)>]
    [<DataRow("ST3c_pred", "def func Test()->pred {dec ~x:pred(y:obj); return x};", 0)>]

    // mismatch return with mapping having simple type func
    [<DataRow("ST0_func", "def func Test()->func {dec ~x:obj; return x};", 1)>]
    [<DataRow("ST1_func", "def func Test()->func {dec ~x:ind; return x};", 1)>]
    [<DataRow("ST2_func", "def func Test()->func {dec ~x:func; return x};", 0)>]
    [<DataRow("ST2a_func", "def func Test()->func {dec ~x:func()->ind; return x};", 0)>]
    [<DataRow("ST2b_func", "def func Test()->func {dec ~x:func(y:obj)->pred; return x};", 0)>]
    [<DataRow("ST2c_func", "def func Test()->func {dec ~x:func(y:obj)->func; return x};", 0)>]
    [<DataRow("ST2d_func", "def func Test()->func {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 0)>]
    [<DataRow("ST3_func", "def func Test()->func {dec ~x:pred; return x};", 1)>]
    [<DataRow("ST3a_func", "def func Test()->func {dec ~x:pred(); return x};", 1)>]
    [<DataRow("ST3b_func", "def func Test()->func {dec ~x:pred; return x};", 1)>]
    [<DataRow("ST3c_func", "def func Test()->func {dec ~x:pred(y:obj); return x};", 1)>]

    // (mis)match return with mapping having pred() types
    [<DataRow("NP0", "def func Test()->pred() {dec ~x:obj; return x};", 1)>]
    [<DataRow("NP1", "def func Test()->pred() {dec ~x:ind; return x};", 1)>]
    [<DataRow("NP2", "def func Test()->pred() {dec ~x:func; return x};", 1)>]
    [<DataRow("NP2a", "def func Test()->pred() {dec ~x:func()->ind; return x};", 1)>]
    [<DataRow("NP2b", "def func Test()->pred() {dec ~x:func(y:obj)->ind; return x};", 1)>]
    [<DataRow("NP2c", "def func Test()->pred() {dec ~x:func(y:obj)->func; return x};", 1)>]
    [<DataRow("NP2d", "def func Test()->pred() {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 1)>]
    [<DataRow("NP3", "def func Test()->pred() {dec ~x:pred; return x};", 1)>]
    [<DataRow("NP3a", "def func Test()->pred() {dec ~x:pred(); return x};", 0)>]
    [<DataRow("NP3b", "def func Test()->pred() {dec ~x:pred; return x};", 1)>]
    [<DataRow("NP3c", "def func Test()->pred() {dec ~x:pred(y:obj); return x};", 1)>]
    // (mis)match return with mapping having pred(...) types
    [<DataRow("NP_0", "def func Test()->pred(a:obj) {dec ~x:obj; return x};", 1)>]
    [<DataRow("NP_1", "def func Test()->pred(a:obj) {dec ~x:ind; return x};", 1)>]
    [<DataRow("NP_2", "def func Test()->pred(a:obj) {dec ~x:func; return x};", 1)>]
    [<DataRow("NP_2a", "def func Test()->pred(a:obj) {dec ~x:func()->ind; return x};", 1)>]
    [<DataRow("NP_2b", "def func Test()->pred(a:obj) {dec ~x:func(y:obj)->ind; return x};", 1)>]
    [<DataRow("NP_2c", "def func Test()->pred(a:obj) {dec ~x:func(y:obj)->func; return x};", 1)>]
    [<DataRow("NP_2d", "def func Test()->pred(a:obj) {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 1)>]
    [<DataRow("NP_3", "def func Test()->pred(a:obj) {dec ~x:pred; return x};", 1)>]
    [<DataRow("NP_3a", "def func Test()->pred(a:obj) {dec ~x:pred(); return x};", 1)>]
    [<DataRow("NP_3b", "def func Test()->pred(a:obj) {dec ~x:pred; return x};", 1)>]
    [<DataRow("NP_3c", "def func Test()->pred(a:obj) {dec ~x:pred(y:obj); return x};", 0)>]
    [<DataRow("NP_3c", "def func Test()->pred(a:obj) {dec ~x:pred(y:ind); return x};", 1)>]

    // (mis)match return with mapping having func() types
    [<DataRow("NF0", "def func Test()->func()->ind {dec ~x:obj; return x};", 1)>]
    [<DataRow("NF1", "def func Test()->func()->ind {dec ~x:ind; return x};", 1)>]
    [<DataRow("NF2", "def func Test()->func()->ind {dec ~x:func; return x};", 1)>]
    [<DataRow("NF2a", "def func Test()->func()->ind {dec ~x:func()->ind; return x};", 0)>]
    [<DataRow("NF2b", "def func Test()->func()->ind {dec ~x:func(y:obj)->ind; return x};", 1)>]
    [<DataRow("NF2c", "def func Test()->func()->ind {dec ~x:func(y:obj)->obj; return x};", 1)>]
    [<DataRow("NF2d", "def func Test()->func()->ind {dec ~x:func(y:ind)->ind; return x};", 1)>]
    [<DataRow("NF2e", "def func Test()->func()->ind {dec ~x:func(y:obj)->func; return x};", 1)>]
    [<DataRow("NF2f", "def func Test()->func()->ind {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 1)>]
    [<DataRow("NF3", "def func Test()->func()->ind {dec ~x:pred; return x};", 1)>]
    [<DataRow("NF3a", "def func Test()->func()->ind {dec ~x:pred(); return x};", 1)>]
    [<DataRow("NF3b", "def func Test()->func()->ind {dec ~x:pred; return x};", 1)>]
    [<DataRow("NF3c", "def func Test()->func()->ind {dec ~x:pred(y:obj); return x};", 1)>]
    // (mis)match return with mapping having func(...) types
    [<DataRow("NF_0", "def func Test()->func(a:obj)->ind {dec ~x:obj; return x};", 1)>]
    [<DataRow("NF_1", "def func Test()->func(a:obj)->ind {dec ~x:ind; return x};", 1)>]
    [<DataRow("NF_2", "def func Test()->func(a:obj)->ind {dec ~x:func; return x};", 1)>]
    [<DataRow("NF_2a", "def func Test()->func(a:obj)->ind {dec ~x:func()->ind; return x};", 1)>]
    [<DataRow("NF_2b", "def func Test()->func(a:obj)->ind {dec ~x:func(y:obj)->ind; return x};", 0)>]
    [<DataRow("NF_2c", "def func Test()->func(a:obj)->ind {dec ~x:func(y:obj)->obj; return x};", 1)>]
    [<DataRow("NF_2d", "def func Test()->func(a:obj)->ind {dec ~x:func(y:ind)->ind; return x};", 1)>]
    [<DataRow("NF_2e", "def func Test()->func(a:obj)->ind {dec ~x:func(y:obj)->func; return x};", 1)>]
    [<DataRow("NF_2f", "def func Test()->func(a:obj)->ind {dec ~x:func(y:obj)->func(z:pred)->pred; return x};", 1)>]
    [<DataRow("NF_3", "def func Test()->func(a:obj)->ind {dec ~x:pred; return x};", 1)>]
    [<DataRow("NF_3a", "def func Test()->func(a:obj)->ind {dec ~x:pred(); return x};", 1)>]
    [<DataRow("NF_3b", "def func Test()->func(a:obj)->ind {dec ~x:pred; return x};", 1)>]
    [<DataRow("NF_3c", "def func Test()->func(a:obj)->ind {dec ~x:pred(y:obj); return x};", 1)>]
    [<DataRow("NF_3d", "def func Test()->func(a:obj)->ind {dec ~x:pred(y:ind); return x};", 1)>]

    // match return with mapping having class type
    [<DataRow("CT1", "def cl A {intr} def func Test()->obj {dec ~x:A; return x};", 0)>] // A is obj, no error
    [<DataRow("CT2", "def cl A {intr} def func Test()->A {dec ~x:A; return x};", 0)>] // A is A, no error
    [<DataRow("CT3", "def cl A {intr} def cl B:A {intr} def func Test()->A {dec ~x:B; return x};", 0)>] // x is also B:A, no error
    [<DataRow("CT4", "def cl A {intr} def cl B:A {intr} def func Test()->B {dec ~x:B; return x};", 0)>] // x is B, no error
    [<DataRow("CT5", "def cl A {intr} def cl B:A {intr} def func Test()->obj {dec ~x:B; return x};", 0)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1", "def cl A {intr} def func Test()->obj {dec ~x:A x:=A(); return x};", 0)>] // A is obj, no error
    [<DataRow("CI2", "def cl A {intr} def func Test()->A {dec ~x:A x:=A(); return x};", 0)>] // A is A, no error
    [<DataRow("CI3", "def cl A {intr} def cl B:A {intr} def func Test()->A {dec ~x:B x:=B(); return x};", 0)>] // x is also B:A, no error
    [<DataRow("CI4", "def cl A {intr} def cl B:A {intr} def func Test()->B {dec ~x:B x:=B(); return x};", 0)>] // x is B, no error
    [<DataRow("CI5", "def cl A {intr} def cl B:A {intr} def func Test()->obj {dec ~x:B x:=B(); return x};", 0)>] // x is also B:A:obj, no error

    // mismatch return with mapping having class type
    [<DataRow("CT1_", "def func Test()->obj {dec ~x:A; return x};", 1)>] // A is undefined, error
    [<DataRow("CT2_", "def cl A def func Test()->A {dec ~x:obj; return x};", 1)>] // obj is not A, error
    [<DataRow("CT3_", "def cl A def cl B:A def func Test()->B {dec ~a:A; return a};", 1)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_", "def func Test()->obj {dec ~x:A x:=A; return x};", 1)>] // A is undefined, error
    [<DataRow("CI2_", "def cl A def cl B:A def func Test()->B {dec ~a:A a:=A; return a};", 1)>] // A is not B, error
    [<DataRow("CI3_", "def cl A def cl B:A def func Test()->B {dec ~a:B a:=B; return a};", 1)>] // B is B, but a class reference, error
    [<DataRow("CI4_", "def cl A def func Test()->obj {dec ~x:A x:=A; return x};", 1)>] // A is obj, but x is class reference, error
    [<DataRow("CI5_", "def cl A def cl B:A def func Test()->B {dec ~x:B x:=B; return x};", 1)>] // B is B, but x is class referene, error
    [<DataRow("CI6_", "def cl A def func Test()->A {dec ~x:A x:=A; return x};", 1)>] // A is A, but x is class reference, error
    [<DataRow("CI7_", "def cl A def cl B:A def func Test()->A {dec ~x:B x:=B; return x};", 1)>] // B is A but x is a class reference, error 
    [<DataRow("CI8_", "def cl A def cl B:A def func Test()->B {dec ~x:B x:=B; return x};", 1)>] // x is B, but class reference, error
    [<DataRow("CI9_", "def cl A def cl B:A def func Test()->obj {dec ~x:B x:=B; return x};", 1)>] // B is obj but x is class reference, error

    // match return with mapping having the type pred(...) 
    [<DataRow("MS1", "def pred A(z:obj) def func Test()->pred(y:obj) {return A};", 0)>] // OK: ->pred(y:obj) matches signature A(obj), whole node would be returned
    [<DataRow("MS1a", "def pred A(z:obj) def func Test()->pred(y:obj) {dec ~x:obj; return A(x)};", 1)>] // SIG03: ->pred(y:obj) does not match value A(obj) 
    [<DataRow("MS1b", "def pred A(z:obj) def func Test()->pred(y:obj) {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->pred(y:obj) does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c", "def pred A(z:ind) def func Test()->pred(y:obj) {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "def pred A(z:ind) def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "ax A {true} def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "thm A {true} def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "lem A {true} def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "prop A {true} def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "conj A {true} def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "cor A$1 {true} def func Test()->pred(y:obj) {return A$1};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "proof A$1 {1. |- trivial} def func Test()->pred(y:obj) {return A$1};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "inf A {pre:true con:true} def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def func A()->obj def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (extension)
    [<DataRow("MS1o", "def cl A def func Test()->pred(y:obj) {return A};", 1)>] // SIG03: ->pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {return A.X};", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3", "def cl A {intr prty pred X() } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4", "def cl A {intr prty func X()->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred(y:obj) {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3", "def cl A {intr prty pred X() } def func Test()->pred(y:obj) {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4", "def cl A {intr prty func X()->obj } def func Test()->pred(y:obj) {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred(y:obj) {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred(y:obj) {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec ~a:obj ~o:A o:=A(); return o.X(a)};", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {return A.X};", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2", "def pred A() {intr prty pred X(x:ind) } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3", "def pred A() {intr prty pred X() } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4", "def pred A() {intr prty func X()->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5", "def pred A() {intr prty func X(x:obj)->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {return A.X};", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3", "def func A()->obj {intr prty pred X() } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4", "def func A()->obj {intr prty func X()->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5", "def func A()->obj {intr prty func X(x:obj)->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->pred(y:obj) {return A.X};", 1)>] // SIG03: ->pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred(y:obj) {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred(y:obj) does not match by value A.X(obj) 
    
    // match return with mapping having the type pred() 
    [<DataRow("MS1_", "def pred A() def func Test()->pred() {return A};", 0)>] // OK: ->pred() matches signature A(), whole node would be returned
    [<DataRow("MS1a_", "def pred A() def func Test()->pred() {dec ~x:obj; return A(x)};", 1)>] // SIG03: ->pred() does not match value A(obj) 
    [<DataRow("MS1b_", "def pred A() def func Test()->pred() {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_", "def pred A() def func Test()->pred() {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->pred() does not match value A(ind) 
    [<DataRow("MS1d_", "def pred A(x:ind) def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A(ind)
    [<DataRow("MS1e_", "ax A {true} def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (axiom)
    [<DataRow("MS1f_", "thm A {true} def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (theorem)
    [<DataRow("MS1g_", "lem A {true} def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (lemma)
    [<DataRow("MS1h_", "prop A {true} def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (proposition)
    [<DataRow("MS1i_", "conj A {true} def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (conjecture)
    [<DataRow("MS1j_", "cor A$1 {true} def func Test()->pred() {return A$1};", 1)>] // SIG03: ->pred() does not match signature A (corollary)
    [<DataRow("MS1k_", "proof A$1 {1. |- trivial} def func Test()->pred() {return A$1};", 1)>] // SIG03: ->pred() does not match signature A (proof)
    [<DataRow("MS1l_", "inf A {pre:true con:true} def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_", "def func A()->obj def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (functional term)
    [<DataRow("MS1n_", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (extension)
    [<DataRow("MS1o_", "def cl A def func Test()->pred() {return A};", 1)>] // SIG03: ->pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3", "def cl A {intr prty pred X() } def func Test()->pred() {return A.X};", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1p_4", "def cl A {intr prty func X()->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred() {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred() {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred() {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3", "def cl A {intr prty pred X() } def func Test()->pred() {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1q_4", "def cl A {intr prty func X()->obj } def func Test()->pred() {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred() {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred() {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred() {dec ~a:obj ~o:A o:=A(); return o.X(a)};", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2", "def pred A() {intr prty pred X(x:ind) } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3", "def pred A() {intr prty pred X() } def func Test()->pred() {return A.X};", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1r_4", "def pred A() {intr prty func X()->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X()
    [<DataRow("MS1r_5", "def pred A() {intr prty func X(x:obj)->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred() {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3", "def func A()->obj {intr prty pred X() } def func Test()->pred() {return A.X};", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1s_4", "def func A()->obj {intr prty func X()->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5", "def func A()->obj {intr prty func X(x:obj)->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->pred() {return A.X};", 1)>] // SIG03: ->pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred() {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred() does not match by value A.X(obj) 
     
    // match return with mapping having the type pred 
    [<DataRow("MS2", "def pred A(z:obj) def func Test()->pred {return A};", 0)>] // OK: ->pred matches signature A(obj), whole node would be returned
    [<DataRow("MS2a", "def pred A(z:obj) def func Test()->pred {dec ~x:obj; return A(x)};", 0)>] // OK: ->pred matches value A(obj) 
    [<DataRow("MS2b", "def pred A(z:obj) def func Test()->pred {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c", "def pred A(z:ind) def func Test()->pred {dec ~x:ind; return A(x)};", 0)>] // OK: ->pred matches value A(ind) 
    [<DataRow("MS2d", "def pred A(z:ind) def func Test()->pred {return A};", 0)>] // OK: ->pred matches signature A(ind)
    [<DataRow("MS2e", "ax A {true} def func Test()->pred {return A};", 0)>] // OK: ->pred matches signature A (axiom)
    [<DataRow("MS2f", "thm A {true} def func Test()->pred {return A};", 0)>] // OK: ->pred matches signature A (theorem)
    [<DataRow("MS2g", "lem A {true} def func Test()->pred {return A};", 0)>] // OK: ->pred matches signature A (lemma)
    [<DataRow("MS2h", "prop A {true} def func Test()->pred {return A};", 0)>] // OK: ->pred matches signature A (proposition)
    [<DataRow("MS2i", "conj A {true} def func Test()->pred {return A};", 0)>] // OK: ->pred does matches signature A (conjecture)
    [<DataRow("MS2j", "cor A$1 {true} def func Test()->pred {return A$1};", 0)>] // OK: ->pred matches signature A$1 (corollary)
    [<DataRow("MS2k", "proof A$1 {1. |- trivial} def func Test()->pred {return A$1};", 1)>] // SIG03: ->pred does not match signature A$1 (proof)
    [<DataRow("MS2l", "inf A {pre:true con:true} def func Test()->pred {return A};", 1)>] // SIG03: ->pred does not match signature A (rule of inference)
    [<DataRow("MS2m", "def func A()->obj def func Test()->pred {return A};", 1)>] // SIG03: ->pred does not match signature A (functional term)
    [<DataRow("MS2n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def func Test()->pred {return A};", 1)>] // SIG03: ->pred does not match signature A (extension)
    [<DataRow("MS2o", "def cl A def func Test()->pred {return A};", 1)>] // SIG03: ->pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred {return A.X};", 0)>] // OK: ->pred matches signature A.A(obj) 
    [<DataRow("MS2p2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred {return A.X};", 0)>] // OK: ->pred does not match signature A.X(ind)
    [<DataRow("MS2p3", "def cl A {intr prty pred X() } def func Test()->pred {return A.X};", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2p4", "def cl A {intr prty func X()->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2p5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1", "def cl A {intr prty pred X(x:obj) } def func Test()->pred {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2q2", "def cl A {intr prty pred X(x:ind) } def func Test()->pred {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2q3", "def cl A {intr prty pred X() } def func Test()->pred {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2q4", "def cl A {intr prty func X()->obj } def func Test()->pred {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2q5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->pred {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->pred {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7", "def cl A {intr prty pred X(x:obj) } def func Test()->pred {dec ~a:obj ~o:A o:=A(); return o.X(a)};", 1)>] // SIG03: ->pred does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred {return A.X};", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2r2", "def pred A() {intr prty pred X(x:ind) } def func Test()->pred {return A.X};", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2r3", "def pred A() {intr prty pred X() } def func Test()->pred {return A.X};", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2r4", "def pred A() {intr prty func X()->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2r5", "def pred A() {intr prty func X(x:obj)->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7", "def pred A() {intr prty pred X(x:obj) } def func Test()->pred {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred {return A.X};", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2s2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->pred {return A.X};", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2s3", "def func A()->obj {intr prty pred X() } def func Test()->pred {return A.X};", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2s4", "def func A()->obj {intr prty func X()->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X()->obj
    [<DataRow("MS2s5", "def func A()->obj {intr prty func X(x:obj)->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->pred {return A.X};", 1)>] // SIG03: ->pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->pred {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->pred does not match by value A.X(obj) 

    // match return with mapping having the type func(...)->...
    [<DataRow("MS3", "def func A(z:obj)->ind def func Test()->func(y:obj)->ind {return A};", 0)>] // OK: ->func(y:obj)->ind matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS3a", "def func A(z:obj)->ind def func Test()->func(y:obj)->ind {dec ~x:obj; return A(x)};", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b", "def func A(z:obj)->ind def func Test()->func(y:obj)->ind {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c", "def func A(z:ind)->ind def func Test()->func(y:obj)->ind {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d", "def func A(z:ind)->ind def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e", "ax A {true} def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f", "thm A {true} def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g", "lem A {true} def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h", "prop A {true} def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i", "conj A {true} def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j", "cor A$1 {true} def func Test()->func(y:obj)->ind {return A$1};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k", "proof A$1 {1. |- trivial} def func Test()->func(y:obj)->ind {return A$1};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l", "inf A {pre:true con:true} def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m", "def func A(z:obj)->func()->obj def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o", "def cl A def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1", "def cl A {intr prty pred X(x:obj) } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3p2", "def cl A {intr prty pred X(x:ind) } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3", "def cl A {intr prty pred X() } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4", "def cl A {intr prty func X()->obj } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->func(y:obj)->obj {return A.X};", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func(y:obj)->ind {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1", "def cl A {intr prty pred X(x:obj) } def func Test()->func(y:obj)->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2", "def cl A {intr prty pred X(x:ind) } def func Test()->func(y:obj)->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3", "def cl A {intr prty pred X() } def func Test()->func(y:obj)->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4", "def cl A {intr prty func X()->obj } def func Test()->func(y:obj)->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5", "def cl A {intr prty func X(x:obj)->obj } def func Test()->func(y:obj)->obj {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func(y:obj)->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func(y:obj)->ind {dec ~a:obj ~o:A o:=A(); return o.X(a)};", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1", "def pred A() {intr prty pred X(x:obj) } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2", "def pred A() {intr prty pred X(x:ind) } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3", "def pred A() {intr prty pred X() } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4", "def pred A() {intr prty func X()->obj } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5", "def pred A() {intr prty func X(x:obj)->obj } def func Test()->func(y:obj)->obj {return A.X};", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7", "def pred A() {intr prty func X(x:obj)->ind } def func Test()->func(y:obj)->ind {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3", "def func A()->obj {intr prty pred X() } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4", "def func A()->obj {intr prty func X()->obj } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5", "def func A()->obj {intr prty func X(x:obj)->obj } def func Test()->func(y:obj)->obj {return A.X};", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->func(y:obj)->ind {return A.X};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7", "def func A()->obj {intr prty func X(x:obj)->ind } def func Test()->func(y:obj)->ind {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func(y:obj)->ind does not match by value A.X(obj) 
    
    // match return with mapping having the type func()->...
    [<DataRow("MS3_", "def func A()->ind def func Test()->func()->ind {return A};", 0)>] // OK: ->func()->ind matches signature A()->ind, whole node would be returned
    [<DataRow("MS3a_", "def func A()->ind def func Test()->func()->ind {dec ~x:obj; return A(x)};", 1)>] // SIG03: ->func()->ind does not match value A(obj) 
    [<DataRow("MS3b_", "def func A()->ind def func Test()->func()->ind {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->func()->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c_", "def func A()->ind def func Test()->func()->ind {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->func()->ind does not match value A(ind) 
    [<DataRow("MS3d_", "def func A(z:ind)->ind def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_", "ax A {true} def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_", "thm A {true} def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_", "lem A {true} def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_", "prop A {true} def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_", "conj A {true} def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_", "cor A$1 {true} def func Test()->func()->ind {return A$1};", 1)>] // SIG03: ->func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_", "proof A$1 {1. |- trivial} def func Test()->func()->ind {return A$1};", 1)>] // SIG03: ->func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_", "inf A {pre:true con:true} def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_", "def func A()->func()->obj def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def func Test()->func(y:obj)->ind {return A};", 1)>] // SIG03: ->func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_", "def cl A def func Test()->func()->ind {return A};", 1)>] // SIG03: ->func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1", "def cl A {intr prty pred X(x:obj) } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2", "def cl A {intr prty pred X(x:ind) } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3", "def cl A {intr prty pred X() } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3p_4", "def cl A {intr prty func X()->obj } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5", "def cl A {intr prty func X()->obj } def func Test()->func()->obj {return A.X};", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func()->ind {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1", "def cl A {intr prty pred X(x:obj) } def func Test()->func()->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2", "def cl A {intr prty pred X(x:ind) } def func Test()->func()->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3", "def cl A {intr prty pred X() } def func Test()->func()->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3q_4", "def cl A {intr prty func X()->obj } def func Test()->func()->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5", "def cl A {intr prty func X()->obj } def func Test()->func()->obj {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func()->ind {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func()->ind {dec ~a:obj ~o:A o:=A(); return o.X(a)};", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1", "def pred A() {intr prty pred X(x:obj) } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2", "def pred A() {intr prty pred X(x:ind) } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3", "def pred A() {intr prty pred X() } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3r_4", "def pred A() {intr prty func X()->obj } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5", "def pred A() {intr prty func X()->obj } def func Test()->func()->obj {return A.X};", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7", "def pred A() {intr prty func X(x:obj)->ind } def func Test()->func()->ind {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3", "def func A()->obj {intr prty pred X() } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind match signature A.X()
    [<DataRow("MS3s_4", "def func A()->obj {intr prty func X()->obj } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5", "def func A()->obj {intr prty func X()->obj } def func Test()->func()->obj {return A.X};", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->func()->ind {return A.X};", 1)>] // SIG03: ->func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7", "def func A()->obj {intr prty func X(x:obj)->ind } def func Test()->func()->ind {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func()->ind does not match by value A.X(obj) 

    // match return with mapping having the type func
    [<DataRow("MS4", "def func A(z:obj)->ind def func Test()->func {return A};", 0)>] // OK: ->func matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS4a", "def func A(z:obj)->ind def func Test()->func {dec ~x:obj; return A(x)};", 1)>] // SIG03: ->func does not match value A(obj) 
    [<DataRow("MS4b", "def func A(z:obj)->ind def func Test()->func {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4c", "def func A(z:ind)->ind def func Test()->func {dec ~x:ind; return A(x)};", 1)>] // SIG03: ->func does not match value A(ind) 
    [<DataRow("MS4d", "def func A(z:ind)->func(a:obj)->ind def func Test()->func {return A};", 0)>] // OK: ->func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e", "ax A {true} def func Test()->func {return A};", 1)>] // SIG03: ->func does not matche signature A (axiom)
    [<DataRow("MS4f", "thm A {true} def func Test()->func {return A};", 1)>] // SIG03: ->func does not matche signature A (theorem)
    [<DataRow("MS4g", "lem A {true} def func Test()->func {return A};", 1)>] // SIG03: ->func does not matche signature A (lemma)
    [<DataRow("MS4h", "prop A {true} def func Test()->func {return A};", 1)>] // SIG03: ->func does not matche signature A (proposition)
    [<DataRow("MS4i", "conj A {true} def func Test()->func {return A};", 1)>] // SIG03: ->func does not matche signature A (conjecture)
    [<DataRow("MS4j", "cor A$1 {true} def func Test()->func {return A$1};", 1)>] // SIG03: ->func does not matche signature A$1 (corollary)
    [<DataRow("MS4k", "proof A$1 {1. |- trivial} def func Test()->func {return A$1};", 1)>] // SIG03: ->func does not matche signature A$1 (proof)
    [<DataRow("MS4l", "inf A {pre:true con:true} def func Test()->func {return A};", 1)>] // SIG03: ->func does not matche signature A (rule of inference)
    [<DataRow("MS4m", "def func A(z:obj)->func()->obj def func Test()->func {return A};", 0)>] // OK: ->func matches signature A (functional term)
    [<DataRow("MS4n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def func Test()->func {return A};", 1)>] // SIG03: ->func does not match signature A (extension)
    [<DataRow("MS4o", "def cl A def func Test()->func {return A};", 1)>] // SIG03: ->func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1", "def cl A {intr prty pred X(x:obj) } def func Test()->func {return A.X};", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4p2", "def cl A {intr prty pred X(x:ind) } def func Test()->func {return A.X};", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4p3", "def cl A {intr prty pred X() } def func Test()->func {return A.X};", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4p4", "def cl A {intr prty func X()->obj } def func Test()->func {return A.X};", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4p5", "def cl A {intr prty func X()->obj } def func Test()->func()->obj {return A.X};", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4p6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func {return A.X};", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4p7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1", "def cl A {intr prty pred X(x:obj) } def func Test()->func {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4q2", "def cl A {intr prty pred X(x:ind) } def func Test()->func {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4q3", "def cl A {intr prty pred X() } def func Test()->func {dec ~o:A o:=A(); return o.X};", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4q4", "def cl A {intr prty func X()->obj } def func Test()->func {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4q5", "def cl A {intr prty func X()->obj } def func Test()->func()->obj {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4q6", "def cl A {intr prty func X(x:ind)->obj } def func Test()->func {dec ~o:A o:=A(); return o.X};", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4q7", "def cl A {intr prty func X(x:obj)->ind } def func Test()->func {dec ~a:obj ~o:A o:=A(); return o.X(a)};", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1", "def pred A() {intr prty pred X(x:obj) } def func Test()->func {return A.X};", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4r2", "def pred A() {intr prty pred X(x:ind) } def func Test()->func {return A.X};", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4r3", "def pred A() {intr prty pred X() } def func Test()->func {return A.X};", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4r4", "def pred A() {intr prty func X()->obj } def func Test()->func {return A.X};", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4r5", "def pred A() {intr prty func X()->obj } def func Test()->func()->obj {return A.X};", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4r6", "def pred A() {intr prty func X(x:ind)->obj } def func Test()->func {return A.X};", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4r7", "def pred A() {intr prty func X(x:obj)->ind } def func Test()->func {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1", "def func A()->obj {intr prty pred X(x:obj) } def func Test()->func {return A.X};", 1)>] // SIG03: ->func matches signature A.A(obj) 
    [<DataRow("MS4s2", "def func A()->obj {intr prty pred X(x:ind) } def func Test()->func {return A.X};", 1)>] // SIG03: ->func does not match signature A.X(ind)
    [<DataRow("MS4s3", "def func A()->obj {intr prty pred X() } def func Test()->func {return A.X};", 1)>] // SIG03: ->func match signature A.X()
    [<DataRow("MS4s4", "def func A()->obj {intr prty func X()->obj } def func Test()->func {return A.X};", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4s5", "def func A()->obj {intr prty func X()->obj } def func Test()->func()->obj {return A.X};", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4s6", "def func A()->obj {intr prty func X(x:ind)->obj } def func Test()->func {return A.X};", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4s7", "def func A()->obj {intr prty func X(x:obj)->ind } def func Test()->func {dec ~a:obj; return A.X(a)};", 1)>] // SIG03: ->func does not match by value A.X(obj) 

    [<TestMethod>]
    member this.TestSIG03(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG03 ""
            ad.Clear()
            runTestHelper "TestSIG03.fpl" fplCode code expected

    [<DataRow("00", "def cl Test {intr};", 0)>]
    [<DataRow("01", "def cl Test:Set {intr};", 0)>] // this should cause the ID010 error only and not SIG04
    [<DataRow("02", "def class Set def cl Test:Set {intr};", 0)>]
    [<DataRow("03", "def cl Set {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("04", "def cl Set {intr} def cl Test:SetTypo {intr};", 0)>] // this should cause the ID010 error only and not SIG04
    [<DataRow("05", "def pred Test(x:Set) {intr};", 1)>]
    [<DataRow("06", "def cl Set {intr} def pred Test(x:Set) {intr};", 0)>]
    [<DataRow("07", "def cl Set {intr} def pred Test(x:SetTypo) {intr};", 1)>]
    [<DataRow("08", "def cl Set {intr} axiom Test {dec ~x:SetTypo; true};", 1)>]
    [<DataRow("09", "def cl Set {intr} axiom Test {dec ~x:Set; true};", 0)>]
    [<DataRow("10", "def cl Set {intr} def func PowerSer(x:Set) -> Set {dec ~y:Set; return y};", 0)>]
    [<DataRow("11", "def cl Set {intr} axiom Test {dec ~x:Set; true};", 0)>]
    [<DataRow("12", "def cl Set {intr} axiom Test {dec ~x:SetTypo; true};", 1)>]
    [<DataRow("13", "def pred Test() {dec ~x:Set; true};", 1)>]
    [<DataRow("14", "axiom A { all x:Nat {true} };", 1)>]
    [<DataRow("15", "def pred Test() {dec ~x:object; is(x,Set)};", 1)>]
    [<DataRow("15a", "def cl Set def pred Test() {dec ~x:object; is(x,Set)};", 0)>]
    [<DataRow("16", "def cl Set def pred Test() {dec ~x:object; is(x,Set)};", 0)>]
    [<DataRow("16a", "def cl C {ctor C(x:ind) {}} def cl A:C { ctor A() {dec ~x:obj base.C(x); } };", 1)>]
    [<DataRow("17", """def pred T1() {true} def pred Test() { dec ~x:obj; T1(x) };""", 1)>]
    [<DataRow("18", """def pred T1() {true} def pred Test() { OtherTest() };""", 1)>]
    [<DataRow("19", """def pred T (x:obj) {true} def pred Caller() {dec ~x:obj; T(x)} ;""", 0)>]
    [<DataRow("20", """def pred T (x:obj) {true} def pred Caller() {dec ~x:ind; T(x)} ;""", 1)>]
    [<DataRow("21", "inf ExistsByExample {dec ~p: pred(c: obj) ~x: obj; pre: p(c) con: ex x:obj {p(x)}};", 0)>]
    [<DataRow("22", """loc NotEqual(x,y) := !tex: x "\neq" y; ;""", 0)>]
    [<DataRow("23", """def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec ~x,y:obj; (x = y) };""", 0)>]
    [<DataRow("24", """def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec ~x,y:obj; Eq(x,y) };""", 0)>]
    [<DataRow("25", """def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec ~x:ind ~y:obj; (x = y) };""", 1)>]
    [<DataRow("26", """def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec ~x:ind ~y:obj; (x = y) };""", 1)>]
    [<DataRow("27", """def pred Eq(x,y: Nat) infix "=" 1000 {intr} axiom A {dec ~x:ind ~y:obj; (x = y) };""", 2)>]
    [<DataRow("28", """def pred Eq(x,y: ind) infix "=" 1000 {intr} axiom A {dec ~x:ind ~y:obj; (x = y) };""", 1)>]
    [<DataRow("29", """def pred Mul(x,y: pred) infix "*" 1 {intr} def pred Add(x,y: ind) infix "+" 2 {intr} def pred Eq (x,y: obj) infix "=" 1000 {intr} def pred T1() { (x = y * z + 1) };""", 3)>]
    [<DataRow("30", """def pred T (x:tpl) {true} def pred Caller() {dec ~x:ind; T(x)} ;""", 0)>]
    [<DataRow("31", """def pred T (x:tplTest) {true} def pred Caller() {dec ~x:ind; T(x)} ;""", 0)>]
    [<DataRow("32", """def pred T (x,y,z:obj) {true} def pred Caller() {dec ~x:obj; T(x)} ;""", 1)>]
    [<DataRow("33", """def pred T (x,y,z:obj) {true} def pred Caller() {dec ~x:obj; T(x,x)} ;""", 1)>]
    [<DataRow("34", """def pred T (x,y,z:obj) {true} def pred Caller() {dec ~x:obj; T(x,x,x)} ;""", 0)>]
    [<DataRow("35", """def pred T (x,y,z:obj) {true} def pred Caller() {dec ~x,y:obj ~z:ind; T(x,y,z)} ;""", 1)>]
    [<DataRow("36", """def pred T (x,y:obj,z:ind) {true} def pred Caller() {dec ~x,y:obj ~z:ind; T(x,y,z)} ;""", 0)>]
    [<DataRow("37", """def pred T (x,y:obj) {true} def pred Caller() {dec ~x,y:obj ~z:ind; T(x,y,z)} ;""", 1)>]
    [<DataRow("38", """def class Nat  {ctor Nat(){dec self:=x.R(); }};""", 0)>] // this would cause SIG03 error
    [<DataRow("39", """def func Succ(n:Nat) -> obj {intr};""", 1)>]
    [<DataRow("40", """def func T()->obj { dec ~x:obj; return x};""", 0)>]
    [<DataRow("40a", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return S(x)} ;""", 0)>]
    [<DataRow("40b", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return (S(x)) } ;""", 0)>]
    [<DataRow("40c", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return S(S(x))} ;""", 0)>]
    [<DataRow("41", """def func T(y:obj)->obj { return self(y)} ;""", 0)>]
    [<DataRow("42", """def func T(y:obj)->obj def func S()->obj {dec ~x:obj; return T(x)} ;""", 0)>]
    [<DataRow("43", """axiom T { dec ~x:obj; all p:pred(y:obj) {p(x)}};""", 0)>]
    [<DataRow("44", """def cl A {intr} def func Add(n,m:A)->A {return self(n,m)};""", 0)>]
    [<DataRow("45", """def cl A {intr} def func Add(n,m:A)->A {dec ~x:A; return x};""", 0)>]
    [<DataRow("46", """def cl A {intr} def func Add(n,m:A)->A {dec ~x:A; return x} prop P {dec ~op:Add; true};""", 0)>]
    [<DataRow("47", """def cl A {intr property pred T() {true} property pred S() {T()}};""", 0)>]
    [<DataRow("48", """def cl Obj def cl A:Obj {dec ~x:obj; ctor A(y:obj) {dec base.Obj() x:=y; } property func P()->obj {return x}} def pred T(r:A) {r.P()};""", 0)>]
    [<DataRow("49", """def cl A:Obj {ctor A(y:*obj[ind]) {}} def class B {ctor B(z:*obj[ind]) {dec ~a:A base.Obj() a := A(z); }};""", 0)>]
    [<DataRow("50", """def cl A {intr property pred T() {true}} def cl B:A {ctor B() {dec base.A() assert self.T(); }};""", 0)>]
    [<DataRow("51", """def func A(n,m:obj)->obj {intr} prop T {dec ~op:A ~x,y:obj; (op(x,y) = x)};""", 0)>]
    [<DataRow("52", """def cl T { dec ~x:*tpl[ind]; ctor T(y:*tpl[ind]) {dec x:=y; } property func C(i:ind) -> tpl {return x[i]}};""", 0)>]
    [<DataRow("53", """def cl Nat ext D x@/\d+/ -> Nat {dec ~n,m:Nat cases ( | (x = @0) : n:=m ? m:=n ); return n } def func Add(x,y:Nat)->obj {intr} prop K {dec ~op:Add ~n:Nat; ( op(n,@0) = n ) } ;""", 0)>]
    [<DataRow("54", """def cl C {ctor C() {}} def pred T() {dec ~cI1:C cI1:=C; true } ;""", 1)>]
    [<DataRow("54a", """def cl C {ctor C() {}} def pred T() {dec ~cI1:C cI1:=C(); true } ;""", 0)>]
    [<DataRow("54b", """def cl C {ctor C(x:obj) {}} def pred T() {dec ~cI1:C cI1:=C(); true } ;""", 1)>]
    [<DataRow("54c", """def cl C {ctor C(x:obj) {}} def pred T() {dec ~x:obj ~cI1:C cI1:=C(x); true } ;""", 0)>]
    [<DataRow("54b_", """def cl C1 {ctor C1(i1:ind) {dec ~o:ind o:=i1; }} ;""", 0)>]
    [<DataRow("54c_", """def cl C1 {ctor C1(i1:ind) {dec ~o:ind o:=i1; }} ;""", 0)>]
    [<DataRow("55", """def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI1:D dI1:=D; true } ;""", 0)>]
    [<DataRow("55a", """def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI1:D dI1:=D(); true } ;""", 0)>]
    [<DataRow("55b", """def cl B: A {intr} def cl D: B {ctor D(x:obj) {dec base.B();  }} def pred T() {dec ~dI1:D dI1:=D; true } ;""", 1)>]
    [<DataRow("55c", """def cl B: A {intr} def cl D: B {ctor D(x:obj) {dec base.B();  }} def pred T() {dec ~dI1:D dI1:=D(); true } ;""", 1)>]
    [<DataRow("56", """def pred T() {dec ~x:ind x:=$1; true } ;""", 0)>]
    [<DataRow("57", """def pred T() {dec ~x:pred x:=true; true } ;""", 0)>]
    [<DataRow("57a", """def pred T() {dec ~x:pred x:=not true; true } ;""", 0)>]
    [<DataRow("66", """def cl Set def pred In(x,y: Set) def cl SetRoster:Set { ctor SetRoster(list:* Set[ind]) { dec ~e:Set base.Set() for e in list {assert In(e, parent)}; } };""", 0)>]
    [<DataRow("67", """def class Set def pred In(x,y: Set) def pred IsEmpty(x: Set) { all y:Set { not In(y, x) } };""", 0)>]
    [<DataRow("68", """def class Set def pred In(x,y: Set) def cl SetBuilder: Set { ctor SetBuilder(x: Set, p: pred(u1: Set, o:* obj[ind])) { dec base.Set() assert all u2:Set { iif (In(u2,parent), and ( In(u2,x), p(u2,o) ) ) }; } };""", 0)>]
    [<DataRow("69", """def cl A {dec ~myX:obj; ctor A(x:obj) {dec myX:=x;}} def cl B:A { ctor B(x:obj) {dec base.A(del.Decrement(x)); } } def pred T() { dec ~v:B v:=B(@2); false};""", 0)>]
    [<DataRow("70", """def cl A def pred T() { is (self,ATypo) };""", 1)>]
    [<DataRow("71", """def pred Equal(x,y: tpl) infix "=" 50 { del.Equal(x,y) } def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def pred T() {all x,y:Nat {(x = Succ(y))}};""", 0)>]    
    // mapping
    [<DataRow("MAP1", """def func T()->A {intr};""", 1)>]
    [<DataRow("MAP1a", """def cl A def func T()->A {intr};""", 0)>]
    [<DataRow("MAP2", """def func T()->*A[ind] {intr};""", 1)>]
    [<DataRow("MAP2a", """def cl A def func T()->*A[ind] {intr};""", 0)>]
    [<DataRow("MAP3", """def func T()->*ind[A] {intr};""", 1)>]
    [<DataRow("MAP3a", """def cl A def func T()->*ind[A] {intr};""", 0)>]

    // pointers to predicates matching pred(..)
    [<DataRow("MS1", "def pred B(a:pred(y:obj)) def pred A(z:obj) def pred Test() {B(A)};", 0)>] // OK: pred(y:obj) matches signature A(obj)
    [<DataRow("MS1a", "def pred B(a:pred(y:obj)) def pred A(z:obj) def pred Test() {dec ~x:obj; B(A(x))};", 1)>] // SIG04: pred(y:obj) does not match value A(obj) 
    [<DataRow("MS1b", "def pred B(a:pred(y:obj)) def pred A(z:obj) def pred Test() {dec ~x:ind; B(A(x))};", 1)>] // SIG04: pred(y:obj) does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c", "def pred B(a:pred(y:obj)) def pred A(z:ind) def pred Test() {dec ~x:ind; B(A(x))};", 1)>] // SIG04: pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "def pred B(a:pred(y:obj)) def pred A(z:ind) def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "def pred B(a:pred(y:obj)) ax A {true} def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "def pred B(a:pred(y:obj)) thm A {true} def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "def pred B(a:pred(y:obj)) lem A {true} def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "def pred B(a:pred(y:obj)) prop A {true} def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "def pred B(a:pred(y:obj)) conj A {true} def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "def pred B(a:pred(y:obj)) cor A$1 {true} def pred Test() {B(A$1)};", 1)>] // SIG04: pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "def pred B(a:pred(y:obj)) proof A$1 {1. |- trivial} def pred Test() {B(A$1)};", 1)>] // SIG04: pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "def pred B(a:pred(y:obj)) inf A {pre:true con:true} def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def pred B(a:pred(y:obj)) def func A()obj def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "def pred B(a:pred(y:obj)) ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test() {B(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (extension)

    // -----------------------------

    // match return with mapping having simple types
    [<DataRow("ST0", "def pred Test(v:obj) def pred T() {dec ~x:obj; Test(x)};", 0)>]
    [<DataRow("ST1", "def pred Test(v:ind) def pred T() {dec ~x:ind; Test(x)};", 0)>]
    [<DataRow("ST2", "def pred Test(v:func) def pred T() {dec ~x:func; Test(x)};", 0)>]
    [<DataRow("ST2a", "def pred Test(v:func) def pred T() {dec ~x:func()->ind; Test(x)};", 0)>]
    [<DataRow("ST2b", "def pred Test(v:func) def pred T() {dec ~x:func(y:obj)->ind; Test(x)};", 0)>]
    [<DataRow("ST2c", "def pred Test(v:func) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 0)>]
    [<DataRow("ST2d", "def pred Test(v:func) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 0)>]
    [<DataRow("ST3", "def pred Test(v:pred) def pred T() {dec ~x:pred; Test(x)};", 0)>]
    [<DataRow("ST3a", "def pred Test(v:pred) def pred T() {dec ~x:pred(); Test(x)};", 0)>]
    [<DataRow("ST3b", "def pred Test(v:pred) def pred T() {dec ~x:pred; Test(x)};", 0)>]
    [<DataRow("ST3c", "def pred Test(v:pred) def pred T() {dec ~x:pred(y:obj); Test(x)};", 0)>]

    // mismatch return with mapping having simple type obj
    [<DataRow("ST0_obj", "def pred Test(v:obj) def pred T() {dec ~x:obj; Test(x)};", 0)>]
    [<DataRow("ST1_obj", "def pred Test(v:obj) def pred T() {dec ~x:ind; Test(x)};", 1)>]
    [<DataRow("ST2_obj", "def pred Test(v:obj) def pred T() {dec ~x:func; Test(x)};", 1)>]
    [<DataRow("ST2a_obj", "def pred Test(v:obj) def pred T() {dec ~x:func()->ind; Test(x)};", 1)>]
    [<DataRow("ST2b_obj", "def pred Test(v:obj) def pred T() {dec ~x:func(y:obj)->ind; Test(x)};", 1)>]
    [<DataRow("ST2c_obj", "def pred Test(v:obj) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 1)>]
    [<DataRow("ST2d_obj", "def pred Test(v:obj) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 1)>]
    [<DataRow("ST3_obj", "def pred Test(v:obj) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("ST3a_obj", "def pred Test(v:obj) def pred T() {dec ~x:pred(); Test(x)};", 1)>]
    [<DataRow("ST3b_obj", "def pred Test(v:obj) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("ST3c_obj", "def pred Test(v:obj) def pred T() {dec ~x:pred(y:obj); Test(x)};", 1)>]

    // mismatch return with mapping having simple type ind
    [<DataRow("ST0_ind", "def pred Test(v:ind) def pred T() {dec ~x:obj; Test(x)};", 1)>]
    [<DataRow("ST1_ind", "def pred Test(v:ind) def pred T() {dec ~x:ind; Test(x)};", 0)>]
    [<DataRow("ST2_ind", "def pred Test(v:ind) def pred T() {dec ~x:func; Test(x)};", 1)>]
    [<DataRow("ST2a_ind", "def pred Test(v:ind) def pred T() {dec ~x:func()->ind; Test(x)};", 1)>]
    [<DataRow("ST2b_ind", "def pred Test(v:ind) def pred T() {dec ~x:func(y:obj)->ind; Test(x)};", 1)>]
    [<DataRow("ST2c_ind", "def pred Test(v:ind) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 1)>]
    [<DataRow("ST2d_ind", "def pred Test(v:ind) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 1)>]
    [<DataRow("ST3_ind", "def pred Test(v:ind) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("ST3a_ind", "def pred Test(v:ind) def pred T() {dec ~x:pred(); Test(x)};", 1)>]
    [<DataRow("ST3b_ind", "def pred Test(v:ind) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("ST3c_ind", "def pred Test(v:ind) def pred T() {dec ~x:pred(y:obj); Test(x)};", 1)>]

    // mismatch return with mapping having simple type pred
    [<DataRow("ST0_pred", "def pred Test(v:pred) def pred T() {dec ~x:obj; Test(x)};", 1)>]
    [<DataRow("ST1_pred", "def pred Test(v:pred) def pred T() {dec ~x:ind; Test(x)};", 1)>]
    [<DataRow("ST2_pred", "def pred Test(v:pred) def pred T() {dec ~x:func; Test(x)};", 1)>]
    [<DataRow("ST2a_pred", "def pred Test(v:pred) def pred T() {dec ~x:func()->ind; Test(x)};", 1)>]
    [<DataRow("ST2b_pred", "def pred Test(v:pred) def pred T() {dec ~x:func(y:obj)->pred; Test(x)};", 1)>]
    [<DataRow("ST2c_pred", "def pred Test(v:pred) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 1)>]
    [<DataRow("ST2d_pred", "def pred Test(v:pred) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 1)>]
    [<DataRow("ST3_pred", "def pred Test(v:pred) def pred T() {dec ~x:pred; Test(x)};", 0)>]
    [<DataRow("ST3a_pred", "def pred Test(v:pred) def pred T() {dec ~x:pred(); Test(x)};", 0)>]
    [<DataRow("ST3b_pred", "def pred Test(v:pred) def pred T() {dec ~x:pred; Test(x)};", 0)>]
    [<DataRow("ST3c_pred", "def pred Test(v:pred) def pred T() {dec ~x:pred(y:obj); Test(x)};", 0)>]

    // mismatch return with mapping having simple type func
    [<DataRow("ST0_func", "def pred Test(v:func) def pred T() {dec ~x:obj; Test(x)};", 1)>]
    [<DataRow("ST1_func", "def pred Test(v:func) def pred T() {dec ~x:ind; Test(x)};", 1)>]
    [<DataRow("ST2_func", "def pred Test(v:func) def pred T() {dec ~x:func; Test(x)};", 0)>]
    [<DataRow("ST2a_func", "def pred Test(v:func) def pred T() {dec ~x:func()->ind; Test(x)};", 0)>]
    [<DataRow("ST2b_func", "def pred Test(v:func) def pred T() {dec ~x:func(y:obj)->pred; Test(x)};", 0)>]
    [<DataRow("ST2c_func", "def pred Test(v:func) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 0)>]
    [<DataRow("ST2d_func", "def pred Test(v:func) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 0)>]
    [<DataRow("ST3_func", "def pred Test(v:func) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("ST3a_func", "def pred Test(v:func) def pred T() {dec ~x:pred(); Test(x)};", 1)>]
    [<DataRow("ST3b_func", "def pred Test(v:func) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("ST3c_func", "def pred Test(v:func) def pred T() {dec ~x:pred(y:obj); Test(x)};", 1)>]

    // (mis)match return with mapping having pred() types
    [<DataRow("NP0", "def pred Test(v:pred()) def pred T() {dec ~x:obj; Test(x)};", 1)>]
    [<DataRow("NP1", "def pred Test(v:pred()) def pred T() {dec ~x:ind; Test(x)};", 1)>]
    [<DataRow("NP2", "def pred Test(v:pred()) def pred T() {dec ~x:func; Test(x)};", 1)>]
    [<DataRow("NP2a", "def pred Test(v:pred()) def pred T() {dec ~x:func()->ind; Test(x)};", 1)>]
    [<DataRow("NP2b", "def pred Test(v:pred()) def pred T() {dec ~x:func(y:obj)->ind; Test(x)};", 1)>]
    [<DataRow("NP2c", "def pred Test(v:pred()) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 1)>]
    [<DataRow("NP2d", "def pred Test(v:pred()) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 1)>]
    [<DataRow("NP3", "def pred Test(v:pred()) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("NP3a", "def pred Test(v:pred()) def pred T() {dec ~x:pred(); Test(x)};", 0)>]
    [<DataRow("NP3b", "def pred Test(v:pred()) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("NP3c", "def pred Test(v:pred()) def pred T() {dec ~x:pred(y:obj); Test(x)};", 1)>]
    // (mis)match return with mapping having pred(...) types
    [<DataRow("NP_0", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:obj; Test(x)};", 1)>]
    [<DataRow("NP_1", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:ind; Test(x)};", 1)>]
    [<DataRow("NP_2", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:func; Test(x)};", 1)>]
    [<DataRow("NP_2a", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:func()->ind; Test(x)};", 1)>]
    [<DataRow("NP_2b", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:func(y:obj)->ind; Test(x)};", 1)>]
    [<DataRow("NP_2c", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 1)>]
    [<DataRow("NP_2d", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 1)>]
    [<DataRow("NP_3", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("NP_3a", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:pred(); Test(x)};", 1)>]
    [<DataRow("NP_3b", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("NP_3c", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:pred(y:obj); Test(x)};", 0)>]
    [<DataRow("NP_3c", "def pred Test(v:pred(a:obj)) def pred T() {dec ~x:pred(y:ind); Test(x)};", 1)>]

    // (mis)match return with mapping having func() types
    [<DataRow("NF0", "def pred Test(v:func()->ind) def pred T() {dec ~x:obj; Test(x)};", 1)>]
    [<DataRow("NF1", "def pred Test(v:func()->ind) def pred T() {dec ~x:ind; Test(x)};", 1)>]
    [<DataRow("NF2", "def pred Test(v:func()->ind) def pred T() {dec ~x:func; Test(x)};", 1)>]
    [<DataRow("NF2a", "def pred Test(v:func()->ind) def pred T() {dec ~x:func()->ind; Test(x)};", 0)>]
    [<DataRow("NF2b", "def pred Test(v:func()->ind) def pred T() {dec ~x:func(y:obj)->ind; Test(x)};", 1)>]
    [<DataRow("NF2c", "def pred Test(v:func()->ind) def pred T() {dec ~x:func(y:obj)->obj; Test(x)};", 1)>]
    [<DataRow("NF2d", "def pred Test(v:func()->ind) def pred T() {dec ~x:func(y:ind)->ind; Test(x)};", 1)>]
    [<DataRow("NF2e", "def pred Test(v:func()->ind) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 1)>]
    [<DataRow("NF2f", "def pred Test(v:func()->ind) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 1)>]
    [<DataRow("NF3", "def pred Test(v:func()->ind) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("NF3a", "def pred Test(v:func()->ind) def pred T() {dec ~x:pred(); Test(x)};", 1)>]
    [<DataRow("NF3b", "def pred Test(v:func()->ind) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("NF3c", "def pred Test(v:func()->ind) def pred T() {dec ~x:pred(y:obj); Test(x)};", 1)>]
    // (mis)match return with mapping having func(...) types
    [<DataRow("NF_0", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:obj; Test(x)};", 1)>]
    [<DataRow("NF_1", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:ind; Test(x)};", 1)>]
    [<DataRow("NF_2", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:func; Test(x)};", 1)>]
    [<DataRow("NF_2a", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:func()->ind; Test(x)};", 1)>]
    [<DataRow("NF_2b", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:func(y:obj)->ind; Test(x)};", 0)>]
    [<DataRow("NF_2c", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:func(y:obj)->obj; Test(x)};", 1)>]
    [<DataRow("NF_2d", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:func(y:ind)->ind; Test(x)};", 1)>]
    [<DataRow("NF_2e", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:func(y:obj)->func; Test(x)};", 1)>]
    [<DataRow("NF_2f", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:func(y:obj)->func(z:pred)->pred; Test(x)};", 1)>]
    [<DataRow("NF_3", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("NF_3a", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:pred(); Test(x)};", 1)>]
    [<DataRow("NF_3b", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:pred; Test(x)};", 1)>]
    [<DataRow("NF_3c", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:pred(y:obj); Test(x)};", 1)>]
    [<DataRow("NF_3d", "def pred Test(v:func(a:obj)->ind) def pred T() {dec ~x:pred(y:ind); Test(x)};", 1)>]

    // match return with mapping having class type
    [<DataRow("CT1", "def cl A def pred Test(v:obj) def pred T() {dec ~x:A; Test(x)};", 0)>] // A is obj, no error
    [<DataRow("CT2", "def cl A def pred Test(v:A) def pred T() {dec ~x:A; Test(x)};", 0)>] // A is A, no error
    [<DataRow("CT3", "def cl A def cl B:A def pred Test(v:A) def pred T() {dec ~x:B; Test(x)};", 0)>] // x is also B:A, no error
    [<DataRow("CT4", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec ~x:B; Test(x)};", 0)>] // x is B, no error
    [<DataRow("CT5", "def cl A def cl B:A def pred Test(v:obj) def pred T() {dec ~x:B; Test(x)};", 0)>] // x is also B:A:obj, no error
    // ... with instances
    [<DataRow("CI1", "def cl A def pred Test(v:obj) def pred T() {dec ~x:A x:=A(); Test(x)};", 0)>] // A is obj, no error
    [<DataRow("CI2", "def cl A def pred Test(v:A) def pred T() {dec ~x:A x:=A(); Test(x)};", 0)>] // A is A, no error
    [<DataRow("CI3", "def cl A def cl B:A def pred Test(v:A) def pred T() {dec ~x:B x:=B(); Test(x)};", 0)>] // x is also B:A, no error
    [<DataRow("CI4", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec ~x:B x:=B(); Test(x)};", 0)>] // x is B, no error
    [<DataRow("CI5", "def cl A def cl B:A def pred Test(v:obj) def pred T() {dec ~x:B x:=B(); Test(x)};", 0)>] // x is also B:A:obj, no error

    // mismatch return with mapping having class type
    [<DataRow("CT1_", "def pred Test(v:obj) def pred T() {dec ~x:A; Test(x)};", 1)>] // A is undefined, error
    [<DataRow("CT2_", "def cl A def pred Test(v:A) def pred T() {dec ~x:obj; Test(x)};", 1)>] // obj is not A, error
    [<DataRow("CT3_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec ~a:A; Test(a)};", 1)>] // A is not B, error
    // mismatch with class references
    [<DataRow("CI1_", "def pred Test(v:obj) def pred T() {dec ~x:A x:=A; Test(x)};", 1)>] // A is undefined, error
    [<DataRow("CI2_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec ~a:A a:=A; Test(a)};", 1)>] // A is not B, error
    [<DataRow("CI3_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec ~a:B a:=B; Test(a)};", 1)>] // B is B, but a class reference, error
    [<DataRow("CI4_", "def cl A def pred Test(v:obj) def pred T() {dec ~x:A x:=A; Test(x)};", 1)>] // A is obj, but x is class reference, error
    [<DataRow("CI5_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec ~x:B x:=B; Test(x)};", 1)>] // B is B, but x is class referene, error
    [<DataRow("CI6_", "def cl A def pred Test(v:A) def pred T() {dec ~x:A x:=A; Test(x)};", 1)>] // A is A, but x is class reference, error
    [<DataRow("CI7_", "def cl A def cl B:A def pred Test(v:A) def pred T() {dec ~x:B x:=B; Test(x)};", 1)>] // B is A but x is a class reference, error 
    [<DataRow("CI8_", "def cl A def cl B:A def pred Test(v:B) def pred T() {dec ~x:B x:=B; Test(x)};", 1)>] // x is B, but class reference, error
    [<DataRow("CI9_", "def cl A def cl B:A def pred Test(v:obj) def pred T() {dec ~x:B x:=B; Test(x)};", 1)>] // B is obj but x is class reference, error

    // match return with mapping having the type pred(...) 
    [<DataRow("MS1", "def pred A(z:obj) def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 0)>] // OK: ->pred(y:obj) matches signature A(obj), whole node would be returned
    [<DataRow("MS1a", "def pred A(z:obj) def pred Test(v:pred(y:obj)) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: pred(y:obj) does not match value A(obj) 
    [<DataRow("MS1b", "def pred A(z:obj) def pred Test(v:pred(y:obj)) def pred T() {dec ~x:ind; Test(A(x))};", 1)>] // SIG04: pred(y:obj) does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c", "def pred A(z:ind) def pred Test(v:pred(y:obj)) def pred T() {dec ~x:ind; Test(A(x))};", 1)>] // SIG04: pred(y:obj) does not match value A(ind) 
    [<DataRow("MS1d", "def pred A(z:ind) def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A(ind)
    [<DataRow("MS1e", "ax A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (axiom)
    [<DataRow("MS1f", "thm A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (theorem)
    [<DataRow("MS1g", "lem A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (lemma)
    [<DataRow("MS1h", "prop A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (proposition)
    [<DataRow("MS1i", "conj A {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (conjecture)
    [<DataRow("MS1j", "cor A$1 {true} def pred Test(v:pred(y:obj)) def pred T() {Test(A$1)};", 1)>] // SIG04: pred(y:obj) does not match signature A (corollary)
    [<DataRow("MS1k", "proof A$1 {1. |- trivial} def pred Test(v:pred(y:obj)) def pred T() {Test(A$1)};", 1)>] // SIG04: pred(y:obj) does not match signature A (proof)
    [<DataRow("MS1l", "inf A {pre:true con:true} def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (rule of inference)
    [<DataRow("MS1m", "def func A()->obj def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (functional term)
    [<DataRow("MS1n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test(v:pred(y:obj)) def pred T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (extension)
    [<DataRow("MS1o", "def cl A def pred Test(v:pred(y:obj)) def T() {Test(A)};", 1)>] // SIG04: pred(y:obj) does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1p2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1p3", "def cl A {intr prty pred X() } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1p4", "def cl A {intr prty func X()->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X()->obj
    [<DataRow("MS1p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1p7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1q2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred(y:obj)) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1q3", "def cl A {intr prty pred X() } def pred Test(v:pred(y:obj)) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q4", "def cl A {intr prty func X()->obj } def pred Test(v:pred(y:obj)) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred(y:obj)) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred(y:obj)) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1q7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec ~a:obj ~o:A o:=A(); Test(o.X(a))};", 1)>] // SIG04: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1r2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1r3", "def pred A() {intr prty pred X() } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r4", "def pred A() {intr prty func X()->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1r7", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: pred(y:obj) does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 0)>] // OK: ->pred(y:obj) matches signature A.X(obj) 
    [<DataRow("MS1s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)
    [<DataRow("MS1s3", "def func A()->obj {intr prty pred X() } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X()
    [<DataRow("MS1s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(obj)->obj
    [<DataRow("MS1s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:pred(y:obj)) def pred T() {Test(A.X)};", 1)>] // SIG04: pred(y:obj) does not match signature A.X(ind)->obj
    [<DataRow("MS1s7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred(y:obj)) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: pred(y:obj) does not match by value A.X(obj) 
    
    // match return with mapping having the type pred() 
    [<DataRow("MS1_", "def pred A() def pred Test(v:pred()) def pred T() {Test(A)};", 0)>] // OK: ->pred() matches signature A(), whole node would be returned
    [<DataRow("MS1a_", "def pred A() def pred Test(v:pred()) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: pred() does not match value A(obj) 
    [<DataRow("MS1b_", "def pred A() def pred Test(v:pred()) def pred T() {dec ~x:ind; Test(A(x))};", 1)>] // SIG04: pred() does not match value A(ind) not matching A(obj)
    [<DataRow("MS1c_", "def pred A() def pred Test(v:pred()) def pred T() {dec ~x:ind; Test(A(x))};", 1)>] // SIG04: pred() does not match value A(ind) 
    [<DataRow("MS1d_", "def pred A(x:ind) def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A(ind)
    [<DataRow("MS1e_", "ax A {true} def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (axiom)
    [<DataRow("MS1f_", "thm A {true} def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (theorem)
    [<DataRow("MS1g_", "lem A {true} def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (lemma)
    [<DataRow("MS1h_", "prop A {true} def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (proposition)
    [<DataRow("MS1i_", "conj A {true} def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (conjecture)
    [<DataRow("MS1j_", "cor A$1 {true} def pred Test(v:pred()) def pred T() {Test(A$1)};", 1)>] // SIG04: pred() does not match signature A (corollary)
    [<DataRow("MS1k_", "proof A$1 {1. |- trivial} def pred Test(v:pred()) def pred T() {Test(A$1)};", 1)>] // SIG04: pred() does not match signature A (proof)
    [<DataRow("MS1l_", "inf A {pre:true con:true} def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (rule of inference)
    [<DataRow("MS1m_", "def func A()->obj def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (functional term)
    [<DataRow("MS1n_", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (extension)
    [<DataRow("MS1o_", "def cl A def pred Test(v:pred()) def pred T() {Test(A)};", 1)>] // SIG04: pred() does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS1p_1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1p_2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1p_3", "def cl A {intr prty pred X() } def pred Test(v:pred()) def pred T() {Test(A.X)};", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1p_4", "def cl A {intr prty func X()->obj } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1p_5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1p_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1p_7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS1q_1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1q_2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred()) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1q_3", "def cl A {intr prty pred X() } def pred Test(v:pred()) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1q_4", "def cl A {intr prty func X()->obj } def pred Test(v:pred()) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1q_5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred()) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1q_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred()) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1q_7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec ~a:obj ~o:A o:=A(); Test(o.X(a))};", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS1r_1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1r_2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1r_3", "def pred A() {intr prty pred X() } def pred Test(v:pred()) def pred T() {Test(A.X)};", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1r_4", "def pred A() {intr prty func X()->obj } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X()
    [<DataRow("MS1r_5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1r_6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1r_7", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: pred() does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS1s_1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(obj) 
    [<DataRow("MS1s_2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(ind)
    [<DataRow("MS1s_3", "def func A()->obj {intr prty pred X() } def pred Test(v:pred()) def pred T() {Test(A.X)};", 0)>] // OK: ->pred() match signature A.X()
    [<DataRow("MS1s_4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X()->obj
    [<DataRow("MS1s_5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)};", 1)>] // SIG04: pred() does not match signature A.X(obj)->obj
    [<DataRow("MS1s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:pred()) def pred T() {Test(A.X)}", 1)>] // SIG04: pred() does not match signature A.X(ind)->obj
    [<DataRow("MS1s_7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred()) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: pred() does not match by value A.X(obj) 
     
    // match return with mapping having the type pred 
    [<DataRow("MS2", "def pred A(z:obj) def pred Test(v:pred) def pred T() {Test(A)};", 0)>] // OK: ->pred matches signature A(obj), whole node would be returned
    [<DataRow("MS2a", "def pred A(z:obj) def pred Test(v:pred) def pred T() {dec ~x:obj; Test(A(x))};", 0)>] // OK: ->pred matches value A(obj) 
    [<DataRow("MS2b", "def pred A(z:obj) def pred Test(v:pred) def pred T() {dec ~x:ind; Test(A(x))};", 1)>] // SIG04: pred does not match value A(ind) since it does not match A(obj)
    [<DataRow("MS2c", "def pred A(z:ind) def pred Test(v:pred) def pred T() {dec ~x:ind; Test(A(x))};", 0)>] // OK: ->pred matches value A(ind) 
    [<DataRow("MS2d", "def pred A(z:ind) def pred Test(v:pred) def pred T() {Test(A)};", 0)>] // OK: ->pred matches signature A(ind)
    [<DataRow("MS2e", "ax A {true} def pred Test(v:pred) def pred T() {Test(A)};", 0)>] // OK: ->pred matches signature A (axiom)
    [<DataRow("MS2f", "thm A {true} def pred Test(v:pred) def pred T() {Test(A)};", 0)>] // OK: ->pred matches signature A (theorem)
    [<DataRow("MS2g", "lem A {true} def pred Test(v:pred) def pred T() {Test(A)};", 0)>] // OK: ->pred matches signature A (lemma)
    [<DataRow("MS2h", "prop A {true} def pred Test(v:pred) def pred T() {Test(A)};", 0)>] // OK: ->pred matches signature A (proposition)
    [<DataRow("MS2i", "conj A {true} def pred Test(v:pred) def pred T() {Test(A)};", 0)>] // OK: ->pred does matches signature A (conjecture)
    [<DataRow("MS2j", "cor A$1 {true} def pred Test(v:pred) def pred T() {Test(A$1)};", 0)>] // OK: ->pred matches signature A$1 (corollary)
    [<DataRow("MS2k", "proof A$1 {1. |- trivial} def pred Test(v:pred) def pred T() {Test(A$1)};", 1)>] // SIG04: pred does not match signature A$1 (proof)
    [<DataRow("MS2l", "inf A {pre:true con:true} def pred Test(v:pred) def pred T() {Test(A)};", 1)>] // SIG04: pred does not match signature A (rule of inference)
    [<DataRow("MS2m", "def func A()->obj def pred Test(v:pred) def pred T() {Test(A)};", 1)>] // SIG04: pred does not match signature A (functional term)
    [<DataRow("MS2n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test(v:pred) def pred T() {Test(A)};", 1)>] // SIG04: pred does not match signature A (extension)
    [<DataRow("MS2o", "def cl A def pred Test(v:pred) def pred T() {Test(A)};", 1)>] // SIG04: pred does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS2p1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred matches signature A.A(obj) 
    [<DataRow("MS2p2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred does not match signature A.X(ind)
    [<DataRow("MS2p3", "def cl A {intr prty pred X() } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2p4", "def cl A {intr prty func X()->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2p7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: pred does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS2q1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2q2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:pred) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2q3", "def cl A {intr prty pred X() } def pred Test(v:pred) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2q4", "def cl A {intr prty func X()->obj } def pred Test(v:pred) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:pred) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:pred) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2q7", "def cl A {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec ~a:obj ~o:A o:=A(); Test(o.X(a))};", 1)>] // SIG04: pred does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS2r1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2r2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2r3", "def pred A() {intr prty pred X() } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2r4", "def pred A() {intr prty func X()->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2r7", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec ~a:obj; T(A.X(a))};", 1)>] // SIG04: pred does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS2s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred matches signature A.X(obj) 
    [<DataRow("MS2s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred matches signature A.X(ind)
    [<DataRow("MS2s3", "def func A()->obj {intr prty pred X() } def pred Test(v:pred) def pred T() {Test(A.X)};", 0)>] // OK: ->pred match signature A.X()
    [<DataRow("MS2s4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X()->obj
    [<DataRow("MS2s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X(obj)->obj
    [<DataRow("MS2s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:pred) def pred T() {Test(A.X)};", 1)>] // SIG04: pred does not match signature A.X(ind)->obj
    [<DataRow("MS2s7", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:pred) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: pred does not match by value A.X(obj) 

    // match return with mapping having the type func(...)->...
    [<DataRow("MS3", "def func A(z:obj)->ind def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 0)>] // OK: ->func(y:obj)->ind matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS3a", "def func A(z:obj)->ind def pred Test(v:func(y:obj)->ind) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func(y:obj)->ind does not match value A(obj) 
    [<DataRow("MS3b", "def func A(z:obj)->ind def pred Test(v:func(y:obj)->ind) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func(y:obj)->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c", "def func A(z:ind)->ind def pred Test(v:func(y:obj)->ind) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func(y:obj)->ind does not match value A(ind) 
    [<DataRow("MS3d", "def func A(z:ind)->ind def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A(ind)->ind
    [<DataRow("MS3e", "ax A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (axiom)
    [<DataRow("MS3f", "thm A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (theorem)
    [<DataRow("MS3g", "lem A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (lemma)
    [<DataRow("MS3h", "prop A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (proposition)
    [<DataRow("MS3i", "conj A {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (conjecture)
    [<DataRow("MS3j", "cor A$1 {true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A$1)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k", "proof A$1 {1. |- trivial} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A$1)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A$1 (proof)
    [<DataRow("MS3l", "inf A {pre:true con:true} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (rule of inference)
    [<DataRow("MS3m", "def func A(z:obj)->func()->obj def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o", "def cl A def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3p2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3p3", "def cl A {intr prty pred X() } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3p4", "def cl A {intr prty func X()->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3p5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:func(y:obj)->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3q2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3q3", "def cl A {intr prty pred X() } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3q4", "def cl A {intr prty func X()->obj } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3q5", "def cl A {intr prty func X(x:obj)->obj } def pred Test(v:func(y:obj)->obj) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~a:obj ~o:A o:=A(); Test(o.X(a))};", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3r2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3r3", "def pred A() {intr prty pred X() } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3r4", "def pred A() {intr prty func X()->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3r5", "def pred A() {intr prty func X(x:obj)->obj } def pred Test(v:func(y:obj)->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind matches signature A.A(obj) 
    [<DataRow("MS3s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)
    [<DataRow("MS3s3", "def func A()->obj {intr prty pred X() } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind match signature A.X()
    [<DataRow("MS3s4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X()->obj
    [<DataRow("MS3s5", "def func A()->obj {intr prty func X(x:obj)->obj } def pred Test(v:func(y:obj)->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func(y:obj)->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:func(y:obj)->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test(v:func(y:obj)->ind) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func(y:obj)->ind does not match by value A.X(obj) 
    
    // match return with mapping having the type func()->...
    [<DataRow("MS3_", "def func A()->ind def pred Test(v:func()->ind) def pred T() {Test(A)};", 0)>] // OK: ->func()->ind matches signature A()->ind, whole node would be returned
    [<DataRow("MS3a_", "def func A()->ind def pred Test(v:func()->ind) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func()->ind does not match value A(obj) 
    [<DataRow("MS3b_", "def func A()->ind def pred Test(v:func()->ind) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func()->ind does not match value A(ind) not matching A(obj)
    [<DataRow("MS3c_", "def func A()->ind def pred Test(v:func()->ind) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func()->ind does not match value A(ind) 
    [<DataRow("MS3d_", "def func A(z:ind)->ind def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A(ind)->ind
    [<DataRow("MS3e_", "ax A {true} def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A (axiom)
    [<DataRow("MS3f_", "thm A {true} def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A (theorem)
    [<DataRow("MS3g_", "lem A {true} def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A (lemma)
    [<DataRow("MS3h_", "prop A {true} def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A (proposition)
    [<DataRow("MS3i_", "conj A {true} def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A (conjecture)
    [<DataRow("MS3j_", "cor A$1 {true} def pred Test(v:func()->ind) def pred T() {Test(A$1)};", 1)>] // SIG04: func()->ind does not match signature A$1 (corollary)
    [<DataRow("MS3k_", "proof A$1 {1. |- trivial} def pred Test(v:func()->ind) def pred T() {Test(A$1)};", 1)>] // SIG04: func()->ind does not match signature A$1 (proof)
    [<DataRow("MS3l_", "inf A {pre:true con:true} def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A (rule of inference)
    [<DataRow("MS3m_", "def func A()->func()->obj def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A(z:obj)->func()->obj (functional term)
    [<DataRow("MS3n_", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test(v:func(y:obj)->ind) def pred T() {Test(A)};", 1)>] // SIG04: func(y:obj)->ind does not match signature A (extension)
    [<DataRow("MS3o_", "def cl A def pred Test(v:func()->ind) def pred T() {Test(A)};", 1)>] // SIG04: func()->ind does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS3p_1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3p_2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3p_3", "def cl A {intr prty pred X() } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3p_4", "def cl A {intr prty func X()->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3p_5", "def cl A {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3p_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3p_7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func()->ind) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS3q_1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func()->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3q_2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func()->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3q_3", "def cl A {intr prty pred X() } def pred Test(v:func()->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3q_4", "def cl A {intr prty func X()->obj } def pred Test(v:func()->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3q_5", "def cl A {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3q_6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func()->ind) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3q_7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func()->ind) def pred T() {dec ~a:obj ~o:A o:=A(); Test(o.X(a))};", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS3r_1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3r_2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3r_3", "def pred A() {intr prty pred X() } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3r_4", "def pred A() {intr prty func X()->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3r_5", "def pred A() {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3r_6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3r_7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test(v:func()->ind) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS3s_1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind matches signature A.A(obj) 
    [<DataRow("MS3s_2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X(ind)
    [<DataRow("MS3s_3", "def func A()->obj {intr prty pred X() } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind match signature A.X()
    [<DataRow("MS3s_4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X()->obj
    [<DataRow("MS3s_5", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func()->ind matches signature A.X(obj)->obj
    [<DataRow("MS3s_6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:func()->ind) def pred T() {Test(A.X)};", 1)>] // SIG04: func()->ind does not match signature A.X(ind)->obj
    [<DataRow("MS3s_7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test(v:func()->ind) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func()->ind does not match by value A.X(obj) 

    // match return with mapping having the type func
    [<DataRow("MS4", "def func A(z:obj)->ind def pred Test(v:func) def pred T() {Test(A)};", 0)>] // OK: ->func matches signature A(obj)->ind, whole node would be returned
    [<DataRow("MS4a", "def func A(z:obj)->ind def pred Test(v:func) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func does not match value A(obj) 
    [<DataRow("MS4b", "def func A(z:obj)->ind def pred Test(v:func) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func does not match value A(ind) not matching A(obj)
    [<DataRow("MS4c", "def func A(z:ind)->ind def pred Test(v:func) def pred T() {dec ~x:obj; Test(A(x))};", 1)>] // SIG04: func does not match value A(ind) 
    [<DataRow("MS4d", "def func A(z:ind)->func(a:obj)->ind def pred Test(v:func) def pred T() {Test(A)};", 0)>] // OK: ->func matches signature A(ind)->func(obj)->ind
    [<DataRow("MS4e", "ax A {true} def pred Test(v:func) def pred T() {Test(A)};", 1)>] // SIG04: func does not matche signature A (axiom)
    [<DataRow("MS4f", "thm A {true} def pred Test(v:func) def pred T() {Test(A)};", 1)>] // SIG04: func does not matche signature A (theorem)
    [<DataRow("MS4g", "lem A {true} def pred Test(v:func) def pred T() {Test(A)};", 1)>] // SIG04: func does not matche signature A (lemma)
    [<DataRow("MS4h", "prop A {true} def pred Test(v:func) def pred T() {Test(A)};", 1)>] // SIG04: func does not matche signature A (proposition)
    [<DataRow("MS4i", "conj A {true} def pred Test(v:func) def pred T() {Test(A)};", 1)>] // SIG04: func does not matche signature A (conjecture)
    [<DataRow("MS4j", "cor A$1 {true} def pred Test(v:func) def pred T() {Test(A$1)};", 1)>] // SIG04: func does not matche signature A$1 (corollary)
    [<DataRow("MS4k", "proof A$1 {1. |- trivial} def pred Test(v:func) def pred T() {Test(A$1)};", 1)>] // SIG04: func does not matche signature A$1 (proof)
    [<DataRow("MS4l", "inf A {pre:true con:true} def pred Test(v:func) def pred T() {Test(A)};", 1)>] // SIG04: func does not matche signature A (rule of inference)
    [<DataRow("MS4m", "def func A(z:obj)->func()->obj def pred Test(v:func) def pred T() {Test(A)};", 0)>] // OK: ->func matches signature A (functional term)
    [<DataRow("MS4n", "ext A x@/\d+/ -> obj {dec ~y:obj; return y} def pred Test(v:func) def pred T() {Test(A)};", 1)>] // SIG04: func does not match signature A (extension)
    [<DataRow("MS4o", "def cl A def pred Test(v:func) def pred T() {Test(A)};", 1)>] // SIG04: func does not match signature A (class)
    // ... using properties of classes
    [<DataRow("MS4p1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4p2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4p3", "def cl A {intr prty pred X() } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4p4", "def cl A {intr prty func X()->obj } def pred Test(v:func) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4p5", "def cl A {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4p6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4p7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of instances
    [<DataRow("MS4q1", "def cl A {intr prty pred X(x:obj) } def pred Test(v:func) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4q2", "def cl A {intr prty pred X(x:ind) } def pred Test(v:func) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4q3", "def cl A {intr prty pred X() } def pred Test(v:func) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4q4", "def cl A {intr prty func X()->obj } def pred Test(v:func) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4q5", "def cl A {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4q6", "def cl A {intr prty func X(x:ind)->obj } def pred Test(v:func) def pred T() {dec ~o:A o:=A(); Test(o.X)};", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4q7", "def cl A {intr prty func X(x:obj)->ind } def pred Test(v:func) def pred T() {dec ~a:obj ~o:A o:=A(); Test(o.X(a))};", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of predicates
    [<DataRow("MS4r1", "def pred A() {intr prty pred X(x:obj) } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4r2", "def pred A() {intr prty pred X(x:ind) } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4r3", "def pred A() {intr prty pred X() } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4r4", "def pred A() {intr prty func X()->obj } def pred Test(v:func) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4r5", "def pred A() {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4r6", "def pred A() {intr prty func X(x:ind)->obj } def pred Test(v:func) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4r7", "def pred A() {intr prty func X(x:obj)->ind } def pred Test(v:func) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func does not match by value A.X(obj) 
    // ... using properties of functional terms
    [<DataRow("MS4s1", "def func A()->obj {intr prty pred X(x:obj) } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func matches signature A.A(obj) 
    [<DataRow("MS4s2", "def func A()->obj {intr prty pred X(x:ind) } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func does not match signature A.X(ind)
    [<DataRow("MS4s3", "def func A()->obj {intr prty pred X() } def pred Test(v:func) def pred T() {Test(A.X)};", 1)>] // SIG04: func match signature A.X()
    [<DataRow("MS4s4", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X()->obj
    [<DataRow("MS4s5", "def func A()->obj {intr prty func X()->obj } def pred Test(v:func()->obj) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X(obj)->obj
    [<DataRow("MS4s6", "def func A()->obj {intr prty func X(x:ind)->obj } def pred Test(v:func) def pred T() {Test(A.X)};", 0)>] // OK: ->func matches signature A.X(ind)->obj
    [<DataRow("MS4s7", "def func A()->obj {intr prty func X(x:obj)->ind } def pred Test(v:func) def pred T() {dec ~a:obj; Test(A.X(a))};", 1)>] // SIG04: func does not match by value A.X(obj) 

    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG04(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG04 ("", 0, "")
            ad.Clear()
            runTestHelper "TestSIG04.fpl" fplCode code expected

    [<DataRow("inh", """def cl A def pred T() {dec ~n:A n:=A(); true};""", 0)>]
    [<DataRow("inh_a", """def cl A def pred T() {dec ~n:obj n:=A(); true};""", 0)>]
    [<DataRow("inh_b", """def cl A def cl B:A def pred T() {dec ~n:A n:=B(); true};""", 0)>]
    [<DataRow("inh_c", """def cl A def cl B:A def pred T() {dec ~n:B n:=A(); true};""", 1)>]
    [<DataRow("inh_d", """def cl A def cl B:A def pred T() {dec ~n:obj n:=B(); true};""", 0)>]
    [<DataRow("inh_e", """def cl A def cl B:A def pred T() {dec ~n:obj n:=A(); true};""", 0)>]
    [<DataRow("inh_f", """def cl A def cl B def pred T() {dec ~n:B n:=A(); true};""", 1)>]
    [<DataRow("inh_g", """def cl A def cl B def pred T() {dec ~n:A n:=B(); true};""", 1)>]
    [<DataRow("inh_type_a", """def cl A  def pred T() {dec ~n:ind n:=A(); true};""", 1)>]
    [<DataRow("inh_type_b", """def cl A def pred T() {dec ~n:pred n:=A(); true};""", 1)>]
    [<DataRow("inh_type_c", """def cl A def pred T() {dec ~n:func n:=A(); true};""", 1)>]
    [<DataRow("constr_a", """def cl A def cl B: A def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI2:D dI2:=D(); true};""", 0)>]
    [<DataRow("constr_b", """def cl A def cl B:A def cl D:B def pred T() {dec ~dI2:B dI2:=D(); true};""", 0)>]
    [<DataRow("constr_b1", """def cl A def cl B:A def cl D:B def pred T() {dec ~dI2:B dI2:=D(); true};""", 0)>]
    [<DataRow("constr_c", """def cl B { ctor B(x:obj) {dec ~y:obj x:=y; } } def pred T() {dec ~n,y:obj n:=B(y); true};""", 0)>]
    [<DataRow("constr_d", """def cl A def cl B: A def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI2:obj dI2:=D(); true};""", 0)>]
    [<DataRow("constr_inh_a", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:A n:=B(); true};""", 1)>]
    [<DataRow("constr_inh_b", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:obj n:=B(); true};""", 1)>]
    [<DataRow("constr_inh_c", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:obj n:=A(); true};""", 1)>]
    [<DataRow("constr_inh_d", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:B ~x:obj n:=A(x); true};""", 1)>]
    [<DataRow("constr_inh_e", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B { ctor B(x:pred) {dec base.Obj(); } } def pred T() {dec ~n:B ~x:obj n:=A(x); true};""", 1)>]
    [<DataRow("constr_inh_f", """def cl A { ctor A(x:obj) {dec base.Obj(); } } def cl B { ctor B(x:pred) {dec base.Obj(); } } def pred T() {dec ~n:A ~x:pred n:=B(x); true};""", 1)>]
    [<DataRow("ass_ind_ind", """def cl C1 {ctor C1(i1:ind) {dec ~o:ind o:=i1; }} ;""", 0)>]
    [<DataRow("ass_ind_pred1", """def cl A {dec ~myX:ind; ctor A(x:pred) {dec myX:=x;}};""", 1)>]
    [<DataRow("ass_ind_pred", """def cl C1 {ctor C1(i1:pred) {dec ~o:ind o:=i1; }};""", 1)>]
    [<DataRow("ass_ind_func", """def cl C1 {ctor C1(i1:func) {dec ~o:ind o:=i1; }};""", 1)>]
    [<DataRow("ass_ind_obj", """def cl C1 {ctor C1(i1:obj) {dec ~o:ind o:=i1; }} ;""", 1)>]
    [<DataRow("ass_pred_ind", """def cl C1 {ctor C1(i1:ind) {dec ~o:pred o:=i1; }} ;""", 1)>]
    [<DataRow("ass_pred_pred", """def cl C1 {ctor C1(i1:pred) {dec ~o:pred o:=i1; }};""", 0)>]
    [<DataRow("ass_pred_func", """def cl C1 {ctor C1(i1:func) {dec ~o:pred o:=i1; }};""", 1)>]
    [<DataRow("ass_pred_obj", """def cl C1 {ctor C1(i1:obj) {dec ~o:pred o:=i1; }};""", 1)>]
    [<DataRow("ass_func_ind", """def cl C1 {ctor C1(i1:ind) {dec ~o:func o:=i1; }} ;""", 1)>]
    [<DataRow("ass_func_pred", """def cl C1 {ctor C1(i1:pred) {dec ~o:func o:=i1; }};""", 1)>]
    [<DataRow("ass_func_func", """def cl C1 {ctor C1(i1:func) {dec ~o:func o:=i1; }};""", 0)>]
    [<DataRow("ass_func_obj", """def cl C1 {ctor C1(i1:obj) {dec ~o:func o:=i1; }};""", 1)>]
    [<DataRow("ass_obj_ind", """def cl C1 {ctor C1(i1:ind) {dec ~o:obj o:=i1; }} ;""", 1)>]
    [<DataRow("ass_obj_pred", """def cl C1 {ctor C1(i1:pred) {dec ~o:obj o:=i1; }};""", 1)>]
    [<DataRow("ass_obj_func", """def cl C1 {ctor C1(i1:func) {dec ~o:obj o:=i1; }};""", 1)>]
    [<DataRow("ass_obj_obj", """def cl C1 {ctor C1(i1:obj) {dec ~o:obj o:=i1; }};""", 0)>]
    [<DataRow("24a", "def cl A {dec ~myX:obj; ctor A(x:obj) {dec myX:=x;}} def cl B:A { ctor B(x:obj) {dec base.A(del.Decrement(x)); } } def pred T() { dec ~v:B v:=B(@2); false};", 0)>]    
    [<DataRow("24b", "def cl A {dec ~myX:ind; ctor A(x:obj) {dec myX:=x;}} def cl B:A { ctor B(x:obj) {dec base.A(del.Decrement(x)); } } def pred T() { dec ~v:B v:=B(@2); false};", 1)>]    
    [<DataRow("25", "def cl Nat def func Succ(x:Nat)->Nat def cl A {dec ~myX:Nat; ctor A(i:Nat) {dec myX:=Succ(i);}};", 0)>]    
    [<DataRow("26", "def cl Nat def cl A {dec ~arr:*Nat[Nat]; ctor A(i:Nat) {dec arr[i]:=i;}};", 0)>]    
    [<DataRow("27", "def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def pred T() {dec ~n:Nat n:=Succ(Zero()); true};", 0)>]    
    [<DataRow("28", "def cl Nat def func Succ(n:Nat)->Nat ext Digits x@/\d+/ -> Nat {dec ~n:Nat n:=Succ(delegate.Decrement(x)); return n};", 0)>]      
    [<DataRow("29", "def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def func Add(x,y: Nat)->Nat {dec ~r:Nat r := Succ(self(x,y)); return r };", 0)>]    
    [<DataRow("29a", "def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def func Add(n,m: Nat)->Nat {return Succ(self(n,m))};", 0)>]    
    [<DataRow("30", "def cl Nat def cl Zero:Nat def func Succ(n:Nat)->Nat def pred T() {dec ~r:Nat r:= undef; true };", 0)>]    
    [<DataRow("31", "def cl Nat def cl Tuple {ctor Tuple(l:*tpl[Nat]) {} } def pred T(x,y:Nat) {dec ~tuple:Tuple tuple:=Tuple(x,y); tuple};", 0)>]    
    [<DataRow("32", "def cl Nat def cl C {dec ~myLength: Nat; ctor C(x:Nat) {dec myLength:=x;} property func Length() -> Nat {return myLength} } def pred T() {dec ~l:Nat ~c:C c:=C(l) l:=c.Length(); l};", 0)>]    
    [<DataRow("32", "def pred T(x:ind) {dec ~v:*ind[ind] v[$1]:=x; true};", 0)>]    
    [<DataRow("32a", "def pred T(x:obj) {dec ~v:*ind[ind] v[$1]:=x; true};", 1)>]    
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG05(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG05 ""
            ad.Clear()
            runTestHelper "TestSIG05.fpl" fplCode code expected

    [<DataRow("""def pred Eq(x,y: obj) infix "=" 1000 axiom A {dec ~x:ind ~y:obj; (x = y) };""", 
        "No overload matching `=(ind, obj)`. `x:ind` does not match `x:obj` in a predicate definition TestSIG04MsgSpecificity.Eq(obj, obj).")>]
    [<DataRow("""def func Succ(n:Nat) -> obj {intr};""", 
        "No overload matching `Nat`, no candidates were found. Are you missing a uses clause?")>]
    [<TestMethod>]
    member this.TestSIG04MsgSpecificity(fplCode:string, (expected:string)) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG04 ("", 0, "" )
            prepareFplCode ("TestSIG04MsgSpecificity.fpl", fplCode, false) |> ignore
            checkForUnexpectedErrors code
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<string>(expected, result.Head.Message)

    [<DataRow("00a", "def cl A { intr prty pred T() {intr} } def cl B { intr prty pred T() {intr} } def cl C:A,B ;", 1)>]
    [<DataRow("00b", "def cl A { intr prty pred T() {intr} } def cl B { intr prty pred S() {intr} } def cl C:A,B ;", 0)>]
    [<DataRow("00c", "def cl A { intr prty pred T() {intr} } def cl B { intr prty pred T(x:func) {intr} } def cl C:A,B ;", 0)>]
    [<DataRow("01a", "def cl A { intr prty func T()->obj {intr} } def cl B { intr prty func T()->obj {intr} } def cl C:A,B ;", 1)>]
    [<DataRow("01b", "def cl A { intr prty func T()->obj {intr} } def cl B { intr prty pred T() {intr} } def cl C:A,B ;", 0)>]
    [<DataRow("01c", "def cl A { intr prty func T()->obj {intr} } def cl B { intr prty func T(x:ind)->obj {intr} } def cl C:A,B ;", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG06Classes(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG06 ("","","","")
            runTestHelper "TestSIG06Classes.fpl" fplCode code expected

    [<DataRow("00a", "def func A()->obj { intr prty pred T() {intr} } def func B()->obj { intr prty pred T() {intr} } def func C:A,B()->obj ;", 1)>]
    [<DataRow("00b", "def func A()->obj { intr prty pred T() {intr} } def func B()->obj { intr prty pred S() {intr} } def func C:A,B()->obj ;", 0)>]
    [<DataRow("00c", "def func A()->obj { intr prty pred T() {intr} } def func B()->obj { intr prty pred T(x:func) {intr} } def func C:A,B()->obj ;", 0)>]
    [<DataRow("01a", "def func A()->obj { intr prty func T()->obj {intr} } def func B()->obj { intr prty func T()->obj {intr} } def func C:A,B()->obj ;", 1)>]
    [<DataRow("01b", "def func A()->obj { intr prty func T()->obj {intr} } def func B()->obj { intr prty pred T() {intr} } def func C:A,B()->obj ;", 0)>]
    [<DataRow("01c", "def func A()->obj { intr prty func T()->obj {intr} } def func B()->obj { intr prty func T(x:ind)->obj {intr} } def func C:A,B()->obj ;", 0)>]
    [<DataRow("02a", "def func A()->obj { intr prty pred T() {intr} } def func B:A()->obj { intr prty pred T() {intr} } def func C:A()->obj ;", 1)>]
    [<DataRow("02b", "def func A()->obj { intr prty pred T() {intr} } def func B:A()->obj { intr prty pred T() {intr} } def func C:B()->obj { intr prty pred T() {intr} } ;", 2)>]
    [<DataRow("03a", "def func A()->obj { intr prty func T()->obj {intr} } def func B()->obj { intr prty func T()->obj {intr} } def func C:A,B()->obj { intr prty func T()->obj {intr} } ;", 2)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG06FunctionalTerms(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG06 ("","","","")
            runTestHelper "TestSIG06FunctionalTerms.fpl" fplCode code expected

    [<DataRow("00a", "def pred S(x:pred) {dec T:=true; true};", 1)>]
    [<DataRow("00b", "ax T {true} def pred S(x:pred) {dec T:=true; true};", 1)>]
    [<DataRow("00c", "thm T {true} def pred S(x:pred) {dec T:=true; true};", 1)>]
    [<DataRow("00d", "lem T {true} def pred S(x:pred) {dec T:=true; true};", 1)>]
    [<DataRow("00e", "prop T {true} def pred S(x:pred) {dec T:=true; true};", 1)>]
    [<DataRow("00f", "conj T {true} def pred S(x:pred) {dec T:=true; true};", 1)>]
    [<DataRow("00g", "ext T x@/\d+/ -> obj {ret x} def pred S(x:pred) {dec T:=true; true};", 1)>]
    [<DataRow("00h", "def cl T def pred S(x:pred) {dec T:=true; true};", 1)>]
    [<DataRow("00i", """loc T := !tex: "T"; def pred S(x:pred) {dec T:=true; true};""", 1)>]
    [<DataRow("00k", "inf T {pre:true con:true} def pred S(x:pred) {dec T:=true; true};", 1)>]
    //[<DataRow("00l", "proof T$1 {1. |- trivial} def pred S(x:pred) {dec T$1:=true; true};", 1)>] // syntactically incorrect
    //[<DataRow("00m", "cor T$1 {true} def pred S(x:pred) {dec T$1:=true; true};", 1)>] // syntactically incorrect

    [<DataRow("01a", "def pred T() {intr} def pred S(x:pred) {dec T():=true; true};", 1)>]
    [<DataRow("01b", "def func T()->obj {intr} def pred S(x:pred) {dec T():=true; true};", 1)>]
    [<DataRow("01c", "def pred T(a:ind) {intr} def pred S(x:pred) {dec T(a):=true; true};", 1)>]
    [<DataRow("01d", "def func T(a:ind)->obj {intr} def pred S(x:pred) {dec T(a):=true; true};", 1)>]
    [<DataRow("02a", "def pred S() {dec self:=true; true};", 1)>]
    [<DataRow("02b", "def pred S() {dec parent:=true; true};", 1)>]
    [<DataRow("02c", "def pred S() {dec self():=true; true};", 1)>]
    [<DataRow("02d", "def pred S() {dec parent():=true; true};", 1)>]
    [<DataRow("02e", "def pred S() {dec self(a):=true; true};", 1)>]
    [<DataRow("02f", "def pred S() {dec parent(a):=true; true};", 1)>]
    [<DataRow("03a", "def pred S() {dec A:=true; true};", 1)>]
    [<DataRow("03b", "ax A {true} def pred S() {dec A:=true; true};", 1)>]
    [<DataRow("03c", "def pred S() {dec A():=true; true};", 1)>]
    [<DataRow("03d", "ax A {true} def pred S() {dec A():=true; true};", 1)>]
    [<DataRow("03e", "def pred S() {dec A(a):=true; true};", 1)>]
    [<DataRow("03f", "ax A {true} def pred S() {dec A(a):=true; true};", 1)>]
    [<DataRow("04a", "def pred S() {dec @1:=true; true};", 1)>]
    [<DataRow("04b", "ext D x@/\d+/ -> obj { ret x } def pred S() {dec @1:=true; true};", 1)>]
    [<DataRow("04c", "def pred S() {dec @1():=true; true};", 1)>]
    [<DataRow("04d", "ext D x@/\d+/ -> obj { ret x } def pred S() {dec @1():=true; true};", 1)>]
    [<DataRow("04e", "def pred S() {dec @1(a):=true; true};", 1)>]
    [<DataRow("04f", "ext D x@/\d+/ -> obj { ret x } def pred S() {dec @1(a):=true; true};", 1)>]

    [<DataRow("10a", "def pred S(x:pred) {dec x:=true; true};", 0)>]
    [<DataRow("10b", "def pred S(x:pred) {dec x():=true; true};", 1)>]
    [<DataRow("10c", "def pred S(x:*pred[ind]) {dec x[$1]:=true; true};", 0)>]
    [<DataRow("10d", "def pred S(x:*pred[obj]) {dec x[@1]:=true; true};", 0)>]
    [<DataRow("10e", "def pred S(x:*pred[Nat]) {dec ~a:Nat x[a]:=true; true};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG07(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG07 ("", "", "")
            runTestHelper "TestSIG07.fpl" fplCode code expected


    [<DataRow("00", "def pred T() {dec ~i:ind ~arr:*ind[ind] arr[i]:=undef; true};", 0)>]    
    [<DataRow("00a", "def pred T() {dec ~i:Nat ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    
    [<DataRow("00b", "def pred T() {dec ~i:pred ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    
    [<DataRow("00c", "def pred T() {dec ~i:func ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    
    [<DataRow("00d", "def pred T() {dec ~i:*ind[ind] ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    
    [<DataRow("00e", "def pred T() {dec ~i:*Nat[ind] ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    
    [<DataRow("00f", "def pred T() {dec ~i:*pred[ind] ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    
    [<DataRow("00g", "def pred T() {dec ~i:*func[ind] ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    
    [<DataRow("01", "def pred T() {dec ~i:ind i:=$1 ~arr:*ind[ind] arr[i]:=undef; true};", 0)>]    
    [<DataRow("02", "def pred T(i:ind) {dec ~arr:*ind[ind] arr[i]:=undef; true};", 0)>]    
    [<DataRow("03", "def pred T() {dec ~i:ind i:=$1 ~arr:*ind[ind] arr[i]:=undef; true};", 0)>]    
    [<DataRow("04", "def pred T() {dec ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    
    [<DataRow("05", "def pred T() {dec ~i:obj ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]    // index typed `obj` — invalid
    [<DataRow("06", "def pred T() {dec ~i:ind ~arr:*obj[ind] arr[i]:=undef; true};", 0)>]    // array element type `obj` incompatible with expected `ind`
    [<DataRow("07", "def pred T(i:obj) {dec ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]        // parameter `i` typed `obj` — invalid
    [<DataRow("08", "def pred T() {dec ~i:ind ~arr:*ind[ind] arr[i]:=undef arr[i]:=undef; true};", 0)>] // repeated valid indexing, should succeed
    [<DataRow("08a", "def pred T() {dec ~i:ind ~arr1:*ind[ind] ~arr2:*ind[ind] arr2[arr1[i]]:=undef; true};", 0)>]   // arr indexed by array type *ind[ind], i has that type -> valid
    [<DataRow("08b", "def pred T() {dec ~i:*ind[ind] ~arr:*ind[ind] arr[i]:=undef; true};", 1)>]         // i is array, arr expects ind -> invalid
    [<DataRow("08c", "def pred T() {dec ~i:ind ~arr:*ind[ind] arr[i]:=undef; true};", 0)>]       // arr expects array index, provided ind -> invalid
    [<DataRow("08d", "def pred T() {dec ~i:*ind[ind] ~j:ind ~arr:*ind[ind] arr[i[j]]:=undef; true};", 0)>] // i[j] is ind, arr indexed by ind -> valid
    [<DataRow("08e", "def pred T() {dec ~i:*ind[ind] ~j:ind ~arr:*ind[obj] arr[i[j]]:=undef; true};", 1)>] // i[j] is ind but arr expects obj -> invalid
    [<DataRow("08f", "def pred T() {dec ~irr:*pred[ind] ~krr:*ind[ind] ~arr:*ind[pred,ind] arr[irr[$1],krr[$1]]:=undef; true};", 0)>] // irr[$1] is pred, *krr[$1] is ind, arr accepts that -> valid
    
    // arrays indexed by nested arrays 
    [<DataRow("N01", "def pred T() {dec ~i:*ind[ind] ~j:ind ~arr:*ind[ind] arr[i[j]]:=undef; true};", 0)>]  // i[j] is ind, arr expects ind
    [<DataRow("N02", "def pred T() {dec ~i:*obj[ind] ~j:ind ~arr:*ind[ind] arr[i[j]]:=undef; true};", 1)>]   // i[j] is obj, arr expects ind
    [<DataRow("N03", "def pred T() {dec ~i:*ind[ind] ~j:obj ~arr:*ind[ind] arr[i[j]]:=undef; true};", 1)>]    // j has wrong type to index i
    [<DataRow("N04", "def pred T() {dec ~i:*ind[ind] ~j:ind ~arr:*obj[ind] arr[i[j]]:=undef; true};", 0)>]   // arr element type obj is irrelevant for indexing, valid
    [<DataRow("N06", "def pred T() {dec ~idx:*ind[ind] ~k:ind ~arr:*ind[ind] arr[idx[k]]:=undef; true};", 0)>]   // idx[k] yields ind, valid as arr index
    [<DataRow("N07", "def pred T() {dec ~j:*ind[ind] ~l:ind ~i:*ind[ind] ~arr:*ind[ind] arr[i[j[l]]]:=undef; true};", 0)>] // j[l] -> ind, i[...] -> ind, arr[...] valid
    [<DataRow("N08", "def pred T() {dec ~j:*obj[ind] ~l:ind ~i:*ind[ind] ~arr:*ind[ind] arr[i[j[l]]]:=undef; true};", 1)>] // j[l] -> obj, breaks inner indexing chain

    // functional terms as indexes of arrays
    [<DataRow("F01", "def func T()->obj def pred T() {dec ~arr:*ind[obj] arr[T()]:=undef; true};", 0)>]  // T() is obj, arr accepts it
    [<DataRow("F02", "def func T(x:ind)->obj def pred T() {dec ~x:ind ~arr:*ind[obj] arr[T(x)]:=undef; true};", 0)>]  // T() is obj, arr accepts it
    [<DataRow("F03", "def func T(x:ind)->pred def pred T() {dec ~x:ind ~arr:*ind[obj] arr[T(x)]:=undef; true};", 1)>]  // T() is pred, arr does not accept it
    [<DataRow("F04", "def func T()->ind def pred S() {dec ~arr:*obj[ind] arr[T()]:=undef; true};", 0)>]   // T() -> ind, arr indexed by ind -> valid
    [<DataRow("F05", "def func T()->ind def pred S() {dec ~arr:*obj[obj] arr[T()]:=undef; true};", 1)>]   // T() -> ind, arr expects obj -> invalid
    [<DataRow("F06", "def func T()->func def pred S() {dec ~arr:*obj[func] arr[T()]:=undef; true};", 0)>] // T() -> func, arr indexed by func -> valid
    [<DataRow("F07", "def func T()->pred def pred S() {dec ~arr:*obj[pred] arr[T()]:=undef; true};", 0)>] // T() -> pred, arr indexed by pred -> valid
    [<DataRow("F08", "def func U()->ind def func T(x:ind)->obj def pred S() {dec ~arr:*ind[obj] arr[T(U())]:=undef; true};", 0)>] // nested call T(U()) -> obj, arr expects obj -> valid
    [<DataRow("F09", "def func U()->obj def func T(x:obj)->ind def pred S() {dec ~arr:*obj[obj] arr[T(U())]:=undef; true};", 1)>] // T(U()) -> ind, arr expects obj -> invalid
    [<DataRow("F10", "def func T(x:ind)->obj def pred S(x:ind) {dec ~arr:*ind[obj] arr[T(x)]:=undef; true};", 0)>] // function with param, return obj used as index -> valid
    [<DataRow("F11", "def func T()->obj def pred S() {dec ~arr:*ind[func] arr[T()]:=undef; true};", 1)>] // T() -> obj, arr expects func -> invalid
    [<DataRow("F12", "def func T()->ind def pred S() {dec ~arr:*ind[ind] arr[T()]:=undef; true};", 0)>] // simple match, T() -> ind and arr indexed by ind -> valid

    // classes as indexes of arrays
    [<DataRow("C01", "def cl A def pred T() {dec ~i:A ~arr:*ind[obj] arr[i]:=undef; true};", 0)>]  // i is A, obj accepts any class, arr expects an obj index -> valid
    [<DataRow("C02", "def cl A def cl B def pred T() {dec ~i:B ~arr:*ind[A] arr[i]:=undef; true};", 1)>]  // i is B, B is not derived from A, arr expects an A index -> invalid
    [<DataRow("C03", "def cl A def cl B:A def pred T() {dec ~i:B ~arr:*ind[A] arr[i]:=undef; true};", 0)>]  // i is B, B is derived from A, arr expects an A index -> valid
    [<DataRow("C04", "def cl A def pred T() {dec ~i:A ~arr:*ind[A] arr[i]:=undef; true};", 0)>]   // i:A, arr indexed by A -> valid
    [<DataRow("C05", "def cl A def pred T() {dec ~i:obj ~arr:*ind[A] arr[i]:=undef; true};", 1)>] // i:obj is supertype, arr expects A, cannot index arr[obj] -> invalid
    [<DataRow("C06", "def cl A def cl D:A def pred T() {dec ~i:D ~arr:*ind[A] arr[i]:=undef; true};", 0)>] // D derives A -> valid
    [<DataRow("C07", "def cl A def cl B def cl C def pred T() {dec ~i:C ~arr:*ind[A] arr[i]:=undef; true};", 1)>] // C unrelated to A -> invalid
    [<DataRow("C08", "def cl X def cl Y def cl M:X,Y def pred T() {dec ~i:M ~arr:*ind[X] arr[i]:=undef; true};", 0)>] // M derives X and Y -> valid for X-index
    [<DataRow("C09", "def cl Base def cl Mid:Base def cl Leaf:Mid def pred T() {dec ~i:Leaf ~arr:*ind[Base] arr[i]:=undef; true};", 0)>] // Leaf->Mid->Base chain -> valid
    [<DataRow("C10", "def cl A def cl B def cl M:A,B def pred T() {dec ~i:B ~arr:*ind[M] arr[i]:=undef; true};", 1)>] // B is a base of M? here M derives from A,B so B is base but arr expects M -> invalid (B is not M)
    [<DataRow("C11", "def cl A def cl B:A def pred T() {dec ~i:obj ~arr:*ind[obj] arr[i]:=undef; true};", 0)>] // i:obj and arr indexed by obj -> valid

    // mixed - functional terms yielding class types used as indexes
    [<DataRow("M01", "def cl A def func F()->A def pred T() {dec ~arr:*ind[obj] arr[F()]:=undef; true};", 0)>]  // F() is A, obj accepts any class, arr expects an obj index -> valid
    [<DataRow("M02", "def cl A def func F()->A def pred T() {dec ~arr:*ind[A] arr[F()]:=undef; true};", 0)>]   // F() -> A, arr expects A -> valid
    [<DataRow("M03", "def cl A def cl B:A def func F()->B def pred T() {dec ~arr:*ind[A] arr[F()]:=undef; true};", 0)>] // F() -> B (inherits A), arr expects A -> valid
    [<DataRow("M04", "def cl A def func F()->A def pred T() {dec ~arr:*ind[B] arr[F()]:=undef; true};", 1)>]   // F() -> A, arr expects B (unrelated) -> invalid
    [<DataRow("M05", "def cl A def func F()->obj def pred T() {dec ~arr:*ind[A] arr[F()]:=undef; true};", 1)>] // F() -> obj, arr expects A -> invalid (obj is supertype)
    [<DataRow("M06", "def cl Base def cl Derived:Base def func F()->Derived def pred T() {dec ~arr:*ind[Base] arr[F()]:=undef; true};", 0)>] // Derived->Base chain -> valid
    [<DataRow("M07", "def cl X def cl Y def cl M:X,Y def func F()->M def pred T() {dec ~arr:*ind[X] arr[F()]:=undef; true};", 0)>] // M derives X,Y -> valid for X-index
    [<DataRow("M08", "def cl B def cl A def func U()->B def func F(x:B)->A def pred T() {dec ~arr:*ind[A] arr[F(U())]:=undef; true};", 0)>] // nested call F(U()) -> A -> valid
    [<DataRow("M09", "def cl A def func F()->A def pred T() {dec ~arr:*ind[obj] arr[F()]:=undef; true};", 0)>] // arr indexed by obj accepts class A -> valid
    [<DataRow("M10", "def cl A def cl B:A def func F()->A def pred T() {dec ~arr:*ind[B] arr[F()]:=undef; true};", 1)>] // F() -> A, arr expects B (derived) -> invalid

    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG08(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG08 ("", "", "", "", 0)
            runTestHelper "TestSIG08.fpl" fplCode code expected

    [<DataRow("01", "def pred T() {dec ~i:ind ~arr:*obj[ind,ind] arr[i]:=undef; true};", 1)>] // array two-dimensional, index one-dimensional -> invalid
    [<DataRow("02", "def pred T() {dec ~i,j:obj ~arr:*Base[obj,obj] arr[i,j]:=undef; true};", 0)>] // array two-dimensional, index two-dimensional -> valid
    [<DataRow("03", "def pred T() {dec ~i:ind ~arr:*obj[ind,ind,ind] arr[i]:=undef; true};", 2)>]   // 3D array, single index -> 2x SIG09
    [<DataRow("04", "def pred T() {dec ~i,j:ind ~arr:*obj[ind,ind,ind] arr[i,j]:=undef; true};", 1)>] // 3D array, two indices -> SIG09
    [<DataRow("05", "def pred T() {dec ~i,j,k:ind ~arr:*obj[ind,ind,ind] arr[i,j,k]:=undef; true};", 0)>] // 3D array, three indices -> valid
    [<DataRow("06", "def func X()->ind def pred T() {dec ~arr:*obj[ind,ind] arr[X()]:=undef; true};", 1)>] // 2D array, one index via functional term -> SIG09
    [<DataRow("06a", "def func X()->ind def pred T() {dec ~arr:*obj[ind,ind] arr[X(),X()]:=undef; true};", 0)>] // 2D array, two indexes via functional term -> valid
    [<DataRow("07", "def func A()->ind def func B()->ind def pred T() {dec ~arr:*obj[ind,ind,ind] arr[A(),B()]:=undef; true};", 1)>] // 3D array, two functional indices -> SIG09
    [<DataRow("07a", "def func A()->ind def func B()->ind def pred T() {dec ~arr:*obj[ind,ind,ind] arr[A(),B(),$1]:=undef; true};", 0)>] // 3D array, indices -> valid
    [<DataRow("08", "def pred T() {dec ~arr:*obj[ind,ind] arr[$1,$2]:=undef; true};", 0)>] // 2D array, two indices -> valid
    // nested indices
    [<DataRow("N01", "def pred T() {dec ~i:*ind[ind,ind] ~j:ind ~arr:*ind[ind] arr[i[j]]:=undef; true};", 1)>]    // i is 2D, i[j] is 1D -> SIG09
    [<DataRow("N02", "def pred T() {dec ~i:*ind[obj,ind,ind] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,j]]:=undef; true};", 1)>]    // i is 3D, i[k,j] is 2D -> SIG09
    [<DataRow("N03", "def pred T() {dec ~i:*ind[obj,ind,obj] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,j,k]]:=undef; true};", 0)>]    // i is 3D, i[k,j,k] is 3D -> ok
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG09(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG09 ("", "", 0)
            runTestHelper "TestSIG09.fpl" fplCode code expected

    [<DataRow("01", "def pred T() {dec ~i,j:ind ~arr:*obj[ind] arr[i,j]:=undef; true};", 1)>]   // 1D array, 2 indices -> 1 extra (SIG10)
    [<DataRow("01b", "def pred T() {dec ~i:ind ~arr:*obj[ind] arr[i]:=undef; true};", 0)>]   // 1D array, 1 indices -> valid
    [<DataRow("02", "def pred T() {dec ~i,j,k:ind ~arr:*obj[ind] arr[i,j,k]:=undef; true};", 2)>] // 1D array, 3 indices -> 2 extra (SIG10 x2)
    [<DataRow("02a", "def pred T() {dec ~i,j,k:ind ~arr:*obj[ind,ind,ind] arr[i,j,k]:=undef; true};", 0)>] // 3D array, 3 indices -> valid
    [<DataRow("02b", "def pred T() {dec ~i,j,k:ind ~arr:*obj[ind,ind,ind] arr[i,j]:=undef; true};", 0)>] // 3D array, 2 indices -> not relevant for SIG10
    [<DataRow("03", "def pred T() {dec ~i,j,k:ind ~arr:*obj[ind,ind] arr[i,j,k]:=undef; true};", 1)>] // 2D array, 3 indices -> 1 extra
    [<DataRow("04", "def pred T() {dec ~i,j,k,l:ind ~arr:*obj[ind,ind,ind] arr[i,j,k,l]:=undef; true};", 1)>] // 3D array, 4 indices -> 1 extra
    [<DataRow("04a", "def pred T() {dec ~i,j,k,l:ind ~arr:*obj[ind,ind,ind,ind] arr[i,j,k,l]:=undef; true};", 0)>] // 4D array, 4 indices -> valid
    [<DataRow("05", "def func A()->ind def func B()->ind def pred T() {dec ~arr:*obj[ind] arr[A(),B()]:=undef; true};", 1)>] // functional indices: 2 supplied for 1D array -> 1 extra
    [<DataRow("06", "def func A()->ind def func B()->ind def func C()->ind def pred T() {dec ~arr:*obj[ind,ind] arr[A(),B(),C()]:=undef; true};", 1)>] // 3 functional indices for 2D array -> 1 extra
    [<DataRow("07", "def pred T() {dec ~arr:*obj[ind,ind] arr[$1,$2,$3]:=undef; true};", 1)>] // explicit $-indices: 3 provided for 2D array -> 1 extra
    // nested indices
    [<DataRow("N01", "def pred T() {dec ~i:*ind[ind] ~j:ind ~arr:*ind[ind] arr[i[j]]:=undef; true};", 0)>]    // i is 1D, i[j] is 1D -> ok
    [<DataRow("N02", "def pred T() {dec ~i:*ind[obj,ind] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,k,j,j], i[k,j]]:=undef; true};", 3)>]    // i is 2D, i[k,k,j,j] is 4D -> 2x SIG10; arr is 1D, arr[i[k,k,j,j], i[k,j]] is 2D -> 1x SIG10 
    [<DataRow("N02a", "def pred T() {dec ~i:*ind[obj,ind] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,k,j], i[k,j]]:=undef; true};", 2)>]    // i is 2D, i[k,k,j] is 3D -> 1x SIG10; arr is 1D, arr[i[k,k,j], i[k,j]] is 2D -> 2x SIG10 
    [<DataRow("N02b", "def pred T() {dec ~i:*ind[obj,ind] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,k], i[k,j]]:=undef; true};", 1)>]    // i is 2D, i[k,k] is 2D -> valid; arr is 1D, arr[i[k,k], i[k,j]] is 2D -> 1x SIG10 
    [<DataRow("N02c", "def pred T() {dec ~i:*ind[obj,ind] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,k]]:=undef; true};", 0)>]    // i is 2D, i[k,k] is 2D -> valid, arr is 1D; arr[i[k,k]] is 1D -> valid
    [<DataRow("N03", "def pred T() {dec ~i:*ind[obj,obj] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,j,k,k,j]]:=undef; true};", 3)>]    // i is 2D, i[k,j,k,k,j] is 5D -> 3x SIG10; arr is 1D arr[i[k,j,k,k,j]]] is 1D -> valid
    [<DataRow("N03a", "def pred T() {dec ~i:*ind[obj,obj] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,j,k,k]]:=undef; true};", 2)>]    // i is 2D, i[k,j,k,k] is 4D -> 2x SIG10; arr is 1D arr[i[k,j,k,k]] is 1D -> valid
    [<DataRow("N03b", "def pred T() {dec ~i:*ind[obj,obj] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,j,k]]:=undef; true};", 1)>]    // i is 2D, i[k,j,k] is 3D -> 1x SIG10; arr is 1D arr[i[k,j,k]] is 1D -> valid
    [<DataRow("N03c", "def pred T() {dec ~i:*ind[obj,obj] ~k:obj ~j:ind ~arr:*ind[ind] arr[i[k,j]]:=undef; true};", 0)>]    // i is 2D, i[k,j] is 2D -> valid; arr is 1D arr[i[k,j]] is 1D -> valid
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG10(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG10 ("", "", 0)
            runTestHelper "TestSIG10.fpl" fplCode code expected

    // mapping
    [<DataRow("01", """def func T()->A;""", 0)>] // undefined mapping -> no SIG11
    [<DataRow("01a", """def func T()->*A[ind];""", 0)>] // undefined mapping -> no SIG11
    [<DataRow("02", """def cl A def func T()->A;""", 0)>] // mapping is defined class -> no SIG11
    [<DataRow("03", """def cl A def func T()->*A[ind];""", 0)>] // mapping is defined class -> no SIG11
    [<DataRow("04", """ext A x@/\d+/->obj {ret x} def func T()->A;""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("05", """def pred A() {intr} def func T()->A;""", 1)>] // mapping is a defined predicate -> issue SIG11
    [<DataRow("06", """def func A()->obj {intr} def func T()->A;""", 1)>] // mapping is a defined predicate -> issue SIG11
    [<DataRow("07", """inf A {pre:true con:true} def func T()->A;""", 1)>] // mapping is a defined predicate -> issue SIG11
    [<DataRow("07", """ax A {true} def func T()->A;""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("08", """thm A {true} def func T()->A;""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("09", """prop A {true} def func T()->A;""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("10", """lem A {true} def func T()->A;""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("11", """conj A {true} def func T()->A;""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG11(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG11 ("", "")
            ad.Clear()
            runTestHelper "TestSIG11.fpl" fplCode code expected


    [<DataRow("00a", "def cl A {intr} ;", 1)>]
    [<DataRow("00b", "def cl A:B {intr} ;", 1)>]
    [<DataRow("00c", "def cl A:B {intr property pred T() {true} } ;", 0)>]
    [<DataRow("00d", "def cl A:B {ctor A(){dec base.B();} } ;", 0)>]
    [<DataRow("00e", "def cl A {ctor A(){} } ;", 0)>]
    [<DataRow("00f", "def cl A;", 0)>]
    [<DataRow("01a", "def func A()->obj {intr} ;", 1)>]
    [<DataRow("01b", "def func A:B()->obj {intr} ;", 1)>]
    [<DataRow("01c", "def func A(x:obj)->obj {intr} ;", 1)>]
    [<DataRow("01d", "def func A:B(x:obj)->obj {intr} ;", 1)>]
    [<DataRow("01c", "def func A:B()->obj {intr property pred T() {true} } ;", 0)>]
    [<DataRow("01d", "def func A:B()->obj {dec ~x:obj; return x};", 0)>]
    [<DataRow("01e", "def func A()->obj;", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestST001(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST001 ""
            runTestHelper "TestST001.fpl" fplCode code expected

    [<DataRow("00a", "def cl A {ctor A(x:obj){} } ;", 1)>]
    [<DataRow("00b", "def cl A:B {ctor A(){dec base.B();} } ;", 0)>]
    [<DataRow("00c", "def cl A {ctor A(){} } ;", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestST002(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST002 ""
            runTestHelper "TestST002.fpl" fplCode code expected

    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestST003(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST003 ""
            runTestHelper "TestST003.fpl" fplCode code expected

    [<DataRow("def predicate Test(x,y:* pred[ind]) {true};", 1)>]
    [<DataRow("def predicate Test(x,y:* pred[obj]) {true};", 1)>]
    [<DataRow("def predicate Test(x,y: pred) {true};", 0)>]
    [<DataRow("def predicate Test(x:* pred[ind]) {true};", 0)>]
    [<DataRow("def predicate Test(x:* pred[obj]) {true};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR00(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR00
            runTestHelper "TestVAR00.fpl" fplCode code expected


    [<DataRow("00", "def pred Test() {x};", 1)>]
    [<DataRow("01", "inf ExistsByExample {dec ~p: pred(c: obj); pre: p(c) con: ex x:obj {p(x)}};", 0)>]
    [<DataRow("02", "axiom A { all x:Nat {true} };", 0)>]
    [<DataRow("03", "axiom A { all x:obj {y} };", 1)>]
    [<DataRow("04", "axiom A { dec ~x:obj; true };", 0)>]
    [<DataRow("05", "axiom A { dec ~x:obj; true };", 0)>]
    [<DataRow("06", """loc and(p,q) := !tex: p "\wedge" q;;""", 0)>]
    [<DataRow("07", """loc and(p,q) := !tex: x "\wedge" q;;""", 1)>]
    [<DataRow("08", """loc and(p,q) := !tex: x "\wedge" y;;""", 2)>]
    [<DataRow("09", """def pred Add(x,y: obj) infix "+" 2 {intr} loc (x + y) := !tex: x "+" y !eng: x "plus" y !ger: x "plus" y;;""", 0)>]
    [<DataRow("10", """def pred Add(x,y: obj) infix "+" 2 {intr} axiom A {(x + y * z = 1)};""", 3)>]
    [<DataRow("11", "axiom A {dec ~arr: tpl; x };", 1)>]
    [<DataRow("12", "prop A {dec ~d:pred; true} proof A$1 {1. |- d qed};", 0)>]
    [<DataRow("13", "prop A {dec ~d:pred; true} cor A$1 { d };", 0)>]
    [<DataRow("14", "def class A {ctor A(x: obj, p:pred(u: pred)) {dec assert u;  }};", 0)>]
    [<DataRow("15", "ext D x@/\d+/ -> pred { ret (x = @1) };", 0)>]
    [<DataRow("16", "ext D x@/\d+/ -> pred { dec ~y:obj; ret (x = y) };", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR01(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR01 ""
            runTestHelper "TestVAR01.fpl" fplCode code expected

    [<DataRow("00", "axiom T {all n:pred { n } };", 0)>]
    [<DataRow("00a", "axiom T {all n, n:pred { n } };", 1)>]
    [<DataRow("00b", "axiom T {all n:pred, n:pred { n } };", 1)>]
    [<DataRow("01", "axiom T {ex n:pred { n } };", 0)>]
    [<DataRow("01a", "axiom T {ex n, n:pred { n } };", 1)>]
    [<DataRow("01b", "axiom T {ex n:pred, n:pred { n } };", 1)>]
    [<DataRow("02", "axiom T {exn$1 n:pred { n } };", 0)>]
    [<DataRow("02a", "axiom T {exn$1 n, n:pred { n } };", 1)>]
    [<DataRow("02a", "axiom T {exn$1 n:pred, n:pred { n } };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR02(no: string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR02 ""
            runTestHelper "TestVAR02.fpl" fplCode code expected

    // simple blocks
    [<DataRow("00", "def pred Test(x,x:* pred[ind]) {true};", 1)>]
    [<DataRow("00a", "def pred Test(x,x:* pred[obj]) {true};", 1)>]
    [<DataRow("00b", "def pred Test(x,x: pred) {true};", 1)>]
    [<DataRow("00c", "def pred Test(x: pred) {true};", 0)>]
    [<DataRow("00d", "def pred Test(x:* pred[ind]) {dec ~x:obj; true};", 1)>]
    [<DataRow("00_1", "def func Test(x,x:* pred[ind])->obj {intr};", 1)>]
    [<DataRow("00a_1", "def func Test(x,x:* pred[obj])->obj {intr};", 1)>]
    [<DataRow("00b_1", "def func Test(x,x: pred)->obj {intr};", 1)>]
    [<DataRow("00c_1", "def func Test(x: pred)->obj {intr};", 0)>]
    [<DataRow("00d_1", "def func Test(x:* pred[ind])->obj {dec ~x:obj; return x};", 1)>]
    [<DataRow("00_2", "def cl Test {dec ~x,x:* pred[ind]; ctor Test(){}};", 1)>]
    [<DataRow("00a_2", "def cl Test {dec ~x,x:* pred[obj]; ctor Test(){}};", 1)>]
    [<DataRow("00b_2", "def cl Test {dec ~x,x: pred; ctor Test(){}};", 1)>]
    [<DataRow("00c_2", "def cl Test {dec ~x: pred; ctor Test(){}};", 0)>]
    [<DataRow("00_3", "ax Test {dec ~x,x:* pred[ind]; true};", 1)>]
    [<DataRow("00a_3", "ax Test{dec ~x,x:* pred[obj]; true};", 1)>]
    [<DataRow("00b_3", "ax Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_3", "ax Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_4", "thm Test {dec ~x,x:* pred[ind]; true};", 1)>]
    [<DataRow("00a_4", "thm Test{dec ~x,x:* pred[obj]; true};", 1)>]
    [<DataRow("00b_4", "thm Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_4", "thm Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_5", "lem Test {dec ~x,x:* pred[ind]; true};", 1)>]
    [<DataRow("00a_5", "lem Test{dec ~x,x:* pred[obj]; true};", 1)>]
    [<DataRow("00b_5", "lem Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_5", "lem Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_6", "prop Test {dec ~x,x:* pred[ind]; true};", 1)>]
    [<DataRow("00a_6", "prop Test{dec ~x,x:* pred[obj]; true};", 1)>]
    [<DataRow("00b_6", "prop Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_6", "prop Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_7", "conj Test {dec ~x,x:* pred[ind]; true};", 1)>]
    [<DataRow("00a_7", "conj Test{dec ~x,x:* pred[obj]; true};", 1)>]
    [<DataRow("00b_7", "conj Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_7", "conj Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_8", "inf Test {dec ~x,x:* pred[ind]; pre: true con:true};", 1)>]
    [<DataRow("00a_8", "inf Test {dec ~x,x:* pred[obj]; pre: true con:true};", 1)>]
    [<DataRow("00b_8", "inf Test {dec ~x,x: pred; pre: true con:true};", 1)>]
    [<DataRow("00c_8", "inf Test {dec ~x: pred; pre: true con:true};", 0)>]

    // properties
    [<DataRow("01a_0", "def pred Test() {true prty pred X(x,x:* pred[ind]) {true} };", 1)>]
    [<DataRow("01b_0", "def pred Test() {true prty pred X(x,x:* pred[obj]) {true} };", 1)>]
    [<DataRow("01c_0", "def pred Test() {true prty pred X(x,x: pred) {true} };", 1)>]
    [<DataRow("01d_0", "def pred Test() {true prty pred X(x: pred) {true} };", 0)>]
    [<DataRow("01e_0", "def pred Test(x:ind) {true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("01f_0", "def pred Test() {dec ~x:ind; true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("01a_1", "def pred Test() {true prty func X(x,x:* pred[ind])->obj {intr} };", 1)>]
    [<DataRow("01b_1", "def pred Test() {true prty func X(x,x:* pred[obj])->obj {intr} };", 1)>]
    [<DataRow("01c_1", "def pred Test() {true prty func X(x,x: pred)->obj {intr} };", 1)>]
    [<DataRow("01d_1", "def pred Test() {true prty func X(x: pred)->obj {intr} };", 0)>]
    [<DataRow("01e_1", "def pred Test(x:ind) {true prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("01f_1", "def pred Test() {dec ~x:ind; true prty pred X(x: pred) {intr} };", 1)>]
    [<DataRow("01a_2", "def pred Test() {true prty pred X(x,x:* pred[ind]) {true} };", 1)>]
    [<DataRow("01b_2", "def pred Test() {true prty pred X(x,x:* pred[obj]) {true} };", 1)>]
    [<DataRow("01c_2", "def pred Test() {true prty pred X(x,x: pred) {true} };", 1)>]
    [<DataRow("01d_2", "def pred Test() {true prty pred X(x: pred) {true} };", 0)>]
    [<DataRow("01e_2", "def pred Test(x:ind) {true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("01f_2", "def pred Test() {dec ~x:ind; true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("01a_3", "def pred Test() {true prty func X(x,x:* pred[ind])->obj {intr} };", 1)>]
    [<DataRow("01b_3", "def pred Test() {true prty func X(x,x:* pred[obj])->obj {intr} };", 1)>]
    [<DataRow("01c_3", "def pred Test() {true prty func X(x,x: pred)->obj {intr} };", 1)>]
    [<DataRow("01d_3", "def pred Test() {true prty func X(x: pred)->obj {intr} };", 0)>]
    [<DataRow("01e_3", "def pred Test(x:ind) {true prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("01f_3", "def pred Test() {dec ~x:ind; true prty pred X(x: pred) {intr} };", 1)>]

    // proofs or corollaries
    [<DataRow("02a_0", "theorem TestId {dec ~x:ind; true}       proof       TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("02b_0", "theorem TestId {dec ~x: ind; true}      proof       TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("02c_0", "theorem TestId {dec ~x:ind; true}       proof       TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("02d_0", "theorem TestId {dec ~x: ind; true}      corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02e_0", "theorem TestId {dec ~x:ind; true}       corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02f_0", "theorem TestId {dec ~x: ind; true}      corollary   TestId$1 { true };", 0)>]
    [<DataRow("02g_0", "theorem TestId {dec ~x:ind; true}       corollary   TestId$1 { true };", 0)>]
    [<DataRow("02h_0", "theorem TestId {dec ~x: ind; true}      corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02i_0", "theorem TestId {dec ~x: ind; true}      corollary   TestId$1 { true }   proof       TestId$1$1 { dec ~x:obj; 1. |- trivial } ;", 1)>]
    [<DataRow("02j_0", "theorem TestId {dec ~x: ind; true}      corollary   TestId$1 { true }   corollary   TestId$1$1 { dec ~x:obj; true } ;", 1)>]
    [<DataRow("02k_0", "theorem TestId {dec ~x: ind; true}      corollary   TestId$1 { true }   corollary   TestId$1$1 {dec ~x:obj; true } ;", 1)>]
    [<DataRow("02l_0", "theorem TestId {dec ~x:ind; true}       corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02m_0", "theorem TestId {dec ~x: ind; true}      corollary   TestId$1 { true };", 0)>]
    [<DataRow("02n_0", "theorem TestId {dec ~x:ind; true}       corollary   TestId$1 { true };", 0)>]
    [<DataRow("02o_0", "theorem TestId {dec ~x: ind; true}      proof       TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("02a_1", "lemma TestId {dec ~x:ind; true}         proof       TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("02b_1", "lemma TestId {dec ~x: ind; true}        proof       TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("02c_1", "lemma TestId {dec ~x:ind; true}         proof       TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("02d_1", "lemma TestId {dec ~x: ind; true}        corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02e_1", "lemma TestId {dec ~x:ind; true}         corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02f_1", "lemma TestId {dec ~x: ind; true}        corollary   TestId$1 { true };", 0)>]
    [<DataRow("02g_1", "lemma TestId {dec ~x:ind; true}         corollary   TestId$1 { true };", 0)>]
    [<DataRow("02h_1", "lemma TestId {dec ~x: ind; true}        corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02i_1", "lemma TestId {dec ~x: ind; true}        corollary   TestId$1 { true }   proof       TestId$1$1 { dec ~x:obj; 1. |- trivial } ;", 1)>]
    [<DataRow("02j_1", "lemma TestId {dec ~x: ind; true}        corollary   TestId$1 { true }   corollary   TestId$1$1 { dec ~x:obj; true } ;", 1)>]
    [<DataRow("02k_1", "lemma TestId {dec ~x: ind; true}        corollary   TestId$1 { true }   corollary   TestId$1$1 {dec ~x:obj; true } ;", 1)>]
    [<DataRow("02l_1", "lemma TestId {dec ~x:ind; true}         corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02m_1", "lemma TestId {dec ~x: ind; true}        corollary   TestId$1 { true };", 0)>]
    [<DataRow("02n_1", "lemma TestId {dec ~x:ind; true}         corollary   TestId$1 { true };", 0)>]
    [<DataRow("02o_1", "lemma TestId {dec ~x: ind; true}        proof       TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("02a_2", "proposition TestId {dec ~x:ind; true}   proof       TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("02b_2", "proposition TestId {dec ~x: ind; true}  proof       TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("02c_2", "proposition TestId {dec ~x:ind; true}   proof       TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("02d_2", "proposition TestId {dec ~x: ind; true}  corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02e_2", "proposition TestId {dec ~x:ind; true}   corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02f_2", "proposition TestId {dec ~x: ind; true}  corollary   TestId$1 { true };", 0)>]
    [<DataRow("02g_2", "proposition TestId {dec ~x:ind; true}   corollary   TestId$1 { true };", 0)>]
    [<DataRow("02h_2", "proposition TestId {dec ~x: ind; true}  corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02i_2", "proposition TestId {dec ~x: ind; true}  corollary   TestId$1 { true }   proof       TestId$1$1 { dec ~x:obj; 1. |- trivial } ;", 1)>]
    [<DataRow("02j_2", "proposition TestId {dec ~x: ind; true}  corollary   TestId$1 { true }   corollary   TestId$1$1 { dec ~x:obj; true } ;", 1)>]
    [<DataRow("02k_2", "proposition TestId {dec ~x: ind; true}  corollary   TestId$1 { true }   corollary   TestId$1$1 {dec ~x:obj; true } ;", 1)>]
    [<DataRow("02l_2", "proposition TestId {dec ~x:ind; true}   corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02m_2", "proposition TestId {dec ~x: ind; true}  corollary   TestId$1 { true };", 0)>]
    [<DataRow("02n_2", "proposition TestId {dec ~x:ind; true}   corollary   TestId$1 { true };", 0)>]
    [<DataRow("02o_2", "proposition TestId {dec ~x: ind; true}  proof       TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("02a_3", "corollary TestId$1 {dec ~x:ind; true}   proof       TestId$1$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("02b_3", "corollary TestId$1 {dec ~x: ind; true}  proof       TestId$1$1 { 1. |- trivial };", 0)>]
    [<DataRow("02c_3", "corollary TestId$1 {dec ~x:ind; true}   proof       TestId$1$1 { 1. |- trivial };", 0)>]
    [<DataRow("02d_3", "corollary TestId$1 {dec ~x: ind; true}  corollary   TestId$1$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02e_3", "corollary TestId$1 {dec ~x:ind; true}   corollary   TestId$1$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02f_3", "corollary TestId$1 {dec ~x: ind; true}  corollary   TestId$1$1 { true };", 0)>]
    [<DataRow("02g_3", "corollary TestId$1 {dec ~x:ind; true}   corollary   TestId$1$1 { true };", 0)>]
    [<DataRow("02h_3", "corollary TestId$1 {dec ~x: ind; true}  corollary   TestId$1$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02i_3", "corollary TestId$1 {dec ~x: ind; true}  corollary   TestId$1$1 { true } proof       TestId$1$1$1 { dec ~x:obj; 1. |- trivial } ;", 1)>]
    [<DataRow("02j_3", "corollary TestId$1 {dec ~x: ind; true}  corollary   TestId$1$1 { true } corollary   TestId$1$1$1 { dec ~x:obj; true } ;", 1)>]
    [<DataRow("02k_3", "corollary TestId$1 {dec ~x: ind; true}  corollary   TestId$1$1 { true } corollary   TestId$1$1$1 {dec ~x:obj; true } ;", 1)>]
    [<DataRow("02l_3", "corollary TestId$1 {dec ~x:ind; true}   corollary   TestId$1$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02m_3", "corollary TestId$1 {dec ~x: ind; true}  corollary   TestId$1$1 { true };", 0)>]
    [<DataRow("02n_3", "corollary TestId$1 {dec ~x:ind; true}   corollary   TestId$1$1 { true };", 0)>]
    [<DataRow("02o_3", "corollary TestId$1 {dec ~x: ind; true}  proof       TestId$1$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    
    [<DataRow("02d_4", "conjecture TestId  {dec ~x: ind; true}  corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02e_4", "conjecture TestId  {dec ~x:ind; true}   corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02f_4", "conjecture TestId  {dec ~x: ind; true}  corollary   TestId$1 { true };", 0)>]
    [<DataRow("02g_4", "conjecture TestId  {dec ~x:ind; true}   corollary   TestId$1 { true };", 0)>]
    [<DataRow("02h_4", "conjecture TestId  {dec ~x: ind; true}  corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02i_4", "conjecture TestId  {dec ~x: ind; true}  corollary   TestId$1 { true } proof       TestId$1$1 { dec ~x:obj; 1. |- trivial } ;", 1)>]
    [<DataRow("02j_4", "conjecture TestId  {dec ~x: ind; true}  corollary   TestId$1 { true } corollary   TestId$1$1 { dec ~x:obj; true } ;", 1)>]
    [<DataRow("02k_4", "conjecture TestId  {dec ~x: ind; true}  corollary   TestId$1 { true } corollary   TestId$1$1 {dec ~x:obj; true } ;", 1)>]
    [<DataRow("02l_4", "conjecture TestId  {dec ~x:ind; true}   corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02m_4", "conjecture TestId  {dec ~x: ind; true}  corollary   TestId$1 { true };", 0)>]
    [<DataRow("02n_4", "conjecture TestId  {dec ~x:ind; true}   corollary   TestId$1 { true };", 0)>]

    [<DataRow("02d_5", "axiom      TestId  {dec ~x: ind; true}  corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02e_5", "axiom      TestId  {dec ~x:ind; true}   corollary   TestId$1 { dec ~x:obj; true };", 1)>]
    [<DataRow("02f_5", "axiom      TestId  {dec ~x: ind; true}  corollary   TestId$1 { true };", 0)>]
    [<DataRow("02g_5", "axiom      TestId  {dec ~x:ind; true}   corollary   TestId$1 { true };", 0)>]
    [<DataRow("02h_5", "axiom      TestId  {dec ~x: ind; true}  corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02i_5", "axiom      TestId  {dec ~x: ind; true}  corollary   TestId$1 { true } proof       TestId$1$1 { dec ~x:obj; 1. |- trivial } ;", 1)>]
    [<DataRow("02j_5", "axiom      TestId  {dec ~x: ind; true}  corollary   TestId$1 { true } corollary   TestId$1$1 { dec ~x:obj; true } ;", 1)>]
    [<DataRow("02k_5", "axiom      TestId  {dec ~x: ind; true}  corollary   TestId$1 { true } corollary   TestId$1$1 {dec ~x:obj; true } ;", 1)>]
    [<DataRow("02l_5", "axiom      TestId  {dec ~x:ind; true}   corollary   TestId$1 {dec ~x:obj; true };", 1)>]
    [<DataRow("02m_5", "axiom      TestId  {dec ~x: ind; true}  corollary   TestId$1 { true };", 0)>]
    [<DataRow("02n_5", "axiom      TestId  {dec ~x:ind; true}   corollary   TestId$1 { true };", 0)>]

    [<DataRow("03a", "ext Digits x@/\d+/ -> Nat {dec ~n:Nat; return n};", 0)>]
    [<DataRow("03b", "ext Digits x@/\d+/ -> Nat {dec ~x:Nat; return x};", 1)>]
    [<DataRow("03c", "ext Digits x@/\d+/ -> Nat {dec ~x:*obj[ind]; return x};", 1)>]
    [<DataRow("03d", "ext Digits x@/\d+/ -> Nat {dec ~x:*ind[ind]; return x};", 1)>]

    [<DataRow("04", "inf ModusPonens {dec ~p,q: pred; pre: and (p, impl (p,q) ) con: q};", 0)>]
    [<DataRow("05", "def pred Test() {true prty pred X(y:* pred[ind]) {dec ~x:obj; ex x:obj {true}} };", 1)>]
    [<DataRow("06", "def pred Test() {true prty pred X(y:* pred[ind]) {ex x:obj {true}} };", 0)>]
    [<DataRow("07", "def pred Test() {true prty func X(y:* pred[ind])->obj {dec ~x:obj; return x} };", 0)>]
    [<DataRow("09", "def pred Test(x: ind) {true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("09a", "def pred Test(x: ind) {true prty pred X(x:* pred[ind]) {true} };", 1)>]
    [<DataRow("09b", "def pred Test(x: ind) {true prty pred X(x:* pred[obj]) {true} };", 1)>]
    [<DataRow("09c", "def pred Test(x: ind) {true prty pred X() {dec ~x: obj; true} };", 1)>]
    [<DataRow("09d", "def pred Test(x: ind) {true prty pred X() {dec ~x:* obj[ind]; true} };", 1)>]
    [<DataRow("09e", "def pred Test(x: ind) {true prty pred X() {dec ~x:* obj[obj]; true} };", 1)>]
    [<DataRow("09f", "def pred Test(x: ind) {true prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("09g", "def pred Test(x: ind) {true prty func X(x:* pred[ind])->obj {intr} };", 1)>]
    [<DataRow("09h", "def pred Test(x: ind) {true prty func X(x:* pred[obj])->obj {intr} };", 1)>]
    [<DataRow("09i", "def pred Test(x: ind) {true prty func X()->obj {dec ~x: obj; return x} };", 1)>]
    [<DataRow("09j", "def pred Test(x: ind) {true prty func X()->obj {dec ~x:* obj[ind]; return x} };", 1)>]
    [<DataRow("09k", "def pred Test(x: ind) {true prty func X()->obj {dec ~x:* obj[obj]; return x} };", 1)>]
    [<DataRow("10", "def cl Test {dec ~x:ind; ctor Test(x: pred) {} };", 1)>]
    [<DataRow("10a", "def cl Test {dec ~x:ind; ctor Test(x:* pred[ind]) {} };", 1)>]
    [<DataRow("10b", "def cl Test {dec ~x:ind; ctor Test(x:* pred[ind]) {} };", 1)>]
    [<DataRow("10c", "def cl Test {dec ~x:ind; ctor Test() {dec ~x: obj; } };", 1)>]
    [<DataRow("10d", "def cl Test {dec ~x:ind; ctor Test() {dec ~x:* obj[ind]; } };", 1)>]
    [<DataRow("10e", "def cl Test {dec ~x:ind; ctor Test() {dec ~x:* obj[obj]; } };", 1)>]
    [<DataRow("11", "def cl Test {dec ~x:obj; constructor Test() {} prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("11a", "def cl Test {dec ~x:obj; constructor Test(x: pred) {} prty func X()->obj {intr} };", 1)>]
    [<DataRow("11b", "def cl Test {dec ~x:obj; constructor Test() {dec ~x: pred; } prty func X()->obj {intr} };", 1)>]
    [<DataRow("11c", "def cl Test {dec ~x:obj; constructor Test() {} prty func X()->obj {dec ~x: pred; return x} };", 1)>]
    [<DataRow("12", "inf ExistsByExample {dec ~p: pred(c: obj) ~x: obj; pre: p(c) con: ex x:obj {p(x)}};", 1)>]
    [<DataRow("12a", "inf ExistsByExample {dec ~p: pred(c: obj); pre: p(c) con: ex x:obj {p(x)}};", 0)>]
    [<DataRow("12b", "inf ExistsByExample {dec ~p: pred(c: obj) ~c: obj; pre: true con: true};", 1)>]
    [<DataRow("13", """loc and(p,q) := !tex: p "\wedge" q;;""", 0)>]
    [<DataRow("14", """def cl B {intr} def cl A {dec ~x:obj; ctor A(x:B) {} };""", 1)>]
    [<DataRow("15", "axiom T {dec ~p:pred(n:obj); all n:Nat{p(n)} };", 1)>]
    [<DataRow("15a", "axiom T {dec ~p:pred(n:obj); all n1:Nat{p(n1)} };", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR03(no: string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR03 ("", "")
            runTestHelper "TestVAR03.fpl" fplCode code expected

    [<DataRow("01", "def pred T(x:obj) {true};", 1)>]
    [<DataRow("01a", "def pred T(x:obj) {intr};", 0)>]
    [<DataRow("01b", "def pred T(x:obj) {x};", 0)>]
    [<DataRow("01c", "def pred T(x:obj) {y};", 1)>]
    [<DataRow("01d", "def pred T() {dec ~x:obj; true};", 1)>]
    [<DataRow("01e", "def pred T() {dec ~x:obj; x};", 0)>]
    [<DataRow("01f", "def pred T() {dec ~x:obj; y};", 1)>]
    [<DataRow("02", "def func T(x:obj)->obj {return xy};", 1)>]
    [<DataRow("02a", "def func T(x:obj)->obj {intr};", 0)>]
    [<DataRow("02b", "def func T(x:obj)->obj {return x};", 0)>]
    [<DataRow("02c", "def func T()->obj {dec ~x:obj; return xy};", 1)>]
    [<DataRow("02d", "def func T()->obj {dec ~x:obj; return x};", 0)>]
    [<DataRow("03", "def cl T {dec ~x:obj; ctor T() {}};", 1)>]
    [<DataRow("03a", "def cl T {dec ~x:obj; ctor T() {dec x:=x; }};", 0)>]
    [<DataRow("03b", "def cl T {dec ~x:obj; ctor T() {dec y:=y; }};", 1)>]
    [<DataRow("04", "thm T {dec ~x:obj; true};", 1)>]
    [<DataRow("04a", "thm T {dec ~x:obj; true} prf T$1 {1. |- x qed};", 1)>]
    [<DataRow("04b", "thm T {dec ~x:obj; true} prf T$1 {1. |- trivial qed};", 1)>]
    [<DataRow("04c", "thm T {dec ~x:obj; x};", 0)>]
    [<DataRow("04d", "thm T {dec ~x:pred; x};", 0)>]
    [<DataRow("04e", "thm T {dec ~x:obj; y};", 1)>]
    [<DataRow("04f", "thm T {dec ~x:pred; y};", 1)>]
    [<DataRow("05", "prop T {dec ~x:obj; true};", 1)>]
    [<DataRow("05a", "prop T {dec ~x:obj; true} prf T$1 {1. |- x qed};", 1)>]
    [<DataRow("05b", "prop T {dec ~x:obj; true} prf T$1 {1. |- trivial qed};", 1)>]
    [<DataRow("05c", "prop T {dec ~x:obj; x};", 0)>]
    [<DataRow("05d", "prop T {dec ~x:pred; x};", 0)>]
    [<DataRow("05e", "prop T {dec ~x:obj; y};", 1)>]
    [<DataRow("05f", "prop T {dec ~x:pred; y};", 1)>]
    [<DataRow("06", "lem T {dec ~x:obj; true};", 1)>]
    [<DataRow("06a", "lem T {dec ~x:obj; true} prf T$1 {1. |- x qed};", 1)>]
    [<DataRow("06b", "lem T {dec ~x:obj; true} prf T$1 {1. |- trivial qed};", 1)>]
    [<DataRow("06c", "lem T {dec ~x:obj; x};", 0)>]
    [<DataRow("06d", "lem T {dec ~x:pred; x};", 0)>]
    [<DataRow("06e", "lem T {dec ~x:obj; y};", 1)>]
    [<DataRow("06f", "lem T {dec ~x:pred; y};", 1)>]
    [<DataRow("06", "ax T {dec ~x:obj; true};", 1)>]
    [<DataRow("06a", "ax T {dec ~x:obj; x};", 0)>]
    [<DataRow("06b", "ax T {dec ~x:pred; x};", 0)>]
    [<DataRow("06c", "ax T {dec ~x:obj; y};", 1)>]
    [<DataRow("06d", "ax T {dec ~x:pred; y};", 1)>]
    [<DataRow("07", "conj T {dec ~x:obj; true};", 1)>]
    [<DataRow("07a", "conj T {dec ~x:obj; x};", 0)>]
    [<DataRow("07b", "conj T {dec ~x:pred; x};", 0)>]
    [<DataRow("07c", "conj T {dec ~x:obj; y};", 1)>]
    [<DataRow("07d", "conj T {dec ~x:pred; y};", 1)>]
    [<DataRow("08", "cor T$1 {dec ~x:obj; true};", 1)>]
    [<DataRow("08a", "cor T$1 {dec ~x:obj; true} prf T$1 {1. |- x qed};", 1)>]
    [<DataRow("08b", "cor T$1 {dec ~x:obj; true} prf T$1 {1. |- trivial qed};", 1)>]
    [<DataRow("08c", "cor T$1 {dec ~x:obj; x};", 0)>]
    [<DataRow("08d", "cor T$1 {dec ~x:pred; x};", 0)>]
    [<DataRow("08e", "cor T$1 {dec ~x:obj; y};", 1)>]
    [<DataRow("08f", "cor T$1 {dec ~x:pred; y};", 1)>]
    [<DataRow("09", "inf T {dec ~x:pred; pre: true con:true};", 1)>]
    [<DataRow("09a", "inf T {dec ~x:pred; pre: true con:x};", 0)>]
    [<DataRow("09b", "inf T {dec ~x:pred; pre: x con:true};", 0)>]
    [<DataRow("09c", "inf T {dec ~x:pred; pre: true con:y};", 1)>]
    [<DataRow("09d", "inf T {dec ~x:pred; pre: y con:true};", 1)>]
    [<DataRow("09e", "inf T {dec ~x:pred; pre: true con:true};", 1)>]
    [<DataRow("09f", "inf T {dec ~x:pred; pre: true con:x};", 0)>]
    [<DataRow("09g", "inf T {dec ~x:pred; pre: x con:true};", 0)>]
    [<DataRow("09h", "inf T {dec ~x:pred; pre: true con:y};", 1)>]
    [<DataRow("09i", "inf T {dec ~x:pred; pre: y con:true};", 1)>]
    [<DataRow("19", """loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;;""", 0)>]
    [<DataRow("19a", """loc iif(x, y) := !tex: x "\Leftrightarrow" !eng: x " if and only if " !ger: x " dann und nur dann wenn ";;""", 3)>]
    [<DataRow("19b", """loc iif(x, y) := !tex: "\Leftrightarrow" !eng: " if and only if " !ger: " dann und nur dann wenn ";;""", 6)>]
    [<DataRow("20", "def pred S() {intr prty pred T(x:obj) {intr}};", 0)>]
    [<DataRow("20a", "def pred S() {intr prty pred T(x:obj) {x}};", 0)>]
    [<DataRow("20b", "def pred S() {intr prty pred T(x:obj) {y}};", 1)>]
    [<DataRow("20c", "def pred S() {intr prty pred T() {dec ~x:obj; true}};", 1)>]
    [<DataRow("20d", "def pred S() {intr prty pred T() {dec ~x:obj; x}};", 0)>]
    [<DataRow("20e", "def pred S() {intr prty pred T() {dec ~x:obj; y}};", 1)>]
    [<DataRow("21", "def pred S() {intr prty func T(x:obj)->obj {return xy}};", 1)>]
    [<DataRow("21a", "def pred S() {intr prty func T(x:obj)->obj {intr}};", 0)>]
    [<DataRow("21b", "def pred S() {intr prty func T(x:obj)->obj {return x}};", 0)>]
    [<DataRow("21c", "def pred S() {intr prty func T()->obj {dec ~x:obj; return xy}};", 1)>]
    [<DataRow("21d", "def pred S() {intr prty func T()->obj {dec ~x:obj; return x}};", 0)>]
    [<DataRow("22", "axiom T {dec ~p:pred(n:obj); all n:Nat{p(n)} };", 0)>]
    [<DataRow("23", "axiom T {dec ~p:pred(n:obj); p(@0)};", 0)>]
    [<DataRow("24a", "def cl A {dec ~myX:obj; ctor A(x:obj) {dec myX:=x;}};", 0)>]    
    [<DataRow("24b", "def cl A {dec ~myX:obj; ctor A(x:obj) {dec myX:=x;}} def cl B:A { ctor B() {dec base.A(@1); } };", 0)>]
    [<DataRow("25", "def func Sum(list:*Nat[ind])->Nat {dec ~result, addend: Nat result:=Zero() for addend in list { result:=Add(result,addend) } ; return result};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR04(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR04 ""
            runTestHelper "TestVAR04.fpl" fplCode code expected

    [<DataRow("def pred T() { all x:obj {true}};", 1)>]
    [<DataRow("def pred T() { all x:obj {x}};", 0)>]
    [<DataRow("def pred T() { ex x:obj {true}};", 1)>]
    [<DataRow("def pred T() { ex x:obj {x}};", 0)>]
    [<DataRow("def pred T() { exn$1 x:obj {true}};", 1)>]
    [<DataRow("def pred T() { exn$1 x:obj {x}};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR05(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR05 ""
            runTestHelper "TestVAR05.fpl" fplCode code expected

    [<DataRow("00a", "def cl T { dec ~x:obj; ctor T() { dec base.Obj() ; }} def cl S:T { dec ~x:obj; ctor S() { dec base.T() ; }} ;", 1)>]
    [<DataRow("00b", "def cl T { dec ~x:obj; ctor T() { dec base.Obj() ; }} def cl S:T { dec ~y:obj; ctor S() { dec base.T() ; }} ;", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR06Classes(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR06 ("","","","")
            runTestHelper "TestVAR06Classes.fpl" fplCode code expected
            
    // base inner / derived inner 
    [<DataRow("IIa", "def func T()->obj { dec ~x:obj; return x} def func S:T()->obj { dec ~x:obj; return x } ;", 1)>]
    [<DataRow("IIb", "def func T()->obj { dec ~x:obj; return x} def func S:T()->obj { dec ~y:obj; return y} ;", 0)>]
    [<DataRow("IIc", "def func T()->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T()->*obj[ind] { dec ~x:*obj[ind]; return x } ;", 1)>]
    [<DataRow("IId", "def func T()->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T()->*obj[ind] { dec ~y:*obj[ind]; return y} ;", 0)>]
    [<DataRow("IIe", "def func T()->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T()->*obj[ind] { dec ~x:obj; return x } ;", 1)>]
    [<DataRow("IIf", "def func T()->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T()->*obj[ind] { dec ~y:obj; return y} ;", 0)>]
    [<DataRow("IIg", "def func T()->obj { dec ~x:obj; return x} def func S:T()->obj { dec ~x:*obj[ind]; return x } ;", 1)>]
    [<DataRow("IIh", "def func T()->obj { dec ~x:obj; return x} def func S:T()->obj { dec ~y:*obj[ind]; return y} ;", 0)>]
    // base inner / derived signature 
    [<DataRow("ISa", "def func T(a:obj)->obj { dec ~x:obj; return x} def func S:T(x:obj)->obj ;", 1)>]
    [<DataRow("ISb", "def func T(a:obj)->obj { dec ~x:obj; return x} def func S:T(y:obj)->obj ;", 0)>]
    [<DataRow("ISc", "def func T(a:obj)->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T(x:obj)->*obj[ind] ;", 1)>]
    [<DataRow("ISd", "def func T(a:obj)->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T(y:obj)->*obj[ind] ;", 0)>]
    // base inner / derived signature (will cause SIG07 instead, VAR06 is only possible if SIG07 is not issued)
    [<DataRow("ISe", "def func T(a:obj)->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T(x:*obj[ind])->*obj[ind] ;", 0)>]
    [<DataRow("ISf", "def func T(a:obj)->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T(y:*obj[ind])->*obj[ind] ;", 0)>]
    [<DataRow("ISg", "def func T(a:*obj[ind])->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T(x:obj)->*obj[ind] ;", 0)>]
    [<DataRow("ISh", "def func T(a:*obj[ind])->*obj[ind] { dec ~x:*obj[ind]; return x} def func S:T(y:obj)->*obj[ind] ;", 0)>]
    // base inner / derived pred mapping 
    [<DataRow("IPa", "def func T()->pred(z:obj) { dec ~x:obj; return x} def func S:T()->pred(x:obj) ;", 1)>]
    [<DataRow("IPb", "def func T()->pred(z:obj) { dec ~x:obj; return x} def func S:T()->pred(y:obj) ;", 0)>]
    [<DataRow("IPc", "def func T()->pred(z:*obj[ind]) { dec ~x:*obj[ind]; return x} def func S:T()->pred(x:*obj[ind]) ;", 1)>]
    [<DataRow("IPd", "def func T()->pred(z:*obj[ind]) { dec ~x:*obj[ind]; return x} def func S:T()->pred(y:*obj[ind]) ;", 0)>]
    // base inner / derived pred mapping (will cause SIG07 instead, VAR06 is only possible if SIG07 is not issued)
    [<DataRow("IPe", "def func T()->pred(z:*obj[ind]) { dec ~x:*obj[ind]; return x} def func S:T()->pred(x:obj) ;", 0)>]
    [<DataRow("IPf", "def func T()->pred(z:*obj[ind]) { dec ~x:*obj[ind]; return x} def func S:T()->pred(y:obj) ;", 0)>]
    [<DataRow("IPg", "def func T()->pred(z:obj) { dec ~x:*obj[ind]; return x} def func S:T()->pred(x:*obj[ind]) ;", 0)>]
    [<DataRow("IPh", "def func T()->pred(z:obj) { dec ~x:*obj[ind]; return x} def func S:T()->pred(y:*obj[ind]) ;", 0)>]
    // base inner / derived func mapping 
    [<DataRow("IFa", "def func T()->func(z:obj)->obj { dec ~x:obj; return x} def func S:T()->func(x:obj)->obj ;", 1)>]
    [<DataRow("IFb", "def func T()->func(z:obj)->obj { dec ~x:obj; return x} def func S:T()->func(y:obj)->obj ;", 0)>]
    [<DataRow("IFc", "def func T()->func(z:*obj[ind])->obj { dec ~x:*obj[ind]; return x} def func S:T()->func(x:*obj[ind])->obj ;", 1)>]
    [<DataRow("IFd", "def func T()->func(z:*obj[ind])->obj { dec ~x:*obj[ind]; return x} def func S:T()->func(y:*obj[ind])->obj ;", 0)>]
    // base signature / derived inner 
    [<DataRow("SIa", "def func T(x:obj)->obj def func S:T(z:obj)->obj { dec ~x:obj; return x } ;", 1)>]
    [<DataRow("SIb", "def func T(x:obj)->obj def func S:T(z:obj)->obj { dec ~y:obj; return y } ;", 0)>]
    [<DataRow("SIc", "def func T(x:*obj[ind])->obj def func S:T(z:*obj[ind])->obj { dec ~x:*obj[ind]; return x } ;", 1)>]
    [<DataRow("SId", "def func T(x:*obj[ind])->obj def func S:T(z:*obj[ind])->obj { dec ~y:*obj[ind]; return y } ;", 0)>]
    // base signature / derived signature 
    [<DataRow("SSa", "def func T(x:obj)->obj def func S:T(x:obj)->obj ;", 1)>]
    [<DataRow("SSb", "def func T(x:obj)->obj def func S:T(y:obj)->obj ;", 0)>]
    [<DataRow("SSc", "def func T(x:*obj[ind])->obj def func S:T(x:*obj[ind])->obj ;", 1)>]
    [<DataRow("SSd", "def func T(x:*obj[ind])->obj def func S:T(y:*obj[ind])->obj ;", 0)>]
    // base signature / derived pred mapping 
    [<DataRow("SPa", "def func T(x:obj)->pred(a:obj) def func S:T(z:obj)->pred(x:obj) ;", 1)>]
    [<DataRow("SPb", "def func T(x:obj)->pred(a:obj) def func S:T(z:obj)->pred(y:obj) ;", 0)>]
    [<DataRow("SPc", "def func T(x:*obj[ind])->pred(a:*obj[ind]) def func S:T(z:*obj[ind])->pred(x:*obj[ind]) ;", 1)>]
    [<DataRow("SPd", "def func T(x:*obj[ind])->pred(a:*obj[ind]) def func S:T(z:*obj[ind])->pred(y:*obj[ind]) ;", 0)>]
    // base signature / derived func mapping 
    [<DataRow("SFa", "def func T(x:obj)->func(a:obj)->obj def func S:T(z:obj)->func(x:obj)->obj ;", 1)>]
    [<DataRow("SFb", "def func T(x:obj)->func(a:obj)->obj def func S:T(z:obj)->func(y:obj)->obj ;", 0)>]
    [<DataRow("SFc", "def func T(x:*obj[ind])->func(a:*obj[ind])->obj def func S:T(z:*obj[ind])->func(x:*obj[ind])->obj ;", 1)>]
    [<DataRow("SFd", "def func T(x:*obj[ind])->func(a:*obj[ind])->obj def func S:T(z:*obj[ind])->func(y:*obj[ind])->obj ;", 0)>]
    // base pred mapping / derived inner 
    [<DataRow("PIa", "def func T()->pred(x:obj) def func S:T()->pred(z:obj) { dec ~x:obj; return true} ;", 1)>]
    [<DataRow("PIb", "def func T()->pred(x:obj) def func S:T()->pred(z:obj) { dec ~y:obj; return true} ;", 0)>]
    [<DataRow("PIc", "def func T()->pred(x:*obj[ind]) def func S:T()->pred(z:*obj[ind]) { dec ~x:*obj[ind]; return true} ;", 1)>]
    [<DataRow("PId", "def func T()->pred(x:*obj[ind]) def func S:T()->pred(z:*obj[ind]) { dec ~y:*obj[ind]; return true} ;", 0)>]
    // base pred mapping / derived signature 
    [<DataRow("PSa", "def func T(a:obj)->pred(x:obj) def func S:T(z:obj)->pred(x:obj) ;", 1)>]
    [<DataRow("PSb", "def func T(a:obj)->pred(x:obj) def func S:T(z:obj)->pred(y:obj) ;", 0)>]
    [<DataRow("PSc", "def func T(a:*obj[ind])->pred(x:*obj[ind]) def func S:T(z:*obj[ind])->pred(x:*obj[ind]) ;", 1)>]
    [<DataRow("PSd", "def func T(a:*obj[ind])->pred(x:*obj[ind]) def func S:T(z:*obj[ind])->pred(y:*obj[ind]) ;", 0)>]
    // base pred mapping / derived pred mapping 
    [<DataRow("PPa", "def func T()->pred(x:obj) def func S:T()->pred(x:obj) ;", 1)>]
    [<DataRow("PPb", "def func T()->pred(x:obj) def func S:T()->pred(y:obj) ;", 0)>]
    [<DataRow("PPc", "def func T()->pred(x:*obj[ind]) def func S:T()->pred(x:*obj[ind]) ;", 1)>]
    [<DataRow("PPd", "def func T()->pred(x:*obj[ind]) def func S:T()->pred(y:*obj[ind]) ;", 0)>]
    // base pred mapping / derived func mapping (will cause SIG07 instead, VAR06 is only possible if SIG07 is not issued)
    [<DataRow("PFa", "def func T()->pred(x:obj) def func S:T()->func(x:obj)->obj ;", 0)>]
    [<DataRow("PFb", "def func T()->pred(x:obj) def func S:T()->func(y:obj)->obj ;", 0)>]
    [<DataRow("PFc", "def func T()->pred(x:*obj[ind]) def func S:T()->func(x:*obj[ind])->obj ;", 0)>]
    [<DataRow("PFd", "def func T()->pred(x:*obj[ind]) def func S:T()->func(y:*obj[ind])->obj ;", 0)>]
    // base func mapping / derived inner
    [<DataRow("FIa", "def func T()->func(x:obj)->obj def func S:T()->func(z:obj)->obj { dec ~x:obj; return x } ;", 1)>]
    [<DataRow("FIb", "def func T()->func(x:obj)->obj def func S:T()->func(z:obj)->obj { dec ~y:obj; return y } ;", 0)>]
    [<DataRow("FIc", "def func T()->func(x:*obj[ind])->obj def func S:T()->func(z:*obj[ind])->obj { dec ~x:*obj[ind]; return x } ;", 1)>]
    [<DataRow("FId", "def func T()->func(x:*obj[ind])->obj def func S:T()->func(z:*obj[ind])->obj { dec ~y:*obj[ind]; return y } ;", 0)>]
    // base func mapping / derived signature 
    [<DataRow("FSa", "def func T(a:obj)->func(x:obj)->obj def func S:T(x:obj)->func(z:obj)->obj ;", 1)>]
    [<DataRow("FSb", "def func T(a:obj)->func(x:obj)->obj def func S:T(y:obj)->func(z:obj)->obj ;", 0)>]
    [<DataRow("FSc", "def func T(a:*obj[ind])->func(x:*obj[ind])->obj def func S:T(x:*obj[ind])->func(z:*obj[ind])->obj ;", 1)>]
    [<DataRow("FSd", "def func T(a:*obj[ind])->func(x:*obj[ind])->obj def func S:T(y:*obj[ind])->func(z:*obj[ind])->obj ;", 0)>]
    // base func mapping / derived pred mapping (will cause SIG07 instead, VAR06 is only possible if SIG07 is not issued)
    [<DataRow("FPa", "def func T()->func(x:obj)->obj def func S:T()->pred(x:obj) ;", 0)>]
    [<DataRow("FPb", "def func T()->func(x:obj)->obj def func S:T()->pred(y:obj) ;", 0)>]
    [<DataRow("FPc", "def func T()->func(x:*obj[ind])->obj def func S:T()->pred(x:*obj[ind]) ;", 0)>]
    [<DataRow("FPd", "def func T()->func(x:*obj[ind])->obj def func S:T()->pred(y:*obj[ind]) ;", 0)>]
    // base func mapping / derived func mapping 
    [<DataRow("FFa", "def func T()->func(x:obj)->obj def func S:T()->func(x:obj)->obj ;", 1)>]
    [<DataRow("FFb", "def func T()->func(x:obj)->obj def func S:T()->func(y:obj)->obj ;", 0)>]
    [<DataRow("FFc", "def func T()->func(x:*obj[ind])->obj def func S:T()->func(x:*obj[ind])->obj ;", 1)>]
    [<DataRow("FFd", "def func T()->func(x:*obj[ind])->obj def func S:T()->func(y:*obj[ind])->obj ;", 0)>]
    // general
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR06FunctionalTerms(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR06 ("","","","")
            runTestHelper "TestVAR06FunctionalTerms.fpl" fplCode code expected

    [<DataRow("00", "def pred T() {exn$1 n:pred { n } };", 0)>]
    [<DataRow("01", "def pred T() {exn$1 n, m:pred { n } };", 1)>]
    [<DataRow("02", "def pred T() {exn$1 n:pred, m:pred { n } };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR07(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR07 ""
            runTestHelper "TestVAR07.fpl" fplCode code expected


    [<DataRow("00", "axiom T {all n:pred { n } };", 0)>]
    [<DataRow("00a", "axiom T {all n:*pred[ind] { n } };", 1)>]
    [<DataRow("00b", "axiom T {all n:*pred[obj] { n } };", 1)>]
    [<DataRow("01", "axiom T {ex n:pred { n } };", 0)>]
    [<DataRow("01a", "axiom T {ex n:*pred[ind] { n } };", 1)>]
    [<DataRow("01b", "axiom T {ex n:*pred[obj] { n } };", 1)>]
    [<DataRow("02", "axiom T {exn$1 n:pred { n } };", 0)>]
    [<DataRow("02a", "axiom T {exn$1 n:*pred[ind] { n } };", 1)>]
    [<DataRow("02a", "axiom T {exn$1 n:*pred[obj] { n } };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR08(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR08
            runTestHelper "TestVAR08.fpl" fplCode code expected

    [<DataRow("00", """def pred T() { not true };""", 0)>]
    [<DataRow("01", """def pred T() { dec ~x:pred; not x };""", 1)>] 
    [<DataRow("01a", """def pred T(x:pred) { not x };""", 0)>] 
    [<DataRow("01b", """def func T()->obj { dec ~x:pred assert not x; return x };""", 1)>] 
    [<DataRow("01c", """def func T()->pred(x:obj) { dec assert not x; return x };""", 0)>] 
    [<DataRow("01d", """def cl T {dec ~x:pred assert not x; ctor T(){}};""", 1)>] 
    [<DataRow("01e", """def cl T {ctor T(){dec ~x:pred assert not x; }};""", 1)>] 
    [<DataRow("01f", """def cl T {ctor T(x:pred){dec assert not x;}};""", 0)>] 
    [<DataRow("01g", """def cl T {intr prty pred T() {dec ~x:pred; not x}};""", 1)>] 
    [<DataRow("01h", """def cl T {intr prty pred T(x:pred) {not x}};""", 0)>] 
    [<DataRow("01i", """def cl T {intr prty func T()->obj {dec ~x:pred assert not x; return x}};""", 1)>] 
    [<DataRow("01j", """def cl T {intr prty func T(x:pred)->obj {dec assert not x; return x}};""", 0)>] 
    [<DataRow("01k", """def cl T {intr prty func T()->pred(x:pred) {dec assert not x; return x }};""", 0)>] 
    [<DataRow("02", """def pred T() { dec ~x:ind; not x };""", 1)>]
    [<DataRow("03", """def pred T() { dec ~x:pred; not (x) };""", 1)>]
    [<DataRow("04", """def pred T() { dec ~x:pred; not ((x)) };""", 1)>]
    [<DataRow("05", """def pred T() { dec ~x:pred; not (((x))) };""", 1)>]
    [<DataRow("06", """def pred T() { all x:obj {true} };""", 0)>]
    [<DataRow("07", """def pred T() { dec ~x:pred; and(x,true) };""", 1)>]
    [<DataRow("08", """def pred T() { dec ~x:pred; all y:obj {and(x,true)} };""", 1)>]
    [<DataRow("09", """def pred T() { dec ~x:pred; or(x,false) };""", 1)>]
    [<DataRow("10", """def pred T() { dec ~x,y:pred; or(x,y) };""", 2)>]
    [<DataRow("11a", """def pred T() { all y:obj {and(x,y)} };""", 1)>]
    [<DataRow("11b", """def pred T() { ex y:obj {and(x,y)} };""", 1)>]
    [<DataRow("11c", """def pred T() { exn$1 y:obj {and(x,y)} };""", 1)>]
    [<DataRow("11d", """def pred T() { all x,y:obj {and(x,y)} };""", 0)>]
    [<DataRow("11e", """def pred T() { ex x,y:obj {and(x,y)} };""", 0)>]
    [<DataRow("11f", """def pred T() { exn$1 y:obj {and(true,y)} };""", 0)>]
    [<DataRow("12", """inf ModusTollens {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("13", """inf ModusTollens {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not p};""", 0)>]
    [<DataRow("14", """def pred T() { dec ~x:pred; impl(true,x) };""", 1)>]
    [<DataRow("15", """def pred T() { dec ~x,y:pred; impl(x,y) };""", 2)>]
    [<DataRow("16", """def pred T() { impl(true,true) };""", 0)>]
    [<DataRow("17", """def pred T() { dec ~x:pred; iif(true,x) };""", 1)>]
    [<DataRow("18", """def pred T() { dec ~x,y:pred; iif(x,y) };""", 2)>]
    [<DataRow("19", """def pred T() { iif(true,true) };""", 0)>]
    [<DataRow("20", """def pred T() { xor(xor(true,true),true) };""", 0)>]
    [<DataRow("21", """def pred T() { all x,y:pred { xor(xor(y,x),true) } };""", 0)>]
    [<DataRow("22", """def pred T() { all i:Nat {true} };""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR09(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR09 ("","")
            runTestHelper "TestVAR09.fpl" fplCode code expected
