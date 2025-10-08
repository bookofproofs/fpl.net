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

    [<DataRow("def class SomeClass:obj {intrinsic} def class SomeClass:obj {intrinsic} ;", 1)>]
    [<DataRow("def class SomeClass:obj {intrinsic} ;", 0)>]

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
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T(x:@Digits) {true};", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T(x:@Typo) {true};", 0)>]

    [<DataRow("def func Sum(list:* Nat)->Nat {dec ~result: Nat; return result} def func Sum2(list:* Nat)->Nat {dec ~result: Nat; return result};", 0)>]
    [<DataRow("""def cl B:obj {intr} def cl A:obj {dec ~x:obj; ctor A(y:B) {} };""", 0)>]
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
    [<DataRow("def cl Test:obj {intr} proof Test$1 {1. |- trivial};", 1)>]
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
    [<DataRow("def cl Test:obj {intr} corollary Test$1 {true};", 1)>]
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

    [<DataRow("""def cl A:obj {intr} def pred T() {dec ~x:A x:=A; x};""", 1)>]
    [<DataRow("""def cl A:obj {intr} def pred T() {dec ~x:A x:=A(); x};""", 0)>]
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

    [<DataRow("def cl Test:obj {ctor TestTypo(x:Nat) {}};", 1)>]
    [<DataRow("def cl Test:obj {ctor TestTypo1() {}};", 1)>]
    [<DataRow("def cl Test:obj {ctor Test() {}};", 0)>]
    [<DataRow("def cl Test:obj {dec ~x:obj x := 0; ctor Test() {dec base.obj(); }};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID008(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID008 ("", "") 
            runTestHelper "TestID008.fpl" fplCode code expected

    [<DataRow("def cl Test:obj {intr};", 0)>]
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

    [<DataRow("def cl Test:obj {intr};", 0)>]
    [<DataRow("def cl Test:Set {intr};", 1)>]
    [<DataRow("def class Set: obj {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("def cl Set:obj {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("def cl Set:obj {intr} def cl Test:SetTypo {intr};", 1)>]
    [<DataRow("def cl Set:obj {intr} def pred Test() {dec ~x:Set; true};", 0)>]
    [<DataRow("def cl Set:obj {intr} def pred Test() {dec ~x:object; is(x,Set)};", 0)>]

    [<DataRow("def cl A:obj {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("def cl A:obj {intr} thm T {true} proof T$1 {1. bydef B |- trivial };", 1)>]
    [<DataRow("thm A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("thm B {true} thm T {true} proof T$1 {1. A |- trivial };", 1)>]

    // the following examples should not emit ID010 because this context is covered by the SIG04 diagnostics
    [<DataRow("def pred Test(x:Set) {intr};", 0)>]
    [<DataRow("def class Set: obj {intr} def pred IsEmpty(x: Set) {true};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID010(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID010 ""
            runTestHelper "TestID010.fpl" fplCode code expected

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
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID011(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID011 ("","")
            runTestHelper "TestID011.fpl" fplCode code expected

    [<DataRow("def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); } };", 1)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {ctor B() {dec base.A(); } };", 0)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {ctor B() {dec base.C(); } };", 1)>]
    [<DataRow("def cl A:obj { ctor A() {dec base.obj(); } };", 0)>]
    [<DataRow("def cl A:obj { ctor A() {dec base.B(); } };", 1)>]
    [<DataRow("def cl A:C { ctor A() {dec base.obj(); } };", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.obj(); } };", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); } };", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID012(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID012 ("","")
            runTestHelper "TestID012.fpl" fplCode code expected


    [<DataRow("00", "def pred T() {del.Test()};", 1, "Unknown delegate `Test`")>]
    [<DataRow("01", "def pred T() {del.Test1(x,y)};", 1, "Unknown delegate `Test1`")>]
    [<DataRow("02", "def pred T() {del.Equal(x,y)};", 1, "Predicate `=` cannot be evaluated because the left argument is undefined.")>]
    [<DataRow("03", "def pred T(x:pred) {del.Equal(x,y)};", 1, "Predicate `=` cannot be evaluated because the right argument is undefined.")>]
    [<DataRow("04", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)};""", 0, "missing error message")>]
    [<DataRow("05", "def pred T(x,y:pred) {del.Equal(true,y)};", 1, "Predicate `=` cannot be evaluated because the right argument is undetermined.")>]
    [<DataRow("06", "ax T {all x,y:obj {del.Equal(x,y)}};", 0, "missing error message")>]
    [<DataRow("06a", "def cl Nat: obj {intr} ax T {all x,y:Nat {del.Equal(x,y)}};", 0, "missing error message")>]
    [<DataRow("06b", "ax T {all x,y:Bla {del.Equal(x,y)}};", 0, "Predicate `=` cannot be evaluated because the left argument is undefined.")>]
    [<DataRow("07", "ax T {exn$1 x:obj {del.Equal(x,@1)}};", 0, "missing error message")>]
    [<DataRow("07a", "def cl Nat: obj {intr} ax T {exn$1 x:Nat {del.Equal(x,@1)}};", 0, "missing error message")>]
    [<DataRow("07b", "ax T {exn$1 x:obj {del.Equal(x,$1)}};", 0, "missing error message")>]
    [<DataRow("07b_", "def cl Nat: obj {intr} ax T {exn$1 x:Nat {del.Equal(x,$1)}};", 0, "missing error message")>]
    [<DataRow("08", """ax T {all n:obj {exn$1 y:obj {del.Equal(y,n)}}};""", 0, "missing error message")>]
    [<DataRow("08a", """def cl Nat: obj {intr} ax T {all n:Nat {exn$1 y:Nat {del.Equal(y,n)}}};""", 0, "missing error message")>]
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

    [<DataRow("00a", """def cl A:obj {dec ~x:obj x:=parent; ctor A() {}};""", 1)>]
    [<DataRow("00b", """def cl A:obj {ctor A() {dec ~x:obj x:=parent;}};""", 0)>]
    [<DataRow("00c", """def cl A:obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("00d", """def cl A:obj {intr property func T()->obj {dec ~x:obj x:=parent; return x } };""", 0)>]
    [<DataRow("00e", """def cl A:obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("00f", """def cl A:obj {intr property func T()->obj {dec ~x:obj x:=parent; return x } };""", 0)>]
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

    [<DataRow("00a", """def cl A:obj {dec ~x:obj x:=self; ctor A() {}};""", 0)>]
    [<DataRow("00b", """def cl A:obj {ctor A() {dec ~x:obj x:=self;}};""", 1)>]
    [<DataRow("00c", """def cl A:obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("00d", """def cl A:obj {intr property func T()->obj {dec ~x:obj x:=self; return x } };""", 0)>]
    [<DataRow("00e", """def cl A:obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("00f", """def cl A:obj {intr property func T()->obj {dec ~x:obj x:=self; return x } };""", 0)>]
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

    [<DataRow("def pred A() {true} def pred A(x:obj) {true} def pred T(x:A) {intr};", 1)>]
    [<DataRow("def pred A() {true} def func A(x:obj)->obj {intr} def pred T(x:A) {intr};", 1)>]
    [<DataRow("def pred A() {true} def func A()->obj {intr} def pred T(x:A) {intr};", 1)>]
    [<DataRow("def pred A() {true} def pred T(x:A) {intr};", 0)>]
    [<DataRow("def func A(x:obj)->obj {intr} def pred T(x:A) {intr};", 0)>]
    [<DataRow("def pred B() {true} def func A()->obj {intr} def pred T(x:A) {intr};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID017(fplCode:string, expected) =
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

    [<DataRow("00", @"ext Digits x @ /\d+/->obj {dec ~y:obj; return y} def pred T(x:@Digits) {true};", 0)>]
    [<DataRow("01", @"ext Digits x @ /\d+/->obj {dec ~y:obj; return y} def pred T(x:@Typo) {true};", 1)>]
    [<DataRow("01", @"ext Digits x @ /\d+/->obj {dec ~y:obj; return y} def pred T(x:tpl) {true};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID019(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID019 ""
            runTestHelper "TestID019.fpl" fplCode code expected


    [<DataRow("00", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.A(); } };", 2)>]
    [<DataRow("00a", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.B(); } };", 2)>]
    [<DataRow("00b", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.C(); } };", 2)>]
    [<DataRow("00c", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.A() base.B(); } };", 1)>]
    [<DataRow("00d", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.A() base.C(); } };", 1)>]
    [<DataRow("00e", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.B() base.C(); } };", 1)>]
    [<DataRow("00f", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.A() base.B() base.C(); } };", 0)>]
    [<DataRow("01", "def cl A:obj {intr} def cl B:A {ctor B() {dec base.A(); } };", 0)>]
    [<DataRow("01a", "def cl A:obj {intr} def cl B:A {ctor B() {} };", 1)>]
    [<DataRow("02", "def cl A:obj { ctor A() {dec base.obj(); } };", 0)>]
    [<DataRow("02a", "def cl A:obj { ctor A() {} };", 1)>]
    [<DataRow("03", "def cl A:obj { ctor A() {dec base.obj(); } };", 0)>]
    [<DataRow("03a", "def cl A:C { ctor A() {dec base.obj(); } };", 1)>]
    [<DataRow("03b", "def cl A:obj {intr} def cl B:A {intr} def cl C:B { ctor C() {dec base.obj(); } };", 1)>]
    [<DataRow("04", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.obj(); } };", 1)>]
    [<DataRow("04a", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); } };", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID020(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID020 ""
            runTestHelper "TestID020.fpl" fplCode code expected

    [<DataRow("01", "def cl A:obj {intr} def cl B:A {ctor B() {dec base.A(); } };", 0)>]
    [<DataRow("01a", "def cl A:obj {intr} def cl B:A {ctor B() {dec base.A() base.A(); } };", 1)>]
    [<DataRow("02", "def cl A:obj { ctor A() {dec base.obj(); } };", 0)>]
    [<DataRow("02a", "def cl A:obj { ctor A() {dec base.obj() base.obj(); } };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID021(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID021 ""
            runTestHelper "TestID021.fpl" fplCode code expected

    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID022(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID022 "" 
            runTestHelper "TestID022.fpl" fplCode code expected

    [<DataRow("00a", "def cl A:obj {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("00b", "def cl A:obj {intr} def cl B:A {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("01a", "lem A {true} thm T {true} proof T$1 {1. A |- trivial };", 0)>]
    [<DataRow("01b", "lem A {true} def pred A(x:obj) {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 1)>]
    [<DataRow("02a", "cor A$1 {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 0)>]
    [<DataRow("02b", "proof A$1 {1. |-  trivial} cor A$1 {true} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
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
    [<DataRow("01e", """def cl A:obj {intr} ax T { A };""", 1)>]
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
            let code = ID025 ("", "", "")
            runTestHelper "TestID025.fpl" fplCode code expected

    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID026(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID026 ("","")
            runTestHelper "TestID026.fpl" fplCode code expected

    [<DataRow("00", """def pred T() { not true };""", 0)>]
    [<DataRow("01", """def pred T() { dec ~x:pred; not x };""", 0)>] // no LG000 diagnostics because of intrinsic use x
    [<DataRow("02", """def pred T() { dec ~x:ind; not x };""", 0)>]
    [<DataRow("03", """def pred T() { dec ~x:pred; not (x) };""", 0)>]
    [<DataRow("04", """def pred T() { dec ~x:pred; not ((x)) };""", 0)>]
    [<DataRow("05", """def pred T() { dec ~x:pred; not (((x))) };""", 0)>]
    [<DataRow("06", """def pred T() { all x:obj {true} };""", 0)>]
    [<DataRow("07", """def pred T() { dec ~x:pred; and(x,true) };""", 1)>]
    [<DataRow("08", """def pred T() { dec ~x:pred; all y:obj {and(x,true)} };""", 1)>]
    [<DataRow("09", """def pred T() { dec ~x:pred; or(x,false) };""", 1)>]
    [<DataRow("10", """def pred T() { dec ~x,y:pred; or(x,y) };""", 0)>]
    [<DataRow("11", """def pred T() { all y:obj {and(x,y)} };""", 0)>]
    [<DataRow("12", """inf ModusTollens {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("13", """inf ModusTollens {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not p};""", 0)>]
    [<DataRow("14", """def pred T() { dec ~x:pred; impl(true,x) };""", 1)>]
    [<DataRow("15", """def pred T() { dec ~x,y:pred; impl(x,y) };""", 0)>]
    [<DataRow("16", """def pred T() { impl(true,true) };""", 0)>]
    [<DataRow("17", """def pred T() { dec ~x:pred; iif(true,x) };""", 1)>]
    [<DataRow("18", """def pred T() { dec ~x,y:pred; iif(x,y) };""", 0)>]
    [<DataRow("19", """def pred T() { iif(true,true) };""", 0)>]
    [<DataRow("20", """def pred T() { xor(xor(true,true),true) };""", 0)>]
    [<DataRow("21", """def pred T() { dec ~x,y:pred; xor(xor(y,x),true) };""", 1)>]
    [<DataRow("22", """def pred T() { all i:Nat {true} };""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestLG000(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG000 ("","")
            runTestHelper "TestLG000.fpl" fplCode code expected

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
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestLG001(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("","","")
            runTestHelper "TestLG001.fpl" fplCode code expected

    [<DataRow("""axiom A {dec ~x,y:Nat; impl(x,y)};""", 31)>]
    [<TestMethod>]
    member this.TestLG001Position(fplCode:string, (expected:int64)) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("","","")
            prepareFplCode ("TestLG001Position.fpl", fplCode, false) |> ignore
            checkForUnexpectedErrors code
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<int64>(expected, result.Head.StartPos.Column)
        
    [<DataRow("""axiom A {dec ~x,y:obj; impl(x,y)};""", "Cannot evaluate `implication`; expecting a predicate argument `x`, got `obj`.")>]
    [<DataRow("""axiom A {dec ~x,y:ind; impl(x,y)};""", "Cannot evaluate `implication`; expecting a predicate argument `x`, got `ind`.")>]
    [<DataRow("""axiom A {dec ~x,y:func; impl(x,y)};""", "Cannot evaluate `implication`; expecting a predicate argument `x`, got `func`.")>]
    [<DataRow("""axiom A {impl(x,y)};""", "Cannot evaluate `implication`; expecting a predicate argument `x`, got `undef`.")>]
    [<DataRow("""axiom A {impl(T(),true)};""", "Cannot evaluate `implication`; expecting a predicate argument `T()`, got `undef`.")>]
    [<DataRow("""axiom A {impl(T,true)};""", "Cannot evaluate `implication`; expecting a predicate argument `T`, got `undef`.")>]
    [<DataRow("""def cl T:obj {intr} axiom A {impl(T,true)};""", "Cannot evaluate `implication`; expecting a predicate argument `T`, got `undef`.")>]
    [<TestMethod>]
    member this.TestLG001MsgSpecificity(fplCode:string, (expected:string)) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG001 ("","","")
            prepareFplCode ("TestLG001MsgSpecificity.fpl", fplCode, false) |> ignore
            checkForUnexpectedErrors code
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<string>(expected, result.Head.Message)


    [<DataRow("00", """def pred T() { true };""", 0)>]
    [<DataRow("01", """def pred T() { self };""", 1)>]
    [<DataRow("02", """def func T()->obj { intr };""", 0)>]
    [<DataRow("03", """def func T()->obj { return self };""", 1)>]
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
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestLG005(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG005 "" 
            runTestHelper "TestLG005.fpl" fplCode code expected

    [<DataRow("00", "def cl A:obj {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
    [<DataRow("00a", "def cl A:obj {intr} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
    [<DataRow("00b", "def cl A:obj {intr} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
    [<DataRow("00c", "def cl A:obj {intr} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
    [<DataRow("00d", "def cl A:obj {intr} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
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
    [<DataRow("03l", "def cl A:obj {intr} thm T {true} proof T$1 {1. byax A |- trivial };", 1)>]
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
    [<DataRow("04l", "def cl A:obj {intr} thm T {true} proof T$1 {1. byconj A |- trivial };", 1)>]
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
    [<DataRow("05f", "def cl A:obj {intr} thm T {true} proof T$1 {1. A |- trivial };", 1)>]
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
    [<DataRow("08k", "def cl A:obj {intr} thm T {true} proof T$1 {1. byinf A |- trivial };", 1)>]
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
    [<DataRow("09j", "def cl A:obj {intr} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
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
    [<DataRow("10j", "def cl A:obj {intr} thm T {true} proof T$1 {1. bycor A$1 |- trivial };", 1)>]
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
    [<DataRow("0a", "def cl A:obj {intr} thm T {true} proof T$1 {1. A$1:1 |- trivial };", 1)>]
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
    [<DataRow("3a", "def cl A:obj {intr} thm T {true} proof T$1 {1. A$1 |- trivial };", 1)>]
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
    [<DataRow("z0a", "def cl A:obj {intr} thm T {true} proof T$1 {1. bydef A |- trivial };", 0)>]
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
           
    [<DataRow("""def pred Or (x:+ pred) infix "or" 0 {true};""", 0)>]
    [<DataRow("""def pred Or (x:* pred) infix "or" 0 {true};""", 0)>]
    [<DataRow("""def pred T() {true};""", 1)>]
    [<DataRow("""def pred T(x:obj) infix "+" 0 {true};""", 1)>]
    [<DataRow("""def pred T(x,y:obj) infix "+" 0{true};""", 0)>]
    [<DataRow("""def pred T(x,y,z:obj) infix "+" 0{true};""", 1)>]
    [<DataRow("""def func T()->obj infix "+" 0{intr};""", 1)>]
    [<DataRow("""def func T(x:obj)->obj infix "+" 0{intr};""", 1)>]
    [<DataRow("""def func T(x,y:obj)->obj infix "+" 0{intr};""", 0)>]
    [<DataRow("""def func T(x,y,z:obj)->obj infix "+" 0{intr};""", 1)>]
    [<DataRow("""def pred T () prefix "+" {true};""", 1)>]
    [<DataRow("""def pred T(x:obj) prefix "+"  {true};""", 0)>]
    [<DataRow("""def pred T(x,y:obj) prefix "+" {true};""", 1)>]
    [<DataRow("""def func T()->obj prefix "+" {intr};""", 1)>]
    [<DataRow("""def func T(x:obj)->obj prefix "+" {intr};""", 0)>]
    [<DataRow("""def func T(x,y:obj)->obj prefix "+" {intr};""", 1)>]
    [<DataRow("""def pred T() postfix "+" {true};""", 1)>]
    [<DataRow("""def pred T(x:obj) postfix "+" {true};""", 0)>]
    [<DataRow("""def pred T(x,y:obj) postfix "+" {true};""", 1)>]
    [<DataRow("""def func T()->obj postfix "+" {intr};""", 1)>]
    [<DataRow("""def func T(x:obj)->obj postfix "+" {intr};""", 0)>]
    [<DataRow("""def func T(x,y:obj)->obj postfix "+" {intr};""", 1)>]
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
    [<DataRow("23", """def cl A:obj symbol "0" {intr} axiom T {0} ;""", 0)>]
    [<DataRow("24", """def cl A:obj symbol "1" {intr} axiom T {0} ;""", 1)>]
    [<DataRow("25", """def cl A:obj {intr} axiom T {0} ;""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG01(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG01 ""
            runTestHelper "TestSIG01.fpl" fplCode code expected

    [<DataRow("""def pred T (x,y:obj) infix "+" 1 {true};""", 0)>]
    [<DataRow("""def pred T1 (x,y:obj) infix "+" 1 {true} def pred T2 (x,y:obj) infix "+" 1 {true};""", 1)>]
    [<DataRow("""def pred T1  (x,y: obj) infix "+" 2 {intr} def pred T2 (x,y: obj) infix "*" 1 {intr};""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG02(fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG02 ("",0, "")
            runTestHelper "TestSIG02.fpl" fplCode code expected

    [<DataRow("00", "def func Test()->obj {dec ~x:obj; return x};", 0)>]
    [<DataRow("01", "def func Test()->obj {dec ~x:Nat; return x};", 1)>] // Nat is undefined, error
    [<DataRow("02", "def cl Nat:obj {intr} def func Test()->obj {dec ~x:Nat; return x};", 0)>]
    [<DataRow("03", "def cl Nat:obj {intr} def func Test()->Nat {dec ~x:Nat; return x};", 0)>] // Nat is obj, no error
    [<DataRow("03a", "def cl A:obj {intr} def cl B:A {intr} def func Test()->B {dec ~x:B; return x};", 0)>] // x is B, no error
    [<DataRow("03b", "def cl A:obj {intr} def cl B:A {intr} def func Test()->A {dec ~x:B; return x};", 0)>] // x is also A, no error
    [<DataRow("03c", "def cl A:obj {intr} def cl B:A {intr} def func Test()->obj {dec ~x:B; return x};", 0)>] // x is also obj, no error
    [<DataRow("04", "def cl Nat:obj {intr} def func Test()->obj {dec ~x:obj; return x};", 0)>]
    [<DataRow("04a", "def cl Nat:obj {intr} def func Test()->Nat {dec ~x:obj; return x};", 1)>] // obj is not Nat, error
    [<DataRow("05", "def func Test()->pred {dec ~x:pred; return x};", 0)>]
    [<DataRow("05a", "def func Test()->pred(y:obj) {dec ~a:pred(b:obj); return a};", 0)>]
    [<DataRow("05b", "def func Test()->pred(y:pred(z:ind)) {dec ~a:pred(b:obj); return a};", 0)>]
    [<DataRow("05b1", "def func Test()->pred(y:pred(z:ind)) {dec ~a:pred(b:pred(c:obj)); return a};", 1)>]
    [<DataRow("06", "def func Test()->func {dec ~x:func; return x};", 0)>]
    [<DataRow("07", "def func Test()->ind {dec ~x:ind; return x};", 0)>]
    [<TestMethod>]
    member this.TestSIG03(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG03 ("","")
            ad.Clear()
            runTestHelper "TestSIG03.fpl" fplCode code expected

    [<DataRow("00", "def cl Test:obj {intr};", 0)>]
    [<DataRow("01", "def cl Test:Set {intr};", 0)>] // this should cause the ID010 error only and not SIG04
    [<DataRow("02", "def class Set: obj {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("03", "def cl Set:obj {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("04", "def cl Set:obj {intr} def cl Test:SetTypo {intr};", 0)>] // this should cause the ID010 error only and not SIG04
    [<DataRow("05", "def pred Test(x:Set) {intr};", 1)>]
    [<DataRow("06", "def cl Set:obj {intr} def pred Test(x:Set) {intr};", 0)>]
    [<DataRow("07", "def cl Set:obj {intr} def pred Test(x:SetTypo) {intr};", 1)>]
    [<DataRow("08", "def cl Set:obj {intr} axiom Test {dec ~x:SetTypo; true};", 1)>]
    [<DataRow("09", "def cl Set:obj {intr} axiom Test {dec ~x:Set; true};", 0)>]
    [<DataRow("10", "def cl Set:obj {intr} def func PowerSer(x:Set) -> Set {dec ~y:Set; return y};", 0)>]
    [<DataRow("11", "def cl Set:obj {intr} axiom Test {dec ~x:Set; true};", 0)>]
    [<DataRow("12", "def cl Set:obj {intr} axiom Test {dec ~x:SetTypo; true};", 1)>]
    [<DataRow("13", "def pred Test() {dec ~x:Set; true};", 1)>]
    [<DataRow("14", "axiom A { all x:Nat {true} };", 1)>]
    [<DataRow("15", "def pred Test() {dec ~x:object; is(x,Set)};", 1)>]
    [<DataRow("16", "def cl Set:obj {intr} def pred Test() {dec ~x:object; is(x,Set)};", 0)>]
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
    [<DataRow("38", """def class Nat: obj {ctor Nat(){dec self:=x.R(); }};""", 1)>]
    [<DataRow("39", """def func Succ(n:Nat) -> obj {intr};""", 1)>]
    [<DataRow("40", """def func T()->obj { dec ~x:obj; return x};""", 0)>]
    [<DataRow("40a", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return S(x)} ;""", 0)>]
    [<DataRow("40b", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return (S(x)) } ;""", 0)>]
    [<DataRow("40c", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return S(S(x))} ;""", 0)>]
    [<DataRow("41", """def func T(y:obj)->obj { return self(y)} ;""", 0)>]
    [<DataRow("42", """def func T(y:obj)->obj { intr } def func S()->obj {dec ~x:obj; return T(x)} ;""", 0)>]
    [<DataRow("43", """axiom T { dec ~x:obj; all p:pred(y:obj) {p(x)}};""", 0)>]
    [<DataRow("44", """def cl A:obj {intr} def func Add(n,m:A)->A {return self(n,m)};""", 0)>]
    [<DataRow("45", """def cl A:obj {intr} def func Add(n,m:A)->A {dec ~x:A; return x};""", 0)>]
    [<DataRow("46", """def cl A:obj {intr} def func Add(n,m:A)->A {dec ~x:A; return x} prop P {dec ~op:Add; true};""", 0)>]
    [<DataRow("47", """def cl A:obj {intr property pred T() {true} property pred S() {T()}};""", 0)>]
    [<DataRow("48", """def cl A:obj {dec ~x:obj; ctor A(y:obj) {dec base.obj() x:=y; } property func P()->obj {return x}} def pred T(r:A) {r.P()};""", 0)>]
    [<DataRow("49", """def cl A:obj {ctor A(y:+obj) {}} def class B:obj {ctor B(z:+obj) {dec ~a:A base.obj() a := A(z); }};""", 0)>]
    [<DataRow("50", """def cl A:obj {intr property pred T() {true}} def cl B:A {ctor B() {dec base.A() assert self.T(); }};""", 0)>]
    [<DataRow("51", """def func A(n,m:obj)->obj {intr} prop T {dec ~op:A ~x,y:obj; (op(x,y) = x)};""", 0)>]
    [<DataRow("52", """def cl T:obj { dec ~x:+tpl; ctor T(y:+tpl) {dec base.obj() x:=y; } property func C(i:ind) -> tpl {return x[i]}};""", 0)>]
    [<DataRow("53", """def cl Nat:obj {intr} ext D x@/\d+/ -> Nat {dec ~n,m:Nat cases ( | (x = @0) : n:=m ? m:=n ); return n } def func Add()->obj {intr} prop K {dec ~op:Add ~n:Nat; ( op(n,@0) = n ) } ;""", 0)>]
    [<DataRow("54", """def cl C: obj {ctor C() {dec base.obj ();  }} def pred T() {dec ~cI1:C cI1:=C; true } ;""", 0)>]
    [<DataRow("54a", """def cl C: obj {ctor C() {dec base.obj ();  }} def pred T() {dec ~cI1:C cI1:=C(); true } ;""", 0)>]
    [<DataRow("54b", """def cl C: obj {ctor C(x:obj) {dec base.obj ();  }} def pred T() {dec ~cI1:C cI1:=C(); true } ;""", 1)>]
    [<DataRow("54c", """def cl C: obj {ctor C(x:obj) {dec base.obj ();  }} def pred T() {dec ~x:obj ~cI1:C cI1:=C(x); true } ;""", 0)>]
    [<DataRow("54b_", """def cl C1: obj {ctor C1(i1:ind) {dec ~o:ind o:=i1 base.obj ();  }} ;""", 0)>]
    [<DataRow("54c_", """def cl C1: obj {ctor C1(i1:ind) {dec ~o:ind o:=i1 base.obj ();  }} ;""", 0)>]
    [<DataRow("55", """def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI1:D dI1:=D; true } ;""", 0)>]
    [<DataRow("55a", """def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI1:D dI1:=D(); true } ;""", 0)>]
    [<DataRow("55b", """def cl B: A {intr} def cl D: B {ctor D(x:obj) {dec base.B();  }} def pred T() {dec ~dI1:D dI1:=D; true } ;""", 1)>]
    [<DataRow("55c", """def cl B: A {intr} def cl D: B {ctor D(x:obj) {dec base.B();  }} def pred T() {dec ~dI1:D dI1:=D(); true } ;""", 1)>]
    [<DataRow("56", """def pred T() {dec ~x:ind x:=$1; true } ;""", 0)>]
    [<DataRow("57", """def pred T() {dec ~x:pred x:=true; true } ;""", 0)>]
    [<DataRow("57a", """def pred T() {dec ~x:pred x:=not true; true } ;""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG04(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG04 ("",0,[""])
            ad.Clear()
            runTestHelper "TestSIG04.fpl" fplCode code expected

    [<DataRow("inh", """def cl A:obj { intr } def pred T() {dec ~n:A n:=A(); true};""", 0)>]
    [<DataRow("inh_a", """def cl A:obj { intr } def pred T() {dec ~n:obj n:=A(); true};""", 0)>]
    [<DataRow("inh_b", """def cl A:obj {intr} def cl B:A { intr } def pred T() {dec ~n:A n:=B(); true};""", 0)>]
    [<DataRow("inh_c", """def cl A:obj {intr} def cl B:A { intr } def pred T() {dec ~n:B n:=A(); true};""", 1)>]
    [<DataRow("inh_d", """def cl A:obj {intr} def cl B:A { intr } def pred T() {dec ~n:obj n:=B(); true};""", 0)>]
    [<DataRow("inh_e", """def cl A:obj {intr} def cl B:A { intr } def pred T() {dec ~n:obj n:=A(); true};""", 0)>]
    [<DataRow("inh_f", """def cl A:obj {intr} def cl B:obj { intr } def pred T() {dec ~n:B n:=A(); true};""", 1)>]
    [<DataRow("inh_g", """def cl A:obj {intr} def cl B:obj { intr } def pred T() {dec ~n:A n:=B(); true};""", 1)>]
    [<DataRow("inh_type_a", """def cl A:obj { intr } def pred T() {dec ~n:ind n:=A(); true};""", 1)>]
    [<DataRow("inh_type_b", """def cl A:obj { intr } def pred T() {dec ~n:pred n:=A(); true};""", 1)>]
    [<DataRow("inh_type_c", """def cl A:obj { intr } def pred T() {dec ~n:func n:=A(); true};""", 1)>]
    [<DataRow("constr_a", """def cl A:obj { intr } def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI2:D dI2:=D(); true};""", 0)>]
    [<DataRow("constr_b", """def cl A:obj { intr } def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI2:B dI2:=D(); true};""", 0)>]
    [<DataRow("constr_c", """def cl A:obj { intr } def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI2:A dI2:=D(); true};""", 0)>]
    [<DataRow("constr_d", """def cl A:obj { intr } def cl B: A {intr} def cl D: B {ctor D() {dec base.B();  }} def pred T() {dec ~dI2:obj dI2:=D(); true};""", 0)>]
    [<DataRow("constr_inh_a", """def cl A:obj { ctor A(x:obj) {dec base.obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:A n:=B(); true};""", 1)>]
    [<DataRow("constr_inh_b", """def cl A:obj { ctor A(x:obj) {dec base.obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:obj n:=B(); true};""", 1)>]
    [<DataRow("constr_inh_c", """def cl A:obj { ctor A(x:obj) {dec base.obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:obj n:=A(); true};""", 1)>]
    [<DataRow("constr_inh_d", """def cl A:obj { ctor A(x:obj) {dec base.obj(); } } def cl B:A { ctor B(x:pred) {dec base.A(); } } def pred T() {dec ~n:B ~x:obj n:=A(x); true};""", 1)>]
    [<DataRow("constr_inh_e", """def cl A:obj { ctor A(x:obj) {dec base.obj(); } } def cl B:obj { ctor B(x:pred) {dec base.obj(); } } def pred T() {dec ~n:B ~x:obj n:=A(x); true};""", 1)>]
    [<DataRow("constr_inh_f", """def cl A:obj { ctor A(x:obj) {dec base.obj(); } } def cl B:obj { ctor B(x:pred) {dec base.obj(); } } def pred T() {dec ~n:A ~x:pred n:=B(x); true};""", 1)>]
    [<DataRow("ass_ind_ind", """def cl C1: obj {ctor C1(i1:ind) {dec ~o:ind o:=i1 base.obj ();  }} ;""", 0)>]
    [<DataRow("ass_ind_pred", """def cl C1: obj {ctor C1(i1:pred) {dec ~o:ind o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_ind_func", """def cl C1: obj {ctor C1(i1:func) {dec ~o:ind o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_ind_obj", """def cl C1: obj {ctor C1(i1:obj) {dec ~o:ind o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_pred_ind", """def cl C1: obj {ctor C1(i1:ind) {dec ~o:pred o:=i1 base.obj ();  }} ;""", 1)>]
    [<DataRow("ass_pred_pred", """def cl C1: obj {ctor C1(i1:pred) {dec ~o:pred o:=i1 base.obj ();  }};""", 0)>]
    [<DataRow("ass_pred_func", """def cl C1: obj {ctor C1(i1:func) {dec ~o:pred o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_pred_obj", """def cl C1: obj {ctor C1(i1:obj) {dec ~o:pred o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_func_ind", """def cl C1: obj {ctor C1(i1:ind) {dec ~o:func o:=i1 base.obj ();  }} ;""", 1)>]
    [<DataRow("ass_func_pred", """def cl C1: obj {ctor C1(i1:pred) {dec ~o:func o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_func_func", """def cl C1: obj {ctor C1(i1:func) {dec ~o:func o:=i1 base.obj ();  }};""", 0)>]
    [<DataRow("ass_func_obj", """def cl C1: obj {ctor C1(i1:obj) {dec ~o:func o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_obj_ind", """def cl C1: obj {ctor C1(i1:ind) {dec ~o:obj o:=i1 base.obj ();  }} ;""", 1)>]
    [<DataRow("ass_obj_pred", """def cl C1: obj {ctor C1(i1:pred) {dec ~o:obj o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_obj_func", """def cl C1: obj {ctor C1(i1:func) {dec ~o:obj o:=i1 base.obj ();  }};""", 1)>]
    [<DataRow("ass_obj_obj", """def cl C1: obj {ctor C1(i1:obj) {dec ~o:obj o:=i1 base.obj ();  }};""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG05Assignments(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG05 ("","")
            ad.Clear()
            runTestHelper "TestSIG05.fpl" fplCode code expected

    [<DataRow("""def pred Eq(x,y: obj) infix "=" 1000 {intr} axiom A {dec ~x:ind ~y:obj; (x = y) };""", 
        "No overload matching `=(ind, obj)`. `x:ind` does not match `x:obj` in TestSIG04MsgSpecificity.Eq(obj, obj).")>]
    [<DataRow("""def func Succ(n:Nat) -> obj {intr};""", 
        "No overload matching `Nat`, no candidates were found. Are you missing a uses clause?")>]
    [<TestMethod>]
    member this.TestSIG04MsgSpecificity(fplCode:string, (expected:string)) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG04 ("",0,[""])
            prepareFplCode ("TestSIG04MsgSpecificity.fpl", fplCode, false) |> ignore
            checkForUnexpectedErrors code
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<string>(expected, result.Head.Message)

    [<DataRow("00a", "def cl A:obj { intr prty pred T() {intr} } def cl B:obj { intr prty pred T() {intr} } def cl C:A,B { intr } def pred Test() {dec ~c:C c:=C(); true} ;", 1)>]
    [<DataRow("00b", "def cl A:obj { intr prty pred T() {intr} } def cl B:obj { intr prty pred S() {intr} } def cl C:A,B { intr } def pred Test() {dec ~c:C c:=C(); true} ;", 0)>]
    [<DataRow("00c", "def cl A:obj { intr prty pred T() {intr} } def cl B:obj { intr prty pred T(x:func) {intr} } def cl C:A,B { intr } def pred Test() {dec ~c:C c:=C(); true} ;", 0)>]
    [<DataRow("01a", "def cl A:obj { intr prty func T()->obj {intr} } def cl B:obj { intr prty func T()->obj {intr} } def cl C:A,B { intr } def pred Test() {dec ~c:C c:=C(); true} ;", 1)>]
    [<DataRow("01b", "def cl A:obj { intr prty func T()->obj {intr} } def cl B:obj { intr prty pred T() {intr} } def cl C:A,B { intr } def pred Test() {dec ~c:C c:=C(); true} ;", 0)>]
    [<DataRow("01c", "def cl A:obj { intr prty func T()->obj {intr} } def cl B:obj { intr prty func T(x:ind)->obj {intr} } def cl C:A,B { intr } def pred Test() {dec ~c:C c:=C(); true} ;", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG06(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG06 ("","","", true)
            runTestHelper "TestVAR06.fpl" fplCode code expected


    [<DataRow("def predicate Test(x,y:* pred) {true};", 1)>]
    [<DataRow("def predicate Test(x,y:+ pred) {true};", 1)>]
    [<DataRow("def predicate Test(x,y: pred) {true};", 0)>]
    [<DataRow("def predicate Test(x:* pred) {true};", 0)>]
    [<DataRow("def predicate Test(x:+ pred) {true};", 0)>]
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
    [<DataRow("14", "def class A: obj {ctor A(x: obj, p:pred(u: pred)) {dec assert u;  }};", 0)>]
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
    [<DataRow("00", "def pred Test(x,x:* pred) {true};", 1)>]
    [<DataRow("00a", "def pred Test(x,x:+ pred) {true};", 1)>]
    [<DataRow("00b", "def pred Test(x,x: pred) {true};", 1)>]
    [<DataRow("00c", "def pred Test(x: pred) {true};", 0)>]
    [<DataRow("00d", "def pred Test(x:+ pred) {dec ~x:obj; true};", 1)>]
    [<DataRow("00_1", "def func Test(x,x:* pred)->obj {intr};", 1)>]
    [<DataRow("00a_1", "def func Test(x,x:+ pred)->obj {intr};", 1)>]
    [<DataRow("00b_1", "def func Test(x,x: pred)->obj {intr};", 1)>]
    [<DataRow("00c_1", "def func Test(x: pred)->obj {intr};", 0)>]
    [<DataRow("00d_1", "def func Test(x:+ pred)->obj {dec ~x:obj; return x};", 1)>]
    [<DataRow("00_2", "def cl Test:obj {dec ~x,x:* pred; ctor Test(){}};", 1)>]
    [<DataRow("00a_2", "def cl Test:obj {dec ~x,x:+ pred; ctor Test(){}};", 1)>]
    [<DataRow("00b_2", "def cl Test:obj {dec ~x,x: pred; ctor Test(){}};", 1)>]
    [<DataRow("00c_2", "def cl Test:obj {dec ~x: pred; ctor Test(){}};", 0)>]
    [<DataRow("00_3", "ax Test {dec ~x,x:* pred; true};", 1)>]
    [<DataRow("00a_3", "ax Test{dec ~x,x:+ pred; true};", 1)>]
    [<DataRow("00b_3", "ax Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_3", "ax Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_4", "thm Test {dec ~x,x:* pred; true};", 1)>]
    [<DataRow("00a_4", "thm Test{dec ~x,x:+ pred; true};", 1)>]
    [<DataRow("00b_4", "thm Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_4", "thm Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_5", "lem Test {dec ~x,x:* pred; true};", 1)>]
    [<DataRow("00a_5", "lem Test{dec ~x,x:+ pred; true};", 1)>]
    [<DataRow("00b_5", "lem Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_5", "lem Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_6", "prop Test {dec ~x,x:* pred; true};", 1)>]
    [<DataRow("00a_6", "prop Test{dec ~x,x:+ pred; true};", 1)>]
    [<DataRow("00b_6", "prop Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_6", "prop Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_7", "conj Test {dec ~x,x:* pred; true};", 1)>]
    [<DataRow("00a_7", "conj Test{dec ~x,x:+ pred; true};", 1)>]
    [<DataRow("00b_7", "conj Test{dec ~x,x: pred; true};", 1)>]
    [<DataRow("00c_7", "conj Test{dec ~x: pred; true};", 0)>]
    [<DataRow("00_8", "inf Test {dec ~x,x:* pred; pre: true con:true};", 1)>]
    [<DataRow("00a_8", "inf Test {dec ~x,x:+ pred; pre: true con:true};", 1)>]
    [<DataRow("00b_8", "inf Test {dec ~x,x: pred; pre: true con:true};", 1)>]
    [<DataRow("00c_8", "inf Test {dec ~x: pred; pre: true con:true};", 0)>]

    // properties
    [<DataRow("01a_0", "def pred Test() {true prty pred X(x,x:* pred) {true} };", 1)>]
    [<DataRow("01b_0", "def pred Test() {true prty pred X(x,x:+ pred) {true} };", 1)>]
    [<DataRow("01c_0", "def pred Test() {true prty pred X(x,x: pred) {true} };", 1)>]
    [<DataRow("01d_0", "def pred Test() {true prty pred X(x: pred) {true} };", 0)>]
    [<DataRow("01e_0", "def pred Test(x:ind) {true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("01f_0", "def pred Test() {dec ~x:ind; true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("01a_1", "def pred Test() {true prty func X(x,x:* pred)->obj {intr} };", 1)>]
    [<DataRow("01b_1", "def pred Test() {true prty func X(x,x:+ pred)->obj {intr} };", 1)>]
    [<DataRow("01c_1", "def pred Test() {true prty func X(x,x: pred)->obj {intr} };", 1)>]
    [<DataRow("01d_1", "def pred Test() {true prty func X(x: pred)->obj {intr} };", 0)>]
    [<DataRow("01e_1", "def pred Test(x:ind) {true prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("01f_1", "def pred Test() {dec ~x:ind; true prty pred X(x: pred) {intr} };", 1)>]
    [<DataRow("01a_2", "def pred Test() {true prty pred X(x,x:* pred) {true} };", 1)>]
    [<DataRow("01b_2", "def pred Test() {true prty pred X(x,x:+ pred) {true} };", 1)>]
    [<DataRow("01c_2", "def pred Test() {true prty pred X(x,x: pred) {true} };", 1)>]
    [<DataRow("01d_2", "def pred Test() {true prty pred X(x: pred) {true} };", 0)>]
    [<DataRow("01e_2", "def pred Test(x:ind) {true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("01f_2", "def pred Test() {dec ~x:ind; true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("01a_3", "def pred Test() {true prty func X(x,x:* pred)->obj {intr} };", 1)>]
    [<DataRow("01b_3", "def pred Test() {true prty func X(x,x:+ pred)->obj {intr} };", 1)>]
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
    [<DataRow("03c", "ext Digits x@/\d+/ -> Nat {dec ~x:+obj; return x};", 1)>]
    [<DataRow("03d", "ext Digits x@/\d+/ -> Nat {dec ~x:*ind; return x};", 1)>]

    [<DataRow("04", "inf ModusPonens {dec ~p,q: pred; pre: and (p, impl (p,q) ) con: q};", 0)>]
    [<DataRow("05", "def pred Test() {true prty pred X(y:+ pred) {dec ~x:obj; ex x:obj {true}} };", 1)>]
    [<DataRow("06", "def pred Test() {true prty pred X(y:+ pred) {ex x:obj {true}} };", 0)>]
    [<DataRow("07", "def pred Test() {true prty func X(y:+ pred)->obj {dec ~x:obj; return x} };", 0)>]
    [<DataRow("09", "def pred Test(x: ind) {true prty pred X(x: pred) {true} };", 1)>]
    [<DataRow("09a", "def pred Test(x: ind) {true prty pred X(x:* pred) {true} };", 1)>]
    [<DataRow("09b", "def pred Test(x: ind) {true prty pred X(x:+ pred) {true} };", 1)>]
    [<DataRow("09c", "def pred Test(x: ind) {true prty pred X() {dec ~x: obj; true} };", 1)>]
    [<DataRow("09d", "def pred Test(x: ind) {true prty pred X() {dec ~x:* obj; true} };", 1)>]
    [<DataRow("09e", "def pred Test(x: ind) {true prty pred X() {dec ~x:+ obj; true} };", 1)>]
    [<DataRow("09f", "def pred Test(x: ind) {true prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("09g", "def pred Test(x: ind) {true prty func X(x:* pred)->obj {intr} };", 1)>]
    [<DataRow("09h", "def pred Test(x: ind) {true prty func X(x:+ pred)->obj {intr} };", 1)>]
    [<DataRow("09i", "def pred Test(x: ind) {true prty func X()->obj {dec ~x: obj; return x} };", 1)>]
    [<DataRow("09j", "def pred Test(x: ind) {true prty func X()->obj {dec ~x:* obj; return x} };", 1)>]
    [<DataRow("09k", "def pred Test(x: ind) {true prty func X()->obj {dec ~x:+ obj; return x} };", 1)>]
    [<DataRow("10", "def cl Test:obj {dec ~x:ind; ctor Test(x: pred) {} };", 1)>]
    [<DataRow("10a", "def cl Test:obj {dec ~x:ind; ctor Test(x:* pred) {} };", 1)>]
    [<DataRow("10b", "def cl Test:obj {dec ~x:ind; ctor Test(x:+ pred) {} };", 1)>]
    [<DataRow("10c", "def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x: obj; } };", 1)>]
    [<DataRow("10d", "def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x:* obj; } };", 1)>]
    [<DataRow("10e", "def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x:+ obj; } };", 1)>]
    [<DataRow("11", "def cl Test:obj {dec ~x:obj; constructor Test() {} prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("11a", "def cl Test:obj {dec ~x:obj; constructor Test(x: pred) {} prty func X()->obj {intr} };", 1)>]
    [<DataRow("11b", "def cl Test:obj {dec ~x:obj; constructor Test() {dec ~x: pred; } prty func X()->obj {intr} };", 1)>]
    [<DataRow("11c", "def cl Test:obj {dec ~x:obj; constructor Test() {} prty func X()->obj {dec ~x: pred; return x} };", 1)>]
    [<DataRow("12", "inf ExistsByExample {dec ~p: pred(c: obj) ~x: obj; pre: p(c) con: ex x:obj {p(x)}};", 1)>]
    [<DataRow("12a", "inf ExistsByExample {dec ~p: pred(c: obj); pre: p(c) con: ex x:obj {p(x)}};", 0)>]
    [<DataRow("12b", "inf ExistsByExample {dec ~p: pred(c: obj) ~c: obj; pre: true con: true};", 1)>]
    [<DataRow("13", """loc and(p,q) := !tex: p "\wedge" q;;""", 0)>]
    [<DataRow("14", """def cl B:obj {intr} def cl A:obj {dec ~x:obj; ctor A(y:B) {} };""", 1)>]
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
    [<DataRow("03", "def cl T:obj {dec ~x:obj; ctor T() {}};", 1)>]
    [<DataRow("03a", "def cl T:obj {dec ~x:obj; ctor T() {dec x:=x; }};", 0)>]
    [<DataRow("03b", "def cl T:obj {dec ~x:obj; ctor T() {dec y:=y; }};", 1)>]
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

    [<DataRow("00a", "def cl T:obj { dec ~x:obj; ctor T() { dec base.obj() ; }} def cl S:T { dec ~x:obj; ctor S() { dec base.T() ; }} ;", 1)>]
    [<DataRow("00b", "def cl T:obj { dec ~x:obj; ctor T() { dec base.obj() ; }} def cl S:T { dec ~y:obj; ctor S() { dec base.T() ; }} ;", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR06(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR06 ("","","", true)
            runTestHelper "TestVAR06.fpl" fplCode code expected
            
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
    [<DataRow("00a", "axiom T {all n:*pred { n } };", 1)>]
    [<DataRow("00b", "axiom T {all n:+pred { n } };", 1)>]
    [<DataRow("01", "axiom T {ex n:pred { n } };", 0)>]
    [<DataRow("01a", "axiom T {ex n:*pred { n } };", 1)>]
    [<DataRow("01b", "axiom T {ex n:+pred { n } };", 1)>]
    [<DataRow("02", "axiom T {exn$1 n:pred { n } };", 0)>]
    [<DataRow("02a", "axiom T {exn$1 n:*pred { n } };", 1)>]
    [<DataRow("02a", "axiom T {exn$1 n:+pred { n } };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR08(no:string, fplCode:string, expected) =
        if TestConfig.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR08
            runTestHelper "TestVAR08.fpl" fplCode code expected