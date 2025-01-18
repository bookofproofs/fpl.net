namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

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
            let st = SymbolTable(parsedAsts, true)
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
            let st = SymbolTable(parsedAsts, true)
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
            let st = SymbolTable(parsedAsts, true)
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
            let st = SymbolTable(parsedAsts, true)
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
            let st = SymbolTable(parsedAsts, true)
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
            let st = SymbolTable(parsedAsts, true)
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
        let st = SymbolTable(parsedAsts, true)
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
        let st = SymbolTable(parsedAsts, true)
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

    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits x@/\d+/ -> X {ret x};", 1)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} ;", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} def pred S() {true} ;", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T() {true} ext Digits1 x@/\d+/ -> X {ret x} def pred T() {@1} ;", 1)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T(x:@Digits) {true};", 0)>]
    [<DataRow(@"ext Digits x@/\d+/ -> X {ret x} def pred T(x:@Typo) {true};", 0)>]

    [<DataRow("def func Sum(list:* Nat)->Nat {dec ~result: Nat; return result} def func Sum2(list:* Nat)->Nat {dec ~result: Nat; return result};", 0)>]
    [<DataRow("""def cl B:obj {intr} def cl A:obj {dec ~x:obj; ctor A(y:B[x:obj]) {self} };""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID001(fplCode:string, expected:int) =
        let code = ID001 ("", "")
        runTestHelper "TestID001.fpl" fplCode code expected

    [<DataRow("""loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;;""", 0)>]
    [<DataRow("""loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !tex: x " dann und nur dann wenn " y;;""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID014(fplCode:string, expected:int) =
        let code = ID014 ("", "")
        runTestHelper "TestID014.fpl" fplCode code expected

    [<DataRow("00", """def cl A:obj {ctor A() {self}};""", 0)>]
    [<DataRow("00a", """def cl A:obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("00b", """def cl A:obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("00c", """def cl A:obj {ctor A() {self}};""", 0)>]
    [<DataRow("01", """def cl A:obj {dec assert is(parent,A); ctor A() {self}};""", 1)>]
    [<DataRow("02", """def cl A:obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("03", """def cl A:obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("04", """def pred A() {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("04a", """def pred A() {dec assert is(parent,A); true};""", 1)>]
    [<DataRow("04b", """def pred A() {dec assert is(self,A); true};""", 0)>]
    [<DataRow("05", """def pred A() {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("06", """def func A()->obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("06a", """def func A()->obj {dec ~x:obj assert is(parent,A); return x};""", 1)>]
    [<DataRow("06b", """def func A()->obj {dec ~x:obj assert is(self,A); return x};""", 0)>]
    [<DataRow("07", """def func A()->obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("08", """axiom A() {self};""", 1)>]
    [<DataRow("09", """axiom A() {parent};""", 1)>]
    [<DataRow("10", """theorem A() {self};""", 1)>]
    [<DataRow("11", """theorem A() {parent};""", 1)>]
    [<DataRow("12", """lemma A() {self};""", 1)>]
    [<DataRow("13", """lemma A() {parent};""", 1)>]
    [<DataRow("14", """prop A() {self};""", 1)>]
    [<DataRow("15", """prop A() {parent};""", 1)>]
    [<DataRow("16", """conj A() {self};""", 1)>]
    [<DataRow("17", """conj A() {parent};""", 1)>]
    [<DataRow("18", """cor A$1() {self};""", 1)>]
    [<DataRow("19", """cor A$1() {parent};""", 1)>]
    [<DataRow("20", """prf A$1 {1. |- self qed};""", 1)>]
    [<DataRow("21", """prf A$1 {1. |- parent qed};""", 1)>]
    [<DataRow("22", """inf A() {pre: true con: parent};""", 1)>]
    [<DataRow("23", """inf A() {pre: true con: self};""", 1)>]
    [<DataRow("24", """inf A() {pre: self con: true};""", 1)>]
    [<DataRow("25", """inf A() {pre: parent con: true};""", 1)>]
    [<DataRow("26", """loc not(self) := !tex: "\neg(" x ")";;""", 1)>]
    [<DataRow("27", """loc not(parent) := !tex: "\neg(" x ")";;""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID015(no:string, fplCode:string, expected:int) =
        let code = ID015 ""
        runTestHelper "TestID015.fpl" fplCode code expected

    [<DataRow("00", """def cl A:obj {ctor A() {self}};""", 0)>]
    [<DataRow("01", """def cl A:obj {dec assert is(parent,A); ctor A() {self}};""", 0)>]
    [<DataRow("02", """def cl A:obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("03", """def cl A:obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("04", """def pred A() {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("04a", """def pred A() {dec assert is(parent,A); true};""", 0)>]
    [<DataRow("04b", """def pred A() {dec assert is(self,A); true};""", 0)>]
    [<DataRow("05", """def pred A() {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("06", """def func A()->obj {intr property pred T() { is(parent,A) } };""", 0)>]
    [<DataRow("06a", """def func A()->obj {dec ~x:obj assert is(parent,A); return x};""", 0)>]
    [<DataRow("06b", """def func A()->obj {dec ~x:obj assert is(self,A); return x};""", 0)>]
    [<DataRow("07", """def func A()->obj {intr property pred T() { is(self,A) } };""", 0)>]
    [<DataRow("08", """axiom A() {self};""", 0)>]
    [<DataRow("09", """axiom A() {parent};""", 0)>]
    [<DataRow("10", """theorem A() {self};""", 0)>]
    [<DataRow("11", """theorem A() {parent};""", 0)>]
    [<DataRow("12", """lemma A() {self};""", 0)>]
    [<DataRow("13", """lemma A() {parent};""", 0)>]
    [<DataRow("14", """prop A() {self};""", 0)>]
    [<DataRow("15", """prop A() {parent};""", 0)>]
    [<DataRow("16", """conj A() {self};""", 0)>]
    [<DataRow("17", """conj A() {parent};""", 0)>]
    [<DataRow("18", """cor A$1() {self};""", 0)>]
    [<DataRow("19", """cor A$1() {parent};""", 0)>]
    [<DataRow("20", """prf A$1 {1. |- self qed};""", 0)>]
    [<DataRow("21", """prf A$1 {1. |- parent qed};""", 0)>]
    [<DataRow("22", """inf A() {pre: true con: parent};""", 0)>]
    [<DataRow("23", """inf A() {pre: true con: self};""", 0)>]
    [<DataRow("24", """inf A() {pre: self con: true};""", 0)>]
    [<DataRow("25", """inf A() {pre: parent con: true};""", 0)>]
    [<DataRow("26", """loc not(self) := !tex: "\neg(" x ")";;""", 0)>]
    [<DataRow("27", """loc not(parent) := !tex: "\neg(" x ")";;""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID016(no: string, fplCode:string, expected:int) =
        let code = ID016 ""
        runTestHelper "TestID016.fpl" fplCode code expected

    [<DataRow("""proof T$1 { 100. |- assume somePremise 300. |- trivial 100. |- trivial qed};""", 1)>]
    [<DataRow("""proof T$1 { 1. |- trivial 1. |- trivial qed};""", 1)>]
    [<DataRow("""proof T$1 { 1. |- trivial 2. |- trivial qed};""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR003(fplCode:string, expected:int) =
        let code = PR003 ("", "")
        runTestHelper "TestPR003.fpl" fplCode code expected

    [<DataRow("""proof T$1 { 1. |- trivial qed};""", 0)>]
    [<DataRow("""proof T$1 { 1. 2., 3. |- trivial qed};""", 0)>]
    [<DataRow("""proof T$1 { 1. 1., 1. |- trivial qed};""", 1)>]
    [<DataRow("""proof T$1 { 1. 1., 1., 1. |- trivial qed};""", 2)>]
    [<DataRow("""proof T$1 { 1. 1., 2., 1. |- trivial qed};""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR004(fplCode:string, expected:int) =
        let code = PR004 ("", "")
        runTestHelper "TestPR004.fpl" fplCode code expected


    [<DataRow("""proof T$1 { 1. |- trivial qed};""", 0)>]
    [<DataRow("""proof T$1 { 1. 2., 3. |- trivial qed};""", 2)>]
    [<DataRow("""proof T$1 { 1. |- trivial 2. 1. |- trivial qed};""", 0)>]
    [<DataRow("""proof T$1 { 1. 1., 1., 1. |- trivial qed};""", 3)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR005(fplCode:string, expected:int) =
        let code = PR005 ""
        runTestHelper "TestPR005.fpl" fplCode code expected

    [<DataRow("""def pred Equal infix "=" 50 (x,y: tpl) {intr} ;""", 0)>]
    [<DataRow("uses Fpl.Commons inf ModusPonens() {pre:true con:true} ;", 1)>]
    [<DataRow("uses Fpl.Commons theorem ModusTollens() {true} ;", 1)>]
    [<DataRow("uses Fpl.Commons def pred HypotheticalSyllogism() {true} ;", 1)>]
    [<DataRow("uses Fpl.Commons axiom DisjunctiveSyllogism() {true} ;", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID001ConflictWithOtherTheories(fplCode:string, expected:int) =
        let code = ID001 ("", "")
        runTestHelper "TestID001ConflictWithOtherTheories.fpl" fplCode code expected

    [<DataRow("def predicate Test(x,y:* pred) {true};", 1)>]
    [<DataRow("def predicate Test(x,y:+ pred) {true};", 1)>]
    [<DataRow("def predicate Test(x,y: pred) {true};", 0)>]
    [<DataRow("def predicate Test(x:* pred) {true};", 0)>]
    [<DataRow("def predicate Test(x:+ pred) {true};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR00(fplCode:string, expected) =
        let code = VAR00
        runTestHelper "TestVAR00.fpl" fplCode code expected


    [<DataRow("def pred Test() {x};", 1)>]
    [<DataRow("inf ExistsByExample(p: pred(c: obj)) {pre: p(c) con: ex x:obj {p(x)}};", 0)>]
    [<DataRow("axiom A() { all x:Nat {true} };", 0)>]
    [<DataRow("axiom A() { all x:obj {y} };", 1)>]
    [<DataRow("axiom A() { dec ~x:obj; true };", 0)>]
    [<DataRow("axiom A() { dec ~x:obj; true };", 0)>]
    [<DataRow("""loc and(p,q) := !tex: p "\wedge" q;;""", 0)>]
    [<DataRow("""loc and(p,q) := !tex: x "\wedge" q;;""", 1)>]
    [<DataRow("""loc and(p,q) := !tex: x "\wedge" y;;""", 2)>]
    [<DataRow("""def pred Add infix "+" 2 (x,y: obj) {intr} loc (x + y) := !tex: x "+" y !eng: x "plus" y !ger: x "plus" y;;""", 0)>]
    [<DataRow("""def pred Add infix "+" 2 (x,y: obj) {intr} axiom A() {(x + y * z = 1)};""", 3)>]
    [<DataRow("axiom A(arr: tpl[x:pred]) { x };", 0)>]
    [<DataRow("prop A(d:pred) {true} proof A$1 {1. |- d qed};", 0)>]
    [<DataRow("prop A(d:pred) {true} cor A$1() { d };", 0)>]
    [<DataRow("def class A: obj {ctor A(x: obj, p:obj(u: pred)) {dec assert u; self }};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR01(fplCode:string, expected) =
        let code = VAR01 ""
        runTestHelper "TestVAR01.fpl" fplCode code expected

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
    [<DataRow("03", "def cl T:obj {dec ~x:obj; ctor T() {self}};", 1)>]
    [<DataRow("03a", "def cl T:obj {dec ~x:obj; ctor T() {dec x:=x; self}};", 0)>]
    [<DataRow("03b", "def cl T:obj {dec ~x:obj; ctor T() {dec y:=y; self}};", 1)>]
    [<DataRow("04", "thm T() {dec ~x:obj; true};", 1)>]
    [<DataRow("04a", "thm T() {dec ~x:obj; true} prf T$1 {1. |- x qed};", 1)>]
    [<DataRow("04b", "thm T() {dec ~x:obj; true} prf T$1 {1. |- trivial qed};", 1)>]
    [<DataRow("04c", "thm T() {dec ~x:obj; x};", 0)>]
    [<DataRow("04d", "thm T(x:pred) {x};", 0)>]
    [<DataRow("04e", "thm T() {dec ~x:obj; y};", 1)>]
    [<DataRow("04f", "thm T(x:pred) {y};", 1)>]
    [<DataRow("05", "prop T() {dec ~x:obj; true};", 1)>]
    [<DataRow("05a", "prop T() {dec ~x:obj; true} prf T$1 {1. |- x qed};", 1)>]
    [<DataRow("05b", "prop T() {dec ~x:obj; true} prf T$1 {1. |- trivial qed};", 1)>]
    [<DataRow("05c", "prop T() {dec ~x:obj; x};", 0)>]
    [<DataRow("05d", "prop T(x:pred) {x};", 0)>]
    [<DataRow("05e", "prop T() {dec ~x:obj; y};", 1)>]
    [<DataRow("05f", "prop T(x:pred) {y};", 1)>]
    [<DataRow("06", "lem T() {dec ~x:obj; true};", 1)>]
    [<DataRow("06a", "lem T() {dec ~x:obj; true} prf T$1 {1. |- x qed};", 1)>]
    [<DataRow("06b", "lem T() {dec ~x:obj; true} prf T$1 {1. |- trivial qed};", 1)>]
    [<DataRow("06c", "lem T() {dec ~x:obj; x};", 0)>]
    [<DataRow("06d", "lem T(x:pred) {x};", 0)>]
    [<DataRow("06e", "lem T() {dec ~x:obj; y};", 1)>]
    [<DataRow("06f", "lem T(x:pred) {y};", 1)>]
    [<DataRow("06", "ax T() {dec ~x:obj; true};", 1)>]
    [<DataRow("06a", "ax T() {dec ~x:obj; x};", 0)>]
    [<DataRow("06b", "ax T(x:pred) {x};", 0)>]
    [<DataRow("06c", "ax T() {dec ~x:obj; y};", 1)>]
    [<DataRow("06d", "ax T(x:pred) {y};", 1)>]
    [<DataRow("07", "conj T() {dec ~x:obj; true};", 1)>]
    [<DataRow("07a", "conj T() {dec ~x:obj; x};", 0)>]
    [<DataRow("07b", "conj T(x:pred) {x};", 0)>]
    [<DataRow("07c", "conj T() {dec ~x:obj; y};", 1)>]
    [<DataRow("07d", "conj T(x:pred) {y};", 1)>]
    [<DataRow("08", "cor T$1() {dec ~x:obj; true};", 1)>]
    [<DataRow("08a", "cor T$1() {dec ~x:obj; true} prf T$1 {1. |- x qed};", 1)>]
    [<DataRow("08b", "cor T$1() {dec ~x:obj; true} prf T$1 {1. |- trivial qed};", 1)>]
    [<DataRow("08c", "cor T$1() {dec ~x:obj; x};", 0)>]
    [<DataRow("08d", "cor T$1(x:pred) {x};", 0)>]
    [<DataRow("08e", "cor T$1() {dec ~x:obj; y};", 1)>]
    [<DataRow("08f", "cor T$1(x:pred) {y};", 1)>]
    [<DataRow("09", "inf T(x:pred) {pre: true con:true};", 1)>]
    [<DataRow("09a", "inf T(x:pred) {pre: true con:x};", 0)>]
    [<DataRow("09b", "inf T(x:pred) {pre: x con:true};", 0)>]
    [<DataRow("09c", "inf T(x:pred) {pre: true con:y};", 1)>]
    [<DataRow("09d", "inf T(x:pred) {pre: y con:true};", 1)>]
    [<DataRow("09e", "inf T() {dec ~x:pred; pre: true con:true};", 1)>]
    [<DataRow("09f", "inf T() {dec ~x:pred; pre: true con:x};", 0)>]
    [<DataRow("09g", "inf T() {dec ~x:pred; pre: x con:true};", 0)>]
    [<DataRow("09h", "inf T() {dec ~x:pred; pre: true con:y};", 1)>]
    [<DataRow("09i", "inf T() {dec ~x:pred; pre: y con:true};", 1)>]
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
    [<DataRow("22", "axiom T(p:pred(n:obj)) {all n:Nat{p(n)} };", 0)>]
    [<DataRow("23", "axiom T(p:pred(n:obj)) {p(@0)};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR04(no:string, fplCode:string, expected) =
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
        let code = VAR05 ""
        runTestHelper "TestVAR05.fpl" fplCode code expected

    [<DataRow("00", "def cl T:obj { dec ~x:obj; ctor T() { dec base.obj() ; self}} def cl S:T { dec ~x:obj; ctor S() { dec base.T() ; self}} ;", 1)>]
    [<DataRow("01", "def cl T:obj { dec ~x:obj; ctor T() { dec base.obj() ; self}} def cl S:T { dec ~y:obj; ctor S() { dec base.T() ; self}} ;", 0)>]
    [<DataRow("02", "def cl T:Stypo { dec ~x:obj; ctor T() { dec base.Stypo() ; self}} ;", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR06(no:string, fplCode:string, expected) =
        let code = VAR06 ("","")
        runTestHelper "TestVAR06.fpl" fplCode code expected

    [<DataRow("00", "def pred Test(x,x:* pred) {true};", 1)>]
    [<DataRow("00a", "def pred Test(x,x:+ pred) {true};", 1)>]
    [<DataRow("00b", "def pred Test(x,x: pred) {true};", 1)>]
    [<DataRow("00c", "def pred Test(x: pred) {true};", 0)>]
    [<DataRow("00d", "def pred Test(x:+ pred) {dec ~x:obj; true};", 1)>]

    [<DataRow("01", "def pred Test() {true prty pred X(x,x:* pred) {true} };", 1)>]
    [<DataRow("01a", "def pred Test() {true prty pred X(x,x:+ pred) {true} };", 1)>]
    [<DataRow("01b", "def pred Test() {true prty pred X(x,x: pred) {true} };", 1)>]
    [<DataRow("01c", "def pred Test() {true prty pred X(x: pred) {true} };", 0)>]
    [<DataRow("02", "def pred Test() {true prty pred X(x:+ pred) {dec ~x:obj; true} };", 1)>]
    [<DataRow("02a", "def pred Test() {true prty func X(x,x:* pred)->obj {intr} };", 1)>]
    [<DataRow("02b", "def pred Test() {true prty func X(x,x:+ pred)->obj {intr} };", 1)>]
    [<DataRow("02c", "def pred Test() {true prty func X(x,x: pred)->obj {intr} };", 1)>]
    [<DataRow("02d", "def pred Test() {true prty func X(x: pred)->obj {intr} };", 0)>]
    [<DataRow("03", "def pred Test() {true prty func X(x:+ pred)->obj {dec ~x:obj; return x} };", 1)>]
    [<DataRow("04", "inf ModusPonens() {dec ~p,q: pred; pre: and (p, impl (p,q) ) con: q};", 0)>]
    [<DataRow("05", "def pred Test() {true prty pred X(y:+ pred) {dec ~x:obj; ex x:obj {true}} };", 1)>]
    [<DataRow("06", "def pred Test() {true prty pred X(y:+ pred) {ex x:obj {true}} };", 0)>]
    [<DataRow("07", "def pred Test() {true prty func X(y:+ pred)->obj {dec ~x:obj; return x} };", 0)>]
    [<DataRow("08", "theorem TestId(x: ind) {true}       proof TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("08a", "theorem TestId() {dec ~x:ind; true} proof TestId$1 { dec ~x:obj; 1. |- trivial };", 1)>]
    [<DataRow("08b", "theorem TestId(x: ind) {true}       proof TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("08c", "theorem TestId() {dec ~x:ind; true} proof TestId$1 { 1. |- trivial };", 0)>]
    [<DataRow("08d", "theorem TestId(x: ind) {true}       corollary TestId$1() { dec ~x:obj; true };", 1)>]
    [<DataRow("08e", "theorem TestId() {dec ~x:ind; true} corollary TestId$1() { dec ~x:obj; true };", 1)>]
    [<DataRow("08f", "theorem TestId(x: ind) {true}       corollary TestId$1() { true };", 0)>]
    [<DataRow("08g", "theorem TestId() {dec ~x:ind; true} corollary TestId$1() { true };", 0)>]
    [<DataRow("08h", "theorem TestId(x: ind) {true}       corollary TestId$1(x:obj) { true };", 1)>]
    [<DataRow("08i", "theorem TestId(x: ind) {true}       corollary TestId$1() { true }       proof TestId$1$1   { dec ~x:obj; 1. |- trivial } ;", 1)>]
    [<DataRow("08j", "theorem TestId(x: ind) {true}       corollary TestId$1() { true }   corollary TestId$1$1() { dec ~x:obj; true } ;", 1)>]
    [<DataRow("08k", "theorem TestId(x: ind) {true}       corollary TestId$1() { true }   corollary TestId$1$1(x:obj) { true } ;", 1)>]
    [<DataRow("08l", "theorem TestId() {dec ~x:ind; true} corollary TestId$1(x:obj) { true };", 1)>]
    [<DataRow("08m", "theorem TestId(x: ind) {true}       corollary TestId$1() { true };", 0)>]
    [<DataRow("08n", "theorem TestId() {dec ~x:ind; true} corollary TestId$1() { true };", 0)>]
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
    [<DataRow("10", "def cl Test:obj {dec ~x:ind; ctor Test(x: pred) {self} };", 1)>]
    [<DataRow("10a", "def cl Test:obj {dec ~x:ind; ctor Test(x:* pred) {self} };", 1)>]
    [<DataRow("10b", "def cl Test:obj {dec ~x:ind; ctor Test(x:+ pred) {self} };", 1)>]
    [<DataRow("10c", "def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x: obj; self} };", 1)>]
    [<DataRow("10d", "def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x:* obj; self} };", 1)>]
    [<DataRow("10e", "def cl Test:obj {dec ~x:ind; ctor Test() {dec ~x:+ obj; self} };", 1)>]
    [<DataRow("11", "def cl Test:obj {dec ~x:obj; constructor Test() {self} prty func X(x: pred)->obj {intr} };", 1)>]
    [<DataRow("11a", "def cl Test:obj {dec ~x:obj; constructor Test(x: pred) {self} prty func X()->obj {intr} };", 1)>]
    [<DataRow("11b", "def cl Test:obj {dec ~x:obj; constructor Test() {dec ~x: pred; self} prty func X()->obj {intr} };", 1)>]
    [<DataRow("11c", "def cl Test:obj {dec ~x:obj; constructor Test() {self} prty func X()->obj {dec ~x: pred; return x} };", 1)>]
    [<DataRow("12", "inf ExistsByExample(p: pred(c: obj)) {dec ~x: obj; pre: p(c) con: ex x:obj {p(x)}};", 1)>]
    [<DataRow("12a", "inf ExistsByExample(p: pred(c: obj)) {pre: p(c) con: ex x:obj {p(x)}};", 0)>]
    [<DataRow("12b", "inf ExistsByExample(p: pred(c: obj)) {dec ~c: obj; pre: true con: true};", 1)>]
    [<DataRow("13", """loc and(p,q) := !tex: p "\wedge" q;;""", 0)>]
    [<DataRow("14", """def cl B:obj {intr} def cl A:obj {dec ~x:obj; ctor A(y:B[x:obj]) {self} };""", 1)>]
    [<DataRow("15", "axiom T(p:pred(n:obj)) {all n:Nat{p(n)} };", 1)>]
    [<DataRow("15a", "axiom T(p:pred(n:obj)) {all n1:Nat{p(n1)} };", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestVAR03(no: string, fplCode:string, expected) =
        let code = VAR03 ("", "")
        runTestHelper "TestVAR03.fpl" fplCode code expected

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
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID002(fplCode:string, expected) =
        let code = ID002 ("","")
        runTestHelper "TestID002.fpl" fplCode code expected

    [<DataRow("def pred Test() {true} corollary Test$1() {true};", 1)>]
    [<DataRow("def cl Test:obj {intr} corollary Test$1() {true};", 1)>]
    [<DataRow("def func Test()->obj {intr} corollary Test$1() {true};", 1)>]
    [<DataRow("proof Test$1 {1. |- trivial} corollary Test$1$1() {true};", 1)>] // corollaries of proofs are not allowed
    [<DataRow("theorem Test() {true} corollary Test$1() {true};", 0)>]
    [<DataRow("lemma Test() {true} corollary Test$1() {true};", 0)>]
    [<DataRow("proposition Test() {true} corollary Test$1() {true};", 0)>]
    [<DataRow("axiom Test() {true} corollary Test$1() {true};", 0)>] // corollaries of axioms are allowed
    [<DataRow("postulate Test() {true} corollary Test$1() {true};", 0)>] // corollaries of postulates (axioms) not allowed
    [<DataRow("conjecture Test() {true} corollary Test$1() {true};", 0)>] // corollaries of conjectures are not allowed
    [<DataRow("corollary Test$1() {true} corollary Test$1$1() {true};", 0)>] // corollaries of corollaries are allowed
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID005(fplCode:string, expected) =
        let code = ID005 ("","")
        runTestHelper "TestID005.fpl" fplCode code expected

    [<DataRow("theorem Test() {true} proof Test$1 {1. |- trivial};", 0)>]
    [<DataRow("theorem TestTypo() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("corollary Test$1() {true} proof Test$1$1 {1. |- trivial};", 0)>]
    [<DataRow("theorem Test() {true} corollary Test$1() {true} proof Test$1$1 {1. |- trivial};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID003(fplCode:string, expected) =
        let code = ID003 ""
        runTestHelper "TestID003.fpl" fplCode code expected

    [<DataRow("theorem Test() {true} corollary Test$1() {true};", 0)>]
    [<DataRow("theorem TestTypo() {true} corollary Test$1() {true};", 1)>]
    [<DataRow("theorem Test() {true} corollary Test$1() {true} corollary Test$1$1() {true};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID006(fplCode:string, expected) =
        let code = ID006 ""
        runTestHelper "TestID006.fpl" fplCode code expected

    [<DataRow("theorem Test() {true} lemma Test(x:obj) {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("theorem Test(x:ind) {true} theorem Test() {true} proof Test$1 {1. |- trivial};", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID004(fplCode:string, expected) =
        let code = ID004 ("", "") 
        runTestHelper "TestID004.fpl" fplCode code expected

    [<DataRow("theorem Test() {true} lemma Test(x:obj) {true} corollary Test$1() {true};", 1)>]
    [<DataRow("theorem Test(x:ind) {true} theorem Test() {true} corollary Test$1() {true};", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID007(fplCode:string, expected) =
        let code = ID007 ("", "") 
        runTestHelper "TestID007.fpl" fplCode code expected

    [<DataRow("def cl Test:obj {ctor TestTypo(x:Nat) {self}};", 1)>]
    [<DataRow("def cl Test:obj {ctor TestTypo1() {self}};", 1)>]
    [<DataRow("def cl Test:obj {ctor Test() {self}};", 0)>]
    [<DataRow("def cl Test:obj {dec ~x:obj x := 0; ctor Test() {dec base.obj(); self}};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID008(fplCode:string, expected) =
        let code = ID008 ("", "") 
        runTestHelper "TestID008.fpl" fplCode code expected

    [<DataRow("def cl Test:obj {intr};", 0)>]
    [<DataRow("def cl Test:Test {intr};", 1)>]
    [<DataRow("def cl Test:Test1, Test2, Test3 {intr};", 0)>]
    [<DataRow("def cl Test:Test1, Test2, Test3, Test {intr};", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID009(fplCode:string, expected) =
        let code = ID009 ""
        runTestHelper "TestID009.fpl" fplCode code expected

    [<DataRow("def cl Test:obj {intr};", 0)>]
    [<DataRow("def cl Test:Set {intr};", 1)>]
    [<DataRow("def class Set: obj {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("def cl Set:obj {intr} def cl Test:Set {intr};", 0)>]
    [<DataRow("def cl Set:obj {intr} def cl Test:SetTypo {intr};", 1)>]
    [<DataRow("def cl Set:obj {intr} def pred Test() {dec ~x:Set; true};", 0)>]
    [<DataRow("def cl Set:obj {intr} def pred Test() {dec ~x:object; is(x,Set)};", 0)>]
    // the following examples should not emit ID010 because this context is covered by the SIG04 diagnostics
    [<DataRow("def pred Test(x:Set) {intr};", 0)>]
    [<DataRow("def class Set: obj {intr} def pred IsEmpty(x: Set) {true};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID010(fplCode:string, expected) =
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
        let code = ID011 ("","")
        runTestHelper "TestID011.fpl" fplCode code expected

    [<DataRow("def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C,E {ctor D() {dec base.A() base.B() base.C() base.F(); self} };", 1)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {ctor B() {dec base.A(); self} };", 0)>]
    [<DataRow("def cl A:obj {intr} def cl B:A {ctor B() {dec base.C(); self} };", 1)>]
    [<DataRow("def cl A:obj { ctor A() {dec base.obj(); self} };", 0)>]
    [<DataRow("def cl A:obj { ctor A() {dec base.B(); self} };", 1)>]
    [<DataRow("def cl A:C { ctor A() {dec base.obj(); self} };", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.obj(); self} };", 1)>]
    [<DataRow("uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); self} };", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID012(fplCode:string, expected) =
        let code = ID012 ("","")
        runTestHelper "TestID012.fpl" fplCode code expected

    [<DataRow("def pred A() {true} def pred A(x:obj) {true} def pred T(x:A) {intr};", 1)>]
    [<DataRow("def pred A() {true} def func A(x:obj)->obj {intr} def pred T(x:A) {intr};", 1)>]
    [<DataRow("def pred A() {true} def func A()->obj {intr} def pred T(x:A) {intr};", 1)>]
    [<DataRow("def pred A() {true} def pred T(x:A) {intr};", 0)>]
    [<DataRow("def func A(x:obj)->obj {intr} def pred T(x:A) {intr};", 0)>]
    [<DataRow("def pred B() {true} def func A()->obj {intr} def pred T(x:A) {intr};", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID017(fplCode:string, expected) =
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
        let code = ID018 ""
        runTestHelper "TestID018.fpl" fplCode code expected

    [<DataRow("00", @"ext Digits x @ /\d+/ def pred T(x:@Digits) {true};", 0)>]
    [<DataRow("01", @"ext Digits x @ /\d+/ def pred T(x:@Typo) {true};", 1)>]
    [<DataRow("01", @"ext Digits x @ /\d+/ def pred T(x:tpl) {true};", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID019(no:string, fplCode:string, expected) =
        let code = ID019 ""
        runTestHelper "TestID019.fpl" fplCode code expected


    [<DataRow("00", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.A(); self} };", 2)>]
    [<DataRow("00a", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.B(); self} };", 2)>]
    [<DataRow("00b", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.C(); self} };", 2)>]
    [<DataRow("00c", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.A() base.B(); self} };", 1)>]
    [<DataRow("00d", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.A() base.C(); self} };", 1)>]
    [<DataRow("00e", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.B() base.C(); self} };", 1)>]
    [<DataRow("00f", "def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {ctor D() {dec base.A() base.B() base.C(); self} };", 0)>]
    [<DataRow("01", "def cl A:obj {intr} def cl B:A {ctor B() {dec base.A(); self} };", 0)>]
    [<DataRow("01a", "def cl A:obj {intr} def cl B:A {ctor B() {self} };", 1)>]
    [<DataRow("02", "def cl A:obj { ctor A() {dec base.obj(); self} };", 0)>]
    [<DataRow("02a", "def cl A:obj { ctor A() {self} };", 1)>]
    [<DataRow("03", "def cl A:obj { ctor A() {dec base.obj(); self} };", 0)>]
    [<DataRow("03a", "def cl A:C { ctor A() {dec base.obj(); self} };", 1)>]
    [<DataRow("03b", "def cl A:obj {intr} def cl B:A {intr} def cl C:B { ctor C() {dec base.obj(); self} };", 1)>]
    [<DataRow("04", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.obj(); self} };", 1)>]
    [<DataRow("04a", "uses Fpl.SetTheory def cl Test:Set {ctor Test() {dec base.Set(); self} };", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID020(no:string, fplCode:string, expected) =
        let code = ID020 ""
        runTestHelper "TestID020.fpl" fplCode code expected

    [<DataRow("01", "def cl A:obj {intr} def cl B:A {ctor B() {dec base.A(); self} };", 0)>]
    [<DataRow("01a", "def cl A:obj {intr} def cl B:A {ctor B() {dec base.A() base.A(); self} };", 1)>]
    [<DataRow("02", "def cl A:obj { ctor A() {dec base.obj(); self} };", 0)>]
    [<DataRow("02a", "def cl A:obj { ctor A() {dec base.obj() base.obj(); self} };", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestID021(no:string, fplCode:string, expected) =
        let code = ID021 ""
        runTestHelper "TestID021.fpl" fplCode code expected

    [<DataRow("def pred T() {del.Test()};", 1, "Unknown delegate `Test`")>]
    [<DataRow("def pred T() {del.Test1(x,y)};", 1, "Unknown delegate `Test1`")>]
    [<DataRow("def pred T() {del.Equal(x,y)};", 1, "Predicate `=` cannot be evaluated because the argument `x` is undefined.")>]
    [<DataRow("def pred T(x:pred) {del.Equal(x,y)};", 1, "Predicate `=` cannot be evaluated because the argument `y` is undefined.")>]
    [<DataRow("""def pred Equal infix "=" 50 (x,y: tpl) {del.Equal(x,y)};""", 0, "missing error message")>]
    [<DataRow("def pred T(x,y:pred) {del.Equal(true,y)};", 1, "Predicate `=` cannot be evaluated because the argument `y` is undetermined.")>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0, "missing error message")>]
    [<TestMethod>]
    member this.TestID013(fplCode:string, expected, expectedErrMsg:string) =
        let code = ID013 ""
        let errMsg = runTestHelperWithText "TestID013.fpl" fplCode code expected
        Assert.AreEqual<string>(expectedErrMsg, errMsg)

    [<DataRow("""def pred Or infix "or" 0 (x:+ pred) {true};""", 0)>]
    [<DataRow("""def pred Or infix "or" 0 (x:* pred) {true};""", 0)>]
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
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG00(fplCode:string, expected) =
        let code = SIG00 ("",0)
        runTestHelper "TestSIG00.fpl" fplCode code expected

    [<DataRow("01", """def pred Equal infix "=" 0 (x,y:tpl) { delegate.Equal(x,y) } def pred NotEqual (x,y: tpl) { not (x = y) };""", 0)>]
    [<DataRow("02", """def pred NotEqual (x,y: tpl) { not (x = y) };""", 1)>]
    [<DataRow("03", """def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("04", """def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {+x};""", 0)>]
    [<DataRow("05", """def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {x+};""", 0)>]
    [<DataRow("06", """def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("07", """def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {-x};""", 1)>]
    [<DataRow("08", """def pred T infix "+" 0 (x,y:obj) {true} def pred Test() {x-};""", 1)>]
    [<DataRow("09", """def pred T prefix "+" (x,y:obj) {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("10", """def pred T prefix "+" (x,y:obj) {true} def pred Test() {+x};""", 0)>]
    [<DataRow("11", """def pred T prefix "+" (x,y:obj) {true} def pred Test() {x+};""", 0)>]
    [<DataRow("12", """def pred T prefix "+" (x,y:obj) {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("13", """def pred T prefix "+" (x,y:obj) {true} def pred Test() {-x};""", 1)>]
    [<DataRow("14", """def pred T prefix "+" (x,y:obj) {true} def pred Test() {x-};""", 1)>]
    [<DataRow("15", """def pred T postfix "+" (x,y:obj) {true} def pred Test() {(x + y)};""", 0)>]
    [<DataRow("16", """def pred T postfix "+" (x,y:obj) {true} def pred Test() {+x};""", 0)>]
    [<DataRow("17", """def pred T postfix "+" (x,y:obj) {true} def pred Test() {x+};""", 0)>]
    [<DataRow("18", """def pred T postfix "+" (x,y:obj) {true} def pred Test() {(x - y)};""", 1)>]
    [<DataRow("19", """def pred T postfix "+" (x,y:obj) {true} def pred Test() {-x};""", 1)>]
    [<DataRow("20", """def pred T postfix "+" (x,y:obj) {true} def pred Test() {x-};""", 1)>]
    [<DataRow("21", """loc (x + y) := !tex: x "+" y; ;""", 0)>]
    [<DataRow("22", """loc (x + y) := !tex: x "+" y; ;""", 0)>]
    [<DataRow("23", """def cl A symbol "0": obj {intr} axiom T() {0} ;""", 0)>]
    [<DataRow("24", """def cl A symbol "1": obj {intr} axiom T() {0} ;""", 1)>]
    [<DataRow("25", """def cl A: obj {intr} axiom T() {0} ;""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG01(no:string, fplCode:string, expected) =
        let code = SIG01 ""
        runTestHelper "TestSIG01.fpl" fplCode code expected

    [<DataRow("""def pred T infix "+" 1 (x,y:obj) {true};""", 0)>]
    [<DataRow("""def pred T1 infix "+" 1 (x,y:obj) {true} def pred T2 infix "+" 1 (x,y:obj) {true};""", 1)>]
    [<DataRow("""def pred T1 infix "+" 2 (x,y: obj) {intr} def pred T2 infix "*" 1 (x,y: obj) {intr};""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG02(fplCode:string, expected) =
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
    [<DataRow("05b", "def func Test()->pred(y:obj(z:ind)) {dec ~a:pred(b:obj(c:ind)); return a};", 0)>]
    [<DataRow("05b1", "def func Test()->pred(y:obj(z:ind)) {dec ~a:pred(b:obj(c:obj)); return a};", 1)>]
    [<DataRow("06", "def func Test()->func {dec ~x:func; return x};", 0)>]
    [<DataRow("07", "def func Test()->ind {dec ~x:ind; return x};", 0)>]
    [<TestMethod>]
    member this.TestSIG03(no:string, fplCode:string, expected) =
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
    [<DataRow("08", "def cl Set:obj {intr} axiom Test(x:SetTypo) {true};", 1)>]
    [<DataRow("09", "def cl Set:obj {intr} axiom Test(x:Set) {true};", 0)>]
    [<DataRow("10", "def cl Set:obj {intr} def func PowerSer(x:Set) -> Set {dec ~y:Set; return y};", 0)>]
    [<DataRow("11", "def cl Set:obj {intr} axiom Test() {dec ~x:Set; true};", 0)>]
    [<DataRow("12", "def cl Set:obj {intr} axiom Test() {dec ~x:SetTypo; true};", 1)>]
    [<DataRow("13", "def pred Test() {dec ~x:Set; true};", 1)>]
    [<DataRow("14", "axiom A() { all x:Nat {true} };", 1)>]
    [<DataRow("15", "def pred Test() {dec ~x:object; is(x,Set)};", 1)>]
    [<DataRow("16", "def cl Set:obj {intr} def pred Test() {dec ~x:object; is(x,Set)};", 0)>]
    [<DataRow("17", """def pred T1() {true} def pred Test() { dec ~x:obj; T1(x) };""", 1)>]
    [<DataRow("18", """def pred T1() {true} def pred Test() { OtherTest() };""", 1)>]
    [<DataRow("19", """def pred T (x:obj) {true} def pred Caller() {dec ~x:obj; T(x)} ;""", 0)>]
    [<DataRow("20", """def pred T (x:obj) {true} def pred Caller() {dec ~x:ind; T(x)} ;""", 1)>]
    [<DataRow("21", "inf ExistsByExample(p: pred(c: obj)) {dec ~x: obj; pre: p(c) con: ex x:obj {p(x)}};", 0)>]
    [<DataRow("22", """loc NotEqual(x,y) := !tex: x "\neq" y; ;""", 0)>]
    [<DataRow("23", """def pred Eq infix "=" 1000 (x,y: obj) {intr} axiom A(x,y:obj) { (x = y) };""", 0)>]
    [<DataRow("24", """def pred Eq infix "=" 1000 (x,y: obj) {intr} axiom A(x,y:obj) { Eq(x,y) };""", 0)>]
    [<DataRow("25", """def pred Eq infix "=" 1000 (x,y: obj) {intr} axiom A(x:ind,y:obj) { (x = y) };""", 1)>]
    [<DataRow("26", """def pred Eq infix "=" 1000 (x,y: obj) {intr} axiom A(x:ind,y:obj) { (x = y) };""", 1)>]
    [<DataRow("27", """def pred Eq infix "=" 1000 (x,y: Nat) {intr} axiom A(x:ind,y:obj) { (x = y) };""", 2)>]
    [<DataRow("28", """def pred Eq infix "=" 1000 (x,y: ind) {intr} axiom A(x:ind,y:obj) { (x = y) };""", 1)>]
    [<DataRow("29", """def pred Mul infix "*" 1 (x,y: pred) {intr} def pred Add infix "+" 2 (x,y: ind) {intr} def pred Eq infix "=" 1000 (x,y: obj) {intr} def pred T1() { (x = y * z + 1) };""", 3)>]
    [<DataRow("30", """def pred T (x:tpl) {true} def pred Caller() {dec ~x:ind; T(x)} ;""", 0)>]
    [<DataRow("31", """def pred T (x:tplTest) {true} def pred Caller() {dec ~x:ind; T(x)} ;""", 0)>]
    [<DataRow("32", """def pred T (x,y,z:obj) {true} def pred Caller() {dec ~x:obj; T(x)} ;""", 1)>]
    [<DataRow("33", """def pred T (x,y,z:obj) {true} def pred Caller() {dec ~x:obj; T(x,x)} ;""", 1)>]
    [<DataRow("34", """def pred T (x,y,z:obj) {true} def pred Caller() {dec ~x:obj; T(x,x,x)} ;""", 0)>]
    [<DataRow("35", """def pred T (x,y,z:obj) {true} def pred Caller() {dec ~x,y:obj ~z:ind; T(x,y,z)} ;""", 1)>]
    [<DataRow("36", """def pred T (x,y:obj,z:ind) {true} def pred Caller() {dec ~x,y:obj ~z:ind; T(x,y,z)} ;""", 0)>]
    [<DataRow("37", """def pred T (x,y:obj) {true} def pred Caller() {dec ~x,y:obj ~z:ind; T(x,y,z)} ;""", 1)>]
    [<DataRow("38", """def class Nat: obj {ctor Nat(){dec self:=x.R(); self}};""", 1)>]
    [<DataRow("39", """def func Succ(n:Nat) -> obj {intr};""", 1)>]
    [<DataRow("40", """def func T()->obj { dec ~x:obj; return x};""", 0)>]
    [<DataRow("40a", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return S(x)} ;""", 0)>]
    [<DataRow("40b", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return (S(x)) } ;""", 0)>]
    [<DataRow("40c", """def func S(n:obj) -> obj {intr} def func T()->obj { dec ~x:obj; return S(S(x))} ;""", 0)>]
    [<DataRow("41", """def func T(y:obj)->obj { return self(y)} ;""", 0)>]
    [<DataRow("42", """def func T(y:obj)->obj { intr } def func S()->obj {dec ~x:obj; return T(x)} ;""", 0)>]
    [<DataRow("43", """axiom T() { dec ~x:obj; all p:pred(y:obj) {p(x)}};""", 0)>]
    [<DataRow("44", """def cl A:obj {intr} def func Add(n,m:A)->A {return self(n,m)};""", 0)>]
    [<DataRow("45", """def cl A:obj {intr} def func Add(n,m:A)->A {dec ~x:A; return x};""", 0)>]
    [<DataRow("46", """def cl A:obj {intr} def func Add(n,m:A)->A {dec ~x:A; return x} prop P(op:Add) {true};""", 0)>]
    [<DataRow("47", """def cl A:obj {intr property pred T() {true} property pred S() {T()}};""", 0)>]
    [<DataRow("48", """def cl A:obj {dec ~x:obj; ctor A(y:obj) {dec base.obj() x:=y; self} property func P()->obj {return x}} def pred T(r:A) {r.P()};""", 0)>]
    [<DataRow("49", """def cl A:obj {ctor A(y:+obj) {self}} def class B:obj {ctor B(z:+obj) {dec ~a:A base.obj() a := A(z); self}};""", 0)>]
    [<DataRow("50", """def cl A:obj {intr property pred T() {true}} def cl B:A {ctor B() {dec base.A() assert self.T(); self}};""", 0)>]
    [<DataRow("51", """def func A(n,m:obj)->obj {intr} prop T(op:A) {dec ~x,y:obj; (op(x,y) = x)};""", 0)>]
    [<DataRow("52", """def cl T:obj { dec ~x:+tpl; ctor T(y:+tpl) {dec base.obj() x:=y; self} property func C(i:ind) -> tpl {return x[i]}};""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestSIG04(no:string, fplCode:string, expected) =
        let code = SIG04 ("",0,[""])
        ad.Clear()
        runTestHelper "TestSIG04.fpl" fplCode code expected

    [<DataRow("""def pred Eq infix "=" 1000 (x,y: obj) {intr} axiom A(x:ind,y:obj) { (x = y) };""", 
        "No overload matching `=(ind, obj)`. `x:ind` does not match `x:obj` in TestSIG04MsgSpecificity.Eq(obj, obj).")>]
    [<DataRow("""def func Succ(n:Nat) -> obj {intr};""", 
        "No overload matching `Nat`, no candidates were found. Are you missing a uses clause?")>]
    [<TestMethod>]
    member this.TestSIG04MsgSpecificity(fplCode:string, (expected:string)) =
        let code = SIG04 ("",0,[""])
        prepareFplCode ("TestSIG04MsgSpecificity.fpl", fplCode, false) |> ignore
        checkForUnexpectedErrors code
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<string>(expected, result.Head.Message)


    [<DataRow("""def pred T() { 1. };;""", 1)>]
    [<DataRow("""proof T$1 {1. |- trivial };""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR000(fplCode:string, expected) =
        let code = PR000 ""
        runTestHelper "TestPR000.fpl" fplCode code expected

    [<DataRow("""def pred T() { bydef A };;""", 1)>]
    [<DataRow("""proof T$1 {1. bydef A |- true qed };""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR001(fplCode:string, expected) =
        let code = PR001 
        runTestHelper "TestPR001.fpl" fplCode code expected

    [<DataRow("""def pred T() { Test$1 };;""", 1)>]
    [<DataRow("""proof T$1 {1. |- Test$1 };""", 1)>]
    [<DataRow("""def pred T() { Test$1() };;""", 0)>]
    [<DataRow("""proof T$1 {1. |- Test$1() };""", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ;", 0)>]
    [<TestMethod>]
    member this.TestPR002(fplCode:string, expected) =
        let code = PR002
        runTestHelper "TestPR002.fpl" fplCode code expected


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
    [<DataRow("12", """inf ModusTollens() {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("13", """inf ModusTollens() {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not p};""", 0)>]
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
        let code = LG000 ("","")
        runTestHelper "TestLG000.fpl" fplCode code expected

    [<DataRow("00", """def pred T() { not true };""", 0)>]
    [<DataRow("01", """def pred T() { dec ~x:pred; not x };""", 0)>]
    [<DataRow("02", """def pred T() { dec ~x:pred; not (x) };""", 0)>]
    [<DataRow("03", """def pred T() { dec ~x:pred; not ((x)) };""", 0)>]
    [<DataRow("04", """def pred T() { dec ~x:pred; not (((x))) };""", 0)>]
    [<DataRow("05", """def pred T() { dec ~x:ind; not x };""", 1)>]
    [<DataRow("06", """def pred T() { all x:obj {true} };""", 0)>]
    [<DataRow("07", """inf ModusTollens() {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("08", """inf ModusTollens() {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("09", """inf ModusTollens() {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not (p)};""", 0)>]
    [<DataRow("10", """inf ModusTollens() {dec ~p,q: pred; pre: and (not q, impl(p,q) ) con: not p};""", 0)>]
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
        let code = LG001 ("","","")
        runTestHelper "TestLG001.fpl" fplCode code expected

    [<DataRow("""axiom A() {dec ~x,y:Nat; impl(x,y)};""", 31)>]
    [<TestMethod>]
    member this.TestLG001Position(fplCode:string, (expected:int64)) =
        let code = LG001 ("","","")
        prepareFplCode ("TestLG001Position.fpl", fplCode, false) |> ignore
        checkForUnexpectedErrors code
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int64>(expected, result.Head.StartPos.Column)
        
    [<DataRow("""axiom A() {dec ~x,y:obj; impl(x,y)};""", "Cannot evaluate `implication`; expecting a predicate argument `x`, got `obj`.")>]
    [<DataRow("""axiom A() {dec ~x,y:ind; impl(x,y)};""", "Cannot evaluate `implication`; expecting a predicate argument `x`, got `ind`.")>]
    [<DataRow("""axiom A() {dec ~x,y:func; impl(x,y)};""", "Cannot evaluate `implication`; expecting a predicate argument `x`, got `func`.")>]
    [<DataRow("""axiom A() {impl(x,y)};""", "Cannot evaluate `implication`; expecting a predicate argument `x`, got `undef`.")>]
    [<DataRow("""axiom A() {impl(T(),true)};""", "Cannot evaluate `implication`; expecting a predicate argument `T()`, got `undef`.")>]
    [<DataRow("""axiom A() {impl(T,true)};""", "Cannot evaluate `implication`; expecting a predicate argument `T`, got `undef`.")>]
    [<DataRow("""def cl T:obj {intr} axiom A() {impl(T,true)};""", "Cannot evaluate `implication`; expecting a predicate argument `T`, got `undef`.")>]
    [<TestMethod>]
    member this.TestLG001MsgSpecificity(fplCode:string, (expected:string)) =
        let code = LG001 ("","","")
        prepareFplCode ("TestLG001MsgSpecificity.fpl", fplCode, false) |> ignore
        checkForUnexpectedErrors code
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<string>(expected, result.Head.Message)

