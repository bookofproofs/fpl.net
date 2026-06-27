namespace FixOps
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Primitives
open Fpl.Interpreter.Helpers.Debug
open Fpl.Interpreter.SymbolTable.Storage.Heap
open TestFplInterpreter.Helpers.Common

[<TestClass>]
type InfixOperations() =

    [<DataRow("""def pred T1() { dec x,y:obj; (x = y) }""", LiteralUndet)>]
    [<DataRow("""ext T x@/\d+/->T {ret x} def pred T1() { (@1 = @2) }""", LiteralFalse)>]
    [<DataRow("""ext T x@/\d+/->T {ret x} def pred T1() { (@42 = @2) }""", LiteralFalse)>]
    [<DataRow("""ext T x@/\d+/->T {ret x} def pred T1() { (@42 = @42) }""", LiteralTrue)>]
    [<DataRow("""def pred T1() { (@1 = @1) }""", LiteralTrue)>]
    [<TestMethod>]
    member this.TestEqualityPredicate(varVal, expected:string) =
        
        let fplCode = sprintf """def pred Equal (x,y: tpl) infix "=" 0 { del.Equal(x,y) } %s""" varVal
        let filename = "TestEqualityPredicate"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]

        let pr1 = theory.Scope["T1()"] 
        pr1.Run()
        Assert.AreEqual<string>(expected, pr1.Represent())
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00a", """def pred T1() { (true ⇒ false) }""", LiteralFalse)>]
    [<DataRow("00b", """def pred T1() { (false ⇒ false) }""", LiteralTrue)>]
    [<DataRow("00c", """def pred T1() { (false ⇒ true) }""", LiteralTrue)>]
    [<DataRow("00d", """def pred T1() { (true ⇒ true) }""", LiteralTrue)>]
    [<TestMethod>]
    member this.TestImplicationCallsFplCommons(no:string, varVal, expected:string) =
        
        if offlineWatcher.OfflineMode then 
            ()
        else
            let fplCode = sprintf """uses Fpl.Commons %s""" varVal
            let filename = "TestImplicationCallsFplCommons"
            prepareFplCode(filename + ".fpl", fplCode, false) 
            checkForUnexpectedErrors filename fplCode
            let r = heap.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            Assert.AreEqual<string>(expected, pr1.Represent())
            prepareFplCode(filename, "", false) |> ignore

    [<DataRow("01a", """def pred T1() { (true ⇔ false) }""", LiteralFalse)>]
    [<DataRow("01b", """def pred T1() { (false ⇔ false) }""", LiteralTrue)>]
    [<DataRow("01c", """def pred T1() { (false ⇔ true) }""", LiteralFalse)>]
    [<DataRow("01d", """def pred T1() { (true ⇔ true) }""", LiteralTrue)>]
    [<TestMethod>]
    member this.TestEquivalenceCallsFplCommons(no:string, varVal, expected:string) =
        
        if offlineWatcher.OfflineMode then 
            ()
        else
            let fplCode = sprintf """uses Fpl.Commons %s""" varVal
            let filename = "TestEquivalenceCallsFplCommons"
            prepareFplCode(filename + ".fpl", fplCode, false) 
            checkForUnexpectedErrors filename fplCode
            let r = heap.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            Assert.AreEqual<string>(expected, pr1.Represent())
            prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(true) ) }""", LiteralFalse)>]
    [<DataRow("00a", """def pred Neg(x:tpl) {not x} def pred T1() { ( Neg(true) ) }""", LiteralFalse)>]
    [<DataRow("01", """def pred Neg(x:pred) {not x} def pred T1() { ( Neg(false) ) }""", LiteralTrue)>]
    [<DataRow("01a", """def pred Neg(x:tpl) {not x} def pred T1() { ( Neg(false) ) }""", LiteralTrue)>]
    [<TestMethod>]
    member this.TestNegationCalls(no:string, varVal, expected:string) =
        
        let fplCode = sprintf """%s""" varVal
        let filename = "TestNegationCalls"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors filename fplCode
        let r = heap.Root
        let theory = r.Scope[filename]

        let pr1 = theory.Scope["T1()"] 
        Assert.AreEqual<string>(expected, pr1.Represent())
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00", """def pred T1() { (false ∧ false ∧ false) }""", LiteralFalse)>]
    [<DataRow("01", """def pred T1() { (true ∧ false ∧ false) }""", LiteralFalse)>]
    [<DataRow("02", """def pred T1() { (false ∧ true ∧ false) }""", LiteralFalse)>]
    [<DataRow("03", """def pred T1() { (true ∧ true ∧ false) }""", LiteralFalse)>]
    [<DataRow("04", """def pred T1() { (false ∧ false ∧ true) }""", LiteralFalse)>]
    [<DataRow("05", """def pred T1() { (true ∧ false ∧ true) }""", LiteralFalse)>]
    [<DataRow("06", """def pred T1() { (false ∧ true ∧ true) }""", LiteralFalse)>]
    [<DataRow("07", """def pred T1() { (true ∧ true ∧ true) }""", LiteralTrue)>]
    [<DataRow("08a", """def pred T1() { (true ∧ true) }""", LiteralTrue)>]
    [<DataRow("08b", """def pred T1() { (true ∧ false) }""", LiteralFalse)>]
    [<DataRow("08c", """def pred T1() { (false ∧ true) }""", LiteralFalse)>]
    [<DataRow("08d", """def pred T1() { (false ∧ false) }""", LiteralFalse)>]
    [<TestMethod>]
    member this.TestConjunctionCallsFplCommons(no:string, varVal, expected:string) =
        
        if offlineWatcher.OfflineMode then 
            ()
        else
            let fplCode = sprintf """uses Fpl.Commons %s""" varVal
            let filename = "TestConjunctionCallsFplCommons"
            prepareFplCode(filename + ".fpl", fplCode, false) 
            checkForUnexpectedErrors filename fplCode 
            let r = heap.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            pr1.Run()
            Assert.AreEqual<string>(expected, pr1.Represent())
            prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00", """def pred T1() { false ∧ false ∧ false }""", LiteralFalse)>]
    [<DataRow("01", """def pred T1() { (true ∧ false ∧ false) }""", LiteralFalse)>]
    [<DataRow("02", """def pred T1() { (false ∧ true ∧ false) }""", LiteralFalse)>]
    [<DataRow("03", """def pred T1() { (true ∧ true ∧ false) }""", LiteralFalse)>]
    [<DataRow("04", """def pred T1() { (false ∧ false ∧ true) }""", LiteralFalse)>]
    [<DataRow("05", """def pred T1() { (true ∧ false ∧ true) }""", LiteralFalse)>]
    [<DataRow("06", """def pred T1() { (false ∧ true ∧ true) }""", LiteralFalse)>]
    [<DataRow("07", """def pred T1() { (true ∧ true ∧ true) }""", LiteralTrue)>]
    [<DataRow("08a", """def pred T1() { (true ∧ true) }""", LiteralTrue)>]
    [<DataRow("08b", """def pred T1() { (true ∧ false) }""", LiteralFalse)>]
    [<DataRow("08c", """def pred T1() { (false ∧ true) }""", LiteralFalse)>]
    [<DataRow("08d", """def pred T1() { (false ∧ false) }""", LiteralFalse)>]
    [<TestMethod>]
    member this.TestConjunctionCalls(no:string, varVal, expected:string) =
        
        let fplCode = sprintf """def pred And(x,y: pred) infix "∧" 7 { and (x,y) } %s""" varVal
        let filename = "TestConjunctionCalls"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        checkForUnexpectedErrors filename fplCode
        let r = heap.Root
        let theory = r.Scope[filename]

        let pr1 = theory.Scope["T1()"] 
        Assert.AreEqual<string>(expected, pr1.Represent())
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00", """def pred T1() { (false ∨ false ∨ false) }""", LiteralFalse)>]
    [<DataRow("01", """def pred T1() { (true ∨ false ∨ false) }""", LiteralTrue)>]
    [<DataRow("02", """def pred T1() { (false ∨ true ∨ false) }""", LiteralTrue)>]
    [<DataRow("03", """def pred T1() { (true ∨ true ∨ false) }""", LiteralTrue)>]
    [<DataRow("04", """def pred T1() { (false ∨ false ∨ true) }""", LiteralTrue)>]
    [<DataRow("05", """def pred T1() { (true ∨ false ∨ true) }""", LiteralTrue)>]
    [<DataRow("06", """def pred T1() { (false ∨ true ∨ true) }""", LiteralTrue)>]
    [<DataRow("07", """def pred T1() { (true ∨ true ∨ true) }""", LiteralTrue)>]
    [<DataRow("08a", """def pred T1() { (true ∨ true) }""", LiteralTrue)>]
    [<DataRow("08b", """def pred T1() { (true ∨ false) }""", LiteralTrue)>]
    [<DataRow("08c", """def pred T1() { (false ∨ true) }""", LiteralTrue)>]
    [<DataRow("08d", """def pred T1() { (false ∨ false) }""", LiteralFalse)>]
    [<TestMethod>]
    member this.TestDisjunctionCallsFplCommons(no:string, varVal, expected:string) =
        
        if offlineWatcher.OfflineMode then 
            ()
        else
            let fplCode = sprintf """uses Fpl.Commons %s""" varVal
            let filename = "TestDisjunctionCallsFplCommons"
            prepareFplCode(filename + ".fpl", fplCode, false) 
            checkForUnexpectedErrors filename fplCode
            let r = heap.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            Assert.AreEqual<string>(expected, pr1.Represent())
            prepareFplCode(filename, "", false) |> ignore

    [<DataRow("00", """def pred T1() { (false ⩡ false ⩡ false) }""", LiteralFalse)>]
    [<DataRow("01", """def pred T1() { (true ⩡ false ⩡ false) }""", LiteralTrue)>]
    [<DataRow("02", """def pred T1() { (false ⩡ true ⩡ false) }""", LiteralTrue)>]
    [<DataRow("03", """def pred T1() { (true ⩡ true ⩡ false) }""", LiteralFalse)>]
    [<DataRow("04", """def pred T1() { (false ⩡ false ⩡ true) }""", LiteralTrue)>]
    [<DataRow("05", """def pred T1() { (true ⩡ false ⩡ true) }""", LiteralFalse)>]
    [<DataRow("06", """def pred T1() { (false ⩡ true ⩡ true) }""", LiteralFalse)>]
    [<DataRow("07", """def pred T1() { (true ⩡ true ⩡ true) }""", LiteralTrue)>]
    [<DataRow("08a", """def pred T1() { (true ⩡ true) }""", LiteralFalse)>]
    [<DataRow("08b", """def pred T1() { (true ⩡ false) }""", LiteralTrue)>]
    [<DataRow("08c", """def pred T1() { (false ⩡ true) }""", LiteralTrue)>]
    [<DataRow("08d", """def pred T1() { (false ⩡ false) }""", LiteralFalse)>]
    [<TestMethod>]
    member this.TestExDisjunctionCallsFplCommons(no:string, varVal, expected:string) =
        
        if offlineWatcher.OfflineMode then 
            ()
        else
            let fplCode = sprintf """uses Fpl.Commons %s""" varVal
            let filename = "TestDisjunctionCallsFplCommons"
            prepareFplCode(filename + ".fpl", fplCode, false) 
            checkForUnexpectedErrors filename fplCode
            let r = heap.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            Assert.AreEqual<string>(expected, pr1.Represent())
            prepareFplCode(filename, "", false) |> ignore
