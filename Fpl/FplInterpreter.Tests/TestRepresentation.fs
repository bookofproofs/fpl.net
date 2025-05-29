namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestRepresentation() =

    [<DataRow("00","n:=Zero()", "Zero()")>]
    [<TestMethod>]
    member this.TestAssignmentRepresentation(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.PeanoArithmetics def pred T() { dec ~n:Nat %s; true };""" varVal
        let filename = "TestAssignmentRepresentation.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let fn = theory.Scope["T()"] 
            let assignmentStmt = fn.ValueList[0]
            Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
            
    [<DataRow("00","n:=Zero()", "Zero()")>]
    [<TestMethod>]
    member this.TestReturnRepresentation(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.PeanoArithmetics def func T()->Nat { dec ~n:Nat %s; return n };""" varVal
        let filename = "TestReturnRepresentation.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let fn = theory.Scope["T() -> Nat"] 
            let retStmt = fn.ValueList[0]
            Assert.AreEqual<string>(expected, retStmt.Type(SignatureType.Repr))
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","(@0 = Zero())", "true")>]
    [<TestMethod>]
    member this.TestCasesRepresentation(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.PeanoArithmetics def pred T() { %s };""" varVal
        let filename = "TestCasesRepresentation.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let base1 = pr.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Type(SignatureType.Repr))
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00","(@0 = A())", "false")>]
    [<DataRow("00a","(@1 = A())", "false")>]
    [<DataRow("00b","(@2 = A())", "true")>]
    [<DataRow("01","(@0 = B())", "true")>]
    [<DataRow("01a","(@1 = B())", "false")>]
    [<DataRow("01b","(@2 = A())", "false")>]
    [<DataRow("02","(@0 = C())", "false")>]
    [<DataRow("02a","(@1 = C())", "true")>]
    [<DataRow("02b","(@2 = C())", "false")>]
    [<TestMethod>]
    member this.TestCasesRepresentation2(var:string, varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """
            uses Fpl.Commons
            
            def cl X: obj { intr }
            
            def cl A: obj { intr }
            def cl B: A { intr }
            def cl C: A { intr }

            ext Digits x@/\d+/ -> A 
            {
                dec
                ~n:A
                cases
                (
                    | (x = @0) : n := B() 
                    | (x = @1) : n := C() 
                    ? n := A()  
                )
                ;
                return n
            }        
        
        
        def pred T() { %s };""" varVal
        let filename = "TestCasesRepresentation2.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let base1 = pr.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Type(SignatureType.Repr))
        | None -> 
            Assert.IsTrue(false)
