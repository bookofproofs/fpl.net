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
        let filename = "TestNestedRepresentation.fpl"
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

