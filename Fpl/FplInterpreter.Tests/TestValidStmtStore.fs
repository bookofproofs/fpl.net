namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreter.Globals.Heap
open FplInterpreterBasicTypes
open CommonTestHelpers
open System


[<TestClass>]
type TestValidStmtStore() =



    [<DataRow("axiom X { true };", 1)>] // one axiom, one stmt
    [<DataRow("axiom X { true } axiom Y { true };", 1)>] // single stmt, two axioms
    [<DataRow("axiom X { true } axiom Y { ex x:obj { true } };", 2)>] // two axioms, two stmts 
    [<TestMethod>]
    member this.TestStmtStoreAddsAxioms(varVal, expected:int) =
       
        let fplCode = sprintf "%s" varVal
        let filename = "TestStmtStoreAddsAxioms"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        Assert.AreEqual<int>(expected, heap.ValidStmtStore.Count)
        heap.ValidStmtStore.ClearValidityStore()
        prepareFplCode(filename, "", false) |> ignore

