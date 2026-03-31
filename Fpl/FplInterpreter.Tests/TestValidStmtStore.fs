namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreter.Globals.Main
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
        ad.Clear()
        let fplCode = sprintf "%s" varVal
        let filename = "TestStmtStoreAddsAxioms"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            Assert.AreEqual<int>(expected, variableStack.ValidStmtStore.Count)
            variableStack.ValidStmtStore.Clear()
        | _ -> Assert.IsTrue(false)

