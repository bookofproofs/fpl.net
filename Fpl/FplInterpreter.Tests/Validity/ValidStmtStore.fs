namespace Validity
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Interpreter.SymbolTable.Storage.Heap
open TestFplInterpreter.Helpers.Common


[<TestClass>]
type ValidStmtStore() =



    [<DataRow("axiom X { true }", 1)>] // one axiom, one stmt
    [<DataRow("axiom X { true } axiom Y { true }", 1)>] // single stmt, two axioms
    [<DataRow("axiom X { true } axiom Y { ex x:obj { true } }", 2)>] // two axioms, two stmts 
    [<TestMethod>]
    member this.TestStmtStoreAddsAxioms(varVal, expected:int) =
       
        let fplCode = sprintf "%s" varVal
        let filename = "TestStmtStoreAddsAxioms"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        Assert.AreEqual<int>(expected, heap.ValidStmtStore.Count)
        heap.ValidStmtStore.ClearValidityStore()
        prepareFplCode(filename, "", false) |> ignore

