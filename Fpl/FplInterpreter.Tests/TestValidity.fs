namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreter.Globals.Heap
open CommonTestHelpers

[<TestClass>]
type TestValidity() =


    // nodes to be stored in the theorem store
    [<DataRow("s00", "ax A {true};", 1)>]
    [<DataRow("s01", "thm T {true};", 1)>]
    [<DataRow("s02", "lem T {true};", 1)>]
    [<DataRow("s03", "prop T {true};", 1)>]
    [<DataRow("s04", "cor T$1 {true};", 1)>]
    // nodes not to be stored in the theorem store
    [<DataRow("n00", "def pred T(x,y:obj) {true};", 0)>]
    [<DataRow("n01", "def func T()->obj {intr};", 0)>]
    [<DataRow("n02", "conj T {true};", 0)>]
    [<DataRow("n03", "inf T {pre: true con:true};", 0)>]
    [<TestMethod>]
    member this.TestValidityTheoremStore(no:string, fplCode:string, expected:int) =
        let filename = "TestValidityTheoremStore.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        Assert.AreEqual<int>(expected, heap.ValidStmtStore.Count)
        prepareFplCode(filename, "", false) |> ignore
