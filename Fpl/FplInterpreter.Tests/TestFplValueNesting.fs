namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers


[<TestClass>]
type TestFplValueNesting() =



    [<DataRow("axiom X() { true };")>]
    [<TestMethod>]
    member this.TestAxiomHasValue(varVal) =
        ad.Clear()
        let fplCode = sprintf "%s" varVal
        let filename = "TestAxiomHasValue"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["X()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>("true", base1.Name)
        | _ -> Assert.IsTrue(false)
