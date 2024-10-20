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

    [<DataRow("lem Le2() { true } proof Le2$1 {  1. |- trivial  2. 1., 2., 3.|- trivial 3. |- trivial qed };")>]
    [<TestMethod>]
    member this.TestProofArgumentsInScope(varVal) =
        ad.Clear()
        let fplCode = sprintf "%s" varVal
        let filename = "TestProofArgumentsInScope"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["Le2()"] 
            let base1 = pr1.Scope["1"]
            let base2 = pr1.Scope["2"]
            let base3 = pr1.Scope["3"]
            Assert.AreEqual<string>("true", base1.Name)
        | _ -> Assert.IsTrue(false)
