namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplGrammarCommons
open FplInterpreterTypes
open CommonTestHelpers
open System


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
            let base1 = pr1.ArgList[0]
            let resS = getType SignatureType.Mixed base1

            Assert.AreEqual<string>(keywTrue, resS)
        | _ -> Assert.IsTrue(false)

    [<DataRow("lem Le2() { true } proof Le2$1 {  1. |- trivial  2. 1., 2., 3. |- trivial 3. |- trivial qed };")>]
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
            let lem = theory.Scope["Le2()"] 
            let prf = lem.Scope["Le2$1"]
            let arg1 = prf.Scope["1."]
            let arg2 = prf.Scope["2."]
            let arg3 = prf.Scope["3."]
            Assert.AreEqual<string>("1.",getType SignatureType.Mixed arg1)
            Assert.AreEqual<string>("2.",getType SignatureType.Mixed arg2)
            Assert.AreEqual<string>("3.",getType SignatureType.Mixed arg3)
        | _ -> Assert.IsTrue(false)
