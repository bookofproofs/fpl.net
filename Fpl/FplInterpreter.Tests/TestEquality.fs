﻿namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestEquality() =

    [<DataRow("dec ~x,y:obj; (x = y)")>]
    [<TestMethod>]
    member this.TestEqualityPredicate(varVal) =
        ad.Clear()
        let fplCode = sprintf "uses Fpl.Commons def pred T1() { %s };" varVal
        let filename = "TestEqualityPredicate.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>("true", base1.Type(SignatureType.Repr))
        | None -> 
            Assert.IsTrue(false)

