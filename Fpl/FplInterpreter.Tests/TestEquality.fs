﻿namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestEquality() =

    [<DataRow("""def pred T1() { dec ~x,y:obj; (x = y) }""", "true")>]
    [<DataRow("""def pred T1() { (@1 = @2) }""", "false")>]
    [<DataRow("""def pred T1() { (@1 = @1) }""", "true")>]
    [<TestMethod>]
    member this.TestEqualityPredicate(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred Equal infix "=" 0 (x,y: tpl) { del.Equal(x,y) } %s;""" varVal
        let filename = "TestEqualityPredicate"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            Assert.AreEqual<string>(expected, base1.Type(SignatureType.Repr))
        | None -> 
            Assert.IsTrue(false)

