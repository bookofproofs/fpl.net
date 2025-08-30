namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplGrammarCommons
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestDecrement() =


    [<DataRow("@0", literalUndef)>]
    [<DataRow("@1", "0")>]
    [<DataRow("@2", "1")>]
    [<DataRow("@3", "2")>]
    [<DataRow("@4", "3")>]
    [<DataRow("@100", "99")>]
    [<DataRow("@42", "41")>]
    [<TestMethod>]
    member this.TestDecrement(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrement.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let predicateValue = pr.ArgList |> Seq.toList |> List.rev |> List.head
            Assert.AreEqual<string>(expected, predicateValue.Represent())
        | None -> 
            Assert.IsTrue(false)

