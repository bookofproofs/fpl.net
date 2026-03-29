namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterBasicTypes
open CommonTestHelpers

[<TestClass>]
type TestDecrement() =


    [<DataRow("@0", PrimUndetermined)>]
    [<DataRow("@1", "0")>]
    [<DataRow("@2", "1")>]
    [<DataRow("@3", "2")>]
    [<DataRow("@4", "3")>]
    [<DataRow("@100", "99")>]
    [<DataRow("@42", "41")>]
    [<TestMethod>]
    member this.TestDecrementRepresent(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """ext Digits x@/\d+/ -> Digits {ret x} def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrementRepresent.fpl"
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

    [<DataRow("@0", PrimUndetermined)>]
    [<DataRow("@1", PrimUndetermined)>]
    [<DataRow("@2", PrimUndetermined)>]
    [<DataRow("@3", PrimUndetermined)>]
    [<DataRow("@4", PrimUndetermined)>]
    [<DataRow("@100", PrimUndetermined)>]
    [<DataRow("@42", PrimUndetermined)>]
    [<TestMethod>]
    member this.TestDecrementRepresentWrongType(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """ext Digits x@/\d+/ -> ind {ret $100} def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrementRepresent.fpl"
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

    [<DataRow("@0", PrimDigits)>]
    [<DataRow("@1", PrimDigits)>]
    [<DataRow("@2", PrimDigits)>]
    [<DataRow("@3", PrimDigits)>]
    [<DataRow("@4", PrimDigits)>]
    [<DataRow("@100", PrimDigits)>]
    [<DataRow("@42", PrimDigits)>]
    [<TestMethod>]
    member this.TestDecrementType(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrementType.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let predicateValue = pr.ArgList |> Seq.toList |> List.rev |> List.head
            Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Type)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("@0", "Decrement(obj)")>]
    [<DataRow("@1", "Decrement(obj)")>]
    [<DataRow("@2", "Decrement(obj)")>]
    [<DataRow("@3", "Decrement(obj)")>]
    [<DataRow("@4", "Decrement(obj)")>]
    [<DataRow("@100", "Decrement(obj)")>]
    [<DataRow("@42", "Decrement(obj)")>]
    [<TestMethod>]
    member this.TestDecrementMixed(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrementMixed.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let predicateValue = pr.ArgList |> Seq.toList |> List.rev |> List.head
            Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Mixed)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("@0", "Decrement(Nat)")>]
    [<DataRow("@1", "Decrement(Nat)")>]
    [<DataRow("@2", "Decrement(Nat)")>]
    [<DataRow("@3", "Decrement(Nat)")>]
    [<DataRow("@4", "Decrement(Nat)")>]
    [<DataRow("@100", "Decrement(Nat)")>]
    [<DataRow("@42", "Decrement(Nat)")>]
    [<TestMethod>]
    member this.TestDecrementMixedWithExtensionNat(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """ext Digits x@/\d+/ -> Nat {return x} def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrementMixedWithExtensionNat.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let predicateValue = pr.ArgList |> Seq.toList |> List.rev |> List.head
            Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Mixed)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("@0", "Decrement(Digits)")>]
    [<DataRow("@1", "Decrement(Digits)")>]
    [<DataRow("@2", "Decrement(Digits)")>]
    [<DataRow("@3", "Decrement(Digits)")>]
    [<DataRow("@4", "Decrement(Digits)")>]
    [<DataRow("@100", "Decrement(Digits)")>]
    [<DataRow("@42", "Decrement(Digits)")>]
    [<TestMethod>]
    member this.TestDecrementMixedWithExtensionDigits(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """ext Digits x@/\d+/ -> Digits {return x} def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrementMixedWithExtensionDigits.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let predicateValue = pr.ArgList |> Seq.toList |> List.rev |> List.head
            Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Mixed)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("@0", "Decrement(0)")>]
    [<DataRow("@1", "Decrement(1)")>]
    [<DataRow("@2", "Decrement(2)")>]
    [<DataRow("@3", "Decrement(3)")>]
    [<DataRow("@4", "Decrement(4)")>]
    [<DataRow("@100", "Decrement(100)")>]
    [<DataRow("@42", "Decrement(42)")>]
    [<TestMethod>]
    member this.TestDecrementName(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrementName.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let predicateValue = pr.ArgList |> Seq.toList |> List.rev |> List.head
            Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Name)
        | None -> 
            Assert.IsTrue(false)
