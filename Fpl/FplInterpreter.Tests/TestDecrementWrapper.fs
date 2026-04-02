namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterBasicTypes
open FplInterpreter.Globals.Heap
open CommonTestHelpers

[<TestClass>]
type TestDecrementWrapper() =


    [<DataRow("@0", PrimUndetermined)>]
    [<DataRow("@1", "0")>]
    [<DataRow("@2", "1")>]
    [<DataRow("@3", "2")>]
    [<DataRow("@4", "3")>]
    [<DataRow("@100", "99")>]
    [<DataRow("@42", "41")>]
    [<TestMethod>]
    member this.TestDecrementWrapperRepresent(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """ext Digits x@/\d+/ -> Digits {ret x} def func Decr(x:Digits)->Digits { ret del.Decrement(x) } def func T()->Digits { ret Decr(%s) };""" varVal
        let filename = "TestDecrementWrapperRepresent.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pr = theory.Scope["T() -> Digits"] 
        let predicateValue = pr.ArgList |> Seq.toList |> List.rev |> List.head
        Assert.AreEqual<string>(expected, predicateValue.Represent())
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("@0", LiteralObj)>]
    [<DataRow("@1", LiteralObj)>]
    [<DataRow("@2", LiteralObj)>]
    [<DataRow("@3", LiteralObj)>]
    [<DataRow("@4", LiteralObj)>]
    [<DataRow("@100", LiteralObj)>]
    [<DataRow("@42", LiteralObj)>]
    [<TestMethod>]
    member this.TestDecrementWrapperType(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """ext Digits x@/\d+/ -> obj {ret x} def func Decr(x:obj)->obj { ret del.Decrement(x) } def func T()->obj { ret Decr(%s) };""" varVal
        let filename = "TestDecrementWrapperType.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pr = theory.Scope["T() -> obj"] 
        let ret = pr.ArgList |> Seq.rev |> Seq.head
        let predicateValue = ret.ArgList |> Seq.head
        Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Type)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("@0", "Decr(obj)")>]
    [<DataRow("@1", "Decr(obj)")>]
    [<DataRow("@2", "Decr(obj)")>]
    [<DataRow("@3", "Decr(obj)")>]
    [<DataRow("@4", "Decr(obj)")>]
    [<DataRow("@100", "Decr(obj)")>]
    [<DataRow("@42", "Decr(obj)")>]
    [<TestMethod>]
    member this.TestDecrementWrapperMixed(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def func Decr(x:obj)->obj { ret del.Decrement(x) } def func T()->obj { ret Decr(%s) };""" varVal
        let filename = "TestDecrementWrapperMixed.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pr = theory.Scope["T() -> obj"] 
        let ret = pr.ArgList |> Seq.rev |> Seq.head
        let predicateValue = ret.ArgList |> Seq.head
        Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Mixed)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("@0", "Decr(Nat)")>]
    [<DataRow("@1", "Decr(Nat)")>]
    [<DataRow("@2", "Decr(Nat)")>]
    [<DataRow("@3", "Decr(Nat)")>]
    [<DataRow("@4", "Decr(Nat)")>]
    [<DataRow("@100", "Decr(Nat)")>]
    [<DataRow("@42", "Decr(Nat)")>]
    [<TestMethod>]
    member this.TestDecrementWrapperMixedWithExtension(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """ext Digits x@/\d+/ -> Nat {ret x} def func Decr(x:obj)->obj { ret del.Decrement(x) } def func T()->obj { ret Decr(%s) };""" varVal
        let filename = "TestDecrementWrapperMixedWithExtension.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pr = theory.Scope["T() -> obj"] 
        let ret = pr.ArgList |> Seq.rev |> Seq.head
        let predicateValue = ret.ArgList |> Seq.head
        Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Mixed)
        prepareFplCode(filename, "", false) |> ignore


    [<DataRow("@0", "Decr(0)")>]
    [<DataRow("@1", "Decr(1)")>]
    [<DataRow("@2", "Decr(2)")>]
    [<DataRow("@3", "Decr(3)")>]
    [<DataRow("@4", "Decr(4)")>]
    [<DataRow("@100", "Decr(100)")>]
    [<DataRow("@42", "Decr(42)")>]
    [<TestMethod>]
    member this.TestDecrementWrapperName(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """ext Digits x@/\d+/ -> obj {ret x} def func Decr(x:obj)->obj { ret del.Decrement(x) } def func T()->obj { ret Decr(%s) };""" varVal
        let filename = "TestDecrementWrapperName.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pr = theory.Scope["T() -> obj"] 
        let ret = pr.ArgList |> Seq.rev |> Seq.head
        let predicateValue = ret.ArgList |> Seq.head
        Assert.AreEqual<string>(expected, predicateValue.Type SignatureType.Name)
        prepareFplCode(filename, "", false) |> ignore
