namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestArity() =

    [<DataRow("theorem T(x,y:obj) {true}", 2)>]
    [<DataRow("theorem T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("theorem T(x:+obj) {true}", 1)>]
    [<DataRow("theorem T() {true}", 0)>]
    [<DataRow("lem T(x,y:obj) {true}", 2)>]
    [<DataRow("lem T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("lem T(x:*obj) {true}", 1)>]
    [<DataRow("lem T() {true}", 0)>]
    [<DataRow("prop T(x,y:obj) {true}", 2)>]
    [<DataRow("prop T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("prop T(x:*obj) {true}", 1)>]
    [<DataRow("prop T() {true}", 0)>]
    [<DataRow("cor T$1(x,y:obj) {true}", 2)>]
    [<DataRow("cor T$1(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("cor T$1(x:*obj) {true}", 1)>]
    [<DataRow("cor T$1() {true}", 0)>]
    [<DataRow("def pred T(x,y:obj) {true}", 2)>]
    [<DataRow("def pred T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("def pred T(x:*obj) {true}", 1)>]
    [<DataRow("def pred T() {true}", 0)>]
    [<DataRow("def func T(x,y:obj)->obj(z:obj) {intr}", 2)>]
    [<DataRow("def func T(x,y:obj, a,b,c:pred)->ind {intr}", 5)>]
    [<DataRow("def func T(x:*obj)->pred {intr}", 1)>]
    [<DataRow("def func T()->obj(z:ind) {intr}", 0)>]
    [<DataRow("def func T()->obj {intr}", 0)>]
    [<DataRow("ax T(x,y:obj) {true}", 2)>]
    [<DataRow("ax T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("ax T(x:*obj) {true}", 1)>]
    [<DataRow("ax T() {true}", 0)>]
    [<DataRow("conj T(x,y:obj) {true}", 2)>]
    [<DataRow("conj T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("conj T(x:*obj) {true}", 1)>]
    [<DataRow("conj T() {true}", 0)>]
    [<DataRow("inf T(x,y:obj) {pre: true con:true}", 2)>]
    [<DataRow("inf T(x,y:obj, a,b,c:pred) {pre: true con:true}", 5)>]
    [<DataRow("inf T(x:*obj) {pre: true con:true}", 1)>]
    [<DataRow("inf T() {pre: true con:true}", 0)>]
    [<TestMethod>]
    member this.TestArityBlocks(varVal, (arity:int)) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestArityBlocks.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let test = theory.Scope.Values |> Seq.toList |> List.head 
            Assert.AreEqual<int>(arity, test.Arity)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("theorem T(x,y:obj) {true}", 2)>]
    [<DataRow("theorem T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("theorem T(x:+obj) {true}", 1)>]
    [<DataRow("theorem T() {true}", 0)>]
    [<DataRow("lem T(x,y:obj) {true}", 2)>]
    [<DataRow("lem T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("lem T(x:*obj) {true}", 1)>]
    [<DataRow("lem T() {true}", 0)>]
    [<DataRow("prop T(x,y:obj) {true}", 2)>]
    [<DataRow("prop T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("prop T(x:*obj) {true}", 1)>]
    [<DataRow("prop T() {true}", 0)>]
    [<DataRow("cor T$1(x,y:obj) {true}", 2)>]
    [<DataRow("cor T$1(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("cor T$1(x:*obj) {true}", 1)>]
    [<DataRow("cor T$1() {true}", 0)>]
    [<DataRow("def pred T(x,y:obj) {true}", 2)>]
    [<DataRow("def pred T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("def pred T(x:*obj) {true}", 1)>]
    [<DataRow("def pred T() {true}", 0)>]
    [<DataRow("def func T(x,y:obj)->obj(z:obj) {intr}", 2)>]
    [<DataRow("def func T(x,y:obj, a,b,c:pred)->ind {intr}", 5)>]
    [<DataRow("def func T(x:*obj)->pred {intr}", 1)>]
    [<DataRow("def func T()->obj(z:ind) {intr}", 0)>]
    [<DataRow("def func T()->obj {intr}", 0)>]
    [<DataRow("ax T(x,y:obj) {true}", 2)>]
    [<DataRow("ax T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("ax T(x:*obj) {true}", 1)>]
    [<DataRow("ax T() {true}", 0)>]
    [<DataRow("conj T(x,y:obj) {true}", 2)>]
    [<DataRow("conj T(x,y:obj, a,b,c:pred) {true}", 5)>]
    [<DataRow("conj T(x:*obj) {true}", 1)>]
    [<DataRow("conj T() {true}", 0)>]
    [<DataRow("inf T(x,y:obj) {pre: true con:true}", 2)>]
    [<DataRow("inf T(x,y:obj, a,b,c:pred) {pre: true con:true}", 5)>]
    [<DataRow("inf T(x:*obj) {pre: true con:true}", 1)>]
    [<DataRow("inf T() {pre: true con:true}", 0)>]
    [<TestMethod>]
    member this.TestIsReadyBlocks(varVal, (arity:int)) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestIsReadyBlocks.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let test = theory.Scope.Values |> Seq.toList |> List.head 
            match test with
            | :? FplRuleOfInference ->
                match box test with
                | :? IReady as testReady -> 
                    failwith("Interface IReady should not be implemented for rules of inferences because they are can be run repeatedly from different proofs with different predicates.")
                | _ -> 
                      Assert.AreEqual<int>(arity, test.Arity)
            | :? FplProof ->
                match box test with
                | :? IReady as testReady -> 
                    failwith("Interface IReady should not be implemented for proofs because they are already run from blocks implementing this interface. This, they will be run only when their caller is not ready.")
                | _ -> 
                      Assert.AreEqual<int>(arity, test.Arity)
            | _ ->
                match box test with
                | :? IReady as testReady -> 
                    if test.Arity = 0 && testReady.IsReady then
                        Assert.AreEqual<string>("arity 0 => ready", "arity 0 => ready")
                    elif test.Arity = 0 && not testReady.IsReady then
                        Assert.AreEqual<string>("arity 0 => ready", "arity 0 but not ready")
                    elif test.Arity <> 0 && testReady.IsReady then
                        Assert.AreEqual<string>("arity not 0 => not ready", "arity not 0, but ready")
                    elif test.Arity <> 0 && not testReady.IsReady then
                        Assert.AreEqual<string>("arity not 0 => not ready", "arity not 0 => not ready")
                | _ -> failwith("Interface IReady not implemented")
        | None -> 
            Assert.IsTrue(false)