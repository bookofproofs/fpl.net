namespace FplInterpreter.Tests
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValue() =



    [<TestMethod>]
    member this.TestInitialFactory() =
        FplParser.parserDiagnostics.Clear()
        let r = FplValue.CreateRoot()
        Assert.AreEqual (FplBlockType.Root, r.BlockType)
        let testFactory fplBlockType fplType =
            let fv = FplValue.CreateFplValue((Position("",0,1,1),Position("",0,1,1)), fplBlockType, r)
            Assert.AreEqual (fplBlockType, fv.BlockType)
            Assert.AreEqual ("", fv.Name)
            Assert.AreEqual ("", fv.FplRepresentation)
            Assert.AreEqual ([], fv.TypeSignature)
            Assert.AreEqual (fplType, fv.EvaluationType)
        testFactory FplBlockType.VariadicVariable FplType.Object
        testFactory FplBlockType.Axiom FplType.Predicate
        testFactory FplBlockType.Theorem FplType.Predicate
        testFactory FplBlockType.Lemma FplType.Predicate
        testFactory FplBlockType.Proposition FplType.Predicate
        testFactory FplBlockType.Corollary FplType.Predicate
        testFactory FplBlockType.Conjecture FplType.Predicate
        testFactory FplBlockType.Premise FplType.Predicate
        testFactory FplBlockType.Conclusion FplType.Predicate
        testFactory FplBlockType.Proof FplType.Predicate
        testFactory FplBlockType.RuleOfInference FplType.Predicate
        testFactory FplBlockType.Expression FplType.Predicate
        testFactory FplBlockType.Theory FplType.Predicate
        testFactory FplBlockType.Predicate FplType.Predicate 
        testFactory FplBlockType.Constructor FplType.Object
        testFactory FplBlockType.FunctionalTerm FplType.Object
        testFactory FplBlockType.Variable FplType.Object
        testFactory FplBlockType.VariadicVariable FplType.Object
        testFactory FplBlockType.MandatoryProperty FplType.Object
        testFactory FplBlockType.OptionalProperty FplType.Object
        testFactory FplBlockType.Class FplType.Object 


    member this.ScopeVariablesInSignature() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestPredicate(x,y:pred(u,v,w:func(a,b,c:obj))) 
            {true}
        ;
        """
        let stOption = prepareFplCode(fplCode, false) 
        let result = match stOption with
                        | Some st -> 
                            let name = "TestPredicate(pred(func(obj, obj, obj), func(obj, obj, obj), func(obj, obj, obj)), pred(func(obj, obj, obj), func(obj, obj, obj), func(obj, obj, obj)))"
                            let r = st.Root
                            let theory = r.Scope["Test"]
                            let block = theory.Scope[name]
                            let x = block.Scope["x"]
                            let y = block.Scope["y"]
                            let xw = x.Scope["w"]
                            let xu = x.Scope["u"]
                            let xv = x.Scope["v"]
                            let yw = y.Scope["w"]
                            let yu = y.Scope["u"]
                            let yv = y.Scope["v"]
                            let xwa = xw.Scope["a"]
                            let xwb = xw.Scope["b"]
                            let xwc = xw.Scope["c"]
                            let xua = xu.Scope["a"]
                            let xub = xu.Scope["b"]
                            let xuc = xu.Scope["c"]
                            let xva = xv.Scope["a"]
                            let xvb = xv.Scope["b"]
                            let xvc = xv.Scope["c"]
                            let ywa = yw.Scope["a"]
                            let ywb = yw.Scope["b"]
                            let ywc = yw.Scope["c"]
                            let yua = yu.Scope["a"]
                            let yub = yu.Scope["b"]
                            let yuc = yu.Scope["c"]
                            let yva = yv.Scope["a"]
                            let yvb = yv.Scope["b"]
                            let yvc = yv.Scope["c"]
                            Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc)
                        | None -> None
        prepareFplCode("", true) |> ignore
        result

    [<TestMethod>]
    member this.TestScopeVariablesInSignatureIsComplete() =
        try
            this.ScopeVariablesInSignature() |> ignore
            Assert.IsTrue(true)
        with
        | ex -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestScopeVariablesInSignatureTypeSignature() =
        let result = this.ScopeVariablesInSignature()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->

            Assert.AreEqual(["obj"], ywc.TypeSignature)
            Assert.AreEqual(["obj"], ywb.TypeSignature)
            Assert.AreEqual(["obj"], ywa.TypeSignature)
            Assert.AreEqual(["obj"], yvc.TypeSignature)
            Assert.AreEqual(["obj"], yvb.TypeSignature)
            Assert.AreEqual(["obj"], yva.TypeSignature)
            Assert.AreEqual(["obj"], yuc.TypeSignature)
            Assert.AreEqual(["obj"], yub.TypeSignature)
            Assert.AreEqual(["obj"], yua.TypeSignature)
            Assert.AreEqual(["obj"], xwc.TypeSignature)
            Assert.AreEqual(["obj"], xwb.TypeSignature)
            Assert.AreEqual(["obj"], xwa.TypeSignature)
            Assert.AreEqual(["obj"], xvc.TypeSignature)
            Assert.AreEqual(["obj"], xvb.TypeSignature)
            Assert.AreEqual(["obj"], xva.TypeSignature)
            Assert.AreEqual(["obj"], xuc.TypeSignature)
            Assert.AreEqual(["obj"], xub.TypeSignature)
            Assert.AreEqual(["obj"], xua.TypeSignature)

            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], yw.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], yv.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], yu.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], xw.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], xv.TypeSignature)
            Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"], xu.TypeSignature)
            Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; ")"], y.TypeSignature)
            Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; ")"], x.TypeSignature)
            Assert.AreEqual(["TestPredicate"; "("; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; ")"; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; "func"; "("; "obj"; "obj"; "obj"; ")"; ")"; ")"], block.TypeSignature)
            Assert.AreEqual([], theory.TypeSignature)
            Assert.AreEqual([], r.TypeSignature)
        | None -> ()
