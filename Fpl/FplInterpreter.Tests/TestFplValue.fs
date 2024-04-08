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
            Assert.AreEqual ("", fv.TypeSignature)
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

    [<TestMethod>]
    member this.TestTypeSignature() =
        FplParser.parserDiagnostics.Clear()
        let fplCode = """
        def pred TestPredicate(x,y:pred(u,v,w:func(a,b,c:obj))) 
            {true}
        ;
        """
        let result = prepareFplCode(fplCode, false) 
        match result with
        | Some st -> 
            let name = "TestPredicate(2:predicate3:3:(3:))"
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

            Assert.AreEqual("object", ywc.TypeSignature)
            Assert.AreEqual("object", ywb.TypeSignature)
            Assert.AreEqual("object", ywa.TypeSignature)
            Assert.AreEqual("object", yvc.TypeSignature)
            Assert.AreEqual("object", yvb.TypeSignature)
            Assert.AreEqual("object", yva.TypeSignature)
            Assert.AreEqual("object", yuc.TypeSignature)
            Assert.AreEqual("object", yub.TypeSignature)
            Assert.AreEqual("object", yua.TypeSignature)
            Assert.AreEqual("object", xwc.TypeSignature)
            Assert.AreEqual("object", xwb.TypeSignature)
            Assert.AreEqual("object", xwa.TypeSignature)
            Assert.AreEqual("object", xvc.TypeSignature)
            Assert.AreEqual("object", xvb.TypeSignature)
            Assert.AreEqual("object", xva.TypeSignature)
            Assert.AreEqual("object", xuc.TypeSignature)
            Assert.AreEqual("object", xub.TypeSignature)
            Assert.AreEqual("object", xua.TypeSignature)

            Assert.AreEqual("function(3:object)", yw.TypeSignature)
            Assert.AreEqual("function(3:object)", yv.TypeSignature)
            Assert.AreEqual("function(3:object)", yu.TypeSignature)
            Assert.AreEqual("function(3:object)", xw.TypeSignature)
            Assert.AreEqual("function(3:object)", yv.TypeSignature)
            Assert.AreEqual("function(3:object)", xu.TypeSignature)
            Assert.AreEqual("predicate(3:function(3:object))", y.TypeSignature)
            Assert.AreEqual("predicate(3:function(3:object))", x.TypeSignature)
            Assert.AreEqual("TestPredicate(2:predicate(3:function(3:object)))", block.TypeSignature)
            Assert.AreEqual("", theory.TypeSignature)
            Assert.AreEqual("", r.TypeSignature)
        | None -> ()
        prepareFplCode("", true) |> ignore