namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes
open CommonTestHelpers


[<TestClass>]
type TestFplValueScopeBlockType() =

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("inf1")>]
    [<DataRow("inf2")>]
    [<DataRow("axi1")>]
    [<DataRow("axi2")>]
    [<DataRow("pst1")>]
    [<DataRow("pst2")>]
    [<DataRow("thm1")>]
    [<DataRow("thm2")>]
    [<DataRow("pro1")>]
    [<DataRow("pro2")>]
    [<DataRow("lem1")>]
    [<DataRow("lem2")>]
    [<DataRow("cor1")>]
    [<DataRow("cor2")>]
    [<DataRow("con1")>]
    [<DataRow("con2")>]
    [<DataRow("cla1")>]
    [<DataRow("cla2")>]
    [<DataRow("pre1")>]
    [<DataRow("pre2")>]
    [<DataRow("fun1")>]
    [<DataRow("fun2")>]
    [<DataRow("prf1")>]
    [<DataRow("prf2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks() 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplValueType.Theory, theory.BlockType)
            | "inf1" -> Assert.AreEqual(FplValueType.RuleOfInference, inf1.BlockType)
            | "inf2" -> Assert.AreEqual(FplValueType.RuleOfInference, inf2.BlockType)
            | "axi1" -> Assert.AreEqual(FplValueType.Axiom, axi1.BlockType)
            | "axi2" -> Assert.AreEqual(FplValueType.Axiom, axi2.BlockType)
            | "pst1" -> Assert.AreEqual(FplValueType.Axiom, pst1.BlockType)
            | "pst2" -> Assert.AreEqual(FplValueType.Axiom, pst2.BlockType)
            | "thm1" -> Assert.AreEqual(FplValueType.Theorem, thm1.BlockType)
            | "thm2" -> Assert.AreEqual(FplValueType.Theorem, thm2.BlockType)
            | "pro1" -> Assert.AreEqual(FplValueType.Proposition, pro1.BlockType)
            | "pro2" -> Assert.AreEqual(FplValueType.Proposition, pro2.BlockType)
            | "lem1" -> Assert.AreEqual(FplValueType.Lemma, lem1.BlockType)
            | "lem2" -> Assert.AreEqual(FplValueType.Lemma, lem2.BlockType)
            | "cor1" -> Assert.AreEqual(FplValueType.Corollary, cor1.BlockType)
            | "cor2" -> Assert.AreEqual(FplValueType.Corollary, cor1.BlockType)
            | "con1" -> Assert.AreEqual(FplValueType.Conjecture, con1.BlockType)
            | "con2" -> Assert.AreEqual(FplValueType.Conjecture, con2.BlockType)
            | "cla1" -> Assert.AreEqual(FplValueType.Class, cla1.BlockType)
            | "cla2" -> Assert.AreEqual(FplValueType.Class, cla2.BlockType)
            | "pre1" -> Assert.AreEqual(FplValueType.Predicate, pre1.BlockType)
            | "pre2" -> Assert.AreEqual(FplValueType.Predicate, pre2.BlockType)
            | "fun1" -> Assert.AreEqual(FplValueType.FunctionalTerm, fun1.BlockType)
            | "fun2" -> Assert.AreEqual(FplValueType.FunctionalTerm, fun2.BlockType)
            | "prf1" -> Assert.AreEqual(FplValueType.Proof, prf1.BlockType)
            | "prf2" -> Assert.AreEqual(FplValueType.Proof, prf2.BlockType)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("t1")>]
    [<DataRow("t2")>]
    [<DataRow("t3")>]
    [<DataRow("t4")>]
    [<TestMethod>]
    member this.TestConstructors(var) =
        let res = CommonFplValueTestCases.ScopeConstructors() 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplValueType.Class, block.BlockType)
            | "t1" -> Assert.AreEqual(FplValueType.Constructor, t1.BlockType)
            | "t2" -> Assert.AreEqual(FplValueType.Constructor, t2.BlockType)
            | "t3" -> Assert.AreEqual(FplValueType.Constructor, t3.BlockType)
            | "t4" -> Assert.AreEqual(FplValueType.Constructor, t4.BlockType)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("thm1")>]
    [<DataRow("proofThm1")>]
    [<DataRow("lem1")>]
    [<DataRow("proofLem1")>]
    [<DataRow("prp1")>]
    [<DataRow("proofPrp1")>]
    [<DataRow("cor1")>]
    [<DataRow("proofCor1")>]
    [<DataRow("thm2")>]
    [<DataRow("corThm2")>]
    [<DataRow("lem2")>]
    [<DataRow("corLem2")>]
    [<DataRow("prp2")>]
    [<DataRow("corPrp2")>]
    [<DataRow("cor2")>]
    [<DataRow("corCor2")>]
    [<DataRow("con1")>]
    [<DataRow("corCon1")>]
    [<DataRow("axi1")>]
    [<DataRow("corAxi1")>]
    [<TestMethod>]
    member this.TestProofsAndCorollaries(var) =
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries() 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual(FplValueType.Root, r.BlockType)
                | "theory" -> Assert.AreEqual(FplValueType.Theory, theory.BlockType)
                | "thm1" -> Assert.AreEqual(FplValueType.Theorem, thm1.BlockType)
                | "proofThm1" -> Assert.AreEqual(FplValueType.Proof, proofThm1.BlockType)
                | "lem1" -> Assert.AreEqual(FplValueType.Lemma, lem1.BlockType)
                | "proofLem1" -> Assert.AreEqual(FplValueType.Proof, proofLem1.BlockType)
                | "prp1" -> Assert.AreEqual(FplValueType.Proposition, prp1.BlockType)
                | "proofPrp1" -> Assert.AreEqual(FplValueType.Proof, proofPrp1.BlockType)
                | "cor1" -> Assert.AreEqual(FplValueType.Corollary, cor1.BlockType)
                | "proofCor1" -> Assert.AreEqual(FplValueType.Proof, proofCor1.BlockType)
                | "thm2" -> Assert.AreEqual(FplValueType.Theorem, thm2.BlockType)
                | "corThm2" -> Assert.AreEqual(FplValueType.Corollary, corThm2.BlockType)
                | "lem2" -> Assert.AreEqual(FplValueType.Lemma, lem2.BlockType)
                | "corLem2" -> Assert.AreEqual(FplValueType.Corollary, corLem2.BlockType)
                | "prp2" -> Assert.AreEqual(FplValueType.Proposition, prp2.BlockType)
                | "corPrp2" -> Assert.AreEqual(FplValueType.Corollary, corPrp2.BlockType)
                | "cor2" -> Assert.AreEqual(FplValueType.Corollary, cor2.BlockType)
                | "corCor2" -> Assert.AreEqual(FplValueType.Corollary, corCor2.BlockType)
                | "con1" -> Assert.AreEqual(FplValueType.Conjecture, con1.BlockType)
                | "corCon1" -> Assert.AreEqual(FplValueType.Corollary, corCon1.BlockType)
                | "axi1" -> Assert.AreEqual(FplValueType.Axiom, axi1.BlockType)
                | "corAxi1"  -> Assert.AreEqual(FplValueType.Corollary, corAxi1.BlockType) 
                | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("t1")>]
    [<DataRow("t2")>]
    [<DataRow("t3")>]
    [<DataRow("t4")>]
    [<DataRow("t5")>]
    [<DataRow("t6")>]
    [<DataRow("t7")>]
    [<DataRow("t8")>]
    [<DataRow("t9")>]
    [<DataRow("t10")>]
    [<DataRow("t11")>]
    [<DataRow("t12")>]
    [<TestMethod>]
    member this.TestProperties(var) =
        let res = CommonFplValueTestCases.ScopeProperties() 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplValueType.Predicate, block.BlockType)
            | "t1" -> Assert.AreEqual(FplValueType.MandatoryPredicate, t1.BlockType)
            | "t2" -> Assert.AreEqual(FplValueType.OptionalPredicate, t2.BlockType)
            | "t3" -> Assert.AreEqual(FplValueType.MandatoryFunctionalTerm, t3.BlockType)
            | "t4" -> Assert.AreEqual(FplValueType.OptionalFunctionalTerm, t4.BlockType)
            | "t5" -> Assert.AreEqual(FplValueType.MandatoryFunctionalTerm, t5.BlockType)
            | "t6" -> Assert.AreEqual(FplValueType.OptionalFunctionalTerm, t6.BlockType)
            | "t7" -> Assert.AreEqual(FplValueType.MandatoryFunctionalTerm, t7.BlockType)
            | "t8" -> Assert.AreEqual(FplValueType.OptionalFunctionalTerm, t8.BlockType)
            | "t9" -> Assert.AreEqual(FplValueType.MandatoryFunctionalTerm, t9.BlockType)
            | "t10" -> Assert.AreEqual(FplValueType.OptionalFunctionalTerm, t10.BlockType)
            | "t11" -> Assert.AreEqual(FplValueType.MandatoryFunctionalTerm, t11.BlockType)
            | "t12" -> Assert.AreEqual(FplValueType.OptionalFunctionalTerm, t12.BlockType)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("xw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("yw")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<TestMethod>]
    member this.TestVariablesInBlock(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInBlock()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplValueType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplValueType.Variable, x.BlockType)
            | "y" -> Assert.AreEqual(FplValueType.Variable, y.BlockType)
            | "xu" -> Assert.AreEqual(FplValueType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplValueType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplValueType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplValueType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplValueType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplValueType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplValueType.Variable, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplValueType.Variable, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplValueType.Variable, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplValueType.Variable, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplValueType.Variable, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplValueType.Variable, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplValueType.Variable, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplValueType.Variable, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplValueType.Variable, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplValueType.Variable, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplValueType.Variable, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplValueType.Variable, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplValueType.Variable, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplValueType.Variable, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplValueType.Variable, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplValueType.Variable, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplValueType.Variable, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplValueType.Variable, ywc.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("xw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("yw")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<TestMethod>]
    member this.TestVariablesInBlockVariadic(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplValueType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplValueType.VariadicVariableMany1, x.BlockType)
            | "y" -> Assert.AreEqual(FplValueType.VariadicVariableMany1, y.BlockType)
            | "xu" -> Assert.AreEqual(FplValueType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplValueType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplValueType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplValueType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplValueType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplValueType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplValueType.VariadicVariableMany, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplValueType.VariadicVariableMany, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, ywc.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("xw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("yw")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<TestMethod>]
    member this.TestVariablesInSignature(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInSignature()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplValueType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplValueType.Variable, x.BlockType)
            | "y" -> Assert.AreEqual(FplValueType.Variable, y.BlockType)
            | "xu" -> Assert.AreEqual(FplValueType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplValueType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplValueType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplValueType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplValueType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplValueType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplValueType.Variable, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplValueType.Variable, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplValueType.Variable, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplValueType.Variable, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplValueType.Variable, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplValueType.Variable, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplValueType.Variable, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplValueType.Variable, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplValueType.Variable, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplValueType.Variable, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplValueType.Variable, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplValueType.Variable, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplValueType.Variable, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplValueType.Variable, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplValueType.Variable, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplValueType.Variable, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplValueType.Variable, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplValueType.Variable, ywc.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("xw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("yw")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<TestMethod>]
    member this.TestVariablesInSignatureVariadic(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplValueType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplValueType.VariadicVariableMany1, x.BlockType)
            | "y" -> Assert.AreEqual(FplValueType.VariadicVariableMany1, y.BlockType)
            | "xu" -> Assert.AreEqual(FplValueType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplValueType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplValueType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplValueType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplValueType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplValueType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplValueType.VariadicVariableMany, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplValueType.VariadicVariableMany, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplValueType.VariadicVariableMany, ywc.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("base1")>]
    [<DataRow("base2")>]
    [<DataRow("base3")>]
    [<DataRow("base4")>]
    [<DataRow("base5")>]
    [<DataRow("base6")>]
    [<DataRow("base7")>]
    [<DataRow("base8")>]
    [<DataRow("base9")>]
    [<DataRow("base10")>]
    [<DataRow("base11")>]
    [<DataRow("base12")>]
    [<DataRow("base13")>]
    [<DataRow("base11a")>]
    [<DataRow("base12a")>]
    [<DataRow("base10b")>]
    [<DataRow("base11b")>]
    [<DataRow("base12b")>]
    [<DataRow("base13b")>]
    [<DataRow("base10c")>]
    [<DataRow("base11c")>]
    [<DataRow("base12c")>]
    [<DataRow("base13c")>]
    [<DataRow("base10d")>]
    [<DataRow("base11d")>]
    [<DataRow("base12d")>]
    [<DataRow("base13d")>]
    [<DataRow("base10e")>]
    [<DataRow("base11e")>]
    [<DataRow("base12e")>]
    [<DataRow("base13e")>]
    [<DataRow("base10f")>]
    [<DataRow("base11f")>]
    [<DataRow("base12f")>]
    [<DataRow("base13f")>]
    [<DataRow("base14")>]
    [<DataRow("base15")>]
    [<DataRow("base16")>]
    [<DataRow("base17")>]
    [<DataRow("base18")>]
    [<DataRow("base19")>]
    [<DataRow("base20")>]
    [<DataRow("base21")>]
    [<DataRow("base22")>]
    [<DataRow("base23")>]
    [<DataRow("base24")>]
    [<DataRow("base25")>]
    [<DataRow("base26")>]
    [<TestMethod>]
    member this.TestPredicate(var) =
        let result = CommonFplValueTestCases.ScopePredicate()
        match result with
        | Some (base1,base2,base3,base4,base5, base6, base7, 
                                    base8, base9, base10, base11, base12, base13,
                                    base11a, base12a, base10b, base11b, base12b, base13b,
                                    base10c, base11c, base12c, base13c, base10d, base11d,
                                    base12d, base10e, base11e, base12e, base13d, base13e,
                                    base10f, base11f, base12f, base13f, base14, base15,
                                    base16, base17, base18, base19, base20, base21, base22,
                                    base23, base24, base25, base26) ->
            match var with
            | "base1" -> Assert.AreEqual(FplValueType.Expression, base1.BlockType)
            | "base2" -> Assert.AreEqual(FplValueType.Expression, base2.BlockType)
            | "base3" -> Assert.AreEqual(FplValueType.Expression, base3.BlockType)
            | "base4" -> Assert.AreEqual(FplValueType.Expression, base4.BlockType)
            | "base5" -> Assert.AreEqual(FplValueType.Expression, base5.BlockType)
            | "base6" -> Assert.AreEqual(FplValueType.Expression, base6.BlockType)
            | "base7" -> Assert.AreEqual(FplValueType.Expression, base7.BlockType)
            | "base8" -> Assert.AreEqual(FplValueType.Expression, base8.BlockType)
            | "base9" -> Assert.AreEqual(FplValueType.Expression, base9.BlockType)
            | "base10" -> Assert.AreEqual(FplValueType.Expression, base10.BlockType)
            | "base11" -> Assert.AreEqual(FplValueType.Expression, base11.BlockType)
            | "base12" -> Assert.AreEqual(FplValueType.Expression, base12.BlockType)
            | "base13" -> Assert.AreEqual(FplValueType.Expression, base13.BlockType)
            | "base11a" -> Assert.AreEqual(FplValueType.Expression, base11a.BlockType)
            | "base12a" -> Assert.AreEqual(FplValueType.Expression, base12a.BlockType)
            | "base10b" -> Assert.AreEqual(FplValueType.Expression, base10b.BlockType)
            | "base11b" -> Assert.AreEqual(FplValueType.Expression, base11b.BlockType)
            | "base12b" -> Assert.AreEqual(FplValueType.Expression, base12b.BlockType)
            | "base13b" -> Assert.AreEqual(FplValueType.Expression, base13b.BlockType)
            | "base10c" -> Assert.AreEqual(FplValueType.Expression, base10c.BlockType)
            | "base11c" -> Assert.AreEqual(FplValueType.Expression, base11c.BlockType)
            | "base12c" -> Assert.AreEqual(FplValueType.Expression, base12c.BlockType)
            | "base13c" -> Assert.AreEqual(FplValueType.Expression, base13c.BlockType)
            | "base10d" -> Assert.AreEqual(FplValueType.Expression, base10d.BlockType)
            | "base11d" -> Assert.AreEqual(FplValueType.Expression, base11d.BlockType)
            | "base12d" -> Assert.AreEqual(FplValueType.Expression, base12d.BlockType)
            | "base13d" -> Assert.AreEqual(FplValueType.Expression, base13d.BlockType)
            | "base10e" -> Assert.AreEqual(FplValueType.Expression, base10e.BlockType)
            | "base11e" -> Assert.AreEqual(FplValueType.Expression, base11e.BlockType)
            | "base12e" -> Assert.AreEqual(FplValueType.Expression, base12e.BlockType)
            | "base13e" -> Assert.AreEqual(FplValueType.Expression, base13e.BlockType)
            | "base10f" -> Assert.AreEqual(FplValueType.Expression, base10f.BlockType)
            | "base11f" -> Assert.AreEqual(FplValueType.Expression, base11f.BlockType)
            | "base12f" -> Assert.AreEqual(FplValueType.Expression, base12f.BlockType)
            | "base13f" -> Assert.AreEqual(FplValueType.Expression, base13f.BlockType)
            | "base14" -> Assert.AreEqual(FplValueType.Expression, base14.BlockType)
            | "base15" -> Assert.AreEqual(FplValueType.Expression, base15.BlockType)
            | "base16" -> Assert.AreEqual(FplValueType.Expression, base16.BlockType)
            | "base17" -> Assert.AreEqual(FplValueType.Expression, base17.BlockType)
            | "base18" -> Assert.AreEqual(FplValueType.Expression, base18.BlockType)
            | "base19" -> Assert.AreEqual(FplValueType.Expression, base19.BlockType)
            | "base20" -> Assert.AreEqual(FplValueType.Expression, base20.BlockType)
            | "base21" -> Assert.AreEqual(FplValueType.Expression, base21.BlockType)
            | "base22" -> Assert.AreEqual(FplValueType.Expression, base22.BlockType)
            | "base23" -> Assert.AreEqual(FplValueType.Expression, base23.BlockType)
            | "base24" -> Assert.AreEqual(FplValueType.Expression, base24.BlockType)
            | "base25" -> Assert.AreEqual(FplValueType.Expression, base25.BlockType)
            | "base26" -> Assert.AreEqual(FplValueType.Expression, base26.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

