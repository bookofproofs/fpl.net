namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "inf1" -> Assert.AreEqual(FplBlockType.RuleOfInference, inf1.BlockType)
            | "inf2" -> Assert.AreEqual(FplBlockType.RuleOfInference, inf2.BlockType)
            | "axi1" -> Assert.AreEqual(FplBlockType.Axiom, axi1.BlockType)
            | "axi2" -> Assert.AreEqual(FplBlockType.Axiom, axi2.BlockType)
            | "pst1" -> Assert.AreEqual(FplBlockType.Axiom, pst1.BlockType)
            | "pst2" -> Assert.AreEqual(FplBlockType.Axiom, pst2.BlockType)
            | "thm1" -> Assert.AreEqual(FplBlockType.Theorem, thm1.BlockType)
            | "thm2" -> Assert.AreEqual(FplBlockType.Theorem, thm2.BlockType)
            | "pro1" -> Assert.AreEqual(FplBlockType.Proposition, pro1.BlockType)
            | "pro2" -> Assert.AreEqual(FplBlockType.Proposition, pro2.BlockType)
            | "lem1" -> Assert.AreEqual(FplBlockType.Lemma, lem1.BlockType)
            | "lem2" -> Assert.AreEqual(FplBlockType.Lemma, lem2.BlockType)
            | "cor1" -> Assert.AreEqual(FplBlockType.Corollary, cor1.BlockType)
            | "cor2" -> Assert.AreEqual(FplBlockType.Corollary, cor1.BlockType)
            | "con1" -> Assert.AreEqual(FplBlockType.Conjecture, con1.BlockType)
            | "con2" -> Assert.AreEqual(FplBlockType.Conjecture, con2.BlockType)
            | "cla1" -> Assert.AreEqual(FplBlockType.Class, cla1.BlockType)
            | "cla2" -> Assert.AreEqual(FplBlockType.Class, cla2.BlockType)
            | "pre1" -> Assert.AreEqual(FplBlockType.Predicate, pre1.BlockType)
            | "pre2" -> Assert.AreEqual(FplBlockType.Predicate, pre2.BlockType)
            | "fun1" -> Assert.AreEqual(FplBlockType.FunctionalTerm, fun1.BlockType)
            | "fun2" -> Assert.AreEqual(FplBlockType.FunctionalTerm, fun2.BlockType)
            | "prf1" -> Assert.AreEqual(FplBlockType.Proof, prf1.BlockType)
            | "prf2" -> Assert.AreEqual(FplBlockType.Proof, prf2.BlockType)
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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Class, block.BlockType)
            | "t1" -> Assert.AreEqual(FplBlockType.Constructor, t1.BlockType)
            | "t2" -> Assert.AreEqual(FplBlockType.Constructor, t2.BlockType)
            | "t3" -> Assert.AreEqual(FplBlockType.Constructor, t3.BlockType)
            | "t4" -> Assert.AreEqual(FplBlockType.Constructor, t4.BlockType)
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
                | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
                | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
                | "thm1" -> Assert.AreEqual(FplBlockType.Theorem, thm1.BlockType)
                | "proofThm1" -> Assert.AreEqual(FplBlockType.Proof, proofThm1.BlockType)
                | "lem1" -> Assert.AreEqual(FplBlockType.Lemma, lem1.BlockType)
                | "proofLem1" -> Assert.AreEqual(FplBlockType.Proof, proofLem1.BlockType)
                | "prp1" -> Assert.AreEqual(FplBlockType.Proposition, prp1.BlockType)
                | "proofPrp1" -> Assert.AreEqual(FplBlockType.Proof, proofPrp1.BlockType)
                | "cor1" -> Assert.AreEqual(FplBlockType.Corollary, cor1.BlockType)
                | "proofCor1" -> Assert.AreEqual(FplBlockType.Proof, proofCor1.BlockType)
                | "thm2" -> Assert.AreEqual(FplBlockType.Theorem, thm2.BlockType)
                | "corThm2" -> Assert.AreEqual(FplBlockType.Corollary, corThm2.BlockType)
                | "lem2" -> Assert.AreEqual(FplBlockType.Lemma, lem2.BlockType)
                | "corLem2" -> Assert.AreEqual(FplBlockType.Corollary, corLem2.BlockType)
                | "prp2" -> Assert.AreEqual(FplBlockType.Proposition, prp2.BlockType)
                | "corPrp2" -> Assert.AreEqual(FplBlockType.Corollary, corPrp2.BlockType)
                | "cor2" -> Assert.AreEqual(FplBlockType.Corollary, cor2.BlockType)
                | "corCor2" -> Assert.AreEqual(FplBlockType.Corollary, corCor2.BlockType)
                | "con1" -> Assert.AreEqual(FplBlockType.Conjecture, con1.BlockType)
                | "corCon1" -> Assert.AreEqual(FplBlockType.Corollary, corCon1.BlockType)
                | "axi1" -> Assert.AreEqual(FplBlockType.Axiom, axi1.BlockType)
                | "corAxi1"  -> Assert.AreEqual(FplBlockType.Corollary, corAxi1.BlockType) 
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
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Predicate, block.BlockType)
            | "t1" -> Assert.AreEqual(FplBlockType.MandatoryProperty, t1.BlockType)
            | "t2" -> Assert.AreEqual(FplBlockType.OptionalProperty, t2.BlockType)
            | "t3" -> Assert.AreEqual(FplBlockType.MandatoryProperty, t3.BlockType)
            | "t4" -> Assert.AreEqual(FplBlockType.OptionalProperty, t4.BlockType)
            | "t5" -> Assert.AreEqual(FplBlockType.MandatoryProperty, t5.BlockType)
            | "t6" -> Assert.AreEqual(FplBlockType.OptionalProperty, t6.BlockType)
            | "t7" -> Assert.AreEqual(FplBlockType.MandatoryProperty, t7.BlockType)
            | "t8" -> Assert.AreEqual(FplBlockType.OptionalProperty, t8.BlockType)
            | "t9" -> Assert.AreEqual(FplBlockType.MandatoryProperty, t9.BlockType)
            | "t10" -> Assert.AreEqual(FplBlockType.OptionalProperty, t10.BlockType)
            | "t11" -> Assert.AreEqual(FplBlockType.MandatoryProperty, t11.BlockType)
            | "t12" -> Assert.AreEqual(FplBlockType.OptionalProperty, t12.BlockType)
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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplBlockType.Variable, x.BlockType)
            | "y" -> Assert.AreEqual(FplBlockType.Variable, y.BlockType)
            | "xu" -> Assert.AreEqual(FplBlockType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplBlockType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplBlockType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplBlockType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplBlockType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplBlockType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplBlockType.Variable, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplBlockType.Variable, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplBlockType.Variable, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplBlockType.Variable, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplBlockType.Variable, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplBlockType.Variable, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplBlockType.Variable, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplBlockType.Variable, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplBlockType.Variable, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplBlockType.Variable, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplBlockType.Variable, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplBlockType.Variable, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplBlockType.Variable, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplBlockType.Variable, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplBlockType.Variable, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplBlockType.Variable, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplBlockType.Variable, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplBlockType.Variable, ywc.BlockType)
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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplBlockType.VariadicVariableMany1, x.BlockType)
            | "y" -> Assert.AreEqual(FplBlockType.VariadicVariableMany1, y.BlockType)
            | "xu" -> Assert.AreEqual(FplBlockType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplBlockType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplBlockType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplBlockType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplBlockType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplBlockType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, ywc.BlockType)
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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplBlockType.Variable, x.BlockType)
            | "y" -> Assert.AreEqual(FplBlockType.Variable, y.BlockType)
            | "xu" -> Assert.AreEqual(FplBlockType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplBlockType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplBlockType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplBlockType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplBlockType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplBlockType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplBlockType.Variable, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplBlockType.Variable, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplBlockType.Variable, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplBlockType.Variable, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplBlockType.Variable, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplBlockType.Variable, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplBlockType.Variable, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplBlockType.Variable, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplBlockType.Variable, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplBlockType.Variable, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplBlockType.Variable, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplBlockType.Variable, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplBlockType.Variable, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplBlockType.Variable, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplBlockType.Variable, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplBlockType.Variable, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplBlockType.Variable, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplBlockType.Variable, ywc.BlockType)
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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplBlockType.VariadicVariableMany1, x.BlockType)
            | "y" -> Assert.AreEqual(FplBlockType.VariadicVariableMany1, y.BlockType)
            | "xu" -> Assert.AreEqual(FplBlockType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplBlockType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplBlockType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplBlockType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplBlockType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplBlockType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplBlockType.VariadicVariableMany, ywc.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
