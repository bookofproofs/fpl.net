namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
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
    [<DataRow("loc1")>]
    [<DataRow("loc2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("BlockType") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplValueType>(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual<FplValueType>(FplValueType.Theory, theory.BlockType)
            | "inf1" -> Assert.AreEqual<FplValueType>(FplValueType.RuleOfInference, inf1.BlockType)
            | "inf2" -> Assert.AreEqual<FplValueType>(FplValueType.RuleOfInference, inf2.BlockType)
            | "axi1" -> Assert.AreEqual<FplValueType>(FplValueType.Axiom, axi1.BlockType)
            | "axi2" -> Assert.AreEqual<FplValueType>(FplValueType.Axiom, axi2.BlockType)
            | "pst1" -> Assert.AreEqual<FplValueType>(FplValueType.Axiom, pst1.BlockType)
            | "pst2" -> Assert.AreEqual<FplValueType>(FplValueType.Axiom, pst2.BlockType)
            | "thm1" -> Assert.AreEqual<FplValueType>(FplValueType.Theorem, thm1.BlockType)
            | "thm2" -> Assert.AreEqual<FplValueType>(FplValueType.Theorem, thm2.BlockType)
            | "pro1" -> Assert.AreEqual<FplValueType>(FplValueType.Proposition, pro1.BlockType)
            | "pro2" -> Assert.AreEqual<FplValueType>(FplValueType.Proposition, pro2.BlockType)
            | "lem1" -> Assert.AreEqual<FplValueType>(FplValueType.Lemma, lem1.BlockType)
            | "lem2" -> Assert.AreEqual<FplValueType>(FplValueType.Lemma, lem2.BlockType)
            | "cor1" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, cor1.BlockType)
            | "cor2" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, cor1.BlockType)
            | "con1" -> Assert.AreEqual<FplValueType>(FplValueType.Conjecture, con1.BlockType)
            | "con2" -> Assert.AreEqual<FplValueType>(FplValueType.Conjecture, con2.BlockType)
            | "cla1" -> Assert.AreEqual<FplValueType>(FplValueType.Class, cla1.BlockType)
            | "cla2" -> Assert.AreEqual<FplValueType>(FplValueType.Class, cla2.BlockType)
            | "pre1" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, pre1.BlockType)
            | "pre2" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, pre2.BlockType)
            | "fun1" -> Assert.AreEqual<FplValueType>(FplValueType.FunctionalTerm, fun1.BlockType)
            | "fun2" -> Assert.AreEqual<FplValueType>(FplValueType.FunctionalTerm, fun2.BlockType)
            | "prf1" -> Assert.AreEqual<FplValueType>(FplValueType.Proof, prf1.BlockType)
            | "prf2" -> Assert.AreEqual<FplValueType>(FplValueType.Proof, prf2.BlockType)
            | "loc1" -> Assert.AreEqual<FplValueType>(FplValueType.Localization, loc1.BlockType)
            | "loc2" -> Assert.AreEqual<FplValueType>(FplValueType.Localization, loc2.BlockType)
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
        let res = CommonFplValueTestCases.ScopeConstructors("BlockType") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplValueType>(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual<FplValueType>(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual<FplValueType>(FplValueType.Class, block.BlockType)
            | "t1" -> Assert.AreEqual<FplValueType>(FplValueType.Constructor, t1.BlockType)
            | "t2" -> Assert.AreEqual<FplValueType>(FplValueType.Constructor, t2.BlockType)
            | "t3" -> Assert.AreEqual<FplValueType>(FplValueType.Constructor, t3.BlockType)
            | "t4" -> Assert.AreEqual<FplValueType>(FplValueType.Constructor, t4.BlockType)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("BlockType") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<FplValueType>(FplValueType.Root, r.BlockType)
                | "theory" -> Assert.AreEqual<FplValueType>(FplValueType.Theory, theory.BlockType)
                | "thm1" -> Assert.AreEqual<FplValueType>(FplValueType.Theorem, thm1.BlockType)
                | "proofThm1" -> Assert.AreEqual<FplValueType>(FplValueType.Proof, proofThm1.BlockType)
                | "lem1" -> Assert.AreEqual<FplValueType>(FplValueType.Lemma, lem1.BlockType)
                | "proofLem1" -> Assert.AreEqual<FplValueType>(FplValueType.Proof, proofLem1.BlockType)
                | "prp1" -> Assert.AreEqual<FplValueType>(FplValueType.Proposition, prp1.BlockType)
                | "proofPrp1" -> Assert.AreEqual<FplValueType>(FplValueType.Proof, proofPrp1.BlockType)
                | "cor1" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, cor1.BlockType)
                | "proofCor1" -> Assert.AreEqual<FplValueType>(FplValueType.Proof, proofCor1.BlockType)
                | "thm2" -> Assert.AreEqual<FplValueType>(FplValueType.Theorem, thm2.BlockType)
                | "corThm2" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, corThm2.BlockType)
                | "lem2" -> Assert.AreEqual<FplValueType>(FplValueType.Lemma, lem2.BlockType)
                | "corLem2" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, corLem2.BlockType)
                | "prp2" -> Assert.AreEqual<FplValueType>(FplValueType.Proposition, prp2.BlockType)
                | "corPrp2" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, corPrp2.BlockType)
                | "cor2" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, cor2.BlockType)
                | "corCor2" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, corCor2.BlockType)
                | "con1" -> Assert.AreEqual<FplValueType>(FplValueType.Conjecture, con1.BlockType)
                | "corCon1" -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, corCon1.BlockType)
                | "axi1" -> Assert.AreEqual<FplValueType>(FplValueType.Axiom, axi1.BlockType)
                | "corAxi1"  -> Assert.AreEqual<FplValueType>(FplValueType.Corollary, corAxi1.BlockType) 
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
    [<DataRow("t13")>]
    [<DataRow("t14")>]
    [<TestMethod>]
    member this.TestProperties(var) =
        let res = CommonFplValueTestCases.ScopeProperties("BlockType") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplValueType>(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual<FplValueType>(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, block.BlockType)
            | "t1" -> Assert.AreEqual<FplValueType>(FplValueType.MandatoryPredicate, t1.BlockType)
            | "t2" -> Assert.AreEqual<FplValueType>(FplValueType.OptionalPredicate, t2.BlockType)
            | "t3" -> Assert.AreEqual<FplValueType>(FplValueType.MandatoryFunctionalTerm, t3.BlockType)
            | "t4" -> Assert.AreEqual<FplValueType>(FplValueType.OptionalFunctionalTerm, t4.BlockType)
            | "t5" -> Assert.AreEqual<FplValueType>(FplValueType.MandatoryFunctionalTerm, t5.BlockType)
            | "t6" -> Assert.AreEqual<FplValueType>(FplValueType.OptionalFunctionalTerm, t6.BlockType)
            | "t7" -> Assert.AreEqual<FplValueType>(FplValueType.MandatoryFunctionalTerm, t7.BlockType)
            | "t8" -> Assert.AreEqual<FplValueType>(FplValueType.OptionalFunctionalTerm, t8.BlockType)
            | "t9" -> Assert.AreEqual<FplValueType>(FplValueType.MandatoryFunctionalTerm, t9.BlockType)
            | "t10" -> Assert.AreEqual<FplValueType>(FplValueType.OptionalFunctionalTerm, t10.BlockType)
            | "t11" -> Assert.AreEqual<FplValueType>(FplValueType.MandatoryFunctionalTerm, t11.BlockType)
            | "t12" -> Assert.AreEqual<FplValueType>(FplValueType.OptionalFunctionalTerm, t12.BlockType)
            | "t13" -> Assert.AreEqual<FplValueType>(FplValueType.MandatoryFunctionalTerm, t13.BlockType)
            | "t14" -> Assert.AreEqual<FplValueType>(FplValueType.OptionalFunctionalTerm, t14.BlockType)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("s")>]
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("BlockType")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FplValueType>(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual<FplValueType>(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, x.BlockType)
            | "y" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, y.BlockType)
            | "s" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, s.BlockType)
            | "xu" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xua.BlockType)
            | "xub" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xub.BlockType)
            | "xuc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xuc.BlockType)
            | "xva" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xva.BlockType)
            | "xvb" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xvb.BlockType)
            | "xvc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xvc.BlockType)
            | "xwa" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xwa.BlockType)
            | "xwb" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xwb.BlockType)
            | "xwc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xwc.BlockType)
            | "yua" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yua.BlockType)
            | "yub" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yub.BlockType)
            | "yuc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yuc.BlockType)
            | "yva" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yva.BlockType)
            | "yvb" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yvb.BlockType)
            | "yvc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yvc.BlockType)
            | "ywa" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, ywa.BlockType)
            | "ywb" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, ywb.BlockType)
            | "ywc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, ywc.BlockType)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FplValueType>(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual<FplValueType>(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany1, x.BlockType)
            | "y" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany1, y.BlockType)
            | "xu" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xua.BlockType)
            | "xub" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xub.BlockType)
            | "xuc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xuc.BlockType)
            | "xva" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xva.BlockType)
            | "xvb" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xvb.BlockType)
            | "xvc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xvc.BlockType)
            | "xwa" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xwa.BlockType)
            | "xwb" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xwb.BlockType)
            | "xwc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xwc.BlockType)
            | "yua" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yua.BlockType)
            | "yub" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yub.BlockType)
            | "yuc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yuc.BlockType)
            | "yva" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yva.BlockType)
            | "yvb" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yvb.BlockType)
            | "yvc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yvc.BlockType)
            | "ywa" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, ywa.BlockType)
            | "ywb" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, ywb.BlockType)
            | "ywc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, ywc.BlockType)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FplValueType>(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual<FplValueType>(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, x.BlockType)
            | "y" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, y.BlockType)
            | "xu" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xua.BlockType)
            | "xub" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xub.BlockType)
            | "xuc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xuc.BlockType)
            | "xva" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xva.BlockType)
            | "xvb" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xvb.BlockType)
            | "xvc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xvc.BlockType)
            | "xwa" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xwa.BlockType)
            | "xwb" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xwb.BlockType)
            | "xwc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xwc.BlockType)
            | "yua" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yua.BlockType)
            | "yub" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yub.BlockType)
            | "yuc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yuc.BlockType)
            | "yva" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yva.BlockType)
            | "yvb" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yvb.BlockType)
            | "yvc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yvc.BlockType)
            | "ywa" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, ywa.BlockType)
            | "ywb" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, ywb.BlockType)
            | "ywc" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, ywc.BlockType)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FplValueType>(FplValueType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual<FplValueType>(FplValueType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany1, x.BlockType)
            | "y" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany1, y.BlockType)
            | "xu" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual<FplValueType>(FplValueType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xua.BlockType)
            | "xub" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xub.BlockType)
            | "xuc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xuc.BlockType)
            | "xva" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xva.BlockType)
            | "xvb" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xvb.BlockType)
            | "xvc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xvc.BlockType)
            | "xwa" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xwa.BlockType)
            | "xwb" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xwb.BlockType)
            | "xwc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, xwc.BlockType)
            | "yua" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yua.BlockType)
            | "yub" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yub.BlockType)
            | "yuc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yuc.BlockType)
            | "yva" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yva.BlockType)
            | "yvb" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yvb.BlockType)
            | "yvc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, yvc.BlockType)
            | "ywa" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, ywa.BlockType)
            | "ywb" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, ywb.BlockType)
            | "ywc" -> Assert.AreEqual<FplValueType>(FplValueType.VariadicVariableMany, ywc.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "true")>]
    [<DataRow("base2", "false")>]
    [<DataRow("base3", "undef")>]
    [<DataRow("base4", "1.")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "$1")>]
    [<DataRow("base7", "bydef Test()")>] 
    [<DataRow("base8", "Test$1")>]
    [<DataRow("base9", "Test$1()")>]
    [<DataRow("base10", "Test")>]
    [<DataRow("base11", "v")>]
    [<DataRow("base12", "self")>]
    [<DataRow("base13", "1")>]
    [<DataRow("base11a", "v.x")>]
    [<DataRow("base12a", "self.x")>]
    [<DataRow("base10b", "Test()")>]
    [<DataRow("base11b", "v()")>]
    [<DataRow("base12b", "self()")>]
    [<DataRow("base13b", "1()")>]
    [<DataRow("base10c", "Test(x, y)")>]
    [<DataRow("base11c", "v(x, y)")>]
    [<DataRow("base12c", "self(x, y)")>]
    [<DataRow("base13c", "1(x, y)")>]
    [<DataRow("base10d", "Test[x, y]")>]
    [<DataRow("base11d", "v[x, y]")>]
    [<DataRow("base12d", "self[x, y]")>]
    [<DataRow("base13d", "1[x.y]")>]
    [<DataRow("base10e", "Test(x, y).@self[a, b]")>]
    [<DataRow("base11e", "v(x, y).x[a, b]")>]
    [<DataRow("base12e", "self(x, y).3[a, b]")>]
    [<DataRow("base13e", "1(x, y).T[a, b]")>]
    [<DataRow("base10f", "Test[x, y].x(a, b)")>]
    [<DataRow("base11f", "v[x, y].x(a, b)")>]
    [<DataRow("base12f", "self[x, y].self(a, b)")>]
    [<DataRow("base13f", "1[x.y].T(a, b)")>]
    [<DataRow("base14", "∅")>]
    [<DataRow("base15", "-x")>]
    [<DataRow("base15a", "x'")>]
    [<DataRow("base15b", "-x'")>]
    [<DataRow("base16", "-(y + x = 2 * x)")>]
    [<DataRow("base17", "(y + x' = 2 * x)'")>]
    [<DataRow("base18", "ex x:Range(a:T), y:C, z:obj {and (a,b,c)}")>]
    [<DataRow("base19", "exn$1 x:obj {all y:N {true}}")>]
    [<DataRow("base20", "all x:obj {not x}")>]
    [<DataRow("base21", "and (x, y, z)")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not (x)")>]
    [<DataRow("base22", "xor (x, y, z)")>]
    [<DataRow("base23", "or (x, y, z)")>]
    [<DataRow("base24", "iif (x, y)")>]
    [<DataRow("base25", "impl (x, y)")>]
    [<DataRow("base26", "is (x, Nat)")>]
    [<DataRow("base27", "B()")>]
    [<DataRow("base28", "C(a,b,c,d)")>]
    [<DataRow("base29", "D(self,b,c)")>]
    [<DataRow("base30", "B(In(x))")>]
    [<DataRow("base31", "C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base32", "E(true, undef, false)")>]
    [<DataRow("base33", "dec ~p: pred(c: obj); p(c)")>]
    [<DataRow("base34", "is(x, Set)")>]
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestPredicateBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base2" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base3" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base4" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base5" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base6" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base7" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base8" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base9" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base10" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base11" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base12" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base13" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base11a" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base12a" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base10b" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base11b" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base12b" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base13b" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base10c" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base11c" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base12c" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base13c" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base10d" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base11d" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base12d" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base13d" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base10e" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base11e" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base12e" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base13e" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base10f" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base11f" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base12f" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base13f" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base14" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base15" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base15a" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base15b" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base16" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base17" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base18" -> Assert.AreEqual<FplValueType>(FplValueType.Quantor, base1.BlockType)
            | "base19" -> Assert.AreEqual<FplValueType>(FplValueType.Quantor, base1.BlockType)
            | "base20" -> Assert.AreEqual<FplValueType>(FplValueType.Quantor, base1.BlockType)
            | "base21" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base21a" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base21b" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base22" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base23" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base24" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base25" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base26" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base27" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base28" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base29" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base30" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base31" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base32" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base33" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base34" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "base.B()")>]
    [<DataRow("base2", "base.C(a, b, c, d)")>]
    [<DataRow("base3", "base.D(self, a, b)")>]
    [<DataRow("base4", "base.B(In(x))")>]
    [<DataRow("base5", "base.C(Test1(a), Test2(b, c, d))")>]
    [<DataRow("base6", "base.E(true, undef, false)")>]
    [<TestMethod>]
    member this.TestCallConstructorParentClass(var, varVal) =
        ad.Clear()
        let fplCode = sprintf """
                        def cl B:obj {intr}
                        def cl C:obj {intr}
                        def cl D:obj {intr}

                        def cl A:B,C,D,E
                        {
                            ctor A(a:T1, b:func, c:ind, d:pred) 
                            {
                                dec
                                    %s
                                ;
                                self
                            }
                        }
                        ;""" varVal
        let filename = "TestCallConstructorParentClassBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let cl = theory.Scope["A"]
            let ctor = cl.Scope["A(T1, func, ind, pred)"]
            let stmt = ctor.ValueList[0]
            let base1 = stmt.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base2" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base3" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base4" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base5" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base6" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "del.B()")>]
    [<DataRow("base2", "del.C(a,b,c,d)")>]
    [<DataRow("base3", "del.D(self,b,c)")>]
    [<DataRow("base4", "del.B(In(x))")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "del.C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base7", "del.E(true, undef, false)")>] 
    [<TestMethod>]
    member this.TestDelegate(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestDelegateBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base2" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base3" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base4" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base5" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base6" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | "base7" -> Assert.AreEqual<FplValueType>(FplValueType.Reference, base1.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """def pred T1() {intr};""")>]
    [<DataRow("base2", """def pred infix ">" -1 T1() {intr};""")>]
    [<DataRow("base3", """def pred postfix "'" T1() {intr};""")>]
    [<DataRow("base4", """def pred prefix "-" T1() {intr};""")>]
    [<DataRow("base5", """def cl symbol "∅" T1:obj {intr};""")>]
    [<DataRow("base5a", """def cl T1:obj {intr};""")>]
    [<DataRow("base6", """def func T1()->obj {intr};""")>]
    [<DataRow("base7", """def func infix ">" -1 T1()->obj {intr};""")>]
    [<DataRow("base8", """def func postfix "'" T1()->obj {intr};""")>]
    [<DataRow("base9", """def func prefix "-" T1()->obj {intr};""")>]
    [<TestMethod>]
    member this.TestFixNotation(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestFixNotationBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = 
                if varVal.Contains "cl" then 
                    theory.Scope["T1"]
                elif varVal.Contains "func" then 
                    theory.Scope["T1() -> obj"]
                else 
                    theory.Scope["T1()"]

            match var with
            | "base1" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, base1.BlockType)
            | "base2" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, base1.BlockType)
            | "base3" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, base1.BlockType)
            | "base4" -> Assert.AreEqual<FplValueType>(FplValueType.Predicate, base1.BlockType)
            | "base5" -> Assert.AreEqual<FplValueType>(FplValueType.Class, base1.BlockType)
            | "base5a" -> Assert.AreEqual<FplValueType>(FplValueType.Class, base1.BlockType)
            | "base6" -> Assert.AreEqual<FplValueType>(FplValueType.FunctionalTerm, base1.BlockType)
            | "base7" -> Assert.AreEqual<FplValueType>(FplValueType.FunctionalTerm, base1.BlockType)
            | "base8" -> Assert.AreEqual<FplValueType>(FplValueType.FunctionalTerm, base1.BlockType)
            | "base9" -> Assert.AreEqual<FplValueType>(FplValueType.FunctionalTerm, base1.BlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
