namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeEvaluationType() =

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
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "inf1" -> Assert.AreEqual(FplType.Predicate, inf1.EvaluationType)
            | "inf2" -> Assert.AreEqual(FplType.Predicate, inf2.EvaluationType)
            | "axi1" -> Assert.AreEqual(FplType.Predicate, axi1.EvaluationType)
            | "axi2" -> Assert.AreEqual(FplType.Predicate, axi2.EvaluationType)
            | "pst1" -> Assert.AreEqual(FplType.Predicate, pst1.EvaluationType)
            | "pst2" -> Assert.AreEqual(FplType.Predicate, pst2.EvaluationType)
            | "thm1" -> Assert.AreEqual(FplType.Predicate, thm1.EvaluationType)
            | "thm2" -> Assert.AreEqual(FplType.Predicate, thm2.EvaluationType)
            | "pro1" -> Assert.AreEqual(FplType.Predicate, pro1.EvaluationType)
            | "pro2" -> Assert.AreEqual(FplType.Predicate, pro2.EvaluationType)
            | "lem1" -> Assert.AreEqual(FplType.Predicate, lem1.EvaluationType)
            | "lem2" -> Assert.AreEqual(FplType.Predicate, lem2.EvaluationType)
            | "cor1" -> Assert.AreEqual(FplType.Predicate, cor1.EvaluationType)
            | "cor2" -> Assert.AreEqual(FplType.Predicate, cor2.EvaluationType)
            | "con1" -> Assert.AreEqual(FplType.Predicate, con1.EvaluationType)
            | "con2" -> Assert.AreEqual(FplType.Predicate, con2.EvaluationType)
            | "cla1" -> Assert.AreEqual(FplType.Object, cla1.EvaluationType)
            | "cla2" -> Assert.AreEqual(FplType.Object, cla2.EvaluationType)
            | "pre1" -> Assert.AreEqual(FplType.Predicate, pre1.EvaluationType)
            | "pre2" -> Assert.AreEqual(FplType.Predicate, pre2.EvaluationType)
            | "fun1" -> Assert.AreEqual(FplType.Object, fun1.EvaluationType)
            | "fun2" -> Assert.AreEqual(FplType.Object, fun2.EvaluationType)
            | "prf1" -> Assert.AreEqual(FplType.Predicate, prf1.EvaluationType)
            | "prf2" -> Assert.AreEqual(FplType.Predicate, prf2.EvaluationType)
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
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "block" -> Assert.AreEqual(FplType.Object, block.EvaluationType)
            | "t1" -> Assert.AreEqual(FplType.Object, t1.EvaluationType)
            | "t2" -> Assert.AreEqual(FplType.Object, t2.EvaluationType)
            | "t3" -> Assert.AreEqual(FplType.Object, t3.EvaluationType)
            | "t4" -> Assert.AreEqual(FplType.Object, t4.EvaluationType)
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
                | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
                | "theory" -> Assert.AreEqual("", theory.QualifiedStartPos)
                | "thm1" -> Assert.AreEqual("", thm1.QualifiedStartPos)
                | "proofThm1" -> Assert.AreEqual("", proofThm1.QualifiedStartPos)
                | "lem1" -> Assert.AreEqual("", lem1.QualifiedStartPos)
                | "proofLem1" -> Assert.AreEqual("", proofLem1.QualifiedStartPos)
                | "prp1" -> Assert.AreEqual("", prp1.QualifiedStartPos)
                | "proofPrp1" -> Assert.AreEqual("", proofPrp1.QualifiedStartPos)
                | "cor1" -> Assert.AreEqual("", cor1.QualifiedStartPos)
                | "proofCor1" -> Assert.AreEqual("", proofCor1.QualifiedStartPos)
                | "thm2" -> Assert.AreEqual("", thm2.QualifiedStartPos)
                | "corThm2" -> Assert.AreEqual("", corThm2.QualifiedStartPos)
                | "lem2" -> Assert.AreEqual("", lem2.QualifiedStartPos)
                | "corLem2" -> Assert.AreEqual("", corLem2.QualifiedStartPos)
                | "prp2" -> Assert.AreEqual("", prp2.QualifiedStartPos)
                | "corPrp2" -> Assert.AreEqual("", corPrp2.QualifiedStartPos)
                | "cor2" -> Assert.AreEqual("", cor2.QualifiedStartPos)
                | "corCor2" -> Assert.AreEqual("", corCor2.QualifiedStartPos)
                | "con1" -> Assert.AreEqual("", con1.QualifiedStartPos)
                | "corCon1" -> Assert.AreEqual("", corCon1.QualifiedStartPos)
                | "axi1" -> Assert.AreEqual("", axi1.QualifiedStartPos)
                | "corAxi1"  -> Assert.AreEqual("", corAxi1.QualifiedStartPos) 
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
    member this.TestProperties(var) =
        let res = CommonFplValueTestCases.ScopeProperties() 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "block" -> Assert.AreEqual(FplType.Predicate, block.EvaluationType)
            | "t1" -> Assert.AreEqual(FplType.Predicate, t1.EvaluationType)
            | "t2" -> Assert.AreEqual(FplType.Predicate, t2.EvaluationType)
            | "t3" -> Assert.AreEqual(FplType.Object, t3.EvaluationType)
            | "t4" -> Assert.AreEqual(FplType.Object, t4.EvaluationType)
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
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "block" -> Assert.AreEqual(FplType.Predicate, block.EvaluationType)
            | "x" -> Assert.AreEqual(FplType.Object, x.EvaluationType)
            | "y" -> Assert.AreEqual(FplType.Object, y.EvaluationType)
            | "xu" -> Assert.AreEqual(FplType.Object, xu.EvaluationType)
            | "xv" -> Assert.AreEqual(FplType.Object, xv.EvaluationType)
            | "xw" -> Assert.AreEqual(FplType.Object, xw.EvaluationType)
            | "yu" -> Assert.AreEqual(FplType.Object, yu.EvaluationType)
            | "yv" -> Assert.AreEqual(FplType.Object, yv.EvaluationType)
            | "yw" -> Assert.AreEqual(FplType.Object, yw.EvaluationType)
            | "xua" -> Assert.AreEqual(FplType.Object, xua.EvaluationType)
            | "xub" -> Assert.AreEqual(FplType.Object, xub.EvaluationType)
            | "xuc" -> Assert.AreEqual(FplType.Object, xuc.EvaluationType)
            | "xva" -> Assert.AreEqual(FplType.Object, xva.EvaluationType)
            | "xvb" -> Assert.AreEqual(FplType.Object, xvb.EvaluationType)
            | "xvc" -> Assert.AreEqual(FplType.Object, xvc.EvaluationType)
            | "xwa" -> Assert.AreEqual(FplType.Object, xwa.EvaluationType)
            | "xwb" -> Assert.AreEqual(FplType.Object, xwb.EvaluationType)
            | "xwc" -> Assert.AreEqual(FplType.Object, xwc.EvaluationType)
            | "yua" -> Assert.AreEqual(FplType.Object, yua.EvaluationType)
            | "yub" -> Assert.AreEqual(FplType.Object, yub.EvaluationType)
            | "yuc" -> Assert.AreEqual(FplType.Object, yuc.EvaluationType)
            | "yva" -> Assert.AreEqual(FplType.Object, yva.EvaluationType)
            | "yvb" -> Assert.AreEqual(FplType.Object, yvb.EvaluationType)
            | "yvc" -> Assert.AreEqual(FplType.Object, yvc.EvaluationType)
            | "ywa" -> Assert.AreEqual(FplType.Object, ywa.EvaluationType)
            | "ywb" -> Assert.AreEqual(FplType.Object, ywb.EvaluationType)
            | "ywc" -> Assert.AreEqual(FplType.Object, ywc.EvaluationType)
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
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "block" -> Assert.AreEqual(FplType.Predicate, block.EvaluationType)
            | "x" -> Assert.AreEqual(FplType.Object, x.EvaluationType)
            | "y" -> Assert.AreEqual(FplType.Object, y.EvaluationType)
            | "xu" -> Assert.AreEqual(FplType.Object, xu.EvaluationType)
            | "xv" -> Assert.AreEqual(FplType.Object, xv.EvaluationType)
            | "xw" -> Assert.AreEqual(FplType.Object, xw.EvaluationType)
            | "yu" -> Assert.AreEqual(FplType.Object, yu.EvaluationType)
            | "yv" -> Assert.AreEqual(FplType.Object, yv.EvaluationType)
            | "yw" -> Assert.AreEqual(FplType.Object, yw.EvaluationType)
            | "xua" -> Assert.AreEqual(FplType.Object, xua.EvaluationType)
            | "xub" -> Assert.AreEqual(FplType.Object, xub.EvaluationType)
            | "xuc" -> Assert.AreEqual(FplType.Object, xuc.EvaluationType)
            | "xva" -> Assert.AreEqual(FplType.Object, xva.EvaluationType)
            | "xvb" -> Assert.AreEqual(FplType.Object, xvb.EvaluationType)
            | "xvc" -> Assert.AreEqual(FplType.Object, xvc.EvaluationType)
            | "xwa" -> Assert.AreEqual(FplType.Object, xwa.EvaluationType)
            | "xwb" -> Assert.AreEqual(FplType.Object, xwb.EvaluationType)
            | "xwc" -> Assert.AreEqual(FplType.Object, xwc.EvaluationType)
            | "yua" -> Assert.AreEqual(FplType.Object, yua.EvaluationType)
            | "yub" -> Assert.AreEqual(FplType.Object, yub.EvaluationType)
            | "yuc" -> Assert.AreEqual(FplType.Object, yuc.EvaluationType)
            | "yva" -> Assert.AreEqual(FplType.Object, yva.EvaluationType)
            | "yvb" -> Assert.AreEqual(FplType.Object, yvb.EvaluationType)
            | "yvc" -> Assert.AreEqual(FplType.Object, yvc.EvaluationType)
            | "ywa" -> Assert.AreEqual(FplType.Object, ywa.EvaluationType)
            | "ywb" -> Assert.AreEqual(FplType.Object, ywb.EvaluationType)
            | "ywc" -> Assert.AreEqual(FplType.Object, ywc.EvaluationType)
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
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "block" -> Assert.AreEqual(FplType.Predicate, block.EvaluationType)
            | "x" -> Assert.AreEqual(FplType.Object, x.EvaluationType)
            | "y" -> Assert.AreEqual(FplType.Object, y.EvaluationType)
            | "xu" -> Assert.AreEqual(FplType.Object, xu.EvaluationType)
            | "xv" -> Assert.AreEqual(FplType.Object, xv.EvaluationType)
            | "xw" -> Assert.AreEqual(FplType.Object, xw.EvaluationType)
            | "yu" -> Assert.AreEqual(FplType.Object, yu.EvaluationType)
            | "yv" -> Assert.AreEqual(FplType.Object, yv.EvaluationType)
            | "yw" -> Assert.AreEqual(FplType.Object, yw.EvaluationType)
            | "xua" -> Assert.AreEqual(FplType.Object, xua.EvaluationType)
            | "xub" -> Assert.AreEqual(FplType.Object, xub.EvaluationType)
            | "xuc" -> Assert.AreEqual(FplType.Object, xuc.EvaluationType)
            | "xva" -> Assert.AreEqual(FplType.Object, xva.EvaluationType)
            | "xvb" -> Assert.AreEqual(FplType.Object, xvb.EvaluationType)
            | "xvc" -> Assert.AreEqual(FplType.Object, xvc.EvaluationType)
            | "xwa" -> Assert.AreEqual(FplType.Object, xwa.EvaluationType)
            | "xwb" -> Assert.AreEqual(FplType.Object, xwb.EvaluationType)
            | "xwc" -> Assert.AreEqual(FplType.Object, xwc.EvaluationType)
            | "yua" -> Assert.AreEqual(FplType.Object, yua.EvaluationType)
            | "yub" -> Assert.AreEqual(FplType.Object, yub.EvaluationType)
            | "yuc" -> Assert.AreEqual(FplType.Object, yuc.EvaluationType)
            | "yva" -> Assert.AreEqual(FplType.Object, yva.EvaluationType)
            | "yvb" -> Assert.AreEqual(FplType.Object, yvb.EvaluationType)
            | "yvc" -> Assert.AreEqual(FplType.Object, yvc.EvaluationType)
            | "ywa" -> Assert.AreEqual(FplType.Object, ywa.EvaluationType)
            | "ywb" -> Assert.AreEqual(FplType.Object, ywb.EvaluationType)
            | "ywc" -> Assert.AreEqual(FplType.Object, ywc.EvaluationType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)