namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeNameIsFinal() =

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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "inf1" -> Assert.AreEqual(true, inf1.NameIsFinal)
            | "inf2" -> Assert.AreEqual(true, inf2.NameIsFinal)
            | "axi1" -> Assert.AreEqual(true, axi1.NameIsFinal)
            | "axi2" -> Assert.AreEqual(true, axi2.NameIsFinal)
            | "pst1" -> Assert.AreEqual(true, pst1.NameIsFinal)
            | "pst2" -> Assert.AreEqual(true, pst2.NameIsFinal)
            | "thm1" -> Assert.AreEqual(true, thm1.NameIsFinal)
            | "thm2" -> Assert.AreEqual(true, thm2.NameIsFinal)
            | "pro1" -> Assert.AreEqual(true, pro1.NameIsFinal)
            | "pro2" -> Assert.AreEqual(true, pro2.NameIsFinal)
            | "lem1" -> Assert.AreEqual(true, lem1.NameIsFinal)
            | "lem2" -> Assert.AreEqual(true, lem2.NameIsFinal)
            | "cor1" -> Assert.AreEqual(true, cor1.NameIsFinal)
            | "cor2" -> Assert.AreEqual(true, cor2.NameIsFinal)
            | "con1" -> Assert.AreEqual(true, con1.NameIsFinal)
            | "con2" -> Assert.AreEqual(true, con2.NameIsFinal)
            | "cla1" -> Assert.AreEqual(true, cla1.NameIsFinal)
            | "cla2" -> Assert.AreEqual(true, cla2.NameIsFinal)
            | "pre1" -> Assert.AreEqual(true, pre1.NameIsFinal)
            | "pre2" -> Assert.AreEqual(true, pre2.NameIsFinal)
            | "fun1" -> Assert.AreEqual(true, fun1.NameIsFinal)
            | "fun2" -> Assert.AreEqual(true, fun2.NameIsFinal)
            | "prf1" -> Assert.AreEqual(true, prf1.NameIsFinal)
            | "prf2" -> Assert.AreEqual(true, prf2.NameIsFinal)
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
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "t1" -> Assert.AreEqual(true, t1.NameIsFinal)
            | "t2" -> Assert.AreEqual(true, t2.NameIsFinal)
            | "t3" -> Assert.AreEqual(true, t3.NameIsFinal)
            | "t4" -> Assert.AreEqual(true, t4.NameIsFinal)
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
                | "r" -> Assert.AreEqual(true, r.NameIsFinal)
                | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
                | "thm1" -> Assert.AreEqual(true, thm1.NameIsFinal)
                | "proofThm1" -> Assert.AreEqual(true, proofThm1.NameIsFinal)
                | "lem1" -> Assert.AreEqual(true, lem1.NameIsFinal)
                | "proofLem1" -> Assert.AreEqual(true, proofLem1.NameIsFinal)
                | "prp1" -> Assert.AreEqual(true, prp1.NameIsFinal)
                | "proofPrp1" -> Assert.AreEqual(true, proofPrp1.NameIsFinal)
                | "cor1" -> Assert.AreEqual(true, cor1.NameIsFinal)
                | "proofCor1" -> Assert.AreEqual(true, proofCor1.NameIsFinal)
                | "thm2" -> Assert.AreEqual(true, thm2.NameIsFinal)
                | "corThm2" -> Assert.AreEqual(true, corThm2.NameIsFinal)
                | "lem2" -> Assert.AreEqual(true, lem2.NameIsFinal)
                | "corLem2" -> Assert.AreEqual(true, corLem2.NameIsFinal)
                | "prp2" -> Assert.AreEqual(true, prp2.NameIsFinal)
                | "corPrp2" -> Assert.AreEqual(true, corPrp2.NameIsFinal)
                | "cor2" -> Assert.AreEqual(true, cor2.NameIsFinal)
                | "corCor2" -> Assert.AreEqual(true, corCor2.NameIsFinal)
                | "con1" -> Assert.AreEqual(true, con1.NameIsFinal)
                | "corCon1" -> Assert.AreEqual(true, corCon1.NameIsFinal)
                | "axi1" -> Assert.AreEqual(true, axi1.NameIsFinal)
                | "corAxi1"  -> Assert.AreEqual(true, corAxi1.NameIsFinal) 
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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "t1" -> Assert.AreEqual(true, t1.NameIsFinal)
            | "t2" -> Assert.AreEqual(true, t2.NameIsFinal)
            | "t3" -> Assert.AreEqual(true, t3.NameIsFinal)
            | "t4" -> Assert.AreEqual(true, t4.NameIsFinal)
            | "t5" -> Assert.AreEqual(true, t4.NameIsFinal)
            | "t6" -> Assert.AreEqual(true, t4.NameIsFinal)
            | "t7" -> Assert.AreEqual(true, t4.NameIsFinal)
            | "t8" -> Assert.AreEqual(true, t4.NameIsFinal)
            | "t9" -> Assert.AreEqual(true, t4.NameIsFinal)
            | "t10" -> Assert.AreEqual(true, t4.NameIsFinal)
            | "t11" -> Assert.AreEqual(true, t4.NameIsFinal)
            | "t12" -> Assert.AreEqual(true, t4.NameIsFinal)
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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual(true, y.NameIsFinal)
            | "xu" -> Assert.AreEqual(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual(true, ywc.NameIsFinal)
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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual(true, y.NameIsFinal)
            | "xu" -> Assert.AreEqual(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual(true, ywc.NameIsFinal)
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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual(true, y.NameIsFinal)
            | "xu" -> Assert.AreEqual(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual(true, ywc.NameIsFinal)
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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual(true, y.NameIsFinal)
            | "xu" -> Assert.AreEqual(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual(true, ywc.NameIsFinal)
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
            | "base1" -> Assert.AreEqual(true, base1.NameIsFinal)
            | "base2" -> Assert.AreEqual(true, base2.NameIsFinal)
            | "base3" -> Assert.AreEqual(true, base3.NameIsFinal)
            | "base4" -> Assert.AreEqual(true, base4.NameIsFinal)
            | "base5" -> Assert.AreEqual(true, base5.NameIsFinal)
            | "base6" -> Assert.AreEqual(true, base6.NameIsFinal)
            | "base7" -> Assert.AreEqual(true, base7.NameIsFinal)
            | "base8" -> Assert.AreEqual(true, base8.NameIsFinal)
            | "base9" -> Assert.AreEqual(true, base9.NameIsFinal)
            | "base10" -> Assert.AreEqual(true, base10.NameIsFinal)
            | "base11" -> Assert.AreEqual(true, base11.NameIsFinal)
            | "base12" -> Assert.AreEqual(true, base12.NameIsFinal)
            | "base13" -> Assert.AreEqual(true, base13.NameIsFinal)
            | "base11a" -> Assert.AreEqual(true, base11a.NameIsFinal)
            | "base12a" -> Assert.AreEqual(true, base12a.NameIsFinal)
            | "base10b" -> Assert.AreEqual(true, base10b.NameIsFinal)
            | "base11b" -> Assert.AreEqual(true, base11b.NameIsFinal)
            | "base12b" -> Assert.AreEqual(true, base12b.NameIsFinal)
            | "base13b" -> Assert.AreEqual(true, base13b.NameIsFinal)
            | "base10c" -> Assert.AreEqual(true, base10c.NameIsFinal)
            | "base11c" -> Assert.AreEqual(true, base11c.NameIsFinal)
            | "base12c" -> Assert.AreEqual(true, base12c.NameIsFinal)
            | "base13c" -> Assert.AreEqual(true, base13c.NameIsFinal)
            | "base10d" -> Assert.AreEqual(true, base10d.NameIsFinal)
            | "base11d" -> Assert.AreEqual(true, base11d.NameIsFinal)
            | "base12d" -> Assert.AreEqual(true, base12d.NameIsFinal)
            | "base13d" -> Assert.AreEqual(true, base13d.NameIsFinal)
            | "base10e" -> Assert.AreEqual(true, base10e.NameIsFinal)
            | "base11e" -> Assert.AreEqual(true, base11e.NameIsFinal)
            | "base12e" -> Assert.AreEqual(true, base12e.NameIsFinal)
            | "base13e" -> Assert.AreEqual(true, base13e.NameIsFinal)
            | "base10f" -> Assert.AreEqual(true, base10f.NameIsFinal)
            | "base11f" -> Assert.AreEqual(true, base11f.NameIsFinal)
            | "base12f" -> Assert.AreEqual(true, base12f.NameIsFinal)
            | "base13f" -> Assert.AreEqual(true, base13f.NameIsFinal)
            | "base14" -> Assert.AreEqual(true, base14.NameIsFinal)
            | "base15" -> Assert.AreEqual(true, base15.NameIsFinal)
            | "base16" -> Assert.AreEqual(true, base16.NameIsFinal)
            | "base17" -> Assert.AreEqual(true, base17.NameIsFinal)
            | "base18" -> Assert.AreEqual(true, base18.NameIsFinal)
            | "base19" -> Assert.AreEqual(true, base19.NameIsFinal)
            | "base20" -> Assert.AreEqual(true, base20.NameIsFinal)
            | "base21" -> Assert.AreEqual(true, base21.NameIsFinal)
            | "base22" -> Assert.AreEqual(true, base22.NameIsFinal)
            | "base23" -> Assert.AreEqual(true, base23.NameIsFinal)
            | "base24" -> Assert.AreEqual(true, base24.NameIsFinal)
            | "base25" -> Assert.AreEqual(true, base25.NameIsFinal)
            | "base26" -> Assert.AreEqual(true, base26.NameIsFinal)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)