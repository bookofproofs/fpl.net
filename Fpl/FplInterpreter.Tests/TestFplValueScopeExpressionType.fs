namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeExpressionType() =

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
            | "r" -> Assert.AreEqual(ExprType.NoType, r.ExpressionType)
            | "theory" -> Assert.AreEqual(ExprType.NoType, theory.ExpressionType)
            | "inf1" -> Assert.AreEqual(ExprType.NoType, inf1.ExpressionType)
            | "inf2" -> Assert.AreEqual(ExprType.NoType, inf2.ExpressionType)
            | "axi1" -> Assert.AreEqual(ExprType.NoType, axi1.ExpressionType)
            | "axi2" -> Assert.AreEqual(ExprType.NoType, axi2.ExpressionType)
            | "pst1" -> Assert.AreEqual(ExprType.NoType, pst1.ExpressionType)
            | "pst2" -> Assert.AreEqual(ExprType.NoType, pst2.ExpressionType)
            | "thm1" -> Assert.AreEqual(ExprType.NoType, thm1.ExpressionType)
            | "thm2" -> Assert.AreEqual(ExprType.NoType, thm2.ExpressionType)
            | "pro1" -> Assert.AreEqual(ExprType.NoType, pro1.ExpressionType)
            | "pro2" -> Assert.AreEqual(ExprType.NoType, pro2.ExpressionType)
            | "lem1" -> Assert.AreEqual(ExprType.NoType, lem1.ExpressionType)
            | "lem2" -> Assert.AreEqual(ExprType.NoType, lem2.ExpressionType)
            | "cor1" -> Assert.AreEqual(ExprType.NoType, cor1.ExpressionType)
            | "cor2" -> Assert.AreEqual(ExprType.NoType, cor1.ExpressionType)
            | "con1" -> Assert.AreEqual(ExprType.NoType, con1.ExpressionType)
            | "con2" -> Assert.AreEqual(ExprType.NoType, con2.ExpressionType)
            | "cla1" -> Assert.AreEqual(ExprType.NoType, cla1.ExpressionType)
            | "cla2" -> Assert.AreEqual(ExprType.NoType, cla2.ExpressionType)
            | "pre1" -> Assert.AreEqual(ExprType.NoType, pre1.ExpressionType)
            | "pre2" -> Assert.AreEqual(ExprType.NoType, pre2.ExpressionType)
            | "fun1" -> Assert.AreEqual(ExprType.NoType, fun1.ExpressionType)
            | "fun2" -> Assert.AreEqual(ExprType.NoType, fun2.ExpressionType)
            | "prf1" -> Assert.AreEqual(ExprType.NoType, prf1.ExpressionType)
            | "prf2" -> Assert.AreEqual(ExprType.NoType, prf2.ExpressionType)
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
            | "r" -> Assert.AreEqual(ExprType.NoType, r.ExpressionType)
            | "theory" -> Assert.AreEqual(ExprType.NoType, theory.ExpressionType)
            | "block" -> Assert.AreEqual(ExprType.NoType, block.ExpressionType)
            | "t1" -> Assert.AreEqual(ExprType.NoType, t1.ExpressionType)
            | "t2" -> Assert.AreEqual(ExprType.NoType, t2.ExpressionType)
            | "t3" -> Assert.AreEqual(ExprType.NoType, t3.ExpressionType)
            | "t4" -> Assert.AreEqual(ExprType.NoType, t4.ExpressionType)
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
                | "r" -> Assert.AreEqual(ExprType.NoType, r.ExpressionType)
                | "theory" -> Assert.AreEqual(ExprType.NoType, theory.ExpressionType)
                | "thm1" -> Assert.AreEqual(ExprType.NoType, thm1.ExpressionType)
                | "proofThm1" -> Assert.AreEqual(ExprType.NoType, proofThm1.ExpressionType)
                | "lem1" -> Assert.AreEqual(ExprType.NoType, lem1.ExpressionType)
                | "proofLem1" -> Assert.AreEqual(ExprType.NoType, proofLem1.ExpressionType)
                | "prp1" -> Assert.AreEqual(ExprType.NoType, prp1.ExpressionType)
                | "proofPrp1" -> Assert.AreEqual(ExprType.NoType, proofPrp1.ExpressionType)
                | "cor1" -> Assert.AreEqual(ExprType.NoType, cor1.ExpressionType)
                | "proofCor1" -> Assert.AreEqual(ExprType.NoType, proofCor1.ExpressionType)
                | "thm2" -> Assert.AreEqual(ExprType.NoType, thm2.ExpressionType)
                | "corThm2" -> Assert.AreEqual(ExprType.NoType, corThm2.ExpressionType)
                | "lem2" -> Assert.AreEqual(ExprType.NoType, lem2.ExpressionType)
                | "corLem2" -> Assert.AreEqual(ExprType.NoType, corLem2.ExpressionType)
                | "prp2" -> Assert.AreEqual(ExprType.NoType, prp2.ExpressionType)
                | "corPrp2" -> Assert.AreEqual(ExprType.NoType, corPrp2.ExpressionType)
                | "cor2" -> Assert.AreEqual(ExprType.NoType, cor2.ExpressionType)
                | "corCor2" -> Assert.AreEqual(ExprType.NoType, corCor2.ExpressionType)
                | "con1" -> Assert.AreEqual(ExprType.NoType, con1.ExpressionType)
                | "corCon1" -> Assert.AreEqual(ExprType.NoType, corCon1.ExpressionType)
                | "axi1" -> Assert.AreEqual(ExprType.NoType, axi1.ExpressionType)
                | "corAxi1"  -> Assert.AreEqual(ExprType.NoType, corAxi1.ExpressionType) 
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
            | "r" -> Assert.AreEqual(ExprType.NoType, r.ExpressionType)
            | "theory" -> Assert.AreEqual(ExprType.NoType, theory.ExpressionType)
            | "block" -> Assert.AreEqual(ExprType.NoType, block.ExpressionType)
            | "t1" -> Assert.AreEqual(ExprType.NoType, t1.ExpressionType)
            | "t2" -> Assert.AreEqual(ExprType.NoType, t2.ExpressionType)
            | "t3" -> Assert.AreEqual(ExprType.NoType, t3.ExpressionType)
            | "t4" -> Assert.AreEqual(ExprType.NoType, t4.ExpressionType)
            | "t5" -> Assert.AreEqual(ExprType.NoType, t5.ExpressionType)
            | "t6" -> Assert.AreEqual(ExprType.NoType, t6.ExpressionType)
            | "t7" -> Assert.AreEqual(ExprType.NoType, t7.ExpressionType)
            | "t8" -> Assert.AreEqual(ExprType.NoType, t8.ExpressionType)
            | "t9" -> Assert.AreEqual(ExprType.NoType, t9.ExpressionType)
            | "t10" -> Assert.AreEqual(ExprType.NoType, t10.ExpressionType)
            | "t11" -> Assert.AreEqual(ExprType.NoType, t11.ExpressionType)
            | "t12" -> Assert.AreEqual(ExprType.NoType, t12.ExpressionType)
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
            | "r" -> Assert.AreEqual(ExprType.NoType, r.ExpressionType)
            | "theory" -> Assert.AreEqual(ExprType.NoType, theory.ExpressionType)
            | "block" -> Assert.AreEqual(ExprType.NoType, block.ExpressionType)
            | "x" -> Assert.AreEqual(ExprType.NoType, x.ExpressionType)
            | "y" -> Assert.AreEqual(ExprType.NoType, y.ExpressionType)
            | "xu" -> Assert.AreEqual(ExprType.NoType, xu.ExpressionType)
            | "xv" -> Assert.AreEqual(ExprType.NoType, xv.ExpressionType)
            | "xw" -> Assert.AreEqual(ExprType.NoType, xw.ExpressionType)
            | "yu" -> Assert.AreEqual(ExprType.NoType, yu.ExpressionType)
            | "yv" -> Assert.AreEqual(ExprType.NoType, yv.ExpressionType)
            | "yw" -> Assert.AreEqual(ExprType.NoType, yw.ExpressionType)
            | "xua" -> Assert.AreEqual(ExprType.NoType, xua.ExpressionType)
            | "xub" -> Assert.AreEqual(ExprType.NoType, xub.ExpressionType)
            | "xuc" -> Assert.AreEqual(ExprType.NoType, xuc.ExpressionType)
            | "xva" -> Assert.AreEqual(ExprType.NoType, xva.ExpressionType)
            | "xvb" -> Assert.AreEqual(ExprType.NoType, xvb.ExpressionType)
            | "xvc" -> Assert.AreEqual(ExprType.NoType, xvc.ExpressionType)
            | "xwa" -> Assert.AreEqual(ExprType.NoType, xwa.ExpressionType)
            | "xwb" -> Assert.AreEqual(ExprType.NoType, xwb.ExpressionType)
            | "xwc" -> Assert.AreEqual(ExprType.NoType, xwc.ExpressionType)
            | "yua" -> Assert.AreEqual(ExprType.NoType, yua.ExpressionType)
            | "yub" -> Assert.AreEqual(ExprType.NoType, yub.ExpressionType)
            | "yuc" -> Assert.AreEqual(ExprType.NoType, yuc.ExpressionType)
            | "yva" -> Assert.AreEqual(ExprType.NoType, yva.ExpressionType)
            | "yvb" -> Assert.AreEqual(ExprType.NoType, yvb.ExpressionType)
            | "yvc" -> Assert.AreEqual(ExprType.NoType, yvc.ExpressionType)
            | "ywa" -> Assert.AreEqual(ExprType.NoType, ywa.ExpressionType)
            | "ywb" -> Assert.AreEqual(ExprType.NoType, ywb.ExpressionType)
            | "ywc" -> Assert.AreEqual(ExprType.NoType, ywc.ExpressionType)
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
            | "r" -> Assert.AreEqual(ExprType.NoType, r.ExpressionType)
            | "theory" -> Assert.AreEqual(ExprType.NoType, theory.ExpressionType)
            | "block" -> Assert.AreEqual(ExprType.NoType, block.ExpressionType)
            | "x" -> Assert.AreEqual(ExprType.NoType, x.ExpressionType)
            | "y" -> Assert.AreEqual(ExprType.NoType, y.ExpressionType)
            | "xu" -> Assert.AreEqual(ExprType.NoType, xu.ExpressionType)
            | "xv" -> Assert.AreEqual(ExprType.NoType, xv.ExpressionType)
            | "xw" -> Assert.AreEqual(ExprType.NoType, xw.ExpressionType)
            | "yu" -> Assert.AreEqual(ExprType.NoType, yu.ExpressionType)
            | "yv" -> Assert.AreEqual(ExprType.NoType, yv.ExpressionType)
            | "yw" -> Assert.AreEqual(ExprType.NoType, yw.ExpressionType)
            | "xua" -> Assert.AreEqual(ExprType.NoType, xua.ExpressionType)
            | "xub" -> Assert.AreEqual(ExprType.NoType, xub.ExpressionType)
            | "xuc" -> Assert.AreEqual(ExprType.NoType, xuc.ExpressionType)
            | "xva" -> Assert.AreEqual(ExprType.NoType, xva.ExpressionType)
            | "xvb" -> Assert.AreEqual(ExprType.NoType, xvb.ExpressionType)
            | "xvc" -> Assert.AreEqual(ExprType.NoType, xvc.ExpressionType)
            | "xwa" -> Assert.AreEqual(ExprType.NoType, xwa.ExpressionType)
            | "xwb" -> Assert.AreEqual(ExprType.NoType, xwb.ExpressionType)
            | "xwc" -> Assert.AreEqual(ExprType.NoType, xwc.ExpressionType)
            | "yua" -> Assert.AreEqual(ExprType.NoType, yua.ExpressionType)
            | "yub" -> Assert.AreEqual(ExprType.NoType, yub.ExpressionType)
            | "yuc" -> Assert.AreEqual(ExprType.NoType, yuc.ExpressionType)
            | "yva" -> Assert.AreEqual(ExprType.NoType, yva.ExpressionType)
            | "yvb" -> Assert.AreEqual(ExprType.NoType, yvb.ExpressionType)
            | "yvc" -> Assert.AreEqual(ExprType.NoType, yvc.ExpressionType)
            | "ywa" -> Assert.AreEqual(ExprType.NoType, ywa.ExpressionType)
            | "ywb" -> Assert.AreEqual(ExprType.NoType, ywb.ExpressionType)
            | "ywc" -> Assert.AreEqual(ExprType.NoType, ywc.ExpressionType)
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
            | "r" -> Assert.AreEqual(ExprType.NoType, r.ExpressionType)
            | "theory" -> Assert.AreEqual(ExprType.NoType, theory.ExpressionType)
            | "block" -> Assert.AreEqual(ExprType.NoType, block.ExpressionType)
            | "x" -> Assert.AreEqual(ExprType.NoType, x.ExpressionType)
            | "y" -> Assert.AreEqual(ExprType.NoType, y.ExpressionType)
            | "xu" -> Assert.AreEqual(ExprType.NoType, xu.ExpressionType)
            | "xv" -> Assert.AreEqual(ExprType.NoType, xv.ExpressionType)
            | "xw" -> Assert.AreEqual(ExprType.NoType, xw.ExpressionType)
            | "yu" -> Assert.AreEqual(ExprType.NoType, yu.ExpressionType)
            | "yv" -> Assert.AreEqual(ExprType.NoType, yv.ExpressionType)
            | "yw" -> Assert.AreEqual(ExprType.NoType, yw.ExpressionType)
            | "xua" -> Assert.AreEqual(ExprType.NoType, xua.ExpressionType)
            | "xub" -> Assert.AreEqual(ExprType.NoType, xub.ExpressionType)
            | "xuc" -> Assert.AreEqual(ExprType.NoType, xuc.ExpressionType)
            | "xva" -> Assert.AreEqual(ExprType.NoType, xva.ExpressionType)
            | "xvb" -> Assert.AreEqual(ExprType.NoType, xvb.ExpressionType)
            | "xvc" -> Assert.AreEqual(ExprType.NoType, xvc.ExpressionType)
            | "xwa" -> Assert.AreEqual(ExprType.NoType, xwa.ExpressionType)
            | "xwb" -> Assert.AreEqual(ExprType.NoType, xwb.ExpressionType)
            | "xwc" -> Assert.AreEqual(ExprType.NoType, xwc.ExpressionType)
            | "yua" -> Assert.AreEqual(ExprType.NoType, yua.ExpressionType)
            | "yub" -> Assert.AreEqual(ExprType.NoType, yub.ExpressionType)
            | "yuc" -> Assert.AreEqual(ExprType.NoType, yuc.ExpressionType)
            | "yva" -> Assert.AreEqual(ExprType.NoType, yva.ExpressionType)
            | "yvb" -> Assert.AreEqual(ExprType.NoType, yvb.ExpressionType)
            | "yvc" -> Assert.AreEqual(ExprType.NoType, yvc.ExpressionType)
            | "ywa" -> Assert.AreEqual(ExprType.NoType, ywa.ExpressionType)
            | "ywb" -> Assert.AreEqual(ExprType.NoType, ywb.ExpressionType)
            | "ywc" -> Assert.AreEqual(ExprType.NoType, ywc.ExpressionType)
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
            | "r" -> Assert.AreEqual(ExprType.NoType, r.ExpressionType)
            | "theory" -> Assert.AreEqual(ExprType.NoType, theory.ExpressionType)
            | "block" -> Assert.AreEqual(ExprType.NoType, block.ExpressionType)
            | "x" -> Assert.AreEqual(ExprType.NoType, x.ExpressionType)
            | "y" -> Assert.AreEqual(ExprType.NoType, y.ExpressionType)
            | "xu" -> Assert.AreEqual(ExprType.NoType, xu.ExpressionType)
            | "xv" -> Assert.AreEqual(ExprType.NoType, xv.ExpressionType)
            | "xw" -> Assert.AreEqual(ExprType.NoType, xw.ExpressionType)
            | "yu" -> Assert.AreEqual(ExprType.NoType, yu.ExpressionType)
            | "yv" -> Assert.AreEqual(ExprType.NoType, yv.ExpressionType)
            | "yw" -> Assert.AreEqual(ExprType.NoType, yw.ExpressionType)
            | "xua" -> Assert.AreEqual(ExprType.NoType, xua.ExpressionType)
            | "xub" -> Assert.AreEqual(ExprType.NoType, xub.ExpressionType)
            | "xuc" -> Assert.AreEqual(ExprType.NoType, xuc.ExpressionType)
            | "xva" -> Assert.AreEqual(ExprType.NoType, xva.ExpressionType)
            | "xvb" -> Assert.AreEqual(ExprType.NoType, xvb.ExpressionType)
            | "xvc" -> Assert.AreEqual(ExprType.NoType, xvc.ExpressionType)
            | "xwa" -> Assert.AreEqual(ExprType.NoType, xwa.ExpressionType)
            | "xwb" -> Assert.AreEqual(ExprType.NoType, xwb.ExpressionType)
            | "xwc" -> Assert.AreEqual(ExprType.NoType, xwc.ExpressionType)
            | "yua" -> Assert.AreEqual(ExprType.NoType, yua.ExpressionType)
            | "yub" -> Assert.AreEqual(ExprType.NoType, yub.ExpressionType)
            | "yuc" -> Assert.AreEqual(ExprType.NoType, yuc.ExpressionType)
            | "yva" -> Assert.AreEqual(ExprType.NoType, yva.ExpressionType)
            | "yvb" -> Assert.AreEqual(ExprType.NoType, yvb.ExpressionType)
            | "yvc" -> Assert.AreEqual(ExprType.NoType, yvc.ExpressionType)
            | "ywa" -> Assert.AreEqual(ExprType.NoType, ywa.ExpressionType)
            | "ywb" -> Assert.AreEqual(ExprType.NoType, ywb.ExpressionType)
            | "ywc" -> Assert.AreEqual(ExprType.NoType, ywc.ExpressionType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
