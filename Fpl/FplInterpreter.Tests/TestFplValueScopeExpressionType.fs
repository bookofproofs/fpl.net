namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

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
    [<DataRow("loc1")>]
    [<DataRow("loc2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("ExpressionType") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FixType>(FixType.NoFix, r.ExpressionType)
            | "theory" -> Assert.AreEqual<FixType>(FixType.NoFix, theory.ExpressionType)
            | "inf1" -> Assert.AreEqual<FixType>(FixType.NoFix, inf1.ExpressionType)
            | "inf2" -> Assert.AreEqual<FixType>(FixType.NoFix, inf2.ExpressionType)
            | "axi1" -> Assert.AreEqual<FixType>(FixType.NoFix, axi1.ExpressionType)
            | "axi2" -> Assert.AreEqual<FixType>(FixType.NoFix, axi2.ExpressionType)
            | "pst1" -> Assert.AreEqual<FixType>(FixType.NoFix, pst1.ExpressionType)
            | "pst2" -> Assert.AreEqual<FixType>(FixType.NoFix, pst2.ExpressionType)
            | "thm1" -> Assert.AreEqual<FixType>(FixType.NoFix, thm1.ExpressionType)
            | "thm2" -> Assert.AreEqual<FixType>(FixType.NoFix, thm2.ExpressionType)
            | "pro1" -> Assert.AreEqual<FixType>(FixType.NoFix, pro1.ExpressionType)
            | "pro2" -> Assert.AreEqual<FixType>(FixType.NoFix, pro2.ExpressionType)
            | "lem1" -> Assert.AreEqual<FixType>(FixType.NoFix, lem1.ExpressionType)
            | "lem2" -> Assert.AreEqual<FixType>(FixType.NoFix, lem2.ExpressionType)
            | "cor1" -> Assert.AreEqual<FixType>(FixType.NoFix, cor1.ExpressionType)
            | "cor2" -> Assert.AreEqual<FixType>(FixType.NoFix, cor1.ExpressionType)
            | "con1" -> Assert.AreEqual<FixType>(FixType.NoFix, con1.ExpressionType)
            | "con2" -> Assert.AreEqual<FixType>(FixType.NoFix, con2.ExpressionType)
            | "cla1" -> Assert.AreEqual<FixType>(FixType.NoFix, cla1.ExpressionType)
            | "cla2" -> Assert.AreEqual<FixType>(FixType.NoFix, cla2.ExpressionType)
            | "pre1" -> Assert.AreEqual<FixType>(FixType.NoFix, pre1.ExpressionType)
            | "pre2" -> Assert.AreEqual<FixType>(FixType.NoFix, pre2.ExpressionType)
            | "fun1" -> Assert.AreEqual<FixType>(FixType.NoFix, fun1.ExpressionType)
            | "fun2" -> Assert.AreEqual<FixType>(FixType.NoFix, fun2.ExpressionType)
            | "prf1" -> Assert.AreEqual<FixType>(FixType.NoFix, prf1.ExpressionType)
            | "prf2" -> Assert.AreEqual<FixType>(FixType.NoFix, prf2.ExpressionType)
            | "loc1" -> Assert.AreEqual<FixType>(FixType.NoFix, loc1.ExpressionType)
            | "loc2" -> Assert.AreEqual<FixType>(FixType.NoFix, loc2.ExpressionType)
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
        let res = CommonFplValueTestCases.ScopeConstructors("ExpressionType") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FixType>(FixType.NoFix, r.ExpressionType)
            | "theory" -> Assert.AreEqual<FixType>(FixType.NoFix, theory.ExpressionType)
            | "block" -> Assert.AreEqual<FixType>(FixType.NoFix, block.ExpressionType)
            | "t1" -> Assert.AreEqual<FixType>(FixType.NoFix, t1.ExpressionType)
            | "t2" -> Assert.AreEqual<FixType>(FixType.NoFix, t2.ExpressionType)
            | "t3" -> Assert.AreEqual<FixType>(FixType.NoFix, t3.ExpressionType)
            | "t4" -> Assert.AreEqual<FixType>(FixType.NoFix, t4.ExpressionType)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("ExpressionType") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<FixType>(FixType.NoFix, r.ExpressionType)
                | "theory" -> Assert.AreEqual<FixType>(FixType.NoFix, theory.ExpressionType)
                | "thm1" -> Assert.AreEqual<FixType>(FixType.NoFix, thm1.ExpressionType)
                | "proofThm1" -> Assert.AreEqual<FixType>(FixType.NoFix, proofThm1.ExpressionType)
                | "lem1" -> Assert.AreEqual<FixType>(FixType.NoFix, lem1.ExpressionType)
                | "proofLem1" -> Assert.AreEqual<FixType>(FixType.NoFix, proofLem1.ExpressionType)
                | "prp1" -> Assert.AreEqual<FixType>(FixType.NoFix, prp1.ExpressionType)
                | "proofPrp1" -> Assert.AreEqual<FixType>(FixType.NoFix, proofPrp1.ExpressionType)
                | "cor1" -> Assert.AreEqual<FixType>(FixType.NoFix, cor1.ExpressionType)
                | "proofCor1" -> Assert.AreEqual<FixType>(FixType.NoFix, proofCor1.ExpressionType)
                | "thm2" -> Assert.AreEqual<FixType>(FixType.NoFix, thm2.ExpressionType)
                | "corThm2" -> Assert.AreEqual<FixType>(FixType.NoFix, corThm2.ExpressionType)
                | "lem2" -> Assert.AreEqual<FixType>(FixType.NoFix, lem2.ExpressionType)
                | "corLem2" -> Assert.AreEqual<FixType>(FixType.NoFix, corLem2.ExpressionType)
                | "prp2" -> Assert.AreEqual<FixType>(FixType.NoFix, prp2.ExpressionType)
                | "corPrp2" -> Assert.AreEqual<FixType>(FixType.NoFix, corPrp2.ExpressionType)
                | "cor2" -> Assert.AreEqual<FixType>(FixType.NoFix, cor2.ExpressionType)
                | "corCor2" -> Assert.AreEqual<FixType>(FixType.NoFix, corCor2.ExpressionType)
                | "con1" -> Assert.AreEqual<FixType>(FixType.NoFix, con1.ExpressionType)
                | "corCon1" -> Assert.AreEqual<FixType>(FixType.NoFix, corCon1.ExpressionType)
                | "axi1" -> Assert.AreEqual<FixType>(FixType.NoFix, axi1.ExpressionType)
                | "corAxi1"  -> Assert.AreEqual<FixType>(FixType.NoFix, corAxi1.ExpressionType) 
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
        let res = CommonFplValueTestCases.ScopeProperties("ExpressionType") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FixType>(FixType.NoFix, r.ExpressionType)
            | "theory" -> Assert.AreEqual<FixType>(FixType.NoFix, theory.ExpressionType)
            | "block" -> Assert.AreEqual<FixType>(FixType.NoFix, block.ExpressionType)
            | "t1" -> Assert.AreEqual<FixType>(FixType.NoFix, t1.ExpressionType)
            | "t2" -> Assert.AreEqual<FixType>(FixType.NoFix, t2.ExpressionType)
            | "t3" -> Assert.AreEqual<FixType>(FixType.NoFix, t3.ExpressionType)
            | "t4" -> Assert.AreEqual<FixType>(FixType.NoFix, t4.ExpressionType)
            | "t5" -> Assert.AreEqual<FixType>(FixType.NoFix, t5.ExpressionType)
            | "t6" -> Assert.AreEqual<FixType>(FixType.NoFix, t6.ExpressionType)
            | "t7" -> Assert.AreEqual<FixType>(FixType.NoFix, t7.ExpressionType)
            | "t8" -> Assert.AreEqual<FixType>(FixType.NoFix, t8.ExpressionType)
            | "t9" -> Assert.AreEqual<FixType>(FixType.NoFix, t9.ExpressionType)
            | "t10" -> Assert.AreEqual<FixType>(FixType.NoFix, t10.ExpressionType)
            | "t11" -> Assert.AreEqual<FixType>(FixType.NoFix, t11.ExpressionType)
            | "t12" -> Assert.AreEqual<FixType>(FixType.NoFix, t12.ExpressionType)
            | "t13" -> Assert.AreEqual<FixType>(FixType.NoFix, t13.ExpressionType)
            | "t14" -> Assert.AreEqual<FixType>(FixType.NoFix, t14.ExpressionType)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("ExpressionType")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FixType>(FixType.NoFix, r.ExpressionType)
            | "theory" -> Assert.AreEqual<FixType>(FixType.NoFix, theory.ExpressionType)
            | "block" -> Assert.AreEqual<FixType>(FixType.NoFix, block.ExpressionType)
            | "x" -> Assert.AreEqual<FixType>(FixType.NoFix, x.ExpressionType)
            | "y" -> Assert.AreEqual<FixType>(FixType.NoFix, y.ExpressionType)
            | "s" -> Assert.AreEqual<FixType>(FixType.NoFix, s.ExpressionType)
            | "xu" -> Assert.AreEqual<FixType>(FixType.NoFix, xu.ExpressionType)
            | "xv" -> Assert.AreEqual<FixType>(FixType.NoFix, xv.ExpressionType)
            | "xw" -> Assert.AreEqual<FixType>(FixType.NoFix, xw.ExpressionType)
            | "yu" -> Assert.AreEqual<FixType>(FixType.NoFix, yu.ExpressionType)
            | "yv" -> Assert.AreEqual<FixType>(FixType.NoFix, yv.ExpressionType)
            | "yw" -> Assert.AreEqual<FixType>(FixType.NoFix, yw.ExpressionType)
            | "xua" -> Assert.AreEqual<FixType>(FixType.NoFix, xua.ExpressionType)
            | "xub" -> Assert.AreEqual<FixType>(FixType.NoFix, xub.ExpressionType)
            | "xuc" -> Assert.AreEqual<FixType>(FixType.NoFix, xuc.ExpressionType)
            | "xva" -> Assert.AreEqual<FixType>(FixType.NoFix, xva.ExpressionType)
            | "xvb" -> Assert.AreEqual<FixType>(FixType.NoFix, xvb.ExpressionType)
            | "xvc" -> Assert.AreEqual<FixType>(FixType.NoFix, xvc.ExpressionType)
            | "xwa" -> Assert.AreEqual<FixType>(FixType.NoFix, xwa.ExpressionType)
            | "xwb" -> Assert.AreEqual<FixType>(FixType.NoFix, xwb.ExpressionType)
            | "xwc" -> Assert.AreEqual<FixType>(FixType.NoFix, xwc.ExpressionType)
            | "yua" -> Assert.AreEqual<FixType>(FixType.NoFix, yua.ExpressionType)
            | "yub" -> Assert.AreEqual<FixType>(FixType.NoFix, yub.ExpressionType)
            | "yuc" -> Assert.AreEqual<FixType>(FixType.NoFix, yuc.ExpressionType)
            | "yva" -> Assert.AreEqual<FixType>(FixType.NoFix, yva.ExpressionType)
            | "yvb" -> Assert.AreEqual<FixType>(FixType.NoFix, yvb.ExpressionType)
            | "yvc" -> Assert.AreEqual<FixType>(FixType.NoFix, yvc.ExpressionType)
            | "ywa" -> Assert.AreEqual<FixType>(FixType.NoFix, ywa.ExpressionType)
            | "ywb" -> Assert.AreEqual<FixType>(FixType.NoFix, ywb.ExpressionType)
            | "ywc" -> Assert.AreEqual<FixType>(FixType.NoFix, ywc.ExpressionType)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("ExpressionType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FixType>(FixType.NoFix, r.ExpressionType)
            | "theory" -> Assert.AreEqual<FixType>(FixType.NoFix, theory.ExpressionType)
            | "block" -> Assert.AreEqual<FixType>(FixType.NoFix, block.ExpressionType)
            | "x" -> Assert.AreEqual<FixType>(FixType.NoFix, x.ExpressionType)
            | "y" -> Assert.AreEqual<FixType>(FixType.NoFix, y.ExpressionType)
            | "xu" -> Assert.AreEqual<FixType>(FixType.NoFix, xu.ExpressionType)
            | "xv" -> Assert.AreEqual<FixType>(FixType.NoFix, xv.ExpressionType)
            | "xw" -> Assert.AreEqual<FixType>(FixType.NoFix, xw.ExpressionType)
            | "yu" -> Assert.AreEqual<FixType>(FixType.NoFix, yu.ExpressionType)
            | "yv" -> Assert.AreEqual<FixType>(FixType.NoFix, yv.ExpressionType)
            | "yw" -> Assert.AreEqual<FixType>(FixType.NoFix, yw.ExpressionType)
            | "xua" -> Assert.AreEqual<FixType>(FixType.NoFix, xua.ExpressionType)
            | "xub" -> Assert.AreEqual<FixType>(FixType.NoFix, xub.ExpressionType)
            | "xuc" -> Assert.AreEqual<FixType>(FixType.NoFix, xuc.ExpressionType)
            | "xva" -> Assert.AreEqual<FixType>(FixType.NoFix, xva.ExpressionType)
            | "xvb" -> Assert.AreEqual<FixType>(FixType.NoFix, xvb.ExpressionType)
            | "xvc" -> Assert.AreEqual<FixType>(FixType.NoFix, xvc.ExpressionType)
            | "xwa" -> Assert.AreEqual<FixType>(FixType.NoFix, xwa.ExpressionType)
            | "xwb" -> Assert.AreEqual<FixType>(FixType.NoFix, xwb.ExpressionType)
            | "xwc" -> Assert.AreEqual<FixType>(FixType.NoFix, xwc.ExpressionType)
            | "yua" -> Assert.AreEqual<FixType>(FixType.NoFix, yua.ExpressionType)
            | "yub" -> Assert.AreEqual<FixType>(FixType.NoFix, yub.ExpressionType)
            | "yuc" -> Assert.AreEqual<FixType>(FixType.NoFix, yuc.ExpressionType)
            | "yva" -> Assert.AreEqual<FixType>(FixType.NoFix, yva.ExpressionType)
            | "yvb" -> Assert.AreEqual<FixType>(FixType.NoFix, yvb.ExpressionType)
            | "yvc" -> Assert.AreEqual<FixType>(FixType.NoFix, yvc.ExpressionType)
            | "ywa" -> Assert.AreEqual<FixType>(FixType.NoFix, ywa.ExpressionType)
            | "ywb" -> Assert.AreEqual<FixType>(FixType.NoFix, ywb.ExpressionType)
            | "ywc" -> Assert.AreEqual<FixType>(FixType.NoFix, ywc.ExpressionType)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("ExpressionType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FixType>(FixType.NoFix, r.ExpressionType)
            | "theory" -> Assert.AreEqual<FixType>(FixType.NoFix, theory.ExpressionType)
            | "block" -> Assert.AreEqual<FixType>(FixType.NoFix, block.ExpressionType)
            | "x" -> Assert.AreEqual<FixType>(FixType.NoFix, x.ExpressionType)
            | "y" -> Assert.AreEqual<FixType>(FixType.NoFix, y.ExpressionType)
            | "xu" -> Assert.AreEqual<FixType>(FixType.NoFix, xu.ExpressionType)
            | "xv" -> Assert.AreEqual<FixType>(FixType.NoFix, xv.ExpressionType)
            | "xw" -> Assert.AreEqual<FixType>(FixType.NoFix, xw.ExpressionType)
            | "yu" -> Assert.AreEqual<FixType>(FixType.NoFix, yu.ExpressionType)
            | "yv" -> Assert.AreEqual<FixType>(FixType.NoFix, yv.ExpressionType)
            | "yw" -> Assert.AreEqual<FixType>(FixType.NoFix, yw.ExpressionType)
            | "xua" -> Assert.AreEqual<FixType>(FixType.NoFix, xua.ExpressionType)
            | "xub" -> Assert.AreEqual<FixType>(FixType.NoFix, xub.ExpressionType)
            | "xuc" -> Assert.AreEqual<FixType>(FixType.NoFix, xuc.ExpressionType)
            | "xva" -> Assert.AreEqual<FixType>(FixType.NoFix, xva.ExpressionType)
            | "xvb" -> Assert.AreEqual<FixType>(FixType.NoFix, xvb.ExpressionType)
            | "xvc" -> Assert.AreEqual<FixType>(FixType.NoFix, xvc.ExpressionType)
            | "xwa" -> Assert.AreEqual<FixType>(FixType.NoFix, xwa.ExpressionType)
            | "xwb" -> Assert.AreEqual<FixType>(FixType.NoFix, xwb.ExpressionType)
            | "xwc" -> Assert.AreEqual<FixType>(FixType.NoFix, xwc.ExpressionType)
            | "yua" -> Assert.AreEqual<FixType>(FixType.NoFix, yua.ExpressionType)
            | "yub" -> Assert.AreEqual<FixType>(FixType.NoFix, yub.ExpressionType)
            | "yuc" -> Assert.AreEqual<FixType>(FixType.NoFix, yuc.ExpressionType)
            | "yva" -> Assert.AreEqual<FixType>(FixType.NoFix, yva.ExpressionType)
            | "yvb" -> Assert.AreEqual<FixType>(FixType.NoFix, yvb.ExpressionType)
            | "yvc" -> Assert.AreEqual<FixType>(FixType.NoFix, yvc.ExpressionType)
            | "ywa" -> Assert.AreEqual<FixType>(FixType.NoFix, ywa.ExpressionType)
            | "ywb" -> Assert.AreEqual<FixType>(FixType.NoFix, ywb.ExpressionType)
            | "ywc" -> Assert.AreEqual<FixType>(FixType.NoFix, ywc.ExpressionType)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("ExpressionType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FixType>(FixType.NoFix, r.ExpressionType)
            | "theory" -> Assert.AreEqual<FixType>(FixType.NoFix, theory.ExpressionType)
            | "block" -> Assert.AreEqual<FixType>(FixType.NoFix, block.ExpressionType)
            | "x" -> Assert.AreEqual<FixType>(FixType.NoFix, x.ExpressionType)
            | "y" -> Assert.AreEqual<FixType>(FixType.NoFix, y.ExpressionType)
            | "xu" -> Assert.AreEqual<FixType>(FixType.NoFix, xu.ExpressionType)
            | "xv" -> Assert.AreEqual<FixType>(FixType.NoFix, xv.ExpressionType)
            | "xw" -> Assert.AreEqual<FixType>(FixType.NoFix, xw.ExpressionType)
            | "yu" -> Assert.AreEqual<FixType>(FixType.NoFix, yu.ExpressionType)
            | "yv" -> Assert.AreEqual<FixType>(FixType.NoFix, yv.ExpressionType)
            | "yw" -> Assert.AreEqual<FixType>(FixType.NoFix, yw.ExpressionType)
            | "xua" -> Assert.AreEqual<FixType>(FixType.NoFix, xua.ExpressionType)
            | "xub" -> Assert.AreEqual<FixType>(FixType.NoFix, xub.ExpressionType)
            | "xuc" -> Assert.AreEqual<FixType>(FixType.NoFix, xuc.ExpressionType)
            | "xva" -> Assert.AreEqual<FixType>(FixType.NoFix, xva.ExpressionType)
            | "xvb" -> Assert.AreEqual<FixType>(FixType.NoFix, xvb.ExpressionType)
            | "xvc" -> Assert.AreEqual<FixType>(FixType.NoFix, xvc.ExpressionType)
            | "xwa" -> Assert.AreEqual<FixType>(FixType.NoFix, xwa.ExpressionType)
            | "xwb" -> Assert.AreEqual<FixType>(FixType.NoFix, xwb.ExpressionType)
            | "xwc" -> Assert.AreEqual<FixType>(FixType.NoFix, xwc.ExpressionType)
            | "yua" -> Assert.AreEqual<FixType>(FixType.NoFix, yua.ExpressionType)
            | "yub" -> Assert.AreEqual<FixType>(FixType.NoFix, yub.ExpressionType)
            | "yuc" -> Assert.AreEqual<FixType>(FixType.NoFix, yuc.ExpressionType)
            | "yva" -> Assert.AreEqual<FixType>(FixType.NoFix, yva.ExpressionType)
            | "yvb" -> Assert.AreEqual<FixType>(FixType.NoFix, yvb.ExpressionType)
            | "yvc" -> Assert.AreEqual<FixType>(FixType.NoFix, yvc.ExpressionType)
            | "ywa" -> Assert.AreEqual<FixType>(FixType.NoFix, ywa.ExpressionType)
            | "ywb" -> Assert.AreEqual<FixType>(FixType.NoFix, ywb.ExpressionType)
            | "ywc" -> Assert.AreEqual<FixType>(FixType.NoFix, ywc.ExpressionType)
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
    [<DataRow("base10e", "Test(x, y).parent[a, b]")>]
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
        let filename = "TestPredicateExpressionType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base6" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base7" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base8" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base9" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base10" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base11" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base12" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base13" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base11a" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base12a" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base10b" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base11b" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base12b" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base13b" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base10c" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base11c" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base12c" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base13c" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base10d" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base11d" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base12d" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base13d" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base10e" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base11e" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base12e" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base13e" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base10f" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base11f" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base12f" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base13f" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base14" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base15" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base15a" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base15b" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base16" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base17" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base18" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base19" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base20" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base21" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base21a" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base21b" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base22" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base23" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base24" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base25" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base26" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base27" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base28" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base29" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base30" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base31" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base32" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base33" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base34" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
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
        let filename = "TestCallConstructorParentClassExpressionType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let cl = theory.Scope["A"]
            let ctor = cl.Scope["A(T1, func, ind, pred)"]
            let base1 = ctor.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base6" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
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
        let filename = "TestDelegateExpressionType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base6" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base7" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
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
        let filename = "TestFixNotationExpressionType"
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
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base5a" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base6" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base7" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base8" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | "base9" -> Assert.AreEqual<FixType>(FixType.NoFix, base1.ExpressionType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """def func T()->obj {intr};""")>]
    [<DataRow("base2", """def func T()->ind {intr};""")>]
    [<DataRow("base3", """def func T()->func {intr};""")>]
    [<DataRow("base4", """def func T()->pred {intr};""")>]
    [<DataRow("base5", """def cl A:obj {intr} def func T()->A {intr};""")>]
    [<DataRow("base6", """def func T()->obj(z:ind) {intr};""")>]
    [<DataRow("base7", """def func T()->pred(z:*obj) {intr};""")>]
    [<DataRow("base8", """def func T()->func(p:*pred(x:obj))->pred(x:ind) {intr};""")>]
    [<DataRow("base9", """def func T()->pred(f:+func(x:A)->A) {intr};""")>]
    [<DataRow("base10", """def cl A:obj {intr} def func T()->A(f:func(x:A)->A) {intr};""")>]
    [<TestMethod>]
    member this.TestMapping(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestMappingExpressionType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ValueList[0]
            match var with
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base6" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base7" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base8" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base9" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | "base10" -> Assert.AreEqual<FixType>(FixType.NoFix, mapping.ExpressionType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """100. |- trivial""", 0)>]
    [<DataRow("base2", """100. ExistsByExample(c), 1. |- false""", 2)>]
    [<DataRow("base3", """100. T1() |- assume not somePremise """, 1)>]
    [<DataRow("base4", """100. 2., 3., 5. |- iif (a,b)""", 3)>]
    [<DataRow("base5", """100. |- revoke 3.""", 0)>]
    [<TestMethod>]
    member this.TestArgument(var, argExpression, expNumber:int) =
        ad.Clear()
        let fplCode = sprintf """proof T$1 { %s };""" argExpression
        let filename = "TestArgumentExpressionType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let proof = theory.Scope["T$1"]
            let arg = proof.Scope["100."]
            let just = arg.ValueList[0]
            let ainf = arg.ValueList[1]
            let numbOfJustifications = just.Scope.Count
 
            Assert.AreEqual<int>(expNumber, numbOfJustifications)

            match var with
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, arg.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, arg.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, arg.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, arg.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, arg.ExpressionType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", "true", """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLanguage(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestLanguageExpressionType.Column"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<FixType>(FixType.NoFix, lang.ExpressionType)
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, lang.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, lang.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, lang.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, lang.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, lang.ExpressionType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLocalization(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestLocalizationExpressionType.Column"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, pred.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, pred.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, pred.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, pred.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, pred.ExpressionType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", "true", """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x " \Leftrightarrow " y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p " \wedge " q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq " y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestTranslation(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestTranslationExpressionType.Column"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]
            let trsl = lang.ValueList[0]

            match var with
            | "base0" -> Assert.AreEqual<FixType>(FixType.NoFix, trsl.ExpressionType)
            | "base1" -> Assert.AreEqual<FixType>(FixType.NoFix, trsl.ExpressionType)
            | "base2" -> Assert.AreEqual<FixType>(FixType.NoFix, trsl.ExpressionType)
            | "base3" -> Assert.AreEqual<FixType>(FixType.NoFix, trsl.ExpressionType)
            | "base4" -> Assert.AreEqual<FixType>(FixType.NoFix, trsl.ExpressionType)
            | "base5" -> Assert.AreEqual<FixType>(FixType.NoFix, trsl.ExpressionType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)