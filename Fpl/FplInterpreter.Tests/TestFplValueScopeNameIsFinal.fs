namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes
open CommonTestHelpers

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
        let res = CommonFplValueTestCases.ScopeBlocks("NameIsFinal") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<bool>(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual<bool>(true, theory.NameIsFinal)
            | "inf1" -> Assert.AreEqual<bool>(true, inf1.NameIsFinal)
            | "inf2" -> Assert.AreEqual<bool>(true, inf2.NameIsFinal)
            | "axi1" -> Assert.AreEqual<bool>(true, axi1.NameIsFinal)
            | "axi2" -> Assert.AreEqual<bool>(true, axi2.NameIsFinal)
            | "pst1" -> Assert.AreEqual<bool>(true, pst1.NameIsFinal)
            | "pst2" -> Assert.AreEqual<bool>(true, pst2.NameIsFinal)
            | "thm1" -> Assert.AreEqual<bool>(true, thm1.NameIsFinal)
            | "thm2" -> Assert.AreEqual<bool>(true, thm2.NameIsFinal)
            | "pro1" -> Assert.AreEqual<bool>(true, pro1.NameIsFinal)
            | "pro2" -> Assert.AreEqual<bool>(true, pro2.NameIsFinal)
            | "lem1" -> Assert.AreEqual<bool>(true, lem1.NameIsFinal)
            | "lem2" -> Assert.AreEqual<bool>(true, lem2.NameIsFinal)
            | "cor1" -> Assert.AreEqual<bool>(true, cor1.NameIsFinal)
            | "cor2" -> Assert.AreEqual<bool>(true, cor2.NameIsFinal)
            | "con1" -> Assert.AreEqual<bool>(true, con1.NameIsFinal)
            | "con2" -> Assert.AreEqual<bool>(true, con2.NameIsFinal)
            | "cla1" -> Assert.AreEqual<bool>(true, cla1.NameIsFinal)
            | "cla2" -> Assert.AreEqual<bool>(true, cla2.NameIsFinal)
            | "pre1" -> Assert.AreEqual<bool>(true, pre1.NameIsFinal)
            | "pre2" -> Assert.AreEqual<bool>(true, pre2.NameIsFinal)
            | "fun1" -> Assert.AreEqual<bool>(true, fun1.NameIsFinal)
            | "fun2" -> Assert.AreEqual<bool>(true, fun2.NameIsFinal)
            | "prf1" -> Assert.AreEqual<bool>(true, prf1.NameIsFinal)
            | "prf2" -> Assert.AreEqual<bool>(true, prf2.NameIsFinal)
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
        let res = CommonFplValueTestCases.ScopeConstructors("NameIsFinal") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<bool>(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual<bool>(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual<bool>(true, block.NameIsFinal)
            | "t1" -> Assert.AreEqual<bool>(true, t1.NameIsFinal)
            | "t2" -> Assert.AreEqual<bool>(true, t2.NameIsFinal)
            | "t3" -> Assert.AreEqual<bool>(true, t3.NameIsFinal)
            | "t4" -> Assert.AreEqual<bool>(true, t4.NameIsFinal)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("NameIsFinal") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<bool>(true, r.NameIsFinal)
                | "theory" -> Assert.AreEqual<bool>(true, theory.NameIsFinal)
                | "thm1" -> Assert.AreEqual<bool>(true, thm1.NameIsFinal)
                | "proofThm1" -> Assert.AreEqual<bool>(true, proofThm1.NameIsFinal)
                | "lem1" -> Assert.AreEqual<bool>(true, lem1.NameIsFinal)
                | "proofLem1" -> Assert.AreEqual<bool>(true, proofLem1.NameIsFinal)
                | "prp1" -> Assert.AreEqual<bool>(true, prp1.NameIsFinal)
                | "proofPrp1" -> Assert.AreEqual<bool>(true, proofPrp1.NameIsFinal)
                | "cor1" -> Assert.AreEqual<bool>(true, cor1.NameIsFinal)
                | "proofCor1" -> Assert.AreEqual<bool>(true, proofCor1.NameIsFinal)
                | "thm2" -> Assert.AreEqual<bool>(true, thm2.NameIsFinal)
                | "corThm2" -> Assert.AreEqual<bool>(true, corThm2.NameIsFinal)
                | "lem2" -> Assert.AreEqual<bool>(true, lem2.NameIsFinal)
                | "corLem2" -> Assert.AreEqual<bool>(true, corLem2.NameIsFinal)
                | "prp2" -> Assert.AreEqual<bool>(true, prp2.NameIsFinal)
                | "corPrp2" -> Assert.AreEqual<bool>(true, corPrp2.NameIsFinal)
                | "cor2" -> Assert.AreEqual<bool>(true, cor2.NameIsFinal)
                | "corCor2" -> Assert.AreEqual<bool>(true, corCor2.NameIsFinal)
                | "con1" -> Assert.AreEqual<bool>(true, con1.NameIsFinal)
                | "corCon1" -> Assert.AreEqual<bool>(true, corCon1.NameIsFinal)
                | "axi1" -> Assert.AreEqual<bool>(true, axi1.NameIsFinal)
                | "corAxi1"  -> Assert.AreEqual<bool>(true, corAxi1.NameIsFinal) 
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
        let res = CommonFplValueTestCases.ScopeProperties("NameIsFinal") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<bool>(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual<bool>(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual<bool>(true, block.NameIsFinal)
            | "t1" -> Assert.AreEqual<bool>(true, t1.NameIsFinal)
            | "t2" -> Assert.AreEqual<bool>(true, t2.NameIsFinal)
            | "t3" -> Assert.AreEqual<bool>(true, t3.NameIsFinal)
            | "t4" -> Assert.AreEqual<bool>(true, t4.NameIsFinal)
            | "t5" -> Assert.AreEqual<bool>(true, t5.NameIsFinal)
            | "t6" -> Assert.AreEqual<bool>(true, t6.NameIsFinal)
            | "t7" -> Assert.AreEqual<bool>(true, t7.NameIsFinal)
            | "t8" -> Assert.AreEqual<bool>(true, t8.NameIsFinal)
            | "t9" -> Assert.AreEqual<bool>(true, t9.NameIsFinal)
            | "t10" -> Assert.AreEqual<bool>(true, t10.NameIsFinal)
            | "t11" -> Assert.AreEqual<bool>(true, t11.NameIsFinal)
            | "t12" -> Assert.AreEqual<bool>(true, t12.NameIsFinal)
            | "t13" -> Assert.AreEqual<bool>(true, t13.NameIsFinal)
            | "t14" -> Assert.AreEqual<bool>(true, t14.NameIsFinal)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("NameIsFinal")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<bool>(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual<bool>(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual<bool>(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual<bool>(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual<bool>(true, y.NameIsFinal)
            | "s" -> Assert.AreEqual<bool>(true, s.NameIsFinal)
            | "xu" -> Assert.AreEqual<bool>(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual<bool>(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual<bool>(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual<bool>(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual<bool>(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual<bool>(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual<bool>(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual<bool>(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual<bool>(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual<bool>(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual<bool>(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual<bool>(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual<bool>(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual<bool>(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual<bool>(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual<bool>(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual<bool>(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual<bool>(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual<bool>(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual<bool>(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual<bool>(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual<bool>(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual<bool>(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual<bool>(true, ywc.NameIsFinal)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("NameIsFinal")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<bool>(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual<bool>(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual<bool>(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual<bool>(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual<bool>(true, y.NameIsFinal)
            | "xu" -> Assert.AreEqual<bool>(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual<bool>(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual<bool>(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual<bool>(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual<bool>(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual<bool>(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual<bool>(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual<bool>(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual<bool>(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual<bool>(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual<bool>(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual<bool>(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual<bool>(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual<bool>(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual<bool>(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual<bool>(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual<bool>(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual<bool>(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual<bool>(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual<bool>(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual<bool>(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual<bool>(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual<bool>(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual<bool>(true, ywc.NameIsFinal)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("NameIsFinal")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<bool>(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual<bool>(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual<bool>(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual<bool>(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual<bool>(true, y.NameIsFinal)
            | "xu" -> Assert.AreEqual<bool>(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual<bool>(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual<bool>(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual<bool>(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual<bool>(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual<bool>(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual<bool>(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual<bool>(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual<bool>(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual<bool>(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual<bool>(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual<bool>(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual<bool>(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual<bool>(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual<bool>(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual<bool>(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual<bool>(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual<bool>(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual<bool>(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual<bool>(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual<bool>(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual<bool>(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual<bool>(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual<bool>(true, ywc.NameIsFinal)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("NameIsFinal")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<bool>(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual<bool>(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual<bool>(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual<bool>(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual<bool>(true, y.NameIsFinal)
            | "xu" -> Assert.AreEqual<bool>(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual<bool>(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual<bool>(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual<bool>(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual<bool>(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual<bool>(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual<bool>(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual<bool>(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual<bool>(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual<bool>(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual<bool>(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual<bool>(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual<bool>(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual<bool>(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual<bool>(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual<bool>(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual<bool>(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual<bool>(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual<bool>(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual<bool>(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual<bool>(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual<bool>(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual<bool>(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual<bool>(true, ywc.NameIsFinal)
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
    [<DataRow("base18", "ex x in Range(a, b), y in c, z {and (a, b, c)}")>]
    [<DataRow("base19", "exn$1 x {all y {true}}")>]
    [<DataRow("base20", "all x {not x}")>]
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
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestPredicateNameIsFinal"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base2" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base3" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base4" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base5" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base6" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base7" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base8" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base9" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base10" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base11" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base12" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base13" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base11a" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base12a" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base10b" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base11b" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base12b" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base13b" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base10c" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base11c" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base12c" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base13c" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base10d" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base11d" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base12d" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base13d" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base10e" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base11e" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base12e" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base13e" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base10f" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base11f" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base12f" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base13f" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base14" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base15" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base15a" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base15b" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base16" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base17" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base18" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base19" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base20" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base21" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base21a" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base21b" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base22" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base23" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base24" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base25" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base26" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base27" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base28" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base29" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base30" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base31" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base32" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base33" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base34" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
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
        FplParser.parserDiagnostics.Clear()
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
        let filename = "TestCallConstructorParentClassNameIsFinal"
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
            | "base1" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base2" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base3" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base4" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base5" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base6" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
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
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestDelegateNameIsFinal"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base2" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base3" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base4" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base5" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base6" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base7" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
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
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestFixNotationNameIsFinal"
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
            | "base1" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base2" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base3" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base4" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base5" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base5a" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base6" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base7" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base8" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | "base9" -> Assert.AreEqual<bool>(true, base1.NameIsFinal)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
