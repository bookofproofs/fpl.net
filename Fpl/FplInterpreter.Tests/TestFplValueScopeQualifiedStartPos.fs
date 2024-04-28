namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeQualifiedStartPos() =

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
            | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual("Test(Ln: 1, Col: 1)", theory.QualifiedStartPos)
            | "inf1" -> Assert.AreEqual("Test(Ln: 2, Col: 13)", inf1.QualifiedStartPos)
            | "inf2" -> Assert.AreEqual("Test(Ln: 3, Col: 13)", inf2.QualifiedStartPos)
            | "axi1" -> Assert.AreEqual("Test(Ln: 4, Col: 13)", axi1.QualifiedStartPos)
            | "axi2" -> Assert.AreEqual("Test(Ln: 5, Col: 13)", axi2.QualifiedStartPos)
            | "pst1" -> Assert.AreEqual("Test(Ln: 6, Col: 13)", pst1.QualifiedStartPos)
            | "pst2" -> Assert.AreEqual("Test(Ln: 7, Col: 13)", pst2.QualifiedStartPos)
            | "thm1" -> Assert.AreEqual("Test(Ln: 8, Col: 13)", thm1.QualifiedStartPos)
            | "thm2" -> Assert.AreEqual("Test(Ln: 9, Col: 13)", thm2.QualifiedStartPos)
            | "pro1" -> Assert.AreEqual("Test(Ln: 10, Col: 13)", pro1.QualifiedStartPos)
            | "pro2" -> Assert.AreEqual("Test(Ln: 11, Col: 13)", pro2.QualifiedStartPos)
            | "lem1" -> Assert.AreEqual("Test(Ln: 12, Col: 13)", lem1.QualifiedStartPos)
            | "lem2" -> Assert.AreEqual("Test(Ln: 13, Col: 13)", lem2.QualifiedStartPos)
            | "cor1" -> Assert.AreEqual("Test(Ln: 14, Col: 13)", cor1.QualifiedStartPos)
            | "cor2" -> Assert.AreEqual("Test(Ln: 15, Col: 13)", cor2.QualifiedStartPos)
            | "con1" -> Assert.AreEqual("Test(Ln: 16, Col: 13)", con1.QualifiedStartPos)
            | "con2" -> Assert.AreEqual("Test(Ln: 17, Col: 13)", con2.QualifiedStartPos)
            | "cla1" -> Assert.AreEqual("Test(Ln: 18, Col: 17)", cla1.QualifiedStartPos)
            | "cla2" -> Assert.AreEqual("Test(Ln: 19, Col: 17)", cla2.QualifiedStartPos)
            | "pre1" -> Assert.AreEqual("Test(Ln: 20, Col: 17)", pre1.QualifiedStartPos)
            | "pre2" -> Assert.AreEqual("Test(Ln: 21, Col: 17)", pre2.QualifiedStartPos)
            | "fun1" -> Assert.AreEqual("Test(Ln: 22, Col: 17)", fun1.QualifiedStartPos)
            | "fun2" -> Assert.AreEqual("Test(Ln: 23, Col: 17)", fun2.QualifiedStartPos)
            | "prf1" -> Assert.AreEqual("Test(Ln: 24, Col: 13)", prf1.QualifiedStartPos)
            | "prf2" -> Assert.AreEqual("Test(Ln: 25, Col: 13)", prf2.QualifiedStartPos)
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
            | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual("Test(Ln: 1, Col: 1)", theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual("Test(Ln: 2, Col: 13)", block.QualifiedStartPos)
            | "t1" -> Assert.AreEqual("Test(Ln: 4, Col: 13)", t1.QualifiedStartPos)
            | "t2" -> Assert.AreEqual("Test(Ln: 5, Col: 13)", t2.QualifiedStartPos)
            | "t3" -> Assert.AreEqual("Test(Ln: 6, Col: 13)", t3.QualifiedStartPos)
            | "t4" -> Assert.AreEqual("Test(Ln: 7, Col: 13)", t4.QualifiedStartPos)
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
            | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual("Test(Ln: 1, Col: 1)", theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual("Test(Ln: 2, Col: 13)", block.QualifiedStartPos)
            | "t1" -> Assert.AreEqual("Test(Ln: 5, Col: 13)", t1.QualifiedStartPos)
            | "t2" -> Assert.AreEqual("Test(Ln: 6, Col: 13)", t2.QualifiedStartPos)
            | "t3" -> Assert.AreEqual("Test(Ln: 7, Col: 13)", t3.QualifiedStartPos)
            | "t4" -> Assert.AreEqual("Test(Ln: 8, Col: 13)", t4.QualifiedStartPos)
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
            | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual("Test(Ln: 1, Col: 1)", theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual("Test(Ln: 2, Col: 13)", block.QualifiedStartPos); 
            | "x" -> Assert.AreEqual("Test(Ln: 3, Col: 19)", x.QualifiedStartPos)
            | "y" -> Assert.AreEqual("Test(Ln: 3, Col: 21)", y.QualifiedStartPos)
            | "xu" -> Assert.AreEqual("Test(Ln: 3, Col: 28)", xu.QualifiedStartPos)
            | "xv" -> Assert.AreEqual("Test(Ln: 3, Col: 30)", xv.QualifiedStartPos)
            | "xw" -> Assert.AreEqual("Test(Ln: 3, Col: 32)", xw.QualifiedStartPos)
            | "yu" -> Assert.AreEqual("Test(Ln: 3, Col: 28)", yu.QualifiedStartPos)
            | "yv" -> Assert.AreEqual("Test(Ln: 3, Col: 30)", yv.QualifiedStartPos)
            | "yw" -> Assert.AreEqual("Test(Ln: 3, Col: 32)", yw.QualifiedStartPos)
            | "xua" -> Assert.AreEqual("Test(Ln: 3, Col: 39)", xua.QualifiedStartPos)
            | "xub" -> Assert.AreEqual("Test(Ln: 3, Col: 41)", xub.QualifiedStartPos)
            | "xuc" -> Assert.AreEqual("Test(Ln: 3, Col: 43)", xuc.QualifiedStartPos)
            | "xva" -> Assert.AreEqual("Test(Ln: 3, Col: 39)", xva.QualifiedStartPos)
            | "xvb" -> Assert.AreEqual("Test(Ln: 3, Col: 41)", xvb.QualifiedStartPos)
            | "xvc" -> Assert.AreEqual("Test(Ln: 3, Col: 43)", xvc.QualifiedStartPos)
            | "xwa" -> Assert.AreEqual("Test(Ln: 3, Col: 39)", xwa.QualifiedStartPos)
            | "xwb" -> Assert.AreEqual("Test(Ln: 3, Col: 41)", xwb.QualifiedStartPos)
            | "xwc" -> Assert.AreEqual("Test(Ln: 3, Col: 43)", xwc.QualifiedStartPos)
            | "yua" -> Assert.AreEqual("Test(Ln: 3, Col: 39)", yua.QualifiedStartPos)
            | "yub" -> Assert.AreEqual("Test(Ln: 3, Col: 41)", yub.QualifiedStartPos)
            | "yuc" -> Assert.AreEqual("Test(Ln: 3, Col: 43)", yuc.QualifiedStartPos)
            | "yva" -> Assert.AreEqual("Test(Ln: 3, Col: 39)", yva.QualifiedStartPos)
            | "yvb" -> Assert.AreEqual("Test(Ln: 3, Col: 41)", yvb.QualifiedStartPos)
            | "yvc" -> Assert.AreEqual("Test(Ln: 3, Col: 43)", yvc.QualifiedStartPos)
            | "ywa" -> Assert.AreEqual("Test(Ln: 3, Col: 39)", ywa.QualifiedStartPos)
            | "ywb" -> Assert.AreEqual("Test(Ln: 3, Col: 41)", ywb.QualifiedStartPos)
            | "ywc" -> Assert.AreEqual("Test(Ln: 3, Col: 43)", ywc.QualifiedStartPos)
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
            | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual("Test(Ln: 1, Col: 1)", theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual("Test(Ln: 2, Col: 13)", block.QualifiedStartPos); 
            | "x" -> Assert.AreEqual("Test(Ln: 3, Col: 19)", x.QualifiedStartPos)
            | "y" -> Assert.AreEqual("Test(Ln: 3, Col: 21)", y.QualifiedStartPos)
            | "xu" -> Assert.AreEqual("Test(Ln: 3, Col: 29)", xu.QualifiedStartPos)
            | "xv" -> Assert.AreEqual("Test(Ln: 3, Col: 31)", xv.QualifiedStartPos)
            | "xw" -> Assert.AreEqual("Test(Ln: 3, Col: 33)", xw.QualifiedStartPos)
            | "yu" -> Assert.AreEqual("Test(Ln: 3, Col: 29)", yu.QualifiedStartPos)
            | "yv" -> Assert.AreEqual("Test(Ln: 3, Col: 31)", yv.QualifiedStartPos)
            | "yw" -> Assert.AreEqual("Test(Ln: 3, Col: 33)", yw.QualifiedStartPos)
            | "xua" -> Assert.AreEqual("Test(Ln: 3, Col: 40)", xua.QualifiedStartPos)
            | "xub" -> Assert.AreEqual("Test(Ln: 3, Col: 42)", xub.QualifiedStartPos)
            | "xuc" -> Assert.AreEqual("Test(Ln: 3, Col: 44)", xuc.QualifiedStartPos)
            | "xva" -> Assert.AreEqual("Test(Ln: 3, Col: 40)", xva.QualifiedStartPos)
            | "xvb" -> Assert.AreEqual("Test(Ln: 3, Col: 42)", xvb.QualifiedStartPos)
            | "xvc" -> Assert.AreEqual("Test(Ln: 3, Col: 44)", xvc.QualifiedStartPos)
            | "xwa" -> Assert.AreEqual("Test(Ln: 3, Col: 40)", xwa.QualifiedStartPos)
            | "xwb" -> Assert.AreEqual("Test(Ln: 3, Col: 42)", xwb.QualifiedStartPos)
            | "xwc" -> Assert.AreEqual("Test(Ln: 3, Col: 44)", xwc.QualifiedStartPos)
            | "yua" -> Assert.AreEqual("Test(Ln: 3, Col: 40)", yua.QualifiedStartPos)
            | "yub" -> Assert.AreEqual("Test(Ln: 3, Col: 42)", yub.QualifiedStartPos)
            | "yuc" -> Assert.AreEqual("Test(Ln: 3, Col: 44)", yuc.QualifiedStartPos)
            | "yva" -> Assert.AreEqual("Test(Ln: 3, Col: 40)", yva.QualifiedStartPos)
            | "yvb" -> Assert.AreEqual("Test(Ln: 3, Col: 42)", yvb.QualifiedStartPos)
            | "yvc" -> Assert.AreEqual("Test(Ln: 3, Col: 44)", yvc.QualifiedStartPos)
            | "ywa" -> Assert.AreEqual("Test(Ln: 3, Col: 40)", ywa.QualifiedStartPos)
            | "ywb" -> Assert.AreEqual("Test(Ln: 3, Col: 42)", ywb.QualifiedStartPos)
            | "ywc" -> Assert.AreEqual("Test(Ln: 3, Col: 44)", ywc.QualifiedStartPos)
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
            | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual("Test(Ln: 1, Col: 1)", theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual("Test(Ln: 2, Col: 13)", block.QualifiedStartPos)
            | "x" -> Assert.AreEqual("Test(Ln: 2, Col: 32)", x.QualifiedStartPos)
            | "y" -> Assert.AreEqual("Test(Ln: 2, Col: 34)", y.QualifiedStartPos)
            | "xu" -> Assert.AreEqual("Test(Ln: 2, Col: 41)", xu.QualifiedStartPos)
            | "xv" -> Assert.AreEqual("Test(Ln: 2, Col: 43)", xv.QualifiedStartPos)
            | "xw" -> Assert.AreEqual("Test(Ln: 2, Col: 45)", xw.QualifiedStartPos)
            | "yu" -> Assert.AreEqual("Test(Ln: 2, Col: 41)", yu.QualifiedStartPos)
            | "yv" -> Assert.AreEqual("Test(Ln: 2, Col: 43)", yv.QualifiedStartPos)
            | "yw" -> Assert.AreEqual("Test(Ln: 2, Col: 45)", yw.QualifiedStartPos)
            | "xua" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", xua.QualifiedStartPos)
            | "xub" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", xub.QualifiedStartPos)
            | "xuc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", xuc.QualifiedStartPos)
            | "xva" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", xva.QualifiedStartPos)
            | "xvb" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", xvb.QualifiedStartPos)
            | "xvc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", xvc.QualifiedStartPos)
            | "xwa" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", xwa.QualifiedStartPos)
            | "xwb" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", xwb.QualifiedStartPos)
            | "xwc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", xwc.QualifiedStartPos)
            | "yua" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", yua.QualifiedStartPos)
            | "yub" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", yub.QualifiedStartPos)
            | "yuc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", yuc.QualifiedStartPos)
            | "yva" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", yva.QualifiedStartPos)
            | "yvb" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", yvb.QualifiedStartPos)
            | "yvc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", yvc.QualifiedStartPos)
            | "ywa" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", ywa.QualifiedStartPos)
            | "ywb" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", ywb.QualifiedStartPos)
            | "ywc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", ywc.QualifiedStartPos)
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)