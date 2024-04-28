namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeNameEndPos() =

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
            | "r" -> Assert.AreEqual("(Ln: 1, Col: 1)", r.NameEndPos.ToString())
            | "theory" -> Assert.AreEqual("(Ln: 1, Col: 1)", theory.NameEndPos.ToString())
            | "inf1" -> Assert.AreEqual("(Ln: 2, Col: 33)", inf1.NameEndPos.ToString())
            | "inf2" -> Assert.AreEqual("(Ln: 3, Col: 33)", inf2.NameEndPos.ToString())
            | "axi1" -> Assert.AreEqual("(Ln: 4, Col: 31)", axi1.NameEndPos.ToString())
            | "axi2" -> Assert.AreEqual("(Ln: 5, Col: 31)", axi2.NameEndPos.ToString())
            | "pst1" -> Assert.AreEqual("(Ln: 6, Col: 39)", pst1.NameEndPos.ToString())
            | "pst2" -> Assert.AreEqual("(Ln: 7, Col: 39)", pst2.NameEndPos.ToString())
            | "thm1" -> Assert.AreEqual("(Ln: 8, Col: 35)", thm1.NameEndPos.ToString())
            | "thm2" -> Assert.AreEqual("(Ln: 9, Col: 35)", thm2.NameEndPos.ToString())
            | "pro1" -> Assert.AreEqual("(Ln: 10, Col: 43)", pro1.NameEndPos.ToString())
            | "pro2" -> Assert.AreEqual("(Ln: 11, Col: 43)", pro2.NameEndPos.ToString())
            | "lem1" -> Assert.AreEqual("(Ln: 12, Col: 31)", lem1.NameEndPos.ToString())
            | "lem2" -> Assert.AreEqual("(Ln: 13, Col: 31)", lem2.NameEndPos.ToString())
            | "cor1" -> Assert.AreEqual("(Ln: 14, Col: 37)", cor1.NameEndPos.ToString())
            | "cor2" -> Assert.AreEqual("(Ln: 15, Col: 37)", cor2.NameEndPos.ToString())
            | "con1" -> Assert.AreEqual("(Ln: 16, Col: 41)", con1.NameEndPos.ToString())
            | "con2" -> Assert.AreEqual("(Ln: 17, Col: 41)", con2.NameEndPos.ToString())
            | "cla1" -> Assert.AreEqual("(Ln: 18, Col: 30)", cla1.NameEndPos.ToString())
            | "cla2" -> Assert.AreEqual("(Ln: 19, Col: 30)", cla2.NameEndPos.ToString())
            | "pre1" -> Assert.AreEqual("(Ln: 20, Col: 38)", pre1.NameEndPos.ToString())
            | "pre2" -> Assert.AreEqual("(Ln: 21, Col: 38)", pre2.NameEndPos.ToString())
            | "fun1" -> Assert.AreEqual("(Ln: 22, Col: 48)", fun1.NameEndPos.ToString())
            | "fun2" -> Assert.AreEqual("(Ln: 23, Col: 48)", fun2.NameEndPos.ToString())
            | "prf1" -> Assert.AreEqual("(Ln: 24, Col: 33)", prf1.NameEndPos.ToString())
            | "prf2" -> Assert.AreEqual("(Ln: 25, Col: 33)", prf2.NameEndPos.ToString())
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
            | "r" -> Assert.AreEqual("(Ln: 1, Col: 1)", r.NameEndPos.ToString())
            | "theory" -> Assert.AreEqual("(Ln: 1, Col: 1)", theory.NameEndPos.ToString())
            | "block" -> Assert.AreEqual("(Ln: 2, Col: 22)", block.NameEndPos.ToString())
            | "t1" -> Assert.AreEqual("(Ln: 4, Col: 26)", t1.NameEndPos.ToString())
            | "t2" -> Assert.AreEqual("(Ln: 5, Col: 31)", t2.NameEndPos.ToString())
            | "t3" -> Assert.AreEqual("(Ln: 6, Col: 32)", t3.NameEndPos.ToString())
            | "t4" -> Assert.AreEqual("(Ln: 7, Col: 31)", t4.NameEndPos.ToString())
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
            | "r" -> Assert.AreEqual(Position("", 0, 1, 1), r.NameEndPos)
            | "theory" -> Assert.AreEqual(Position("", 0, 1, 1), theory.NameEndPos)
            | "block" -> Assert.AreEqual(Position("", 27, 2, 26), block.NameEndPos)
            | "t1" -> Assert.AreEqual(Position("", 0, 5, 27), t1.NameEndPos)
            | "t2" -> Assert.AreEqual(Position("", 0, 6, 31), t2.NameEndPos)
            | "t3" -> Assert.AreEqual(Position("", 0, 7, 32), t3.NameEndPos)
            | "t4" -> Assert.AreEqual(Position("", 0, 8, 41), t4.NameEndPos)
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
            | "r" -> Assert.AreEqual("(Ln: 1, Col: 1)", r.NameEndPos.ToString())
            | "theory" -> Assert.AreEqual("(Ln: 1, Col: 1)", theory.NameEndPos.ToString())
            | "block" -> Assert.AreEqual("(Ln: 2, Col: 33)", block.NameEndPos.ToString()); 
            | "x" -> Assert.AreEqual("(Ln: 3, Col: 55)", x.NameEndPos.ToString())
            | "y" -> Assert.AreEqual("(Ln: 3, Col: 55)", y.NameEndPos.ToString())
            | "xu" -> Assert.AreEqual("(Ln: 3, Col: 54)", xu.NameEndPos.ToString())
            | "xv" -> Assert.AreEqual("(Ln: 3, Col: 54)", xv.NameEndPos.ToString())
            | "xw" -> Assert.AreEqual("(Ln: 3, Col: 54)", xw.NameEndPos.ToString())
            | "yu" -> Assert.AreEqual("(Ln: 3, Col: 54)", yu.NameEndPos.ToString())
            | "yv" -> Assert.AreEqual("(Ln: 3, Col: 54)", yv.NameEndPos.ToString())
            | "yw" -> Assert.AreEqual("(Ln: 3, Col: 54)", yw.NameEndPos.ToString())
            | "xua" -> Assert.AreEqual("(Ln: 3, Col: 40)", xua.NameEndPos.ToString())
            | "xub" -> Assert.AreEqual("(Ln: 3, Col: 42)", xub.NameEndPos.ToString())
            | "xuc" -> Assert.AreEqual("(Ln: 3, Col: 44)", xuc.NameEndPos.ToString())
            | "xva" -> Assert.AreEqual("(Ln: 3, Col: 40)", xva.NameEndPos.ToString())
            | "xvb" -> Assert.AreEqual("(Ln: 3, Col: 42)", xvb.NameEndPos.ToString())
            | "xvc" -> Assert.AreEqual("(Ln: 3, Col: 44)", xvc.NameEndPos.ToString())
            | "xwa" -> Assert.AreEqual("(Ln: 3, Col: 40)", xwa.NameEndPos.ToString())
            | "xwb" -> Assert.AreEqual("(Ln: 3, Col: 42)", xwb.NameEndPos.ToString())
            | "xwc" -> Assert.AreEqual("(Ln: 3, Col: 44)", xwc.NameEndPos.ToString())
            | "yua" -> Assert.AreEqual("(Ln: 3, Col: 40)", yua.NameEndPos.ToString())
            | "yub" -> Assert.AreEqual("(Ln: 3, Col: 42)", yub.NameEndPos.ToString())
            | "yuc" -> Assert.AreEqual("(Ln: 3, Col: 44)", yuc.NameEndPos.ToString())
            | "yva" -> Assert.AreEqual("(Ln: 3, Col: 40)", yva.NameEndPos.ToString())
            | "yvb" -> Assert.AreEqual("(Ln: 3, Col: 42)", yvb.NameEndPos.ToString())
            | "yvc" -> Assert.AreEqual("(Ln: 3, Col: 44)", yvc.NameEndPos.ToString())
            | "ywa" -> Assert.AreEqual("(Ln: 3, Col: 40)", ywa.NameEndPos.ToString())
            | "ywb" -> Assert.AreEqual("(Ln: 3, Col: 42)", ywb.NameEndPos.ToString())
            | "ywc" -> Assert.AreEqual("(Ln: 3, Col: 44)", ywc.NameEndPos.ToString())
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlockWithVariadic() =
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
            | "r" -> Assert.AreEqual(Position("", 0, 1, 1), r.NameEndPos)
            | "theory" -> Assert.AreEqual(Position("", 0, 1, 1), theory.NameEndPos)
            | "block" -> Assert.AreEqual(Position("", 70, 2, 69), block.NameEndPos); 
            | "x" -> Assert.AreEqual(Position("", 69, 2, 68), x.NameEndPos)
            | "y" -> Assert.AreEqual(Position("", 69, 2, 68), y.NameEndPos)
            | "xu" -> Assert.AreEqual(Position("", 68, 2, 67), xu.NameEndPos)
            | "xv" -> Assert.AreEqual(Position("", 68, 2, 67), xv.NameEndPos)
            | "xw" -> Assert.AreEqual(Position("", 68, 2, 67), xw.NameEndPos)
            | "yu" -> Assert.AreEqual(Position("", 68, 2, 67), yu.NameEndPos)
            | "yv" -> Assert.AreEqual(Position("", 68, 2, 67), yv.NameEndPos)
            | "yw" -> Assert.AreEqual(Position("", 68, 2, 67), yw.NameEndPos)
            | "xua" -> Assert.AreEqual(Position("", 54, 2, 53), xua.NameEndPos)
            | "xub" -> Assert.AreEqual(Position("", 56, 2, 55), xub.NameEndPos)
            | "xuc" -> Assert.AreEqual(Position("", 58, 2, 57), xuc.NameEndPos)
            | "xva" -> Assert.AreEqual(Position("", 54, 2, 53), xva.NameEndPos)
            | "xvb" -> Assert.AreEqual(Position("", 56, 2, 55), xvb.NameEndPos)
            | "xvc" -> Assert.AreEqual(Position("", 58, 2, 57), xvc.NameEndPos)
            | "xwa" -> Assert.AreEqual(Position("", 54, 2, 53), xwa.NameEndPos)
            | "xwb" -> Assert.AreEqual(Position("", 56, 2, 55), xwb.NameEndPos)
            | "xwc" -> Assert.AreEqual(Position("", 58, 2, 57), xwc.NameEndPos)
            | "yua" -> Assert.AreEqual(Position("", 54, 2, 53), yua.NameEndPos)
            | "yub" -> Assert.AreEqual(Position("", 56, 2, 55), yub.NameEndPos)
            | "yuc" -> Assert.AreEqual(Position("", 58, 2, 57), yuc.NameEndPos)
            | "yva" -> Assert.AreEqual(Position("", 54, 2, 53), yva.NameEndPos)
            | "yvb" -> Assert.AreEqual(Position("", 56, 2, 55), yvb.NameEndPos)
            | "yvc" -> Assert.AreEqual(Position("", 58, 2, 57), yvc.NameEndPos)
            | "ywa" -> Assert.AreEqual(Position("", 54, 2, 53), ywa.NameEndPos)
            | "ywb" -> Assert.AreEqual(Position("", 56, 2, 55), ywb.NameEndPos)
            | "ywc" -> Assert.AreEqual(Position("", 58, 2, 57), ywc.NameEndPos)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)