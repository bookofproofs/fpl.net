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
            | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "inf1" -> Assert.IsTrue(inf1.NameEndPos.ToString().Contains("Ln: 2, Col: 33)"))
            | "inf2" -> Assert.IsTrue(inf2.NameEndPos.ToString().Contains("Ln: 3, Col: 33)"))
            | "axi1" -> Assert.IsTrue(axi1.NameEndPos.ToString().Contains("Ln: 4, Col: 31)"))
            | "axi2" -> Assert.IsTrue(axi2.NameEndPos.ToString().Contains("Ln: 5, Col: 31)"))
            | "pst1" -> Assert.IsTrue(pst1.NameEndPos.ToString().Contains("Ln: 6, Col: 39)"))
            | "pst2" -> Assert.IsTrue(pst2.NameEndPos.ToString().Contains("Ln: 7, Col: 39)"))
            | "thm1" -> Assert.IsTrue(thm1.NameEndPos.ToString().Contains("Ln: 8, Col: 35)"))
            | "thm2" -> Assert.IsTrue(thm2.NameEndPos.ToString().Contains("Ln: 9, Col: 35)"))
            | "pro1" -> Assert.IsTrue(pro1.NameEndPos.ToString().Contains("Ln: 10, Col: 43)"))
            | "pro2" -> Assert.IsTrue(pro2.NameEndPos.ToString().Contains("Ln: 11, Col: 43)"))
            | "lem1" -> Assert.IsTrue(lem1.NameEndPos.ToString().Contains("Ln: 12, Col: 31)"))
            | "lem2" -> Assert.IsTrue(lem2.NameEndPos.ToString().Contains("Ln: 13, Col: 31)"))
            | "cor1" -> Assert.IsTrue(cor1.NameEndPos.ToString().Contains("Ln: 14, Col: 37)"))
            | "cor2" -> Assert.IsTrue(cor2.NameEndPos.ToString().Contains("Ln: 15, Col: 37)"))
            | "con1" -> Assert.IsTrue(con1.NameEndPos.ToString().Contains("Ln: 16, Col: 41)"))
            | "con2" -> Assert.IsTrue(con2.NameEndPos.ToString().Contains("Ln: 17, Col: 41)"))
            | "cla1" -> Assert.IsTrue(cla1.NameEndPos.ToString().Contains("Ln: 18, Col: 30)"))
            | "cla2" -> Assert.IsTrue(cla2.NameEndPos.ToString().Contains("Ln: 19, Col: 30)"))
            | "pre1" -> Assert.IsTrue(pre1.NameEndPos.ToString().Contains("Ln: 20, Col: 38)"))
            | "pre2" -> Assert.IsTrue(pre2.NameEndPos.ToString().Contains("Ln: 21, Col: 38)"))
            | "fun1" -> Assert.IsTrue(fun1.NameEndPos.ToString().Contains("Ln: 22, Col: 48)"))
            | "fun2" -> Assert.IsTrue(fun2.NameEndPos.ToString().Contains("Ln: 23, Col: 48)"))
            | "prf1" -> Assert.IsTrue(prf1.NameEndPos.ToString().Contains("Ln: 24, Col: 33)"))
            | "prf2" -> Assert.IsTrue(prf2.NameEndPos.ToString().Contains("Ln: 25, Col: 33)"))
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
            | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.NameEndPos.ToString().Contains("Ln: 2, Col: 22)"))
            | "t1" -> Assert.IsTrue(t1.NameEndPos.ToString().Contains("Ln: 4, Col: 26)"))
            | "t2" -> Assert.IsTrue(t2.NameEndPos.ToString().Contains("Ln: 5, Col: 31)"))
            | "t3" -> Assert.IsTrue(t3.NameEndPos.ToString().Contains("Ln: 6, Col: 32)"))
            | "t4" -> Assert.IsTrue(t4.NameEndPos.ToString().Contains("Ln: 7, Col: 31)"))
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
                | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
                | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
                | "thm1" -> Assert.IsTrue(thm1.NameEndPos.ToString().Contains("Ln: 3, Col: 35)"))
                | "proofThm1" -> Assert.IsTrue(proofThm1.NameEndPos.ToString().Contains("Ln: 4, Col: 33)"))
                | "lem1" -> Assert.IsTrue(lem1.NameEndPos.ToString().Contains("Ln: 6, Col: 31)"))
                | "proofLem1" -> Assert.IsTrue(proofLem1.NameEndPos.ToString().Contains("Ln: 7, Col: 31)"))
                | "prp1" -> Assert.IsTrue(prp1.NameEndPos.ToString().Contains("Ln: 9, Col: 43)"))
                | "proofPrp1" -> Assert.IsTrue(proofPrp1.NameEndPos.ToString().Contains("Ln: 10, Col: 37)"))
                | "cor1" -> Assert.IsTrue(cor1.NameEndPos.ToString().Contains("Ln: 12, Col: 41)"))
                | "proofCor1" -> Assert.IsTrue(proofCor1.NameEndPos.ToString().Contains("Ln: 13, Col: 37)"))
                | "thm2" -> Assert.IsTrue(thm2.NameEndPos.ToString().Contains("Ln: 15, Col: 35)"))
                | "corThm2" -> Assert.IsTrue(corThm2.NameEndPos.ToString().Contains("Ln: 16, Col: 39)"))
                | "lem2" -> Assert.IsTrue(lem2.NameEndPos.ToString().Contains("Ln: 18, Col: 31)"))
                | "corLem2" -> Assert.IsTrue(corLem2.NameEndPos.ToString().Contains("Ln: 19, Col: 37)"))
                | "prp2" -> Assert.IsTrue(prp2.NameEndPos.ToString().Contains("Ln: 21, Col: 43)"))
                | "corPrp2" -> Assert.IsTrue(corPrp2.NameEndPos.ToString().Contains("Ln: 22, Col: 43)"))
                | "cor2" -> Assert.IsTrue(cor2.NameEndPos.ToString().Contains("Ln: 24, Col: 41)"))
                | "corCor2" -> Assert.IsTrue(corCor2.NameEndPos.ToString().Contains("Ln: 25, Col: 43)"))
                | "con1" -> Assert.IsTrue(con1.NameEndPos.ToString().Contains("Ln: 27, Col: 40)"))
                | "corCon1" -> Assert.IsTrue(corCon1.NameEndPos.ToString().Contains("Ln: 28, Col: 41)"))
                | "axi1" -> Assert.IsTrue(axi1.NameEndPos.ToString().Contains("Ln: 30, Col: 30)"))
                | "corAxi1"  -> Assert.IsTrue(corAxi1.NameEndPos.ToString().Contains("Ln: 31, Col: 36)")) 
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
            | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.NameEndPos.ToString().Contains("Ln: 2, Col: 26)"))
            | "t1" -> Assert.IsTrue(t1.NameEndPos.ToString().Contains("Ln: 5, Col: 27)"))
            | "t2" -> Assert.IsTrue(t2.NameEndPos.ToString().Contains("Ln: 6, Col: 31)"))
            | "t3" -> Assert.IsTrue(t3.NameEndPos.ToString().Contains("Ln: 7, Col: 32)"))
            | "t4" -> Assert.IsTrue(t4.NameEndPos.ToString().Contains("Ln: 8, Col: 36)"))
            | "t5" -> Assert.IsTrue(t5.NameEndPos.ToString().Contains("Ln: 9, Col: 32)"))
            | "t6" -> Assert.IsTrue(t6.NameEndPos.ToString().Contains("Ln: 10, Col: 36)"))
            | "t7" -> Assert.IsTrue(t7.NameEndPos.ToString().Contains("Ln: 11, Col: 33)"))
            | "t8" -> Assert.IsTrue(t8.NameEndPos.ToString().Contains("Ln: 12, Col: 37)"))
            | "t9" -> Assert.IsTrue(t9.NameEndPos.ToString().Contains("Ln: 13, Col: 32)"))
            | "t10" -> Assert.IsTrue(t10.NameEndPos.ToString().Contains("Ln: 14, Col: 37)"))
            | "t11" -> Assert.IsTrue(t11.NameEndPos.ToString().Contains("Ln: 15, Col: 33)"))
            | "t12" -> Assert.IsTrue(t12.NameEndPos.ToString().Contains("Ln: 16, Col: 37)"))
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
            | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.NameEndPos.ToString().Contains("Ln: 2, Col: 33)")); 
            | "x" -> Assert.IsTrue(x.NameEndPos.ToString().Contains("Ln: 3, Col: 55)"))
            | "y" -> Assert.IsTrue(y.NameEndPos.ToString().Contains("Ln: 3, Col: 55)"))
            | "xu" -> Assert.IsTrue(xu.NameEndPos.ToString().Contains("Ln: 3, Col: 54)"))
            | "xv" -> Assert.IsTrue(xv.NameEndPos.ToString().Contains("Ln: 3, Col: 54)"))
            | "xw" -> Assert.IsTrue(xw.NameEndPos.ToString().Contains("Ln: 3, Col: 54)"))
            | "yu" -> Assert.IsTrue(yu.NameEndPos.ToString().Contains("Ln: 3, Col: 54)"))
            | "yv" -> Assert.IsTrue(yv.NameEndPos.ToString().Contains("Ln: 3, Col: 54)"))
            | "yw" -> Assert.IsTrue(yw.NameEndPos.ToString().Contains("Ln: 3, Col: 54)"))
            | "xua" -> Assert.IsTrue(xua.NameEndPos.ToString().Contains("Ln: 3, Col: 40)"))
            | "xub" -> Assert.IsTrue(xub.NameEndPos.ToString().Contains("Ln: 3, Col: 42)"))
            | "xuc" -> Assert.IsTrue(xuc.NameEndPos.ToString().Contains("Ln: 3, Col: 44)"))
            | "xva" -> Assert.IsTrue(xva.NameEndPos.ToString().Contains("Ln: 3, Col: 40)"))
            | "xvb" -> Assert.IsTrue(xvb.NameEndPos.ToString().Contains("Ln: 3, Col: 42)"))
            | "xvc" -> Assert.IsTrue(xvc.NameEndPos.ToString().Contains("Ln: 3, Col: 44)"))
            | "xwa" -> Assert.IsTrue(xwa.NameEndPos.ToString().Contains("Ln: 3, Col: 40)"))
            | "xwb" -> Assert.IsTrue(xwb.NameEndPos.ToString().Contains("Ln: 3, Col: 42)"))
            | "xwc" -> Assert.IsTrue(xwc.NameEndPos.ToString().Contains("Ln: 3, Col: 44)"))
            | "yua" -> Assert.IsTrue(yua.NameEndPos.ToString().Contains("Ln: 3, Col: 40)"))
            | "yub" -> Assert.IsTrue(yub.NameEndPos.ToString().Contains("Ln: 3, Col: 42)"))
            | "yuc" -> Assert.IsTrue(yuc.NameEndPos.ToString().Contains("Ln: 3, Col: 44)"))
            | "yva" -> Assert.IsTrue(yva.NameEndPos.ToString().Contains("Ln: 3, Col: 40)"))
            | "yvb" -> Assert.IsTrue(yvb.NameEndPos.ToString().Contains("Ln: 3, Col: 42)"))
            | "yvc" -> Assert.IsTrue(yvc.NameEndPos.ToString().Contains("Ln: 3, Col: 44)"))
            | "ywa" -> Assert.IsTrue(ywa.NameEndPos.ToString().Contains("Ln: 3, Col: 40)"))
            | "ywb" -> Assert.IsTrue(ywb.NameEndPos.ToString().Contains("Ln: 3, Col: 42)"))
            | "ywc" -> Assert.IsTrue(ywc.NameEndPos.ToString().Contains("Ln: 3, Col: 44)"))
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
            | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.NameEndPos.ToString().Contains("Ln: 2, Col: 33)")); 
            | "x" -> Assert.IsTrue(x.NameEndPos.ToString().Contains("Ln: 3, Col: 57)"))
            | "y" -> Assert.IsTrue(y.NameEndPos.ToString().Contains("Ln: 3, Col: 57)"))
            | "xu" -> Assert.IsTrue(xu.NameEndPos.ToString().Contains("Ln: 3, Col: 56)"))
            | "xv" -> Assert.IsTrue(xv.NameEndPos.ToString().Contains("Ln: 3, Col: 56)"))
            | "xw" -> Assert.IsTrue(xw.NameEndPos.ToString().Contains("Ln: 3, Col: 56)"))
            | "yu" -> Assert.IsTrue(yu.NameEndPos.ToString().Contains("Ln: 3, Col: 56)"))
            | "yv" -> Assert.IsTrue(yv.NameEndPos.ToString().Contains("Ln: 3, Col: 56)"))
            | "yw" -> Assert.IsTrue(yw.NameEndPos.ToString().Contains("Ln: 3, Col: 56)"))
            | "xua" -> Assert.IsTrue(xua.NameEndPos.ToString().Contains("Ln: 3, Col: 41)"))
            | "xub" -> Assert.IsTrue(xub.NameEndPos.ToString().Contains("Ln: 3, Col: 43)"))
            | "xuc" -> Assert.IsTrue(xuc.NameEndPos.ToString().Contains("Ln: 3, Col: 45)"))
            | "xva" -> Assert.IsTrue(xva.NameEndPos.ToString().Contains("Ln: 3, Col: 41)"))
            | "xvb" -> Assert.IsTrue(xvb.NameEndPos.ToString().Contains("Ln: 3, Col: 43)"))
            | "xvc" -> Assert.IsTrue(xvc.NameEndPos.ToString().Contains("Ln: 3, Col: 45)"))
            | "xwa" -> Assert.IsTrue(xwa.NameEndPos.ToString().Contains("Ln: 3, Col: 41)"))
            | "xwb" -> Assert.IsTrue(xwb.NameEndPos.ToString().Contains("Ln: 3, Col: 43)"))
            | "xwc" -> Assert.IsTrue(xwc.NameEndPos.ToString().Contains("Ln: 3, Col: 45)"))
            | "yua" -> Assert.IsTrue(yua.NameEndPos.ToString().Contains("Ln: 3, Col: 41)"))
            | "yub" -> Assert.IsTrue(yub.NameEndPos.ToString().Contains("Ln: 3, Col: 43)"))
            | "yuc" -> Assert.IsTrue(yuc.NameEndPos.ToString().Contains("Ln: 3, Col: 45)"))
            | "yva" -> Assert.IsTrue(yva.NameEndPos.ToString().Contains("Ln: 3, Col: 41)"))
            | "yvb" -> Assert.IsTrue(yvb.NameEndPos.ToString().Contains("Ln: 3, Col: 43)"))
            | "yvc" -> Assert.IsTrue(yvc.NameEndPos.ToString().Contains("Ln: 3, Col: 45)"))
            | "ywa" -> Assert.IsTrue(ywa.NameEndPos.ToString().Contains("Ln: 3, Col: 41)"))
            | "ywb" -> Assert.IsTrue(ywb.NameEndPos.ToString().Contains("Ln: 3, Col: 43)"))
            | "ywc" -> Assert.IsTrue(ywc.NameEndPos.ToString().Contains("Ln: 3, Col: 45)"))
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
            | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.NameEndPos.ToString().Contains("Ln: 2, Col: 69)")); 
            | "x" -> Assert.IsTrue(x.NameEndPos.ToString().Contains("Ln: 2, Col: 68)"))
            | "y" -> Assert.IsTrue(y.NameEndPos.ToString().Contains("Ln: 2, Col: 68)"))
            | "xu" -> Assert.IsTrue(xu.NameEndPos.ToString().Contains("Ln: 2, Col: 67)"))
            | "xv" -> Assert.IsTrue(xv.NameEndPos.ToString().Contains("Ln: 2, Col: 67)"))
            | "xw" -> Assert.IsTrue(xw.NameEndPos.ToString().Contains("Ln: 2, Col: 67)"))
            | "yu" -> Assert.IsTrue(yu.NameEndPos.ToString().Contains("Ln: 2, Col: 67)"))
            | "yv" -> Assert.IsTrue(yv.NameEndPos.ToString().Contains("Ln: 2, Col: 67)"))
            | "yw" -> Assert.IsTrue(yw.NameEndPos.ToString().Contains("Ln: 2, Col: 67)"))
            | "xua" -> Assert.IsTrue(xua.NameEndPos.ToString().Contains("Ln: 2, Col: 53)"))
            | "xub" -> Assert.IsTrue(xub.NameEndPos.ToString().Contains("Ln: 2, Col: 55)"))
            | "xuc" -> Assert.IsTrue(xuc.NameEndPos.ToString().Contains("Ln: 2, Col: 57)"))
            | "xva" -> Assert.IsTrue(xva.NameEndPos.ToString().Contains("Ln: 2, Col: 53)"))
            | "xvb" -> Assert.IsTrue(xvb.NameEndPos.ToString().Contains("Ln: 2, Col: 55)"))
            | "xvc" -> Assert.IsTrue(xvc.NameEndPos.ToString().Contains("Ln: 2, Col: 57)"))
            | "xwa" -> Assert.IsTrue(xwa.NameEndPos.ToString().Contains("Ln: 2, Col: 53)"))
            | "xwb" -> Assert.IsTrue(xwb.NameEndPos.ToString().Contains("Ln: 2, Col: 55)"))
            | "xwc" -> Assert.IsTrue(xwc.NameEndPos.ToString().Contains("Ln: 2, Col: 57)"))
            | "yua" -> Assert.IsTrue(yua.NameEndPos.ToString().Contains("Ln: 2, Col: 53)"))
            | "yub" -> Assert.IsTrue(yub.NameEndPos.ToString().Contains("Ln: 2, Col: 55)"))
            | "yuc" -> Assert.IsTrue(yuc.NameEndPos.ToString().Contains("Ln: 2, Col: 57)"))
            | "yva" -> Assert.IsTrue(yva.NameEndPos.ToString().Contains("Ln: 2, Col: 53)"))
            | "yvb" -> Assert.IsTrue(yvb.NameEndPos.ToString().Contains("Ln: 2, Col: 55)"))
            | "yvc" -> Assert.IsTrue(yvc.NameEndPos.ToString().Contains("Ln: 2, Col: 57)"))
            | "ywa" -> Assert.IsTrue(ywa.NameEndPos.ToString().Contains("Ln: 2, Col: 53)"))
            | "ywb" -> Assert.IsTrue(ywb.NameEndPos.ToString().Contains("Ln: 2, Col: 55)"))
            | "ywc" -> Assert.IsTrue(ywc.NameEndPos.ToString().Contains("Ln: 2, Col: 57)"))
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
            | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.NameEndPos.ToString().Contains("Ln: 2, Col: 71)")); 
            | "x" -> Assert.IsTrue(x.NameEndPos.ToString().Contains("Ln: 2, Col: 70)"))
            | "y" -> Assert.IsTrue(y.NameEndPos.ToString().Contains("Ln: 2, Col: 70)"))
            | "xu" -> Assert.IsTrue(xu.NameEndPos.ToString().Contains("Ln: 2, Col: 69)"))
            | "xv" -> Assert.IsTrue(xv.NameEndPos.ToString().Contains("Ln: 2, Col: 69)"))
            | "xw" -> Assert.IsTrue(xw.NameEndPos.ToString().Contains("Ln: 2, Col: 69)"))
            | "yu" -> Assert.IsTrue(yu.NameEndPos.ToString().Contains("Ln: 2, Col: 69)"))
            | "yv" -> Assert.IsTrue(yv.NameEndPos.ToString().Contains("Ln: 2, Col: 69)"))
            | "yw" -> Assert.IsTrue(yw.NameEndPos.ToString().Contains("Ln: 2, Col: 69)"))
            | "xua" -> Assert.IsTrue(xua.NameEndPos.ToString().Contains("Ln: 2, Col: 54)"))
            | "xub" -> Assert.IsTrue(xub.NameEndPos.ToString().Contains("Ln: 2, Col: 56)"))
            | "xuc" -> Assert.IsTrue(xuc.NameEndPos.ToString().Contains("Ln: 2, Col: 58)"))
            | "xva" -> Assert.IsTrue(xva.NameEndPos.ToString().Contains("Ln: 2, Col: 54)"))
            | "xvb" -> Assert.IsTrue(xvb.NameEndPos.ToString().Contains("Ln: 2, Col: 56)"))
            | "xvc" -> Assert.IsTrue(xvc.NameEndPos.ToString().Contains("Ln: 2, Col: 58)"))
            | "xwa" -> Assert.IsTrue(xwa.NameEndPos.ToString().Contains("Ln: 2, Col: 54)"))
            | "xwb" -> Assert.IsTrue(xwb.NameEndPos.ToString().Contains("Ln: 2, Col: 56)"))
            | "xwc" -> Assert.IsTrue(xwc.NameEndPos.ToString().Contains("Ln: 2, Col: 58)"))
            | "yua" -> Assert.IsTrue(yua.NameEndPos.ToString().Contains("Ln: 2, Col: 54)"))
            | "yub" -> Assert.IsTrue(yub.NameEndPos.ToString().Contains("Ln: 2, Col: 56)"))
            | "yuc" -> Assert.IsTrue(yuc.NameEndPos.ToString().Contains("Ln: 2, Col: 58)"))
            | "yva" -> Assert.IsTrue(yva.NameEndPos.ToString().Contains("Ln: 2, Col: 54)"))
            | "yvb" -> Assert.IsTrue(yvb.NameEndPos.ToString().Contains("Ln: 2, Col: 56)"))
            | "yvc" -> Assert.IsTrue(yvc.NameEndPos.ToString().Contains("Ln: 2, Col: 58)"))
            | "ywa" -> Assert.IsTrue(ywa.NameEndPos.ToString().Contains("Ln: 2, Col: 54)"))
            | "ywb" -> Assert.IsTrue(ywb.NameEndPos.ToString().Contains("Ln: 2, Col: 56)"))
            | "ywc" -> Assert.IsTrue(ywc.NameEndPos.ToString().Contains("Ln: 2, Col: 58)"))
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
            | "base1" -> Assert.IsTrue(base1.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base2" -> Assert.IsTrue(base2.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base3" -> Assert.IsTrue(base3.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base4" -> Assert.IsTrue(base4.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base5" -> Assert.IsTrue(base5.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base6" -> Assert.IsTrue(base6.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base7" -> Assert.IsTrue(base7.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base8" -> Assert.IsTrue(base8.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base9" -> Assert.IsTrue(base9.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base10" -> Assert.IsTrue(base10.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base11" -> Assert.IsTrue(base11.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base12" -> Assert.IsTrue(base12.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base13" -> Assert.IsTrue(base13.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base11a" -> Assert.IsTrue(base11a.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base12a" -> Assert.IsTrue(base12a.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base10b" -> Assert.IsTrue(base10b.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base11b" -> Assert.IsTrue(base11b.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base12b" -> Assert.IsTrue(base12b.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base13b" -> Assert.IsTrue(base13b.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base10c" -> Assert.IsTrue(base10c.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base11c" -> Assert.IsTrue(base11c.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base12c" -> Assert.IsTrue(base12c.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base13c" -> Assert.IsTrue(base13c.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base10d" -> Assert.IsTrue(base10d.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base11d" -> Assert.IsTrue(base11d.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base12d" -> Assert.IsTrue(base12d.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base13d" -> Assert.IsTrue(base13d.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base10e" -> Assert.IsTrue(base10e.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base11e" -> Assert.IsTrue(base11e.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base12e" -> Assert.IsTrue(base12e.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base13e" -> Assert.IsTrue(base13e.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base10f" -> Assert.IsTrue(base10f.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base11f" -> Assert.IsTrue(base11f.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base12f" -> Assert.IsTrue(base12f.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base13f" -> Assert.IsTrue(base13f.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base14" -> Assert.IsTrue(base14.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base15" -> Assert.IsTrue(base15.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base16" -> Assert.IsTrue(base16.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base17" -> Assert.IsTrue(base17.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base18" -> Assert.IsTrue(base18.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base19" -> Assert.IsTrue(base19.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base20" -> Assert.IsTrue(base20.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base21" -> Assert.IsTrue(base21.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base22" -> Assert.IsTrue(base22.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base23" -> Assert.IsTrue(base23.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base24" -> Assert.IsTrue(base24.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base25" -> Assert.IsTrue(base25.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "base26" -> Assert.IsTrue(base26.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)