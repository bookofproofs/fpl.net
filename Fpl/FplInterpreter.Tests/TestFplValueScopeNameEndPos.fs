namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

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
    [<DataRow("loc1")>]
    [<DataRow("loc2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("NameEndPos") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
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
            | "loc1" -> Assert.IsTrue(loc1.NameEndPos.ToString().Contains("Ln: 26, Col: 24)"))
            | "loc2" -> Assert.IsTrue(loc2.NameEndPos.ToString().Contains("Ln: 27, Col: 27)"))
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
        let res = CommonFplValueTestCases.ScopeConstructors("NameEndPos") 
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("NameEndPos") 
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
    [<DataRow("t13")>]
    [<DataRow("t14")>]
    [<TestMethod>]
    member this.TestProperties(var) =
        let res = CommonFplValueTestCases.ScopeProperties("NameEndPos") 
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
            | "t13" -> Assert.IsTrue(t13.NameEndPos.ToString().Contains("Ln: 17, Col: 34)"))
            | "t14" -> Assert.IsTrue(t14.NameEndPos.ToString().Contains("Ln: 18, Col: 38)"))
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("NameEndPos")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsTrue(r.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "theory" -> Assert.IsTrue(theory.NameEndPos.ToString().Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.NameEndPos.ToString().Contains("Ln: 2, Col: 33)")); 
            | "x" -> Assert.IsTrue(x.NameEndPos.ToString().Contains("Ln: 4, Col: 54)"))
            | "y" -> Assert.IsTrue(y.NameEndPos.ToString().Contains("Ln: 4, Col: 54)"))
            | "s" -> Assert.IsTrue(s.NameEndPos.ToString().Contains("Ln: 5, Col: 19)"))
            | "xu" -> Assert.IsTrue(xu.NameEndPos.ToString().Contains("Ln: 4, Col: 53)"))
            | "xv" -> Assert.IsTrue(xv.NameEndPos.ToString().Contains("Ln: 4, Col: 53)"))
            | "xw" -> Assert.IsTrue(xw.NameEndPos.ToString().Contains("Ln: 4, Col: 53)"))
            | "yu" -> Assert.IsTrue(yu.NameEndPos.ToString().Contains("Ln: 4, Col: 53)"))
            | "yv" -> Assert.IsTrue(yv.NameEndPos.ToString().Contains("Ln: 4, Col: 53)"))
            | "yw" -> Assert.IsTrue(yw.NameEndPos.ToString().Contains("Ln: 4, Col: 53)"))
            | "xua" -> Assert.IsTrue(xua.NameEndPos.ToString().Contains("Ln: 4, Col: 39)"))
            | "xub" -> Assert.IsTrue(xub.NameEndPos.ToString().Contains("Ln: 4, Col: 41)"))
            | "xuc" -> Assert.IsTrue(xuc.NameEndPos.ToString().Contains("Ln: 4, Col: 43)"))
            | "xva" -> Assert.IsTrue(xva.NameEndPos.ToString().Contains("Ln: 4, Col: 39)"))
            | "xvb" -> Assert.IsTrue(xvb.NameEndPos.ToString().Contains("Ln: 4, Col: 41)"))
            | "xvc" -> Assert.IsTrue(xvc.NameEndPos.ToString().Contains("Ln: 4, Col: 43)"))
            | "xwa" -> Assert.IsTrue(xwa.NameEndPos.ToString().Contains("Ln: 4, Col: 39)"))
            | "xwb" -> Assert.IsTrue(xwb.NameEndPos.ToString().Contains("Ln: 4, Col: 41)"))
            | "xwc" -> Assert.IsTrue(xwc.NameEndPos.ToString().Contains("Ln: 4, Col: 43)"))
            | "yua" -> Assert.IsTrue(yua.NameEndPos.ToString().Contains("Ln: 4, Col: 39)"))
            | "yub" -> Assert.IsTrue(yub.NameEndPos.ToString().Contains("Ln: 4, Col: 41)"))
            | "yuc" -> Assert.IsTrue(yuc.NameEndPos.ToString().Contains("Ln: 4, Col: 43)"))
            | "yva" -> Assert.IsTrue(yva.NameEndPos.ToString().Contains("Ln: 4, Col: 39)"))
            | "yvb" -> Assert.IsTrue(yvb.NameEndPos.ToString().Contains("Ln: 4, Col: 41)"))
            | "yvc" -> Assert.IsTrue(yvc.NameEndPos.ToString().Contains("Ln: 4, Col: 43)"))
            | "ywa" -> Assert.IsTrue(ywa.NameEndPos.ToString().Contains("Ln: 4, Col: 39)"))
            | "ywb" -> Assert.IsTrue(ywb.NameEndPos.ToString().Contains("Ln: 4, Col: 41)"))
            | "ywc" -> Assert.IsTrue(ywc.NameEndPos.ToString().Contains("Ln: 4, Col: 43)"))
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("NameEndPos")
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("NameEndPos")
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("NameEndPos")
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
    [<DataRow("base12", "parent")>]
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
        let filename = "TestPredicateNameEndPos.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<int64>((int64)22, base1.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)23, base1.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)23, base1.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)19, base1.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)27, base1.NameEndPos.Column)
            | "base6" -> Assert.AreEqual<int64>((int64)19, base1.NameEndPos.Column)
            | "base7" -> Assert.AreEqual<int64>((int64)29, base1.NameEndPos.Column)
            | "base8" -> Assert.AreEqual<int64>((int64)24, base1.NameEndPos.Column)
            | "base9" -> Assert.AreEqual<int64>((int64)26, base1.NameEndPos.Column)
            | "base10" -> Assert.AreEqual<int64>((int64)21, base1.NameEndPos.Column)
            | "base11" -> Assert.AreEqual<int64>((int64)18, base1.NameEndPos.Column)
            | "base12" -> Assert.AreEqual<int64>((int64)24, base1.NameEndPos.Column)
            | "base13" -> Assert.AreEqual<int64>((int64)18, base1.NameEndPos.Column)
            | "base11a" -> Assert.AreEqual<int64>((int64)20, base1.NameEndPos.Column)
            | "base12a" -> Assert.AreEqual<int64>((int64)23, base1.NameEndPos.Column)
            | "base10b" -> Assert.AreEqual<int64>((int64)23, base1.NameEndPos.Column)
            | "base11b" -> Assert.AreEqual<int64>((int64)20, base1.NameEndPos.Column)
            | "base12b" -> Assert.AreEqual<int64>((int64)23, base1.NameEndPos.Column)
            | "base13b" -> Assert.AreEqual<int64>((int64)20, base1.NameEndPos.Column)
            | "base10c" -> Assert.AreEqual<int64>((int64)27, base1.NameEndPos.Column)
            | "base11c" -> Assert.AreEqual<int64>((int64)24, base1.NameEndPos.Column)
            | "base12c" -> Assert.AreEqual<int64>((int64)27, base1.NameEndPos.Column)
            | "base13c" -> Assert.AreEqual<int64>((int64)24, base1.NameEndPos.Column)
            | "base10d" -> Assert.AreEqual<int64>((int64)28, base1.NameEndPos.Column)
            | "base11d" -> Assert.AreEqual<int64>((int64)25, base1.NameEndPos.Column)
            | "base12d" -> Assert.AreEqual<int64>((int64)28, base1.NameEndPos.Column)
            | "base13d" -> Assert.AreEqual<int64>((int64)24, base1.NameEndPos.Column)
            | "base10e" -> Assert.AreEqual<int64>((int64)41, base1.NameEndPos.Column)
            | "base11e" -> Assert.AreEqual<int64>((int64)33, base1.NameEndPos.Column)
            | "base12e" -> Assert.AreEqual<int64>((int64)36, base1.NameEndPos.Column)
            | "base13e" -> Assert.AreEqual<int64>((int64)33, base1.NameEndPos.Column)
            | "base10f" -> Assert.AreEqual<int64>((int64)35, base1.NameEndPos.Column)
            | "base11f" -> Assert.AreEqual<int64>((int64)32, base1.NameEndPos.Column)
            | "base12f" -> Assert.AreEqual<int64>((int64)38, base1.NameEndPos.Column)
            | "base13f" -> Assert.AreEqual<int64>((int64)31, base1.NameEndPos.Column)
            | "base14" -> Assert.AreEqual<int64>((int64)19, base1.NameEndPos.Column)
            | "base15" -> Assert.AreEqual<int64>((int64)19, base1.NameEndPos.Column)
            | "base15a" -> Assert.AreEqual<int64>((int64)20, base1.NameEndPos.Column)
            | "base15b" -> Assert.AreEqual<int64>((int64)21, base1.NameEndPos.Column)
            | "base16" -> Assert.AreEqual<int64>((int64)33, base1.NameEndPos.Column)
            | "base17" -> Assert.AreEqual<int64>((int64)35, base1.NameEndPos.Column)
            | "base18" -> Assert.AreEqual<int64>((int64)59, base1.NameEndPos.Column)
            | "base19" -> Assert.AreEqual<int64>((int64)46, base1.NameEndPos.Column)
            | "base20" -> Assert.AreEqual<int64>((int64)35, base1.NameEndPos.Column)
            | "base21" -> Assert.AreEqual<int64>((int64)30, base1.NameEndPos.Column)
            | "base21a" -> Assert.AreEqual<int64>((int64)23, base1.NameEndPos.Column)
            | "base21b" -> Assert.AreEqual<int64>((int64)25, base1.NameEndPos.Column)
            | "base22" -> Assert.AreEqual<int64>((int64)30, base1.NameEndPos.Column)
            | "base23" -> Assert.AreEqual<int64>((int64)29, base1.NameEndPos.Column)
            | "base24" -> Assert.AreEqual<int64>((int64)27, base1.NameEndPos.Column)
            | "base25" -> Assert.AreEqual<int64>((int64)28, base1.NameEndPos.Column)
            | "base26" -> Assert.AreEqual<int64>((int64)28, base1.NameEndPos.Column)
            | "base27" -> Assert.AreEqual<int64>((int64)20, base1.NameEndPos.Column)
            | "base28" -> Assert.AreEqual<int64>((int64)27, base1.NameEndPos.Column)
            | "base29" -> Assert.AreEqual<int64>((int64)28, base1.NameEndPos.Column)
            | "base30" -> Assert.AreEqual<int64>((int64)25, base1.NameEndPos.Column)
            | "base31" -> Assert.AreEqual<int64>((int64)41, base1.NameEndPos.Column)
            | "base32" -> Assert.AreEqual<int64>((int64)38, base1.NameEndPos.Column)
            | "base33" -> Assert.AreEqual<int64>((int64)43, base1.NameEndPos.Column)
            | "base34" -> Assert.AreEqual<int64>((int64)27, base1.NameEndPos.Column)
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
        let filename = "TestCallConstructorParentClassNameEndPos"
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
            | "base1" -> Assert.AreEqual<int64>((int64)45, base1.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)54, base1.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)54, base1.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)49, base1.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)68, base1.NameEndPos.Column)
            | "base6" -> Assert.AreEqual<int64>((int64)62, base1.NameEndPos.Column)
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
        let filename = "TestDelegateNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<int64>((int64)24, base1.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)31, base1.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)32, base1.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)29, base1.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)27, base1.NameEndPos.Column)
            | "base6" -> Assert.AreEqual<int64>((int64)45, base1.NameEndPos.Column)
            | "base7" -> Assert.AreEqual<int64>((int64)42, base1.NameEndPos.Column)
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
        let filename = "TestFixNotationNameEndPos"
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
            | "base1" -> Assert.AreEqual<int64>((int64)14, base1.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)27, base1.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)26, base1.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)25, base1.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)21, base1.NameEndPos.Column)
            | "base5a" -> Assert.AreEqual<int64>((int64)10, base1.NameEndPos.Column)
            | "base6" -> Assert.AreEqual<int64>((int64)19, base1.NameEndPos.Column)
            | "base7" -> Assert.AreEqual<int64>((int64)32, base1.NameEndPos.Column)
            | "base8" -> Assert.AreEqual<int64>((int64)31, base1.NameEndPos.Column)
            | "base9" -> Assert.AreEqual<int64>((int64)30, base1.NameEndPos.Column)
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
        let filename = "TestMappingNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ValueList[0]
            match var with
            | "base1" -> Assert.AreEqual<int64>((int64)18, mapping.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)18, mapping.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)19, mapping.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)19, mapping.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)36, mapping.NameEndPos.Column)
            | "base6" -> Assert.AreEqual<int64>((int64)25, mapping.NameEndPos.Column)
            | "base7" -> Assert.AreEqual<int64>((int64)27, mapping.NameEndPos.Column)
            | "base8" -> Assert.AreEqual<int64>((int64)48, mapping.NameEndPos.Column)
            | "base9" -> Assert.AreEqual<int64>((int64)36, mapping.NameEndPos.Column)
            | "base10" -> Assert.AreEqual<int64>((int64)52, mapping.NameEndPos.Column)
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
        let filename = "TestArgumentNameEndPos"
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
            | "base1" -> Assert.AreEqual<int64>((int64)17, arg.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)17, arg.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)17, arg.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)17, arg.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)17, arg.NameEndPos.Column)
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
        let filename = "TestLanguageNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<int64>((int64)17, lang.NameEndPos.Column)
            | "base1" -> Assert.AreEqual<int64>((int64)22, lang.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)19, lang.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)22, lang.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)24, lang.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)27, lang.NameEndPos.Column)
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
        let filename = "TestLocalizationNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<int64>((int64)14, pred.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)12, pred.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)14, pred.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)16, pred.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)19, pred.NameEndPos.Column)
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
        let filename = "TestTranslationNameEndPos"
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
            | "base0" -> Assert.AreEqual<int64>((int64)23, trsl.NameEndPos.Column)
            | "base1" -> Assert.AreEqual<int64>((int64)48, trsl.NameEndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)35, trsl.NameEndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)39, trsl.NameEndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)34, trsl.NameEndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)41, trsl.NameEndPos.Column)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
