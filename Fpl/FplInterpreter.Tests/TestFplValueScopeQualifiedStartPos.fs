namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes
open CommonTestHelpers

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
        let res = CommonFplValueTestCases.ScopeBlocks("QualifiedStartPos") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.QualifiedStartPos)
            | "theory" -> Assert.IsTrue(theory.QualifiedStartPos.Contains("Ln: 1, Col: 1)"))
            | "inf1" -> Assert.IsTrue(inf1.QualifiedStartPos.Contains("Ln: 2, Col: 13)"))
            | "inf2" -> Assert.IsTrue(inf2.QualifiedStartPos.Contains("Ln: 3, Col: 13)"))
            | "axi1" -> Assert.IsTrue(axi1.QualifiedStartPos.Contains("Ln: 4, Col: 13)"))
            | "axi2" -> Assert.IsTrue(axi2.QualifiedStartPos.Contains("Ln: 5, Col: 13)"))
            | "pst1" -> Assert.IsTrue(pst1.QualifiedStartPos.Contains("Ln: 6, Col: 13)"))
            | "pst2" -> Assert.IsTrue(pst2.QualifiedStartPos.Contains("Ln: 7, Col: 13)"))
            | "thm1" -> Assert.IsTrue(thm1.QualifiedStartPos.Contains("Ln: 8, Col: 13)"))
            | "thm2" -> Assert.IsTrue(thm2.QualifiedStartPos.Contains("Ln: 9, Col: 13)"))
            | "pro1" -> Assert.IsTrue(pro1.QualifiedStartPos.Contains("Ln: 10, Col: 13)"))
            | "pro2" -> Assert.IsTrue(pro2.QualifiedStartPos.Contains("Ln: 11, Col: 13)"))
            | "lem1" -> Assert.IsTrue(lem1.QualifiedStartPos.Contains("Ln: 12, Col: 13)"))
            | "lem2" -> Assert.IsTrue(lem2.QualifiedStartPos.Contains("Ln: 13, Col: 13)"))
            | "cor1" -> Assert.IsTrue(cor1.QualifiedStartPos.Contains("Ln: 14, Col: 13)"))
            | "cor2" -> Assert.IsTrue(cor2.QualifiedStartPos.Contains("Ln: 15, Col: 13)"))
            | "con1" -> Assert.IsTrue(con1.QualifiedStartPos.Contains("Ln: 16, Col: 13)"))
            | "con2" -> Assert.IsTrue(con2.QualifiedStartPos.Contains("Ln: 17, Col: 13)"))
            | "cla1" -> Assert.IsTrue(cla1.QualifiedStartPos.Contains("Ln: 18, Col: 17)"))
            | "cla2" -> Assert.IsTrue(cla2.QualifiedStartPos.Contains("Ln: 19, Col: 17)"))
            | "pre1" -> Assert.IsTrue(pre1.QualifiedStartPos.Contains("Ln: 20, Col: 17)"))
            | "pre2" -> Assert.IsTrue(pre2.QualifiedStartPos.Contains("Ln: 21, Col: 17)"))
            | "fun1" -> Assert.IsTrue(fun1.QualifiedStartPos.Contains("Ln: 22, Col: 17)"))
            | "fun2" -> Assert.IsTrue(fun2.QualifiedStartPos.Contains("Ln: 23, Col: 17)"))
            | "prf1" -> Assert.IsTrue(prf1.QualifiedStartPos.Contains("Ln: 24, Col: 13)"))
            | "prf2" -> Assert.IsTrue(prf2.QualifiedStartPos.Contains("Ln: 25, Col: 13)"))
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
        let res = CommonFplValueTestCases.ScopeConstructors("QualifiedStartPos") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.QualifiedStartPos)
            | "theory" -> Assert.IsTrue(theory.QualifiedStartPos.Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.QualifiedStartPos.Contains("Ln: 2, Col: 13)"))
            | "t1" -> Assert.IsTrue(t1.QualifiedStartPos.Contains("Ln: 4, Col: 13)"))
            | "t2" -> Assert.IsTrue(t2.QualifiedStartPos.Contains("Ln: 5, Col: 13)"))
            | "t3" -> Assert.IsTrue(t3.QualifiedStartPos.Contains("Ln: 6, Col: 13)"))
            | "t4" -> Assert.IsTrue(t4.QualifiedStartPos.Contains("Ln: 7, Col: 13)"))
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("QualifiedStartPos") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<string>("", r.QualifiedStartPos)
                | "theory" -> Assert.IsTrue(theory.QualifiedStartPos.Contains("Ln: 1, Col: 1)"))
                | "thm1" -> Assert.IsTrue(thm1.QualifiedStartPos.Contains("Ln: 3, Col: 13)"))
                | "proofThm1" -> Assert.IsTrue(proofThm1.QualifiedStartPos.Contains("Ln: 4, Col: 13)"))
                | "lem1" -> Assert.IsTrue(lem1.QualifiedStartPos.Contains("Ln: 6, Col: 13)"))
                | "proofLem1" -> Assert.IsTrue(proofLem1.QualifiedStartPos.Contains("Ln: 7, Col: 13)"))
                | "prp1" -> Assert.IsTrue(prp1.QualifiedStartPos.Contains("Ln: 9, Col: 13)"))
                | "proofPrp1" -> Assert.IsTrue(proofPrp1.QualifiedStartPos.Contains("Ln: 10, Col: 13)"))
                | "cor1" -> Assert.IsTrue(cor1.QualifiedStartPos.Contains("Ln: 12, Col: 13)"))
                | "proofCor1" -> Assert.IsTrue(proofCor1.QualifiedStartPos.Contains("Ln: 13, Col: 13)"))
                | "thm2" -> Assert.IsTrue(thm2.QualifiedStartPos.Contains("Ln: 15, Col: 13)"))
                | "corThm2" -> Assert.IsTrue(corThm2.QualifiedStartPos.Contains("Ln: 16, Col: 13)"))
                | "lem2" -> Assert.IsTrue(lem2.QualifiedStartPos.Contains("Ln: 18, Col: 13)"))
                | "corLem2" -> Assert.IsTrue(corLem2.QualifiedStartPos.Contains("Ln: 19, Col: 13)"))
                | "prp2" -> Assert.IsTrue(prp2.QualifiedStartPos.Contains("Ln: 21, Col: 13)"))
                | "corPrp2" -> Assert.IsTrue(corPrp2.QualifiedStartPos.Contains("Ln: 22, Col: 13)"))
                | "cor2" -> Assert.IsTrue(cor2.QualifiedStartPos.Contains("Ln: 24, Col: 13)"))
                | "corCor2" -> Assert.IsTrue(corCor2.QualifiedStartPos.Contains("Ln: 25, Col: 13)"))
                | "con1" -> Assert.IsTrue(con1.QualifiedStartPos.Contains("Ln: 27, Col: 13)"))
                | "corCon1" -> Assert.IsTrue(corCon1.QualifiedStartPos.Contains("Ln: 28, Col: 13)"))
                | "axi1" -> Assert.IsTrue(axi1.QualifiedStartPos.Contains("Ln: 30, Col: 13)"))
                | "corAxi1"  -> Assert.IsTrue(corAxi1.QualifiedStartPos.Contains("Ln: 31, Col: 13)")) 
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
        let res = CommonFplValueTestCases.ScopeProperties("QualifiedStartPos") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.QualifiedStartPos)
            | "theory" -> Assert.IsTrue(theory.QualifiedStartPos.Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.QualifiedStartPos.Contains("Ln: 2, Col: 13)"))
            | "t1" -> Assert.IsTrue(t1.QualifiedStartPos.Contains("Ln: 5, Col: 13)"))
            | "t2" -> Assert.IsTrue(t2.QualifiedStartPos.Contains("Ln: 6, Col: 13)"))
            | "t3" -> Assert.IsTrue(t3.QualifiedStartPos.Contains("Ln: 7, Col: 13)"))
            | "t4" -> Assert.IsTrue(t4.QualifiedStartPos.Contains("Ln: 8, Col: 13)"))
            | "t5" -> Assert.IsTrue(t5.QualifiedStartPos.Contains("Ln: 9, Col: 13)"))
            | "t6" -> Assert.IsTrue(t6.QualifiedStartPos.Contains("Ln: 10, Col: 13)"))
            | "t7" -> Assert.IsTrue(t7.QualifiedStartPos.Contains("Ln: 11, Col: 13)"))
            | "t8" -> Assert.IsTrue(t8.QualifiedStartPos.Contains("Ln: 12, Col: 13)"))
            | "t9" -> Assert.IsTrue(t9.QualifiedStartPos.Contains("Ln: 13, Col: 13)"))
            | "t10" -> Assert.IsTrue(t10.QualifiedStartPos.Contains("Ln: 14, Col: 13)"))
            | "t11" -> Assert.IsTrue(t11.QualifiedStartPos.Contains("Ln: 15, Col: 13)"))
            | "t12" -> Assert.IsTrue(t12.QualifiedStartPos.Contains("Ln: 16, Col: 13)"))
            | "t13" -> Assert.IsTrue(t13.QualifiedStartPos.Contains("Ln: 17, Col: 13)"))
            | "t14" -> Assert.IsTrue(t14.QualifiedStartPos.Contains("Ln: 18, Col: 13)"))
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("QualifiedStartPos")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.QualifiedStartPos)
            | "theory" -> Assert.IsTrue(theory.QualifiedStartPos.Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.QualifiedStartPos.Contains("Ln: 2, Col: 13)")); 
            | "x" -> Assert.IsTrue(x.QualifiedStartPos.Contains("Ln: 4, Col: 18)"))
            | "y" -> Assert.IsTrue(y.QualifiedStartPos.Contains("Ln: 4, Col: 20)"))
            | "s" -> Assert.IsTrue(s.QualifiedStartPos.Contains("Ln: 5, Col: 18)"))
            | "xu" -> Assert.IsTrue(xu.QualifiedStartPos.Contains("Ln: 4, Col: 27)"))
            | "xv" -> Assert.IsTrue(xv.QualifiedStartPos.Contains("Ln: 4, Col: 29)"))
            | "xw" -> Assert.IsTrue(xw.QualifiedStartPos.Contains("Ln: 4, Col: 31)"))
            | "yu" -> Assert.IsTrue(yu.QualifiedStartPos.Contains("Ln: 4, Col: 27)"))
            | "yv" -> Assert.IsTrue(yv.QualifiedStartPos.Contains("Ln: 4, Col: 29)"))
            | "yw" -> Assert.IsTrue(yw.QualifiedStartPos.Contains("Ln: 4, Col: 31)"))
            | "xua" -> Assert.IsTrue(xua.QualifiedStartPos.Contains("Ln: 4, Col: 38)"))
            | "xub" -> Assert.IsTrue(xub.QualifiedStartPos.Contains("Ln: 4, Col: 40)"))
            | "xuc" -> Assert.IsTrue(xuc.QualifiedStartPos.Contains("Ln: 4, Col: 42)"))
            | "xva" -> Assert.IsTrue(xva.QualifiedStartPos.Contains("Ln: 4, Col: 38)"))
            | "xvb" -> Assert.IsTrue(xvb.QualifiedStartPos.Contains("Ln: 4, Col: 40)"))
            | "xvc" -> Assert.IsTrue(xvc.QualifiedStartPos.Contains("Ln: 4, Col: 42)"))
            | "xwa" -> Assert.IsTrue(xwa.QualifiedStartPos.Contains("Ln: 4, Col: 38)"))
            | "xwb" -> Assert.IsTrue(xwb.QualifiedStartPos.Contains("Ln: 4, Col: 40)"))
            | "xwc" -> Assert.IsTrue(xwc.QualifiedStartPos.Contains("Ln: 4, Col: 42)"))
            | "yua" -> Assert.IsTrue(yua.QualifiedStartPos.Contains("Ln: 4, Col: 38)"))
            | "yub" -> Assert.IsTrue(yub.QualifiedStartPos.Contains("Ln: 4, Col: 40)"))
            | "yuc" -> Assert.IsTrue(yuc.QualifiedStartPos.Contains("Ln: 4, Col: 42)"))
            | "yva" -> Assert.IsTrue(yva.QualifiedStartPos.Contains("Ln: 4, Col: 38)"))
            | "yvb" -> Assert.IsTrue(yvb.QualifiedStartPos.Contains("Ln: 4, Col: 40)"))
            | "yvc" -> Assert.IsTrue(yvc.QualifiedStartPos.Contains("Ln: 4, Col: 42)"))
            | "ywa" -> Assert.IsTrue(ywa.QualifiedStartPos.Contains("Ln: 4, Col: 38)"))
            | "ywb" -> Assert.IsTrue(ywb.QualifiedStartPos.Contains("Ln: 4, Col: 40)"))
            | "ywc" -> Assert.IsTrue(ywc.QualifiedStartPos.Contains("Ln: 4, Col: 42)"))
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("QualifiedStartPos")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.QualifiedStartPos)
            | "theory" -> Assert.IsTrue(theory.QualifiedStartPos.Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.QualifiedStartPos.Contains("Ln: 2, Col: 13)")); 
            | "x" -> Assert.IsTrue(x.QualifiedStartPos.Contains("Ln: 3, Col: 19)"))
            | "y" -> Assert.IsTrue(y.QualifiedStartPos.Contains("Ln: 3, Col: 21)"))
            | "xu" -> Assert.IsTrue(xu.QualifiedStartPos.Contains("Ln: 3, Col: 29)"))
            | "xv" -> Assert.IsTrue(xv.QualifiedStartPos.Contains("Ln: 3, Col: 31)"))
            | "xw" -> Assert.IsTrue(xw.QualifiedStartPos.Contains("Ln: 3, Col: 33)"))
            | "yu" -> Assert.IsTrue(yu.QualifiedStartPos.Contains("Ln: 3, Col: 29)"))
            | "yv" -> Assert.IsTrue(yv.QualifiedStartPos.Contains("Ln: 3, Col: 31)"))
            | "yw" -> Assert.IsTrue(yw.QualifiedStartPos.Contains("Ln: 3, Col: 33)"))
            | "xua" -> Assert.IsTrue(xua.QualifiedStartPos.Contains("Ln: 3, Col: 40)"))
            | "xub" -> Assert.IsTrue(xub.QualifiedStartPos.Contains("Ln: 3, Col: 42)"))
            | "xuc" -> Assert.IsTrue(xuc.QualifiedStartPos.Contains("Ln: 3, Col: 44)"))
            | "xva" -> Assert.IsTrue(xva.QualifiedStartPos.Contains("Ln: 3, Col: 40)"))
            | "xvb" -> Assert.IsTrue(xvb.QualifiedStartPos.Contains("Ln: 3, Col: 42)"))
            | "xvc" -> Assert.IsTrue(xvc.QualifiedStartPos.Contains("Ln: 3, Col: 44)"))
            | "xwa" -> Assert.IsTrue(xwa.QualifiedStartPos.Contains("Ln: 3, Col: 40)"))
            | "xwb" -> Assert.IsTrue(xwb.QualifiedStartPos.Contains("Ln: 3, Col: 42)"))
            | "xwc" -> Assert.IsTrue(xwc.QualifiedStartPos.Contains("Ln: 3, Col: 44)"))
            | "yua" -> Assert.IsTrue(yua.QualifiedStartPos.Contains("Ln: 3, Col: 40)"))
            | "yub" -> Assert.IsTrue(yub.QualifiedStartPos.Contains("Ln: 3, Col: 42)"))
            | "yuc" -> Assert.IsTrue(yuc.QualifiedStartPos.Contains("Ln: 3, Col: 44)"))
            | "yva" -> Assert.IsTrue(yva.QualifiedStartPos.Contains("Ln: 3, Col: 40)"))
            | "yvb" -> Assert.IsTrue(yvb.QualifiedStartPos.Contains("Ln: 3, Col: 42)"))
            | "yvc" -> Assert.IsTrue(yvc.QualifiedStartPos.Contains("Ln: 3, Col: 44)"))
            | "ywa" -> Assert.IsTrue(ywa.QualifiedStartPos.Contains("Ln: 3, Col: 40)"))
            | "ywb" -> Assert.IsTrue(ywb.QualifiedStartPos.Contains("Ln: 3, Col: 42)"))
            | "ywc" -> Assert.IsTrue(ywc.QualifiedStartPos.Contains("Ln: 3, Col: 44)"))
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("QualifiedStartPos")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.QualifiedStartPos)
            | "theory" -> Assert.IsTrue(theory.QualifiedStartPos.Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.QualifiedStartPos.Contains("Ln: 2, Col: 13)"))
            | "x" -> Assert.IsTrue(x.QualifiedStartPos.Contains("Ln: 2, Col: 32)"))
            | "y" -> Assert.IsTrue(y.QualifiedStartPos.Contains("Ln: 2, Col: 34)"))
            | "xu" -> Assert.IsTrue(xu.QualifiedStartPos.Contains("Ln: 2, Col: 41)"))
            | "xv" -> Assert.IsTrue(xv.QualifiedStartPos.Contains("Ln: 2, Col: 43)"))
            | "xw" -> Assert.IsTrue(xw.QualifiedStartPos.Contains("Ln: 2, Col: 45)"))
            | "yu" -> Assert.IsTrue(yu.QualifiedStartPos.Contains("Ln: 2, Col: 41)"))
            | "yv" -> Assert.IsTrue(yv.QualifiedStartPos.Contains("Ln: 2, Col: 43)"))
            | "yw" -> Assert.IsTrue(yw.QualifiedStartPos.Contains("Ln: 2, Col: 45)"))
            | "xua" -> Assert.IsTrue(xua.QualifiedStartPos.Contains("Ln: 2, Col: 52)"))
            | "xub" -> Assert.IsTrue(xub.QualifiedStartPos.Contains("Ln: 2, Col: 54)"))
            | "xuc" -> Assert.IsTrue(xuc.QualifiedStartPos.Contains("Ln: 2, Col: 56)"))
            | "xva" -> Assert.IsTrue(xva.QualifiedStartPos.Contains("Ln: 2, Col: 52)"))
            | "xvb" -> Assert.IsTrue(xvb.QualifiedStartPos.Contains("Ln: 2, Col: 54)"))
            | "xvc" -> Assert.IsTrue(xvc.QualifiedStartPos.Contains("Ln: 2, Col: 56)"))
            | "xwa" -> Assert.IsTrue(xwa.QualifiedStartPos.Contains("Ln: 2, Col: 52)"))
            | "xwb" -> Assert.IsTrue(xwb.QualifiedStartPos.Contains("Ln: 2, Col: 54)"))
            | "xwc" -> Assert.IsTrue(xwc.QualifiedStartPos.Contains("Ln: 2, Col: 56)"))
            | "yua" -> Assert.IsTrue(yua.QualifiedStartPos.Contains("Ln: 2, Col: 52)"))
            | "yub" -> Assert.IsTrue(yub.QualifiedStartPos.Contains("Ln: 2, Col: 54)"))
            | "yuc" -> Assert.IsTrue(yuc.QualifiedStartPos.Contains("Ln: 2, Col: 56)"))
            | "yva" -> Assert.IsTrue(yva.QualifiedStartPos.Contains("Ln: 2, Col: 52)"))
            | "yvb" -> Assert.IsTrue(yvb.QualifiedStartPos.Contains("Ln: 2, Col: 54)"))
            | "yvc" -> Assert.IsTrue(yvc.QualifiedStartPos.Contains("Ln: 2, Col: 56)"))
            | "ywa" -> Assert.IsTrue(ywa.QualifiedStartPos.Contains("Ln: 2, Col: 52)"))
            | "ywb" -> Assert.IsTrue(ywb.QualifiedStartPos.Contains("Ln: 2, Col: 54)"))
            | "ywc" -> Assert.IsTrue(ywc.QualifiedStartPos.Contains("Ln: 2, Col: 56)"))
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("QualifiedStartPos")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.QualifiedStartPos)
            | "theory" -> Assert.IsTrue(theory.QualifiedStartPos.Contains("Ln: 1, Col: 1)"))
            | "block" -> Assert.IsTrue(block.QualifiedStartPos.Contains("Ln: 2, Col: 13)"))
            | "x" -> Assert.IsTrue(x.QualifiedStartPos.Contains("Ln: 2, Col: 32)"))
            | "y" -> Assert.IsTrue(y.QualifiedStartPos.Contains("Ln: 2, Col: 34)"))
            | "xu" -> Assert.IsTrue(xu.QualifiedStartPos.Contains("Ln: 2, Col: 42)"))
            | "xv" -> Assert.IsTrue(xv.QualifiedStartPos.Contains("Ln: 2, Col: 44)"))
            | "xw" -> Assert.IsTrue(xw.QualifiedStartPos.Contains("Ln: 2, Col: 46)"))
            | "yu" -> Assert.IsTrue(yu.QualifiedStartPos.Contains("Ln: 2, Col: 42)"))
            | "yv" -> Assert.IsTrue(yv.QualifiedStartPos.Contains("Ln: 2, Col: 44)"))
            | "yw" -> Assert.IsTrue(yw.QualifiedStartPos.Contains("Ln: 2, Col: 46)"))
            | "xua" -> Assert.IsTrue(xua.QualifiedStartPos.Contains("Ln: 2, Col: 53)"))
            | "xub" -> Assert.IsTrue(xub.QualifiedStartPos.Contains("Ln: 2, Col: 55)"))
            | "xuc" -> Assert.IsTrue(xuc.QualifiedStartPos.Contains("Ln: 2, Col: 57)"))
            | "xva" -> Assert.IsTrue(xva.QualifiedStartPos.Contains("Ln: 2, Col: 53)"))
            | "xvb" -> Assert.IsTrue(xvb.QualifiedStartPos.Contains("Ln: 2, Col: 55)"))
            | "xvc" -> Assert.IsTrue(xvc.QualifiedStartPos.Contains("Ln: 2, Col: 57)"))
            | "xwa" -> Assert.IsTrue(xwa.QualifiedStartPos.Contains("Ln: 2, Col: 53)"))
            | "xwb" -> Assert.IsTrue(xwb.QualifiedStartPos.Contains("Ln: 2, Col: 55)"))
            | "xwc" -> Assert.IsTrue(xwc.QualifiedStartPos.Contains("Ln: 2, Col: 57)"))
            | "yua" -> Assert.IsTrue(yua.QualifiedStartPos.Contains("Ln: 2, Col: 53)"))
            | "yub" -> Assert.IsTrue(yub.QualifiedStartPos.Contains("Ln: 2, Col: 55)"))
            | "yuc" -> Assert.IsTrue(yuc.QualifiedStartPos.Contains("Ln: 2, Col: 57)"))
            | "yva" -> Assert.IsTrue(yva.QualifiedStartPos.Contains("Ln: 2, Col: 53)"))
            | "yvb" -> Assert.IsTrue(yvb.QualifiedStartPos.Contains("Ln: 2, Col: 55)"))
            | "yvc" -> Assert.IsTrue(yvc.QualifiedStartPos.Contains("Ln: 2, Col: 57)"))
            | "ywa" -> Assert.IsTrue(ywa.QualifiedStartPos.Contains("Ln: 2, Col: 53)"))
            | "ywb" -> Assert.IsTrue(ywb.QualifiedStartPos.Contains("Ln: 2, Col: 55)"))
            | "ywc" -> Assert.IsTrue(ywc.QualifiedStartPos.Contains("Ln: 2, Col: 57)"))
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

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
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestPredicateQualifiedStartPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base2" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base3" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base4" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base5" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base6" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base7" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base8" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base9" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base10" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base11" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base12" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base13" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base11a" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base12a" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base10b" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base11b" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base12b" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base13b" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base10c" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base11c" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base12c" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base13c" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base10d" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base11d" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base12d" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base13d" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base10e" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base11e" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base12e" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base13e" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base10f" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base11f" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base12f" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base13f" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base14" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base15" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base15a" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base15b" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base16" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base17" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base18" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base19" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base20" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base21" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base21a" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base21b" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base22" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base23" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base24" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base25" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base26" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base27" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base28" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base29" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base30" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base31" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base32" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
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
        let filename = "TestCallConstructorParentClassQualifiedStartPos"
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
            | "base1" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 11, Col: 37)"))
            | "base2" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 11, Col: 37)"))
            | "base3" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 11, Col: 37)"))
            | "base4" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 11, Col: 37)"))
            | "base5" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 11, Col: 37)"))
            | "base6" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 11, Col: 37)"))
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
        let filename = "TestDelegateQualifiedStartPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base2" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base3" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base4" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base5" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base6" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
            | "base7" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 17)"))
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
        let filename = "TestFixNotationQualifiedStartPos"
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
            | "base1" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base2" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base3" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base4" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base5" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base5a" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base6" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base7" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base8" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | "base9" -> Assert.IsTrue(base1.QualifiedStartPos.ToString().Contains("Ln: 1, Col: 5)"))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
