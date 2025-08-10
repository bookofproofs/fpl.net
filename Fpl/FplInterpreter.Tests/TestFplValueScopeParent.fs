namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplGrammarCommons
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValueScopeParent() =

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
    [<DataRow("fun3")>]
    [<DataRow("fun4")>]
    [<DataRow("fun5")>]
    [<DataRow("fun6")>]
    [<DataRow("fun7")>]
    [<DataRow("fun8")>]
    [<DataRow("fun9")>]
    [<DataRow("prf1")>]
    [<DataRow("prf2")>]
    [<DataRow("loc1")>]
    [<DataRow("loc2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("Parent") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplValue option>(None, r.Parent)
            | "theory" -> Assert.AreEqual<FplValue>(r, theory.Parent.Value)
            | "inf1" -> Assert.AreEqual<FplValue>(theory, inf1.Parent.Value)
            | "inf2" -> Assert.AreEqual<FplValue>(theory, inf2.Parent.Value)
            | "axi1" -> Assert.AreEqual<FplValue>(theory, axi1.Parent.Value)
            | "axi2" -> Assert.AreEqual<FplValue>(theory, axi2.Parent.Value)
            | "pst1" -> Assert.AreEqual<FplValue>(theory, pst1.Parent.Value)
            | "pst2" -> Assert.AreEqual<FplValue>(theory, pst2.Parent.Value)
            | "thm1" -> Assert.AreEqual<FplValue>(theory, thm1.Parent.Value)
            | "thm2" -> Assert.AreEqual<FplValue>(theory, thm2.Parent.Value)
            | "pro1" -> Assert.AreEqual<FplValue>(theory, pro1.Parent.Value)
            | "pro2" -> Assert.AreEqual<FplValue>(theory, pro2.Parent.Value)
            | "lem1" -> Assert.AreEqual<FplValue>(theory, lem1.Parent.Value)
            | "lem2" -> Assert.AreEqual<FplValue>(theory, lem2.Parent.Value)
            | "cor1" -> Assert.AreEqual<FplValue>(lem1, cor1.Parent.Value)
            | "cor2" -> Assert.AreEqual<FplValue>(lem2, cor2.Parent.Value)
            | "con1" -> Assert.AreEqual<FplValue>(theory, con1.Parent.Value)
            | "con2" -> Assert.AreEqual<FplValue>(theory, con2.Parent.Value)
            | "cla1" -> Assert.AreEqual<FplValue>(theory, cla1.Parent.Value)
            | "cla2" -> Assert.AreEqual<FplValue>(theory, cla2.Parent.Value)
            | "pre1" -> Assert.AreEqual<FplValue>(theory, pre1.Parent.Value)
            | "pre2" -> Assert.AreEqual<FplValue>(theory, pre2.Parent.Value)
            | "fun1" -> Assert.AreEqual<FplValue>(theory, fun1.Parent.Value)
            | "fun2" -> Assert.AreEqual<FplValue>(theory, fun2.Parent.Value)
            | "fun3" -> Assert.AreEqual<FplValue>(theory, fun3.Parent.Value)
            | "fun4" -> Assert.AreEqual<FplValue>(theory, fun4.Parent.Value)
            | "fun5" -> Assert.AreEqual<FplValue>(theory, fun5.Parent.Value)
            | "fun6" -> Assert.AreEqual<FplValue>(theory, fun6.Parent.Value)
            | "fun7" -> Assert.AreEqual<FplValue>(theory, fun7.Parent.Value)
            | "fun8" -> Assert.AreEqual<FplValue>(theory, fun8.Parent.Value)
            | "fun9" -> Assert.AreEqual<FplValue>(theory, fun9.Parent.Value)
            | "prf1" -> Assert.AreEqual<FplValue>(thm1, prf1.Parent.Value)
            | "prf2" -> Assert.AreEqual<FplValue>(thm2, prf2.Parent.Value)
            | "loc1" -> Assert.AreEqual<FplValue>(theory, loc1.Parent.Value)
            | "loc2" -> Assert.AreEqual<FplValue>(theory, loc2.Parent.Value)
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
        let res = CommonFplValueTestCases.ScopeConstructors("Parent") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplValue option>(None, r.Parent)
            | "theory" -> Assert.AreEqual<FplValue>(r, theory.Parent.Value)
            | "block" -> Assert.AreEqual<FplValue>(theory, block.Parent.Value)
            | "t1" -> Assert.AreEqual<FplValue>(block, t1.Parent.Value)
            | "t2" -> Assert.AreEqual<FplValue>(block, t2.Parent.Value)
            | "t3" -> Assert.AreEqual<FplValue>(block, t3.Parent.Value)
            | "t4" -> Assert.AreEqual<FplValue>(block, t4.Parent.Value)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("Parent") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<FplValue option>(None, r.Parent)
                | "theory" -> Assert.AreEqual<FplValue>(r, theory.Parent.Value)
                | "thm1" -> Assert.AreEqual<FplValue>(theory, thm1.Parent.Value)
                | "proofThm1" -> Assert.AreEqual<FplValue>(thm1, proofThm1.Parent.Value)
                | "lem1" -> Assert.AreEqual<FplValue>(theory, lem1.Parent.Value)
                | "proofLem1" -> Assert.AreEqual<FplValue>(lem1, proofLem1.Parent.Value)
                | "prp1" -> Assert.AreEqual<FplValue>(theory, prp1.Parent.Value)
                | "proofPrp1" -> Assert.AreEqual<FplValue>(prp1, proofPrp1.Parent.Value)
                | "cor1" -> Assert.AreEqual<FplValue>(theory, cor1.Parent.Value)
                | "proofCor1" -> Assert.AreEqual<FplValue>(cor1, proofCor1.Parent.Value)
                | "thm2" -> Assert.AreEqual<FplValue>(theory, thm2.Parent.Value)
                | "corThm2" -> Assert.AreEqual<FplValue>(thm2, corThm2.Parent.Value)
                | "lem2" -> Assert.AreEqual<FplValue>(theory, lem2.Parent.Value)
                | "corLem2" -> Assert.AreEqual<FplValue>(lem2, corLem2.Parent.Value)
                | "prp2" -> Assert.AreEqual<FplValue>(theory, prp2.Parent.Value)
                | "corPrp2" -> Assert.AreEqual<FplValue>(prp2, corPrp2.Parent.Value)
                | "cor2" -> Assert.AreEqual<FplValue>(theory, cor2.Parent.Value)
                | "corCor2" -> Assert.AreEqual<FplValue>(cor2, corCor2.Parent.Value)
                | "con1" -> Assert.AreEqual<FplValue>(theory, con1.Parent.Value)
                | "corCon1" -> Assert.AreEqual<FplValue>(con1, corCon1.Parent.Value)
                | "axi1" -> Assert.AreEqual<FplValue>(theory, axi1.Parent.Value)
                | "corAxi1"  -> Assert.AreEqual<FplValue>(axi1, corAxi1.Parent.Value) 
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
        let res = CommonFplValueTestCases.ScopeProperties("Parent") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplValue option>(None, r.Parent)
            | "theory" -> Assert.AreEqual<FplValue>(r, theory.Parent.Value)
            | "block" -> Assert.AreEqual<FplValue>(theory, block.Parent.Value)
            | "t1" -> Assert.AreEqual<FplValue>(block, t1.Parent.Value)
            | "t2" -> Assert.AreEqual<FplValue>(block, t2.Parent.Value)
            | "t3" -> Assert.AreEqual<FplValue>(block, t3.Parent.Value)
            | "t4" -> Assert.AreEqual<FplValue>(block, t4.Parent.Value)
            | "t5" -> Assert.AreEqual<FplValue>(block, t4.Parent.Value)
            | "t6" -> Assert.AreEqual<FplValue>(block, t5.Parent.Value)
            | "t7" -> Assert.AreEqual<FplValue>(block, t6.Parent.Value)
            | "t8" -> Assert.AreEqual<FplValue>(block, t7.Parent.Value)
            | "t9" -> Assert.AreEqual<FplValue>(block, t8.Parent.Value)
            | "t10" -> Assert.AreEqual<FplValue>(block, t9.Parent.Value)
            | "t11" -> Assert.AreEqual<FplValue>(block, t10.Parent.Value)
            | "t12" -> Assert.AreEqual<FplValue>(block, t11.Parent.Value)
            | "t13" -> Assert.AreEqual<FplValue>(block, t13.Parent.Value)
            | "t14" -> Assert.AreEqual<FplValue>(block, t14.Parent.Value)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("s")>]
    [<DataRow("xw")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("yw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<TestMethod>]
    member this.TestVariablesInBlock(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("Parent")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with 
            | "ywc" -> Assert.AreEqual<FplValue>(yw, ywc.Parent.Value)
            | "ywb" -> Assert.AreEqual<FplValue>(yw, ywb.Parent.Value)
            | "ywa" -> Assert.AreEqual<FplValue>(yw, ywa.Parent.Value)
            | "yvc" -> Assert.AreEqual<FplValue>(yv, yvc.Parent.Value)
            | "yvb" -> Assert.AreEqual<FplValue>(yv, yvb.Parent.Value)
            | "yva" -> Assert.AreEqual<FplValue>(yv, yva.Parent.Value)
            | "yuc" -> Assert.AreEqual<FplValue>(yu, yuc.Parent.Value)
            | "yub" -> Assert.AreEqual<FplValue>(yu, yub.Parent.Value)
            | "yua" -> Assert.AreEqual<FplValue>(yu, yua.Parent.Value)
            | "xwc" -> Assert.AreEqual<FplValue>(xw, xwc.Parent.Value)
            | "xwb" -> Assert.AreEqual<FplValue>(xw, xwb.Parent.Value)
            | "xwa" -> Assert.AreEqual<FplValue>(xw, xwa.Parent.Value)
            | "xvc" -> Assert.AreEqual<FplValue>(xv, xvc.Parent.Value)
            | "xvb" -> Assert.AreEqual<FplValue>(xv, xvb.Parent.Value)
            | "xva" -> Assert.AreEqual<FplValue>(xv, xva.Parent.Value)
            | "xuc" -> Assert.AreEqual<FplValue>(xu, xuc.Parent.Value)
            | "xub" -> Assert.AreEqual<FplValue>(xu, xub.Parent.Value)
            | "xua" -> Assert.AreEqual<FplValue>(xu, xua.Parent.Value)

            | "yw" -> Assert.AreEqual<FplValue>(y, yw.Parent.Value)
            | "yv" -> Assert.AreEqual<FplValue>(y, yv.Parent.Value)
            | "yu" -> Assert.AreEqual<FplValue>(y, yu.Parent.Value)
            | "xw" -> Assert.AreEqual<FplValue>(x, xw.Parent.Value)
            | "xv" -> Assert.AreEqual<FplValue>(x, xv.Parent.Value)
            | "xu" -> Assert.AreEqual<FplValue>(x,  xu.Parent.Value)
            | "y" -> Assert.AreEqual<FplValue>(block, y.Parent.Value)
            | "x" -> Assert.AreEqual<FplValue>(block, x.Parent.Value)
            | "s" -> Assert.AreEqual<FplValue>(block, s.Parent.Value)
            | "block" -> Assert.AreEqual<FplValue>(theory, block.Parent.Value)
            | "theory" -> Assert.AreEqual<FplValue>(r, theory.Parent.Value)
            | "r" -> Assert.AreEqual<FplValue option>(None, r.Parent)
            | _ -> 
                Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("xw")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("yw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<TestMethod>]
    member this.TestVariablesInBlockVariadic(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("Parent")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with 
            | "ywc" -> Assert.AreEqual<FplValue>(yw, ywc.Parent.Value)
            | "ywb" -> Assert.AreEqual<FplValue>(yw, ywb.Parent.Value)
            | "ywa" -> Assert.AreEqual<FplValue>(yw, ywa.Parent.Value)
            | "yvc" -> Assert.AreEqual<FplValue>(yv, yvc.Parent.Value)
            | "yvb" -> Assert.AreEqual<FplValue>(yv, yvb.Parent.Value)
            | "yva" -> Assert.AreEqual<FplValue>(yv, yva.Parent.Value)
            | "yuc" -> Assert.AreEqual<FplValue>(yu, yuc.Parent.Value)
            | "yub" -> Assert.AreEqual<FplValue>(yu, yub.Parent.Value)
            | "yua" -> Assert.AreEqual<FplValue>(yu, yua.Parent.Value)
            | "xwc" -> Assert.AreEqual<FplValue>(xw, xwc.Parent.Value)
            | "xwb" -> Assert.AreEqual<FplValue>(xw, xwb.Parent.Value)
            | "xwa" -> Assert.AreEqual<FplValue>(xw, xwa.Parent.Value)
            | "xvc" -> Assert.AreEqual<FplValue>(xv, xvc.Parent.Value)
            | "xvb" -> Assert.AreEqual<FplValue>(xv, xvb.Parent.Value)
            | "xva" -> Assert.AreEqual<FplValue>(xv, xva.Parent.Value)
            | "xuc" -> Assert.AreEqual<FplValue>(xu, xuc.Parent.Value)
            | "xub" -> Assert.AreEqual<FplValue>(xu, xub.Parent.Value)
            | "xua" -> Assert.AreEqual<FplValue>(xu, xua.Parent.Value)

            | "yw" -> Assert.AreEqual<FplValue>(y, yw.Parent.Value)
            | "yv" -> Assert.AreEqual<FplValue>(y, yv.Parent.Value)
            | "yu" -> Assert.AreEqual<FplValue>(y, yu.Parent.Value)
            | "xw" -> Assert.AreEqual<FplValue>(x, xw.Parent.Value)
            | "xv" -> Assert.AreEqual<FplValue>(x, xv.Parent.Value)
            | "xu" -> Assert.AreEqual<FplValue>(x,  xu.Parent.Value)
            | "y" -> Assert.AreEqual<FplValue>(block, y.Parent.Value)
            | "x" -> Assert.AreEqual<FplValue>(block, x.Parent.Value)
            | "block" -> Assert.AreEqual<FplValue>(theory, block.Parent.Value)
            | "theory" -> Assert.AreEqual<FplValue>(r, theory.Parent.Value)
            | "r" -> Assert.AreEqual<FplValue option>(None, r.Parent)
            | _ -> 
                Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("xw")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("yw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<TestMethod>]
    member this.TestVariablesInSignature(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("Parent")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with 
            | "ywc" -> Assert.AreEqual<FplValue>(yw, ywc.Parent.Value)
            | "ywb" -> Assert.AreEqual<FplValue>(yw, ywb.Parent.Value)
            | "ywa" -> Assert.AreEqual<FplValue>(yw, ywa.Parent.Value)
            | "yvc" -> Assert.AreEqual<FplValue>(yv, yvc.Parent.Value)
            | "yvb" -> Assert.AreEqual<FplValue>(yv, yvb.Parent.Value)
            | "yva" -> Assert.AreEqual<FplValue>(yv, yva.Parent.Value)
            | "yuc" -> Assert.AreEqual<FplValue>(yu, yuc.Parent.Value)
            | "yub" -> Assert.AreEqual<FplValue>(yu, yub.Parent.Value)
            | "yua" -> Assert.AreEqual<FplValue>(yu, yua.Parent.Value)
            | "xwc" -> Assert.AreEqual<FplValue>(xw, xwc.Parent.Value)
            | "xwb" -> Assert.AreEqual<FplValue>(xw, xwb.Parent.Value)
            | "xwa" -> Assert.AreEqual<FplValue>(xw, xwa.Parent.Value)
            | "xvc" -> Assert.AreEqual<FplValue>(xv, xvc.Parent.Value)
            | "xvb" -> Assert.AreEqual<FplValue>(xv, xvb.Parent.Value)
            | "xva" -> Assert.AreEqual<FplValue>(xv, xva.Parent.Value)
            | "xuc" -> Assert.AreEqual<FplValue>(xu, xuc.Parent.Value)
            | "xub" -> Assert.AreEqual<FplValue>(xu, xub.Parent.Value)
            | "xua" -> Assert.AreEqual<FplValue>(xu, xua.Parent.Value)

            | "yw" -> Assert.AreEqual<FplValue>(y, yw.Parent.Value)
            | "yv" -> Assert.AreEqual<FplValue>(y, yv.Parent.Value)
            | "yu" -> Assert.AreEqual<FplValue>(y, yu.Parent.Value)
            | "xw" -> Assert.AreEqual<FplValue>(x, xw.Parent.Value)
            | "xv" -> Assert.AreEqual<FplValue>(x, xv.Parent.Value)
            | "xu" -> Assert.AreEqual<FplValue>(x,  xu.Parent.Value)
            | "y" -> Assert.AreEqual<FplValue>(block, y.Parent.Value)
            | "x" -> Assert.AreEqual<FplValue>(block, x.Parent.Value)
            | "block" -> Assert.AreEqual<FplValue>(theory, block.Parent.Value)
            | "theory" -> Assert.AreEqual<FplValue>(r, theory.Parent.Value)
            | "r" -> Assert.AreEqual<FplValue option>(None, r.Parent)
            | _ -> 
                Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("xw")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("yw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<TestMethod>]
    member this.TestVariablesInSignatureVariadic(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("Parent")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with 
            | "ywc" -> Assert.AreEqual<FplValue>(yw, ywc.Parent.Value)
            | "ywb" -> Assert.AreEqual<FplValue>(yw, ywb.Parent.Value)
            | "ywa" -> Assert.AreEqual<FplValue>(yw, ywa.Parent.Value)
            | "yvc" -> Assert.AreEqual<FplValue>(yv, yvc.Parent.Value)
            | "yvb" -> Assert.AreEqual<FplValue>(yv, yvb.Parent.Value)
            | "yva" -> Assert.AreEqual<FplValue>(yv, yva.Parent.Value)
            | "yuc" -> Assert.AreEqual<FplValue>(yu, yuc.Parent.Value)
            | "yub" -> Assert.AreEqual<FplValue>(yu, yub.Parent.Value)
            | "yua" -> Assert.AreEqual<FplValue>(yu, yua.Parent.Value)
            | "xwc" -> Assert.AreEqual<FplValue>(xw, xwc.Parent.Value)
            | "xwb" -> Assert.AreEqual<FplValue>(xw, xwb.Parent.Value)
            | "xwa" -> Assert.AreEqual<FplValue>(xw, xwa.Parent.Value)
            | "xvc" -> Assert.AreEqual<FplValue>(xv, xvc.Parent.Value)
            | "xvb" -> Assert.AreEqual<FplValue>(xv, xvb.Parent.Value)
            | "xva" -> Assert.AreEqual<FplValue>(xv, xva.Parent.Value)
            | "xuc" -> Assert.AreEqual<FplValue>(xu, xuc.Parent.Value)
            | "xub" -> Assert.AreEqual<FplValue>(xu, xub.Parent.Value)
            | "xua" -> Assert.AreEqual<FplValue>(xu, xua.Parent.Value)

            | "yw" -> Assert.AreEqual<FplValue>(y, yw.Parent.Value)
            | "yv" -> Assert.AreEqual<FplValue>(y, yv.Parent.Value)
            | "yu" -> Assert.AreEqual<FplValue>(y, yu.Parent.Value)
            | "xw" -> Assert.AreEqual<FplValue>(x, xw.Parent.Value)
            | "xv" -> Assert.AreEqual<FplValue>(x, xv.Parent.Value)
            | "xu" -> Assert.AreEqual<FplValue>(x,  xu.Parent.Value)
            | "y" -> Assert.AreEqual<FplValue>(block, y.Parent.Value)
            | "x" -> Assert.AreEqual<FplValue>(block, x.Parent.Value)
            | "block" -> Assert.AreEqual<FplValue>(theory, block.Parent.Value)
            | "theory" -> Assert.AreEqual<FplValue>(r, theory.Parent.Value)
            | "r" -> Assert.AreEqual<FplValue option>(None, r.Parent)
            | _ -> 
                Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", literalTrue)>]
    [<DataRow("base2", literalFalse)>]
    [<DataRow("base3", literalUndef)>]
    [<DataRow("base4", "1.")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "$1")>]
    [<DataRow("base7", "bydef Test()")>] 
    [<DataRow("base8", "Test$1")>]
    [<DataRow("base9", "Test$1()")>]
    [<DataRow("base10", "Test")>]
    [<DataRow("base11", "v")>]
    [<DataRow("base12", literalParent)>]
    [<DataRow("base13", "@1")>]
    [<DataRow("base11a", "v.x")>]
    [<DataRow("base12a", "self.x")>]
    [<DataRow("base10b", "Test()")>]
    [<DataRow("base11b", "v()")>]
    [<DataRow("base12b", "self()")>]
    [<DataRow("base13b", "@1()")>]
    [<DataRow("base10c", "Test(x, y)")>]
    [<DataRow("base11c", "v(x, y)")>]
    [<DataRow("base12c", "self(x, y)")>]
    [<DataRow("base13c", "@1(x, y)")>]
    [<DataRow("base10d", "Test[x, y]")>]
    [<DataRow("base11d", "v[x, y]")>]
    [<DataRow("base12d", "self[x, y]")>]
    [<DataRow("base13d", "@1[x.y]")>]
    [<DataRow("base10e", "Test(x, y).parent[a, b]")>]
    [<DataRow("base11e", "v(x, y).x[a, b]")>]
    [<DataRow("base12e", "self(x, y).@3[a, b]")>]
    [<DataRow("base13e", "@1(x, y).T[a, b]")>]
    [<DataRow("base10f", "Test[x, y].x(a, b)")>]
    [<DataRow("base11f", "v[x, y].x(a, b)")>]
    [<DataRow("base12f", "self[x, y].self(a, b)")>]
    [<DataRow("base13f", "@1[x.y].T(a, b)")>]
    [<DataRow("base14", "∅")>]
    [<DataRow("base15", "-x")>]
    [<DataRow("base15a", "x'")>]
    [<DataRow("base15b", "-x'")>]
    [<DataRow("base16", "-(y + x = @2 * x)")>]
    [<DataRow("base17", "(y + x' = @2 * x)'")>]
    [<DataRow("base18", "ex x:Range(a:T), y:C, z:obj {and (a,and(b,c))}")>]
    [<DataRow("base19", "exn$1 x:obj {all y:N {true}}")>]
    [<DataRow("base20", "all x:obj {not x}")>]
    [<DataRow("base21", "and (x, and(y, z))")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not (x)")>]
    [<DataRow("base22", "xor (x, xor(y, z))")>]
    [<DataRow("base23", "or (x, or(y, z))")>]
    [<DataRow("base24", "iif (x, y)")>]
    [<DataRow("base25", "impl (x, y)")>]
    [<DataRow("base26", "is (x, Nat)")>]
    [<DataRow("base27", "B()")>]
    [<DataRow("base28", "C(a,b,c,d)")>]
    [<DataRow("base29", "D(self,b,c)")>]
    [<DataRow("base30", "B(In(x))")>]
    [<DataRow("base31", "C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base32", "E(pr1, undef, false)")>]
    [<DataRow("base33", "dec ~p: pred(c: obj); p(c)")>]
    [<DataRow("base34", "is(x, Set)")>]
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestPredicateParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base6" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base7" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base8" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base9" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base10" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base11" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base12" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base13" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base11a" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base12a" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base10b" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base11b" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base12b" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base13b" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base10c" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base11c" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base12c" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base13c" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base10d" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base11d" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base12d" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base13d" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base10e" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base11e" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base12e" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base13e" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base10f" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base11f" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base12f" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base13f" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base14" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base15" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base15a" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base15b" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base16" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base17" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base18" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base19" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base20" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base21" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base21a" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base21b" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base22" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base23" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base24" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base25" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base26" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base27" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base28" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base29" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base30" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base31" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base32" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base33" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base34" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
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
        let filename = "TestCallConstructorParentClassParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let cl = theory.Scope["A"]
            let ctor = cl.Scope["A(T1, func, ind, pred)"]
            let base1 = ctor.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<FplValue>(ctor, base1.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(ctor, base1.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(ctor, base1.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(ctor, base1.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(ctor, base1.Parent.Value)
            | "base6" -> Assert.AreEqual<FplValue>(ctor, base1.Parent.Value)
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
        let filename = "TestDelegateParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base6" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
            | "base7" -> Assert.AreEqual<FplValue>(pr1, base1.Parent.Value)
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
        let filename = "TestFixNotationParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = 
                if varVal.Contains literalCl then 
                    theory.Scope["T1"]
                elif varVal.Contains literalFunc then 
                    theory.Scope["T1() -> obj"]
                else 
                    theory.Scope["T1()"]

            match var with
            | "base1" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base5a" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base6" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base7" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base8" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
            | "base9" -> Assert.AreEqual<FplValue>(theory, base1.Parent.Value)
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
        let filename = "TestMappingParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base6" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base7" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base8" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base9" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
            | "base10" -> Assert.AreEqual<FplValue>(base1, mapping.Parent.Value)
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
        let filename = "TestArgumentParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let proof = theory.Scope["T$1"]
            let arg = proof.Scope["100."]
            let just = arg.ArgList[0]
            let ainf = arg.ArgList[1]
            let numbOfJustifications = just.Scope.Count
 
            Assert.AreEqual<int>(expNumber, numbOfJustifications)

            match var with
            | "base1" -> Assert.AreEqual<FplValue>(proof, arg.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(proof, arg.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(proof, arg.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(proof, arg.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(proof, arg.Parent.Value)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", literalTrue, """!tex: "1" !eng: literalTrue !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLanguage(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestLanguageParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<FplValue>(pred, lang.Parent.Value)
            | "base1" -> Assert.AreEqual<FplValue>(pred, lang.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(pred, lang.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(pred, lang.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(pred, lang.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(pred, lang.Parent.Value)
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
        let filename = "TestLocalizationParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<FplValue>(theory, pred.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(theory, pred.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(theory, pred.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(theory, pred.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(theory, pred.Parent.Value)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", literalTrue, """!tex: "1" !eng: literalTrue !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x " \Leftrightarrow " y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p " \wedge " q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq " y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestTranslation(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestTranslationParent"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]
            let trsl = lang.ArgList[0]

            match var with
            | "base0" -> Assert.AreEqual<FplValue>(lang, trsl.Parent.Value)
            | "base1" -> Assert.AreEqual<FplValue>(lang, trsl.Parent.Value)
            | "base2" -> Assert.AreEqual<FplValue>(lang, trsl.Parent.Value)
            | "base3" -> Assert.AreEqual<FplValue>(lang, trsl.Parent.Value)
            | "base4" -> Assert.AreEqual<FplValue>(lang, trsl.Parent.Value)
            | "base5" -> Assert.AreEqual<FplValue>(lang, trsl.Parent.Value)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
