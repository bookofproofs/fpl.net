namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeName() =

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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "inf1" -> Assert.AreEqual("SomeInference1()", inf1.Name)
            | "inf2" -> Assert.AreEqual("SomeInference2()", inf2.Name)
            | "axi1" -> Assert.AreEqual("SomeAxiom1()", axi1.Name)
            | "axi2" -> Assert.AreEqual("SomeAxiom2()", axi2.Name)
            | "pst1" -> Assert.AreEqual("SomePostulate1()", pst1.Name)
            | "pst2" -> Assert.AreEqual("SomePostulate2()", pst2.Name)
            | "thm1" -> Assert.AreEqual("SomeTheorem1()", thm1.Name)
            | "thm2" -> Assert.AreEqual("SomeTheorem2()", thm2.Name)
            | "pro1" -> Assert.AreEqual("SomeProposition1()", pro1.Name)
            | "pro2" -> Assert.AreEqual("SomeProposition2()", pro2.Name)
            | "lem1" -> Assert.AreEqual("SomeLemma1()", lem1.Name)
            | "lem2" -> Assert.AreEqual("SomeLemma2()", lem2.Name)
            | "cor1" -> Assert.AreEqual("SomeLemma1$1()", cor1.Name)
            | "cor2" -> Assert.AreEqual("SomeLemma2$1()", cor2.Name)
            | "con1" -> Assert.AreEqual("SomeConjecture1()", con1.Name)
            | "con2" -> Assert.AreEqual("SomeConjecture2()", con2.Name)
            | "cla1" -> Assert.AreEqual("SomeClass1", cla1.Name)
            | "cla2" -> Assert.AreEqual("SomeClass2", cla2.Name)
            | "pre1" -> Assert.AreEqual("SomePredicate1()", pre1.Name)
            | "pre2" -> Assert.AreEqual("SomePredicate2()", pre2.Name)
            | "fun1" -> Assert.AreEqual("SomeFunctionalTerm1() -> obj", fun1.Name)
            | "fun2" -> Assert.AreEqual("SomeFunctionalTerm2() -> obj", fun2.Name)
            | "prf1" -> Assert.AreEqual("SomeTheorem1$1", prf1.Name)
            | "prf2" -> Assert.AreEqual("SomeTheorem2$1", prf2.Name)
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestId", block.Name)
            | "t1" -> Assert.AreEqual("TestId()", t1.Name)
            | "t2" -> Assert.AreEqual("TestId(obj)", t2.Name)
            | "t3" -> Assert.AreEqual("TestId(pred)", t3.Name)
            | "t4" -> Assert.AreEqual("TestId(ind)", t4.Name)
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
                | "r" -> Assert.AreEqual("", r.Name)
                | "theory" -> Assert.AreEqual("Test", theory.Name)
                | "thm1" -> Assert.AreEqual("TestTheorem1()", thm1.Name)
                | "proofThm1" -> Assert.AreEqual("TestTheorem1$1", proofThm1.Name)
                | "lem1" -> Assert.AreEqual("TestLemma1()", lem1.Name)
                | "proofLem1" -> Assert.AreEqual("TestLemma1$1", proofLem1.Name)
                | "prp1" -> Assert.AreEqual("TestProposition1()", prp1.Name)
                | "proofPrp1" -> Assert.AreEqual("TestProposition1$1", proofPrp1.Name)
                | "cor1" -> Assert.AreEqual("TestCorollary1$2()", cor1.Name)
                | "proofCor1" -> Assert.AreEqual("TestCorollary1$2$1", proofCor1.Name)
                | "thm2" -> Assert.AreEqual("TestTheorem2()", thm2.Name)
                | "corThm2" -> Assert.AreEqual("TestTheorem2$1()", corThm2.Name)
                | "lem2" -> Assert.AreEqual("TestLemma2()", lem2.Name)
                | "corLem2" -> Assert.AreEqual("TestLemma2$1()", corLem2.Name)
                | "prp2" -> Assert.AreEqual("TestProposition2()", prp2.Name)
                | "corPrp2" -> Assert.AreEqual("TestProposition2$1()", corPrp2.Name)
                | "cor2" -> Assert.AreEqual("TestCorollary2$2()", cor2.Name)
                | "corCor2" -> Assert.AreEqual("TestCorollary2$2$1()", corCor2.Name)
                | "con1" -> Assert.AreEqual("TestConjecture()", con1.Name)
                | "corCon1" -> Assert.AreEqual("TestConjecture$1()", corCon1.Name)
                | "axi1" -> Assert.AreEqual("TestAxiom()", axi1.Name)
                | "corAxi1"  -> Assert.AreEqual("TestAxiom$1()", corAxi1.Name) 
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestId()", block.Name)
            | "t1" -> Assert.AreEqual("T1()", t1.Name)
            | "t2" -> Assert.AreEqual("T2()", t2.Name)
            | "t3" -> Assert.AreEqual("T3() -> obj", t3.Name)
            | "t4" -> Assert.AreEqual("T4() -> obj", t4.Name)
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestPredicate()", block.Name); 
            | "x" -> Assert.AreEqual("x", x.Name)
            | "y" -> Assert.AreEqual("y", y.Name)
            | "xu" -> Assert.AreEqual("u", xu.Name)
            | "xv" -> Assert.AreEqual("v", xv.Name)
            | "xw" -> Assert.AreEqual("w", xw.Name)
            | "yu" -> Assert.AreEqual("u", yu.Name)
            | "yv" -> Assert.AreEqual("v", yv.Name)
            | "yw" -> Assert.AreEqual("w", yw.Name)
            | "xua" -> Assert.AreEqual("a", xua.Name)
            | "xub" -> Assert.AreEqual("b", xub.Name)
            | "xuc" -> Assert.AreEqual("c", xuc.Name)
            | "xva" -> Assert.AreEqual("a", xva.Name)
            | "xvb" -> Assert.AreEqual("b", xvb.Name)
            | "xvc" -> Assert.AreEqual("c", xvc.Name)
            | "xwa" -> Assert.AreEqual("a", xwa.Name)
            | "xwb" -> Assert.AreEqual("b", xwb.Name)
            | "xwc" -> Assert.AreEqual("c", xwc.Name)
            | "yua" -> Assert.AreEqual("a", yua.Name)
            | "yub" -> Assert.AreEqual("b", yub.Name)
            | "yuc" -> Assert.AreEqual("c", yuc.Name)
            | "yva" -> Assert.AreEqual("a", yva.Name)
            | "yvb" -> Assert.AreEqual("b", yvb.Name)
            | "yvc" -> Assert.AreEqual("c", yvc.Name)
            | "ywa" -> Assert.AreEqual("a", ywa.Name)
            | "ywb" -> Assert.AreEqual("b", ywb.Name)
            | "ywc" -> Assert.AreEqual("c", ywc.Name)
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestPredicate()", block.Name); 
            | "x" -> Assert.AreEqual("x", x.Name)
            | "y" -> Assert.AreEqual("y", y.Name)
            | "xu" -> Assert.AreEqual("u", xu.Name)
            | "xv" -> Assert.AreEqual("v", xv.Name)
            | "xw" -> Assert.AreEqual("w", xw.Name)
            | "yu" -> Assert.AreEqual("u", yu.Name)
            | "yv" -> Assert.AreEqual("v", yv.Name)
            | "yw" -> Assert.AreEqual("w", yw.Name)
            | "xua" -> Assert.AreEqual("a", xua.Name)
            | "xub" -> Assert.AreEqual("b", xub.Name)
            | "xuc" -> Assert.AreEqual("c", xuc.Name)
            | "xva" -> Assert.AreEqual("a", xva.Name)
            | "xvb" -> Assert.AreEqual("b", xvb.Name)
            | "xvc" -> Assert.AreEqual("c", xvc.Name)
            | "xwa" -> Assert.AreEqual("a", xwa.Name)
            | "xwb" -> Assert.AreEqual("b", xwb.Name)
            | "xwc" -> Assert.AreEqual("c", xwc.Name)
            | "yua" -> Assert.AreEqual("a", yua.Name)
            | "yub" -> Assert.AreEqual("b", yub.Name)
            | "yuc" -> Assert.AreEqual("c", yuc.Name)
            | "yva" -> Assert.AreEqual("a", yva.Name)
            | "yvb" -> Assert.AreEqual("b", yvb.Name)
            | "yvc" -> Assert.AreEqual("c", yvc.Name)
            | "ywa" -> Assert.AreEqual("a", ywa.Name)
            | "ywb" -> Assert.AreEqual("b", ywb.Name)
            | "ywc" -> Assert.AreEqual("c", ywc.Name)
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", block.Name); 
            | "x" -> Assert.AreEqual("x", x.Name)
            | "y" -> Assert.AreEqual("y", y.Name)
            | "xu" -> Assert.AreEqual("u", xu.Name)
            | "xv" -> Assert.AreEqual("v", xv.Name)
            | "xw" -> Assert.AreEqual("w", xw.Name)
            | "yu" -> Assert.AreEqual("u", yu.Name)
            | "yv" -> Assert.AreEqual("v", yv.Name)
            | "yw" -> Assert.AreEqual("w", yw.Name)
            | "xua" -> Assert.AreEqual("a", xua.Name)
            | "xub" -> Assert.AreEqual("b", xub.Name)
            | "xuc" -> Assert.AreEqual("c", xuc.Name)
            | "xva" -> Assert.AreEqual("a", xva.Name)
            | "xvb" -> Assert.AreEqual("b", xvb.Name)
            | "xvc" -> Assert.AreEqual("c", xvc.Name)
            | "xwa" -> Assert.AreEqual("a", xwa.Name)
            | "xwb" -> Assert.AreEqual("b", xwb.Name)
            | "xwc" -> Assert.AreEqual("c", xwc.Name)
            | "yua" -> Assert.AreEqual("a", yua.Name)
            | "yub" -> Assert.AreEqual("b", yub.Name)
            | "yuc" -> Assert.AreEqual("c", yuc.Name)
            | "yva" -> Assert.AreEqual("a", yva.Name)
            | "yvb" -> Assert.AreEqual("b", yvb.Name)
            | "yvc" -> Assert.AreEqual("c", yvc.Name)
            | "ywa" -> Assert.AreEqual("a", ywa.Name)
            | "ywb" -> Assert.AreEqual("b", ywb.Name)
            | "ywc" -> Assert.AreEqual("c", ywc.Name)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)