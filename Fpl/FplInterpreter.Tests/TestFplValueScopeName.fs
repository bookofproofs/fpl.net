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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestId()", block.Name)
            | "t1" -> Assert.AreEqual("T1()", t1.Name)
            | "t2" -> Assert.AreEqual("T2()", t2.Name)
            | "t3" -> Assert.AreEqual("T3() -> obj", t3.Name)
            | "t4" -> Assert.AreEqual("T4() -> obj", t4.Name)
            | "t5" -> Assert.AreEqual("T5() -> ind", t5.Name)
            | "t6" -> Assert.AreEqual("T6() -> ind", t6.Name)
            | "t7" -> Assert.AreEqual("T7() -> pred", t7.Name)
            | "t8" -> Assert.AreEqual("T8() -> pred", t8.Name)
            | "t9" -> Assert.AreEqual("T9() -> tpl", t9.Name)
            | "t10" -> Assert.AreEqual("T10() -> tpl", t10.Name)
            | "t11" -> Assert.AreEqual("T11() -> Nat", t11.Name)
            | "t12" -> Assert.AreEqual("T12() -> Nat", t12.Name)
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", block.Name); 
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
        | Some (theory, base1,base2,base3,base4,base5, base6, base7, 
                                    base8, base9, base10, base11, base12, base13,
                                    base11a, base12a, base10b, base11b, base12b, base13b,
                                    base10c, base11c, base12c, base13c, base10d, base11d,
                                    base12d, base10e, base11e, base12e, base13d, base13e,
                                    base10f, base11f, base12f, base13f, base14, base15,
                                    base16, base17, base18, base19, base20, base21, base22,
                                    base23, base24, base25, base26) ->
            match var with
            | "base1" -> Assert.AreEqual("", base1.Name)
            | "base2" -> Assert.AreEqual("", base2.Name)
            | "base3" -> Assert.AreEqual("", base3.Name)
            | "base4" -> Assert.AreEqual("", base4.Name)
            | "base5" -> Assert.AreEqual("", base5.Name)
            | "base6" -> Assert.AreEqual("", base6.Name)
            | "base7" -> Assert.AreEqual("", base7.Name)
            | "base8" -> Assert.AreEqual("", base8.Name)
            | "base9" -> Assert.AreEqual("", base9.Name)
            | "base10" -> Assert.AreEqual("", base10.Name)
            | "base11" -> Assert.AreEqual("", base11.Name)
            | "base12" -> Assert.AreEqual("", base12.Name)
            | "base13" -> Assert.AreEqual("", base13.Name)
            | "base11a" -> Assert.AreEqual("", base11a.Name)
            | "base12a" -> Assert.AreEqual("", base12a.Name)
            | "base10b" -> Assert.AreEqual("", base10b.Name)
            | "base11b" -> Assert.AreEqual("", base11b.Name)
            | "base12b" -> Assert.AreEqual("", base12b.Name)
            | "base13b" -> Assert.AreEqual("", base13b.Name)
            | "base10c" -> Assert.AreEqual("", base10c.Name)
            | "base11c" -> Assert.AreEqual("", base11c.Name)
            | "base12c" -> Assert.AreEqual("", base12c.Name)
            | "base13c" -> Assert.AreEqual("", base13c.Name)
            | "base10d" -> Assert.AreEqual("", base10d.Name)
            | "base11d" -> Assert.AreEqual("", base11d.Name)
            | "base12d" -> Assert.AreEqual("", base12d.Name)
            | "base13d" -> Assert.AreEqual("", base13d.Name)
            | "base10e" -> Assert.AreEqual("", base10e.Name)
            | "base11e" -> Assert.AreEqual("", base11e.Name)
            | "base12e" -> Assert.AreEqual("", base12e.Name)
            | "base13e" -> Assert.AreEqual("", base13e.Name)
            | "base10f" -> Assert.AreEqual("", base10f.Name)
            | "base11f" -> Assert.AreEqual("", base11f.Name)
            | "base12f" -> Assert.AreEqual("", base12f.Name)
            | "base13f" -> Assert.AreEqual("", base13f.Name)
            | "base14" -> Assert.AreEqual("", base14.Name)
            | "base15" -> Assert.AreEqual("", base15.Name)
            | "base16" -> Assert.AreEqual("", base16.Name)
            | "base17" -> Assert.AreEqual("", base17.Name)
            | "base18" -> Assert.AreEqual("", base18.Name)
            | "base19" -> Assert.AreEqual("", base19.Name)
            | "base20" -> Assert.AreEqual("", base20.Name)
            | "base21" -> Assert.AreEqual("", base21.Name)
            | "base22" -> Assert.AreEqual("", base22.Name)
            | "base23" -> Assert.AreEqual("", base23.Name)
            | "base24" -> Assert.AreEqual("", base24.Name)
            | "base25" -> Assert.AreEqual("", base25.Name)
            | "base26" -> Assert.AreEqual("", base26.Name)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)