namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes
open CommonTestHelpers


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
    [<DataRow("t13")>]
    [<DataRow("t14")>]
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
            | "t13" -> Assert.AreEqual("T13() -> func", t13.Name)
            | "t14" -> Assert.AreEqual("T14() -> func", t14.Name)
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
    [<DataRow("base21", "and(x, y, z)")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not (x)")>]
    [<DataRow("base22", "xor(x, y, z)")>]
    [<DataRow("base23", "or(x, y, z)")>]
    [<DataRow("base24", "iif(x, y)")>]
    [<DataRow("base25", "impl(x, y)")>]
    [<DataRow("base26", "is(x, Nat)")>]
    [<DataRow("base27", "B()")>]
    [<DataRow("base28", "C(a, b, c, d)")>]
    [<DataRow("base29", "D(self, b, c)")>]
    [<DataRow("base30", "B(In(x))")>]
    [<DataRow("base31", "C(Test1(a), Test2(b, c, d))")>]
    [<DataRow("base32", "E(true, undef, false)")>]
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let stOption = prepareFplCode(fplCode, false) 
        prepareFplCode("", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope["Test"]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual(varVal, base1.Name)
            | "base2" -> Assert.AreEqual(varVal, base1.Name)
            | "base3" -> Assert.AreEqual(varVal, base1.Name)
            | "base4" -> Assert.AreEqual(varVal, base1.Name)
            | "base5" -> Assert.AreEqual(varVal, base1.Name)
            | "base6" -> Assert.AreEqual(varVal, base1.Name)
            | "base7" -> Assert.AreEqual("bydef.Test()", base1.Name)
            | "base8" -> Assert.AreEqual(varVal, base1.Name)
            | "base9" -> Assert.AreEqual(varVal, base1.Name)
            | "base10" -> Assert.AreEqual(varVal, base1.Name)
            | "base11" -> Assert.AreEqual(varVal, base1.Name)
            | "base12" -> Assert.AreEqual(varVal, base1.Name)
            | "base13" -> Assert.AreEqual(varVal, base1.Name)
            | "base11a" -> Assert.AreEqual(varVal, base1.Name)
            | "base12a" -> Assert.AreEqual(varVal, base1.Name)
            | "base10b" -> Assert.AreEqual(varVal, base1.Name)
            | "base11b" -> Assert.AreEqual(varVal, base1.Name)
            | "base12b" -> Assert.AreEqual(varVal, base1.Name)
            | "base13b" -> Assert.AreEqual(varVal, base1.Name)
            | "base10c" -> Assert.AreEqual(varVal, base1.Name)
            | "base11c" -> Assert.AreEqual(varVal, base1.Name)
            | "base12c" -> Assert.AreEqual(varVal, base1.Name)
            | "base13c" -> Assert.AreEqual(varVal, base1.Name)
            | "base10d" -> Assert.AreEqual(varVal, base1.Name)
            | "base11d" -> Assert.AreEqual(varVal, base1.Name)
            | "base12d" -> Assert.AreEqual(varVal, base1.Name)
            | "base13d" -> Assert.AreEqual(varVal, base1.Name)
            | "base10e" -> Assert.AreEqual(varVal, base1.Name)
            | "base11e" -> Assert.AreEqual(varVal, base1.Name)
            | "base12e" -> Assert.AreEqual(varVal, base1.Name)
            | "base13e" -> Assert.AreEqual(varVal, base1.Name)
            | "base10f" -> Assert.AreEqual(varVal, base1.Name)
            | "base11f" -> Assert.AreEqual(varVal, base1.Name)
            | "base12f" -> Assert.AreEqual(varVal, base1.Name)
            | "base13f" -> Assert.AreEqual(varVal, base1.Name)
            | "base14" -> Assert.AreEqual(varVal, base1.Name)
            | "base15" -> Assert.AreEqual("-(x)", base1.Name)
            | "base15a" -> Assert.AreEqual("'(x)", base1.Name)
            | "base15b" -> Assert.AreEqual("'(-(x))", base1.Name)
            | "base16" -> Assert.AreEqual("-(+(y, =(x, *(2, x))))", base1.Name)
            | "base17" -> Assert.AreEqual("'(+(y, =('(x), *(2, x))))", base1.Name)
            | "base18" -> Assert.AreEqual(varVal, base1.Name)
            | "base19" -> Assert.AreEqual(varVal, base1.Name)
            | "base20" -> Assert.AreEqual(varVal, base1.Name)
            | "base21" -> Assert.AreEqual(varVal, base1.Name)
            | "base21a" -> Assert.AreEqual("not x", base1.Name)
            | "base21b" -> Assert.AreEqual("not x", base1.Name)
            | "base22" -> Assert.AreEqual(varVal, base1.Name)
            | "base23" -> Assert.AreEqual(varVal, base1.Name)
            | "base24" -> Assert.AreEqual(varVal, base1.Name)
            | "base25" -> Assert.AreEqual(varVal, base1.Name)
            | "base26" -> Assert.AreEqual(varVal, base1.Name)
            | "base27" -> Assert.AreEqual(varVal, base1.Name)
            | "base28" -> Assert.AreEqual(varVal, base1.Name)
            | "base29" -> Assert.AreEqual(varVal, base1.Name)
            | "base30" -> Assert.AreEqual(varVal, base1.Name)
            | "base31" -> Assert.AreEqual(varVal, base1.Name)
            | "base32" -> Assert.AreEqual(varVal, base1.Name)
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
        let stOption = prepareFplCode(fplCode, false) 
        prepareFplCode("", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope["Test"]
            let cl = theory.Scope["A"]
            let ctor = cl.Scope["A(T1, func, ind, pred)"]
            let base1 = ctor.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual("bas.B()", base1.Name)
            | "base2" -> Assert.AreEqual("bas.C(a, b, c, d)", base1.Name)
            | "base3" -> Assert.AreEqual("bas.D(self, a, b)", base1.Name)
            | "base4" -> Assert.AreEqual("bas.B(In(x))", base1.Name)
            | "base5" -> Assert.AreEqual("bas.C(Test1(a), Test2(b, c, d))", base1.Name)
            | "base6" -> Assert.AreEqual("bas.E(true, undef, false)", base1.Name)
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
        let stOption = prepareFplCode(fplCode, false) 
        prepareFplCode("", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope["Test"]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual("del.B()", base1.Name)
            | "base2" -> Assert.AreEqual("del.C(a, b, c, d)", base1.Name)
            | "base3" -> Assert.AreEqual("del.D(self, b, c)", base1.Name)
            | "base4" -> Assert.AreEqual("del.B(In(x))", base1.Name)
            | "base5" -> Assert.AreEqual("del.Test()", base1.Name)
            | "base6" -> Assert.AreEqual("del.C(Test1(a), Test2(b, c, d))", base1.Name)
            | "base7" -> Assert.AreEqual("del.E(true, undef, false)", base1.Name)
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
        let stOption = prepareFplCode(fplCode, false) 
        prepareFplCode("", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope["Test"]
            let base1 = 
                if varVal.Contains "cl" then 
                    theory.Scope["T1"]
                elif varVal.Contains "func" then 
                    theory.Scope["T1() -> obj"]
                else 
                    theory.Scope["T1()"]

            match var with
            | "base1" -> Assert.AreEqual("T1()", base1.Name)
            | "base2" -> Assert.AreEqual("T1()", base1.Name)
            | "base3" -> Assert.AreEqual("T1()", base1.Name)
            | "base4" -> Assert.AreEqual("T1()", base1.Name)
            | "base5" -> Assert.AreEqual("T1", base1.Name)
            | "base5a" -> Assert.AreEqual("T1", base1.Name)
            | "base6" -> Assert.AreEqual("T1() -> obj", base1.Name)
            | "base7" -> Assert.AreEqual("T1() -> obj", base1.Name)
            | "base8" -> Assert.AreEqual("T1() -> obj", base1.Name)
            | "base9" -> Assert.AreEqual("T1() -> obj", base1.Name)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
