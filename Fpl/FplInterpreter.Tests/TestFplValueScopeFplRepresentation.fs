namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeFplRepresentation() =

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
            | "r" -> Assert.AreEqual("", r.FplRepresentation)
            | "theory" -> Assert.AreEqual("Test", theory.FplRepresentation)
            | "inf1" -> Assert.AreEqual("SomeInference1()", inf1.FplRepresentation)
            | "inf2" -> Assert.AreEqual("SomeInference2()", inf2.FplRepresentation)
            | "axi1" -> Assert.AreEqual("SomeAxiom1()", axi1.FplRepresentation)
            | "axi2" -> Assert.AreEqual("SomeAxiom2()", axi2.FplRepresentation)
            | "pst1" -> Assert.AreEqual("SomePostulate1()", pst1.FplRepresentation)
            | "pst2" -> Assert.AreEqual("SomePostulate2()", pst2.FplRepresentation)
            | "thm1" -> Assert.AreEqual("SomeTheorem1()", thm1.FplRepresentation)
            | "thm2" -> Assert.AreEqual("SomeTheorem2()", thm2.FplRepresentation)
            | "pro1" -> Assert.AreEqual("SomeProposition1()", pro1.FplRepresentation)
            | "pro2" -> Assert.AreEqual("SomeProposition2()", pro2.FplRepresentation)
            | "lem1" -> Assert.AreEqual("SomeLemma1()", lem1.FplRepresentation)
            | "lem2" -> Assert.AreEqual("SomeLemma2()", lem2.FplRepresentation)
            | "cor1" -> Assert.AreEqual("SomeLemma1$1()", cor1.FplRepresentation)
            | "cor2" -> Assert.AreEqual("SomeLemma2$1()", cor2.FplRepresentation)
            | "con1" -> Assert.AreEqual("SomeConjecture1()", con1.FplRepresentation)
            | "con2" -> Assert.AreEqual("SomeConjecture2()", con2.FplRepresentation)
            | "cla1" -> Assert.AreEqual("SomeClass1", cla1.FplRepresentation)
            | "cla2" -> Assert.AreEqual("SomeClass2", cla2.FplRepresentation)
            | "pre1" -> Assert.AreEqual("SomePredicate1()", pre1.FplRepresentation)
            | "pre2" -> Assert.AreEqual("SomePredicate2()", pre2.FplRepresentation)
            | "fun1" -> Assert.AreEqual("SomeFunctionalTerm1() -> obj", fun1.FplRepresentation)
            | "fun2" -> Assert.AreEqual("SomeFunctionalTerm2() -> obj", fun2.FplRepresentation)
            | "prf1" -> Assert.AreEqual("SomeTheorem1$1", prf1.FplRepresentation)
            | "prf2" -> Assert.AreEqual("SomeTheorem2$1", prf2.FplRepresentation)
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
            | "r" -> Assert.AreEqual("", r.FplRepresentation)
            | "theory" -> Assert.AreEqual("Test", theory.FplRepresentation)
            | "block" -> Assert.AreEqual("TestId", block.FplRepresentation)
            | "t1" -> Assert.AreEqual("TestId()", t1.FplRepresentation)
            | "t2" -> Assert.AreEqual("TestId(obj)", t2.FplRepresentation)
            | "t3" -> Assert.AreEqual("TestId(pred)", t3.FplRepresentation)
            | "t4" -> Assert.AreEqual("TestId(ind)", t4.FplRepresentation)
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
                | "r" -> Assert.AreEqual("", r.FplRepresentation)
                | "theory" -> Assert.AreEqual("Test", theory.FplRepresentation)
                | "thm1" -> Assert.AreEqual("TestTheorem1()", thm1.FplRepresentation)
                | "proofThm1" -> Assert.AreEqual("TestTheorem1$1", proofThm1.FplRepresentation)
                | "lem1" -> Assert.AreEqual("TestLemma1()", lem1.FplRepresentation)
                | "proofLem1" -> Assert.AreEqual("TestLemma1$1", proofLem1.FplRepresentation)
                | "prp1" -> Assert.AreEqual("TestProposition1()", prp1.FplRepresentation)
                | "proofPrp1" -> Assert.AreEqual("TestProposition1$1", proofPrp1.FplRepresentation)
                | "cor1" -> Assert.AreEqual("TestCorollary1$2()", cor1.FplRepresentation)
                | "proofCor1" -> Assert.AreEqual("TestCorollary1$2$1", proofCor1.FplRepresentation)
                | "thm2" -> Assert.AreEqual("TestTheorem2()", thm2.FplRepresentation)
                | "corThm2" -> Assert.AreEqual("TestTheorem2$1()", corThm2.FplRepresentation)
                | "lem2" -> Assert.AreEqual("TestLemma2()", lem2.FplRepresentation)
                | "corLem2" -> Assert.AreEqual("TestLemma2$1()", corLem2.FplRepresentation)
                | "prp2" -> Assert.AreEqual("TestProposition2()", prp2.FplRepresentation)
                | "corPrp2" -> Assert.AreEqual("TestProposition2$1()", corPrp2.FplRepresentation)
                | "cor2" -> Assert.AreEqual("TestCorollary2$2()", cor2.FplRepresentation)
                | "corCor2" -> Assert.AreEqual("TestCorollary2$2$1()", corCor2.FplRepresentation)
                | "con1" -> Assert.AreEqual("TestConjecture()", con1.FplRepresentation)
                | "corCon1" -> Assert.AreEqual("TestConjecture$1()", corCon1.FplRepresentation)
                | "axi1" -> Assert.AreEqual("TestAxiom()", axi1.FplRepresentation)
                | "corAxi1"  -> Assert.AreEqual("TestAxiom$1()", corAxi1.FplRepresentation) 
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
            | "r" -> Assert.AreEqual("", r.FplRepresentation)
            | "theory" -> Assert.AreEqual("Test", theory.FplRepresentation)
            | "block" -> Assert.AreEqual("TestId()", block.FplRepresentation)
            | "t1" -> Assert.AreEqual("T1()", t1.FplRepresentation)
            | "t2" -> Assert.AreEqual("T2()", t2.FplRepresentation)
            | "t3" -> Assert.AreEqual("T3() -> obj", t3.FplRepresentation)
            | "t4" -> Assert.AreEqual("T4() -> obj", t4.FplRepresentation)
            | "t5" -> Assert.AreEqual("T5() -> ind", t5.FplRepresentation)
            | "t6" -> Assert.AreEqual("T6() -> ind", t6.FplRepresentation)
            | "t7" -> Assert.AreEqual("T7() -> pred", t7.FplRepresentation)
            | "t8" -> Assert.AreEqual("T8() -> pred", t8.FplRepresentation)
            | "t9" -> Assert.AreEqual("T9() -> tpl", t9.FplRepresentation)
            | "t10" -> Assert.AreEqual("T10() -> tpl", t10.FplRepresentation)
            | "t11" -> Assert.AreEqual("T11() -> Nat", t11.FplRepresentation)
            | "t12" -> Assert.AreEqual("T12() -> Nat", t12.FplRepresentation)
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
            | "r" -> Assert.AreEqual("", r.FplRepresentation)
            | "theory" -> Assert.AreEqual("Test", theory.FplRepresentation)
            | "block" -> Assert.AreEqual("TestPredicate()", block.FplRepresentation); 
            | "x" -> Assert.AreEqual("x", x.FplRepresentation)
            | "y" -> Assert.AreEqual("y", y.FplRepresentation)
            | "xu" -> Assert.AreEqual("u", xu.FplRepresentation)
            | "xv" -> Assert.AreEqual("v", xv.FplRepresentation)
            | "xw" -> Assert.AreEqual("w", xw.FplRepresentation)
            | "yu" -> Assert.AreEqual("u", yu.FplRepresentation)
            | "yv" -> Assert.AreEqual("v", yv.FplRepresentation)
            | "yw" -> Assert.AreEqual("w", yw.FplRepresentation)
            | "xua" -> Assert.AreEqual("a", xua.FplRepresentation)
            | "xub" -> Assert.AreEqual("b", xub.FplRepresentation)
            | "xuc" -> Assert.AreEqual("c", xuc.FplRepresentation)
            | "xva" -> Assert.AreEqual("a", xva.FplRepresentation)
            | "xvb" -> Assert.AreEqual("b", xvb.FplRepresentation)
            | "xvc" -> Assert.AreEqual("c", xvc.FplRepresentation)
            | "xwa" -> Assert.AreEqual("a", xwa.FplRepresentation)
            | "xwb" -> Assert.AreEqual("b", xwb.FplRepresentation)
            | "xwc" -> Assert.AreEqual("c", xwc.FplRepresentation)
            | "yua" -> Assert.AreEqual("a", yua.FplRepresentation)
            | "yub" -> Assert.AreEqual("b", yub.FplRepresentation)
            | "yuc" -> Assert.AreEqual("c", yuc.FplRepresentation)
            | "yva" -> Assert.AreEqual("a", yva.FplRepresentation)
            | "yvb" -> Assert.AreEqual("b", yvb.FplRepresentation)
            | "yvc" -> Assert.AreEqual("c", yvc.FplRepresentation)
            | "ywa" -> Assert.AreEqual("a", ywa.FplRepresentation)
            | "ywb" -> Assert.AreEqual("b", ywb.FplRepresentation)
            | "ywc" -> Assert.AreEqual("c", ywc.FplRepresentation)
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
            | "r" -> Assert.AreEqual("", r.FplRepresentation)
            | "theory" -> Assert.AreEqual("Test", theory.FplRepresentation)
            | "block" -> Assert.AreEqual("TestPredicate()", block.FplRepresentation); 
            | "x" -> Assert.AreEqual("x", x.FplRepresentation)
            | "y" -> Assert.AreEqual("y", y.FplRepresentation)
            | "xu" -> Assert.AreEqual("u", xu.FplRepresentation)
            | "xv" -> Assert.AreEqual("v", xv.FplRepresentation)
            | "xw" -> Assert.AreEqual("w", xw.FplRepresentation)
            | "yu" -> Assert.AreEqual("u", yu.FplRepresentation)
            | "yv" -> Assert.AreEqual("v", yv.FplRepresentation)
            | "yw" -> Assert.AreEqual("w", yw.FplRepresentation)
            | "xua" -> Assert.AreEqual("a", xua.FplRepresentation)
            | "xub" -> Assert.AreEqual("b", xub.FplRepresentation)
            | "xuc" -> Assert.AreEqual("c", xuc.FplRepresentation)
            | "xva" -> Assert.AreEqual("a", xva.FplRepresentation)
            | "xvb" -> Assert.AreEqual("b", xvb.FplRepresentation)
            | "xvc" -> Assert.AreEqual("c", xvc.FplRepresentation)
            | "xwa" -> Assert.AreEqual("a", xwa.FplRepresentation)
            | "xwb" -> Assert.AreEqual("b", xwb.FplRepresentation)
            | "xwc" -> Assert.AreEqual("c", xwc.FplRepresentation)
            | "yua" -> Assert.AreEqual("a", yua.FplRepresentation)
            | "yub" -> Assert.AreEqual("b", yub.FplRepresentation)
            | "yuc" -> Assert.AreEqual("c", yuc.FplRepresentation)
            | "yva" -> Assert.AreEqual("a", yva.FplRepresentation)
            | "yvb" -> Assert.AreEqual("b", yvb.FplRepresentation)
            | "yvc" -> Assert.AreEqual("c", yvc.FplRepresentation)
            | "ywa" -> Assert.AreEqual("a", ywa.FplRepresentation)
            | "ywb" -> Assert.AreEqual("b", ywb.FplRepresentation)
            | "ywc" -> Assert.AreEqual("c", ywc.FplRepresentation)
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
            | "r" -> Assert.AreEqual("", r.FplRepresentation)
            | "theory" -> Assert.AreEqual("Test", theory.FplRepresentation)
            | "block" -> Assert.AreEqual("TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", block.FplRepresentation); 
            | "x" -> Assert.AreEqual("x", x.FplRepresentation)
            | "y" -> Assert.AreEqual("y", y.FplRepresentation)
            | "xu" -> Assert.AreEqual("u", xu.FplRepresentation)
            | "xv" -> Assert.AreEqual("v", xv.FplRepresentation)
            | "xw" -> Assert.AreEqual("w", xw.FplRepresentation)
            | "yu" -> Assert.AreEqual("u", yu.FplRepresentation)
            | "yv" -> Assert.AreEqual("v", yv.FplRepresentation)
            | "yw" -> Assert.AreEqual("w", yw.FplRepresentation)
            | "xua" -> Assert.AreEqual("a", xua.FplRepresentation)
            | "xub" -> Assert.AreEqual("b", xub.FplRepresentation)
            | "xuc" -> Assert.AreEqual("c", xuc.FplRepresentation)
            | "xva" -> Assert.AreEqual("a", xva.FplRepresentation)
            | "xvb" -> Assert.AreEqual("b", xvb.FplRepresentation)
            | "xvc" -> Assert.AreEqual("c", xvc.FplRepresentation)
            | "xwa" -> Assert.AreEqual("a", xwa.FplRepresentation)
            | "xwb" -> Assert.AreEqual("b", xwb.FplRepresentation)
            | "xwc" -> Assert.AreEqual("c", xwc.FplRepresentation)
            | "yua" -> Assert.AreEqual("a", yua.FplRepresentation)
            | "yub" -> Assert.AreEqual("b", yub.FplRepresentation)
            | "yuc" -> Assert.AreEqual("c", yuc.FplRepresentation)
            | "yva" -> Assert.AreEqual("a", yva.FplRepresentation)
            | "yvb" -> Assert.AreEqual("b", yvb.FplRepresentation)
            | "yvc" -> Assert.AreEqual("c", yvc.FplRepresentation)
            | "ywa" -> Assert.AreEqual("a", ywa.FplRepresentation)
            | "ywb" -> Assert.AreEqual("b", ywb.FplRepresentation)
            | "ywc" -> Assert.AreEqual("c", ywc.FplRepresentation)
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
            | "r" -> Assert.AreEqual("", r.FplRepresentation)
            | "theory" -> Assert.AreEqual("Test", theory.FplRepresentation)
            | "block" -> Assert.AreEqual("TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", block.FplRepresentation); 
            | "x" -> Assert.AreEqual("x", x.FplRepresentation)
            | "y" -> Assert.AreEqual("y", y.FplRepresentation)
            | "xu" -> Assert.AreEqual("u", xu.FplRepresentation)
            | "xv" -> Assert.AreEqual("v", xv.FplRepresentation)
            | "xw" -> Assert.AreEqual("w", xw.FplRepresentation)
            | "yu" -> Assert.AreEqual("u", yu.FplRepresentation)
            | "yv" -> Assert.AreEqual("v", yv.FplRepresentation)
            | "yw" -> Assert.AreEqual("w", yw.FplRepresentation)
            | "xua" -> Assert.AreEqual("a", xua.FplRepresentation)
            | "xub" -> Assert.AreEqual("b", xub.FplRepresentation)
            | "xuc" -> Assert.AreEqual("c", xuc.FplRepresentation)
            | "xva" -> Assert.AreEqual("a", xva.FplRepresentation)
            | "xvb" -> Assert.AreEqual("b", xvb.FplRepresentation)
            | "xvc" -> Assert.AreEqual("c", xvc.FplRepresentation)
            | "xwa" -> Assert.AreEqual("a", xwa.FplRepresentation)
            | "xwb" -> Assert.AreEqual("b", xwb.FplRepresentation)
            | "xwc" -> Assert.AreEqual("c", xwc.FplRepresentation)
            | "yua" -> Assert.AreEqual("a", yua.FplRepresentation)
            | "yub" -> Assert.AreEqual("b", yub.FplRepresentation)
            | "yuc" -> Assert.AreEqual("c", yuc.FplRepresentation)
            | "yva" -> Assert.AreEqual("a", yva.FplRepresentation)
            | "yvb" -> Assert.AreEqual("b", yvb.FplRepresentation)
            | "yvc" -> Assert.AreEqual("c", yvc.FplRepresentation)
            | "ywa" -> Assert.AreEqual("a", ywa.FplRepresentation)
            | "ywb" -> Assert.AreEqual("b", ywb.FplRepresentation)
            | "ywc" -> Assert.AreEqual("c", ywc.FplRepresentation)
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
            | "base1" -> Assert.AreEqual("", base1.FplRepresentation)
            | "base2" -> Assert.AreEqual("", base2.FplRepresentation)
            | "base3" -> Assert.AreEqual("", base3.FplRepresentation)
            | "base4" -> Assert.AreEqual("", base4.FplRepresentation)
            | "base5" -> Assert.AreEqual("", base5.FplRepresentation)
            | "base6" -> Assert.AreEqual("", base6.FplRepresentation)
            | "base7" -> Assert.AreEqual("", base7.FplRepresentation)
            | "base8" -> Assert.AreEqual("", base8.FplRepresentation)
            | "base9" -> Assert.AreEqual("", base9.FplRepresentation)
            | "base10" -> Assert.AreEqual("", base10.FplRepresentation)
            | "base11" -> Assert.AreEqual("", base11.FplRepresentation)
            | "base12" -> Assert.AreEqual("", base12.FplRepresentation)
            | "base13" -> Assert.AreEqual("", base13.FplRepresentation)
            | "base11a" -> Assert.AreEqual("", base11a.FplRepresentation)
            | "base12a" -> Assert.AreEqual("", base12a.FplRepresentation)
            | "base10b" -> Assert.AreEqual("", base10b.FplRepresentation)
            | "base11b" -> Assert.AreEqual("", base11b.FplRepresentation)
            | "base12b" -> Assert.AreEqual("", base12b.FplRepresentation)
            | "base13b" -> Assert.AreEqual("", base13b.FplRepresentation)
            | "base10c" -> Assert.AreEqual("", base10c.FplRepresentation)
            | "base11c" -> Assert.AreEqual("", base11c.FplRepresentation)
            | "base12c" -> Assert.AreEqual("", base12c.FplRepresentation)
            | "base13c" -> Assert.AreEqual("", base13c.FplRepresentation)
            | "base10d" -> Assert.AreEqual("", base10d.FplRepresentation)
            | "base11d" -> Assert.AreEqual("", base11d.FplRepresentation)
            | "base12d" -> Assert.AreEqual("", base12d.FplRepresentation)
            | "base13d" -> Assert.AreEqual("", base13d.FplRepresentation)
            | "base10e" -> Assert.AreEqual("", base10e.FplRepresentation)
            | "base11e" -> Assert.AreEqual("", base11e.FplRepresentation)
            | "base12e" -> Assert.AreEqual("", base12e.FplRepresentation)
            | "base13e" -> Assert.AreEqual("", base13e.FplRepresentation)
            | "base10f" -> Assert.AreEqual("", base10f.FplRepresentation)
            | "base11f" -> Assert.AreEqual("", base11f.FplRepresentation)
            | "base12f" -> Assert.AreEqual("", base12f.FplRepresentation)
            | "base13f" -> Assert.AreEqual("", base13f.FplRepresentation)
            | "base14" -> Assert.AreEqual("", base14.FplRepresentation)
            | "base15" -> Assert.AreEqual("", base15.FplRepresentation)
            | "base16" -> Assert.AreEqual("", base16.FplRepresentation)
            | "base17" -> Assert.AreEqual("", base17.FplRepresentation)
            | "base18" -> Assert.AreEqual("", base18.FplRepresentation)
            | "base19" -> Assert.AreEqual("", base19.FplRepresentation)
            | "base20" -> Assert.AreEqual("", base20.FplRepresentation)
            | "base21" -> Assert.AreEqual("", base21.FplRepresentation)
            | "base22" -> Assert.AreEqual("", base22.FplRepresentation)
            | "base23" -> Assert.AreEqual("", base23.FplRepresentation)
            | "base24" -> Assert.AreEqual("", base24.FplRepresentation)
            | "base25" -> Assert.AreEqual("", base25.FplRepresentation)
            | "base26" -> Assert.AreEqual("", base26.FplRepresentation)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)