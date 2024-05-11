namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeQualifiedName() =

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
            | "r" -> Assert.AreEqual("", r.QualifiedName)
            | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
            | "inf1" -> Assert.AreEqual("Test.SomeInference1()", inf1.QualifiedName)
            | "inf2" -> Assert.AreEqual("Test.SomeInference2()", inf2.QualifiedName)
            | "axi1" -> Assert.AreEqual("Test.SomeAxiom1()", axi1.QualifiedName)
            | "axi2" -> Assert.AreEqual("Test.SomeAxiom2()", axi2.QualifiedName)
            | "pst1" -> Assert.AreEqual("Test.SomePostulate1()", pst1.QualifiedName)
            | "pst2" -> Assert.AreEqual("Test.SomePostulate2()", pst2.QualifiedName)
            | "thm1" -> Assert.AreEqual("Test.SomeTheorem1()", thm1.QualifiedName)
            | "thm2" -> Assert.AreEqual("Test.SomeTheorem2()", thm2.QualifiedName)
            | "pro1" -> Assert.AreEqual("Test.SomeProposition1()", pro1.QualifiedName)
            | "pro2" -> Assert.AreEqual("Test.SomeProposition2()", pro2.QualifiedName)
            | "lem1" -> Assert.AreEqual("Test.SomeLemma1()", lem1.QualifiedName)
            | "lem2" -> Assert.AreEqual("Test.SomeLemma2()", lem2.QualifiedName)
            | "cor1" -> Assert.AreEqual("Test.SomeLemma1().SomeLemma1$1()", cor1.QualifiedName)
            | "cor2" -> Assert.AreEqual("Test.SomeLemma2().SomeLemma2$1()", cor2.QualifiedName)
            | "con1" -> Assert.AreEqual("Test.SomeConjecture1()", con1.QualifiedName)
            | "con2" -> Assert.AreEqual("Test.SomeConjecture2()", con2.QualifiedName)
            | "cla1" -> Assert.AreEqual("Test.SomeClass1", cla1.QualifiedName)
            | "cla2" -> Assert.AreEqual("Test.SomeClass2", cla2.QualifiedName)
            | "pre1" -> Assert.AreEqual("Test.SomePredicate1()", pre1.QualifiedName)
            | "pre2" -> Assert.AreEqual("Test.SomePredicate2()", pre2.QualifiedName)
            | "fun1" -> Assert.AreEqual("Test.SomeFunctionalTerm1() -> obj", fun1.QualifiedName)
            | "fun2" -> Assert.AreEqual("Test.SomeFunctionalTerm2() -> obj", fun2.QualifiedName)
            | "prf1" -> Assert.AreEqual("Test.SomeTheorem1().SomeTheorem1$1", prf1.QualifiedName)
            | "prf2" -> Assert.AreEqual("Test.SomeTheorem2().SomeTheorem2$1", prf2.QualifiedName)
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
            | "r" -> Assert.AreEqual("", r.QualifiedName)
            | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
            | "block" -> Assert.AreEqual("Test.TestId", block.QualifiedName)
            | "t1" -> Assert.AreEqual("Test.TestId.TestId()", t1.QualifiedName)
            | "t2" -> Assert.AreEqual("Test.TestId.TestId(obj)", t2.QualifiedName)
            | "t3" -> Assert.AreEqual("Test.TestId.TestId(pred)", t3.QualifiedName)
            | "t4" -> Assert.AreEqual("Test.TestId.TestId(ind)", t4.QualifiedName)
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
                | "r" -> Assert.AreEqual("", r.QualifiedName)
                | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
                | "thm1" -> Assert.AreEqual("Test.TestTheorem1()", thm1.QualifiedName)
                | "proofThm1" -> Assert.AreEqual("Test.TestTheorem1().TestTheorem1$1", proofThm1.QualifiedName)
                | "lem1" -> Assert.AreEqual("Test.TestLemma1()", lem1.QualifiedName)
                | "proofLem1" -> Assert.AreEqual("Test.TestLemma1().TestLemma1$1", proofLem1.QualifiedName)
                | "prp1" -> Assert.AreEqual("Test.TestProposition1()", prp1.QualifiedName)
                | "proofPrp1" -> Assert.AreEqual("Test.TestProposition1().TestProposition1$1", proofPrp1.QualifiedName)
                | "cor1" -> Assert.AreEqual("Test.TestCorollary1$2()", cor1.QualifiedName)
                | "proofCor1" -> Assert.AreEqual("Test.TestCorollary1$2().TestCorollary1$2$1", proofCor1.QualifiedName)
                | "thm2" -> Assert.AreEqual("Test.TestTheorem2()", thm2.QualifiedName)
                | "corThm2" -> Assert.AreEqual("Test.TestTheorem2().TestTheorem2$1()", corThm2.QualifiedName)
                | "lem2" -> Assert.AreEqual("Test.TestLemma2()", lem2.QualifiedName)
                | "corLem2" -> Assert.AreEqual("Test.TestLemma2().TestLemma2$1()", corLem2.QualifiedName)
                | "prp2" -> Assert.AreEqual("Test.TestProposition2()", prp2.QualifiedName)
                | "corPrp2" -> Assert.AreEqual("Test.TestProposition2().TestProposition2$1()", corPrp2.QualifiedName)
                | "cor2" -> Assert.AreEqual("Test.TestCorollary2$2()", cor2.QualifiedName)
                | "corCor2" -> Assert.AreEqual("Test.TestCorollary2$2().TestCorollary2$2$1()", corCor2.QualifiedName)
                | "con1" -> Assert.AreEqual("Test.TestConjecture()", con1.QualifiedName)
                | "corCon1" -> Assert.AreEqual("Test.TestConjecture().TestConjecture$1()", corCon1.QualifiedName)
                | "axi1" -> Assert.AreEqual("Test.TestAxiom()", axi1.QualifiedName)
                | "corAxi1"  -> Assert.AreEqual("Test.TestAxiom().TestAxiom$1()", corAxi1.QualifiedName) 
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
            | "r" -> Assert.AreEqual("", r.QualifiedName)
            | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
            | "block" -> Assert.AreEqual("Test.TestId()", block.QualifiedName)
            | "t1" -> Assert.AreEqual("Test.TestId().T1()", t1.QualifiedName)
            | "t2" -> Assert.AreEqual("Test.TestId().T2()", t2.QualifiedName)
            | "t3" -> Assert.AreEqual("Test.TestId().T3() -> obj", t3.QualifiedName)
            | "t4" -> Assert.AreEqual("Test.TestId().T4() -> obj", t4.QualifiedName)
            | "t5" -> Assert.AreEqual("Test.TestId().T5() -> ind", t5.QualifiedName)
            | "t6" -> Assert.AreEqual("Test.TestId().T6() -> ind", t6.QualifiedName)
            | "t7" -> Assert.AreEqual("Test.TestId().T7() -> pred", t7.QualifiedName)
            | "t8" -> Assert.AreEqual("Test.TestId().T8() -> pred", t8.QualifiedName)
            | "t9" -> Assert.AreEqual("Test.TestId().T9() -> tpl", t9.QualifiedName)
            | "t10" -> Assert.AreEqual("Test.TestId().T10() -> tpl", t10.QualifiedName)
            | "t11" -> Assert.AreEqual("Test.TestId().T11() -> Nat", t11.QualifiedName)
            | "t12" -> Assert.AreEqual("Test.TestId().T12() -> Nat", t12.QualifiedName)
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
            | "r" -> Assert.AreEqual("", r.QualifiedName)
            | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
            | "block" -> Assert.AreEqual("Test.TestPredicate()", block.QualifiedName)
            | "x" -> Assert.AreEqual("x", x.QualifiedName)
            | "y" -> Assert.AreEqual("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual("y.w.c", ywc.QualifiedName)
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
    member this.TestVariablesInBlockVariadic(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual("", r.QualifiedName)
            | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
            | "block" -> Assert.AreEqual("Test.TestPredicate()", block.QualifiedName)
            | "x" -> Assert.AreEqual("x", x.QualifiedName)
            | "y" -> Assert.AreEqual("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual("y.w.c", ywc.QualifiedName)
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
    member this.TestVariablesInSignature(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInSignature()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual("", r.QualifiedName)
            | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
            | "block" -> Assert.AreEqual("Test.TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", block.QualifiedName)
            | "x" -> Assert.AreEqual("x", x.QualifiedName)
            | "y" -> Assert.AreEqual("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual("y.w.c", ywc.QualifiedName)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual("", r.QualifiedName)
            | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
            | "block" -> Assert.AreEqual("Test.TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", block.QualifiedName)
            | "x" -> Assert.AreEqual("x", x.QualifiedName)
            | "y" -> Assert.AreEqual("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual("y.w.c", ywc.QualifiedName)
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

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
            | "base1" -> Assert.AreEqual("", base1.QualifiedName)
            | "base2" -> Assert.AreEqual("", base2.QualifiedName)
            | "base3" -> Assert.AreEqual("", base3.QualifiedName)
            | "base4" -> Assert.AreEqual("", base4.QualifiedName)
            | "base5" -> Assert.AreEqual("", base5.QualifiedName)
            | "base6" -> Assert.AreEqual("", base6.QualifiedName)
            | "base7" -> Assert.AreEqual("", base7.QualifiedName)
            | "base8" -> Assert.AreEqual("", base8.QualifiedName)
            | "base9" -> Assert.AreEqual("", base9.QualifiedName)
            | "base10" -> Assert.AreEqual("", base10.QualifiedName)
            | "base11" -> Assert.AreEqual("", base11.QualifiedName)
            | "base12" -> Assert.AreEqual("", base12.QualifiedName)
            | "base13" -> Assert.AreEqual("", base13.QualifiedName)
            | "base11a" -> Assert.AreEqual("", base11a.QualifiedName)
            | "base12a" -> Assert.AreEqual("", base12a.QualifiedName)
            | "base10b" -> Assert.AreEqual("", base10b.QualifiedName)
            | "base11b" -> Assert.AreEqual("", base11b.QualifiedName)
            | "base12b" -> Assert.AreEqual("", base12b.QualifiedName)
            | "base13b" -> Assert.AreEqual("", base13b.QualifiedName)
            | "base10c" -> Assert.AreEqual("", base10c.QualifiedName)
            | "base11c" -> Assert.AreEqual("", base11c.QualifiedName)
            | "base12c" -> Assert.AreEqual("", base12c.QualifiedName)
            | "base13c" -> Assert.AreEqual("", base13c.QualifiedName)
            | "base10d" -> Assert.AreEqual("", base10d.QualifiedName)
            | "base11d" -> Assert.AreEqual("", base11d.QualifiedName)
            | "base12d" -> Assert.AreEqual("", base12d.QualifiedName)
            | "base13d" -> Assert.AreEqual("", base13d.QualifiedName)
            | "base10e" -> Assert.AreEqual("", base10e.QualifiedName)
            | "base11e" -> Assert.AreEqual("", base11e.QualifiedName)
            | "base12e" -> Assert.AreEqual("", base12e.QualifiedName)
            | "base13e" -> Assert.AreEqual("", base13e.QualifiedName)
            | "base10f" -> Assert.AreEqual("", base10f.QualifiedName)
            | "base11f" -> Assert.AreEqual("", base11f.QualifiedName)
            | "base12f" -> Assert.AreEqual("", base12f.QualifiedName)
            | "base13f" -> Assert.AreEqual("", base13f.QualifiedName)
            | "base14" -> Assert.AreEqual("", base14.QualifiedName)
            | "base15" -> Assert.AreEqual("", base15.QualifiedName)
            | "base16" -> Assert.AreEqual("", base16.QualifiedName)
            | "base17" -> Assert.AreEqual("", base17.QualifiedName)
            | "base18" -> Assert.AreEqual("", base18.QualifiedName)
            | "base19" -> Assert.AreEqual("", base19.QualifiedName)
            | "base20" -> Assert.AreEqual("", base20.QualifiedName)
            | "base21" -> Assert.AreEqual("", base21.QualifiedName)
            | "base22" -> Assert.AreEqual("", base22.QualifiedName)
            | "base23" -> Assert.AreEqual("", base23.QualifiedName)
            | "base24" -> Assert.AreEqual("", base24.QualifiedName)
            | "base25" -> Assert.AreEqual("", base25.QualifiedName)
            | "base26" -> Assert.AreEqual("", base26.QualifiedName)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
