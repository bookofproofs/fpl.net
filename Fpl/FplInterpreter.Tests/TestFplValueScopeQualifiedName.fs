namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes
open CommonTestHelpers

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
        let res = CommonFplValueTestCases.ScopeBlocks("QualifiedName") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.QualifiedName)
            | "theory" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName", theory.QualifiedName)
            | "inf1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeInference1()", inf1.QualifiedName)
            | "inf2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeInference2()", inf2.QualifiedName)
            | "axi1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeAxiom1()", axi1.QualifiedName)
            | "axi2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeAxiom2()", axi2.QualifiedName)
            | "pst1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomePostulate1()", pst1.QualifiedName)
            | "pst2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomePostulate2()", pst2.QualifiedName)
            | "thm1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeTheorem1()", thm1.QualifiedName)
            | "thm2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeTheorem2()", thm2.QualifiedName)
            | "pro1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeProposition1()", pro1.QualifiedName)
            | "pro2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeProposition2()", pro2.QualifiedName)
            | "lem1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeLemma1()", lem1.QualifiedName)
            | "lem2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeLemma2()", lem2.QualifiedName)
            | "cor1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeLemma1().SomeLemma1$1()", cor1.QualifiedName)
            | "cor2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeLemma2().SomeLemma2$1()", cor2.QualifiedName)
            | "con1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeConjecture1()", con1.QualifiedName)
            | "con2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeConjecture2()", con2.QualifiedName)
            | "cla1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeClass1", cla1.QualifiedName)
            | "cla2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeClass2", cla2.QualifiedName)
            | "pre1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomePredicate1()", pre1.QualifiedName)
            | "pre2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomePredicate2()", pre2.QualifiedName)
            | "fun1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm1() -> obj", fun1.QualifiedName)
            | "fun2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm2() -> obj", fun2.QualifiedName)
            | "prf1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeTheorem1().SomeTheorem1$1", prf1.QualifiedName)
            | "prf2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeTheorem2().SomeTheorem2$1", prf2.QualifiedName)
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
        let res = CommonFplValueTestCases.ScopeConstructors("QualifiedName") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.QualifiedName)
            | "theory" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName", theory.QualifiedName)
            | "block" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId", block.QualifiedName)
            | "t1" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId.TestId()", t1.QualifiedName)
            | "t2" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId.TestId(obj)", t2.QualifiedName)
            | "t3" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId.TestId(pred)", t3.QualifiedName)
            | "t4" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId.TestId(ind)", t4.QualifiedName)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("QualifiedName") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<string>("", r.QualifiedName)
                | "theory" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName", theory.QualifiedName)
                | "thm1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestTheorem1()", thm1.QualifiedName)
                | "proofThm1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestTheorem1().TestTheorem1$1", proofThm1.QualifiedName)
                | "lem1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestLemma1()", lem1.QualifiedName)
                | "proofLem1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestLemma1().TestLemma1$1", proofLem1.QualifiedName)
                | "prp1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestProposition1()", prp1.QualifiedName)
                | "proofPrp1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestProposition1().TestProposition1$1", proofPrp1.QualifiedName)
                | "cor1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestCorollary1$2()", cor1.QualifiedName)
                | "proofCor1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestCorollary1$2().TestCorollary1$2$1", proofCor1.QualifiedName)
                | "thm2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestTheorem2()", thm2.QualifiedName)
                | "corThm2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestTheorem2().TestTheorem2$1()", corThm2.QualifiedName)
                | "lem2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestLemma2()", lem2.QualifiedName)
                | "corLem2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestLemma2().TestLemma2$1()", corLem2.QualifiedName)
                | "prp2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestProposition2()", prp2.QualifiedName)
                | "corPrp2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestProposition2().TestProposition2$1()", corPrp2.QualifiedName)
                | "cor2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestCorollary2$2()", cor2.QualifiedName)
                | "corCor2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestCorollary2$2().TestCorollary2$2$1()", corCor2.QualifiedName)
                | "con1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestConjecture()", con1.QualifiedName)
                | "corCon1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestConjecture().TestConjecture$1()", corCon1.QualifiedName)
                | "axi1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestAxiom()", axi1.QualifiedName)
                | "corAxi1"  -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestAxiom().TestAxiom$1()", corAxi1.QualifiedName) 
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
        let res = CommonFplValueTestCases.ScopeProperties("QualifiedName") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.QualifiedName)
            | "theory" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName", theory.QualifiedName)
            | "block" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId()", block.QualifiedName)
            | "t1" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T1()", t1.QualifiedName)
            | "t2" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T2()", t2.QualifiedName)
            | "t3" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T3() -> obj", t3.QualifiedName)
            | "t4" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T4() -> obj", t4.QualifiedName)
            | "t5" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T5() -> ind", t5.QualifiedName)
            | "t6" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T6() -> ind", t6.QualifiedName)
            | "t7" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T7() -> pred", t7.QualifiedName)
            | "t8" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T8() -> pred", t8.QualifiedName)
            | "t9" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T9() -> tpl", t9.QualifiedName)
            | "t10" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T10() -> tpl", t10.QualifiedName)
            | "t11" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T11() -> Nat", t11.QualifiedName)
            | "t12" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T12() -> Nat", t12.QualifiedName)
            | "t13" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T13() -> func", t13.QualifiedName)
            | "t14" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T14() -> func", t14.QualifiedName)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("QualifiedName")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.QualifiedName)
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInBlockQualifiedName", theory.QualifiedName)
            | "block" -> Assert.AreEqual<string>("TestScopeVariablesInBlockQualifiedName.TestPredicate()", block.QualifiedName)
            | "x" -> Assert.AreEqual<string>("x", x.QualifiedName)
            | "y" -> Assert.AreEqual<string>("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual<string>("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual<string>("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual<string>("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual<string>("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual<string>("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual<string>("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual<string>("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual<string>("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual<string>("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual<string>("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual<string>("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual<string>("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual<string>("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual<string>("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual<string>("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual<string>("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual<string>("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual<string>("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual<string>("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual<string>("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual<string>("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual<string>("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual<string>("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual<string>("y.w.c", ywc.QualifiedName)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("QualifiedName")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.QualifiedName)
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicQualifiedName", theory.QualifiedName)
            | "block" -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicQualifiedName.TestPredicate()", block.QualifiedName)
            | "x" -> Assert.AreEqual<string>("x", x.QualifiedName)
            | "y" -> Assert.AreEqual<string>("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual<string>("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual<string>("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual<string>("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual<string>("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual<string>("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual<string>("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual<string>("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual<string>("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual<string>("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual<string>("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual<string>("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual<string>("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual<string>("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual<string>("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual<string>("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual<string>("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual<string>("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual<string>("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual<string>("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual<string>("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual<string>("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual<string>("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual<string>("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual<string>("y.w.c", ywc.QualifiedName)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("QualifiedName")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.QualifiedName)
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureQualifiedName", theory.QualifiedName)
            | "block" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureQualifiedName.TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", block.QualifiedName)
            | "x" -> Assert.AreEqual<string>("x", x.QualifiedName)
            | "y" -> Assert.AreEqual<string>("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual<string>("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual<string>("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual<string>("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual<string>("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual<string>("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual<string>("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual<string>("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual<string>("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual<string>("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual<string>("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual<string>("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual<string>("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual<string>("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual<string>("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual<string>("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual<string>("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual<string>("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual<string>("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual<string>("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual<string>("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual<string>("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual<string>("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual<string>("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual<string>("y.w.c", ywc.QualifiedName)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("QualifiedName")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.QualifiedName)
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicQualifiedName", theory.QualifiedName)
            | "block" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicQualifiedName.TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", block.QualifiedName)
            | "x" -> Assert.AreEqual<string>("x", x.QualifiedName)
            | "y" -> Assert.AreEqual<string>("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual<string>("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual<string>("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual<string>("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual<string>("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual<string>("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual<string>("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual<string>("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual<string>("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual<string>("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual<string>("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual<string>("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual<string>("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual<string>("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual<string>("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual<string>("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual<string>("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual<string>("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual<string>("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual<string>("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual<string>("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual<string>("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual<string>("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual<string>("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual<string>("y.w.c", ywc.QualifiedName)
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
    [<DataRow("base21", "and(x, y, z)")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not(x)")>]
    [<DataRow("base22", "xor(x, y, z)")>]
    [<DataRow("base23", "or(x, y, z)")>]
    [<DataRow("base24", "iif(x, y)")>]
    [<DataRow("base25", "impl(x, y)")>]
    [<DataRow("base26", "is(x, Nat)")>]
    [<DataRow("base27", "B()")>]
    [<DataRow("base28", "C(a, b, c, d)")>]
    [<DataRow("base29", "D(self, b, c)")>]
    [<DataRow("base30", "B(In(x))")>]
    [<DataRow("base31", "C(Test1(a),Test2(b, c, d))")>]
    [<DataRow("base32", "E(true, undef, false)")>]
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestPredicateQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base2" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base3" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base4" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base5" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base6" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base7" -> Assert.AreEqual<string>("TestPredicate.T1().bydef.Test()", base1.QualifiedName)
            | "base8" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base9" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base10" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base11" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base12" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base13" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base11a" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base12a" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base10b" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base11b" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base12b" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base13b" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base10c" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base11c" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base12c" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base13c" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base10d" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base11d" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base12d" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base13d" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base10e" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base11e" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base12e" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base13e" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base10f" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base11f" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base12f" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base13f" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base14" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base15" -> Assert.AreEqual<string>("TestPredicate.T1().-(x)", base1.QualifiedName)
            | "base15a" -> Assert.AreEqual<string>("TestPredicate.T1().'(x)", base1.QualifiedName)
            | "base15b" -> Assert.AreEqual<string>("TestPredicate.T1().'(-(x))", base1.QualifiedName)
            | "base16" -> Assert.AreEqual<string>("TestPredicate.T1().-(+(y, =(x, *(2, x))))", base1.QualifiedName)
            | "base17" -> Assert.AreEqual<string>("TestPredicate.T1().'(+(y, =('(x), *(2, x))))", base1.QualifiedName)
            | "base18" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base19" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base20" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base21" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base21a" -> Assert.AreEqual<string>("TestPredicate.T1().not x", base1.QualifiedName)
            | "base21b" -> Assert.AreEqual<string>("TestPredicate.T1().not x", base1.QualifiedName)
            | "base22" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base23" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base24" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base25" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base26" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base27" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base28" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base29" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base30" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base31" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
            | "base32" -> Assert.AreEqual<string>("TestPredicate.T1()." + varVal, base1.QualifiedName)
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
        let filename = "TestCallConstructorParentClassQualifiedName"
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
            | "base1" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.B()", base1.QualifiedName)
            | "base2" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.C(a, b, c, d)", base1.QualifiedName)
            | "base3" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.D(self, a, b)", base1.QualifiedName)
            | "base4" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.B(In(x))", base1.QualifiedName)
            | "base5" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.C(Test1(a), Test2(b, c, d))", base1.QualifiedName)
            | "base6" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.E(true, undef, false)", base1.QualifiedName)
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
        let filename = "TestDelegateQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().del.B()", base1.QualifiedName)
            | "base2" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().del.C(a, b, c, d)", base1.QualifiedName)
            | "base3" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().del.D(self, b, c)", base1.QualifiedName)
            | "base4" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().del.B(In(x))", base1.QualifiedName)
            | "base5" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().del.Test()", base1.QualifiedName)
            | "base6" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().del.C(Test1(a), Test2(b, c, d))", base1.QualifiedName)
            | "base7" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().del.E(true, undef, false)", base1.QualifiedName)
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
        let filename = "TestFixNotationQualifiedName"
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
            | "base1" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1()", base1.QualifiedName)
            | "base2" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1()", base1.QualifiedName)
            | "base3" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1()", base1.QualifiedName)
            | "base4" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1()", base1.QualifiedName)
            | "base5" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1", base1.QualifiedName)
            | "base5a" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1", base1.QualifiedName)
            | "base6" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1() -> obj", base1.QualifiedName)
            | "base7" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1() -> obj", base1.QualifiedName)
            | "base8" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1() -> obj", base1.QualifiedName)
            | "base9" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1() -> obj", base1.QualifiedName)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
