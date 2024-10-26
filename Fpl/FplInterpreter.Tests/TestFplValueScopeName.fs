namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
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
    [<DataRow("loc1")>]
    [<DataRow("loc2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("Name") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.Type(false))
            | "theory" -> Assert.AreEqual<string>("TestScopeBlocksName", theory.Type(false))
            | "inf1" -> Assert.AreEqual<string>("SomeInference1()", inf1.Type(false))
            | "inf2" -> Assert.AreEqual<string>("SomeInference2()", inf2.Type(false))
            | "axi1" -> Assert.AreEqual<string>("SomeAxiom1()", axi1.Type(false))
            | "axi2" -> Assert.AreEqual<string>("SomeAxiom2()", axi2.Type(false))
            | "pst1" -> Assert.AreEqual<string>("SomePostulate1()", pst1.Type(false))
            | "pst2" -> Assert.AreEqual<string>("SomePostulate2()", pst2.Type(false))
            | "thm1" -> Assert.AreEqual<string>("SomeTheorem1()", thm1.Type(false))
            | "thm2" -> Assert.AreEqual<string>("SomeTheorem2()", thm2.Type(false))
            | "pro1" -> Assert.AreEqual<string>("SomeProposition1()", pro1.Type(false))
            | "pro2" -> Assert.AreEqual<string>("SomeProposition2()", pro2.Type(false))
            | "lem1" -> Assert.AreEqual<string>("SomeLemma1()", lem1.Type(false))
            | "lem2" -> Assert.AreEqual<string>("SomeLemma2()", lem2.Type(false))
            | "cor1" -> Assert.AreEqual<string>("SomeLemma1$1()", cor1.Type(false))
            | "cor2" -> Assert.AreEqual<string>("SomeLemma2$1()", cor2.Type(false))
            | "con1" -> Assert.AreEqual<string>("SomeConjecture1()", con1.Type(false))
            | "con2" -> Assert.AreEqual<string>("SomeConjecture2()", con2.Type(false))
            | "cla1" -> Assert.AreEqual<string>("SomeClass1", cla1.Type(false))
            | "cla2" -> Assert.AreEqual<string>("SomeClass2", cla2.Type(false))
            | "pre1" -> Assert.AreEqual<string>("SomePredicate1()", pre1.Type(false))
            | "pre2" -> Assert.AreEqual<string>("SomePredicate2()", pre2.Type(false))
            | "fun1" -> Assert.AreEqual<string>("SomeFunctionalTerm1() -> obj", fun1.Type(false))
            | "fun2" -> Assert.AreEqual<string>("SomeFunctionalTerm2() -> obj", fun2.Type(false))
            | "prf1" -> Assert.AreEqual<string>("SomeTheorem1$1", prf1.Type(false))
            | "prf2" -> Assert.AreEqual<string>("SomeTheorem2$1", prf2.Type(false))
            | "loc1" -> Assert.AreEqual<string>("not(x)", loc1.Type(false))
            | "loc2" -> Assert.AreEqual<string>("Equal(x, y)", loc2.Type(false))
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
        let res = CommonFplValueTestCases.ScopeConstructors("Name") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.Type(false))
            | "theory" -> Assert.AreEqual<string>("TestScopeConstructorsName", theory.Type(false))
            | "block" -> Assert.AreEqual<string>("TestId", block.Type(false))
            | "t1" -> Assert.AreEqual<string>("TestId()", t1.Type(false))
            | "t2" -> Assert.AreEqual<string>("TestId(obj)", t2.Type(false))
            | "t3" -> Assert.AreEqual<string>("TestId(pred)", t3.Type(false))
            | "t4" -> Assert.AreEqual<string>("TestId(ind)", t4.Type(false))
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("Name") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<string>("", r.Type(false))
                | "theory" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesName", theory.Type(false))
                | "thm1" -> Assert.AreEqual<string>("TestTheorem1()", thm1.Type(false))
                | "proofThm1" -> Assert.AreEqual<string>("TestTheorem1$1", proofThm1.Type(false))
                | "lem1" -> Assert.AreEqual<string>("TestLemma1()", lem1.Type(false))
                | "proofLem1" -> Assert.AreEqual<string>("TestLemma1$1", proofLem1.Type(false))
                | "prp1" -> Assert.AreEqual<string>("TestProposition1()", prp1.Type(false))
                | "proofPrp1" -> Assert.AreEqual<string>("TestProposition1$1", proofPrp1.Type(false))
                | "cor1" -> Assert.AreEqual<string>("TestCorollary1$2()", cor1.Type(false))
                | "proofCor1" -> Assert.AreEqual<string>("TestCorollary1$2$1", proofCor1.Type(false))
                | "thm2" -> Assert.AreEqual<string>("TestTheorem2()", thm2.Type(false))
                | "corThm2" -> Assert.AreEqual<string>("TestTheorem2$1()", corThm2.Type(false))
                | "lem2" -> Assert.AreEqual<string>("TestLemma2()", lem2.Type(false))
                | "corLem2" -> Assert.AreEqual<string>("TestLemma2$1()", corLem2.Type(false))
                | "prp2" -> Assert.AreEqual<string>("TestProposition2()", prp2.Type(false))
                | "corPrp2" -> Assert.AreEqual<string>("TestProposition2$1()", corPrp2.Type(false))
                | "cor2" -> Assert.AreEqual<string>("TestCorollary2$2()", cor2.Type(false))
                | "corCor2" -> Assert.AreEqual<string>("TestCorollary2$2$1()", corCor2.Type(false))
                | "con1" -> Assert.AreEqual<string>("TestConjecture()", con1.Type(false))
                | "corCon1" -> Assert.AreEqual<string>("TestConjecture$1()", corCon1.Type(false))
                | "axi1" -> Assert.AreEqual<string>("TestAxiom()", axi1.Type(false))
                | "corAxi1"  -> Assert.AreEqual<string>("TestAxiom$1()", corAxi1.Type(false)) 
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
        let res = CommonFplValueTestCases.ScopeProperties("Name") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.Type(false))
            | "theory" -> Assert.AreEqual<string>("TestScopePropertiesName", theory.Type(false))
            | "block" -> Assert.AreEqual<string>("TestId()", block.Type(false))
            | "t1" -> Assert.AreEqual<string>("T1()", t1.Type(false))
            | "t2" -> Assert.AreEqual<string>("T2()", t2.Type(false))
            | "t3" -> Assert.AreEqual<string>("T3() -> obj", t3.Type(false))
            | "t4" -> Assert.AreEqual<string>("T4() -> obj", t4.Type(false))
            | "t5" -> Assert.AreEqual<string>("T5() -> ind", t5.Type(false))
            | "t6" -> Assert.AreEqual<string>("T6() -> ind", t6.Type(false))
            | "t7" -> Assert.AreEqual<string>("T7() -> pred", t7.Type(false))
            | "t8" -> Assert.AreEqual<string>("T8() -> pred", t8.Type(false))
            | "t9" -> Assert.AreEqual<string>("T9() -> tpl", t9.Type(false))
            | "t10" -> Assert.AreEqual<string>("T10() -> tpl", t10.Type(false))
            | "t11" -> Assert.AreEqual<string>("T11() -> Nat", t11.Type(false))
            | "t12" -> Assert.AreEqual<string>("T12() -> Nat", t12.Type(false))
            | "t13" -> Assert.AreEqual<string>("T13() -> func", t13.Type(false))
            | "t14" -> Assert.AreEqual<string>("T14() -> func", t14.Type(false))
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("Name")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.Type(false))
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInBlockName", theory.Type(false))
            | "block" -> Assert.AreEqual<string>("TestPredicate()", block.Type(false)); 
            | "x" -> Assert.AreEqual<string>("x", x.Type(false))
            | "y" -> Assert.AreEqual<string>("y", y.Type(false))
            | "s" -> Assert.AreEqual<string>("s", s.Type(false))
            | "xu" -> Assert.AreEqual<string>("u", xu.Type(false))
            | "xv" -> Assert.AreEqual<string>("v", xv.Type(false))
            | "xw" -> Assert.AreEqual<string>("w", xw.Type(false))
            | "yu" -> Assert.AreEqual<string>("u", yu.Type(false))
            | "yv" -> Assert.AreEqual<string>("v", yv.Type(false))
            | "yw" -> Assert.AreEqual<string>("w", yw.Type(false))
            | "xua" -> Assert.AreEqual<string>("a", xua.Type(false))
            | "xub" -> Assert.AreEqual<string>("b", xub.Type(false))
            | "xuc" -> Assert.AreEqual<string>("c", xuc.Type(false))
            | "xva" -> Assert.AreEqual<string>("a", xva.Type(false))
            | "xvb" -> Assert.AreEqual<string>("b", xvb.Type(false))
            | "xvc" -> Assert.AreEqual<string>("c", xvc.Type(false))
            | "xwa" -> Assert.AreEqual<string>("a", xwa.Type(false))
            | "xwb" -> Assert.AreEqual<string>("b", xwb.Type(false))
            | "xwc" -> Assert.AreEqual<string>("c", xwc.Type(false))
            | "yua" -> Assert.AreEqual<string>("a", yua.Type(false))
            | "yub" -> Assert.AreEqual<string>("b", yub.Type(false))
            | "yuc" -> Assert.AreEqual<string>("c", yuc.Type(false))
            | "yva" -> Assert.AreEqual<string>("a", yva.Type(false))
            | "yvb" -> Assert.AreEqual<string>("b", yvb.Type(false))
            | "yvc" -> Assert.AreEqual<string>("c", yvc.Type(false))
            | "ywa" -> Assert.AreEqual<string>("a", ywa.Type(false))
            | "ywb" -> Assert.AreEqual<string>("b", ywb.Type(false))
            | "ywc" -> Assert.AreEqual<string>("c", ywc.Type(false))
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("Name")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.Type(false))
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicName", theory.Type(false))
            | "block" -> Assert.AreEqual<string>("TestPredicate()", block.Type(false)); 
            | "x" -> Assert.AreEqual<string>("x", x.Type(false))
            | "y" -> Assert.AreEqual<string>("y", y.Type(false))
            | "xu" -> Assert.AreEqual<string>("u", xu.Type(false))
            | "xv" -> Assert.AreEqual<string>("v", xv.Type(false))
            | "xw" -> Assert.AreEqual<string>("w", xw.Type(false))
            | "yu" -> Assert.AreEqual<string>("u", yu.Type(false))
            | "yv" -> Assert.AreEqual<string>("v", yv.Type(false))
            | "yw" -> Assert.AreEqual<string>("w", yw.Type(false))
            | "xua" -> Assert.AreEqual<string>("a", xua.Type(false))
            | "xub" -> Assert.AreEqual<string>("b", xub.Type(false))
            | "xuc" -> Assert.AreEqual<string>("c", xuc.Type(false))
            | "xva" -> Assert.AreEqual<string>("a", xva.Type(false))
            | "xvb" -> Assert.AreEqual<string>("b", xvb.Type(false))
            | "xvc" -> Assert.AreEqual<string>("c", xvc.Type(false))
            | "xwa" -> Assert.AreEqual<string>("a", xwa.Type(false))
            | "xwb" -> Assert.AreEqual<string>("b", xwb.Type(false))
            | "xwc" -> Assert.AreEqual<string>("c", xwc.Type(false))
            | "yua" -> Assert.AreEqual<string>("a", yua.Type(false))
            | "yub" -> Assert.AreEqual<string>("b", yub.Type(false))
            | "yuc" -> Assert.AreEqual<string>("c", yuc.Type(false))
            | "yva" -> Assert.AreEqual<string>("a", yva.Type(false))
            | "yvb" -> Assert.AreEqual<string>("b", yvb.Type(false))
            | "yvc" -> Assert.AreEqual<string>("c", yvc.Type(false))
            | "ywa" -> Assert.AreEqual<string>("a", ywa.Type(false))
            | "ywb" -> Assert.AreEqual<string>("b", ywb.Type(false))
            | "ywc" -> Assert.AreEqual<string>("c", ywc.Type(false))
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("Name")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.Type(false))
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureName", theory.Type(false))
            | "block" -> Assert.AreEqual<string>("TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", block.Type(false)); 
            | "x" -> Assert.AreEqual<string>("x", x.Type(false))
            | "y" -> Assert.AreEqual<string>("y", y.Type(false))
            | "xu" -> Assert.AreEqual<string>("u", xu.Type(false))
            | "xv" -> Assert.AreEqual<string>("v", xv.Type(false))
            | "xw" -> Assert.AreEqual<string>("w", xw.Type(false))
            | "yu" -> Assert.AreEqual<string>("u", yu.Type(false))
            | "yv" -> Assert.AreEqual<string>("v", yv.Type(false))
            | "yw" -> Assert.AreEqual<string>("w", yw.Type(false))
            | "xua" -> Assert.AreEqual<string>("a", xua.Type(false))
            | "xub" -> Assert.AreEqual<string>("b", xub.Type(false))
            | "xuc" -> Assert.AreEqual<string>("c", xuc.Type(false))
            | "xva" -> Assert.AreEqual<string>("a", xva.Type(false))
            | "xvb" -> Assert.AreEqual<string>("b", xvb.Type(false))
            | "xvc" -> Assert.AreEqual<string>("c", xvc.Type(false))
            | "xwa" -> Assert.AreEqual<string>("a", xwa.Type(false))
            | "xwb" -> Assert.AreEqual<string>("b", xwb.Type(false))
            | "xwc" -> Assert.AreEqual<string>("c", xwc.Type(false))
            | "yua" -> Assert.AreEqual<string>("a", yua.Type(false))
            | "yub" -> Assert.AreEqual<string>("b", yub.Type(false))
            | "yuc" -> Assert.AreEqual<string>("c", yuc.Type(false))
            | "yva" -> Assert.AreEqual<string>("a", yva.Type(false))
            | "yvb" -> Assert.AreEqual<string>("b", yvb.Type(false))
            | "yvc" -> Assert.AreEqual<string>("c", yvc.Type(false))
            | "ywa" -> Assert.AreEqual<string>("a", ywa.Type(false))
            | "ywb" -> Assert.AreEqual<string>("b", ywb.Type(false))
            | "ywc" -> Assert.AreEqual<string>("c", ywc.Type(false))
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("Name")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.Type(false))
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicName", theory.Type(false))
            | "block" -> Assert.AreEqual<string>("TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", block.Type(false)); 
            | "x" -> Assert.AreEqual<string>("x", x.Type(false))
            | "y" -> Assert.AreEqual<string>("y", y.Type(false))
            | "xu" -> Assert.AreEqual<string>("u", xu.Type(false))
            | "xv" -> Assert.AreEqual<string>("v", xv.Type(false))
            | "xw" -> Assert.AreEqual<string>("w", xw.Type(false))
            | "yu" -> Assert.AreEqual<string>("u", yu.Type(false))
            | "yv" -> Assert.AreEqual<string>("v", yv.Type(false))
            | "yw" -> Assert.AreEqual<string>("w", yw.Type(false))
            | "xua" -> Assert.AreEqual<string>("a", xua.Type(false))
            | "xub" -> Assert.AreEqual<string>("b", xub.Type(false))
            | "xuc" -> Assert.AreEqual<string>("c", xuc.Type(false))
            | "xva" -> Assert.AreEqual<string>("a", xva.Type(false))
            | "xvb" -> Assert.AreEqual<string>("b", xvb.Type(false))
            | "xvc" -> Assert.AreEqual<string>("c", xvc.Type(false))
            | "xwa" -> Assert.AreEqual<string>("a", xwa.Type(false))
            | "xwb" -> Assert.AreEqual<string>("b", xwb.Type(false))
            | "xwc" -> Assert.AreEqual<string>("c", xwc.Type(false))
            | "yua" -> Assert.AreEqual<string>("a", yua.Type(false))
            | "yub" -> Assert.AreEqual<string>("b", yub.Type(false))
            | "yuc" -> Assert.AreEqual<string>("c", yuc.Type(false))
            | "yva" -> Assert.AreEqual<string>("a", yva.Type(false))
            | "yvb" -> Assert.AreEqual<string>("b", yvb.Type(false))
            | "yvc" -> Assert.AreEqual<string>("c", yvc.Type(false))
            | "ywa" -> Assert.AreEqual<string>("a", ywa.Type(false))
            | "ywb" -> Assert.AreEqual<string>("b", ywb.Type(false))
            | "ywc" -> Assert.AreEqual<string>("c", ywc.Type(false))
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
    [<DataRow("base18", "ex x is Range(a:T), y is C, z {and (a,b,c)}")>]
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
    [<DataRow("base33", "dec ~p: pred(c: obj); p(c)")>]
    [<DataRow("base34", "is(x, Set)")>]
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestPredicateName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base2" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base3" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base4" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base5" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base6" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base7" -> Assert.AreEqual<string>("bydef.Test()", base1.Type(false))
            | "base8" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base9" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base10" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base11" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base12" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base13" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base11a" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base12a" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base10b" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base11b" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base12b" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base13b" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base10c" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base11c" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base12c" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base13c" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base10d" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base11d" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base12d" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base13d" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base10e" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base11e" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base12e" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base13e" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base10f" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base11f" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base12f" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base13f" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base14" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base15" -> Assert.AreEqual<string>("-(x)", base1.Type(false))
            | "base15a" -> Assert.AreEqual<string>("'(x)", base1.Type(false))
            | "base15b" -> Assert.AreEqual<string>("'(-(x))", base1.Type(false))
            | "base16" -> Assert.AreEqual<string>("-(+(y, =(x, *(2, x))))", base1.Type(false))
            | "base17" -> Assert.AreEqual<string>("'(+(y, =('(x), *(2, x))))", base1.Type(false))
            | "base18" -> Assert.AreEqual<string>("ex(x(a), y, z)", base1.Type(false))
            | "base19" -> Assert.AreEqual<string>("exn$1(x)", base1.Type(false))
            | "base20" -> Assert.AreEqual<string>("all(x)", base1.Type(false))
            | "base21" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base21a" -> Assert.AreEqual<string>("not(x)", base1.Type(false))
            | "base21b" -> Assert.AreEqual<string>("not(x)", base1.Type(false))
            | "base22" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base23" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base24" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base25" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base26" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base27" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base28" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base29" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base30" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base31" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base32" -> Assert.AreEqual<string>(varVal, base1.Type(false))
            | "base33" -> Assert.AreEqual<string>("p(c)", base1.Type(false))
            | "base34" -> Assert.AreEqual<string>(varVal, base1.Type(false))
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
        let filename = "TestCallConstructorParentClassName"
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
            | "base1" -> Assert.AreEqual<string>("bas.B()", base1.Type(false))
            | "base2" -> Assert.AreEqual<string>("bas.C(a, b, c, d)", base1.Type(false))
            | "base3" -> Assert.AreEqual<string>("bas.D(self, a, b)", base1.Type(false))
            | "base4" -> Assert.AreEqual<string>("bas.B(In(x))", base1.Type(false))
            | "base5" -> Assert.AreEqual<string>("bas.C(Test1(a), Test2(b, c, d))", base1.Type(false))
            | "base6" -> Assert.AreEqual<string>("bas.E(true, undef, false)", base1.Type(false))
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
        let filename = "TestCallConstructorParentClassName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<string>(predName, pred.Type(false))
            | "base2" -> Assert.AreEqual<string>(predName, pred.Type(false))
            | "base3" -> Assert.AreEqual<string>(predName, pred.Type(false))
            | "base4" -> Assert.AreEqual<string>(predName, pred.Type(false))
            | "base5" -> Assert.AreEqual<string>(predName, pred.Type(false))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestTranslation(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestCallConstructorParentClassName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let trsl = pred.Scope["tex"]

            match var with
            | "base1" -> Assert.AreEqual<string>("tex", trsl.Type(false))
            | "base2" -> Assert.AreEqual<string>("tex", trsl.Type(false))
            | "base3" -> Assert.AreEqual<string>("tex", trsl.Type(false))
            | "base4" -> Assert.AreEqual<string>("tex", trsl.Type(false))
            | "base5" -> Assert.AreEqual<string>("tex", trsl.Type(false))
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
        let filename = "TestCallConstructorParentClassName"
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
            | "base1" -> Assert.AreEqual<string>("100.", arg.Type(false))
            | "base2" -> Assert.AreEqual<string>("100.", arg.Type(false))
            | "base3" -> Assert.AreEqual<string>("100.", arg.Type(false))
            | "base4" -> Assert.AreEqual<string>("100.", arg.Type(false))
            | "base5" -> Assert.AreEqual<string>("100.", arg.Type(false))
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
        let filename = "TestDelegateName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("del.B()", base1.Type(false))
            | "base2" -> Assert.AreEqual<string>("del.C(a, b, c, d)", base1.Type(false))
            | "base3" -> Assert.AreEqual<string>("del.D(self, b, c)", base1.Type(false))
            | "base4" -> Assert.AreEqual<string>("del.B(In(x))", base1.Type(false))
            | "base5" -> Assert.AreEqual<string>("del.Test()", base1.Type(false))
            | "base6" -> Assert.AreEqual<string>("del.C(Test1(a), Test2(b, c, d))", base1.Type(false))
            | "base7" -> Assert.AreEqual<string>("del.E(true, undef, false)", base1.Type(false))
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
        let filename = "TestFixNotationName"
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
            | "base1" -> Assert.AreEqual<string>("T1()", base1.Type(false))
            | "base2" -> Assert.AreEqual<string>("T1()", base1.Type(false))
            | "base3" -> Assert.AreEqual<string>("T1()", base1.Type(false))
            | "base4" -> Assert.AreEqual<string>("T1()", base1.Type(false))
            | "base5" -> Assert.AreEqual<string>("T1", base1.Type(false))
            | "base5a" -> Assert.AreEqual<string>("T1", base1.Type(false))
            | "base6" -> Assert.AreEqual<string>("T1() -> obj", base1.Type(false))
            | "base7" -> Assert.AreEqual<string>("T1() -> obj", base1.Type(false))
            | "base8" -> Assert.AreEqual<string>("T1() -> obj", base1.Type(false))
            | "base9" -> Assert.AreEqual<string>("T1() -> obj", base1.Type(false))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
