namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplPrimitives
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers


[<TestClass>]
type TestFplValueScopeName() =

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
    member this.TestScopeBlocksName(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("Name") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Mixed))
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeBlocksName", theory.Type(SignatureType.Mixed))
            | "inf1" -> Assert.AreEqual<string>("SomeInference1", inf1.Type(SignatureType.Mixed))
            | "inf2" -> Assert.AreEqual<string>("SomeInference2", inf2.Type(SignatureType.Mixed))
            | "axi1" -> Assert.AreEqual<string>("SomeAxiom1", axi1.Type(SignatureType.Mixed))
            | "axi2" -> Assert.AreEqual<string>("SomeAxiom2", axi2.Type(SignatureType.Mixed))
            | "pst1" -> Assert.AreEqual<string>("SomePostulate1", pst1.Type(SignatureType.Mixed))
            | "pst2" -> Assert.AreEqual<string>("SomePostulate2", pst2.Type(SignatureType.Mixed))
            | "thm1" -> Assert.AreEqual<string>("SomeTheorem1", thm1.Type(SignatureType.Mixed))
            | "thm2" -> Assert.AreEqual<string>("SomeTheorem2", thm2.Type(SignatureType.Mixed))
            | "pro1" -> Assert.AreEqual<string>("SomeProposition1", pro1.Type(SignatureType.Mixed))
            | "pro2" -> Assert.AreEqual<string>("SomeProposition2", pro2.Type(SignatureType.Mixed))
            | "lem1" -> Assert.AreEqual<string>("SomeLemma1", lem1.Type(SignatureType.Mixed))
            | "lem2" -> Assert.AreEqual<string>("SomeLemma2", lem2.Type(SignatureType.Mixed))
            | "cor1" -> Assert.AreEqual<string>("SomeLemma1$1", cor1.Type(SignatureType.Mixed))
            | "cor2" -> Assert.AreEqual<string>("SomeLemma2$1", cor2.Type(SignatureType.Mixed))
            | "con1" -> Assert.AreEqual<string>("SomeConjecture1", con1.Type(SignatureType.Mixed))
            | "con2" -> Assert.AreEqual<string>("SomeConjecture2", con2.Type(SignatureType.Mixed))
            | "cla1" -> Assert.AreEqual<string>("SomeClass1", cla1.Type(SignatureType.Mixed))
            | "cla2" -> Assert.AreEqual<string>("SomeClass2", cla2.Type(SignatureType.Mixed))
            | "pre1" -> Assert.AreEqual<string>("SomePredicate1()", pre1.Type(SignatureType.Mixed))
            | "pre2" -> Assert.AreEqual<string>("SomePredicate2()", pre2.Type(SignatureType.Mixed))
            | "fun1" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm1() -> obj", fun1.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm1() -> obj", fun1.Type(SignatureType.Name))
            | "fun2" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm2() -> obj", fun2.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm2() -> obj", fun2.Type(SignatureType.Name))
            | "fun3" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm3() -> obj", fun3.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm3() -> obj", fun3.Type(SignatureType.Name))
            | "fun4" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm4() -> tpl", fun4.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm4() -> tpl", fun4.Type(SignatureType.Name))
            | "fun5" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm5() -> SomeClass1", fun5.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm5() -> SomeClass1", fun5.Type(SignatureType.Name))
            | "fun6" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm6() -> SomeClass1", fun6.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm6() -> SomeClass1", fun6.Type(SignatureType.Name))
            | "fun7" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm7() -> SomeClass1", fun7.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm7() -> SomeClass1", fun7.Type(SignatureType.Name))
            | "fun8" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm8() -> ind", fun8.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm8() -> ind", fun8.Type(SignatureType.Name))
            | "fun9" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm9() -> ind", fun9.Type(SignatureType.Mixed))
                Assert.AreEqual<string>("SomeFunctionalTerm9() -> ind", fun9.Type(SignatureType.Name))
            | "prf1" -> Assert.AreEqual<string>("SomeTheorem1$1", prf1.Type(SignatureType.Mixed))
            | "prf2" -> Assert.AreEqual<string>("SomeTheorem2$1", prf2.Type(SignatureType.Mixed))
            | "loc1" -> Assert.AreEqual<string>("not(x)", loc1.Type(SignatureType.Mixed))
            | "loc2" -> Assert.AreEqual<string>("Equal(x, y)", loc2.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Mixed))
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeConstructorsName", theory.Type(SignatureType.Mixed))
            | "block" -> Assert.AreEqual<string>("TestId", block.Type(SignatureType.Mixed))
            | "t1" -> Assert.AreEqual<string>("TestId()", t1.Type(SignatureType.Mixed))
            | "t2" -> Assert.AreEqual<string>("TestId(obj)", t2.Type(SignatureType.Mixed))
            | "t3" -> Assert.AreEqual<string>("TestId(pred)", t3.Type(SignatureType.Mixed))
            | "t4" -> Assert.AreEqual<string>("TestId(ind)", t4.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
                | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Mixed))
                | PrimTheoryL -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesName", theory.Type(SignatureType.Mixed))
                | "thm1" -> Assert.AreEqual<string>("TestTheorem1()", thm1.Type(SignatureType.Mixed))
                | "proofThm1" -> Assert.AreEqual<string>("TestTheorem1$1", proofThm1.Type(SignatureType.Mixed))
                | "lem1" -> Assert.AreEqual<string>("TestLemma1()", lem1.Type(SignatureType.Mixed))
                | "proofLem1" -> Assert.AreEqual<string>("TestLemma1$1", proofLem1.Type(SignatureType.Mixed))
                | "prp1" -> Assert.AreEqual<string>("TestProposition1()", prp1.Type(SignatureType.Mixed))
                | "proofPrp1" -> Assert.AreEqual<string>("TestProposition1$1", proofPrp1.Type(SignatureType.Mixed))
                | "cor1" -> Assert.AreEqual<string>("TestCorollary1$2()", cor1.Type(SignatureType.Mixed))
                | "proofCor1" -> Assert.AreEqual<string>("TestCorollary1$2$1", proofCor1.Type(SignatureType.Mixed))
                | "thm2" -> Assert.AreEqual<string>("TestTheorem2()", thm2.Type(SignatureType.Mixed))
                | "corThm2" -> Assert.AreEqual<string>("TestTheorem2$1()", corThm2.Type(SignatureType.Mixed))
                | "lem2" -> Assert.AreEqual<string>("TestLemma2()", lem2.Type(SignatureType.Mixed))
                | "corLem2" -> Assert.AreEqual<string>("TestLemma2$1()", corLem2.Type(SignatureType.Mixed))
                | "prp2" -> Assert.AreEqual<string>("TestProposition2()", prp2.Type(SignatureType.Mixed))
                | "corPrp2" -> Assert.AreEqual<string>("TestProposition2$1()", corPrp2.Type(SignatureType.Mixed))
                | "cor2" -> Assert.AreEqual<string>("TestCorollary2$2()", cor2.Type(SignatureType.Mixed))
                | "corCor2" -> Assert.AreEqual<string>("TestCorollary2$2$1()", corCor2.Type(SignatureType.Mixed))
                | "con1" -> Assert.AreEqual<string>("TestConjecture()", con1.Type(SignatureType.Mixed))
                | "corCon1" -> Assert.AreEqual<string>("TestConjecture$1()", corCon1.Type(SignatureType.Mixed))
                | "axi1" -> Assert.AreEqual<string>("TestAxiom()", axi1.Type(SignatureType.Mixed))
                | "corAxi1"  -> Assert.AreEqual<string>("TestAxiom$1()", corAxi1.Type(SignatureType.Mixed)) 
                | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Mixed))
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopePropertiesName", theory.Type(SignatureType.Mixed))
            | "block" -> Assert.AreEqual<string>("TestId()", block.Type(SignatureType.Mixed))
            | "t1" -> Assert.AreEqual<string>("T1()", t1.Type(SignatureType.Mixed))
            | "t2" -> Assert.AreEqual<string>("T2()", t2.Type(SignatureType.Mixed))
            | "t3" -> Assert.AreEqual<string>("T3() -> obj", t3.Type(SignatureType.Mixed))
            | "t4" -> Assert.AreEqual<string>("T4() -> obj", t4.Type(SignatureType.Mixed))
            | "t5" -> Assert.AreEqual<string>("T5() -> ind", t5.Type(SignatureType.Mixed))
            | "t6" -> Assert.AreEqual<string>("T6() -> ind", t6.Type(SignatureType.Mixed))
            | "t7" -> Assert.AreEqual<string>("T7() -> pred", t7.Type(SignatureType.Mixed))
            | "t8" -> Assert.AreEqual<string>("T8() -> pred", t8.Type(SignatureType.Mixed))
            | "t9" -> Assert.AreEqual<string>("T9() -> tpl", t9.Type(SignatureType.Mixed))
            | "t10" -> Assert.AreEqual<string>("T10() -> tpl", t10.Type(SignatureType.Mixed))
            | "t11" -> Assert.AreEqual<string>("T11() -> Nat", t11.Type(SignatureType.Mixed))
            | "t12" -> Assert.AreEqual<string>("T12() -> Nat", t12.Type(SignatureType.Mixed))
            | "t13" -> Assert.AreEqual<string>("T13() -> func", t13.Type(SignatureType.Mixed))
            | "t14" -> Assert.AreEqual<string>("T14() -> func", t14.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("Name")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Mixed))
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInBlockName", theory.Type(SignatureType.Mixed))
            | "block" -> Assert.AreEqual<string>("pred()", block.Type(SignatureType.Type)); 
            | "x" -> Assert.AreEqual<string>("x(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", x.Type(SignatureType.Mixed))
            | "y" -> Assert.AreEqual<string>("y(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", y.Type(SignatureType.Mixed))
            | "s" -> Assert.AreEqual<string>("s", s.Type(SignatureType.Mixed))
            | "xu" -> Assert.AreEqual<string>("u(obj, obj, obj) -> obj", xu.Type(SignatureType.Mixed))
            | "xv" -> Assert.AreEqual<string>("v(obj, obj, obj) -> obj", xv.Type(SignatureType.Mixed))
            | "xw" -> Assert.AreEqual<string>("w(obj, obj, obj) -> obj", xw.Type(SignatureType.Mixed))
            | "yu" -> Assert.AreEqual<string>("u(obj, obj, obj) -> obj", yu.Type(SignatureType.Mixed))
            | "yv" -> Assert.AreEqual<string>("v(obj, obj, obj) -> obj", yv.Type(SignatureType.Mixed))
            | "yw" -> Assert.AreEqual<string>("w(obj, obj, obj) -> obj", yw.Type(SignatureType.Mixed))
            | "xua" -> Assert.AreEqual<string>("a", xua.Type(SignatureType.Mixed))
            | "xub" -> Assert.AreEqual<string>("b", xub.Type(SignatureType.Mixed))
            | "xuc" -> Assert.AreEqual<string>("c", xuc.Type(SignatureType.Mixed))
            | "xva" -> Assert.AreEqual<string>("a", xva.Type(SignatureType.Mixed))
            | "xvb" -> Assert.AreEqual<string>("b", xvb.Type(SignatureType.Mixed))
            | "xvc" -> Assert.AreEqual<string>("c", xvc.Type(SignatureType.Mixed))
            | "xwa" -> Assert.AreEqual<string>("a", xwa.Type(SignatureType.Mixed))
            | "xwb" -> Assert.AreEqual<string>("b", xwb.Type(SignatureType.Mixed))
            | "xwc" -> Assert.AreEqual<string>("c", xwc.Type(SignatureType.Mixed))
            | "yua" -> Assert.AreEqual<string>("a", yua.Type(SignatureType.Mixed))
            | "yub" -> Assert.AreEqual<string>("b", yub.Type(SignatureType.Mixed))
            | "yuc" -> Assert.AreEqual<string>("c", yuc.Type(SignatureType.Mixed))
            | "yva" -> Assert.AreEqual<string>("a", yva.Type(SignatureType.Mixed))
            | "yvb" -> Assert.AreEqual<string>("b", yvb.Type(SignatureType.Mixed))
            | "yvc" -> Assert.AreEqual<string>("c", yvc.Type(SignatureType.Mixed))
            | "ywa" -> Assert.AreEqual<string>("a", ywa.Type(SignatureType.Mixed))
            | "ywb" -> Assert.AreEqual<string>("b", ywb.Type(SignatureType.Mixed))
            | "ywc" -> Assert.AreEqual<string>("c", ywc.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Mixed))
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicName", theory.Type(SignatureType.Mixed))
            | "block" -> Assert.AreEqual<string>("TestPredicate()", block.Type(SignatureType.Mixed)); 
            | "x" -> Assert.AreEqual<string>("x(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", x.Type(SignatureType.Mixed))
            | "y" -> Assert.AreEqual<string>("y(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", y.Type(SignatureType.Mixed))
            | "xu" -> Assert.AreEqual<string>("u(*obj, *obj, *obj) -> obj", xu.Type(SignatureType.Mixed))
            | "xv" -> Assert.AreEqual<string>("v(*obj, *obj, *obj) -> obj", xv.Type(SignatureType.Mixed))
            | "xw" -> Assert.AreEqual<string>("w(*obj, *obj, *obj) -> obj", xw.Type(SignatureType.Mixed))
            | "yu" -> Assert.AreEqual<string>("u(*obj, *obj, *obj) -> obj", yu.Type(SignatureType.Mixed))
            | "yv" -> Assert.AreEqual<string>("v(*obj, *obj, *obj) -> obj", yv.Type(SignatureType.Mixed))
            | "yw" -> Assert.AreEqual<string>("w(*obj, *obj, *obj) -> obj", yw.Type(SignatureType.Mixed))
            | "xua" -> Assert.AreEqual<string>("a", xua.Type(SignatureType.Mixed))
            | "xub" -> Assert.AreEqual<string>("b", xub.Type(SignatureType.Mixed))
            | "xuc" -> Assert.AreEqual<string>("c", xuc.Type(SignatureType.Mixed))
            | "xva" -> Assert.AreEqual<string>("a", xva.Type(SignatureType.Mixed))
            | "xvb" -> Assert.AreEqual<string>("b", xvb.Type(SignatureType.Mixed))
            | "xvc" -> Assert.AreEqual<string>("c", xvc.Type(SignatureType.Mixed))
            | "xwa" -> Assert.AreEqual<string>("a", xwa.Type(SignatureType.Mixed))
            | "xwb" -> Assert.AreEqual<string>("b", xwb.Type(SignatureType.Mixed))
            | "xwc" -> Assert.AreEqual<string>("c", xwc.Type(SignatureType.Mixed))
            | "yua" -> Assert.AreEqual<string>("a", yua.Type(SignatureType.Mixed))
            | "yub" -> Assert.AreEqual<string>("b", yub.Type(SignatureType.Mixed))
            | "yuc" -> Assert.AreEqual<string>("c", yuc.Type(SignatureType.Mixed))
            | "yva" -> Assert.AreEqual<string>("a", yva.Type(SignatureType.Mixed))
            | "yvb" -> Assert.AreEqual<string>("b", yvb.Type(SignatureType.Mixed))
            | "yvc" -> Assert.AreEqual<string>("c", yvc.Type(SignatureType.Mixed))
            | "ywa" -> Assert.AreEqual<string>("a", ywa.Type(SignatureType.Mixed))
            | "ywb" -> Assert.AreEqual<string>("b", ywb.Type(SignatureType.Mixed))
            | "ywc" -> Assert.AreEqual<string>("c", ywc.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Mixed))
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInSignatureName", theory.Type(SignatureType.Mixed))
            | "block" -> Assert.AreEqual<string>("TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", block.Type(SignatureType.Mixed)); 
            | "x" -> Assert.AreEqual<string>("x(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", x.Type(SignatureType.Mixed))
            | "y" -> Assert.AreEqual<string>("y(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", y.Type(SignatureType.Mixed))
            | "xu" -> Assert.AreEqual<string>("u(obj, obj, obj) -> obj", xu.Type(SignatureType.Mixed))
            | "xv" -> Assert.AreEqual<string>("v(obj, obj, obj) -> obj", xv.Type(SignatureType.Mixed))
            | "xw" -> Assert.AreEqual<string>("w(obj, obj, obj) -> obj", xw.Type(SignatureType.Mixed))
            | "yu" -> Assert.AreEqual<string>("u(obj, obj, obj) -> obj", yu.Type(SignatureType.Mixed))
            | "yv" -> Assert.AreEqual<string>("v(obj, obj, obj) -> obj", yv.Type(SignatureType.Mixed))
            | "yw" -> Assert.AreEqual<string>("w(obj, obj, obj) -> obj", yw.Type(SignatureType.Mixed))
            | "xua" -> Assert.AreEqual<string>("a", xua.Type(SignatureType.Mixed))
            | "xub" -> Assert.AreEqual<string>("b", xub.Type(SignatureType.Mixed))
            | "xuc" -> Assert.AreEqual<string>("c", xuc.Type(SignatureType.Mixed))
            | "xva" -> Assert.AreEqual<string>("a", xva.Type(SignatureType.Mixed))
            | "xvb" -> Assert.AreEqual<string>("b", xvb.Type(SignatureType.Mixed))
            | "xvc" -> Assert.AreEqual<string>("c", xvc.Type(SignatureType.Mixed))
            | "xwa" -> Assert.AreEqual<string>("a", xwa.Type(SignatureType.Mixed))
            | "xwb" -> Assert.AreEqual<string>("b", xwb.Type(SignatureType.Mixed))
            | "xwc" -> Assert.AreEqual<string>("c", xwc.Type(SignatureType.Mixed))
            | "yua" -> Assert.AreEqual<string>("a", yua.Type(SignatureType.Mixed))
            | "yub" -> Assert.AreEqual<string>("b", yub.Type(SignatureType.Mixed))
            | "yuc" -> Assert.AreEqual<string>("c", yuc.Type(SignatureType.Mixed))
            | "yva" -> Assert.AreEqual<string>("a", yva.Type(SignatureType.Mixed))
            | "yvb" -> Assert.AreEqual<string>("b", yvb.Type(SignatureType.Mixed))
            | "yvc" -> Assert.AreEqual<string>("c", yvc.Type(SignatureType.Mixed))
            | "ywa" -> Assert.AreEqual<string>("a", ywa.Type(SignatureType.Mixed))
            | "ywb" -> Assert.AreEqual<string>("b", ywb.Type(SignatureType.Mixed))
            | "ywc" -> Assert.AreEqual<string>("c", ywc.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Mixed))
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicName", theory.Type(SignatureType.Mixed))
            | "block" -> Assert.AreEqual<string>("TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", block.Type(SignatureType.Mixed)); 
            | "x" -> Assert.AreEqual<string>("x(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", x.Type(SignatureType.Mixed))
            | "y" -> Assert.AreEqual<string>("y(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", y.Type(SignatureType.Mixed))
            | "xu" -> Assert.AreEqual<string>("u(*obj, *obj, *obj) -> obj", xu.Type(SignatureType.Mixed))
            | "xv" -> Assert.AreEqual<string>("v(*obj, *obj, *obj) -> obj", xv.Type(SignatureType.Mixed))
            | "xw" -> Assert.AreEqual<string>("w(*obj, *obj, *obj) -> obj", xw.Type(SignatureType.Mixed))
            | "yu" -> Assert.AreEqual<string>("u(*obj, *obj, *obj) -> obj", yu.Type(SignatureType.Mixed))
            | "yv" -> Assert.AreEqual<string>("v(*obj, *obj, *obj) -> obj", yv.Type(SignatureType.Mixed))
            | "yw" -> Assert.AreEqual<string>("w(*obj, *obj, *obj) -> obj", yw.Type(SignatureType.Mixed))
            | "xua" -> Assert.AreEqual<string>("a", xua.Type(SignatureType.Mixed))
            | "xub" -> Assert.AreEqual<string>("b", xub.Type(SignatureType.Mixed))
            | "xuc" -> Assert.AreEqual<string>("c", xuc.Type(SignatureType.Mixed))
            | "xva" -> Assert.AreEqual<string>("a", xva.Type(SignatureType.Mixed))
            | "xvb" -> Assert.AreEqual<string>("b", xvb.Type(SignatureType.Mixed))
            | "xvc" -> Assert.AreEqual<string>("c", xvc.Type(SignatureType.Mixed))
            | "xwa" -> Assert.AreEqual<string>("a", xwa.Type(SignatureType.Mixed))
            | "xwb" -> Assert.AreEqual<string>("b", xwb.Type(SignatureType.Mixed))
            | "xwc" -> Assert.AreEqual<string>("c", xwc.Type(SignatureType.Mixed))
            | "yua" -> Assert.AreEqual<string>("a", yua.Type(SignatureType.Mixed))
            | "yub" -> Assert.AreEqual<string>("b", yub.Type(SignatureType.Mixed))
            | "yuc" -> Assert.AreEqual<string>("c", yuc.Type(SignatureType.Mixed))
            | "yva" -> Assert.AreEqual<string>("a", yva.Type(SignatureType.Mixed))
            | "yvb" -> Assert.AreEqual<string>("b", yvb.Type(SignatureType.Mixed))
            | "yvc" -> Assert.AreEqual<string>("c", yvc.Type(SignatureType.Mixed))
            | "ywa" -> Assert.AreEqual<string>("a", ywa.Type(SignatureType.Mixed))
            | "ywb" -> Assert.AreEqual<string>("b", ywb.Type(SignatureType.Mixed))
            | "ywc" -> Assert.AreEqual<string>("c", ywc.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", LiteralTrue)>]
    [<DataRow("base2", LiteralFalse)>]
    [<DataRow("base3", LiteralUndef)>]
    [<DataRow("base4", "-1")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "$1")>]
    [<DataRow("base7", "Test$1(x)")>] 
    [<DataRow("base8", "Test$1")>]
    [<DataRow("base9", "Test$1()")>]
    [<DataRow("base10", "Test")>]
    [<DataRow("base11", "v")>]
    [<DataRow("base12", LiteralParent)>]
    [<DataRow("base13", "@1")>]
    [<DataRow("base11a", "v.x")>]
    [<DataRow("base12a", "parent.x")>]
    [<DataRow("base10b", "Test()")>]
    [<DataRow("base11b", "v()")>]
    [<DataRow("base12b", "parent()")>]
    [<DataRow("base13b", "@1()")>]
    [<DataRow("base10c", "Test(x, y)")>]
    [<DataRow("base11c", "v(x, y)")>]
    [<DataRow("base12c", "parent(x, y)")>]
    [<DataRow("base13c", "@1(x, y)")>]
    [<DataRow("base10d", "Test[x, y]")>]
    [<DataRow("base11d", "v[x, y]")>]
    [<DataRow("base12d", "parent[x, y]")>]
    [<DataRow("base13d", "@1[x.y]")>]
    [<DataRow("base10e", "Test(x, y).parent[a, b]")>]
    [<DataRow("base11e", "v(x, y).x[a, b]")>]
    [<DataRow("base12e", "parent(x, y).3[a, b]")>]
    [<DataRow("base13e", "@1(x, y).T[a, b]")>]
    [<DataRow("base10f", "Test[x, y].x(a, b)")>]
    [<DataRow("base11f", "v[x, y].x(a, b)")>]
    [<DataRow("base12f", "parent[x, y].parent(a, b)")>]
    [<DataRow("base13f", "@1[x.y].T(a, b)")>]
    [<DataRow("base14", "∅")>]
    [<DataRow("base15", "-x")>]
    [<DataRow("base15a", "x'")>]
    [<DataRow("base15b", "-x'")>]
    [<DataRow("base16", "-(y + x = @2 * x)")>]
    [<DataRow("base17", "(y + x' = @2 * x)'")>]
    [<DataRow("base18", "ex x:pred(a:obj,b:T), y:C, z:ind {and (a,abc(b,c))}")>]
    [<DataRow("base19", "exn$1 x:obj {all y:N {true}}")>]
    [<DataRow("base20", "all x:obj {not x}")>]
    [<DataRow("base21", "and(x, abc(y, z))")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not (x)")>]
    [<DataRow("base22", "xor(xor(x, y), z)")>]
    [<DataRow("base23", "or(x, or(y, z))")>]
    [<DataRow("base24", "iif(x, y)")>]
    [<DataRow("base25", "impl(x, y)")>]
    [<DataRow("base26", "is(x, Nat)")>]
    [<DataRow("base27", "B()")>]
    [<DataRow("base28", "C(a, b, c, d)")>]
    [<DataRow("base29", "D(parent, b, c)")>]
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
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base2" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base3" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base4" -> Assert.AreEqual<string>("-1", base1.Type(SignatureType.Name))
            | "base5" -> Assert.AreEqual<string>("Test()", base1.Type(SignatureType.Name))
            | "base6" -> Assert.AreEqual<string>("$1", base1.Type(SignatureType.Name))
            | "base7" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base8" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base9" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base10" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base11" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base12" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base13" -> Assert.AreEqual<string>("1", base1.Type(SignatureType.Name))
            | "base11a" -> Assert.AreEqual<string>("v", base1.Type(SignatureType.Name))
            | "base12a" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base10b" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base11b" -> Assert.AreEqual<string>("v", base1.Type(SignatureType.Name))
            | "base12b" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base13b" -> Assert.AreEqual<string>("1()", base1.Type(SignatureType.Name))
            | "base10c" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base11c" -> Assert.AreEqual<string>("v", base1.Type(SignatureType.Name))
            | "base12c" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base13c" -> Assert.AreEqual<string>("1(x, y)", base1.Type(SignatureType.Name))
            | "base10d" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base11d" -> Assert.AreEqual<string>("v", base1.Type(SignatureType.Name))
            | "base12d" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base13d" -> Assert.AreEqual<string>("1[x]", base1.Type(SignatureType.Name))
            | "base10e" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base11e" -> Assert.AreEqual<string>("v", base1.Type(SignatureType.Name))
            | "base12e" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base13e" -> Assert.AreEqual<string>("1(x, y).T[a, b]", base1.Type(SignatureType.Name))
            | "base10f" -> Assert.AreEqual<string>("Test[x, y].x", base1.Type(SignatureType.Name))
            | "base11f" -> Assert.AreEqual<string>("v", base1.Type(SignatureType.Name))
            | "base12f" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base13f" -> Assert.AreEqual<string>("1[x].T(a, b)", base1.Type(SignatureType.Name))
            | "base14" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base15" -> Assert.AreEqual<string>("-(x)", base1.Type(SignatureType.Name))
            | "base15a" -> Assert.AreEqual<string>("'(x)", base1.Type(SignatureType.Name))
            | "base15b" -> Assert.AreEqual<string>("'(-(x))", base1.Type(SignatureType.Name))
            | "base16" -> Assert.AreEqual<string>("-(*(=(+(y, x), 2), x))", base1.Type(SignatureType.Name))
            | "base17" -> Assert.AreEqual<string>("'(*(=(+(y, '(x)), 2), x))", base1.Type(SignatureType.Name))
            | "base18" -> Assert.AreEqual<string>("ex(x(a, b), y, z)", base1.Type(SignatureType.Name))
            | "base19" -> Assert.AreEqual<string>("exn$1(x)", base1.Type(SignatureType.Name))
            | "base20" -> Assert.AreEqual<string>("all(x)", base1.Type(SignatureType.Name))
            | "base21" -> Assert.AreEqual<string>("and(x, abc)", base1.Type(SignatureType.Name))
            | "base21a" -> Assert.AreEqual<string>("not(x)", base1.Type(SignatureType.Name))
            | "base21b" -> Assert.AreEqual<string>("not(x)", base1.Type(SignatureType.Name))
            | "base22" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base23" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base24" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base25" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base26" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base27" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base28" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base29" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base30" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base31" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base32" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | "base33" -> Assert.AreEqual<string>("p(c)", base1.Type(SignatureType.Name))
            | "base34" -> Assert.AreEqual<string>(varVal, base1.Type(SignatureType.Name))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "base.B()")>]
    [<DataRow("base2", "base.C(a, b, c, d)")>]
    [<DataRow("base3", "base.D(parent, a, b)")>]
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
            let stmt = ctor.ArgList[0]
            let base1 = stmt.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("B()", base1.Type(SignatureType.Mixed))
            | "base2" -> Assert.AreEqual<string>("C(T1, func, ind, pred)", base1.Type(SignatureType.Mixed))
            | "base3" -> Assert.AreEqual<string>("D(parent, T1, func)", base1.Type(SignatureType.Mixed))
            | "base4" -> Assert.AreEqual<string>("B(In(undef))", base1.Type(SignatureType.Mixed))
            | "base5" -> Assert.AreEqual<string>("C(Test1(T1), Test2(func, ind, pred))", base1.Type(SignatureType.Mixed))
            | "base6" -> Assert.AreEqual<string>("E(pred, undef, pred)", base1.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", LiteralTrue, LiteralTrue, """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(undef, undef)", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(undef)", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(undef, undef)", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(undef, undef)", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(undef, undef)", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLocalizationName(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLocalizationName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base0" -> Assert.AreEqual<string>(predDecl, pred.Type(SignatureType.Mixed))
            | "base1" -> Assert.AreEqual<string>(predDecl, pred.Type(SignatureType.Mixed))
            | "base2" -> Assert.AreEqual<string>(predDecl, pred.Type(SignatureType.Mixed))
            | "base3" -> Assert.AreEqual<string>(predDecl, pred.Type(SignatureType.Mixed))
            | "base4" -> Assert.AreEqual<string>(predDecl, pred.Type(SignatureType.Mixed))
            | "base5" -> Assert.AreEqual<string>(predDecl, pred.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", LiteralTrue, LiteralTrue, """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(undef, undef)", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(undef)", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(undef, undef)", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(undef, undef)", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(undef, undef)", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestTranslationName(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestTranslationName"
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
            | "base0" -> Assert.AreEqual<string>(@"1", trsl.Type(SignatureType.Mixed))
            | "base1" -> Assert.AreEqual<string>(@"x \Leftrightarrow y", trsl.Type(SignatureType.Mixed))
            | "base2" -> Assert.AreEqual<string>(@"\neg( x )", trsl.Type(SignatureType.Mixed))
            | "base3" -> Assert.AreEqual<string>(@"p \wedge q", trsl.Type(SignatureType.Mixed))
            | "base4" -> Assert.AreEqual<string>(@"x = y", trsl.Type(SignatureType.Mixed))
            | "base5" -> Assert.AreEqual<string>(@"x \neq y", trsl.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("base0", LiteralTrue, LiteralTrue, """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(undef, undef)", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(undef)", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(undef, undef)", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(undef, undef)", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(undef, undef)", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLanguageNameMixed(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLanguageName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Mixed))
            | "base1" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Mixed))
            | "base2" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Mixed))
            | "base3" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Mixed))
            | "base4" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Mixed))
            | "base5" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """100. |- trivial""")>]
    [<DataRow("base2", """100. ExistsByExample, 1 |- false""")>]
    [<DataRow("base3", """100. T1 |- assume not somePremise """)>]
    [<DataRow("base4", """100. 2, 3, 5 |- iif (a,b)""")>]
    [<DataRow("base5", """100. |- revoke 3""")>]
    [<TestMethod>]
    member this.TestArgumentName(var, argExpression) =
        ad.Clear()
        let fplCode = sprintf """proof T$1 { %s };""" argExpression
        let filename = "TestArgumentName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let proof = theory.Scope["T$1"]
            let arg = proof.Scope["100"]
            match var with
            | "base1" -> Assert.AreEqual<string>("100", arg.Type(SignatureType.Mixed))
            | "base2" -> Assert.AreEqual<string>("100", arg.Type(SignatureType.Mixed))
            | "base3" -> Assert.AreEqual<string>("100", arg.Type(SignatureType.Mixed))
            | "base4" -> Assert.AreEqual<string>("100", arg.Type(SignatureType.Mixed))
            | "base5" -> Assert.AreEqual<string>("100", arg.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "del.B()")>]
    [<DataRow("base2", "del.C(a,b,c,d)")>]
    [<DataRow("base3", "del.D(parent,b,c)")>]
    [<DataRow("base4", "del.B(In(x))")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "del.C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base7", "del.E(true, undef, false)")>] 
    [<TestMethod>]
    member this.TestDelegate(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "def pred T1() { dec ~a:T1 ~b:ind ~c:ind;  %s };" varVal
        let filename = "TestDelegateName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("B()", base1.Type(SignatureType.Mixed))
            | "base2" -> Assert.AreEqual<string>("C(T1, ind, ind, undef)", base1.Type(SignatureType.Mixed))
            | "base3" -> Assert.AreEqual<string>("D(parent, ind, ind)", base1.Type(SignatureType.Mixed))
            | "base4" -> Assert.AreEqual<string>("B(In(undef))", base1.Type(SignatureType.Mixed))
            | "base5" -> Assert.AreEqual<string>("Test()", base1.Type(SignatureType.Mixed))
            | "base6" -> Assert.AreEqual<string>("C(Test1(T1), Test2(ind, ind, undef))", base1.Type(SignatureType.Mixed))
            | "base7" -> Assert.AreEqual<string>("E(pred, undef, pred)", base1.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """def pred T1() {intr};""")>]
    [<DataRow("base2", """def pred T1 () infix ">" -1 {intr};""")>]
    [<DataRow("base3", """def pred T1 () postfix "'" {intr};""")>]
    [<DataRow("base4", """def pred T1 () prefix "-" {intr};""")>]
    [<DataRow("base5", """def cl T1 :obj symbol "∅" {intr};""")>]
    [<DataRow("base5a", """def cl T1:obj {intr};""")>]
    [<DataRow("base6", """def func T1()->obj {intr};""")>]
    [<DataRow("base7", """def func T1 ()->obj infix ">" -1 {intr};""")>]
    [<DataRow("base8", """def func T1  ()->obj postfix "'"{intr};""")>]
    [<DataRow("base9", """def func T1 ()->obj prefix "-" {intr};""")>]
    [<TestMethod>]
    member this.TestFixNotationName(var, varVal) =
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
                if varVal.Contains LiteralCl then 
                    theory.Scope["T1"]
                elif varVal.Contains LiteralFunc then 
                    theory.Scope["T1() -> obj"]
                else 
                    theory.Scope["T1()"]

            match var with
            | "base1" -> Assert.AreEqual<string>("T1()", base1.Type(SignatureType.Mixed))
            | "base2" -> Assert.AreEqual<string>("T1()", base1.Type(SignatureType.Mixed))
            | "base3" -> Assert.AreEqual<string>("T1()", base1.Type(SignatureType.Mixed))
            | "base4" -> Assert.AreEqual<string>("T1()", base1.Type(SignatureType.Mixed))
            | "base5" -> Assert.AreEqual<string>("T1", base1.Type(SignatureType.Mixed))
            | "base5a" -> Assert.AreEqual<string>("T1", base1.Type(SignatureType.Mixed))
            | "base6" -> Assert.AreEqual<string>("T1() -> obj", base1.Type(SignatureType.Mixed))
            | "base7" -> Assert.AreEqual<string>("T1() -> obj", base1.Type(SignatureType.Mixed))
            | "base8" -> Assert.AreEqual<string>("T1() -> obj", base1.Type(SignatureType.Mixed))
            | "base9" -> Assert.AreEqual<string>("T1() -> obj", base1.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("base1", """def func T()->obj {intr};""")>]
    [<DataRow("base2", """def func T()->ind {intr};""")>]
    [<DataRow("base3", """def func T()->func {intr};""")>]
    [<DataRow("base4", """def func T()->pred {intr};""")>]
    [<DataRow("base5", """def cl A:obj {intr} def func T()->A {intr};""")>]
    [<DataRow("base6", """def func T()->pred(z:ind) {intr};""")>]
    [<DataRow("base7", """def func T()->pred(z:*obj) {intr};""")>]
    [<DataRow("base8", """def func T()->func(p:*pred(x:obj))->pred(x:ind) {intr};""")>]
    [<DataRow("base9", """def func T()->pred(f:+func(x:A)->A) {intr};""")>]
    [<DataRow("base10", """def cl A:obj {intr} def func T()->pred(f:func(x:A)->A) {intr};""")>]
    [<TestMethod>]
    member this.TestMapping(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestMappingName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<string>(LiteralObj, mapping.Type(SignatureType.Mixed))
            | "base2" -> Assert.AreEqual<string>(LiteralInd, mapping.Type(SignatureType.Mixed))
            | "base3" -> Assert.AreEqual<string>(LiteralFunc, mapping.Type(SignatureType.Mixed))
            | "base4" -> Assert.AreEqual<string>(LiteralPred, mapping.Type(SignatureType.Mixed))
            | "base5" -> Assert.AreEqual<string>("A", mapping.Type(SignatureType.Mixed))
            | "base6" -> Assert.AreEqual<string>("pred(ind)", mapping.Type(SignatureType.Mixed))
            | "base7" -> Assert.AreEqual<string>("pred(*obj)", mapping.Type(SignatureType.Mixed))
            | "base8" -> Assert.AreEqual<string>("func(*pred(obj)) -> pred(ind)", mapping.Type(SignatureType.Mixed))
            | "base9" -> Assert.AreEqual<string>("pred(+func(A) -> A)", mapping.Type(SignatureType.Mixed))
            | "base10" -> Assert.AreEqual<string>("pred(func(A) -> A)", mapping.Type(SignatureType.Mixed))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
