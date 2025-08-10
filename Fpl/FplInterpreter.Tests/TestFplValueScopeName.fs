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
        let res = CommonFplValueTestCases.ScopeBlocks("Name") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", getType SignatureType.Mixed r)
            | "theory" -> Assert.AreEqual<string>("TestScopeBlocksName", getType SignatureType.Mixed theory)
            | "inf1" -> Assert.AreEqual<string>("SomeInference1()", getType SignatureType.Mixed inf1)
            | "inf2" -> Assert.AreEqual<string>("SomeInference2()", getType SignatureType.Mixed inf2)
            | "axi1" -> Assert.AreEqual<string>("SomeAxiom1()", getType SignatureType.Mixed axi1)
            | "axi2" -> Assert.AreEqual<string>("SomeAxiom2()", getType SignatureType.Mixed axi2)
            | "pst1" -> Assert.AreEqual<string>("SomePostulate1()", getType SignatureType.Mixed pst1)
            | "pst2" -> Assert.AreEqual<string>("SomePostulate2()", getType SignatureType.Mixed pst2)
            | "thm1" -> Assert.AreEqual<string>("SomeTheorem1()", getType SignatureType.Mixed thm1)
            | "thm2" -> Assert.AreEqual<string>("SomeTheorem2()", getType SignatureType.Mixed thm2)
            | "pro1" -> Assert.AreEqual<string>("SomeProposition1()", getType SignatureType.Mixed pro1)
            | "pro2" -> Assert.AreEqual<string>("SomeProposition2()", getType SignatureType.Mixed pro2)
            | "lem1" -> Assert.AreEqual<string>("SomeLemma1()", getType SignatureType.Mixed lem1)
            | "lem2" -> Assert.AreEqual<string>("SomeLemma2()", getType SignatureType.Mixed lem2)
            | "cor1" -> Assert.AreEqual<string>("SomeLemma1$1()", getType SignatureType.Mixed cor1)
            | "cor2" -> Assert.AreEqual<string>("SomeLemma2$1()", getType SignatureType.Mixed cor2)
            | "con1" -> Assert.AreEqual<string>("SomeConjecture1()", getType SignatureType.Mixed con1)
            | "con2" -> Assert.AreEqual<string>("SomeConjecture2()", getType SignatureType.Mixed con2)
            | "cla1" -> Assert.AreEqual<string>("SomeClass1", getType SignatureType.Mixed cla1)
            | "cla2" -> Assert.AreEqual<string>("SomeClass2", getType SignatureType.Mixed cla2)
            | "pre1" -> Assert.AreEqual<string>("SomePredicate1()", getType SignatureType.Mixed pre1)
            | "pre2" -> Assert.AreEqual<string>("SomePredicate2()", getType SignatureType.Mixed pre2)
            | "fun1" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm1() -> obj", getType SignatureType.Mixed fun1)
                Assert.AreEqual<string>("SomeFunctionalTerm1() -> obj", getType SignatureType.Name fun1)
            | "fun2" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm2() -> obj", getType SignatureType.Mixed fun2)
                Assert.AreEqual<string>("SomeFunctionalTerm2() -> obj", getType SignatureType.Name fun2)
            | "fun3" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm3() -> obj", getType SignatureType.Mixed fun3)
                Assert.AreEqual<string>("SomeFunctionalTerm3() -> obj", getType SignatureType.Name fun3)
            | "fun4" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm4() -> obj(pred)", getType SignatureType.Mixed fun4)
                Assert.AreEqual<string>("SomeFunctionalTerm4() -> obj(c)", getType SignatureType.Name fun4)
            | "fun5" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm5() -> SomeClass1", getType SignatureType.Mixed fun5)
                Assert.AreEqual<string>("SomeFunctionalTerm5() -> SomeClass1", getType SignatureType.Name fun5)
            | "fun6" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm6() -> SomeClass1", getType SignatureType.Mixed fun6)
                Assert.AreEqual<string>("SomeFunctionalTerm6() -> SomeClass1", getType SignatureType.Name fun6)
            | "fun7" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm7() -> SomeClass1", getType SignatureType.Mixed fun7)
                Assert.AreEqual<string>("SomeFunctionalTerm7() -> SomeClass1", getType SignatureType.Name fun7)
            | "fun8" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm8() -> ind", getType SignatureType.Mixed fun8)
                Assert.AreEqual<string>("SomeFunctionalTerm8() -> ind", getType SignatureType.Name fun8)
            | "fun9" -> 
                Assert.AreEqual<string>("SomeFunctionalTerm9() -> ind", getType SignatureType.Mixed fun9)
                Assert.AreEqual<string>("SomeFunctionalTerm9() -> ind", getType SignatureType.Name fun9)
            | "prf1" -> Assert.AreEqual<string>("SomeTheorem1$1", getType SignatureType.Mixed prf1)
            | "prf2" -> Assert.AreEqual<string>("SomeTheorem2$1", getType SignatureType.Mixed prf2)
            | "loc1" -> Assert.AreEqual<string>("not(x)", getType SignatureType.Mixed loc1)
            | "loc2" -> Assert.AreEqual<string>("Equal(x, y)", getType SignatureType.Mixed loc2)
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
            | "r" -> Assert.AreEqual<string>("", getType SignatureType.Mixed r)
            | "theory" -> Assert.AreEqual<string>("TestScopeConstructorsName", getType SignatureType.Mixed theory)
            | "block" -> Assert.AreEqual<string>("TestId", getType SignatureType.Mixed block)
            | "t1" -> Assert.AreEqual<string>("TestId()", getType SignatureType.Mixed t1)
            | "t2" -> Assert.AreEqual<string>("TestId(obj)", getType SignatureType.Mixed t2)
            | "t3" -> Assert.AreEqual<string>("TestId(pred)", getType SignatureType.Mixed t3)
            | "t4" -> Assert.AreEqual<string>("TestId(ind)", getType SignatureType.Mixed t4)
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
                | "r" -> Assert.AreEqual<string>("", getType SignatureType.Mixed r)
                | "theory" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesName", getType SignatureType.Mixed theory)
                | "thm1" -> Assert.AreEqual<string>("TestTheorem1()", getType SignatureType.Mixed thm1)
                | "proofThm1" -> Assert.AreEqual<string>("TestTheorem1$1", getType SignatureType.Mixed proofThm1)
                | "lem1" -> Assert.AreEqual<string>("TestLemma1()", getType SignatureType.Mixed lem1)
                | "proofLem1" -> Assert.AreEqual<string>("TestLemma1$1", getType SignatureType.Mixed proofLem1)
                | "prp1" -> Assert.AreEqual<string>("TestProposition1()", getType SignatureType.Mixed prp1)
                | "proofPrp1" -> Assert.AreEqual<string>("TestProposition1$1", getType SignatureType.Mixed proofPrp1)
                | "cor1" -> Assert.AreEqual<string>("TestCorollary1$2()", getType SignatureType.Mixed cor1)
                | "proofCor1" -> Assert.AreEqual<string>("TestCorollary1$2$1", getType SignatureType.Mixed proofCor1)
                | "thm2" -> Assert.AreEqual<string>("TestTheorem2()", getType SignatureType.Mixed thm2)
                | "corThm2" -> Assert.AreEqual<string>("TestTheorem2$1()", getType SignatureType.Mixed corThm2)
                | "lem2" -> Assert.AreEqual<string>("TestLemma2()", getType SignatureType.Mixed lem2)
                | "corLem2" -> Assert.AreEqual<string>("TestLemma2$1()", getType SignatureType.Mixed corLem2)
                | "prp2" -> Assert.AreEqual<string>("TestProposition2()", getType SignatureType.Mixed prp2)
                | "corPrp2" -> Assert.AreEqual<string>("TestProposition2$1()", getType SignatureType.Mixed corPrp2)
                | "cor2" -> Assert.AreEqual<string>("TestCorollary2$2()", getType SignatureType.Mixed cor2)
                | "corCor2" -> Assert.AreEqual<string>("TestCorollary2$2$1()", getType SignatureType.Mixed corCor2)
                | "con1" -> Assert.AreEqual<string>("TestConjecture()", getType SignatureType.Mixed con1)
                | "corCon1" -> Assert.AreEqual<string>("TestConjecture$1()", getType SignatureType.Mixed corCon1)
                | "axi1" -> Assert.AreEqual<string>("TestAxiom()", getType SignatureType.Mixed axi1)
                | "corAxi1"  -> Assert.AreEqual<string>("TestAxiom$1()", getType SignatureType.Mixed corAxi1) 
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
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", getType SignatureType.Mixed r)
            | "theory" -> Assert.AreEqual<string>("TestScopePropertiesName", getType SignatureType.Mixed theory)
            | "block" -> Assert.AreEqual<string>("TestId()", getType SignatureType.Mixed block)
            | "t1" -> Assert.AreEqual<string>("T1()", getType SignatureType.Mixed t1)
            | "t2" -> Assert.AreEqual<string>("T2()", getType SignatureType.Mixed t2)
            | "t3" -> Assert.AreEqual<string>("T3() -> obj", getType SignatureType.Mixed t3)
            | "t4" -> Assert.AreEqual<string>("T4() -> obj", getType SignatureType.Mixed t4)
            | "t5" -> Assert.AreEqual<string>("T5() -> ind", getType SignatureType.Mixed t5)
            | "t6" -> Assert.AreEqual<string>("T6() -> ind", getType SignatureType.Mixed t6)
            | "t7" -> Assert.AreEqual<string>("T7() -> pred", getType SignatureType.Mixed t7)
            | "t8" -> Assert.AreEqual<string>("T8() -> pred", getType SignatureType.Mixed t8)
            | "t9" -> Assert.AreEqual<string>("T9() -> tpl", getType SignatureType.Mixed t9)
            | "t10" -> Assert.AreEqual<string>("T10() -> tpl", getType SignatureType.Mixed t10)
            | "t11" -> Assert.AreEqual<string>("T11() -> Nat", getType SignatureType.Mixed t11)
            | "t12" -> Assert.AreEqual<string>("T12() -> Nat", getType SignatureType.Mixed t12)
            | "t13" -> Assert.AreEqual<string>("T13() -> func", getType SignatureType.Mixed t13)
            | "t14" -> Assert.AreEqual<string>("T14() -> func", getType SignatureType.Mixed t14)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("Name")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", getType SignatureType.Mixed r)
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInBlockName", getType SignatureType.Mixed theory)
            | "block" -> Assert.AreEqual<string>("pred()", getType SignatureType.Type block); 
            | "x" -> Assert.AreEqual<string>("x(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", getType SignatureType.Mixed x)
            | "y" -> Assert.AreEqual<string>("y(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", getType SignatureType.Mixed y)
            | "s" -> Assert.AreEqual<string>("s", getType SignatureType.Mixed s)
            | "xu" -> Assert.AreEqual<string>("u(obj, obj, obj) -> obj", getType SignatureType.Mixed xu)
            | "xv" -> Assert.AreEqual<string>("v(obj, obj, obj) -> obj", getType SignatureType.Mixed xv)
            | "xw" -> Assert.AreEqual<string>("w(obj, obj, obj) -> obj", getType SignatureType.Mixed xw)
            | "yu" -> Assert.AreEqual<string>("u(obj, obj, obj) -> obj", getType SignatureType.Mixed yu)
            | "yv" -> Assert.AreEqual<string>("v(obj, obj, obj) -> obj", getType SignatureType.Mixed yv)
            | "yw" -> Assert.AreEqual<string>("w(obj, obj, obj) -> obj", getType SignatureType.Mixed yw)
            | "xua" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xua)
            | "xub" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xub)
            | "xuc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xuc)
            | "xva" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xva)
            | "xvb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xvb)
            | "xvc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xvc)
            | "xwa" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xwa)
            | "xwb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xwb)
            | "xwc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xwc)
            | "yua" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed yua)
            | "yub" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed yub)
            | "yuc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed yuc)
            | "yva" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed yva)
            | "yvb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed yvb)
            | "yvc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed yvc)
            | "ywa" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed ywa)
            | "ywb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed ywb)
            | "ywc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed ywc)
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
            | "r" -> Assert.AreEqual<string>("", getType SignatureType.Mixed r)
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicName", getType SignatureType.Mixed theory)
            | "block" -> Assert.AreEqual<string>("TestPredicate()", getType SignatureType.Mixed block); 
            | "x" -> Assert.AreEqual<string>("x(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", getType SignatureType.Mixed x)
            | "y" -> Assert.AreEqual<string>("y(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", getType SignatureType.Mixed y)
            | "xu" -> Assert.AreEqual<string>("u(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed xu)
            | "xv" -> Assert.AreEqual<string>("v(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed xv)
            | "xw" -> Assert.AreEqual<string>("w(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed xw)
            | "yu" -> Assert.AreEqual<string>("u(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed yu)
            | "yv" -> Assert.AreEqual<string>("v(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed yv)
            | "yw" -> Assert.AreEqual<string>("w(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed yw)
            | "xua" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xua)
            | "xub" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xub)
            | "xuc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xuc)
            | "xva" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xva)
            | "xvb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xvb)
            | "xvc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xvc)
            | "xwa" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xwa)
            | "xwb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xwb)
            | "xwc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xwc)
            | "yua" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed yua)
            | "yub" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed yub)
            | "yuc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed yuc)
            | "yva" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed yva)
            | "yvb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed yvb)
            | "yvc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed yvc)
            | "ywa" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed ywa)
            | "ywb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed ywb)
            | "ywc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed ywc)
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
            | "r" -> Assert.AreEqual<string>("", getType SignatureType.Mixed r)
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureName", getType SignatureType.Mixed theory)
            | "block" -> Assert.AreEqual<string>("TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", getType SignatureType.Mixed block); 
            | "x" -> Assert.AreEqual<string>("x(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", getType SignatureType.Mixed x)
            | "y" -> Assert.AreEqual<string>("y(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", getType SignatureType.Mixed y)
            | "xu" -> Assert.AreEqual<string>("u(obj, obj, obj) -> obj", getType SignatureType.Mixed xu)
            | "xv" -> Assert.AreEqual<string>("v(obj, obj, obj) -> obj", getType SignatureType.Mixed xv)
            | "xw" -> Assert.AreEqual<string>("w(obj, obj, obj) -> obj", getType SignatureType.Mixed xw)
            | "yu" -> Assert.AreEqual<string>("u(obj, obj, obj) -> obj", getType SignatureType.Mixed yu)
            | "yv" -> Assert.AreEqual<string>("v(obj, obj, obj) -> obj", getType SignatureType.Mixed yv)
            | "yw" -> Assert.AreEqual<string>("w(obj, obj, obj) -> obj", getType SignatureType.Mixed yw)
            | "xua" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xua)
            | "xub" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xub)
            | "xuc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xuc)
            | "xva" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xva)
            | "xvb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xvb)
            | "xvc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xvc)
            | "xwa" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xwa)
            | "xwb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xwb)
            | "xwc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xwc)
            | "yua" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed yua)
            | "yub" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed yub)
            | "yuc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed yuc)
            | "yva" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed yva)
            | "yvb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed yvb)
            | "yvc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed yvc)
            | "ywa" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed ywa)
            | "ywb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed ywb)
            | "ywc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed ywc)
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
            | "r" -> Assert.AreEqual<string>("", getType SignatureType.Mixed r)
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicName", getType SignatureType.Mixed theory)
            | "block" -> Assert.AreEqual<string>("TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", getType SignatureType.Mixed block); 
            | "x" -> Assert.AreEqual<string>("x(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", getType SignatureType.Mixed x)
            | "y" -> Assert.AreEqual<string>("y(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", getType SignatureType.Mixed y)
            | "xu" -> Assert.AreEqual<string>("u(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed xu)
            | "xv" -> Assert.AreEqual<string>("v(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed xv)
            | "xw" -> Assert.AreEqual<string>("w(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed xw)
            | "yu" -> Assert.AreEqual<string>("u(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed yu)
            | "yv" -> Assert.AreEqual<string>("v(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed yv)
            | "yw" -> Assert.AreEqual<string>("w(*obj, *obj, *obj) -> obj", getType SignatureType.Mixed yw)
            | "xua" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xua)
            | "xub" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xub)
            | "xuc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xuc)
            | "xva" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xva)
            | "xvb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xvb)
            | "xvc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xvc)
            | "xwa" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed xwa)
            | "xwb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed xwb)
            | "xwc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed xwc)
            | "yua" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed yua)
            | "yub" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed yub)
            | "yuc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed yuc)
            | "yva" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed yva)
            | "yvb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed yvb)
            | "yvc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed yvc)
            | "ywa" -> Assert.AreEqual<string>("a", getType SignatureType.Mixed ywa)
            | "ywb" -> Assert.AreEqual<string>("b", getType SignatureType.Mixed ywb)
            | "ywc" -> Assert.AreEqual<string>("c", getType SignatureType.Mixed ywc)
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
    [<DataRow("base12", "parent")>]
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
    [<DataRow("base18", "ex x:Range(a:T), y:C, z:ind {and (a,abc(b,c))}")>]
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
            | "base1" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base2" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base3" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base4" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base5" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base6" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base7" -> Assert.AreEqual<string>("bydef.Test()", getType SignatureType.Name base1)
            | "base8" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base9" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base10" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base11" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base12" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base13" -> Assert.AreEqual<string>("1", getType SignatureType.Name base1)
            | "base11a" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base12a" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base10b" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base11b" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base12b" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base13b" -> Assert.AreEqual<string>("1()", getType SignatureType.Name base1)
            | "base10c" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base11c" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base12c" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base13c" -> Assert.AreEqual<string>("1(x, y)", getType SignatureType.Name base1)
            | "base10d" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base11d" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base12d" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base13d" -> Assert.AreEqual<string>("1[x.y]", getType SignatureType.Name base1)
            | "base10e" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base11e" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base12e" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base13e" -> Assert.AreEqual<string>("1(x, y).T[a, b]", getType SignatureType.Name base1)
            | "base10f" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base11f" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base12f" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base13f" -> Assert.AreEqual<string>("1[x.y].T(a, b)", getType SignatureType.Name base1)
            | "base14" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base15" -> Assert.AreEqual<string>("-(x)", getType SignatureType.Name base1)
            | "base15a" -> Assert.AreEqual<string>("'(x)", getType SignatureType.Name base1)
            | "base15b" -> Assert.AreEqual<string>("'(-(x))", getType SignatureType.Name base1)
            | "base16" -> Assert.AreEqual<string>("-(*(=(+(y, x), 2), x))", getType SignatureType.Name base1)
            | "base17" -> Assert.AreEqual<string>("'(*(=(+(y, '(x)), 2), x))", getType SignatureType.Name base1)
            | "base18" -> Assert.AreEqual<string>("ex(x(a), y, z)", getType SignatureType.Name base1)
            | "base19" -> Assert.AreEqual<string>("exn$1(x)", getType SignatureType.Name base1)
            | "base20" -> Assert.AreEqual<string>("all(x)", getType SignatureType.Name base1)
            | "base21" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base21a" -> Assert.AreEqual<string>("not(x)", getType SignatureType.Name base1)
            | "base21b" -> Assert.AreEqual<string>("not(x)", getType SignatureType.Name base1)
            | "base22" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base23" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base24" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base25" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base26" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base27" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base28" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base29" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base30" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base31" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base32" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
            | "base33" -> Assert.AreEqual<string>("p(c)", getType SignatureType.Name base1)
            | "base34" -> Assert.AreEqual<string>(varVal, getType SignatureType.Name base1)
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
            let stmt = ctor.ArgList[0]
            let base1 = stmt.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("B()", getType SignatureType.Mixed base1)
            | "base2" -> Assert.AreEqual<string>("C(T1, func, ind, pred)", getType SignatureType.Mixed base1)
            | "base3" -> Assert.AreEqual<string>("D(parent, T1, func)", getType SignatureType.Mixed base1)
            | "base4" -> Assert.AreEqual<string>("B(In(undef))", getType SignatureType.Mixed base1)
            | "base5" -> Assert.AreEqual<string>("C(Test1(T1), Test2(func, ind, pred))", getType SignatureType.Mixed base1)
            | "base6" -> Assert.AreEqual<string>("E(pred, undef, pred)", getType SignatureType.Mixed base1)
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
        let filename = "TestLocalizationName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<string>(predName, getType SignatureType.Mixed pred)
            | "base2" -> Assert.AreEqual<string>(predName, getType SignatureType.Mixed pred)
            | "base3" -> Assert.AreEqual<string>(predName, getType SignatureType.Mixed pred)
            | "base4" -> Assert.AreEqual<string>(predName, getType SignatureType.Mixed pred)
            | "base5" -> Assert.AreEqual<string>(predName, getType SignatureType.Mixed pred)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", "true", """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x " \Leftrightarrow " y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p " \wedge " q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq " y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestTranslation(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
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
            | "base0" -> Assert.AreEqual<string>(@"1", getType SignatureType.Mixed trsl)
            | "base1" -> Assert.AreEqual<string>(@"x \Leftrightarrow y", getType SignatureType.Mixed trsl)
            | "base2" -> Assert.AreEqual<string>(@"\neg(x)", getType SignatureType.Mixed trsl)
            | "base3" -> Assert.AreEqual<string>(@"p \wedge q", getType SignatureType.Mixed trsl)
            | "base4" -> Assert.AreEqual<string>(@"x=y", getType SignatureType.Mixed trsl)
            | "base5" -> Assert.AreEqual<string>(@"x\neq y", getType SignatureType.Mixed trsl)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("base0", "true", """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLanguage(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
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
            | "base0" -> Assert.AreEqual<string>("tex", getType SignatureType.Mixed lang)
            | "base1" -> Assert.AreEqual<string>("tex", getType SignatureType.Mixed lang)
            | "base2" -> Assert.AreEqual<string>("tex", getType SignatureType.Mixed lang)
            | "base3" -> Assert.AreEqual<string>("tex", getType SignatureType.Mixed lang)
            | "base4" -> Assert.AreEqual<string>("tex", getType SignatureType.Mixed lang)
            | "base5" -> Assert.AreEqual<string>("tex", getType SignatureType.Mixed lang)
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
        let filename = "TestArgumentName"
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
            | "base1" -> Assert.AreEqual<string>("100.", getType SignatureType.Mixed arg)
            | "base2" -> Assert.AreEqual<string>("100.", getType SignatureType.Mixed arg)
            | "base3" -> Assert.AreEqual<string>("100.", getType SignatureType.Mixed arg)
            | "base4" -> Assert.AreEqual<string>("100.", getType SignatureType.Mixed arg)
            | "base5" -> Assert.AreEqual<string>("100.", getType SignatureType.Mixed arg)
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
            | "base1" -> Assert.AreEqual<string>("del.B()", getType SignatureType.Mixed base1)
            | "base2" -> Assert.AreEqual<string>("del.C(T1, ind, ind, undef)", getType SignatureType.Mixed base1)
            | "base3" -> Assert.AreEqual<string>("del.D(parent, ind, ind)", getType SignatureType.Mixed base1)
            | "base4" -> Assert.AreEqual<string>("del.B(In(undef))", getType SignatureType.Mixed base1)
            | "base5" -> Assert.AreEqual<string>("del.Test()", getType SignatureType.Mixed base1)
            | "base6" -> Assert.AreEqual<string>("del.C(Test1(T1), Test2(ind, ind, undef))", getType SignatureType.Mixed base1)
            | "base7" -> Assert.AreEqual<string>("del.E(pred, undef, pred)", getType SignatureType.Mixed base1)
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
            | "base1" -> Assert.AreEqual<string>("T1()", getType SignatureType.Mixed base1)
            | "base2" -> Assert.AreEqual<string>("T1()", getType SignatureType.Mixed base1)
            | "base3" -> Assert.AreEqual<string>("T1()", getType SignatureType.Mixed base1)
            | "base4" -> Assert.AreEqual<string>("T1()", getType SignatureType.Mixed base1)
            | "base5" -> Assert.AreEqual<string>("T1", getType SignatureType.Mixed base1)
            | "base5a" -> Assert.AreEqual<string>("T1", getType SignatureType.Mixed base1)
            | "base6" -> Assert.AreEqual<string>("T1() -> obj", getType SignatureType.Mixed base1)
            | "base7" -> Assert.AreEqual<string>("T1() -> obj", getType SignatureType.Mixed base1)
            | "base8" -> Assert.AreEqual<string>("T1() -> obj", getType SignatureType.Mixed base1)
            | "base9" -> Assert.AreEqual<string>("T1() -> obj", getType SignatureType.Mixed base1)
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
            | "base1" -> Assert.AreEqual<string>("obj", getType SignatureType.Mixed mapping)
            | "base2" -> Assert.AreEqual<string>(constInd, getType SignatureType.Mixed mapping)
            | "base3" -> Assert.AreEqual<string>("func", getType SignatureType.Mixed mapping)
            | "base4" -> Assert.AreEqual<string>("pred", getType SignatureType.Mixed mapping)
            | "base5" -> Assert.AreEqual<string>("A", getType SignatureType.Mixed mapping)
            | "base6" -> Assert.AreEqual<string>("obj(ind)", getType SignatureType.Mixed mapping)
            | "base7" -> Assert.AreEqual<string>("pred(*obj)", getType SignatureType.Mixed mapping)
            | "base8" -> Assert.AreEqual<string>("func(*pred(obj)) -> pred(ind)", getType SignatureType.Mixed mapping)
            | "base9" -> Assert.AreEqual<string>("pred(+func(A) -> A)", getType SignatureType.Mixed mapping)
            | "base10" -> Assert.AreEqual<string>("A(func(A) -> A)", getType SignatureType.Mixed mapping)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
