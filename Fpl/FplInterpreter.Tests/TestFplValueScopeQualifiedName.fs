namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValueScopeQualifiedName() =

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
    [<DataRow("prf1")>]
    [<DataRow("prf2")>]
    [<DataRow("loc1")>]
    [<DataRow("loc2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("QualifiedName") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", qualifiedName r)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName", qualifiedName theory)
            | "inf1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeInference1()", qualifiedName inf1)
            | "inf2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeInference2()", qualifiedName inf2)
            | "axi1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeAxiom1()", qualifiedName axi1)
            | "axi2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeAxiom2()", qualifiedName axi2)
            | "pst1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomePostulate1()", qualifiedName pst1)
            | "pst2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomePostulate2()", qualifiedName pst2)
            | "thm1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeTheorem1()", qualifiedName thm1)
            | "thm2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeTheorem2()", qualifiedName thm2)
            | "pro1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeProposition1()", qualifiedName pro1)
            | "pro2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeProposition2()", qualifiedName pro2)
            | "lem1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeLemma1()", qualifiedName lem1)
            | "lem2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeLemma2()", qualifiedName lem2)
            | "cor1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeLemma1().SomeLemma1$1()", qualifiedName cor1)
            | "cor2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeLemma2().SomeLemma2$1()", qualifiedName cor2)
            | "con1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeConjecture1()", qualifiedName con1)
            | "con2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeConjecture2()", qualifiedName con2)
            | "cla1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeClass1", qualifiedName cla1)
            | "cla2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeClass2", qualifiedName cla2)
            | "pre1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomePredicate1()", qualifiedName pre1)
            | "pre2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomePredicate2()", qualifiedName pre2)
            | "fun1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm1() -> obj", qualifiedName fun1)
            | "fun2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm2() -> obj", qualifiedName fun2)
            | "fun3" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm3() -> obj", qualifiedName fun3)
            | "fun4" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm4() -> obj(pred)", qualifiedName fun4)
            | "fun5" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm5() -> SomeClass1", qualifiedName fun5)
            | "fun6" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm6() -> SomeClass1", qualifiedName fun6)
            | "fun7" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm7() -> SomeClass1", qualifiedName fun7)
            | "fun8" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm8() -> ind", qualifiedName fun8)
            | "fun9" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeFunctionalTerm9() -> ind", qualifiedName fun9)
            | "prf1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeTheorem1().SomeTheorem1$1", qualifiedName prf1)
            | "prf2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.SomeTheorem2().SomeTheorem2$1", qualifiedName prf2)
            | "loc1" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.not(x)", qualifiedName loc1)
            | "loc2" -> Assert.AreEqual<string>("TestScopeBlocksQualifiedName.Equal(x, y)", qualifiedName loc2)
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
        let res = CommonFplValueTestCases.ScopeConstructors("QualifiedName") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", qualifiedName r)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName", qualifiedName theory)
            | "block" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId", qualifiedName block)
            | "t1" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId.TestId()", qualifiedName t1)
            | "t2" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId.TestId(obj)", qualifiedName t2)
            | "t3" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId.TestId(pred)", qualifiedName t3)
            | "t4" -> Assert.AreEqual<string>("TestScopeConstructorsQualifiedName.TestId.TestId(ind)", qualifiedName t4)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("QualifiedName") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<string>("", qualifiedName r)
                | PrimTheoryL -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName", qualifiedName theory)
                | "thm1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestTheorem1()", qualifiedName thm1)
                | "proofThm1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestTheorem1().TestTheorem1$1", qualifiedName proofThm1)
                | "lem1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestLemma1()", qualifiedName lem1)
                | "proofLem1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestLemma1().TestLemma1$1", qualifiedName proofLem1)
                | "prp1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestProposition1()", qualifiedName prp1)
                | "proofPrp1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestProposition1().TestProposition1$1", qualifiedName proofPrp1)
                | "cor1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestCorollary1$2()", qualifiedName cor1)
                | "proofCor1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestCorollary1$2().TestCorollary1$2$1", qualifiedName proofCor1)
                | "thm2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestTheorem2()", qualifiedName thm2)
                | "corThm2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestTheorem2().TestTheorem2$1()", qualifiedName corThm2)
                | "lem2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestLemma2()", qualifiedName lem2)
                | "corLem2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestLemma2().TestLemma2$1()", qualifiedName corLem2)
                | "prp2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestProposition2()", qualifiedName prp2)
                | "corPrp2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestProposition2().TestProposition2$1()", qualifiedName corPrp2)
                | "cor2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestCorollary2$2()", qualifiedName cor2)
                | "corCor2" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestCorollary2$2().TestCorollary2$2$1()", qualifiedName corCor2)
                | "con1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestConjecture()", qualifiedName con1)
                | "corCon1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestConjecture().TestConjecture$1()", qualifiedName corCon1)
                | "axi1" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestAxiom()", qualifiedName axi1)
                | "corAxi1"  -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesQualifiedName.TestAxiom().TestAxiom$1()", qualifiedName corAxi1) 
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
        let res = CommonFplValueTestCases.ScopeProperties("QualifiedName") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", qualifiedName r)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName", qualifiedName theory)
            | "block" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId()", qualifiedName block)
            | "t1" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T1()", qualifiedName t1)
            | "t2" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T2()", qualifiedName t2)
            | "t3" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T3() -> obj", qualifiedName t3)
            | "t4" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T4() -> obj", qualifiedName t4)
            | "t5" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T5() -> ind", qualifiedName t5)
            | "t6" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T6() -> ind", qualifiedName t6)
            | "t7" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T7() -> pred", qualifiedName t7)
            | "t8" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T8() -> pred", qualifiedName t8)
            | "t9" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T9() -> tpl", qualifiedName t9)
            | "t10" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T10() -> tpl", qualifiedName t10)
            | "t11" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T11() -> Nat", qualifiedName t11)
            | "t12" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T12() -> Nat", qualifiedName t12)
            | "t13" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T13() -> func", qualifiedName t13)
            | "t14" -> Assert.AreEqual<string>("TestScopePropertiesQualifiedName.TestId().T14() -> func", qualifiedName t14)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("QualifiedName")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", qualifiedName r)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInBlockQualifiedName", qualifiedName theory)
            | "block" -> Assert.AreEqual<string>("TestScopeVariablesInBlockQualifiedName.TestPredicate()", qualifiedName block)
            | "x" -> Assert.AreEqual<string>("x", qualifiedName x)
            | "y" -> Assert.AreEqual<string>("y", qualifiedName y)
            | "s" -> Assert.AreEqual<string>("s", qualifiedName s)
            | "xu" -> Assert.AreEqual<string>("x.u", qualifiedName xu)
            | "xv" -> Assert.AreEqual<string>("x.v", qualifiedName xv)
            | "xw" -> Assert.AreEqual<string>("x.w", qualifiedName xw)
            | "yu" -> Assert.AreEqual<string>("y.u", qualifiedName yu)
            | "yv" -> Assert.AreEqual<string>("y.v", qualifiedName yv)
            | "yw" -> Assert.AreEqual<string>("y.w", qualifiedName yw)
            | "xua" -> Assert.AreEqual<string>("x.u.a", qualifiedName xua)
            | "xub" -> Assert.AreEqual<string>("x.u.b", qualifiedName xub)
            | "xuc" -> Assert.AreEqual<string>("x.u.c", qualifiedName xuc)
            | "xva" -> Assert.AreEqual<string>("x.v.a", qualifiedName xva)
            | "xvb" -> Assert.AreEqual<string>("x.v.b", qualifiedName xvb)
            | "xvc" -> Assert.AreEqual<string>("x.v.c", qualifiedName xvc)
            | "xwa" -> Assert.AreEqual<string>("x.w.a", qualifiedName xwa)
            | "xwb" -> Assert.AreEqual<string>("x.w.b", qualifiedName xwb)
            | "xwc" -> Assert.AreEqual<string>("x.w.c", qualifiedName xwc)
            | "yua" -> Assert.AreEqual<string>("y.u.a", qualifiedName yua)
            | "yub" -> Assert.AreEqual<string>("y.u.b", qualifiedName yub)
            | "yuc" -> Assert.AreEqual<string>("y.u.c", qualifiedName yuc)
            | "yva" -> Assert.AreEqual<string>("y.v.a", qualifiedName yva)
            | "yvb" -> Assert.AreEqual<string>("y.v.b", qualifiedName yvb)
            | "yvc" -> Assert.AreEqual<string>("y.v.c", qualifiedName yvc)
            | "ywa" -> Assert.AreEqual<string>("y.w.a", qualifiedName ywa)
            | "ywb" -> Assert.AreEqual<string>("y.w.b", qualifiedName ywb)
            | "ywc" -> Assert.AreEqual<string>("y.w.c", qualifiedName ywc)
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("QualifiedName")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", qualifiedName r)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicQualifiedName", qualifiedName theory)
            | "block" -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicQualifiedName.TestPredicate()", qualifiedName block)
            | "x" -> Assert.AreEqual<string>("x", qualifiedName x)
            | "y" -> Assert.AreEqual<string>("y", qualifiedName y)
            | "xu" -> Assert.AreEqual<string>("x.u", qualifiedName xu)
            | "xv" -> Assert.AreEqual<string>("x.v", qualifiedName xv)
            | "xw" -> Assert.AreEqual<string>("x.w", qualifiedName xw)
            | "yu" -> Assert.AreEqual<string>("y.u", qualifiedName yu)
            | "yv" -> Assert.AreEqual<string>("y.v", qualifiedName yv)
            | "yw" -> Assert.AreEqual<string>("y.w", qualifiedName yw)
            | "xua" -> Assert.AreEqual<string>("x.u.a", qualifiedName xua)
            | "xub" -> Assert.AreEqual<string>("x.u.b", qualifiedName xub)
            | "xuc" -> Assert.AreEqual<string>("x.u.c", qualifiedName xuc)
            | "xva" -> Assert.AreEqual<string>("x.v.a", qualifiedName xva)
            | "xvb" -> Assert.AreEqual<string>("x.v.b", qualifiedName xvb)
            | "xvc" -> Assert.AreEqual<string>("x.v.c", qualifiedName xvc)
            | "xwa" -> Assert.AreEqual<string>("x.w.a", qualifiedName xwa)
            | "xwb" -> Assert.AreEqual<string>("x.w.b", qualifiedName xwb)
            | "xwc" -> Assert.AreEqual<string>("x.w.c", qualifiedName xwc)
            | "yua" -> Assert.AreEqual<string>("y.u.a", qualifiedName yua)
            | "yub" -> Assert.AreEqual<string>("y.u.b", qualifiedName yub)
            | "yuc" -> Assert.AreEqual<string>("y.u.c", qualifiedName yuc)
            | "yva" -> Assert.AreEqual<string>("y.v.a", qualifiedName yva)
            | "yvb" -> Assert.AreEqual<string>("y.v.b", qualifiedName yvb)
            | "yvc" -> Assert.AreEqual<string>("y.v.c", qualifiedName yvc)
            | "ywa" -> Assert.AreEqual<string>("y.w.a", qualifiedName ywa)
            | "ywb" -> Assert.AreEqual<string>("y.w.b", qualifiedName ywb)
            | "ywc" -> Assert.AreEqual<string>("y.w.c", qualifiedName ywc)
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)


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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("QualifiedName")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", qualifiedName r)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInSignatureQualifiedName", qualifiedName theory)
            | "block" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureQualifiedName.TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", qualifiedName block)
            | "x" -> Assert.AreEqual<string>("x", qualifiedName x)
            | "y" -> Assert.AreEqual<string>("y", qualifiedName y)
            | "xu" -> Assert.AreEqual<string>("x.u", qualifiedName xu)
            | "xv" -> Assert.AreEqual<string>("x.v", qualifiedName xv)
            | "xw" -> Assert.AreEqual<string>("x.w", qualifiedName xw)
            | "yu" -> Assert.AreEqual<string>("y.u", qualifiedName yu)
            | "yv" -> Assert.AreEqual<string>("y.v", qualifiedName yv)
            | "yw" -> Assert.AreEqual<string>("y.w", qualifiedName yw)
            | "xua" -> Assert.AreEqual<string>("x.u.a", qualifiedName xua)
            | "xub" -> Assert.AreEqual<string>("x.u.b", qualifiedName xub)
            | "xuc" -> Assert.AreEqual<string>("x.u.c", qualifiedName xuc)
            | "xva" -> Assert.AreEqual<string>("x.v.a", qualifiedName xva)
            | "xvb" -> Assert.AreEqual<string>("x.v.b", qualifiedName xvb)
            | "xvc" -> Assert.AreEqual<string>("x.v.c", qualifiedName xvc)
            | "xwa" -> Assert.AreEqual<string>("x.w.a", qualifiedName xwa)
            | "xwb" -> Assert.AreEqual<string>("x.w.b", qualifiedName xwb)
            | "xwc" -> Assert.AreEqual<string>("x.w.c", qualifiedName xwc)
            | "yua" -> Assert.AreEqual<string>("y.u.a", qualifiedName yua)
            | "yub" -> Assert.AreEqual<string>("y.u.b", qualifiedName yub)
            | "yuc" -> Assert.AreEqual<string>("y.u.c", qualifiedName yuc)
            | "yva" -> Assert.AreEqual<string>("y.v.a", qualifiedName yva)
            | "yvb" -> Assert.AreEqual<string>("y.v.b", qualifiedName yvb)
            | "yvc" -> Assert.AreEqual<string>("y.v.c", qualifiedName yvc)
            | "ywa" -> Assert.AreEqual<string>("y.w.a", qualifiedName ywa)
            | "ywb" -> Assert.AreEqual<string>("y.w.b", qualifiedName ywb)
            | "ywc" -> Assert.AreEqual<string>("y.w.c", qualifiedName ywc)
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("QualifiedName")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", qualifiedName r)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicQualifiedName", qualifiedName theory)
            | "block" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicQualifiedName.TestPredicate(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", qualifiedName block)
            | "x" -> Assert.AreEqual<string>("x", qualifiedName x)
            | "y" -> Assert.AreEqual<string>("y", qualifiedName y)
            | "xu" -> Assert.AreEqual<string>("x.u", qualifiedName xu)
            | "xv" -> Assert.AreEqual<string>("x.v", qualifiedName xv)
            | "xw" -> Assert.AreEqual<string>("x.w", qualifiedName xw)
            | "yu" -> Assert.AreEqual<string>("y.u", qualifiedName yu)
            | "yv" -> Assert.AreEqual<string>("y.v", qualifiedName yv)
            | "yw" -> Assert.AreEqual<string>("y.w", qualifiedName yw)
            | "xua" -> Assert.AreEqual<string>("x.u.a", qualifiedName xua)
            | "xub" -> Assert.AreEqual<string>("x.u.b", qualifiedName xub)
            | "xuc" -> Assert.AreEqual<string>("x.u.c", qualifiedName xuc)
            | "xva" -> Assert.AreEqual<string>("x.v.a", qualifiedName xva)
            | "xvb" -> Assert.AreEqual<string>("x.v.b", qualifiedName xvb)
            | "xvc" -> Assert.AreEqual<string>("x.v.c", qualifiedName xvc)
            | "xwa" -> Assert.AreEqual<string>("x.w.a", qualifiedName xwa)
            | "xwb" -> Assert.AreEqual<string>("x.w.b", qualifiedName xwb)
            | "xwc" -> Assert.AreEqual<string>("x.w.c", qualifiedName xwc)
            | "yua" -> Assert.AreEqual<string>("y.u.a", qualifiedName yua)
            | "yub" -> Assert.AreEqual<string>("y.u.b", qualifiedName yub)
            | "yuc" -> Assert.AreEqual<string>("y.u.c", qualifiedName yuc)
            | "yva" -> Assert.AreEqual<string>("y.v.a", qualifiedName yva)
            | "yvb" -> Assert.AreEqual<string>("y.v.b", qualifiedName yvb)
            | "yvc" -> Assert.AreEqual<string>("y.v.c", qualifiedName yvc)
            | "ywa" -> Assert.AreEqual<string>("y.w.a", qualifiedName ywa)
            | "ywb" -> Assert.AreEqual<string>("y.w.b", qualifiedName ywb)
            | "ywc" -> Assert.AreEqual<string>("y.w.c", qualifiedName ywc)
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

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
    [<DataRow("base18", "ex x:pred(a:T), y:C, z:obj {and (a,and(b,c))}")>]
    [<DataRow("base19", "exn$1 x:obj {all y:N {true}}")>]
    [<DataRow("base20", "all x:obj {not x}")>]
    [<DataRow("base21", "and(x, abc(y, z))")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not(x)")>]
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
        let filename = "TestPredicateQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base2" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base3" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base4" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().-(1)", qualifiedName base1)
            | "base5" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().Test()", qualifiedName base1)
            | "base6" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base7" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().Test$1(x)", qualifiedName base1)
            | "base8" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base9" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base10" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base11" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base12" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base13" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().1", qualifiedName base1)
            | "base11a" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().v", qualifiedName base1)
            | "base12a" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base10b" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base11b" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().v", qualifiedName base1)
            | "base12b" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base13b" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().1()", qualifiedName base1)
            | "base10c" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base11c" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().v", qualifiedName base1)
            | "base12c" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base13c" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().1(x, y)", qualifiedName base1)
            | "base10d" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base11d" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().v", qualifiedName base1)
            | "base12d" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base13d" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().1[x]", qualifiedName base1)
            | "base10e" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base11e" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().v", qualifiedName base1)
            | "base12e" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base13e" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().1(x, y).T[a, b]", qualifiedName base1)
            | "base10f" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().Test[x, y].x", qualifiedName base1)
            | "base11f" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().v", qualifiedName base1)
            | "base12f" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base13f" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().1[x].T(a, b)", qualifiedName base1)
            | "base14" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base15" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().-(x)", qualifiedName base1)
            | "base15a" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().'(x)", qualifiedName base1)
            | "base15b" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().'(-(x))", qualifiedName base1)
            | "base16" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().-(*(=(+(y, x), 2), x))", qualifiedName base1)
            | "base17" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().'(*(=(+(y, '(x)), 2), x))", qualifiedName base1)
            | "base18" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().ex(x(T), y, z)", qualifiedName base1)
            | "base19" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().exn$1(x)" , qualifiedName base1)
            | "base20" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().all(x)", qualifiedName base1)
            | "base21" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().and(x, abc)", qualifiedName base1)
            | "base21a" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().not(x)", qualifiedName base1)
            | "base21b" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().not(x)", qualifiedName base1)
            | "base22" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base23" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base24" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base25" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base26" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base27" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base28" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base29" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base30" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base31" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base32" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
            | "base33" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1().p(c)", qualifiedName base1)
            | "base34" -> Assert.AreEqual<string>("TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1)
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
            let stmt = ctor.ArgList[0]
            let base1 = stmt.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.B()", qualifiedName base1)
            | "base2" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.C(a, b, c, d)", qualifiedName base1)
            | "base3" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.D(A, a, b)", qualifiedName base1)
            | "base4" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.B(In(x))", qualifiedName base1)
            | "base5" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.C(Test1(a), Test2(b, c, d))", qualifiedName base1)
            | "base6" -> Assert.AreEqual<string>("TestCallConstructorParentClassQualifiedName.A.A(T1, func, ind, pred).bas.E(true, undef, false)", qualifiedName base1)
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
        let fplCode = sprintf "def pred T1() { dec ~a:T1 ~b:pred ~c:@Nat ~d:ind; %s };" varVal
        let filename = "TestDelegateQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().B()", qualifiedName base1)
            | "base2" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().C(a, b, c, d)", qualifiedName base1)
            | "base3" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().D(T1(), b, c)", qualifiedName base1)
            | "base4" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().B(In(x))", qualifiedName base1)
            | "base5" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().Test()", qualifiedName base1)
            | "base6" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().C(Test1(a), Test2(b, c, d))", qualifiedName base1)
            | "base7" -> Assert.AreEqual<string>("TestDelegateQualifiedName.T1().E(true, undef, false)", qualifiedName base1)
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
    member this.TestFixNotationQualifiedName(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestFixNotationQualifiedName"
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
            | "base1" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1()", qualifiedName base1)
            | "base2" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1()", qualifiedName base1)
            | "base3" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1()", qualifiedName base1)
            | "base4" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1()", qualifiedName base1)
            | "base5" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1", qualifiedName base1)
            | "base5a" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1", qualifiedName base1)
            | "base6" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1() -> obj", qualifiedName base1)
            | "base7" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1() -> obj", qualifiedName base1)
            | "base8" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1() -> obj", qualifiedName base1)
            | "base9" -> Assert.AreEqual<string>("TestFixNotationQualifiedName.T1() -> obj", qualifiedName base1)
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
    member this.TestMappingQualifiedName(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestMappingQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> obj.", qualifiedName mapping)
            | "base2" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> ind.", qualifiedName mapping)
            | "base3" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> func.", qualifiedName mapping)
            | "base4" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> pred.", qualifiedName mapping)
            | "base5" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> A.", qualifiedName mapping)
            | "base6" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> obj(ind).", qualifiedName mapping)
            | "base7" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> pred(*obj).", qualifiedName mapping)
            | "base8" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> func(*pred(obj)) -> pred(ind).", qualifiedName mapping)
            | "base9" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> pred(+func(A) -> A).", qualifiedName mapping)
            | "base10" -> Assert.AreEqual<string>("TestMappingQualifiedName.T() -> A(func(A) -> A).", qualifiedName mapping)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """100. |- trivial""")>]
    [<DataRow("base2", """100. ExistsByExample, 1 |- false""")>]
    [<DataRow("base3", """100. T1 |- assume not somePremise """)>]
    [<DataRow("base4", """100. 2, 3, 5 |- iif (a,b)""")>]
    [<DataRow("base5", """100. |- revoke 3""")>]
    [<TestMethod>]
    member this.TestArgumentQualifiedName(var, argExpression) =
        ad.Clear()
        let fplCode = sprintf """proof T$1 { %s };""" argExpression
        let filename = "TestArgumentQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let proof = theory.Scope["T$1"]
            let arg = proof.Scope["100"]
            match var with
            | "base1" -> Assert.AreEqual<string>("TestArgumentQualifiedName.T$1.100.", qualifiedName arg)
            | "base2" -> Assert.AreEqual<string>("TestArgumentQualifiedName.T$1.100.", qualifiedName arg)
            | "base3" -> Assert.AreEqual<string>("TestArgumentQualifiedName.T$1.100.", qualifiedName arg)
            | "base4" -> Assert.AreEqual<string>("TestArgumentQualifiedName.T$1.100.", qualifiedName arg)
            | "base5" -> Assert.AreEqual<string>("TestArgumentQualifiedName.T$1.100.", qualifiedName arg)
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
    member this.TestLanguageQualifiedName(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLanguageQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<string>("TestLanguageQualifiedName.true.tex", qualifiedName lang)
            | "base1" -> Assert.AreEqual<string>("TestLanguageQualifiedName.iif(x, y).tex", qualifiedName lang)
            | "base2" -> Assert.AreEqual<string>("TestLanguageQualifiedName.not(x).tex", qualifiedName lang)
            | "base3" -> Assert.AreEqual<string>("TestLanguageQualifiedName.and(p, q).tex", qualifiedName lang)
            | "base4" -> Assert.AreEqual<string>("TestLanguageQualifiedName.Equal(x, y).tex", qualifiedName lang)
            | "base5" -> Assert.AreEqual<string>("TestLanguageQualifiedName.NotEqual(x, y).tex", qualifiedName lang)
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
    member this.TestLocalizationQualifiedName(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLocalizationQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<string>("TestLocalizationQualifiedName." + predName, qualifiedName pred)
            | "base2" -> Assert.AreEqual<string>("TestLocalizationQualifiedName." + predName, qualifiedName pred)
            | "base3" -> Assert.AreEqual<string>("TestLocalizationQualifiedName." + predName, qualifiedName pred)
            | "base4" -> Assert.AreEqual<string>("TestLocalizationQualifiedName." + predName, qualifiedName pred)
            | "base5" -> Assert.AreEqual<string>("TestLocalizationQualifiedName." + predName, qualifiedName pred)
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
    member this.TestTranslationQualifiedName(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestTranslationQualifiedName"
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
            | "base0" -> Assert.AreEqual<string>(@"TestTranslationQualifiedName.true.tex.", qualifiedName trsl)
            | "base1" -> Assert.AreEqual<string>(@"TestTranslationQualifiedName.iif(x, y).tex.", qualifiedName trsl)
            | "base2" -> Assert.AreEqual<string>(@"TestTranslationQualifiedName.not(x).tex.", qualifiedName trsl)
            | "base3" -> Assert.AreEqual<string>(@"TestTranslationQualifiedName.and(p, q).tex.", qualifiedName trsl)
            | "base4" -> Assert.AreEqual<string>(@"TestTranslationQualifiedName.Equal(x, y).tex.", qualifiedName trsl)
            | "base5" -> Assert.AreEqual<string>(@"TestTranslationQualifiedName.NotEqual(x, y).tex.", qualifiedName trsl)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
