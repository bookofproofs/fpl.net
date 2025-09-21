namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplPrimitives
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers


[<TestClass>]
type TestFplValueScopeFplId() =

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
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("FplId") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.FplId)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeBlocksFplId", theory.FplId)
            | "inf1" -> Assert.AreEqual<string>("SomeInference1", inf1.FplId)
            | "inf2" -> Assert.AreEqual<string>("SomeInference2", inf2.FplId)
            | "axi1" -> Assert.AreEqual<string>("SomeAxiom1", axi1.FplId)
            | "axi2" -> Assert.AreEqual<string>("SomeAxiom2", axi2.FplId)
            | "pst1" -> Assert.AreEqual<string>("SomePostulate1", pst1.FplId)
            | "pst2" -> Assert.AreEqual<string>("SomePostulate2", pst2.FplId)
            | "thm1" -> Assert.AreEqual<string>("SomeTheorem1", thm1.FplId)
            | "thm2" -> Assert.AreEqual<string>("SomeTheorem2", thm2.FplId)
            | "pro1" -> Assert.AreEqual<string>("SomeProposition1", pro1.FplId)
            | "pro2" -> Assert.AreEqual<string>("SomeProposition2", pro2.FplId)
            | "lem1" -> Assert.AreEqual<string>("SomeLemma1", lem1.FplId)
            | "lem2" -> Assert.AreEqual<string>("SomeLemma2", lem2.FplId)
            | "cor1" -> Assert.AreEqual<string>("SomeLemma1$1", cor1.FplId)
            | "cor2" -> Assert.AreEqual<string>("SomeLemma2$1", cor2.FplId)
            | "con1" -> Assert.AreEqual<string>("SomeConjecture1", con1.FplId)
            | "con2" -> Assert.AreEqual<string>("SomeConjecture2", con2.FplId)
            | "cla1" -> Assert.AreEqual<string>("SomeClass1", cla1.FplId)
            | "cla2" -> Assert.AreEqual<string>("SomeClass2", cla2.FplId)
            | "pre1" -> Assert.AreEqual<string>("SomePredicate1", pre1.FplId)
            | "pre2" -> Assert.AreEqual<string>("SomePredicate2", pre2.FplId)
            | "fun1" -> Assert.AreEqual<string>("SomeFunctionalTerm1", fun1.FplId)
            | "fun2" -> Assert.AreEqual<string>("SomeFunctionalTerm2", fun2.FplId)
            | "fun3" -> Assert.AreEqual<string>("SomeFunctionalTerm3", fun3.FplId)
            | "fun4" -> Assert.AreEqual<string>("SomeFunctionalTerm4", fun4.FplId)
            | "fun5" -> Assert.AreEqual<string>("SomeFunctionalTerm5", fun5.FplId)
            | "fun6" -> Assert.AreEqual<string>("SomeFunctionalTerm6", fun6.FplId)
            | "fun7" -> Assert.AreEqual<string>("SomeFunctionalTerm7", fun7.FplId)
            | "fun8" -> Assert.AreEqual<string>("SomeFunctionalTerm8", fun8.FplId)
            | "fun9" -> Assert.AreEqual<string>("SomeFunctionalTerm9", fun9.FplId)
            | "prf1" -> Assert.AreEqual<string>("SomeTheorem1$1", prf1.FplId)
            | "prf2" -> Assert.AreEqual<string>("SomeTheorem2$1", prf2.FplId)
            | "loc1" -> Assert.AreEqual<string>(LiteralNot, loc1.FplId)
            | "loc2" -> Assert.AreEqual<string>("Equal", loc2.FplId)
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
        let res = CommonFplValueTestCases.ScopeConstructors("FplId") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.FplId)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeConstructorsFplId", theory.FplId)
            | "block" -> Assert.AreEqual<string>("TestId", block.FplId)
            | "t1" -> Assert.AreEqual<string>("TestId", t1.FplId)
            | "t2" -> Assert.AreEqual<string>("TestId", t2.FplId)
            | "t3" -> Assert.AreEqual<string>("TestId", t3.FplId)
            | "t4" -> Assert.AreEqual<string>("TestId", t4.FplId)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("FplId") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<string>("", r.FplId)
                | PrimTheoryL -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesFplId", theory.FplId)
                | "thm1" -> Assert.AreEqual<string>("TestTheorem1", thm1.FplId)
                | "proofThm1" -> Assert.AreEqual<string>("TestTheorem1$1", proofThm1.FplId)
                | "lem1" -> Assert.AreEqual<string>("TestLemma1", lem1.FplId)
                | "proofLem1" -> Assert.AreEqual<string>("TestLemma1$1", proofLem1.FplId)
                | "prp1" -> Assert.AreEqual<string>("TestProposition1", prp1.FplId)
                | "proofPrp1" -> Assert.AreEqual<string>("TestProposition1$1", proofPrp1.FplId)
                | "cor1" -> Assert.AreEqual<string>("TestCorollary1$2", cor1.FplId)
                | "proofCor1" -> Assert.AreEqual<string>("TestCorollary1$2$1", proofCor1.FplId)
                | "thm2" -> Assert.AreEqual<string>("TestTheorem2", thm2.FplId)
                | "corThm2" -> Assert.AreEqual<string>("TestTheorem2$1", corThm2.FplId)
                | "lem2" -> Assert.AreEqual<string>("TestLemma2", lem2.FplId)
                | "corLem2" -> Assert.AreEqual<string>("TestLemma2$1", corLem2.FplId)
                | "prp2" -> Assert.AreEqual<string>("TestProposition2", prp2.FplId)
                | "corPrp2" -> Assert.AreEqual<string>("TestProposition2$1", corPrp2.FplId)
                | "cor2" -> Assert.AreEqual<string>("TestCorollary2$2", cor2.FplId)
                | "corCor2" -> Assert.AreEqual<string>("TestCorollary2$2$1", corCor2.FplId)
                | "con1" -> Assert.AreEqual<string>("TestConjecture", con1.FplId)
                | "corCon1" -> Assert.AreEqual<string>("TestConjecture$1", corCon1.FplId)
                | "axi1" -> Assert.AreEqual<string>("TestAxiom", axi1.FplId)
                | "corAxi1"  -> Assert.AreEqual<string>("TestAxiom$1", corAxi1.FplId) 
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
        let res = CommonFplValueTestCases.ScopeProperties("FplId") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.FplId)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopePropertiesFplId", theory.FplId)
            | "block" -> Assert.AreEqual<string>("TestId", block.FplId)
            | "t1" -> Assert.AreEqual<string>("T1", t1.FplId)
            | "t2" -> Assert.AreEqual<string>("T2", t2.FplId)
            | "t3" -> Assert.AreEqual<string>("T3", t3.FplId)
            | "t4" -> Assert.AreEqual<string>("T4", t4.FplId)
            | "t5" -> Assert.AreEqual<string>("T5", t5.FplId)
            | "t6" -> Assert.AreEqual<string>("T6", t6.FplId)
            | "t7" -> Assert.AreEqual<string>("T7", t7.FplId)
            | "t8" -> Assert.AreEqual<string>("T8", t8.FplId)
            | "t9" -> Assert.AreEqual<string>("T9", t9.FplId)
            | "t10" -> Assert.AreEqual<string>("T10", t10.FplId)
            | "t11" -> Assert.AreEqual<string>("T11", t11.FplId)
            | "t12" -> Assert.AreEqual<string>("T12", t12.FplId)
            | "t13" -> Assert.AreEqual<string>("T13", t13.FplId)
            | "t14" -> Assert.AreEqual<string>("T14", t14.FplId)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("FplId")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.FplId)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInBlockFplId", theory.FplId)
            | "block" -> Assert.AreEqual<string>("TestPredicate", block.FplId); 
            | "x" -> Assert.AreEqual<string>("x", x.FplId)
            | "y" -> Assert.AreEqual<string>("y", y.FplId)
            | "s" -> Assert.AreEqual<string>("s", s.FplId)
            | "xu" -> Assert.AreEqual<string>("u", xu.FplId)
            | "xv" -> Assert.AreEqual<string>("v", xv.FplId)
            | "xw" -> Assert.AreEqual<string>("w", xw.FplId)
            | "yu" -> Assert.AreEqual<string>("u", yu.FplId)
            | "yv" -> Assert.AreEqual<string>("v", yv.FplId)
            | "yw" -> Assert.AreEqual<string>("w", yw.FplId)
            | "xua" -> Assert.AreEqual<string>("a", xua.FplId)
            | "xub" -> Assert.AreEqual<string>("b", xub.FplId)
            | "xuc" -> Assert.AreEqual<string>("c", xuc.FplId)
            | "xva" -> Assert.AreEqual<string>("a", xva.FplId)
            | "xvb" -> Assert.AreEqual<string>("b", xvb.FplId)
            | "xvc" -> Assert.AreEqual<string>("c", xvc.FplId)
            | "xwa" -> Assert.AreEqual<string>("a", xwa.FplId)
            | "xwb" -> Assert.AreEqual<string>("b", xwb.FplId)
            | "xwc" -> Assert.AreEqual<string>("c", xwc.FplId)
            | "yua" -> Assert.AreEqual<string>("a", yua.FplId)
            | "yub" -> Assert.AreEqual<string>("b", yub.FplId)
            | "yuc" -> Assert.AreEqual<string>("c", yuc.FplId)
            | "yva" -> Assert.AreEqual<string>("a", yva.FplId)
            | "yvb" -> Assert.AreEqual<string>("b", yvb.FplId)
            | "yvc" -> Assert.AreEqual<string>("c", yvc.FplId)
            | "ywa" -> Assert.AreEqual<string>("a", ywa.FplId)
            | "ywb" -> Assert.AreEqual<string>("b", ywb.FplId)
            | "ywc" -> Assert.AreEqual<string>("c", ywc.FplId)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("FplId")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.FplId)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicFplId", theory.FplId)
            | "block" -> Assert.AreEqual<string>("TestPredicate", block.FplId); 
            | "x" -> Assert.AreEqual<string>("x", x.FplId)
            | "y" -> Assert.AreEqual<string>("y", y.FplId)
            | "xu" -> Assert.AreEqual<string>("u", xu.FplId)
            | "xv" -> Assert.AreEqual<string>("v", xv.FplId)
            | "xw" -> Assert.AreEqual<string>("w", xw.FplId)
            | "yu" -> Assert.AreEqual<string>("u", yu.FplId)
            | "yv" -> Assert.AreEqual<string>("v", yv.FplId)
            | "yw" -> Assert.AreEqual<string>("w", yw.FplId)
            | "xua" -> Assert.AreEqual<string>("a", xua.FplId)
            | "xub" -> Assert.AreEqual<string>("b", xub.FplId)
            | "xuc" -> Assert.AreEqual<string>("c", xuc.FplId)
            | "xva" -> Assert.AreEqual<string>("a", xva.FplId)
            | "xvb" -> Assert.AreEqual<string>("b", xvb.FplId)
            | "xvc" -> Assert.AreEqual<string>("c", xvc.FplId)
            | "xwa" -> Assert.AreEqual<string>("a", xwa.FplId)
            | "xwb" -> Assert.AreEqual<string>("b", xwb.FplId)
            | "xwc" -> Assert.AreEqual<string>("c", xwc.FplId)
            | "yua" -> Assert.AreEqual<string>("a", yua.FplId)
            | "yub" -> Assert.AreEqual<string>("b", yub.FplId)
            | "yuc" -> Assert.AreEqual<string>("c", yuc.FplId)
            | "yva" -> Assert.AreEqual<string>("a", yva.FplId)
            | "yvb" -> Assert.AreEqual<string>("b", yvb.FplId)
            | "yvc" -> Assert.AreEqual<string>("c", yvc.FplId)
            | "ywa" -> Assert.AreEqual<string>("a", ywa.FplId)
            | "ywb" -> Assert.AreEqual<string>("b", ywb.FplId)
            | "ywc" -> Assert.AreEqual<string>("c", ywc.FplId)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("FplId")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.FplId)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInSignatureFplId", theory.FplId)
            | "block" -> Assert.AreEqual<string>("TestPredicate", block.FplId); 
            | "x" -> Assert.AreEqual<string>("x", x.FplId)
            | "y" -> Assert.AreEqual<string>("y", y.FplId)
            | "xu" -> Assert.AreEqual<string>("u", xu.FplId)
            | "xv" -> Assert.AreEqual<string>("v", xv.FplId)
            | "xw" -> Assert.AreEqual<string>("w", xw.FplId)
            | "yu" -> Assert.AreEqual<string>("u", yu.FplId)
            | "yv" -> Assert.AreEqual<string>("v", yv.FplId)
            | "yw" -> Assert.AreEqual<string>("w", yw.FplId)
            | "xua" -> Assert.AreEqual<string>("a", xua.FplId)
            | "xub" -> Assert.AreEqual<string>("b", xub.FplId)
            | "xuc" -> Assert.AreEqual<string>("c", xuc.FplId)
            | "xva" -> Assert.AreEqual<string>("a", xva.FplId)
            | "xvb" -> Assert.AreEqual<string>("b", xvb.FplId)
            | "xvc" -> Assert.AreEqual<string>("c", xvc.FplId)
            | "xwa" -> Assert.AreEqual<string>("a", xwa.FplId)
            | "xwb" -> Assert.AreEqual<string>("b", xwb.FplId)
            | "xwc" -> Assert.AreEqual<string>("c", xwc.FplId)
            | "yua" -> Assert.AreEqual<string>("a", yua.FplId)
            | "yub" -> Assert.AreEqual<string>("b", yub.FplId)
            | "yuc" -> Assert.AreEqual<string>("c", yuc.FplId)
            | "yva" -> Assert.AreEqual<string>("a", yva.FplId)
            | "yvb" -> Assert.AreEqual<string>("b", yvb.FplId)
            | "yvc" -> Assert.AreEqual<string>("c", yvc.FplId)
            | "ywa" -> Assert.AreEqual<string>("a", ywa.FplId)
            | "ywb" -> Assert.AreEqual<string>("b", ywb.FplId)
            | "ywc" -> Assert.AreEqual<string>("c", ywc.FplId)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("FplId")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>("", r.FplId)
            | PrimTheoryL -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicFplId", theory.FplId)
            | "block" -> Assert.AreEqual<string>("TestPredicate", block.FplId); 
            | "x" -> Assert.AreEqual<string>("x", x.FplId)
            | "y" -> Assert.AreEqual<string>("y", y.FplId)
            | "xu" -> Assert.AreEqual<string>("u", xu.FplId)
            | "xv" -> Assert.AreEqual<string>("v", xv.FplId)
            | "xw" -> Assert.AreEqual<string>("w", xw.FplId)
            | "yu" -> Assert.AreEqual<string>("u", yu.FplId)
            | "yv" -> Assert.AreEqual<string>("v", yv.FplId)
            | "yw" -> Assert.AreEqual<string>("w", yw.FplId)
            | "xua" -> Assert.AreEqual<string>("a", xua.FplId)
            | "xub" -> Assert.AreEqual<string>("b", xub.FplId)
            | "xuc" -> Assert.AreEqual<string>("c", xuc.FplId)
            | "xva" -> Assert.AreEqual<string>("a", xva.FplId)
            | "xvb" -> Assert.AreEqual<string>("b", xvb.FplId)
            | "xvc" -> Assert.AreEqual<string>("c", xvc.FplId)
            | "xwa" -> Assert.AreEqual<string>("a", xwa.FplId)
            | "xwb" -> Assert.AreEqual<string>("b", xwb.FplId)
            | "xwc" -> Assert.AreEqual<string>("c", xwc.FplId)
            | "yua" -> Assert.AreEqual<string>("a", yua.FplId)
            | "yub" -> Assert.AreEqual<string>("b", yub.FplId)
            | "yuc" -> Assert.AreEqual<string>("c", yuc.FplId)
            | "yva" -> Assert.AreEqual<string>("a", yva.FplId)
            | "yvb" -> Assert.AreEqual<string>("b", yvb.FplId)
            | "yvc" -> Assert.AreEqual<string>("c", yvc.FplId)
            | "ywa" -> Assert.AreEqual<string>("a", ywa.FplId)
            | "ywb" -> Assert.AreEqual<string>("b", ywb.FplId)
            | "ywc" -> Assert.AreEqual<string>("c", ywc.FplId)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", LiteralTrue)>]
    [<DataRow("base2", LiteralFalse)>]
    [<DataRow("base3", LiteralUndef)>]
    [<DataRow("base4", "-1")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "$1")>]
    [<DataRow("base7", "Test$1")>] 
    [<DataRow("base8", "Test$1")>]
    [<DataRow("base9", "Test$1")>]
    [<DataRow("base10", "Test")>]
    [<DataRow("base11", "v")>]
    [<DataRow("base12", LiteralSelf)>]
    [<DataRow("base13", "@1")>]
    [<DataRow("base13a", "1")>]
    [<DataRow("base11a", "v.x")>]
    [<DataRow("base12a", "self.x")>]
    [<DataRow("base10b", "Test()")>]
    [<DataRow("base11b", "v()")>]
    [<DataRow("base12b", "self()")>]
    [<DataRow("base13b", "@1()")>]
    [<DataRow("base10c", "Test(x, y)")>]
    [<DataRow("base11c", "v(x, y)")>]
    [<DataRow("base12c", "self(x, y)")>]
    [<DataRow("base13c", "@1(x, y)")>]
    [<DataRow("base10d", "Test[x, y]")>]
    [<DataRow("base11d", "v[x, y]")>]
    [<DataRow("base12d", "self[x, y]")>]
    [<DataRow("base13d", "@1[x.y]")>]
    [<DataRow("base10e", "Test(x, y).parent[a, b]")>]
    [<DataRow("base11e", "v(x, y).x[a, b]")>]
    [<DataRow("base12e", "self(x, y).@3[a, b]")>]
    [<DataRow("base13e", "@1(x, y).T[a, b]")>]
    [<DataRow("base10f", "Test[x, y].x(a, b)")>]
    [<DataRow("base11f", "v[x, y].x(a, b)")>]
    [<DataRow("base12f", "self[x, y].self(a, b)")>]
    [<DataRow("base13f", "@1[x.y].T(a, b)")>]
    [<DataRow("base14", "∅")>]
    [<DataRow("base15", "-x")>]
    [<DataRow("base15a", "x'")>]
    [<DataRow("base15b", "-x'")>]
    [<DataRow("base16", "-(y + x = @2 * x)")>]
    [<DataRow("base17", "(y + x' = @2 * x)'")>]
    [<DataRow("base18", "ex x:Range(a:T), y:C, z:obj {and (a,and(b,c))}")>]
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
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base2" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base3" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base4" -> Assert.AreEqual<string>("-", base1.FplId)
            | "base5" -> Assert.AreEqual<string>("Test", base1.FplId)
            | "base6" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base7" -> Assert.AreEqual<string>("Test$1", base1.FplId)
            | "base8" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base9" -> Assert.AreEqual<string>("Test$1", base1.FplId)
            | "base10" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base11" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base12" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base13" -> Assert.AreEqual<string>("1", base1.FplId)
            | "base13a" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base11a" -> Assert.AreEqual<string>("v", base1.FplId)
            | "base12a" -> Assert.AreEqual<string>(LiteralSelf, base1.FplId)
            | "base10b" -> Assert.AreEqual<string>("Test", base1.FplId)
            | "base11b" -> Assert.AreEqual<string>("v", base1.FplId)
            | "base12b" -> Assert.AreEqual<string>(LiteralSelf, base1.FplId)
            | "base13b" -> Assert.AreEqual<string>("1", base1.FplId)
            | "base10c" -> Assert.AreEqual<string>("Test", base1.FplId)
            | "base11c" -> Assert.AreEqual<string>("v", base1.FplId)
            | "base12c" -> Assert.AreEqual<string>(LiteralSelf, base1.FplId)
            | "base13c" -> Assert.AreEqual<string>("1", base1.FplId)
            | "base10d" -> Assert.AreEqual<string>("Test", base1.FplId)
            | "base11d" -> Assert.AreEqual<string>("v", base1.FplId)
            | "base12d" -> Assert.AreEqual<string>(LiteralSelf, base1.FplId)
            | "base13d" -> Assert.AreEqual<string>("1", base1.FplId)
            | "base10e" -> Assert.AreEqual<string>("Test", base1.FplId)
            | "base11e" -> Assert.AreEqual<string>("v", base1.FplId)
            | "base12e" -> Assert.AreEqual<string>(LiteralSelf, base1.FplId)
            | "base13e" -> Assert.AreEqual<string>("1", base1.FplId)
            | "base10f" -> Assert.AreEqual<string>("Test", base1.FplId)
            | "base11f" -> Assert.AreEqual<string>("v", base1.FplId)
            | "base12f" -> Assert.AreEqual<string>(LiteralSelf, base1.FplId)
            | "base13f" -> Assert.AreEqual<string>("1", base1.FplId)
            | "base14" -> Assert.AreEqual<string>(varVal, base1.FplId)
            | "base15" -> Assert.AreEqual<string>("-", base1.FplId)
            | "base15a" -> Assert.AreEqual<string>("'", base1.FplId)
            | "base15b" -> Assert.AreEqual<string>("'", base1.FplId)
            | "base16" -> Assert.AreEqual<string>("-", base1.FplId)
            | "base17" -> Assert.AreEqual<string>("'", base1.FplId)
            | "base18" -> Assert.AreEqual<string>(LiteralEx, base1.FplId)
            | "base19" -> Assert.AreEqual<string>("exn$1", base1.FplId)
            | "base20" -> Assert.AreEqual<string>(LiteralAll, base1.FplId)
            | "base21" -> Assert.AreEqual<string>(LiteralAnd, base1.FplId)
            | "base21a" -> Assert.AreEqual<string>(LiteralNot, base1.FplId)
            | "base21b" -> Assert.AreEqual<string>(LiteralNot, base1.FplId)
            | "base22" -> Assert.AreEqual<string>(LiteralXor, base1.FplId)
            | "base23" -> Assert.AreEqual<string>(LiteralOr, base1.FplId)
            | "base24" -> Assert.AreEqual<string>(LiteralIif, base1.FplId)
            | "base25" -> Assert.AreEqual<string>(LiteralImpl, base1.FplId)
            | "base26" -> Assert.AreEqual<string>(LiteralIs, base1.FplId)
            | "base27" -> Assert.AreEqual<string>("B", base1.FplId)
            | "base28" -> Assert.AreEqual<string>("C", base1.FplId)
            | "base29" -> Assert.AreEqual<string>("D", base1.FplId)
            | "base30" -> Assert.AreEqual<string>("B", base1.FplId)
            | "base31" -> Assert.AreEqual<string>("C", base1.FplId)
            | "base32" -> Assert.AreEqual<string>("E", base1.FplId)
            | "base33" -> Assert.AreEqual<string>("p", base1.FplId)
            | "base34" -> Assert.AreEqual<string>(LiteralIs, base1.FplId)
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
            | "base1" -> Assert.AreEqual<string>("B", base1.FplId)
            | "base2" -> Assert.AreEqual<string>("C", base1.FplId)
            | "base3" -> Assert.AreEqual<string>("D", base1.FplId)
            | "base4" -> Assert.AreEqual<string>("B", base1.FplId)
            | "base5" -> Assert.AreEqual<string>("C", base1.FplId)
            | "base6" -> Assert.AreEqual<string>("E", base1.FplId)
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
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("B", base1.FplId)
            | "base2" -> Assert.AreEqual<string>("C", base1.FplId)
            | "base3" -> Assert.AreEqual<string>("D", base1.FplId)
            | "base4" -> Assert.AreEqual<string>("B", base1.FplId)
            | "base5" -> Assert.AreEqual<string>("Test", base1.FplId)
            | "base6" -> Assert.AreEqual<string>("C", base1.FplId)
            | "base7" -> Assert.AreEqual<string>("E", base1.FplId)
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
    member this.TestFixNotationFplId(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestFixNotationFplId"
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
            | "base1" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base2" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base3" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base4" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base5" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base5a" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base6" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base7" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base8" -> Assert.AreEqual<string>("T1", base1.FplId)
            | "base9" -> Assert.AreEqual<string>("T1", base1.FplId)
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
        let filename = "TestMappingFplId"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base2" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base3" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base4" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base5" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base6" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base7" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base8" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base9" -> Assert.AreEqual<string>("", mapping.FplId)
            | "base10" -> Assert.AreEqual<string>("", mapping.FplId)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """100. |- trivial""")>]
    [<DataRow("base2", """100. ExistsByExample, 1 |- false""")>]
    [<DataRow("base3", """100. T1 |- assume not somePremise """)>]
    [<DataRow("base4", """100. 2, 3, 5 |- iif (a,b)""")>]
    [<DataRow("base5", """100. |- revoke 3""")>]
    [<TestMethod>]
    member this.TestArgumentFplId(var, argExpression) =
        ad.Clear()
        let fplCode = sprintf """proof T$1 { %s };""" argExpression
        let filename = "TestArgumentFplId"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let proof = theory.Scope["T$1"]
            let arg = proof.Scope["100"]
            match var with
            | "base1" -> Assert.AreEqual<string>("100.", arg.FplId)
            | "base2" -> Assert.AreEqual<string>("100.", arg.FplId)
            | "base3" -> Assert.AreEqual<string>("100.", arg.FplId)
            | "base4" -> Assert.AreEqual<string>("100.", arg.FplId)
            | "base5" -> Assert.AreEqual<string>("100.", arg.FplId)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", LiteralTrue, """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLanguage(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestLanguageFplId"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<string>("tex", lang.FplId)
            | "base1" -> Assert.AreEqual<string>("tex", lang.FplId)
            | "base2" -> Assert.AreEqual<string>("tex", lang.FplId)
            | "base3" -> Assert.AreEqual<string>("tex", lang.FplId)
            | "base4" -> Assert.AreEqual<string>("tex", lang.FplId)
            | "base5" -> Assert.AreEqual<string>("tex", lang.FplId)
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
        let filename = "TestLocalizationFplId"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<string>(LiteralIif , pred.FplId)
            | "base2" -> Assert.AreEqual<string>(LiteralNot, pred.FplId)
            | "base3" -> Assert.AreEqual<string>(LiteralAnd, pred.FplId)
            | "base4" -> Assert.AreEqual<string>("Equal", pred.FplId)
            | "base5" -> Assert.AreEqual<string>("NotEqual", pred.FplId)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", LiteralTrue, """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x " \Leftrightarrow " y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p " \wedge " q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq " y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestTranslation(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestTranslationFplId"
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
            | "base0" -> Assert.AreEqual<string>(@"", trsl.FplId)
            | "base1" -> Assert.AreEqual<string>(@"", trsl.FplId)
            | "base2" -> Assert.AreEqual<string>(@"", trsl.FplId)
            | "base3" -> Assert.AreEqual<string>(@"", trsl.FplId)
            | "base4" -> Assert.AreEqual<string>(@"", trsl.FplId)
            | "base5" -> Assert.AreEqual<string>(@"", trsl.FplId)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("i")>]
    [<DataRow("b")>]
    [<DataRow("p")>]
    [<DataRow("o")>]
    [<DataRow("u")>]
    [<DataRow("t")>]
    [<TestMethod>]
    member this.TestIntrinsicPrimitives(var) =
        let res = CommonFplValueTestCases.ScopeIntrinsicPrimitives("FplId") 
        match res with
        | Some (i:FplValue, b:FplValue, p:FplValue, o:FplValue, u:FplValue, t:FplValue) -> 
            match var with 
            | "i" -> Assert.AreEqual<string>($"$1", i.FplId)
            | "b" -> Assert.AreEqual<string>($"{LiteralFunc}", b.FplId)
            | "p" -> Assert.AreEqual<string>($"{LiteralTrue}", p.FplId)
            | "o" -> Assert.AreEqual<string>($"A", o.FplId)
            | "u" -> Assert.AreEqual<string>($"{LiteralUndef}", u.FplId)
            | "t" -> Assert.AreEqual<string>("$2", t.FplId)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)
