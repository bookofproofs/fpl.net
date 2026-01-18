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
            | "r" -> Assert.AreEqual<string>("a root ", qualifiedName r false)
            | PrimTheoryL -> Assert.AreEqual<string>("a theory TestScopeBlocksQualifiedName", qualifiedName theory false)  
            | "inf1" -> Assert.AreEqual<string>("a rule of inference TestScopeBlocksQualifiedName.SomeInference1", qualifiedName inf1 false) 
            | "inf2" -> Assert.AreEqual<string>("a rule of inference TestScopeBlocksQualifiedName.SomeInference2", qualifiedName inf2 false) 
            | "axi1" -> Assert.AreEqual<string>("an axiom TestScopeBlocksQualifiedName.SomeAxiom1", qualifiedName axi1 false) 
            | "axi2" -> Assert.AreEqual<string>("an axiom TestScopeBlocksQualifiedName.SomeAxiom2", qualifiedName axi2 false) 
            | "pst1" -> Assert.AreEqual<string>("an axiom TestScopeBlocksQualifiedName.SomePostulate1", qualifiedName pst1 false) 
            | "pst2" -> Assert.AreEqual<string>("an axiom TestScopeBlocksQualifiedName.SomePostulate2", qualifiedName pst2 false) 
            | "thm1" -> Assert.AreEqual<string>("a theorem TestScopeBlocksQualifiedName.SomeTheorem1", qualifiedName thm1 false) 
            | "thm2" -> Assert.AreEqual<string>("a theorem TestScopeBlocksQualifiedName.SomeTheorem2", qualifiedName thm2 false) 
            | "pro1" -> Assert.AreEqual<string>("a proposition TestScopeBlocksQualifiedName.SomeProposition1", qualifiedName pro1 false) 
            | "pro2" -> Assert.AreEqual<string>("a proposition TestScopeBlocksQualifiedName.SomeProposition2", qualifiedName pro2 false) 
            | "lem1" -> Assert.AreEqual<string>("a lemma TestScopeBlocksQualifiedName.SomeLemma1", qualifiedName lem1 false) 
            | "lem2" -> Assert.AreEqual<string>("a lemma TestScopeBlocksQualifiedName.SomeLemma2", qualifiedName lem2 false) 
            | "cor1" -> Assert.AreEqual<string>("a corollary TestScopeBlocksQualifiedName.SomeLemma1.SomeLemma1$1", qualifiedName cor1 false) 
            | "cor2" -> Assert.AreEqual<string>("a corollary TestScopeBlocksQualifiedName.SomeLemma2.SomeLemma2$1", qualifiedName cor2 false) 
            | "con1" -> Assert.AreEqual<string>("a conjecture TestScopeBlocksQualifiedName.SomeConjecture1", qualifiedName con1 false) 
            | "con2" -> Assert.AreEqual<string>("a conjecture TestScopeBlocksQualifiedName.SomeConjecture2", qualifiedName con2 false) 
            | "cla1" -> Assert.AreEqual<string>("a class definition TestScopeBlocksQualifiedName.SomeClass1", qualifiedName cla1 false) 
            | "cla2" -> Assert.AreEqual<string>("a class definition TestScopeBlocksQualifiedName.SomeClass2", qualifiedName cla2 false) 
            | "pre1" -> Assert.AreEqual<string>("a predicate definition TestScopeBlocksQualifiedName.SomePredicate1()", qualifiedName pre1 false)
            | "pre2" -> Assert.AreEqual<string>("a predicate definition TestScopeBlocksQualifiedName.SomePredicate2()", qualifiedName pre2 false)
            | "fun1" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm1() -> obj", qualifiedName fun1 false)
            | "fun2" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm2() -> obj", qualifiedName fun2 false)
            | "fun3" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm3() -> obj", qualifiedName fun3 false)
            | "fun4" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm4() -> tpl", qualifiedName fun4 false) 
            | "fun5" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm5() -> SomeClass1", qualifiedName fun5 false)
            | "fun6" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm6() -> SomeClass1", qualifiedName fun6 false)
            | "fun7" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm7() -> SomeClass1", qualifiedName fun7 false)
            | "fun8" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm8() -> ind", qualifiedName fun8 false)
            | "fun9" -> Assert.AreEqual<string>("a functional term definition TestScopeBlocksQualifiedName.SomeFunctionalTerm9() -> ind", qualifiedName fun9 false)
            | "prf1" -> Assert.AreEqual<string>("a proof TestScopeBlocksQualifiedName.SomeTheorem1.SomeTheorem1$1", qualifiedName prf1 false) 
            | "prf2" -> Assert.AreEqual<string>("a proof TestScopeBlocksQualifiedName.SomeTheorem2.SomeTheorem2$1", qualifiedName prf2 false) 
            | "loc1" -> Assert.AreEqual<string>("a localization TestScopeBlocksQualifiedName.not(x)", qualifiedName loc1 false)
            | "loc2" -> Assert.AreEqual<string>("a localization TestScopeBlocksQualifiedName.Equal(x, y)", qualifiedName loc2 false)
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
            | "r" -> Assert.AreEqual<string>("a root ", qualifiedName r false) 
            | PrimTheoryL -> Assert.AreEqual<string>("a theory TestScopeConstructorsQualifiedName", qualifiedName theory false) 
            | "block" -> Assert.AreEqual<string>("a class definition TestScopeConstructorsQualifiedName.TestId", qualifiedName block false) 
            | "t1" -> Assert.AreEqual<string>("a constructor TestScopeConstructorsQualifiedName.TestId.TestId()", qualifiedName t1 false) 
            | "t2" -> Assert.AreEqual<string>("a constructor TestScopeConstructorsQualifiedName.TestId.TestId(obj)", qualifiedName t2 false) 
            | "t3" -> Assert.AreEqual<string>("a constructor TestScopeConstructorsQualifiedName.TestId.TestId(pred)", qualifiedName t3 false) 
            | "t4" -> Assert.AreEqual<string>("a constructor TestScopeConstructorsQualifiedName.TestId.TestId(ind)", qualifiedName t4 false) 
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
                | "r" -> Assert.AreEqual<string>("a root ", qualifiedName r false) 
                | PrimTheoryL -> Assert.AreEqual<string>("a theory TestScopeProofsAndCorollariesQualifiedName", qualifiedName theory false) 
                | "thm1" -> Assert.AreEqual<string>("a theorem TestScopeProofsAndCorollariesQualifiedName.TestTheorem1", qualifiedName thm1 false) 
                | "proofThm1" -> Assert.AreEqual<string>("a proof TestScopeProofsAndCorollariesQualifiedName.TestTheorem1.TestTheorem1$1", qualifiedName proofThm1 false) 
                | "lem1" -> Assert.AreEqual<string>("a lemma TestScopeProofsAndCorollariesQualifiedName.TestLemma1", qualifiedName lem1 false) 
                | "proofLem1" -> Assert.AreEqual<string>("a proof TestScopeProofsAndCorollariesQualifiedName.TestLemma1.TestLemma1$1", qualifiedName proofLem1 false) 
                | "prp1" -> Assert.AreEqual<string>("a proposition TestScopeProofsAndCorollariesQualifiedName.TestProposition1", qualifiedName prp1 false) 
                | "proofPrp1" -> Assert.AreEqual<string>("a proof TestScopeProofsAndCorollariesQualifiedName.TestProposition1.TestProposition1$1", qualifiedName proofPrp1 false) 
                | "cor1" -> Assert.AreEqual<string>("a corollary TestScopeProofsAndCorollariesQualifiedName.TestCorollary1$2", qualifiedName cor1 false) 
                | "proofCor1" -> Assert.AreEqual<string>("a proof TestScopeProofsAndCorollariesQualifiedName.TestCorollary1$2.TestCorollary1$2$1", qualifiedName proofCor1 false) 
                | "thm2" -> Assert.AreEqual<string>("a theorem TestScopeProofsAndCorollariesQualifiedName.TestTheorem2", qualifiedName thm2 false) 
                | "corThm2" -> Assert.AreEqual<string>("a corollary TestScopeProofsAndCorollariesQualifiedName.TestTheorem2.TestTheorem2$1", qualifiedName corThm2 false) 
                | "lem2" -> Assert.AreEqual<string>("a lemma TestScopeProofsAndCorollariesQualifiedName.TestLemma2", qualifiedName lem2 false) 
                | "corLem2" -> Assert.AreEqual<string>("a corollary TestScopeProofsAndCorollariesQualifiedName.TestLemma2.TestLemma2$1", qualifiedName corLem2 false) 
                | "prp2" -> Assert.AreEqual<string>("a proposition TestScopeProofsAndCorollariesQualifiedName.TestProposition2", qualifiedName prp2 false) 
                | "corPrp2" -> Assert.AreEqual<string>("a corollary TestScopeProofsAndCorollariesQualifiedName.TestProposition2.TestProposition2$1", qualifiedName corPrp2 false) 
                | "cor2" -> Assert.AreEqual<string>("a corollary TestScopeProofsAndCorollariesQualifiedName.TestCorollary2$2", qualifiedName cor2 false) 
                | "corCor2" -> Assert.AreEqual<string>("a corollary TestScopeProofsAndCorollariesQualifiedName.TestCorollary2$2.TestCorollary2$2$1", qualifiedName corCor2 false) 
                | "con1" -> Assert.AreEqual<string>("a conjecture TestScopeProofsAndCorollariesQualifiedName.TestConjecture", qualifiedName con1 false) 
                | "corCon1" -> Assert.AreEqual<string>("a corollary TestScopeProofsAndCorollariesQualifiedName.TestConjecture.TestConjecture$1", qualifiedName corCon1 false) 
                | "axi1" -> Assert.AreEqual<string>("an axiom TestScopeProofsAndCorollariesQualifiedName.TestAxiom", qualifiedName axi1 false) 
                | "corAxi1"  -> Assert.AreEqual<string>("a corollary TestScopeProofsAndCorollariesQualifiedName.TestAxiom.TestAxiom$1", qualifiedName corAxi1 false)  
                | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
    [<DataRow("block")>]
    [<DataRow("t1")>]
    [<DataRow("t3")>]
    [<DataRow("t5")>]
    [<DataRow("t7")>]
    [<DataRow("t9")>]
    [<DataRow("t11")>]
    [<DataRow("t13")>]
    [<TestMethod>]
    member this.TestProperties(var) =
        let res = CommonFplValueTestCases.ScopeProperties("QualifiedName") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t3:FplValue,t5:FplValue,t7:FplValue,t9:FplValue,t11:FplValue,t13:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("a root ", qualifiedName r false) 
            | PrimTheoryL -> Assert.AreEqual<string>("a theory TestScopePropertiesQualifiedName", qualifiedName theory false) 
            | "block" -> Assert.AreEqual<string>("a predicate definition TestScopePropertiesQualifiedName.TestId()", qualifiedName block false) 
            | "t1" -> Assert.AreEqual<string>("a predicate property TestScopePropertiesQualifiedName.TestId().T1()", qualifiedName t1 false) 
            | "t3" -> Assert.AreEqual<string>("a functional term property TestScopePropertiesQualifiedName.TestId().T3() -> obj", qualifiedName t3 false) 
            | "t5" -> Assert.AreEqual<string>("a functional term property TestScopePropertiesQualifiedName.TestId().T5() -> ind", qualifiedName t5 false) 
            | "t7" -> Assert.AreEqual<string>("a functional term property TestScopePropertiesQualifiedName.TestId().T7() -> pred", qualifiedName t7 false) 
            | "t9" -> Assert.AreEqual<string>("a functional term property TestScopePropertiesQualifiedName.TestId().T9() -> tpl", qualifiedName t9 false) 
            | "t11" -> Assert.AreEqual<string>("a functional term property TestScopePropertiesQualifiedName.TestId().T11() -> Nat", qualifiedName t11 false) 
            | "t13" -> Assert.AreEqual<string>("a functional term property TestScopePropertiesQualifiedName.TestId().T13() -> func", qualifiedName t13 false) 
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
            | "r" -> Assert.AreEqual<string>("a root ", qualifiedName r false) 
            | PrimTheoryL -> Assert.AreEqual<string>("a theory TestScopeVariablesInBlockQualifiedName", qualifiedName theory false) 
            | "block" -> Assert.AreEqual<string>("a predicate definition TestScopeVariablesInBlockQualifiedName.TestPredicate()", qualifiedName block false) 
            | "x" -> Assert.AreEqual<string>("a variable x", qualifiedName x false) 
            | "y" -> Assert.AreEqual<string>("a variable y", qualifiedName y false) 
            | "s" -> Assert.AreEqual<string>("a variable s", qualifiedName s false) 
            | "xu" -> Assert.AreEqual<string>("a variable x.u", qualifiedName xu false) 
            | "xv" -> Assert.AreEqual<string>("a variable x.v", qualifiedName xv false) 
            | "xw" -> Assert.AreEqual<string>("a variable x.w", qualifiedName xw false) 
            | "yu" -> Assert.AreEqual<string>("a variable y.u", qualifiedName yu false) 
            | "yv" -> Assert.AreEqual<string>("a variable y.v", qualifiedName yv false) 
            | "yw" -> Assert.AreEqual<string>("a variable y.w", qualifiedName yw false) 
            | "xua" -> Assert.AreEqual<string>("a variable x.u.a", qualifiedName xua false) 
            | "xub" -> Assert.AreEqual<string>("a variable x.u.b", qualifiedName xub false) 
            | "xuc" -> Assert.AreEqual<string>("a variable x.u.c", qualifiedName xuc false) 
            | "xva" -> Assert.AreEqual<string>("a variable x.v.a", qualifiedName xva false) 
            | "xvb" -> Assert.AreEqual<string>("a variable x.v.b", qualifiedName xvb false) 
            | "xvc" -> Assert.AreEqual<string>("a variable x.v.c", qualifiedName xvc false) 
            | "xwa" -> Assert.AreEqual<string>("a variable x.w.a", qualifiedName xwa false) 
            | "xwb" -> Assert.AreEqual<string>("a variable x.w.b", qualifiedName xwb false) 
            | "xwc" -> Assert.AreEqual<string>("a variable x.w.c", qualifiedName xwc false) 
            | "yua" -> Assert.AreEqual<string>("a variable y.u.a", qualifiedName yua false) 
            | "yub" -> Assert.AreEqual<string>("a variable y.u.b", qualifiedName yub false) 
            | "yuc" -> Assert.AreEqual<string>("a variable y.u.c", qualifiedName yuc false) 
            | "yva" -> Assert.AreEqual<string>("a variable y.v.a", qualifiedName yva false) 
            | "yvb" -> Assert.AreEqual<string>("a variable y.v.b", qualifiedName yvb false) 
            | "yvc" -> Assert.AreEqual<string>("a variable y.v.c", qualifiedName yvc false) 
            | "ywa" -> Assert.AreEqual<string>("a variable y.w.a", qualifiedName ywa false) 
            | "ywb" -> Assert.AreEqual<string>("a variable y.w.b", qualifiedName ywb false) 
            | "ywc" -> Assert.AreEqual<string>("a variable y.w.c", qualifiedName ywc false) 
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
            | "r" -> Assert.AreEqual<string>("a root ", qualifiedName r false) 
            | PrimTheoryL -> Assert.AreEqual<string>("a theory TestScopeVariablesInBlockVariadicQualifiedName", qualifiedName theory false) 
            | "block" -> Assert.AreEqual<string>("a predicate definition TestScopeVariablesInBlockVariadicQualifiedName.TestPredicate()", qualifiedName block false) 
            | "x" -> Assert.AreEqual<string>("a variable array x", qualifiedName x false) 
            | "y" -> Assert.AreEqual<string>("a variable array y", qualifiedName y false) 
            | "xu" -> Assert.AreEqual<string>("a variable x.u", qualifiedName xu false) 
            | "xv" -> Assert.AreEqual<string>("a variable x.v", qualifiedName xv false) 
            | "xw" -> Assert.AreEqual<string>("a variable x.w", qualifiedName xw false) 
            | "yu" -> Assert.AreEqual<string>("a variable y.u", qualifiedName yu false) 
            | "yv" -> Assert.AreEqual<string>("a variable y.v", qualifiedName yv false) 
            | "yw" -> Assert.AreEqual<string>("a variable y.w", qualifiedName yw false) 
            | "xua" -> Assert.AreEqual<string>("a variable array x.u.a", qualifiedName xua false) 
            | "xub" -> Assert.AreEqual<string>("a variable array x.u.b", qualifiedName xub false) 
            | "xuc" -> Assert.AreEqual<string>("a variable array x.u.c", qualifiedName xuc false) 
            | "xva" -> Assert.AreEqual<string>("a variable array x.v.a", qualifiedName xva false) 
            | "xvb" -> Assert.AreEqual<string>("a variable array x.v.b", qualifiedName xvb false) 
            | "xvc" -> Assert.AreEqual<string>("a variable array x.v.c", qualifiedName xvc false) 
            | "xwa" -> Assert.AreEqual<string>("a variable array x.w.a", qualifiedName xwa false) 
            | "xwb" -> Assert.AreEqual<string>("a variable array x.w.b", qualifiedName xwb false) 
            | "xwc" -> Assert.AreEqual<string>("a variable array x.w.c", qualifiedName xwc false) 
            | "yua" -> Assert.AreEqual<string>("a variable array y.u.a", qualifiedName yua false) 
            | "yub" -> Assert.AreEqual<string>("a variable array y.u.b", qualifiedName yub false) 
            | "yuc" -> Assert.AreEqual<string>("a variable array y.u.c", qualifiedName yuc false) 
            | "yva" -> Assert.AreEqual<string>("a variable array y.v.a", qualifiedName yva false) 
            | "yvb" -> Assert.AreEqual<string>("a variable array y.v.b", qualifiedName yvb false) 
            | "yvc" -> Assert.AreEqual<string>("a variable array y.v.c", qualifiedName yvc false) 
            | "ywa" -> Assert.AreEqual<string>("a variable array y.w.a", qualifiedName ywa false) 
            | "ywb" -> Assert.AreEqual<string>("a variable array y.w.b", qualifiedName ywb false) 
            | "ywc" -> Assert.AreEqual<string>("a variable array y.w.c", qualifiedName ywc false) 
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
            | "r" -> Assert.AreEqual<string>("a root ", qualifiedName r false) 
            | PrimTheoryL -> Assert.AreEqual<string>("a theory TestScopeVariablesInSignatureQualifiedName", qualifiedName theory false) 
            | "block" -> Assert.AreEqual<string>("a predicate definition TestScopeVariablesInSignatureQualifiedName.TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", qualifiedName block false) 
            | "x" -> Assert.AreEqual<string>("a variable x", qualifiedName x false) 
            | "y" -> Assert.AreEqual<string>("a variable y", qualifiedName y false) 
            | "xu" -> Assert.AreEqual<string>("a variable x.u", qualifiedName xu false) 
            | "xv" -> Assert.AreEqual<string>("a variable x.v", qualifiedName xv false) 
            | "xw" -> Assert.AreEqual<string>("a variable x.w", qualifiedName xw false) 
            | "yu" -> Assert.AreEqual<string>("a variable y.u", qualifiedName yu false) 
            | "yv" -> Assert.AreEqual<string>("a variable y.v", qualifiedName yv false) 
            | "yw" -> Assert.AreEqual<string>("a variable y.w", qualifiedName yw false) 
            | "xua" -> Assert.AreEqual<string>("a variable x.u.a", qualifiedName xua false) 
            | "xub" -> Assert.AreEqual<string>("a variable x.u.b", qualifiedName xub false) 
            | "xuc" -> Assert.AreEqual<string>("a variable x.u.c", qualifiedName xuc false) 
            | "xva" -> Assert.AreEqual<string>("a variable x.v.a", qualifiedName xva false) 
            | "xvb" -> Assert.AreEqual<string>("a variable x.v.b", qualifiedName xvb false) 
            | "xvc" -> Assert.AreEqual<string>("a variable x.v.c", qualifiedName xvc false) 
            | "xwa" -> Assert.AreEqual<string>("a variable x.w.a", qualifiedName xwa false) 
            | "xwb" -> Assert.AreEqual<string>("a variable x.w.b", qualifiedName xwb false) 
            | "xwc" -> Assert.AreEqual<string>("a variable x.w.c", qualifiedName xwc false) 
            | "yua" -> Assert.AreEqual<string>("a variable y.u.a", qualifiedName yua false) 
            | "yub" -> Assert.AreEqual<string>("a variable y.u.b", qualifiedName yub false) 
            | "yuc" -> Assert.AreEqual<string>("a variable y.u.c", qualifiedName yuc false) 
            | "yva" -> Assert.AreEqual<string>("a variable y.v.a", qualifiedName yva false) 
            | "yvb" -> Assert.AreEqual<string>("a variable y.v.b", qualifiedName yvb false) 
            | "yvc" -> Assert.AreEqual<string>("a variable y.v.c", qualifiedName yvc false) 
            | "ywa" -> Assert.AreEqual<string>("a variable y.w.a", qualifiedName ywa false) 
            | "ywb" -> Assert.AreEqual<string>("a variable y.w.b", qualifiedName ywb false) 
            | "ywc" -> Assert.AreEqual<string>("a variable y.w.c", qualifiedName ywc false) 
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
            | "r" -> Assert.AreEqual<string>("a root ", qualifiedName r false) 
            | PrimTheoryL -> Assert.AreEqual<string>("a theory TestScopeVariablesInSignatureVariadicQualifiedName", qualifiedName theory false) 
            | "block" -> Assert.AreEqual<string>("a predicate definition TestScopeVariablesInSignatureVariadicQualifiedName.TestPredicate(*pred(func(*obj[ind], *obj[ind], *obj[ind]) -> obj, func(*obj[ind], *obj[ind], *obj[ind]) -> obj, func(*obj[ind], *obj[ind], *obj[ind]) -> obj)[obj], *pred(func(*obj[ind], *obj[ind], *obj[ind]) -> obj, func(*obj[ind], *obj[ind], *obj[ind]) -> obj, func(*obj[ind], *obj[ind], *obj[ind]) -> obj)[obj])", qualifiedName block false) 
            | "x" -> Assert.AreEqual<string>("a variable array x", qualifiedName x false) 
            | "y" -> Assert.AreEqual<string>("a variable array y", qualifiedName y false) 
            | "xu" -> Assert.AreEqual<string>("a variable x.u", qualifiedName xu false) 
            | "xv" -> Assert.AreEqual<string>("a variable x.v", qualifiedName xv false) 
            | "xw" -> Assert.AreEqual<string>("a variable x.w", qualifiedName xw false) 
            | "yu" -> Assert.AreEqual<string>("a variable y.u", qualifiedName yu false) 
            | "yv" -> Assert.AreEqual<string>("a variable y.v", qualifiedName yv false) 
            | "yw" -> Assert.AreEqual<string>("a variable y.w", qualifiedName yw false) 
            | "xua" -> Assert.AreEqual<string>("a variable array x.u.a", qualifiedName xua false) 
            | "xub" -> Assert.AreEqual<string>("a variable array x.u.b", qualifiedName xub false) 
            | "xuc" -> Assert.AreEqual<string>("a variable array x.u.c", qualifiedName xuc false) 
            | "xva" -> Assert.AreEqual<string>("a variable array x.v.a", qualifiedName xva false) 
            | "xvb" -> Assert.AreEqual<string>("a variable array x.v.b", qualifiedName xvb false) 
            | "xvc" -> Assert.AreEqual<string>("a variable array x.v.c", qualifiedName xvc false) 
            | "xwa" -> Assert.AreEqual<string>("a variable array x.w.a", qualifiedName xwa false) 
            | "xwb" -> Assert.AreEqual<string>("a variable array x.w.b", qualifiedName xwb false) 
            | "xwc" -> Assert.AreEqual<string>("a variable array x.w.c", qualifiedName xwc false) 
            | "yua" -> Assert.AreEqual<string>("a variable array y.u.a", qualifiedName yua false) 
            | "yub" -> Assert.AreEqual<string>("a variable array y.u.b", qualifiedName yub false) 
            | "yuc" -> Assert.AreEqual<string>("a variable array y.u.c", qualifiedName yuc false) 
            | "yva" -> Assert.AreEqual<string>("a variable array y.v.a", qualifiedName yva false) 
            | "yvb" -> Assert.AreEqual<string>("a variable array y.v.b", qualifiedName yvb false) 
            | "yvc" -> Assert.AreEqual<string>("a variable array y.v.c", qualifiedName yvc false) 
            | "ywa" -> Assert.AreEqual<string>("a variable array y.w.a", qualifiedName ywa false) 
            | "ywb" -> Assert.AreEqual<string>("a variable array y.w.b", qualifiedName ywb false) 
            | "ywc" -> Assert.AreEqual<string>("a variable array y.w.c", qualifiedName ywc false) 
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
            | "base1" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base2" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base3" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base4" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().-(1)", qualifiedName base1 false) 
            | "base5" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().Test()", qualifiedName base1 false) 
            | "base6" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base7" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().Test$1(x)", qualifiedName base1 false) 
            | "base8" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base9" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base10" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().Test", qualifiedName base1 false) 
            | "base11" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base12" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base13" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().1", qualifiedName base1 false) 
            | "base11a" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().v.x", qualifiedName base1 false) 
            | "base12a" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base10b" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base11b" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().v()", qualifiedName base1 false) 
            | "base12b" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base13b" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().1()", qualifiedName base1 false) 
            | "base10c" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base11c" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().v(x, y)", qualifiedName base1 false) 
            | "base12c" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base13c" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().1(x, y)", qualifiedName base1 false) 
            | "base10d" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base11d" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().v[x, y]", qualifiedName base1 false) 
            | "base12d" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base13d" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().1[x.y]", qualifiedName base1 false) 
            | "base10e" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base11e" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().v(x, y).x[a, b]", qualifiedName base1 false) 
            | "base12e" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base13e" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().1(x, y).T[a, b]", qualifiedName base1 false) 
            | "base10f" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().Test[x, y].x(a, b)", qualifiedName base1 false) 
            | "base11f" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().v[x, y].x(a, b)", qualifiedName base1 false) 
            | "base12f" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base13f" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().1[x.y].T(a, b)", qualifiedName base1 false) 
            | "base14" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base15" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().-(x)", qualifiedName base1 false) 
            | "base15a" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().'(x)", qualifiedName base1 false) 
            | "base15b" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().'(-(x))", qualifiedName base1 false) 
            | "base16" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().-(*(=(+(y, x), 2), x))", qualifiedName base1 false) 
            | "base17" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().'(*(=(+(y, '(x)), 2), x))", qualifiedName base1 false) 
            | "base18" -> Assert.AreEqual<string>("an exists quantor TestPredicateQualifiedName.T1().ex(x(T), y, z)", qualifiedName base1 false) 
            | "base19" -> Assert.AreEqual<string>("an exists n times quantor TestPredicateQualifiedName.T1().exn$1(x)" , qualifiedName base1 false) 
            | "base20" -> Assert.AreEqual<string>("an all quantor TestPredicateQualifiedName.T1().all(x)", qualifiedName base1 false) 
            | "base21" -> Assert.AreEqual<string>("a conjunction TestPredicateQualifiedName.T1().and(x, abc(y, z))", qualifiedName base1 false) 
            | "base21a" -> Assert.AreEqual<string>("a negation TestPredicateQualifiedName.T1().not(x)", qualifiedName base1 false) 
            | "base21b" -> Assert.AreEqual<string>("a negation TestPredicateQualifiedName.T1().not(x)", qualifiedName base1 false) 
            | "base22" -> Assert.AreEqual<string>("an exclusive disjunction TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base23" -> Assert.AreEqual<string>("a disjunction TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base24" -> Assert.AreEqual<string>("an equivalence TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base25" -> Assert.AreEqual<string>("an implication TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base26" -> Assert.AreEqual<string>("an is operator TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base27" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base28" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base29" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base30" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base31" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base32" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
            | "base33" -> Assert.AreEqual<string>("a reference TestPredicateQualifiedName.T1().p(c)", qualifiedName base1 false) 
            | "base34" -> Assert.AreEqual<string>("an is operator TestPredicateQualifiedName.T1()." + varVal, qualifiedName base1 false) 
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
    member this.TestBaseConstructorCall(var, varVal) =
        ad.Clear()
        let fplCode = sprintf """
                        def cl B {intr}
                        def cl C {intr}
                        def cl D {intr}

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
        let filename = "TestBaseConstructorCallQualifiedName"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let cl = theory.Scope["A"]
            let ctor = cl.Scope["A(T1, func, ind, pred)"]
            let base1 = ctor.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("a base constructor call TestBaseConstructorCallQualifiedName.A.A(T1, func, ind, pred).B()", qualifiedName base1 false) 
            | "base2" -> Assert.AreEqual<string>("a base constructor call TestBaseConstructorCallQualifiedName.A.A(T1, func, ind, pred).C(T1, func, ind, pred)", qualifiedName base1 false) 
            | "base3" -> Assert.AreEqual<string>("a base constructor call TestBaseConstructorCallQualifiedName.A.A(T1, func, ind, pred).D(undef, T1, func)", qualifiedName base1 false) 
            | "base4" -> Assert.AreEqual<string>("a base constructor call TestBaseConstructorCallQualifiedName.A.A(T1, func, ind, pred).B(In(undef))", qualifiedName base1 false) 
            | "base5" -> Assert.AreEqual<string>("a base constructor call TestBaseConstructorCallQualifiedName.A.A(T1, func, ind, pred).C(Test1(T1), Test2(func, ind, pred))", qualifiedName base1 false) 
            | "base6" -> Assert.AreEqual<string>("a base constructor call TestBaseConstructorCallQualifiedName.A.A(T1, func, ind, pred).E(pred, undef, pred)", qualifiedName base1 false) 
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
        let fplCode = sprintf "def pred T1() { dec ~a:T1 ~b:pred ~c:Nat ~d:ind; %s };" varVal
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
            | "base1" -> Assert.AreEqual<string>("a reference TestDelegateQualifiedName.T1().B()", qualifiedName base1 false) 
            | "base2" -> Assert.AreEqual<string>("a reference TestDelegateQualifiedName.T1().C(a, b, c, d)", qualifiedName base1 false) 
            | "base3" -> Assert.AreEqual<string>("a reference TestDelegateQualifiedName.T1().D(self, b, c)", qualifiedName base1 false) 
            | "base4" -> Assert.AreEqual<string>("a reference TestDelegateQualifiedName.T1().B(In(x))", qualifiedName base1 false) 
            | "base5" -> Assert.AreEqual<string>("a reference TestDelegateQualifiedName.T1().Test()", qualifiedName base1 false) 
            | "base6" -> Assert.AreEqual<string>("a reference TestDelegateQualifiedName.T1().C(Test1(a), Test2(b, c, d))", qualifiedName base1 false) 
            | "base7" -> Assert.AreEqual<string>("a reference TestDelegateQualifiedName.T1().E(true, undef, false)", qualifiedName base1 false) 
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """def pred T1() {intr};""")>]
    [<DataRow("base2", """def pred T1 () infix ">" -1 {intr};""")>]
    [<DataRow("base3", """def pred T1 () postfix "'" {intr};""")>]
    [<DataRow("base4", """def pred T1 () prefix "-" {intr};""")>]
    [<DataRow("base5", """def cl T1 symbol "∅" {intr};""")>]
    [<DataRow("base5a", """def cl T1 {intr};""")>]
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
            | "base1" -> Assert.AreEqual<string>("a predicate definition TestFixNotationQualifiedName.T1()", qualifiedName base1 false) 
            | "base2" -> Assert.AreEqual<string>("a predicate definition TestFixNotationQualifiedName.T1()", qualifiedName base1 false) 
            | "base3" -> Assert.AreEqual<string>("a predicate definition TestFixNotationQualifiedName.T1()", qualifiedName base1 false) 
            | "base4" -> Assert.AreEqual<string>("a predicate definition TestFixNotationQualifiedName.T1()", qualifiedName base1 false) 
            | "base5" -> Assert.AreEqual<string>("a class definition TestFixNotationQualifiedName.T1", qualifiedName base1 false) 
            | "base5a" -> Assert.AreEqual<string>("a class definition TestFixNotationQualifiedName.T1", qualifiedName base1 false) 
            | "base6" -> Assert.AreEqual<string>("a functional term definition TestFixNotationQualifiedName.T1() -> obj", qualifiedName base1 false) 
            | "base7" -> Assert.AreEqual<string>("a functional term definition TestFixNotationQualifiedName.T1() -> obj", qualifiedName base1 false) 
            | "base8" -> Assert.AreEqual<string>("a functional term definition TestFixNotationQualifiedName.T1() -> obj", qualifiedName base1 false) 
            | "base9" -> Assert.AreEqual<string>("a functional term definition TestFixNotationQualifiedName.T1() -> obj", qualifiedName base1 false) 
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """def func T()->obj {intr};""")>]
    [<DataRow("base2", """def func T()->ind {intr};""")>]
    [<DataRow("base3", """def func T()->func {intr};""")>]
    [<DataRow("base4", """def func T()->pred {intr};""")>]
    [<DataRow("base5", """def cl A {intr} def func T()->A {intr};""")>]
    [<DataRow("base6", """def func T()->pred(z:ind) {intr};""")>]
    [<DataRow("base7", """def func T()->pred(z:*obj[ind]) {intr};""")>]
    [<DataRow("base8", """def func T()->func(p:*pred(x:obj)[ind])->pred(x:ind) {intr};""")>]
    [<DataRow("base9", """def func T()->pred(f:*func(x:A)->A[ind]) {intr};""")>]
    [<DataRow("base10", """def cl A {intr} def func T()->pred(f:func(x:A)->A) {intr};""")>]
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
            | "base1" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> obj.", qualifiedName mapping false) 
            | "base2" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> ind.", qualifiedName mapping false) 
            | "base3" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> func.", qualifiedName mapping false) 
            | "base4" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> pred.", qualifiedName mapping false) 
            | "base5" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> A.", qualifiedName mapping false) 
            | "base6" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> pred(ind).", qualifiedName mapping false) 
            | "base7" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> pred(*obj[ind]).", qualifiedName mapping false) 
            | "base8" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> func(*pred(obj)[ind]) -> pred(ind).", qualifiedName mapping false) 
            | "base9" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> pred(*func(A) -> A[ind]).", qualifiedName mapping false) 
            | "base10" -> Assert.AreEqual<string>("a mapping TestMappingQualifiedName.T() -> pred(func(A) -> A).", qualifiedName mapping false) 
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
            | "base1" -> Assert.AreEqual<string>("an argument TestArgumentQualifiedName.T$1.100", qualifiedName arg false) 
            | "base2" -> Assert.AreEqual<string>("an argument TestArgumentQualifiedName.T$1.100", qualifiedName arg false) 
            | "base3" -> Assert.AreEqual<string>("an argument TestArgumentQualifiedName.T$1.100", qualifiedName arg false) 
            | "base4" -> Assert.AreEqual<string>("an argument TestArgumentQualifiedName.T$1.100", qualifiedName arg false) 
            | "base5" -> Assert.AreEqual<string>("an argument TestArgumentQualifiedName.T$1.100", qualifiedName arg false) 
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
            | "base0" -> Assert.AreEqual<string>("a language TestLanguageQualifiedName.true.tex", qualifiedName lang false) 
            | "base1" -> Assert.AreEqual<string>("a language TestLanguageQualifiedName.iif(x, y).tex", qualifiedName lang false) 
            | "base2" -> Assert.AreEqual<string>("a language TestLanguageQualifiedName.not(x).tex", qualifiedName lang false) 
            | "base3" -> Assert.AreEqual<string>("a language TestLanguageQualifiedName.and(p, q).tex", qualifiedName lang false) 
            | "base4" -> Assert.AreEqual<string>("a language TestLanguageQualifiedName.Equal(x, y).tex", qualifiedName lang false) 
            | "base5" -> Assert.AreEqual<string>("a language TestLanguageQualifiedName.NotEqual(x, y).tex", qualifiedName lang false) 
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
            | "base0" -> Assert.AreEqual<string>("a localization TestLocalizationQualifiedName." + predDecl, qualifiedName pred false) 
            | "base1" -> Assert.AreEqual<string>("a localization TestLocalizationQualifiedName." + predDecl, qualifiedName pred false) 
            | "base2" -> Assert.AreEqual<string>("a localization TestLocalizationQualifiedName." + predDecl, qualifiedName pred false) 
            | "base3" -> Assert.AreEqual<string>("a localization TestLocalizationQualifiedName." + predDecl, qualifiedName pred false) 
            | "base4" -> Assert.AreEqual<string>("a localization TestLocalizationQualifiedName." + predDecl, qualifiedName pred false) 
            | "base5" -> Assert.AreEqual<string>("a localization TestLocalizationQualifiedName." + predDecl, qualifiedName pred false) 
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
            | "base0" -> Assert.AreEqual<string>(@"a translation TestTranslationQualifiedName.true.tex.1", qualifiedName trsl false) 
            | "base1" -> Assert.AreEqual<string>(@"a translation TestTranslationQualifiedName.iif(x, y).tex.x", qualifiedName trsl false) 
            | "base2" -> Assert.AreEqual<string>(@"a translation TestTranslationQualifiedName.not(x).tex.\neg(", qualifiedName trsl false) 
            | "base3" -> Assert.AreEqual<string>(@"a translation TestTranslationQualifiedName.and(p, q).tex.p", qualifiedName trsl false) 
            | "base4" -> Assert.AreEqual<string>(@"a translation TestTranslationQualifiedName.Equal(x, y).tex.x", qualifiedName trsl false) 
            | "base5" -> Assert.AreEqual<string>(@"a translation TestTranslationQualifiedName.NotEqual(x, y).tex.x", qualifiedName trsl false) 
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
