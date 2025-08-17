namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplGrammarCommons
open FplInterpreterTypes
open CommonTestHelpers


[<TestClass>]
type TestFplValueScopeBlockType() =

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
        let res = CommonFplValueTestCases.ScopeBlocks("BlockType") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | "theory" -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "inf1" -> Assert.IsInstanceOfType<FplRuleOfInference>(inf1)
            | "inf2" -> Assert.IsInstanceOfType<FplRuleOfInference>(inf2)
            | "axi1" -> Assert.IsInstanceOfType<FplAxiom>(axi1)
            | "axi2" -> Assert.IsInstanceOfType<FplAxiom>(axi2)
            | "pst1" -> Assert.IsInstanceOfType<FplAxiom>(pst1)
            | "pst2" -> Assert.IsInstanceOfType<FplAxiom>(pst2)
            | "thm1" -> Assert.IsInstanceOfType<FplTheorem>(thm1)
            | "thm2" -> Assert.IsInstanceOfType<FplTheorem>(thm2)
            | "lem1" -> Assert.IsInstanceOfType<FplLemma>(lem1)
            | "lem2" -> Assert.IsInstanceOfType<FplLemma>(lem2)
            | "pro1" -> Assert.IsInstanceOfType<FplProposition>(pro1)
            | "pro2" -> Assert.IsInstanceOfType<FplProposition>(pro2)
            | "cor1" -> Assert.IsInstanceOfType<FplCorollary>(cor1)
            | "cor2" -> Assert.IsInstanceOfType<FplCorollary>(cor2)
            | "con1" -> Assert.IsInstanceOfType<FplConjecture>(con1)
            | "con2" -> Assert.IsInstanceOfType<FplConjecture>(con2)
            | "pre1" -> Assert.IsInstanceOfType<FplPredicate>(pre1)
            | "pre2" -> Assert.IsInstanceOfType<FplPredicate>(pre2)
            | "cla1" -> Assert.IsInstanceOfType<FplClass>(cla1)
            | "cla2" -> Assert.IsInstanceOfType<FplClass>(cla2)
            | "fun1" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun1)
            | "fun2" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun2)
            | "fun3" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun3)
            | "fun4" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun4)
            | "fun5" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun5)
            | "fun6" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun6)
            | "fun7" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun7)
            | "fun8" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun8)
            | "fun9" -> Assert.IsInstanceOfType<FplFunctionalTerm>(fun9)
            | "prf1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Proof, prf1.FplBlockType)
            | "prf2" -> Assert.AreEqual<FplBlockType>(FplBlockType.Proof, prf2.FplBlockType)
            | "loc1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Localization, loc1.FplBlockType)
            | "loc2" -> Assert.AreEqual<FplBlockType>(FplBlockType.Localization, loc2.FplBlockType)
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
        let res = CommonFplValueTestCases.ScopeConstructors("BlockType") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | "theory" -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplClass>(block)
            | "t1" -> Assert.IsInstanceOfType<FplConstructor>(t1)
            | "t2" -> Assert.IsInstanceOfType<FplConstructor>(t2)
            | "t3" -> Assert.IsInstanceOfType<FplConstructor>(t3)
            | "t4" -> Assert.IsInstanceOfType<FplConstructor>(t4)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("BlockType") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
                | "theory" -> Assert.IsInstanceOfType<FplTheory>(theory)
                | "thm1" -> Assert.IsInstanceOfType<FplTheorem>(thm1)
                | "proofThm1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Proof, proofThm1.FplBlockType)
                | "thm2" -> Assert.IsInstanceOfType<FplTheorem>(thm2)
                | "lem1" -> Assert.IsInstanceOfType<FplLemma>(lem1)
                | "proofLem1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Proof, proofLem1.FplBlockType)
                | "lem2" -> Assert.IsInstanceOfType<FplLemma>(lem2)
                | "cor1" -> Assert.IsInstanceOfType<FplCorollary>(cor1)
                | "proofCor1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Proof, proofCor1.FplBlockType)
                | "cor2" -> Assert.IsInstanceOfType<FplCorollary>(cor2)
                | "con1" -> Assert.IsInstanceOfType<FplConjecture>(con1)
                | "corCon1" -> Assert.IsInstanceOfType<FplCorollary>(corCon1)
                | "prp1" -> Assert.IsInstanceOfType<FplProposition>(prp1)
                | "proofPrp1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Proof, proofPrp1.FplBlockType)
                | "prp2" -> Assert.IsInstanceOfType<FplProposition>(prp2)
                | "corPrp2" -> Assert.IsInstanceOfType<FplCorollary>(corPrp2)
                | "corThm2" -> Assert.IsInstanceOfType<FplCorollary>(corThm2)
                | "corLem2" -> Assert.IsInstanceOfType<FplCorollary>(corLem2)
                | "corCor2" -> Assert.IsInstanceOfType<FplCorollary>(corCor2)
                | "axi1" -> Assert.IsInstanceOfType<FplAxiom>(axi1)
                | "corAxi1"  -> Assert.IsInstanceOfType<FplCorollary>(corAxi1) 
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
        let res = CommonFplValueTestCases.ScopeProperties("BlockType") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | "theory" -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "t1" -> Assert.IsInstanceOfType<FplMandatoryPredicate>(t1)
            | "t2" -> Assert.IsInstanceOfType<FplOptionalPredicate>(t2)
            | "t3" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t3)
            | "t4" -> Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(t4)
            | "t5" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t5)
            | "t6" -> Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(t6)
            | "t7" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t7)
            | "t8" -> Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(t8)
            | "t9" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t9)
            | "t10" -> Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(t10)
            | "t11" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t11)
            | "t12" -> Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(t12)
            | "t13" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t13)
            | "t14" -> Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(t14)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("s")>]
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("BlockType")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | "theory" -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "s" -> Assert.IsTrue(s.IsVariable() && not (s :?> FplVariable).IsVariadic)
            | "x" -> Assert.IsTrue(x.IsVariable() && not (x :?> FplVariable).IsVariadic)
            | "y" -> Assert.IsTrue(y.IsVariable() && not (y :?> FplVariable).IsVariadic)
            | "xu" -> Assert.IsTrue(xu.IsVariable() && not (xu :?> FplVariable).IsVariadic) 
            | "xv" -> Assert.IsTrue(xv.IsVariable() && not (xv :?> FplVariable).IsVariadic) 
            | "xw" -> Assert.IsTrue(xw.IsVariable() && not (xw :?> FplVariable).IsVariadic) 
            | "yu" -> Assert.IsTrue(yu.IsVariable() && not (yu :?> FplVariable).IsVariadic) 
            | "yv" -> Assert.IsTrue(yv.IsVariable() && not (yv :?> FplVariable).IsVariadic) 
            | "yw" -> Assert.IsTrue(yw.IsVariable() && not (yw :?> FplVariable).IsVariadic) 
            | "xua" -> Assert.IsTrue(xua.IsVariable() && not (xua :?> FplVariable).IsVariadic) 
            | "xub" -> Assert.IsTrue(xub.IsVariable() && not (xub :?> FplVariable).IsVariadic)
            | "xuc" -> Assert.IsTrue(xuc.IsVariable() && not (xuc :?> FplVariable).IsVariadic)
            | "xva" -> Assert.IsTrue(xva.IsVariable() && not (xva :?> FplVariable).IsVariadic)
            | "xvb" -> Assert.IsTrue(xvb.IsVariable() && not (xvb :?> FplVariable).IsVariadic)
            | "xvc" -> Assert.IsTrue(xvc.IsVariable() && not (xvc :?> FplVariable).IsVariadic)
            | "xwa" -> Assert.IsTrue(xwa.IsVariable() && not (xwa :?> FplVariable).IsVariadic)
            | "xwb" -> Assert.IsTrue(xwb.IsVariable() && not (xwb :?> FplVariable).IsVariadic)
            | "xwc" -> Assert.IsTrue(xwc.IsVariable() && not (xwc :?> FplVariable).IsVariadic)
            | "yua" -> Assert.IsTrue(yua.IsVariable() && not (yua :?> FplVariable).IsVariadic)
            | "yub" -> Assert.IsTrue(yub.IsVariable() && not (yub :?> FplVariable).IsVariadic)
            | "yuc" -> Assert.IsTrue(yuc.IsVariable() && not (yuc :?> FplVariable).IsVariadic)
            | "yva" -> Assert.IsTrue(yva.IsVariable() && not (yva :?> FplVariable).IsVariadic)
            | "yvb" -> Assert.IsTrue(yvb.IsVariable() && not (yvb :?> FplVariable).IsVariadic)
            | "yvc" -> Assert.IsTrue(yvc.IsVariable() && not (yvc :?> FplVariable).IsVariadic)
            | "ywa" -> Assert.IsTrue(ywa.IsVariable() && not (ywa :?> FplVariable).IsVariadic)
            | "ywb" -> Assert.IsTrue(ywb.IsVariable() && not (ywb :?> FplVariable).IsVariadic)
            | "ywc" -> Assert.IsTrue(ywc.IsVariable() && not (ywc :?> FplVariable).IsVariadic)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | "theory" -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "x" -> Assert.IsTrue(x.IsVariable() && (x :?> FplVariable).IsMany1)
            | "y" -> Assert.IsTrue(y.IsVariable() && (y :?> FplVariable).IsMany1)
            | "xu" -> Assert.IsTrue(xu.IsVariable() && not (xu :?> FplVariable).IsVariadic) 
            | "xv" -> Assert.IsTrue(xv.IsVariable() && not (xv :?> FplVariable).IsVariadic) 
            | "xw" -> Assert.IsTrue(xw.IsVariable() && not (xw :?> FplVariable).IsVariadic) 
            | "yu" -> Assert.IsTrue(yu.IsVariable() && not (yu :?> FplVariable).IsVariadic) 
            | "yv" -> Assert.IsTrue(yv.IsVariable() && not (yv :?> FplVariable).IsVariadic) 
            | "yw" -> Assert.IsTrue(yw.IsVariable() && not (yw :?> FplVariable).IsVariadic) 
            | "xua" -> Assert.IsTrue(xua.IsVariable() && (xua :?> FplVariable).IsMany) 
            | "xub" -> Assert.IsTrue(xub.IsVariable() && (xub :?> FplVariable).IsMany)
            | "xuc" -> Assert.IsTrue(xuc.IsVariable() && (xuc :?> FplVariable).IsMany)
            | "xva" -> Assert.IsTrue(xva.IsVariable() && (xva :?> FplVariable).IsMany)
            | "xvb" -> Assert.IsTrue(xvb.IsVariable() && (xvb :?> FplVariable).IsMany)
            | "xvc" -> Assert.IsTrue(xvc.IsVariable() && (xvc :?> FplVariable).IsMany)
            | "xwa" -> Assert.IsTrue(xwa.IsVariable() && (xwa :?> FplVariable).IsMany)
            | "xwb" -> Assert.IsTrue(xwb.IsVariable() && (xwb :?> FplVariable).IsMany)
            | "xwc" -> Assert.IsTrue(xwc.IsVariable() && (xwc :?> FplVariable).IsMany)
            | "yua" -> Assert.IsTrue(yua.IsVariable() && (yua :?> FplVariable).IsMany)
            | "yub" -> Assert.IsTrue(yub.IsVariable() && (yub :?> FplVariable).IsMany)
            | "yuc" -> Assert.IsTrue(yuc.IsVariable() && (yuc :?> FplVariable).IsMany)
            | "yva" -> Assert.IsTrue(yva.IsVariable() && (yva :?> FplVariable).IsMany)
            | "yvb" -> Assert.IsTrue(yvb.IsVariable() && (yvb :?> FplVariable).IsMany)
            | "yvc" -> Assert.IsTrue(yvc.IsVariable() && (yvc :?> FplVariable).IsMany)
            | "ywa" -> Assert.IsTrue(ywa.IsVariable() && (ywa :?> FplVariable).IsMany)
            | "ywb" -> Assert.IsTrue(ywb.IsVariable() && (ywb :?> FplVariable).IsMany)
            | "ywc" -> Assert.IsTrue(ywc.IsVariable() && (ywc :?> FplVariable).IsMany)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | "theory" -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "x" -> Assert.IsTrue(x.IsVariable() && not (x :?> FplVariable).IsVariadic)
            | "y" -> Assert.IsTrue(y.IsVariable() && not (y :?> FplVariable).IsVariadic)
            | "xu" -> Assert.IsTrue(xu.IsVariable() && not (xu :?> FplVariable).IsVariadic) 
            | "xv" -> Assert.IsTrue(xv.IsVariable() && not (xv :?> FplVariable).IsVariadic) 
            | "xw" -> Assert.IsTrue(xw.IsVariable() && not (xw :?> FplVariable).IsVariadic) 
            | "yu" -> Assert.IsTrue(yu.IsVariable() && not (yu :?> FplVariable).IsVariadic) 
            | "yv" -> Assert.IsTrue(yv.IsVariable() && not (yv :?> FplVariable).IsVariadic) 
            | "yw" -> Assert.IsTrue(yw.IsVariable() && not (yw :?> FplVariable).IsVariadic) 
            | "xua" -> Assert.IsTrue(xua.IsVariable() && not (xua :?> FplVariable).IsVariadic) 
            | "xub" -> Assert.IsTrue(xub.IsVariable() && not (xub :?> FplVariable).IsVariadic)
            | "xuc" -> Assert.IsTrue(xuc.IsVariable() && not (xuc :?> FplVariable).IsVariadic)
            | "xva" -> Assert.IsTrue(xva.IsVariable() && not (xva :?> FplVariable).IsVariadic)
            | "xvb" -> Assert.IsTrue(xvb.IsVariable() && not (xvb :?> FplVariable).IsVariadic)
            | "xvc" -> Assert.IsTrue(xvc.IsVariable() && not (xvc :?> FplVariable).IsVariadic)
            | "xwa" -> Assert.IsTrue(xwa.IsVariable() && not (xwa :?> FplVariable).IsVariadic)
            | "xwb" -> Assert.IsTrue(xwb.IsVariable() && not (xwb :?> FplVariable).IsVariadic)
            | "xwc" -> Assert.IsTrue(xwc.IsVariable() && not (xwc :?> FplVariable).IsVariadic)
            | "yua" -> Assert.IsTrue(yua.IsVariable() && not (yua :?> FplVariable).IsVariadic)
            | "yub" -> Assert.IsTrue(yub.IsVariable() && not (yub :?> FplVariable).IsVariadic)
            | "yuc" -> Assert.IsTrue(yuc.IsVariable() && not (yuc :?> FplVariable).IsVariadic)
            | "yva" -> Assert.IsTrue(yva.IsVariable() && not (yva :?> FplVariable).IsVariadic)
            | "yvb" -> Assert.IsTrue(yvb.IsVariable() && not (yvb :?> FplVariable).IsVariadic)
            | "yvc" -> Assert.IsTrue(yvc.IsVariable() && not (yvc :?> FplVariable).IsVariadic)
            | "ywa" -> Assert.IsTrue(ywa.IsVariable() && not (ywa :?> FplVariable).IsVariadic)
            | "ywb" -> Assert.IsTrue(ywb.IsVariable() && not (ywb :?> FplVariable).IsVariadic)
            | "ywc" -> Assert.IsTrue(ywc.IsVariable() && not (ywc :?> FplVariable).IsVariadic)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | "theory" -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "x" -> Assert.IsTrue(x.IsVariable() && (x :?> FplVariable).IsMany1)
            | "y" -> Assert.IsTrue(y.IsVariable() && (y :?> FplVariable).IsMany1)
            | "xu" -> Assert.IsTrue(xu.IsVariable() && not (xu :?> FplVariable).IsVariadic) 
            | "xv" -> Assert.IsTrue(xv.IsVariable() && not (xv :?> FplVariable).IsVariadic) 
            | "xw" -> Assert.IsTrue(xw.IsVariable() && not (xw :?> FplVariable).IsVariadic) 
            | "yu" -> Assert.IsTrue(yu.IsVariable() && not (yu :?> FplVariable).IsVariadic) 
            | "yv" -> Assert.IsTrue(yv.IsVariable() && not (yv :?> FplVariable).IsVariadic) 
            | "yw" -> Assert.IsTrue(yw.IsVariable() && not (yw :?> FplVariable).IsVariadic) 
            | "xua" -> Assert.IsTrue(xua.IsVariable() && (xua :?> FplVariable).IsMany) 
            | "xub" -> Assert.IsTrue(xub.IsVariable() && (xub :?> FplVariable).IsMany)
            | "xuc" -> Assert.IsTrue(xuc.IsVariable() && (xuc :?> FplVariable).IsMany)
            | "xva" -> Assert.IsTrue(xva.IsVariable() && (xva :?> FplVariable).IsMany)
            | "xvb" -> Assert.IsTrue(xvb.IsVariable() && (xvb :?> FplVariable).IsMany)
            | "xvc" -> Assert.IsTrue(xvc.IsVariable() && (xvc :?> FplVariable).IsMany)
            | "xwa" -> Assert.IsTrue(xwa.IsVariable() && (xwa :?> FplVariable).IsMany)
            | "xwb" -> Assert.IsTrue(xwb.IsVariable() && (xwb :?> FplVariable).IsMany)
            | "xwc" -> Assert.IsTrue(xwc.IsVariable() && (xwc :?> FplVariable).IsMany)
            | "yua" -> Assert.IsTrue(yua.IsVariable() && (yua :?> FplVariable).IsMany)
            | "yub" -> Assert.IsTrue(yub.IsVariable() && (yub :?> FplVariable).IsMany)
            | "yuc" -> Assert.IsTrue(yuc.IsVariable() && (yuc :?> FplVariable).IsMany)
            | "yva" -> Assert.IsTrue(yva.IsVariable() && (yva :?> FplVariable).IsMany)
            | "yvb" -> Assert.IsTrue(yvb.IsVariable() && (yvb :?> FplVariable).IsMany)
            | "yvc" -> Assert.IsTrue(yvc.IsVariable() && (yvc :?> FplVariable).IsMany)
            | "ywa" -> Assert.IsTrue(ywa.IsVariable() && (ywa :?> FplVariable).IsMany)
            | "ywb" -> Assert.IsTrue(ywb.IsVariable() && (ywb :?> FplVariable).IsMany)
            | "ywc" -> Assert.IsTrue(ywc.IsVariable() && (ywc :?> FplVariable).IsMany)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", literalTrue)>]
    [<DataRow("base2", literalFalse)>]
    [<DataRow("base3", literalUndef)>]
    [<DataRow("base4", "1.")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "$1")>]
    [<DataRow("base7", "bydef Test()")>] 
    [<DataRow("base8", "Test$1")>]
    [<DataRow("base9", "Test$1()")>]
    [<DataRow("base10", "Test")>]
    [<DataRow("base11", "v")>]
    [<DataRow("base12", literalSelf)>]
    [<DataRow("base13", "@1")>]
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
    [<DataRow("base21", "and (x, and(y, z))")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not (x)")>]
    [<DataRow("base22", "xor (x, xor(y, z))")>]
    [<DataRow("base23", "or (x, or(y, z))")>]
    [<DataRow("base24", "iif (x, y)")>]
    [<DataRow("base25", "impl (x, y)")>]
    [<DataRow("base26", "is (x, Nat)")>]
    [<DataRow("base27", "B()")>]
    [<DataRow("base28", "C(a,b,c,d)")>]
    [<DataRow("base29", "D(self,b,c)")>]
    [<DataRow("base30", "B(In(x))")>]
    [<DataRow("base31", "C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base32", "E(true, undef, false)")>]
    [<DataRow("base33", "dec ~p: pred(c: obj); p(c)")>]
    [<DataRow("base34", "is(x, Set)")>]
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestPredicateBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.IsInstanceOfType<FplIntrinsicPred>(base1)
            | "base2" -> Assert.IsInstanceOfType<FplIntrinsicPred>(base1)
            | "base3" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base4" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base5" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base6" -> Assert.IsInstanceOfType<FplIntrinsicInd>(base1)
            | "base7" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base8" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base9" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base10" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base11" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base12" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base13" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base11a" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base12a" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base10b" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base11b" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base12b" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base13b" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base10c" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base11c" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base12c" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base13c" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base10d" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base11d" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base12d" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base13d" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base10e" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base11e" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base12e" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base13e" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base10f" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base11f" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base12f" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base13f" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base14" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base15" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base15a" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base15b" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base16" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base17" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base18" -> Assert.IsInstanceOfType<FplQuantor>(base1) 
            | "base19" -> Assert.IsInstanceOfType<FplQuantor>(base1)  
            | "base20" -> Assert.IsInstanceOfType<FplQuantor>(base1) 
            | "base21" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base21a" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base21b" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base22" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base23" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base24" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base25" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base26" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base27" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base28" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base29" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base30" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base31" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base32" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base33" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base34" -> Assert.IsInstanceOfType<FplReference>(base1)
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
        let filename = "TestCallConstructorParentClassBlockType"
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
            | "base1" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base2" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base3" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base4" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base5" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base6" -> Assert.IsInstanceOfType<FplReference>(base1)
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
        let filename = "TestDelegateBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base2" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base3" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base4" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base5" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base6" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base7" -> Assert.IsInstanceOfType<FplReference>(base1)
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
        let filename = "TestFixNotationBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = 
                if varVal.Contains literalCl then 
                    theory.Scope["T1"]
                elif varVal.Contains literalFunc then 
                    theory.Scope["T1() -> obj"]
                else 
                    theory.Scope["T1()"]

            match var with
            | "base1" -> Assert.IsInstanceOfType<FplPredicate>(base1)
            | "base2" -> Assert.IsInstanceOfType<FplPredicate>(base1)
            | "base3" -> Assert.IsInstanceOfType<FplPredicate>(base1)
            | "base4" -> Assert.IsInstanceOfType<FplPredicate>(base1)
            | "base5" -> Assert.IsInstanceOfType<FplClass>(base1)
            | "base5a" -> Assert.IsInstanceOfType<FplClass>(base1)
            | "base6" -> Assert.IsInstanceOfType<FplFunctionalTerm>(base1) 
            | "base7" -> Assert.IsInstanceOfType<FplFunctionalTerm>(base1) 
            | "base8" -> Assert.IsInstanceOfType<FplFunctionalTerm>(base1) 
            | "base9" -> Assert.IsInstanceOfType<FplFunctionalTerm>(base1) 
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
        let filename = "TestMappingBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ArgList[0]
            match var with
            | "base1" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base2" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base3" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base4" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base5" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base6" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base7" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base8" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base9" -> Assert.IsInstanceOfType<FplMapping>(mapping)
            | "base10" -> Assert.IsInstanceOfType<FplMapping>(mapping)
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
        let filename = "TestArgumentBlockType"
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
            | "base1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Argument, arg.FplBlockType)
            | "base2" -> Assert.AreEqual<FplBlockType>(FplBlockType.Argument, arg.FplBlockType)
            | "base3" -> Assert.AreEqual<FplBlockType>(FplBlockType.Argument, arg.FplBlockType)
            | "base4" -> Assert.AreEqual<FplBlockType>(FplBlockType.Argument, arg.FplBlockType)
            | "base5" -> Assert.AreEqual<FplBlockType>(FplBlockType.Argument, arg.FplBlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", literalTrue, """!tex: "1" !eng: literalTrue !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLanguage(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestLanguageBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<FplBlockType>(FplBlockType.Language, lang.FplBlockType)
            | "base1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Language, lang.FplBlockType)
            | "base2" -> Assert.AreEqual<FplBlockType>(FplBlockType.Language, lang.FplBlockType)
            | "base3" -> Assert.AreEqual<FplBlockType>(FplBlockType.Language, lang.FplBlockType)
            | "base4" -> Assert.AreEqual<FplBlockType>(FplBlockType.Language, lang.FplBlockType)
            | "base5" -> Assert.AreEqual<FplBlockType>(FplBlockType.Language, lang.FplBlockType)
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
        let filename = "TestLocalizationBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Localization, pred.FplBlockType)
            | "base2" -> Assert.AreEqual<FplBlockType>(FplBlockType.Localization, pred.FplBlockType)
            | "base3" -> Assert.AreEqual<FplBlockType>(FplBlockType.Localization, pred.FplBlockType)
            | "base4" -> Assert.AreEqual<FplBlockType>(FplBlockType.Localization, pred.FplBlockType)
            | "base5" -> Assert.AreEqual<FplBlockType>(FplBlockType.Localization, pred.FplBlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", literalTrue, """!tex: "1" !eng: literalTrue !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x " \Leftrightarrow " y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p " \wedge " q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq " y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestTranslation(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestTranslationBlockType"
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
            | "base0" -> Assert.AreEqual<FplBlockType>(FplBlockType.Translation, trsl.FplBlockType)
            | "base1" -> Assert.AreEqual<FplBlockType>(FplBlockType.Translation, trsl.FplBlockType)
            | "base2" -> Assert.AreEqual<FplBlockType>(FplBlockType.Translation, trsl.FplBlockType)
            | "base3" -> Assert.AreEqual<FplBlockType>(FplBlockType.Translation, trsl.FplBlockType)
            | "base4" -> Assert.AreEqual<FplBlockType>(FplBlockType.Translation, trsl.FplBlockType)
            | "base5" -> Assert.AreEqual<FplBlockType>(FplBlockType.Translation, trsl.FplBlockType)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
