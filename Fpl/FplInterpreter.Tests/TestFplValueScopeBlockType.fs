namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers


[<TestClass>]
type TestFplValueScopeBlockType() =

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
        let res = CommonFplValueTestCases.ScopeBlocks("BlockType") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | PrimTheoryL -> Assert.IsInstanceOfType<FplTheory>(theory)
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
            | "prf1" -> Assert.IsInstanceOfType<FplProof>(prf1)
            | "prf2" -> Assert.IsInstanceOfType<FplProof>(prf2)
            | "loc1" -> Assert.IsInstanceOfType<FplLocalization>(loc1)
            | "loc2" -> Assert.IsInstanceOfType<FplLocalization>(loc2)
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
        let res = CommonFplValueTestCases.ScopeConstructors("BlockType") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | PrimTheoryL -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplClass>(block)
            | "t1" -> Assert.IsInstanceOfType<FplConstructor>(t1)
            | "t2" -> Assert.IsInstanceOfType<FplConstructor>(t2)
            | "t3" -> Assert.IsInstanceOfType<FplConstructor>(t3)
            | "t4" -> Assert.IsInstanceOfType<FplConstructor>(t4)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("BlockType") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
                | PrimTheoryL -> Assert.IsInstanceOfType<FplTheory>(theory)
                | "thm1" -> Assert.IsInstanceOfType<FplTheorem>(thm1)
                | "proofThm1" -> Assert.IsInstanceOfType<FplProof>(proofThm1)
                | "thm2" -> Assert.IsInstanceOfType<FplTheorem>(thm2)
                | "lem1" -> Assert.IsInstanceOfType<FplLemma>(lem1)
                | "proofLem1" -> Assert.IsInstanceOfType<FplProof>(proofLem1)
                | "lem2" -> Assert.IsInstanceOfType<FplLemma>(lem2)
                | "cor1" -> Assert.IsInstanceOfType<FplCorollary>(cor1)
                | "proofCor1" -> Assert.IsInstanceOfType<FplProof>(proofCor1)
                | "cor2" -> Assert.IsInstanceOfType<FplCorollary>(cor2)
                | "con1" -> Assert.IsInstanceOfType<FplConjecture>(con1)
                | "corCon1" -> Assert.IsInstanceOfType<FplCorollary>(corCon1)
                | "prp1" -> Assert.IsInstanceOfType<FplProposition>(prp1)
                | "proofPrp1" -> Assert.IsInstanceOfType<FplProof>(proofPrp1)
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
        let res = CommonFplValueTestCases.ScopeProperties("BlockType") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t3:FplValue,t5:FplValue,t7:FplValue,t9:FplValue,t11:FplValue,t13:FplValue) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | PrimTheoryL -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "t1" -> Assert.IsInstanceOfType<FplMandatoryPredicate>(t1)
            | "t3" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t3)
            | "t5" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t5)
            | "t7" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t7)
            | "t9" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t9)
            | "t11" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t11)
            | "t13" -> Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(t13)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
            | PrimTheoryL -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "s" -> Assert.IsInstanceOfType<FplVariable>(s)
            | "x" -> Assert.IsInstanceOfType<FplVariable>(x)
            | "y" -> Assert.IsInstanceOfType<FplVariable>(y)
            | "xu" -> Assert.IsInstanceOfType<FplVariable>(xu) 
            | "xv" -> Assert.IsInstanceOfType<FplVariable>(xv)
            | "xw" -> Assert.IsInstanceOfType<FplVariable>(xw) 
            | "yu" -> Assert.IsInstanceOfType<FplVariable>(yu) 
            | "yv" -> Assert.IsInstanceOfType<FplVariable>(yv) 
            | "yw" -> Assert.IsInstanceOfType<FplVariable>(yw) 
            | "xua" -> Assert.IsInstanceOfType<FplVariable>(xua)
            | "xub" -> Assert.IsInstanceOfType<FplVariable>(xub)
            | "xuc" -> Assert.IsInstanceOfType<FplVariable>(xuc)
            | "xva" -> Assert.IsInstanceOfType<FplVariable>(xva)
            | "xvb" -> Assert.IsInstanceOfType<FplVariable>(xvb)
            | "xvc" -> Assert.IsInstanceOfType<FplVariable>(xvc)
            | "xwa" -> Assert.IsInstanceOfType<FplVariable>(xwa)
            | "xwb" -> Assert.IsInstanceOfType<FplVariable>(xwb)
            | "xwc" -> Assert.IsInstanceOfType<FplVariable>(xwc)
            | "yua" -> Assert.IsInstanceOfType<FplVariable>(yua)
            | "yub" -> Assert.IsInstanceOfType<FplVariable>(yub)
            | "yuc" -> Assert.IsInstanceOfType<FplVariable>(yuc)
            | "yva" -> Assert.IsInstanceOfType<FplVariable>(yva)
            | "yvb" -> Assert.IsInstanceOfType<FplVariable>(yvb)
            | "yvc" -> Assert.IsInstanceOfType<FplVariable>(yvc)
            | "ywa" -> Assert.IsInstanceOfType<FplVariable>(ywa)
            | "ywb" -> Assert.IsInstanceOfType<FplVariable>(ywb)
            | "ywc" -> Assert.IsInstanceOfType<FplVariable>(ywc)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | PrimTheoryL -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "x" -> Assert.IsInstanceOfType<FplVariableArray>(x)
            | "y" -> Assert.IsInstanceOfType<FplVariableArray>(y)
            | "xu" -> Assert.IsInstanceOfType<FplVariable>(xu) 
            | "xv" -> Assert.IsInstanceOfType<FplVariable>(xv)
            | "xw" -> Assert.IsInstanceOfType<FplVariable>(xw) 
            | "yu" -> Assert.IsInstanceOfType<FplVariable>(yu) 
            | "yv" -> Assert.IsInstanceOfType<FplVariable>(yv) 
            | "yw" -> Assert.IsInstanceOfType<FplVariable>(yw) 
            | "xua" -> Assert.IsInstanceOfType<FplVariableArray>(xua) 
            | "xub" -> Assert.IsInstanceOfType<FplVariableArray>(xub)
            | "xuc" -> Assert.IsInstanceOfType<FplVariableArray>(xuc)
            | "xva" -> Assert.IsInstanceOfType<FplVariableArray>(xva)
            | "xvb" -> Assert.IsInstanceOfType<FplVariableArray>(xvb)
            | "xvc" -> Assert.IsInstanceOfType<FplVariableArray>(xvc)
            | "xwa" -> Assert.IsInstanceOfType<FplVariableArray>(xwa)
            | "xwb" -> Assert.IsInstanceOfType<FplVariableArray>(xwb)
            | "xwc" -> Assert.IsInstanceOfType<FplVariableArray>(xwc)
            | "yua" -> Assert.IsInstanceOfType<FplVariableArray>(yua)
            | "yub" -> Assert.IsInstanceOfType<FplVariableArray>(yub)
            | "yuc" -> Assert.IsInstanceOfType<FplVariableArray>(yuc)
            | "yva" -> Assert.IsInstanceOfType<FplVariableArray>(yva)
            | "yvb" -> Assert.IsInstanceOfType<FplVariableArray>(yvb)
            | "yvc" -> Assert.IsInstanceOfType<FplVariableArray>(yvc)
            | "ywa" -> Assert.IsInstanceOfType<FplVariableArray>(ywa)
            | "ywb" -> Assert.IsInstanceOfType<FplVariableArray>(ywb)
            | "ywc" -> Assert.IsInstanceOfType<FplVariableArray>(ywc)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | PrimTheoryL -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "x" -> Assert.IsInstanceOfType<FplVariable>(x)
            | "y" -> Assert.IsInstanceOfType<FplVariable>(y)
            | "xu" -> Assert.IsInstanceOfType<FplVariable>(xu) 
            | "xv" -> Assert.IsInstanceOfType<FplVariable>(xv) 
            | "xw" -> Assert.IsInstanceOfType<FplVariable>(xw) 
            | "yu" -> Assert.IsInstanceOfType<FplVariable>(yu) 
            | "yv" -> Assert.IsInstanceOfType<FplVariable>(yv) 
            | "yw" -> Assert.IsInstanceOfType<FplVariable>(yw) 
            | "xua" -> Assert.IsInstanceOfType<FplVariable>(xua)
            | "xub" -> Assert.IsInstanceOfType<FplVariable>(xub)
            | "xuc" -> Assert.IsInstanceOfType<FplVariable>(xuc)
            | "xva" -> Assert.IsInstanceOfType<FplVariable>(xva)
            | "xvb" -> Assert.IsInstanceOfType<FplVariable>(xvb)
            | "xvc" -> Assert.IsInstanceOfType<FplVariable>(xvc)
            | "xwa" -> Assert.IsInstanceOfType<FplVariable>(xwa)
            | "xwb" -> Assert.IsInstanceOfType<FplVariable>(xwb)
            | "xwc" -> Assert.IsInstanceOfType<FplVariable>(xwc)
            | "yua" -> Assert.IsInstanceOfType<FplVariable>(yua)
            | "yub" -> Assert.IsInstanceOfType<FplVariable>(yub)
            | "yuc" -> Assert.IsInstanceOfType<FplVariable>(yuc)
            | "yva" -> Assert.IsInstanceOfType<FplVariable>(yva)
            | "yvb" -> Assert.IsInstanceOfType<FplVariable>(yvb)
            | "yvc" -> Assert.IsInstanceOfType<FplVariable>(yvc)
            | "ywa" -> Assert.IsInstanceOfType<FplVariable>(ywa)
            | "ywb" -> Assert.IsInstanceOfType<FplVariable>(ywb)
            | "ywc" -> Assert.IsInstanceOfType<FplVariable>(ywc)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType<FplRoot>(r)
            | PrimTheoryL -> Assert.IsInstanceOfType<FplTheory>(theory)
            | "block" -> Assert.IsInstanceOfType<FplPredicate>(block)
            | "x" -> Assert.IsInstanceOfType<FplVariableArray>(x)
            | "y" -> Assert.IsInstanceOfType<FplVariableArray>(y)
            | "xu" -> Assert.IsInstanceOfType<FplVariable>(xu) 
            | "xv" -> Assert.IsInstanceOfType<FplVariable>(xv)
            | "xw" -> Assert.IsInstanceOfType<FplVariable>(xw) 
            | "yu" -> Assert.IsInstanceOfType<FplVariable>(yu) 
            | "yv" -> Assert.IsInstanceOfType<FplVariable>(yv) 
            | "yw" -> Assert.IsInstanceOfType<FplVariable>(yw) 
            | "xua" -> Assert.IsInstanceOfType<FplVariableArray>(xua) 
            | "xub" -> Assert.IsInstanceOfType<FplVariableArray>(xub)
            | "xuc" -> Assert.IsInstanceOfType<FplVariableArray>(xuc)
            | "xva" -> Assert.IsInstanceOfType<FplVariableArray>(xva)
            | "xvb" -> Assert.IsInstanceOfType<FplVariableArray>(xvb)
            | "xvc" -> Assert.IsInstanceOfType<FplVariableArray>(xvc)
            | "xwa" -> Assert.IsInstanceOfType<FplVariableArray>(xwa)
            | "xwb" -> Assert.IsInstanceOfType<FplVariableArray>(xwb)
            | "xwc" -> Assert.IsInstanceOfType<FplVariableArray>(xwc)
            | "yua" -> Assert.IsInstanceOfType<FplVariableArray>(yua)
            | "yub" -> Assert.IsInstanceOfType<FplVariableArray>(yub)
            | "yuc" -> Assert.IsInstanceOfType<FplVariableArray>(yuc)
            | "yva" -> Assert.IsInstanceOfType<FplVariableArray>(yva)
            | "yvb" -> Assert.IsInstanceOfType<FplVariableArray>(yvb)
            | "yvc" -> Assert.IsInstanceOfType<FplVariableArray>(yvc)
            | "ywa" -> Assert.IsInstanceOfType<FplVariableArray>(ywa)
            | "ywb" -> Assert.IsInstanceOfType<FplVariableArray>(ywb)
            | "ywc" -> Assert.IsInstanceOfType<FplVariableArray>(ywc)
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
    [<DataRow("base12", LiteralSelf)>]
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
    [<DataRow("base18", "ex x:pred(a:obj,b:T), y:C, z:obj {and (a,and(b,c))}")>]
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
            | "base1" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base2" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base3" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base4" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base5" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base6" -> Assert.IsInstanceOfType<FplReference>(base1)
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
            | "base18" -> Assert.IsInstanceOfType<FplGenericQuantor>(base1) 
            | "base19" -> Assert.IsInstanceOfType<FplGenericQuantor>(base1)  
            | "base20" -> Assert.IsInstanceOfType<FplGenericQuantor>(base1) 
            | "base21" -> Assert.IsInstanceOfType<FplConjunction>(base1)
            | "base21a" -> Assert.IsInstanceOfType<FplNegation>(base1)
            | "base21b" -> Assert.IsInstanceOfType<FplNegation>(base1)
            | "base22" -> Assert.IsInstanceOfType<FplExclusiveOr>(base1)
            | "base23" -> Assert.IsInstanceOfType<FplDisjunction>(base1)
            | "base24" -> Assert.IsInstanceOfType<FplEquivalence>(base1)
            | "base25" -> Assert.IsInstanceOfType<FplImplication>(base1)
            | "base26" -> Assert.IsInstanceOfType<FplIsOperator>(base1)
            | "base27" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base28" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base29" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base30" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base31" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base32" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base33" -> Assert.IsInstanceOfType<FplReference>(base1)
            | "base34" -> Assert.IsInstanceOfType<FplIsOperator>(base1)
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
    member this.TestBaseConstructorCallBlockType(var, varVal) =
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
        let filename = "TestBaseConstructorCallBlockType"
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
            | "base1" -> Assert.IsInstanceOfType<FplBaseConstructorCall>(base1)
            | "base2" -> Assert.IsInstanceOfType<FplBaseConstructorCall>(base1)
            | "base3" -> Assert.IsInstanceOfType<FplBaseConstructorCall>(base1)
            | "base4" -> Assert.IsInstanceOfType<FplBaseConstructorCall>(base1)
            | "base5" -> Assert.IsInstanceOfType<FplBaseConstructorCall>(base1)
            | "base6" -> Assert.IsInstanceOfType<FplBaseConstructorCall>(base1)
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
    [<DataRow("base2", """def pred T1() infix ">" -1 {intr};""")>]
    [<DataRow("base3", """def pred T1 () postfix "'" {intr};""")>]
    [<DataRow("base4", """def pred T1 () prefix "-" {intr};""")>]
    [<DataRow("base5", """def cl T1 symbol "∅" {intr};""")>]
    [<DataRow("base5a", """def cl T1 {intr};""")>]
    [<DataRow("base6", """def func T1()->obj {intr};""")>]
    [<DataRow("base7", """def func T1 ()->obj infix ">" -1 {intr};""")>]
    [<DataRow("base8", """def func T1 ()->obj postfix "'" {intr};""")>]
    [<DataRow("base9", """def func T1 ()->obj prefix "-" {intr};""")>]
    [<TestMethod>]
    member this.TestFixNotationBlockType(var, varVal) =
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
                if varVal.Contains LiteralCl then 
                    theory.Scope["T1"]
                elif varVal.Contains LiteralFunc then 
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
    [<DataRow("base5", """def cl A {intr} def func T()->A {intr};""")>]
    [<DataRow("base6", """def func T()->tpl {intr};""")>]
    [<DataRow("base7", """def func T()->pred(z:*obj[ind]) {intr};""")>]
    [<DataRow("base8", """def func T()->func(p:*pred(x:obj)[ind])->pred(x:ind) {intr};""")>]
    [<DataRow("base9", """def func T()->pred(f:*func(x:A)->A[ind]) {intr};""")>]
    [<DataRow("base10", """def cl A {intr} def func T()->pred(f:func(x:A)->A) {intr};""")>]
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
    [<DataRow("base2", """100. ExistsByExample, 1 |- false""", 2)>]
    [<DataRow("base3", """100. T1 |- assume not somePremise """, 1)>]
    [<DataRow("base4", """100. 2, 3, 5 |- iif (a,b)""", 3)>]
    [<DataRow("base5", """100. |- revoke 3""", 0)>]
    [<TestMethod>]
    member this.TestArgumentNumberOfJustifications(var, argExpression, expNumber:int) =
        ad.Clear()
        let fplCode = sprintf """proof T$1 { %s };""" argExpression
        let filename = "TestArgumentNumberOfJustifications"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let proof = theory.Scope["T$1"]
            let arg = proof.Scope["100"]
            let just = arg.ArgList[0]
            let numbOfJustifications = just.ArgList.Count
            Assert.AreEqual<int>(expNumber, numbOfJustifications)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """100. |- trivial""")>]
    [<DataRow("base2", """100. ExistsByExample, 1 |- false""")>]
    [<DataRow("base3", """100. T1 |- assume not somePremise """)>]
    [<DataRow("base4", """100. 2, 3, 5 |- iif (a,b)""")>]
    [<DataRow("base5", """100. |- revoke 3""")>]
    [<TestMethod>]
    member this.TestArgumentBlockType(var, argExpression) =
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
            let arg = proof.Scope["100"]
            match var with
            | "base1" -> Assert.IsInstanceOfType<FplArgument>(arg)
            | "base2" -> Assert.IsInstanceOfType<FplArgument>(arg)
            | "base3" -> Assert.IsInstanceOfType<FplArgument>(arg)
            | "base4" -> Assert.IsInstanceOfType<FplArgument>(arg)
            | "base5" -> Assert.IsInstanceOfType<FplArgument>(arg)
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
    member this.TestLanguageBlockType(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
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
            | "base0" -> Assert.IsInstanceOfType<FplLanguage>(lang)
            | "base1" -> Assert.IsInstanceOfType<FplLanguage>(lang)
            | "base2" -> Assert.IsInstanceOfType<FplLanguage>(lang)
            | "base3" -> Assert.IsInstanceOfType<FplLanguage>(lang)
            | "base4" -> Assert.IsInstanceOfType<FplLanguage>(lang)
            | "base5" -> Assert.IsInstanceOfType<FplLanguage>(lang)
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
    member this.TestLocalizationBlockType(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLocalizationBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base0" -> Assert.IsInstanceOfType<FplLocalization>(pred)
            | "base1" -> Assert.IsInstanceOfType<FplLocalization>(pred)
            | "base2" -> Assert.IsInstanceOfType<FplLocalization>(pred)
            | "base3" -> Assert.IsInstanceOfType<FplLocalization>(pred)
            | "base4" -> Assert.IsInstanceOfType<FplLocalization>(pred)
            | "base5" -> Assert.IsInstanceOfType<FplLocalization>(pred)
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
    member this.TestTranslationBlockType(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
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
            | "base0" -> Assert.IsInstanceOfType<FplTranslation>(trsl)
            | "base1" -> Assert.IsInstanceOfType<FplTranslation>(trsl)
            | "base2" -> Assert.IsInstanceOfType<FplTranslation>(trsl)
            | "base3" -> Assert.IsInstanceOfType<FplTranslation>(trsl)
            | "base4" -> Assert.IsInstanceOfType<FplTranslation>(trsl)
            | "base5" -> Assert.IsInstanceOfType<FplTranslation>(trsl)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("@0", "")>]
    [<DataRow("@1", "")>]
    [<DataRow("@2", "")>]
    [<DataRow("@3", "")>]
    [<DataRow("@4", "")>]
    [<DataRow("@100", "")>]
    [<DataRow("@42", "")>]
    [<TestMethod>]
    member this.TestDecrement(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred T() { del.Decrement(%s) };""" varVal
        let filename = "TestDecrementBlockType.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let pre = pr.ArgList |> Seq.toList |> List.rev |> List.head
            Assert.IsInstanceOfType<FplDecrement>(pre)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("@0", "")>]
    [<DataRow("@1", "")>]
    [<DataRow("@2", "")>]
    [<DataRow("@3", "")>]
    [<DataRow("@4", "")>]
    [<DataRow("@100", "")>]
    [<DataRow("@42", "")>]
    [<TestMethod>]
    member this.TestExtensionObj(varVal, expected:string) =
        ad.Clear()
        let fplCode = sprintf """def pred T() { %s };""" varVal
        let filename = "TestExtensionObjBlockType.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pr = theory.Scope["T()"] 
            let basePre = pr.ArgList |> Seq.head
            let base1 = basePre.Scope.Values |> Seq.head
            Assert.IsInstanceOfType<FplExtensionObj>(base1)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "$1")>]
    [<DataRow("base2", "$2")>]
    [<DataRow("base3", "$3")>]
    [<DataRow("base4", "$0")>]
    [<DataRow("base5", "$4")>]
    [<TestMethod>]
    member this.TestMCaseStatement(var, input) =
        ad.Clear()
        let fplCode = sprintf """
                def pred Equal (x,y: tpl) infix "=" 50 
                {
                    del.Equal(x,y)
                }
                
                def pred Test(x:ind) { dec 
                ~n:pred
                n:= mcases
                (
                    | (x = $1) : false 
                    | (x = $2) : true 
                    | (x = $3) : false 
                    ? undef  
                )
                ;n } def pred T() {Test(%s)};""" input 
        let filename = "TestMCaseStatement"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope["Test(ind)"]
            let assignment = pred.ArgList[0]
            let res = assignment.ArgList[1]
 
            match var with
            | "base1" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | "base2" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | "base3" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | "base4" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | "base5" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "$1")>]
    [<DataRow("base2", "$2")>]
    [<DataRow("base3", "$3")>]
    [<DataRow("base4", "$0")>]
    [<DataRow("base5", "$4")>]
    [<TestMethod>]
    member this.TestMapCasesBlockType(var, input) =
        ad.Clear()
        let fplCode = sprintf """
                def pred Equal (x,y: tpl) infix "=" 50 
                {
                    del.Equal(x,y)
                }              
                
                def pred Test(x:ind) { dec 
                ~n:pred
                n:= mcases
                (
                    | (x = $1) : false 
                    | (x = $2) : true 
                    | (x = $3) : false 
                    ? undef  
                )
                ;n } def pred T() {Test(%s)};""" input 
        let filename = "TestMapCasesBlockType"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope["Test(ind)"]
            let assignment = pred.ArgList[0]
            let res = assignment.ArgList[1]

 
            match var with
            | "base1" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | "base2" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | "base3" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | "base4" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | "base5" -> Assert.IsInstanceOfType<FplMapCases>(res)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "$1",  0)>]
    [<DataRow("base2", "$2",  2)>]
    [<DataRow("base3", "$3",  1)>]
    [<DataRow("base4", "$0",  3)>]
    [<DataRow("base5", "$4", 1)>]
    [<TestMethod>]
    member this.TestCaseStatement(var, input, (output:int)) =
        ad.Clear()
        let fplCode = sprintf """
                def pred Equal (x,y: tpl) infix "=" 50 
                {
                    del.Equal(x,y)
                }              
        
                def pred Test(x:ind) { dec 
                ~n:pred
                cases
                (
                    | (x = $1) : n:=false 
                    | (x = $2) : n:=true 
                    | (x = $3) : n:=false 
                    ? n:=undef  
                )
                ;n } def pred T() {Test(%s)};""" input 
        let filename = "TestMCaseStatement"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope["Test(ind)"]
            let cases = pred.ArgList[0]
 
            match var with
            | "base1" -> Assert.IsInstanceOfType<FplGenericStmt>(cases)
            | "base2" -> Assert.IsInstanceOfType<FplGenericStmt>(cases)
            | "base3" -> Assert.IsInstanceOfType<FplGenericStmt>(cases)
            | "base4" -> Assert.IsInstanceOfType<FplGenericStmt>(cases)
            | "base5" -> Assert.IsInstanceOfType<FplGenericStmt>(cases)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)