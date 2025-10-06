namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValueScopeNameEndPos() =

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
        let res = CommonFplValueTestCases.ScopeBlocks("NameEndPos") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<int64>(1L, r.EndPos.Column)
            | PrimTheoryL -> Assert.AreEqual<int64>(1L, theory.EndPos.Column)
            | "inf1" -> 
                let hasSignature = inf1 :?> FplRuleOfInference
                Assert.AreEqual<int64>(31L, hasSignature.SignEndPos.Column)
            | "inf2" -> 
                let hasSignature = inf2 :?> FplRuleOfInference
                Assert.AreEqual<int64>(31L, hasSignature.SignEndPos.Column) 
            | "axi1" -> 
                let hasSignature = axi1 :?> FplAxiom
                Assert.AreEqual<int64>(29L, hasSignature.SignEndPos.Column) 
            | "axi2" -> 
                let hasSignature = axi2 :?> FplAxiom
                Assert.AreEqual<int64>(29L, hasSignature.SignEndPos.Column) 
            | "pst1" -> 
                let hasSignature = pst1 :?> FplAxiom
                Assert.AreEqual<int64>(37L, hasSignature.SignEndPos.Column) 
            | "pst2" -> 
                let hasSignature = pst2 :?> FplAxiom
                Assert.AreEqual<int64>(37L, hasSignature.SignEndPos.Column) 
            | "thm1" -> 
                let hasSignature = thm1 :?> FplTheorem
                Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column) 
            | "thm2" -> 
                let hasSignature = thm2 :?> FplTheorem
                Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column) 
            | "pro1" -> 
                let hasSignature = pro1 :?> FplProposition
                Assert.AreEqual<int64>(41L, hasSignature.SignEndPos.Column) 
            | "pro2" -> 
                let hasSignature = pro2 :?> FplProposition
                Assert.AreEqual<int64>(41L, hasSignature.SignEndPos.Column) 
            | "lem1" -> 
                let hasSignature = lem1 :?> FplLemma
                Assert.AreEqual<int64>(29L, hasSignature.SignEndPos.Column) 
            | "lem2" -> 
                let hasSignature = lem2 :?> FplLemma
                Assert.AreEqual<int64>(29L, hasSignature.SignEndPos.Column) 
            | "cor1" -> 
                let hasSignature = cor1 :?> FplCorollary
                Assert.AreEqual<int64>(35L, hasSignature.SignEndPos.Column) 
            | "cor2" -> 
                let hasSignature = cor2 :?> FplCorollary
                Assert.AreEqual<int64>(35L, hasSignature.SignEndPos.Column) 
            | "con1" -> 
                let hasSignature = con1 :?> FplConjecture
                Assert.AreEqual<int64>(39L, hasSignature.SignEndPos.Column) 
            | "con2" -> 
                let hasSignature = con2 :?> FplConjecture
                Assert.AreEqual<int64>(39L, hasSignature.SignEndPos.Column) 
            | "cla1" -> 
                let hasSignature = cla1 :?> FplClass
                Assert.AreEqual<int64>(30L, hasSignature.SignEndPos.Column) 
            | "cla2" -> 
                let hasSignature = cla2 :?> FplClass
                Assert.AreEqual<int64>(30L, hasSignature.SignEndPos.Column) 
            | "pre1" -> 
                let hasSignature = pre1 :?> FplPredicate
                Assert.AreEqual<int64>(38L, hasSignature.SignEndPos.Column) 
            | "pre2" -> 
                let hasSignature = pre2 :?> FplPredicate
                Assert.AreEqual<int64>(38L, hasSignature.SignEndPos.Column) 
            | "fun1" -> 
                let hasSignature = fun1 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(48L, hasSignature.SignEndPos.Column) 
            | "fun2" -> 
                let hasSignature = fun2 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(48L, hasSignature.SignEndPos.Column) 
            | "fun3" ->     
                let hasSignature = fun3 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(48L, hasSignature.SignEndPos.Column) 
            | "fun4" -> 
                let hasSignature = fun4 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(48L, hasSignature.SignEndPos.Column) 
            | "fun5" -> 
                let hasSignature = fun5 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(55L, hasSignature.SignEndPos.Column) 
            | "fun6" -> 
                let hasSignature = fun6 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(55L, hasSignature.SignEndPos.Column) 
            | "fun7" -> 
                let hasSignature = fun7 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(55L, hasSignature.SignEndPos.Column) 
            | "fun8" -> 
                let hasSignature = fun8 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(48L, hasSignature.SignEndPos.Column) 
            | "fun9" -> 
                let hasSignature = fun9 :?> FplFunctionalTerm
                Assert.AreEqual<int64>(48L, hasSignature.SignEndPos.Column) 
            | "prf1" -> 
                let hasSignature = prf1 :?> FplProof
                Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column)
            | "prf2" -> 
                let hasSignature = prf2 :?> FplProof
                Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column)
            | "loc1" -> 
                Assert.AreEqual<int64>(13L, loc1.EndPos.Column) 
            | "loc2" -> 
                Assert.AreEqual<int64>(27L, loc2.EndPos.Column) 
            | _ -> Assert.IsTrue(false, "hier1")
        | _ -> 
            Assert.IsTrue(false, "hier2")


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
    [<DataRow("block")>]
    [<DataRow("t1")>]
    [<DataRow("t2")>]
    [<DataRow("t3")>]
    [<DataRow("t4")>]
    [<TestMethod>]
    member this.TestConstructors(var) =
        let res = CommonFplValueTestCases.ScopeConstructors("NameEndPos") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<int64>(1L, r.EndPos.Column)
            | PrimTheoryL -> Assert.AreEqual<int64>(1L, theory.EndPos.Column)
            | "block" -> 
                let hasSignature = block :?> FplClass
                Assert.AreEqual<int64>(22L, hasSignature.SignEndPos.Column)
            | "t1" -> 
                let hasSignature = t1 :?> FplConstructor
                Assert.AreEqual<int64>(26L, hasSignature.SignEndPos.Column)
            | "t2" -> 
                let hasSignature = t2 :?> FplConstructor
                Assert.AreEqual<int64>(31L, hasSignature.SignEndPos.Column)
            | "t3" -> 
                let hasSignature = t3 :?> FplConstructor
                Assert.AreEqual<int64>(32L, hasSignature.SignEndPos.Column)
            | "t4" -> 
                let hasSignature = t4 :?> FplConstructor
                Assert.AreEqual<int64>(31L, hasSignature.SignEndPos.Column)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("NameEndPos") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<int64>(1L, r.EndPos.Column)
                | PrimTheoryL -> Assert.AreEqual<int64>(1L, theory.EndPos.Column)
                | "thm1" -> 
                    let hasSignature = thm1 :?> FplTheorem
                    Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column)
                | "proofThm1" -> 
                    let hasSignature = proofThm1 :?> FplProof
                    Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column)
                | "lem1" -> 
                    let hasSignature = lem1 :?> FplLemma
                    Assert.AreEqual<int64>(29L, hasSignature.SignEndPos.Column)
                | "proofLem1" -> 
                    let hasSignature = proofLem1 :?> FplProof
                    Assert.AreEqual<int64>(31L, hasSignature.SignEndPos.Column)
                | "prp1" -> 
                    let hasSignature = prp1 :?> FplProposition
                    Assert.AreEqual<int64>(41L, hasSignature.SignEndPos.Column)
                | "proofPrp1" -> 
                    let hasSignature = proofPrp1 :?> FplProof
                    Assert.AreEqual<int64>(37L, hasSignature.SignEndPos.Column)
                | "cor1" -> 
                    let hasSignature = cor1 :?> FplCorollary
                    Assert.AreEqual<int64>(39L, hasSignature.SignEndPos.Column)
                | "proofCor1" -> 
                    let hasSignature = proofCor1 :?> FplProof
                    Assert.AreEqual<int64>(37L, hasSignature.SignEndPos.Column)
                | "thm2" -> 
                    let hasSignature = thm2 :?> FplTheorem
                    Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column)
                | "corThm2" -> 
                    let hasSignature = corThm2 :?> FplCorollary
                    Assert.AreEqual<int64>(37L, hasSignature.SignEndPos.Column)
                | "lem2" -> 
                    let hasSignature = lem2 :?> FplLemma
                    Assert.AreEqual<int64>(29L, hasSignature.SignEndPos.Column)
                | "corLem2" -> 
                    let hasSignature = corLem2 :?> FplCorollary
                    Assert.AreEqual<int64>(35L, hasSignature.SignEndPos.Column)
                | "prp2" -> 
                    let hasSignature = prp2 :?> FplProposition
                    Assert.AreEqual<int64>(41L, hasSignature.SignEndPos.Column)
                | "corPrp2" -> 
                    let hasSignature = corPrp2 :?> FplCorollary
                    Assert.AreEqual<int64>(41L, hasSignature.SignEndPos.Column)
                | "cor2" -> 
                    let hasSignature = cor2 :?> FplCorollary
                    Assert.AreEqual<int64>(39L, hasSignature.SignEndPos.Column)
                | "corCor2" -> 
                    let hasSignature = corCor2 :?> FplCorollary
                    Assert.AreEqual<int64>(41L, hasSignature.SignEndPos.Column)
                | "con1" -> 
                    let hasSignature = con1 :?> FplConjecture
                    Assert.AreEqual<int64>(38L, hasSignature.SignEndPos.Column)
                | "corCon1" -> 
                    let hasSignature = corCon1 :?> FplCorollary
                    Assert.AreEqual<int64>(39L, hasSignature.SignEndPos.Column)
                | "axi1" -> 
                    let hasSignature = axi1 :?> FplAxiom
                    Assert.AreEqual<int64>(28L, hasSignature.SignEndPos.Column)
                | "corAxi1"  -> 
                    let hasSignature = corAxi1 :?> FplCorollary
                    Assert.AreEqual<int64>(34L, hasSignature.SignEndPos.Column) 
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
        let res = CommonFplValueTestCases.ScopeProperties("NameEndPos") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<int64>(1L, r.EndPos.Column)
            | PrimTheoryL -> Assert.AreEqual<int64>(1L, theory.EndPos.Column)
            | "block" -> 
                let hasSignature = block :?> FplPredicate
                Assert.AreEqual<int64>(26L, hasSignature.SignEndPos.Column)
            | "t1" -> 
                let hasSignature = t1 :?> FplMandatoryPredicate
                Assert.AreEqual<int64>(27L, hasSignature.SignEndPos.Column)
            | "t2" -> 
                let hasSignature = t2 :?> FplOptionalPredicate
                Assert.AreEqual<int64>(31L, hasSignature.SignEndPos.Column)
            | "t3" -> 
                let hasSignature = t3 :?> FplMandatoryFunctionalTerm
                Assert.AreEqual<int64>(32L, hasSignature.SignEndPos.Column)
            | "t4" -> 
                let hasSignature = t4 :?> FplOptionalFunctionalTerm
                Assert.AreEqual<int64>(36L, hasSignature.SignEndPos.Column)
            | "t5" -> 
                let hasSignature = t5 :?> FplMandatoryFunctionalTerm
                Assert.AreEqual<int64>(32L, hasSignature.SignEndPos.Column)
            | "t6" -> 
                let hasSignature = t6 :?> FplOptionalFunctionalTerm
                Assert.AreEqual<int64>(36L, hasSignature.SignEndPos.Column)
            | "t7" -> 
                let hasSignature = t7 :?> FplMandatoryFunctionalTerm
                Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column)
            | "t8" -> 
                let hasSignature = t8 :?> FplOptionalFunctionalTerm
                Assert.AreEqual<int64>(37L, hasSignature.SignEndPos.Column)
            | "t9" -> 
                let hasSignature = t9 :?> FplMandatoryFunctionalTerm
                Assert.AreEqual<int64>(32L, hasSignature.SignEndPos.Column)
            | "t10" -> 
                let hasSignature = t10 :?> FplOptionalFunctionalTerm
                Assert.AreEqual<int64>(37L, hasSignature.SignEndPos.Column)
            | "t11" -> 
                let hasSignature = t11 :?> FplMandatoryFunctionalTerm
                Assert.AreEqual<int64>(33L, hasSignature.SignEndPos.Column)
            | "t12" -> 
                let hasSignature = t12 :?> FplOptionalFunctionalTerm
                Assert.AreEqual<int64>(37L, hasSignature.SignEndPos.Column)
            | "t13" -> 
                let hasSignature = t13 :?> FplMandatoryFunctionalTerm
                Assert.AreEqual<int64>(34L, hasSignature.SignEndPos.Column)
            | "t14" -> 
                let hasSignature = t14 :?> FplOptionalFunctionalTerm
                Assert.AreEqual<int64>(38L, hasSignature.SignEndPos.Column)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("NameEndPos")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<int64>(1L, r.EndPos.Column)
            | PrimTheoryL -> Assert.AreEqual<int64>(1L, theory.EndPos.Column)
            | "block" -> Assert.AreEqual<int64>(10L, block.EndPos.Column); 
            | "x" -> Assert.AreEqual<int64>(19L, x.EndPos.Column)
            | "y" -> Assert.AreEqual<int64>(21L, y.EndPos.Column)
            | "s" -> Assert.AreEqual<int64>(19L, s.EndPos.Column)
            | "xu" -> Assert.AreEqual<int64>(53L, xu.EndPos.Column)
            | "xv" -> Assert.AreEqual<int64>(53L, xv.EndPos.Column)
            | "xw" -> Assert.AreEqual<int64>(53L, xw.EndPos.Column)
            | "yu" -> Assert.AreEqual<int64>(53L, yu.EndPos.Column)
            | "yv" -> Assert.AreEqual<int64>(53L, yv.EndPos.Column)
            | "yw" -> Assert.AreEqual<int64>(53L, yw.EndPos.Column)
            | "xua" -> Assert.AreEqual<int64>(39L, xua.EndPos.Column)
            | "xub" -> Assert.AreEqual<int64>(41L, xub.EndPos.Column)
            | "xuc" -> Assert.AreEqual<int64>(43L, xuc.EndPos.Column)
            | "xva" -> Assert.AreEqual<int64>(39L, xva.EndPos.Column)
            | "xvb" -> Assert.AreEqual<int64>(41L, xvb.EndPos.Column)
            | "xvc" -> Assert.AreEqual<int64>(43L, xvc.EndPos.Column)
            | "xwa" -> Assert.AreEqual<int64>(39L, xwa.EndPos.Column)
            | "xwb" -> Assert.AreEqual<int64>(41L, xwb.EndPos.Column)
            | "xwc" -> Assert.AreEqual<int64>(43L, xwc.EndPos.Column)
            | "yua" -> Assert.AreEqual<int64>(39L, yua.EndPos.Column)
            | "yub" -> Assert.AreEqual<int64>(41L, yub.EndPos.Column)
            | "yuc" -> Assert.AreEqual<int64>(43L, yuc.EndPos.Column)
            | "yva" -> Assert.AreEqual<int64>(39L, yva.EndPos.Column)
            | "yvb" -> Assert.AreEqual<int64>(41L, yvb.EndPos.Column)
            | "yvc" -> Assert.AreEqual<int64>(43L, yvc.EndPos.Column)
            | "ywa" -> Assert.AreEqual<int64>(39L, ywa.EndPos.Column)
            | "ywb" -> Assert.AreEqual<int64>(41L, ywb.EndPos.Column)
            | "ywc" -> Assert.AreEqual<int64>(43L, ywc.EndPos.Column)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("NameEndPos")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<int64>(1L, r.EndPos.Column)
            | PrimTheoryL -> Assert.AreEqual<int64>(1L, theory.EndPos.Column)
            | "block" -> Assert.AreEqual<int64>(64L, block.EndPos.Column); 
            | "x" -> Assert.AreEqual<int64>(20L, x.EndPos.Column)
            | "y" -> Assert.AreEqual<int64>(22L, y.EndPos.Column)
            | "xu" -> Assert.AreEqual<int64>(56L, xu.EndPos.Column)
            | "xv" -> Assert.AreEqual<int64>(56L, xv.EndPos.Column)
            | "xw" -> Assert.AreEqual<int64>(56L, xw.EndPos.Column)
            | "yu" -> Assert.AreEqual<int64>(56L, yu.EndPos.Column)
            | "yv" -> Assert.AreEqual<int64>(56L, yv.EndPos.Column)
            | "yw" -> Assert.AreEqual<int64>(56L, yw.EndPos.Column)
            | "xua" -> Assert.AreEqual<int64>(41L, xua.EndPos.Column)
            | "xub" -> Assert.AreEqual<int64>(43L, xub.EndPos.Column)
            | "xuc" -> Assert.AreEqual<int64>(45L, xuc.EndPos.Column)
            | "xva" -> Assert.AreEqual<int64>(41L, xva.EndPos.Column)
            | "xvb" -> Assert.AreEqual<int64>(43L, xvb.EndPos.Column)
            | "xvc" -> Assert.AreEqual<int64>(45L, xvc.EndPos.Column)
            | "xwa" -> Assert.AreEqual<int64>(41L, xwa.EndPos.Column)
            | "xwb" -> Assert.AreEqual<int64>(43L, xwb.EndPos.Column)
            | "xwc" -> Assert.AreEqual<int64>(45L, xwc.EndPos.Column)
            | "yua" -> Assert.AreEqual<int64>(41L, yua.EndPos.Column)
            | "yub" -> Assert.AreEqual<int64>(43L, yub.EndPos.Column)
            | "yuc" -> Assert.AreEqual<int64>(45L, yuc.EndPos.Column)
            | "yva" -> Assert.AreEqual<int64>(41L, yva.EndPos.Column)
            | "yvb" -> Assert.AreEqual<int64>(43L, yvb.EndPos.Column)
            | "yvc" -> Assert.AreEqual<int64>(45L, yvc.EndPos.Column)
            | "ywa" -> Assert.AreEqual<int64>(41L, ywa.EndPos.Column)
            | "ywb" -> Assert.AreEqual<int64>(43L, ywb.EndPos.Column)
            | "ywc" -> Assert.AreEqual<int64>(45L, ywc.EndPos.Column)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("NameEndPos")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<int64>(1L, r.EndPos.Column)
            | PrimTheoryL -> Assert.AreEqual<int64>(1L, theory.EndPos.Column)
            | "block" -> Assert.AreEqual<int64>(19L, block.EndPos.Column); 
            | "x" -> Assert.AreEqual<int64>(33L, x.EndPos.Column)
            | "y" -> Assert.AreEqual<int64>(35L, y.EndPos.Column)
            | "xu" -> Assert.AreEqual<int64>(67L, xu.EndPos.Column)
            | "xv" -> Assert.AreEqual<int64>(67L, xv.EndPos.Column)
            | "xw" -> Assert.AreEqual<int64>(67L, xw.EndPos.Column)
            | "yu" -> Assert.AreEqual<int64>(67L, yu.EndPos.Column)
            | "yv" -> Assert.AreEqual<int64>(67L, yv.EndPos.Column)
            | "yw" -> Assert.AreEqual<int64>(67L, yw.EndPos.Column)
            | "xua" -> Assert.AreEqual<int64>(53L, xua.EndPos.Column)
            | "xub" -> Assert.AreEqual<int64>(55L, xub.EndPos.Column)
            | "xuc" -> Assert.AreEqual<int64>(57L, xuc.EndPos.Column)
            | "xva" -> Assert.AreEqual<int64>(53L, xva.EndPos.Column)
            | "xvb" -> Assert.AreEqual<int64>(55L, xvb.EndPos.Column)
            | "xvc" -> Assert.AreEqual<int64>(57L, xvc.EndPos.Column)
            | "xwa" -> Assert.AreEqual<int64>(53L, xwa.EndPos.Column)
            | "xwb" -> Assert.AreEqual<int64>(55L, xwb.EndPos.Column)
            | "xwc" -> Assert.AreEqual<int64>(57L, xwc.EndPos.Column)
            | "yua" -> Assert.AreEqual<int64>(53L, yua.EndPos.Column)
            | "yub" -> Assert.AreEqual<int64>(55L, yub.EndPos.Column)
            | "yuc" -> Assert.AreEqual<int64>(57L, yuc.EndPos.Column)
            | "yva" -> Assert.AreEqual<int64>(53L, yva.EndPos.Column)
            | "yvb" -> Assert.AreEqual<int64>(55L, yvb.EndPos.Column)
            | "yvc" -> Assert.AreEqual<int64>(57L, yvc.EndPos.Column)
            | "ywa" -> Assert.AreEqual<int64>(53L, ywa.EndPos.Column)
            | "ywb" -> Assert.AreEqual<int64>(55L, ywb.EndPos.Column)
            | "ywc" -> Assert.AreEqual<int64>(57L, ywc.EndPos.Column)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("NameEndPos")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<int64>(1L, r.EndPos.Column)
            | PrimTheoryL -> Assert.AreEqual<int64>(1L, theory.EndPos.Column)
            | "block" -> Assert.AreEqual<int64>(19L, block.EndPos.Column); 
            | "x" -> Assert.AreEqual<int64>(33L, x.EndPos.Column)
            | "y" -> Assert.AreEqual<int64>(35L, y.EndPos.Column)
            | "xu" -> Assert.AreEqual<int64>(69L, xu.EndPos.Column)
            | "xv" -> Assert.AreEqual<int64>(69L, xv.EndPos.Column)
            | "xw" -> Assert.AreEqual<int64>(69L, xw.EndPos.Column)
            | "yu" -> Assert.AreEqual<int64>(69L, yu.EndPos.Column)
            | "yv" -> Assert.AreEqual<int64>(69L, yv.EndPos.Column)
            | "yw" -> Assert.AreEqual<int64>(69L, yw.EndPos.Column)
            | "xua" -> Assert.AreEqual<int64>(54L, xua.EndPos.Column)
            | "xub" -> Assert.AreEqual<int64>(56L, xub.EndPos.Column)
            | "xuc" -> Assert.AreEqual<int64>(58L, xuc.EndPos.Column)
            | "xva" -> Assert.AreEqual<int64>(54L, xva.EndPos.Column)
            | "xvb" -> Assert.AreEqual<int64>(56L, xvb.EndPos.Column)
            | "xvc" -> Assert.AreEqual<int64>(58L, xvc.EndPos.Column)
            | "xwa" -> Assert.AreEqual<int64>(54L, xwa.EndPos.Column)
            | "xwb" -> Assert.AreEqual<int64>(56L, xwb.EndPos.Column)
            | "xwc" -> Assert.AreEqual<int64>(58L, xwc.EndPos.Column)
            | "yua" -> Assert.AreEqual<int64>(54L, yua.EndPos.Column)
            | "yub" -> Assert.AreEqual<int64>(56L, yub.EndPos.Column)
            | "yuc" -> Assert.AreEqual<int64>(58L, yuc.EndPos.Column)
            | "yva" -> Assert.AreEqual<int64>(54L, yva.EndPos.Column)
            | "yvb" -> Assert.AreEqual<int64>(56L, yvb.EndPos.Column)
            | "yvc" -> Assert.AreEqual<int64>(58L, yvc.EndPos.Column)
            | "ywa" -> Assert.AreEqual<int64>(54L, ywa.EndPos.Column)
            | "ywb" -> Assert.AreEqual<int64>(56L, ywb.EndPos.Column)
            | "ywc" -> Assert.AreEqual<int64>(58L, ywc.EndPos.Column)
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
    [<DataRow("base18", "ex x:pred(a:T), y:C, z:obj {and (a,and(b,c))}")>]
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
        let filename = "TestPredicateNameEndPos.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<int64>((int64)22, base1.EndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)23, base1.EndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)23, base1.EndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)20, base1.EndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)27, base1.EndPos.Column)
            | "base6" -> Assert.AreEqual<int64>((int64)19, base1.EndPos.Column)
            | "base7" -> Assert.AreEqual<int64>((int64)26, base1.EndPos.Column)
            | "base8" -> Assert.AreEqual<int64>((int64)24, base1.EndPos.Column)
            | "base9" -> Assert.AreEqual<int64>((int64)25, base1.EndPos.Column)
            | "base10" -> Assert.AreEqual<int64>((int64)21, base1.EndPos.Column)
            | "base11" -> Assert.AreEqual<int64>((int64)18, base1.EndPos.Column)
            | "base12" -> Assert.AreEqual<int64>((int64)24, base1.EndPos.Column)
            | "base13" -> Assert.AreEqual<int64>((int64)19, base1.EndPos.Column)
            | "base11a" -> Assert.AreEqual<int64>((int64)20, base1.EndPos.Column)
            | "base12a" -> Assert.AreEqual<int64>((int64)23, base1.EndPos.Column)
            | "base10b" -> Assert.AreEqual<int64>((int64)23, base1.EndPos.Column)
            | "base11b" -> Assert.AreEqual<int64>((int64)20, base1.EndPos.Column)
            | "base12b" -> Assert.AreEqual<int64>((int64)23, base1.EndPos.Column)
            | "base13b" -> Assert.AreEqual<int64>((int64)21, base1.EndPos.Column)
            | "base10c" -> Assert.AreEqual<int64>((int64)27, base1.EndPos.Column)
            | "base11c" -> Assert.AreEqual<int64>((int64)24, base1.EndPos.Column)
            | "base12c" -> Assert.AreEqual<int64>((int64)27, base1.EndPos.Column)
            | "base13c" -> Assert.AreEqual<int64>((int64)25, base1.EndPos.Column)
            | "base10d" -> Assert.AreEqual<int64>((int64)28, base1.EndPos.Column)
            | "base11d" -> Assert.AreEqual<int64>((int64)25, base1.EndPos.Column)
            | "base12d" -> Assert.AreEqual<int64>((int64)28, base1.EndPos.Column)
            | "base13d" -> Assert.AreEqual<int64>((int64)25, base1.EndPos.Column)
            | "base10e" -> Assert.AreEqual<int64>((int64)41, base1.EndPos.Column)
            | "base11e" -> Assert.AreEqual<int64>((int64)33, base1.EndPos.Column)
            | "base12e" -> Assert.AreEqual<int64>((int64)37, base1.EndPos.Column)
            | "base13e" -> Assert.AreEqual<int64>((int64)34, base1.EndPos.Column)
            | "base10f" -> Assert.AreEqual<int64>((int64)35, base1.EndPos.Column)
            | "base11f" -> Assert.AreEqual<int64>((int64)32, base1.EndPos.Column)
            | "base12f" -> Assert.AreEqual<int64>((int64)38, base1.EndPos.Column)
            | "base13f" -> Assert.AreEqual<int64>((int64)32, base1.EndPos.Column)
            | "base14" -> Assert.AreEqual<int64>((int64)19, base1.EndPos.Column)
            | "base15" -> Assert.AreEqual<int64>((int64)19, base1.EndPos.Column)
            | "base15a" -> Assert.AreEqual<int64>((int64)20, base1.EndPos.Column)
            | "base15b" -> Assert.AreEqual<int64>((int64)21, base1.EndPos.Column)
            | "base16" -> Assert.AreEqual<int64>((int64)34, base1.EndPos.Column)
            | "base17" -> Assert.AreEqual<int64>((int64)36, base1.EndPos.Column)
            | "base18" -> Assert.AreEqual<int64>((int64)62, base1.EndPos.Column)
            | "base19" -> Assert.AreEqual<int64>((int64)45, base1.EndPos.Column)
            | "base20" -> Assert.AreEqual<int64>((int64)34, base1.EndPos.Column)
            | "base21" -> Assert.AreEqual<int64>((int64)35, base1.EndPos.Column)
            | "base21a" -> Assert.AreEqual<int64>((int64)23, base1.EndPos.Column)
            | "base21b" -> Assert.AreEqual<int64>((int64)25, base1.EndPos.Column)
            | "base22" -> Assert.AreEqual<int64>((int64)35, base1.EndPos.Column)
            | "base23" -> Assert.AreEqual<int64>((int64)33, base1.EndPos.Column)
            | "base24" -> Assert.AreEqual<int64>((int64)27, base1.EndPos.Column)
            | "base25" -> Assert.AreEqual<int64>((int64)28, base1.EndPos.Column)
            | "base26" -> Assert.AreEqual<int64>((int64)28, base1.EndPos.Column)
            | "base27" -> Assert.AreEqual<int64>((int64)20, base1.EndPos.Column)
            | "base28" -> Assert.AreEqual<int64>((int64)27, base1.EndPos.Column)
            | "base29" -> Assert.AreEqual<int64>((int64)28, base1.EndPos.Column)
            | "base30" -> Assert.AreEqual<int64>((int64)25, base1.EndPos.Column)
            | "base31" -> Assert.AreEqual<int64>((int64)41, base1.EndPos.Column)
            | "base32" -> Assert.AreEqual<int64>((int64)38, base1.EndPos.Column)
            | "base33" -> Assert.AreEqual<int64>((int64)43, base1.EndPos.Column)
            | "base34" -> Assert.AreEqual<int64>((int64)27, base1.EndPos.Column)
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
    member this.TestBaseConstructorCallNameEndPos(var, varVal) =
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
        let filename = "TestBaseConstructorCallNameEndPos"
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
            | "base1" -> 
                Assert.AreEqual<int64>((int64)45, base1.EndPos.Column)
            | "base2" -> 
                Assert.AreEqual<int64>((int64)55, base1.EndPos.Column)
            | "base3" -> 
                Assert.AreEqual<int64>((int64)55, base1.EndPos.Column)
            | "base4" -> 
                Assert.AreEqual<int64>((int64)50, base1.EndPos.Column)
            | "base5" -> 
                Assert.AreEqual<int64>((int64)69, base1.EndPos.Column)
            | "base6" -> 
                Assert.AreEqual<int64>((int64)63, base1.EndPos.Column)
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
        let filename = "TestDelegateNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<int64>((int64)24, base1.EndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)31, base1.EndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)32, base1.EndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)29, base1.EndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)27, base1.EndPos.Column)
            | "base6" -> Assert.AreEqual<int64>((int64)45, base1.EndPos.Column)
            | "base7" -> Assert.AreEqual<int64>((int64)42, base1.EndPos.Column)
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
    member this.TestFixNotationEndPos(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestFixNotationNameEndPos"
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
            | "base1" -> 
                let hasSignature = base1 :?> FplPredicate
                Assert.AreEqual<int64>((int64)14, hasSignature.SignEndPos.Column)
            | "base2" -> 
                let hasSignature = base1 :?> FplPredicate
                Assert.AreEqual<int64>((int64)15, hasSignature.SignEndPos.Column)
            | "base3" -> 
                let hasSignature = base1 :?> FplPredicate
                Assert.AreEqual<int64>((int64)15, hasSignature.SignEndPos.Column)
            | "base4" -> 
                let hasSignature = base1 :?> FplPredicate
                Assert.AreEqual<int64>((int64)15, hasSignature.SignEndPos.Column)
            | "base5" -> 
                let hasSignature = base1 :?> FplClass
                Assert.AreEqual<int64>((int64)10, hasSignature.SignEndPos.Column)
            | "base5a" -> 
                let hasSignature = base1 :?> FplClass
                Assert.AreEqual<int64>((int64)10, hasSignature.SignEndPos.Column)
            | "base6" -> 
                let hasSignature = base1 :?> FplFunctionalTerm
                Assert.AreEqual<int64>((int64)19, hasSignature.SignEndPos.Column)
            | "base7" -> 
                let hasSignature = base1 :?> FplFunctionalTerm
                Assert.AreEqual<int64>((int64)20, hasSignature.SignEndPos.Column)
            | "base8" -> 
                let hasSignature = base1 :?> FplFunctionalTerm
                Assert.AreEqual<int64>((int64)21, hasSignature.SignEndPos.Column)
            | "base9" -> 
                let hasSignature = base1 :?> FplFunctionalTerm
                Assert.AreEqual<int64>((int64)20, hasSignature.SignEndPos.Column)
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
        let filename = "TestMappingNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<int64>((int64)18, mapping.EndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)18, mapping.EndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)19, mapping.EndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)19, mapping.EndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)36, mapping.EndPos.Column)
            | "base6" -> Assert.AreEqual<int64>((int64)26, mapping.EndPos.Column)
            | "base7" -> Assert.AreEqual<int64>((int64)27, mapping.EndPos.Column)
            | "base8" -> Assert.AreEqual<int64>((int64)48, mapping.EndPos.Column)
            | "base9" -> Assert.AreEqual<int64>((int64)36, mapping.EndPos.Column)
            | "base10" -> Assert.AreEqual<int64>((int64)55, mapping.EndPos.Column)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """100. |- trivial""")>]
    [<DataRow("base2", """100. ExistsByExample, 1 |- false""")>]
    [<DataRow("base3", """100. T1 |- assume not somePremise """)>]
    [<DataRow("base4", """100. 2, 3, 5 |- iif (a,b)""")>]
    [<DataRow("base5", """100. |- revoke 3""")>]
    [<TestMethod>]
    member this.TestArgumentNameEndPos(var, argExpression) =
        ad.Clear()
        let fplCode = sprintf """proof T$1 { %s };""" argExpression
        let filename = "TestArgumentNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let proof = theory.Scope["T$1"]
            let arg = proof.Scope["100"]
            match var with
            | "base1" -> Assert.AreEqual<int64>((int64)29, arg.EndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)46, arg.EndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)48, arg.EndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)39, arg.EndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)29, arg.EndPos.Column)
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
    member this.TestLanguageNameEndPos(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLanguageNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<int64>((int64)17, lang.EndPos.Column)
            | "base1" -> Assert.AreEqual<int64>((int64)22, lang.EndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)19, lang.EndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)22, lang.EndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)24, lang.EndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)27, lang.EndPos.Column)
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
    member this.TestLocalizationNameEndPos(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLocalizationNameEndPos"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base0" -> Assert.AreEqual<int64>((int64)10, pred.EndPos.Column)
            | "base1" -> Assert.AreEqual<int64>((int64)15, pred.EndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)12, pred.EndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)15, pred.EndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)16, pred.EndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)19, pred.EndPos.Column)
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
    member this.TestTranslationNameEndPos(var, predName, predDecl, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestTranslationNameEndPos"
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
            | "base0" -> Assert.AreEqual<int64>((int64)23, trsl.EndPos.Column)
            | "base1" -> Assert.AreEqual<int64>((int64)46, trsl.EndPos.Column)
            | "base2" -> Assert.AreEqual<int64>((int64)35, trsl.EndPos.Column)
            | "base3" -> Assert.AreEqual<int64>((int64)37, trsl.EndPos.Column)
            | "base4" -> Assert.AreEqual<int64>((int64)34, trsl.EndPos.Column)
            | "base5" -> Assert.AreEqual<int64>((int64)40, trsl.EndPos.Column)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
