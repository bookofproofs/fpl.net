namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValueScopeFplRepresentation() =

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
        let res = CommonFplValueTestCases.ScopeBlocks("FplRepresentation") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, r.FplRepresentation)
            | "theory" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, theory.FplRepresentation)
            | "inf1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, inf1.FplRepresentation)
            | "inf2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, inf2.FplRepresentation)
            | "axi1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, axi1.FplRepresentation)
            | "axi2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, axi2.FplRepresentation)
            | "pst1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, pst1.FplRepresentation)
            | "pst2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, pst2.FplRepresentation)
            | "thm1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, thm1.FplRepresentation)
            | "thm2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, thm2.FplRepresentation)
            | "pro1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, pro1.FplRepresentation)
            | "pro2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, pro2.FplRepresentation)
            | "lem1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, lem1.FplRepresentation)
            | "lem2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, lem2.FplRepresentation)
            | "cor1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, cor1.FplRepresentation)
            | "cor2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, cor2.FplRepresentation)
            | "con1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, con1.FplRepresentation)
            | "con2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, con2.FplRepresentation)
            | "cla1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Class, cla1.FplRepresentation)
            | "cla2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Class, cla2.FplRepresentation)
            | "pre1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, pre1.FplRepresentation)
            | "pre2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, pre2.FplRepresentation)
            | "fun1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", fun1.FplRepresentation)
            | "fun2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", fun2.FplRepresentation)
            | "prf1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, prf1.FplRepresentation)
            | "prf2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, prf2.FplRepresentation)
            | "loc1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, loc1.FplRepresentation)
            | "loc2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, loc2.FplRepresentation)
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
        let res = CommonFplValueTestCases.ScopeConstructors("FplRepresentation") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, r.FplRepresentation)
            | "theory" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, theory.FplRepresentation)
            | "block" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Class, block.FplRepresentation)
            | "t1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t1.FplRepresentation)
            | "t2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t2.FplRepresentation)
            | "t3" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t3.FplRepresentation)
            | "t4" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t4.FplRepresentation)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("FplRepresentation") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, r.FplRepresentation)
                | "theory" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, theory.FplRepresentation)
                | "thm1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, thm1.FplRepresentation)
                | "proofThm1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, proofThm1.FplRepresentation)
                | "lem1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, lem1.FplRepresentation)
                | "proofLem1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, proofLem1.FplRepresentation)
                | "prp1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, prp1.FplRepresentation)
                | "proofPrp1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, proofPrp1.FplRepresentation)
                | "cor1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, cor1.FplRepresentation)
                | "proofCor1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, proofCor1.FplRepresentation)
                | "thm2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, thm2.FplRepresentation)
                | "corThm2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, corThm2.FplRepresentation)
                | "lem2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, lem2.FplRepresentation)
                | "corLem2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, corLem2.FplRepresentation)
                | "prp2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, prp2.FplRepresentation)
                | "corPrp2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, corPrp2.FplRepresentation)
                | "cor2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, cor2.FplRepresentation)
                | "corCor2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, corCor2.FplRepresentation)
                | "con1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, con1.FplRepresentation)
                | "corCon1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, corCon1.FplRepresentation)
                | "axi1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, axi1.FplRepresentation)
                | "corAxi1"  -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, corAxi1.FplRepresentation) 
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
        let res = CommonFplValueTestCases.ScopeProperties("FplRepresentation") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, r.FplRepresentation)
            | "theory" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, theory.FplRepresentation)
            | "block" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, block.FplRepresentation)
            | "t1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, t1.FplRepresentation)
            | "t2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, t2.FplRepresentation)
            | "t3" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t3.FplRepresentation)
            | "t4" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t4.FplRepresentation)
            | "t5" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Index ((uint)0), t5.FplRepresentation)
            | "t6" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Index ((uint)0), t6.FplRepresentation)
            | "t7" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, t7.FplRepresentation)
            | "t8" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, t8.FplRepresentation)
            | "t9" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t9.FplRepresentation)
            | "t10" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t10.FplRepresentation)
            | "t11" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t11.FplRepresentation)
            | "t12" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", t12.FplRepresentation)
            | "t13" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Function, t13.FplRepresentation)
            | "t14" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Function, t14.FplRepresentation)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("FplRepresentation")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, r.FplRepresentation)
            | "theory" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, theory.FplRepresentation)
            | "block" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, block.FplRepresentation); 
            | "x" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, x.FplRepresentation)
            | "y" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, y.FplRepresentation)
            | "s" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, s.FplRepresentation)
            | "xu" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xu.FplRepresentation)
            | "xv" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xv.FplRepresentation)
            | "xw" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xw.FplRepresentation)
            | "yu" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yu.FplRepresentation)
            | "yv" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yv.FplRepresentation)
            | "yw" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yw.FplRepresentation)
            | "xua" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xua.FplRepresentation)
            | "xub" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xub.FplRepresentation)
            | "xuc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xuc.FplRepresentation)
            | "xva" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xva.FplRepresentation)
            | "xvb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xvb.FplRepresentation)
            | "xvc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xvc.FplRepresentation)
            | "xwa" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwa.FplRepresentation)
            | "xwb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwb.FplRepresentation)
            | "xwc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwc.FplRepresentation)
            | "yua" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yua.FplRepresentation)
            | "yub" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yub.FplRepresentation)
            | "yuc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yuc.FplRepresentation)
            | "yva" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yva.FplRepresentation)
            | "yvb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yvb.FplRepresentation)
            | "yvc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yvc.FplRepresentation)
            | "ywa" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywa.FplRepresentation)
            | "ywb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywb.FplRepresentation)
            | "ywc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywc.FplRepresentation)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("FplRepresentation")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, r.FplRepresentation)
            | "theory" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, theory.FplRepresentation)
            | "block" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, block.FplRepresentation); 
            | "x" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, x.FplRepresentation)
            | "y" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, y.FplRepresentation)
            | "xu" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xu.FplRepresentation)
            | "xv" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xv.FplRepresentation)
            | "xw" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xw.FplRepresentation)
            | "yu" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yu.FplRepresentation)
            | "yv" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yv.FplRepresentation)
            | "yw" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yw.FplRepresentation)
            | "xua" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xua.FplRepresentation)
            | "xub" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xub.FplRepresentation)
            | "xuc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xuc.FplRepresentation)
            | "xva" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xva.FplRepresentation)
            | "xvb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xvb.FplRepresentation)
            | "xvc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xvc.FplRepresentation)
            | "xwa" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwa.FplRepresentation)
            | "xwb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwb.FplRepresentation)
            | "xwc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwc.FplRepresentation)
            | "yua" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yua.FplRepresentation)
            | "yub" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yub.FplRepresentation)
            | "yuc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yuc.FplRepresentation)
            | "yva" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yva.FplRepresentation)
            | "yvb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yvb.FplRepresentation)
            | "yvc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yvc.FplRepresentation)
            | "ywa" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywa.FplRepresentation)
            | "ywb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywb.FplRepresentation)
            | "ywc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywc.FplRepresentation)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("FplRepresentation")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, r.FplRepresentation)
            | "theory" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, theory.FplRepresentation)
            | "block" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, block.FplRepresentation); 
            | "x" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, x.FplRepresentation)
            | "y" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, y.FplRepresentation)
            | "xu" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xu.FplRepresentation)
            | "xv" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xv.FplRepresentation)
            | "xw" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xw.FplRepresentation)
            | "yu" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yu.FplRepresentation)
            | "yv" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yv.FplRepresentation)
            | "yw" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yw.FplRepresentation)
            | "xua" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xua.FplRepresentation)
            | "xub" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xub.FplRepresentation)
            | "xuc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xuc.FplRepresentation)
            | "xva" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xva.FplRepresentation)
            | "xvb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xvb.FplRepresentation)
            | "xvc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xvc.FplRepresentation)
            | "xwa" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwa.FplRepresentation)
            | "xwb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwb.FplRepresentation)
            | "xwc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwc.FplRepresentation)
            | "yua" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yua.FplRepresentation)
            | "yub" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yub.FplRepresentation)
            | "yuc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yuc.FplRepresentation)
            | "yva" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yva.FplRepresentation)
            | "yvb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yvb.FplRepresentation)
            | "yvc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yvc.FplRepresentation)
            | "ywa" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywa.FplRepresentation)
            | "ywb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywb.FplRepresentation)
            | "ywc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywc.FplRepresentation)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("FplRepresentation")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, r.FplRepresentation)
            | "theory" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, theory.FplRepresentation)
            | "block" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, block.FplRepresentation); 
            | "x" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, x.FplRepresentation)
            | "y" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, y.FplRepresentation)
            | "xu" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xu.FplRepresentation)
            | "xv" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xv.FplRepresentation)
            | "xw" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xw.FplRepresentation)
            | "yu" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yu.FplRepresentation)
            | "yv" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yv.FplRepresentation)
            | "yw" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yw.FplRepresentation)
            | "xua" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xua.FplRepresentation)
            | "xub" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xub.FplRepresentation)
            | "xuc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xuc.FplRepresentation)
            | "xva" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xva.FplRepresentation)
            | "xvb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xvb.FplRepresentation)
            | "xvc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xvc.FplRepresentation)
            | "xwa" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwa.FplRepresentation)
            | "xwb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwb.FplRepresentation)
            | "xwc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", xwc.FplRepresentation)
            | "yua" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yua.FplRepresentation)
            | "yub" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yub.FplRepresentation)
            | "yuc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yuc.FplRepresentation)
            | "yva" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yva.FplRepresentation)
            | "yvb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yvb.FplRepresentation)
            | "yvc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", yvc.FplRepresentation)
            | "ywa" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywa.FplRepresentation)
            | "ywb" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywb.FplRepresentation)
            | "ywc" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", ywc.FplRepresentation)
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
    [<DataRow("base18", "ex x in Range(a, b), y in c, z {and (a, b, c)}")>]
    [<DataRow("base19", "exn$1 x {all y {true}}")>]
    [<DataRow("base20", "all x {not x}")>]
    [<DataRow("base21", "and (x, y, z)")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not (x)")>]
    [<DataRow("base22", "xor (x, y, z)")>]
    [<DataRow("base23", "or (x, y, z)")>]
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
        let filename = "TestPredicate.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.True, base1.FplRepresentation)
            | "base2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.False, base1.FplRepresentation)
            | "base3" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
            | "base4" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
            | "base5" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
            | "base6" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Index ((uint)1), base1.FplRepresentation)
            | "base7" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base8" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base9" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base10" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Class, base1.FplRepresentation)
            | "base11" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base12" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base13" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Extension, base1.FplRepresentation)
            | "base11a" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base12a" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base10b" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base11b" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base12b" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base13b" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Extension, base1.FplRepresentation)
            | "base10c" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base11c" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base12c" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base13c" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base10d" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base11d" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base12d" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base13d" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base10e" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base11e" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base12e" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base13e" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base10f" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base11f" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base12f" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base13f" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base14" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base15" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base15a" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base15b" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base16" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base17" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base18" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base19" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base20" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base21" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base21a" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base21b" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base22" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base23" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base24" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base25" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base26" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base27" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base28" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base29" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base30" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base31" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base32" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
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
        let filename = "TestCallConstructorParentClassFplRepresentation"
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
            | "base1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | "base2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | "base3" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | "base4" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | "base5" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | "base6" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "del.Equal(x,y)")>]
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
        let filename = "TestDelegateFplRepresentation"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
            | "base3" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
            | "base4" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
            | "base5" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
            | "base6" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
            | "base7" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.Undef, base1.FplRepresentation)
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
        let filename = "TestFixNotationFplRepresentation"
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
            | "base1" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base2" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base3" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base4" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.PredRepr FplPredicate.Undetermined, base1.FplRepresentation)
            | "base5" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Class, base1.FplRepresentation)
            | "base5a" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.LangRepr FplLanguageConstruct.Class, base1.FplRepresentation)
            | "base6" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | "base7" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | "base8" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | "base9" -> Assert.AreEqual<FplRepresentation>(FplRepresentation.ObjRepr "obj", base1.FplRepresentation)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
