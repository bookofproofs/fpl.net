namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValueScopeTypeSignature() =

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
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks() 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual([], r.TypeSignature)
            | "theory" -> Assert.AreEqual([], theory.TypeSignature)
            | "inf1" -> Assert.AreEqual(["SomeInference1"; "("; ")"], inf1.TypeSignature)
            | "inf2" -> Assert.AreEqual(["SomeInference2"; "("; ")"], inf2.TypeSignature)
            | "axi1" -> Assert.AreEqual(["SomeAxiom1"; "("; ")"], axi1.TypeSignature)
            | "axi2" -> Assert.AreEqual(["SomeAxiom2"; "("; ")"], axi2.TypeSignature)
            | "pst1" -> Assert.AreEqual(["SomePostulate1"; "("; ")"], pst1.TypeSignature)
            | "pst2" -> Assert.AreEqual(["SomePostulate2"; "("; ")"], pst2.TypeSignature)
            | "thm1" -> Assert.AreEqual(["SomeTheorem1"; "("; ")"], thm1.TypeSignature)
            | "thm2" -> Assert.AreEqual(["SomeTheorem2"; "("; ")"], thm2.TypeSignature)
            | "pro1" -> Assert.AreEqual(["SomeProposition1"; "("; ")"], pro1.TypeSignature)
            | "pro2" -> Assert.AreEqual(["SomeProposition2"; "("; ")"], pro2.TypeSignature)
            | "lem1" -> Assert.AreEqual(["SomeLemma1"; "("; ")"], lem1.TypeSignature)
            | "lem2" -> Assert.AreEqual(["SomeLemma2"; "("; ")"], lem2.TypeSignature)
            | "cor1" -> Assert.AreEqual(["SomeLemma1"; "$1"; "("; ")"], cor1.TypeSignature)
            | "cor2" -> Assert.AreEqual(["SomeLemma2"; "$1"; "("; ")"], cor2.TypeSignature)
            | "con1" -> Assert.AreEqual(["SomeConjecture1"; "("; ")"], con1.TypeSignature)
            | "con2" -> Assert.AreEqual(["SomeConjecture2"; "("; ")"], con2.TypeSignature)
            | "cla1" -> Assert.AreEqual(["SomeClass1"], cla1.TypeSignature)
            | "cla2" -> Assert.AreEqual(["SomeClass2"], cla2.TypeSignature)
            | "pre1" -> Assert.AreEqual(["SomePredicate1"; "("; ")"], pre1.TypeSignature)
            | "pre2" -> Assert.AreEqual(["SomePredicate2"; "("; ")"], pre2.TypeSignature)
            | "fun1" -> Assert.AreEqual(["SomeFunctionalTerm1"; "("; ")"; "->"; "obj"], fun1.TypeSignature)
            | "fun2" -> Assert.AreEqual(["SomeFunctionalTerm2"; "("; ")"; "->"; "obj"], fun2.TypeSignature)
            | "prf1" -> Assert.AreEqual(["SomeTheorem1"; "$1"], prf1.TypeSignature)
            | "prf2" -> Assert.AreEqual(["SomeTheorem2"; "$1"], prf2.TypeSignature)
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
        let res = CommonFplValueTestCases.ScopeConstructors() 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual([], r.TypeSignature)
            | "theory" -> Assert.AreEqual([], theory.TypeSignature)
            | "block" -> Assert.AreEqual(["TestId"], block.TypeSignature)
            | "t1" -> Assert.AreEqual(["TestId"; "("; ")"], t1.TypeSignature)
            | "t2" -> Assert.AreEqual(["TestId"; "("; "obj"; ")"], t2.TypeSignature)
            | "t3" -> Assert.AreEqual(["TestId"; "("; "pred"; ")"], t3.TypeSignature)
            | "t4" -> Assert.AreEqual(["TestId"; "("; "ind"; ")"], t4.TypeSignature)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries() 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual([], r.TypeSignature)
                | "theory" -> Assert.AreEqual([], theory.TypeSignature)
                | "thm1" -> Assert.AreEqual(["TestTheorem1"; "("; ")"], thm1.TypeSignature)
                | "proofThm1" -> Assert.AreEqual(["TestTheorem1"; "$1"], proofThm1.TypeSignature)
                | "lem1" -> Assert.AreEqual(["TestLemma1"; "("; ")"], lem1.TypeSignature)
                | "proofLem1" -> Assert.AreEqual(["TestLemma1"; "$1"], proofLem1.TypeSignature)
                | "prp1" -> Assert.AreEqual(["TestProposition1"; "("; ")"], prp1.TypeSignature)
                | "proofPrp1" -> Assert.AreEqual(["TestProposition1"; "$1"], proofPrp1.TypeSignature)
                | "cor1" -> Assert.AreEqual(["TestCorollary1"; "$2"; "("; ")"], cor1.TypeSignature)
                | "proofCor1" -> Assert.AreEqual(["TestCorollary1"; "$2"; "$1"], proofCor1.TypeSignature)
                | "thm2" -> Assert.AreEqual(["TestTheorem2"; "("; ")"], thm2.TypeSignature)
                | "corThm2" -> Assert.AreEqual(["TestTheorem2"; "$1"; "("; ")"], corThm2.TypeSignature)
                | "lem2" -> Assert.AreEqual(["TestLemma2"; "("; ")"], lem2.TypeSignature)
                | "corLem2" -> Assert.AreEqual(["TestLemma2"; "$1"; "("; ")"], corLem2.TypeSignature)
                | "prp2" -> Assert.AreEqual(["TestProposition2"; "("; ")"], prp2.TypeSignature)
                | "corPrp2" -> Assert.AreEqual(["TestProposition2"; "$1"; "("; ")"], corPrp2.TypeSignature)
                | "cor2" -> Assert.AreEqual(["TestCorollary2"; "$2"; "("; ")"], cor2.TypeSignature)
                | "corCor2" -> Assert.AreEqual(["TestCorollary2"; "$2"; "$1"; "("; ")"], corCor2.TypeSignature)
                | "con1" -> Assert.AreEqual(["TestConjecture"; "("; ")"], con1.TypeSignature)
                | "corCon1" -> Assert.AreEqual(["TestConjecture"; "$1"; "("; ")"], corCon1.TypeSignature)
                | "axi1" -> Assert.AreEqual(["TestAxiom"; "("; ")"], axi1.TypeSignature)
                | "corAxi1"  -> Assert.AreEqual(["TestAxiom"; "$1"; "("; ")"], corAxi1.TypeSignature) 
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
        let res = CommonFplValueTestCases.ScopeProperties() 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual([], r.TypeSignature)
            | "theory" -> Assert.AreEqual([], theory.TypeSignature)
            | "block" -> Assert.AreEqual(["TestId"; "("; ")"], block.TypeSignature)
            | "t1" -> Assert.AreEqual(["T1"; "("; ")"], t1.TypeSignature)
            | "t2" -> Assert.AreEqual(["T2"; "("; ")"], t2.TypeSignature)
            | "t3" -> Assert.AreEqual(["T3"; "("; ")"; "->"; "obj"], t3.TypeSignature)
            | "t4" -> Assert.AreEqual(["T4"; "("; ")"; "->"; "obj"], t4.TypeSignature)
            | "t5" -> Assert.AreEqual(["T5"; "("; ")"; "->"; "ind"], t5.TypeSignature)
            | "t6" -> Assert.AreEqual(["T6"; "("; ")"; "->"; "ind"], t6.TypeSignature)
            | "t7" -> Assert.AreEqual(["T7"; "("; ")"; "->"; "pred"], t7.TypeSignature)
            | "t8" -> Assert.AreEqual(["T8"; "("; ")"; "->"; "pred"], t8.TypeSignature)
            | "t9" -> Assert.AreEqual(["T9"; "("; ")"; "->"; "tpl"], t9.TypeSignature)
            | "t10" -> Assert.AreEqual(["T10"; "("; ")"; "->"; "tpl"], t10.TypeSignature)
            | "t11" -> Assert.AreEqual(["T11"; "("; ")"; "->"; "Nat"], t11.TypeSignature)
            | "t12" -> Assert.AreEqual(["T12"; "("; ")"; "->"; "Nat"], t12.TypeSignature)
            | "t13" -> Assert.AreEqual(["T13"; "("; ")"; "->"; "func"], t13.TypeSignature)
            | "t14" -> Assert.AreEqual(["T14"; "("; ")"; "->"; "func"], t14.TypeSignature)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual([], r.TypeSignature)
            | "theory" -> Assert.AreEqual([], theory.TypeSignature)
            | "block" -> Assert.AreEqual(["TestPredicate"; "("; ")"], block.TypeSignature); 
            | "x" -> Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "xu" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual(["obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual(["obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual(["obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual(["obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual(["obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual(["obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual(["obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual(["obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual(["obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual(["obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual(["obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual(["obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual(["obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual(["obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual(["obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual(["obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual(["obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual(["obj"], ywc.TypeSignature)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual([], r.TypeSignature)
            | "theory" -> Assert.AreEqual([], theory.TypeSignature)
            | "block" -> Assert.AreEqual(["TestPredicate"; "("; ")"], block.TypeSignature); 
            | "x" -> Assert.AreEqual(["+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual(["+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "xu" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual(["*"; "obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual(["*"; "obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual(["*"; "obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual(["*"; "obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual(["*"; "obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual(["*"; "obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual(["*"; "obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual(["*"; "obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual(["*"; "obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual(["*"; "obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual(["*"; "obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual(["*"; "obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual(["*"; "obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual(["*"; "obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual(["*"; "obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual(["*"; "obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual(["*"; "obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual(["*"; "obj"], ywc.TypeSignature)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual([], r.TypeSignature)
            | "theory" -> Assert.AreEqual([], theory.TypeSignature)
            | "block" -> Assert.AreEqual(["TestPredicate"; "("; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"; ")"], block.TypeSignature)
            | "x" -> Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "xu" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual(["obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual(["obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual(["obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual(["obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual(["obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual(["obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual(["obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual(["obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual(["obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual(["obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual(["obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual(["obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual(["obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual(["obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual(["obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual(["obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual(["obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual(["obj"], ywc.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual([], r.TypeSignature)
            | "theory" -> Assert.AreEqual([], theory.TypeSignature)
            | "block" -> Assert.AreEqual(["TestPredicate"; "("; "+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"; "+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"; ")"], block.TypeSignature); 
            | "x" -> Assert.AreEqual(["+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual(["+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "xu" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual(["*"; "obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual(["*"; "obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual(["*"; "obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual(["*"; "obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual(["*"; "obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual(["*"; "obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual(["*"; "obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual(["*"; "obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual(["*"; "obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual(["*"; "obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual(["*"; "obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual(["*"; "obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual(["*"; "obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual(["*"; "obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual(["*"; "obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual(["*"; "obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual(["*"; "obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual(["*"; "obj"], ywc.TypeSignature)
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
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let stOption = prepareFplCode(fplCode, false) 
        prepareFplCode("", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope["Test"]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            match var with
            | "base1" -> Assert.AreEqual(["pred"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual(["pred"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual(["undef"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual(["1."], base1.TypeSignature)
            | "base5" -> Assert.AreEqual(["del."; "Test"; "("; ")"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual(["ind"], base1.TypeSignature)
            | "base7" -> Assert.AreEqual(["bydef."; "Test"; "("; ")"], base1.TypeSignature)
            | "base8" -> Assert.AreEqual(["Test"; "ind"], base1.TypeSignature)
            | "base9" -> Assert.AreEqual(["Test"; "ind"; "("; ")"], base1.TypeSignature)
            | "base10" -> Assert.AreEqual(["Test"], base1.TypeSignature)
            | "base11" -> Assert.AreEqual(["undef"], base1.TypeSignature)
            | "base12" -> Assert.AreEqual(["self"], base1.TypeSignature)
            | "base13" -> Assert.AreEqual(["1"], base1.TypeSignature)
            | "base11a" -> Assert.AreEqual(["undef"; "."; "undef"], base1.TypeSignature)
            | "base12a" -> Assert.AreEqual(["self"; "."; "undef"], base1.TypeSignature)
            | "base10b" -> Assert.AreEqual(["Test"; "("; ")"], base1.TypeSignature)
            | "base11b" -> Assert.AreEqual(["undef"; "("; ")"], base1.TypeSignature)
            | "base12b" -> Assert.AreEqual(["self"; "("; ")"], base1.TypeSignature)
            | "base13b" -> Assert.AreEqual(["1"; "("; ")"], base1.TypeSignature)
            | "base10c" -> Assert.AreEqual(["Test"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base11c" -> Assert.AreEqual(["undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base12c" -> Assert.AreEqual(["self"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base13c" -> Assert.AreEqual(["1"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base10d" -> Assert.AreEqual(["Test"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base11d" -> Assert.AreEqual(["undef"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base12d" -> Assert.AreEqual(["self"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base13d" -> Assert.AreEqual(["1"; "["; "undef"; "."; "undef"; "]"], base1.TypeSignature)
            | "base10e" -> Assert.AreEqual(["Test"; "("; "undef"; "undef"; ")"; "."; "@self"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base11e" -> Assert.AreEqual(["undef"; "("; "undef"; "undef"; ")"; "."; "undef"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base12e" -> Assert.AreEqual(["self"; "("; "undef"; "undef"; ")"; "."; "3"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base13e" -> Assert.AreEqual(["1"; "("; "undef"; "undef"; ")"; "."; "T"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base10f" -> Assert.AreEqual(["Test"; "["; "undef"; "undef"; "]"; "."; "undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base11f" -> Assert.AreEqual(["undef"; "["; "undef"; "undef"; "]"; "."; "undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base12f" -> Assert.AreEqual(["self"; "["; "undef"; "undef"; "]"; "."; "self"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base13f" -> Assert.AreEqual(["1"; "["; "undef"; "."; "undef"; "]"; "."; "T"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base14" -> Assert.AreEqual(["∅"], base1.TypeSignature)
            | "base15" -> Assert.AreEqual(["-"; "undef"], base1.TypeSignature)
            | "base16" -> Assert.AreEqual(["-"; "("; "undef"; ], base1.TypeSignature)
            | "base17" -> Assert.AreEqual([], base1.TypeSignature)
            | "base18" -> Assert.AreEqual([], base1.TypeSignature)
            | "base19" -> Assert.AreEqual([], base1.TypeSignature)
            | "base20" -> Assert.AreEqual([], base1.TypeSignature)
            | "base21" -> Assert.AreEqual(["and"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base21a" -> Assert.AreEqual(["not"; "undef"], base1.TypeSignature)
            | "base21b" -> Assert.AreEqual(["not"; "undef"], base1.TypeSignature)
            | "base22" -> Assert.AreEqual(["xor"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base23" -> Assert.AreEqual(["or"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base24" -> Assert.AreEqual(["iif"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base25" -> Assert.AreEqual(["impl"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base26" -> Assert.AreEqual(["is"; "("; "undef"; "Nat"; ")"], base1.TypeSignature)
            | "base27" -> Assert.AreEqual(["B"; "("; ")"], base1.TypeSignature)
            | "base28" -> Assert.AreEqual(["C"; "("; "undef"; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base29" -> Assert.AreEqual(["D"; "("; "self"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base30" -> Assert.AreEqual(["B"; "("; "In"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base31" -> Assert.AreEqual(["C"; "("; "Test1"; "("; "undef"; ")"; "Test2"; "("; "undef"; "undef"; "undef"; ")"; ")"], base1.TypeSignature)
            | "base32" -> Assert.AreEqual(["E"; "("; "pred"; "undef"; "pred"; ")"], base1.TypeSignature)
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
        FplParser.parserDiagnostics.Clear()
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
        let stOption = prepareFplCode(fplCode, false) 
        prepareFplCode("", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope["Test"]
            let cl = theory.Scope["A"]
            let ctor = cl.Scope["A(T1, func, ind, pred)"]
            let base1 = ctor.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual([], base1.TypeSignature)
            | "base2" -> Assert.AreEqual([], base1.TypeSignature)
            | "base3" -> Assert.AreEqual([], base1.TypeSignature)
            | "base4" -> Assert.AreEqual([], base1.TypeSignature)
            | "base5" -> Assert.AreEqual([], base1.TypeSignature)
            | "base6" -> Assert.AreEqual([], base1.TypeSignature)
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
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let stOption = prepareFplCode(fplCode, false) 
        prepareFplCode("", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope["Test"]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual([], base1.TypeSignature)
            | "base2" -> Assert.AreEqual([], base1.TypeSignature)
            | "base3" -> Assert.AreEqual([], base1.TypeSignature)
            | "base4" -> Assert.AreEqual([], base1.TypeSignature)
            | "base5" -> Assert.AreEqual([], base1.TypeSignature)
            | "base6" -> Assert.AreEqual([], base1.TypeSignature)
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
        FplParser.parserDiagnostics.Clear()
        let fplCode = sprintf "%s;" varVal
        let stOption = prepareFplCode(fplCode, false) 
        prepareFplCode("", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope["Test"]
            let base1 = 
                if varVal.Contains "cl" then 
                    theory.Scope["T1"]
                elif varVal.Contains "func" then 
                    theory.Scope["T1() -> obj"]
                else 
                    theory.Scope["T1()"]

            match var with
            | "base1" -> Assert.AreEqual([], base1.TypeSignature)
            | "base2" -> Assert.AreEqual([], base1.TypeSignature)
            | "base3" -> Assert.AreEqual([], base1.TypeSignature)
            | "base4" -> Assert.AreEqual([], base1.TypeSignature)
            | "base5" -> Assert.AreEqual([], base1.TypeSignature)
            | "base5a" -> Assert.AreEqual([], base1.TypeSignature)
            | "base6" -> Assert.AreEqual([], base1.TypeSignature)
            | "base7" -> Assert.AreEqual([], base1.TypeSignature)
            | "base8" -> Assert.AreEqual([], base1.TypeSignature)
            | "base9" -> Assert.AreEqual([], base1.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
