namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

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

    [<DataRow("base1")>]
    [<DataRow("base2")>]
    [<DataRow("base3")>]
    [<DataRow("base4")>]
    [<DataRow("base5")>]
    [<DataRow("base6")>]
    [<DataRow("base7")>]
    [<DataRow("base8")>]
    [<DataRow("base9")>]
    [<DataRow("base10")>]
    [<DataRow("base11")>]
    [<DataRow("base12")>]
    [<DataRow("base13")>]
    [<DataRow("base11a")>]
    [<DataRow("base12a")>]
    [<DataRow("base10b")>]
    [<DataRow("base11b")>]
    [<DataRow("base12b")>]
    [<DataRow("base13b")>]
    [<DataRow("base10c")>]
    [<DataRow("base11c")>]
    [<DataRow("base12c")>]
    [<DataRow("base13c")>]
    [<DataRow("base10d")>]
    [<DataRow("base11d")>]
    [<DataRow("base12d")>]
    [<DataRow("base13d")>]
    [<DataRow("base10e")>]
    [<DataRow("base11e")>]
    [<DataRow("base12e")>]
    [<DataRow("base13e")>]
    [<DataRow("base10f")>]
    [<DataRow("base11f")>]
    [<DataRow("base12f")>]
    [<DataRow("base13f")>]
    [<DataRow("base14")>]
    [<DataRow("base15")>]
    [<DataRow("base16")>]
    [<DataRow("base17")>]
    [<DataRow("base18")>]
    [<DataRow("base19")>]
    [<DataRow("base20")>]
    [<DataRow("base21")>]
    [<DataRow("base22")>]
    [<DataRow("base23")>]
    [<DataRow("base24")>]
    [<DataRow("base25")>]
    [<DataRow("base26")>]
    [<TestMethod>]
    member this.TestPredicate(var) =
        let result = CommonFplValueTestCases.ScopePredicate()
        match result with
        | Some (theory, base1,base2,base3,base4,base5, base6, base7, 
                                    base8, base9, base10, base11, base12, base13,
                                    base11a, base12a, base10b, base11b, base12b, base13b,
                                    base10c, base11c, base12c, base13c, base10d, base11d,
                                    base12d, base10e, base11e, base12e, base13d, base13e,
                                    base10f, base11f, base12f, base13f, base14, base15,
                                    base16, base17, base18, base19, base20, base21, base22,
                                    base23, base24, base25, base26) ->
            match var with
            | "base1" -> Assert.AreEqual([], base1.TypeSignature)
            | "base2" -> Assert.AreEqual([], base2.TypeSignature)
            | "base3" -> Assert.AreEqual([], base3.TypeSignature)
            | "base4" -> Assert.AreEqual([], base4.TypeSignature)
            | "base5" -> Assert.AreEqual([], base5.TypeSignature)
            | "base6" -> Assert.AreEqual([], base6.TypeSignature)
            | "base7" -> Assert.AreEqual([], base7.TypeSignature)
            | "base8" -> Assert.AreEqual([], base8.TypeSignature)
            | "base9" -> Assert.AreEqual([], base9.TypeSignature)
            | "base10" -> Assert.AreEqual([], base10.TypeSignature)
            | "base11" -> Assert.AreEqual([], base11.TypeSignature)
            | "base12" -> Assert.AreEqual([], base12.TypeSignature)
            | "base13" -> Assert.AreEqual([], base13.TypeSignature)
            | "base11a" -> Assert.AreEqual([], base11a.TypeSignature)
            | "base12a" -> Assert.AreEqual([], base12a.TypeSignature)
            | "base10b" -> Assert.AreEqual([], base10b.TypeSignature)
            | "base11b" -> Assert.AreEqual([], base11b.TypeSignature)
            | "base12b" -> Assert.AreEqual([], base12b.TypeSignature)
            | "base13b" -> Assert.AreEqual([], base13b.TypeSignature)
            | "base10c" -> Assert.AreEqual([], base10c.TypeSignature)
            | "base11c" -> Assert.AreEqual([], base11c.TypeSignature)
            | "base12c" -> Assert.AreEqual([], base12c.TypeSignature)
            | "base13c" -> Assert.AreEqual([], base13c.TypeSignature)
            | "base10d" -> Assert.AreEqual([], base10d.TypeSignature)
            | "base11d" -> Assert.AreEqual([], base11d.TypeSignature)
            | "base12d" -> Assert.AreEqual([], base12d.TypeSignature)
            | "base13d" -> Assert.AreEqual([], base13d.TypeSignature)
            | "base10e" -> Assert.AreEqual([], base10e.TypeSignature)
            | "base11e" -> Assert.AreEqual([], base11e.TypeSignature)
            | "base12e" -> Assert.AreEqual([], base12e.TypeSignature)
            | "base13e" -> Assert.AreEqual([], base13e.TypeSignature)
            | "base10f" -> Assert.AreEqual([], base10f.TypeSignature)
            | "base11f" -> Assert.AreEqual([], base11f.TypeSignature)
            | "base12f" -> Assert.AreEqual([], base12f.TypeSignature)
            | "base13f" -> Assert.AreEqual([], base13f.TypeSignature)
            | "base14" -> Assert.AreEqual([], base14.TypeSignature)
            | "base15" -> Assert.AreEqual([], base15.TypeSignature)
            | "base16" -> Assert.AreEqual([], base16.TypeSignature)
            | "base17" -> Assert.AreEqual([], base17.TypeSignature)
            | "base18" -> Assert.AreEqual([], base18.TypeSignature)
            | "base19" -> Assert.AreEqual([], base19.TypeSignature)
            | "base20" -> Assert.AreEqual([], base20.TypeSignature)
            | "base21" -> Assert.AreEqual([], base21.TypeSignature)
            | "base22" -> Assert.AreEqual([], base22.TypeSignature)
            | "base23" -> Assert.AreEqual([], base23.TypeSignature)
            | "base24" -> Assert.AreEqual([], base24.TypeSignature)
            | "base25" -> Assert.AreEqual([], base25.TypeSignature)
            | "base26" -> Assert.AreEqual([], base26.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)















