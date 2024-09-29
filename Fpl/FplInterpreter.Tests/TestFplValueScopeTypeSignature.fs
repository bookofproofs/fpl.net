namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
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
        let res = CommonFplValueTestCases.ScopeBlocks("TypeSignature") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string list>([], r.TypeSignature)
            | "theory" -> Assert.AreEqual<string list>([], theory.TypeSignature)
            | "inf1" -> Assert.AreEqual<string list>(["SomeInference1"; "("; ")"], inf1.TypeSignature)
            | "inf2" -> Assert.AreEqual<string list>(["SomeInference2"; "("; ")"], inf2.TypeSignature)
            | "axi1" -> Assert.AreEqual<string list>(["SomeAxiom1"; "("; ")"], axi1.TypeSignature)
            | "axi2" -> Assert.AreEqual<string list>(["SomeAxiom2"; "("; ")"], axi2.TypeSignature)
            | "pst1" -> Assert.AreEqual<string list>(["SomePostulate1"; "("; ")"], pst1.TypeSignature)
            | "pst2" -> Assert.AreEqual<string list>(["SomePostulate2"; "("; ")"], pst2.TypeSignature)
            | "thm1" -> Assert.AreEqual<string list>(["SomeTheorem1"; "("; ")"], thm1.TypeSignature)
            | "thm2" -> Assert.AreEqual<string list>(["SomeTheorem2"; "("; ")"], thm2.TypeSignature)
            | "pro1" -> Assert.AreEqual<string list>(["SomeProposition1"; "("; ")"], pro1.TypeSignature)
            | "pro2" -> Assert.AreEqual<string list>(["SomeProposition2"; "("; ")"], pro2.TypeSignature)
            | "lem1" -> Assert.AreEqual<string list>(["SomeLemma1"; "("; ")"], lem1.TypeSignature)
            | "lem2" -> Assert.AreEqual<string list>(["SomeLemma2"; "("; ")"], lem2.TypeSignature)
            | "cor1" -> Assert.AreEqual<string list>(["SomeLemma1"; "ind"; "("; ")"], cor1.TypeSignature)
            | "cor2" -> Assert.AreEqual<string list>(["SomeLemma2"; "ind"; "("; ")"], cor2.TypeSignature)
            | "con1" -> Assert.AreEqual<string list>(["SomeConjecture1"; "("; ")"], con1.TypeSignature)
            | "con2" -> Assert.AreEqual<string list>(["SomeConjecture2"; "("; ")"], con2.TypeSignature)
            | "cla1" -> Assert.AreEqual<string list>(["SomeClass1"], cla1.TypeSignature)
            | "cla2" -> Assert.AreEqual<string list>(["SomeClass2"], cla2.TypeSignature)
            | "pre1" -> Assert.AreEqual<string list>(["SomePredicate1"; "("; ")"], pre1.TypeSignature)
            | "pre2" -> Assert.AreEqual<string list>(["SomePredicate2"; "("; ")"], pre2.TypeSignature)
            | "fun1" -> Assert.AreEqual<string list>(["SomeFunctionalTerm1"; "("; ")"; "->"; "obj"], fun1.TypeSignature)
            | "fun2" -> Assert.AreEqual<string list>(["SomeFunctionalTerm2"; "("; ")"; "->"; "obj"], fun2.TypeSignature)
            | "prf1" -> Assert.AreEqual<string list>(["SomeTheorem1"; "ind"], prf1.TypeSignature)
            | "prf2" -> Assert.AreEqual<string list>(["SomeTheorem2"; "ind"], prf2.TypeSignature)
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
        let res = CommonFplValueTestCases.ScopeConstructors("TypeSignature") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string list>([], r.TypeSignature)
            | "theory" -> Assert.AreEqual<string list>([], theory.TypeSignature)
            | "block" -> Assert.AreEqual<string list>(["TestId"], block.TypeSignature)
            | "t1" -> Assert.AreEqual<string list>(["TestId"; "("; ")"], t1.TypeSignature)
            | "t2" -> Assert.AreEqual<string list>(["TestId"; "("; "obj"; ")"], t2.TypeSignature)
            | "t3" -> Assert.AreEqual<string list>(["TestId"; "("; "pred"; ")"], t3.TypeSignature)
            | "t4" -> Assert.AreEqual<string list>(["TestId"; "("; "ind"; ")"], t4.TypeSignature)
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("TypeSignature") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<string list>([], r.TypeSignature)
                | "theory" -> Assert.AreEqual<string list>([], theory.TypeSignature)
                | "thm1" -> Assert.AreEqual<string list>(["TestTheorem1"; "("; ")"], thm1.TypeSignature)
                | "proofThm1" -> Assert.AreEqual<string list>(["TestTheorem1"; "ind"], proofThm1.TypeSignature)
                | "lem1" -> Assert.AreEqual<string list>(["TestLemma1"; "("; ")"], lem1.TypeSignature)
                | "proofLem1" -> Assert.AreEqual<string list>(["TestLemma1"; "ind"], proofLem1.TypeSignature)
                | "prp1" -> Assert.AreEqual<string list>(["TestProposition1"; "("; ")"], prp1.TypeSignature)
                | "proofPrp1" -> Assert.AreEqual<string list>(["TestProposition1"; "ind"], proofPrp1.TypeSignature)
                | "cor1" -> Assert.AreEqual<string list>(["TestCorollary1"; "ind"; "("; ")"], cor1.TypeSignature)
                | "proofCor1" -> Assert.AreEqual<string list>(["TestCorollary1"; "ind"; "ind"], proofCor1.TypeSignature)
                | "thm2" -> Assert.AreEqual<string list>(["TestTheorem2"; "("; ")"], thm2.TypeSignature)
                | "corThm2" -> Assert.AreEqual<string list>(["TestTheorem2"; "ind"; "("; ")"], corThm2.TypeSignature)
                | "lem2" -> Assert.AreEqual<string list>(["TestLemma2"; "("; ")"], lem2.TypeSignature)
                | "corLem2" -> Assert.AreEqual<string list>(["TestLemma2"; "ind"; "("; ")"], corLem2.TypeSignature)
                | "prp2" -> Assert.AreEqual<string list>(["TestProposition2"; "("; ")"], prp2.TypeSignature)
                | "corPrp2" -> Assert.AreEqual<string list>(["TestProposition2"; "ind"; "("; ")"], corPrp2.TypeSignature)
                | "cor2" -> Assert.AreEqual<string list>(["TestCorollary2"; "ind"; "("; ")"], cor2.TypeSignature)
                | "corCor2" -> Assert.AreEqual<string list>(["TestCorollary2"; "ind"; "ind"; "("; ")"], corCor2.TypeSignature)
                | "con1" -> Assert.AreEqual<string list>(["TestConjecture"; "("; ")"], con1.TypeSignature)
                | "corCon1" -> Assert.AreEqual<string list>(["TestConjecture"; "ind"; "("; ")"], corCon1.TypeSignature)
                | "axi1" -> Assert.AreEqual<string list>(["TestAxiom"; "("; ")"], axi1.TypeSignature)
                | "corAxi1"  -> Assert.AreEqual<string list>(["TestAxiom"; "ind"; "("; ")"], corAxi1.TypeSignature) 
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
        let res = CommonFplValueTestCases.ScopeProperties("TypeSignature") 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string list>([], r.TypeSignature)
            | "theory" -> Assert.AreEqual<string list>([], theory.TypeSignature)
            | "block" -> Assert.AreEqual<string list>(["TestId"; "("; ")"], block.TypeSignature)
            | "t1" -> Assert.AreEqual<string list>(["T1"; "("; ")"], t1.TypeSignature)
            | "t2" -> Assert.AreEqual<string list>(["T2"; "("; ")"], t2.TypeSignature)
            | "t3" -> Assert.AreEqual<string list>(["T3"; "("; ")"; "->"; "obj"], t3.TypeSignature)
            | "t4" -> Assert.AreEqual<string list>(["T4"; "("; ")"; "->"; "obj"], t4.TypeSignature)
            | "t5" -> Assert.AreEqual<string list>(["T5"; "("; ")"; "->"; "ind"], t5.TypeSignature)
            | "t6" -> Assert.AreEqual<string list>(["T6"; "("; ")"; "->"; "ind"], t6.TypeSignature)
            | "t7" -> Assert.AreEqual<string list>(["T7"; "("; ")"; "->"; "pred"], t7.TypeSignature)
            | "t8" -> Assert.AreEqual<string list>(["T8"; "("; ")"; "->"; "pred"], t8.TypeSignature)
            | "t9" -> Assert.AreEqual<string list>(["T9"; "("; ")"; "->"; "tpl"], t9.TypeSignature)
            | "t10" -> Assert.AreEqual<string list>(["T10"; "("; ")"; "->"; "tpl"], t10.TypeSignature)
            | "t11" -> Assert.AreEqual<string list>(["T11"; "("; ")"; "->"; "Nat"], t11.TypeSignature)
            | "t12" -> Assert.AreEqual<string list>(["T12"; "("; ")"; "->"; "Nat"], t12.TypeSignature)
            | "t13" -> Assert.AreEqual<string list>(["T13"; "("; ")"; "->"; "func"], t13.TypeSignature)
            | "t14" -> Assert.AreEqual<string list>(["T14"; "("; ")"; "->"; "func"], t14.TypeSignature)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("TypeSignature")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string list>([], r.TypeSignature)
            | "theory" -> Assert.AreEqual<string list>([], theory.TypeSignature)
            | "block" -> Assert.AreEqual<string list>(["TestPredicate"; "("; ")"], block.TypeSignature); 
            | "x" -> Assert.AreEqual<string list>(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual<string list>(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "s" -> Assert.AreEqual<string list>(["Set"], s.TypeSignature)
            | "xu" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual<string list>(["obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual<string list>(["obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual<string list>(["obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual<string list>(["obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual<string list>(["obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual<string list>(["obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual<string list>(["obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual<string list>(["obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual<string list>(["obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual<string list>(["obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual<string list>(["obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual<string list>(["obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual<string list>(["obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual<string list>(["obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual<string list>(["obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual<string list>(["obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual<string list>(["obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual<string list>(["obj"], ywc.TypeSignature)
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("TypeSignature")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string list>([], r.TypeSignature)
            | "theory" -> Assert.AreEqual<string list>([], theory.TypeSignature)
            | "block" -> Assert.AreEqual<string list>(["TestPredicate"; "("; ")"], block.TypeSignature); 
            | "x" -> Assert.AreEqual<string list>(["+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual<string list>(["+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "xu" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual<string list>(["*"; "obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual<string list>(["*"; "obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual<string list>(["*"; "obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual<string list>(["*"; "obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual<string list>(["*"; "obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual<string list>(["*"; "obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual<string list>(["*"; "obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual<string list>(["*"; "obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual<string list>(["*"; "obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual<string list>(["*"; "obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual<string list>(["*"; "obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual<string list>(["*"; "obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual<string list>(["*"; "obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual<string list>(["*"; "obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual<string list>(["*"; "obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual<string list>(["*"; "obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual<string list>(["*"; "obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual<string list>(["*"; "obj"], ywc.TypeSignature)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("TypeSignature")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string list>([], r.TypeSignature)
            | "theory" -> Assert.AreEqual<string list>([], theory.TypeSignature)
            | "block" -> Assert.AreEqual<string list>(["TestPredicate"; "("; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"; ")"], block.TypeSignature)
            | "x" -> Assert.AreEqual<string list>(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual<string list>(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "xu" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual<string list>(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual<string list>(["obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual<string list>(["obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual<string list>(["obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual<string list>(["obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual<string list>(["obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual<string list>(["obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual<string list>(["obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual<string list>(["obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual<string list>(["obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual<string list>(["obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual<string list>(["obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual<string list>(["obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual<string list>(["obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual<string list>(["obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual<string list>(["obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual<string list>(["obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual<string list>(["obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual<string list>(["obj"], ywc.TypeSignature)
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("TypeSignature")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string list>([], r.TypeSignature)
            | "theory" -> Assert.AreEqual<string list>([], theory.TypeSignature)
            | "block" -> Assert.AreEqual<string list>(["TestPredicate"; "("; "+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"; "+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"; ")"], block.TypeSignature); 
            | "x" -> Assert.AreEqual<string list>(["+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual<string list>(["+"; "pred"; "("; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; "func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "xu" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual<string list>(["func"; "("; "*"; "obj"; "*"; "obj"; "*"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual<string list>(["*"; "obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual<string list>(["*"; "obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual<string list>(["*"; "obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual<string list>(["*"; "obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual<string list>(["*"; "obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual<string list>(["*"; "obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual<string list>(["*"; "obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual<string list>(["*"; "obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual<string list>(["*"; "obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual<string list>(["*"; "obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual<string list>(["*"; "obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual<string list>(["*"; "obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual<string list>(["*"; "obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual<string list>(["*"; "obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual<string list>(["*"; "obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual<string list>(["*"; "obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual<string list>(["*"; "obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual<string list>(["*"; "obj"], ywc.TypeSignature)
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
        let filename = "TestPredicateTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]
            match var with
            | "base1" -> Assert.AreEqual<string list>(["pred"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual<string list>(["pred"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual<string list>(["undef"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual<string list>(["1."], base1.TypeSignature)
            | "base5" -> Assert.AreEqual<string list>(["del."; "Test"; "("; ")"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual<string list>(["ind"], base1.TypeSignature)
            | "base7" -> Assert.AreEqual<string list>(["bydef."; "Test"; "("; ")"], base1.TypeSignature)
            | "base8" -> Assert.AreEqual<string list>(["Test"; "ind"], base1.TypeSignature)
            | "base9" -> Assert.AreEqual<string list>(["Test"; "ind"; "("; ")"], base1.TypeSignature)
            | "base10" -> Assert.AreEqual<string list>(["Test"], base1.TypeSignature)
            | "base11" -> Assert.AreEqual<string list>(["undef"], base1.TypeSignature)
            | "base12" -> Assert.AreEqual<string list>(["self"], base1.TypeSignature)
            | "base13" -> Assert.AreEqual<string list>(["1"], base1.TypeSignature)
            | "base11a" -> Assert.AreEqual<string list>(["undef"; "."; "undef"], base1.TypeSignature)
            | "base12a" -> Assert.AreEqual<string list>(["self"; "."; "undef"], base1.TypeSignature)
            | "base10b" -> Assert.AreEqual<string list>(["Test"; "("; ")"], base1.TypeSignature)
            | "base11b" -> Assert.AreEqual<string list>(["undef"; "("; ")"], base1.TypeSignature)
            | "base12b" -> Assert.AreEqual<string list>(["self"; "("; ")"], base1.TypeSignature)
            | "base13b" -> Assert.AreEqual<string list>(["1"; "("; ")"], base1.TypeSignature)
            | "base10c" -> Assert.AreEqual<string list>(["Test"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base11c" -> Assert.AreEqual<string list>(["undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base12c" -> Assert.AreEqual<string list>(["self"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base13c" -> Assert.AreEqual<string list>(["1"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base10d" -> Assert.AreEqual<string list>(["Test"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base11d" -> Assert.AreEqual<string list>(["undef"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base12d" -> Assert.AreEqual<string list>(["self"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base13d" -> Assert.AreEqual<string list>(["1"; "["; "undef"; "."; "undef"; "]"], base1.TypeSignature)
            | "base10e" -> Assert.AreEqual<string list>(["Test"; "("; "undef"; "undef"; ")"; "."; "@self"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base11e" -> Assert.AreEqual<string list>(["undef"; "("; "undef"; "undef"; ")"; "."; "undef"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base12e" -> Assert.AreEqual<string list>(["self"; "("; "undef"; "undef"; ")"; "."; "3"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base13e" -> Assert.AreEqual<string list>(["1"; "("; "undef"; "undef"; ")"; "."; "T"; "["; "undef"; "undef"; "]"], base1.TypeSignature)
            | "base10f" -> Assert.AreEqual<string list>(["Test"; "["; "undef"; "undef"; "]"; "."; "undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base11f" -> Assert.AreEqual<string list>(["undef"; "["; "undef"; "undef"; "]"; "."; "undef"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base12f" -> Assert.AreEqual<string list>(["self"; "["; "undef"; "undef"; "]"; "."; "self"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base13f" -> Assert.AreEqual<string list>(["1"; "["; "undef"; "."; "undef"; "]"; "."; "T"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base14" -> Assert.AreEqual<string list>(["∅"], base1.TypeSignature)
            | "base15" -> Assert.AreEqual<string list>(["-"; "("; "undef"; ")"], base1.TypeSignature)
            | "base15a" -> Assert.AreEqual<string list>(["'"; "("; "undef"; ")"], base1.TypeSignature)
            | "base15b" -> Assert.AreEqual<string list>(["'"; "("; "-"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base16" -> Assert.AreEqual<string list>(["-"; "("; "+"; "("; "undef"; "="; "("; "undef"; "*"; "("; "2"; "undef"; ")"; ")"; ")"; ")"], base1.TypeSignature)
            | "base17" -> Assert.AreEqual<string list>(["'"; "("; "+"; "("; "undef"; "="; "("; "'"; "("; "undef"; ")"; "*"; "("; "2"; "undef"; ")"; ")"; ")"; ")"], base1.TypeSignature)
            | "base18" -> Assert.AreEqual<string list>(["ex"], base1.TypeSignature)
            | "base19" -> Assert.AreEqual<string list>(["exn"], base1.TypeSignature)
            | "base20" -> Assert.AreEqual<string list>(["all"], base1.TypeSignature)
            | "base21" -> Assert.AreEqual<string list>(["and"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base21a" -> Assert.AreEqual<string list>(["not"; "("; "undef"; ")"], base1.TypeSignature)
            | "base21b" -> Assert.AreEqual<string list>(["not"; "("; "undef"; ")"], base1.TypeSignature)
            | "base22" -> Assert.AreEqual<string list>(["xor"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base23" -> Assert.AreEqual<string list>(["or"; "("; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base24" -> Assert.AreEqual<string list>(["iif"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base25" -> Assert.AreEqual<string list>(["impl"; "("; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base26" -> Assert.AreEqual<string list>(["is"; "("; "undef"; "Nat"; ")"], base1.TypeSignature)
            | "base27" -> Assert.AreEqual<string list>(["B"; "("; ")"], base1.TypeSignature)
            | "base28" -> Assert.AreEqual<string list>(["C"; "("; "undef"; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base29" -> Assert.AreEqual<string list>(["D"; "("; "self"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base30" -> Assert.AreEqual<string list>(["B"; "("; "In"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base31" -> Assert.AreEqual<string list>(["C"; "("; "Test1"; "("; "undef"; ")"; "Test2"; "("; "undef"; "undef"; "undef"; ")"; ")"], base1.TypeSignature)
            | "base32" -> Assert.AreEqual<string list>(["E"; "("; "pred"; "undef"; "pred"; ")"], base1.TypeSignature)
            | "base33" -> Assert.AreEqual<string list>(["pred"; "("; "obj"; ")"], base1.TypeSignature)
            | "base34" -> Assert.AreEqual<string list>(["is"; "("; "undef"; "Set"; ")"], base1.TypeSignature)
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
        let filename = "TestCallConstructorParentClassTypeSignature"
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
            | "base1" -> Assert.AreEqual<string list>(["B"; "("; ")"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual<string list>(["C"; "("; "T1"; "func"; "ind"; "pred"; ")"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual<string list>(["D"; "("; "self"; "T1"; "func"; ")"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual<string list>(["B"; "("; "In"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base5" -> Assert.AreEqual<string list>(["C"; "("; "Test1"; "("; "T1"; ")"; "Test2"; "("; "func"; "ind"; "pred"; ")"; ")"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual<string list>(["E"; "("; "pred"; "undef"; "pred"; ")"], base1.TypeSignature)
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
        let filename = "TestDelegateTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ValueList[0]

            match var with
            | "base1" -> Assert.AreEqual<string list>(["del."; "B"; "("; ")"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual<string list>(["del."; "C"; "("; "undef"; "undef"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual<string list>(["del."; "D"; "("; "self"; "undef"; "undef"; ")"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual<string list>(["del."; "B"; "("; "In"; "("; "undef"; ")"; ")"], base1.TypeSignature)
            | "base5" -> Assert.AreEqual<string list>(["del."; "Test"; "("; ")"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual<string list>(["del."; "C"; "("; "Test1"; "("; "undef"; ")"; "Test2"; "("; "undef"; "undef"; "undef"; ")"; ")"], base1.TypeSignature)
            | "base7" -> Assert.AreEqual<string list>(["del."; "E"; "("; "pred"; "undef"; "pred"; ")"], base1.TypeSignature)
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
        let filename = "TestFixNotationTypeSignature"
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
            | "base1" -> Assert.AreEqual<string list>(["T1"; "("; ")"], base1.TypeSignature)
            | "base2" -> Assert.AreEqual<string list>(["T1"; "("; ")"], base1.TypeSignature)
            | "base3" -> Assert.AreEqual<string list>(["T1"; "("; ")"], base1.TypeSignature)
            | "base4" -> Assert.AreEqual<string list>(["T1"; "("; ")"], base1.TypeSignature)
            | "base5" -> Assert.AreEqual<string list>(["T1"], base1.TypeSignature)
            | "base5a" -> Assert.AreEqual<string list>(["T1"], base1.TypeSignature)
            | "base6" -> Assert.AreEqual<string list>(["T1"; "("; ")"; "->"; "obj"], base1.TypeSignature)
            | "base7" -> Assert.AreEqual<string list>(["T1"; "("; ")"; "->"; "obj"], base1.TypeSignature)
            | "base8" -> Assert.AreEqual<string list>(["T1"; "("; ")"; "->"; "obj"], base1.TypeSignature)
            | "base9" -> Assert.AreEqual<string list>(["T1"; "("; ")"; "->"; "obj"], base1.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
