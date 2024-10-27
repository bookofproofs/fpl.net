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
    [<DataRow("loc1")>]
    [<DataRow("loc2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = CommonFplValueTestCases.ScopeBlocks("TypeSignature") 
        match res with
        | Some (r:FplValue,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>("", r.Type(true))
            | "theory" -> Assert.AreEqual<string>("", theory.Type(true))
            | "inf1" -> Assert.AreEqual<string>("SomeInference1()", inf1.Type(true))
            | "inf2" -> Assert.AreEqual<string>("SomeInference2()", inf2.Type(true))
            | "axi1" -> Assert.AreEqual<string>("SomeAxiom1()", axi1.Type(true))
            | "axi2" -> Assert.AreEqual<string>("SomeAxiom2()", axi2.Type(true))
            | "pst1" -> Assert.AreEqual<string>("SomePostulate1()", pst1.Type(true))
            | "pst2" -> Assert.AreEqual<string>("SomePostulate2()", pst2.Type(true))
            | "thm1" -> Assert.AreEqual<string>("SomeTheorem1()", thm1.Type(true))
            | "thm2" -> Assert.AreEqual<string>("SomeTheorem2()", thm2.Type(true))
            | "pro1" -> Assert.AreEqual<string>("SomeProposition1()", pro1.Type(true))
            | "pro2" -> Assert.AreEqual<string>("SomeProposition2()", pro2.Type(true))
            | "lem1" -> Assert.AreEqual<string>("SomeLemma1()", lem1.Type(true))
            | "lem2" -> Assert.AreEqual<string>("SomeLemma2()", lem2.Type(true))
            | "cor1" -> Assert.AreEqual<string>("SomeLemma1$1()", cor1.Type(true))
            | "cor2" -> Assert.AreEqual<string>("SomeLemma2$1()", cor2.Type(true))
            | "con1" -> Assert.AreEqual<string>("SomeConjecture1()", con1.Type(true))
            | "con2" -> Assert.AreEqual<string>("SomeConjecture2()", con2.Type(true))
            | "cla1" -> Assert.AreEqual<string>("SomeClass1", cla1.Type(true))
            | "cla2" -> Assert.AreEqual<string>("SomeClass2", cla2.Type(true))
            | "pre1" -> Assert.AreEqual<string>("SomePredicate1()", pre1.Type(true))
            | "pre2" -> Assert.AreEqual<string>("SomePredicate2()", pre2.Type(true))
            | "fun1" -> Assert.AreEqual<string>("SomeFunctionalTerm1()->obj", fun1.Type(true))
            | "fun2" -> Assert.AreEqual<string>("SomeFunctionalTerm2()->obj", fun2.Type(true))
            | "prf1" -> Assert.AreEqual<string>("SomeTheorem1$1", prf1.Type(true))
            | "prf2" -> Assert.AreEqual<string>("SomeTheorem2$1", prf2.Type(true))
            | "loc1" -> Assert.AreEqual<string>("not(undef)", loc1.Type(true))
            | "loc2" -> Assert.AreEqual<string>("Equal(undefundef)", loc2.Type(true))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(true))
            | "theory" -> Assert.AreEqual<string>("", theory.Type(true))
            | "block" -> Assert.AreEqual<string>("TestId", block.Type(true))
            | "t1" -> Assert.AreEqual<string>("TestId()", t1.Type(true))
            | "t2" -> Assert.AreEqual<string>("TestId(obj)", t2.Type(true))
            | "t3" -> Assert.AreEqual<string>("TestId(pred)", t3.Type(true))
            | "t4" -> Assert.AreEqual<string>("TestId(ind)", t4.Type(true))
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
                | "r" -> Assert.AreEqual<string>("", r.Type(true))
                | "theory" -> Assert.AreEqual<string>("", theory.Type(true))
                | "thm1" -> Assert.AreEqual<string>("TestTheorem1()", thm1.Type(true))
                | "proofThm1" -> Assert.AreEqual<string>("TestTheorem1$1", proofThm1.Type(true))
                | "lem1" -> Assert.AreEqual<string>("TestLemma1()", lem1.Type(true))
                | "proofLem1" -> Assert.AreEqual<string>("TestLemma1$1", proofLem1.Type(true))
                | "prp1" -> Assert.AreEqual<string>("TestProposition1()", prp1.Type(true))
                | "proofPrp1" -> Assert.AreEqual<string>("TestProposition1$1", proofPrp1.Type(true))
                | "cor1" -> Assert.AreEqual<string>("TestCorollary1$2()", cor1.Type(true))
                | "proofCor1" -> Assert.AreEqual<string>("TestCorollary1$2$1", proofCor1.Type(true))
                | "thm2" -> Assert.AreEqual<string>("TestTheorem2()", thm2.Type(true))
                | "corThm2" -> Assert.AreEqual<string>("TestTheorem2$1()", corThm2.Type(true))
                | "lem2" -> Assert.AreEqual<string>("TestLemma2()", lem2.Type(true))
                | "corLem2" -> Assert.AreEqual<string>("TestLemma2$1()", corLem2.Type(true))
                | "prp2" -> Assert.AreEqual<string>("TestProposition2()", prp2.Type(true))
                | "corPrp2" -> Assert.AreEqual<string>("TestProposition2$1()", corPrp2.Type(true))
                | "cor2" -> Assert.AreEqual<string>("TestCorollary2$2()", cor2.Type(true))
                | "corCor2" -> Assert.AreEqual<string>("TestCorollary2$2$1()", corCor2.Type(true))
                | "con1" -> Assert.AreEqual<string>("TestConjecture()", con1.Type(true))
                | "corCon1" -> Assert.AreEqual<string>("TestConjecture$1()", corCon1.Type(true))
                | "axi1" -> Assert.AreEqual<string>("TestAxiom()", axi1.Type(true))
                | "corAxi1"  -> Assert.AreEqual<string>("TestAxiom$1()", corAxi1.Type(true)) 
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
            | "r" -> Assert.AreEqual<string>("", r.Type(true))
            | "theory" -> Assert.AreEqual<string>("", theory.Type(true))
            | "block" -> Assert.AreEqual<string>("TestId()", block.Type(true))
            | "t1" -> Assert.AreEqual<string>("T1()", t1.Type(true))
            | "t2" -> Assert.AreEqual<string>("T2()", t2.Type(true))
            | "t3" -> Assert.AreEqual<string>("T3()->obj", t3.Type(true))
            | "t4" -> Assert.AreEqual<string>("T4()->obj", t4.Type(true))
            | "t5" -> Assert.AreEqual<string>("T5()->ind", t5.Type(true))
            | "t6" -> Assert.AreEqual<string>("T6()->ind", t6.Type(true))
            | "t7" -> Assert.AreEqual<string>("T7()->pred", t7.Type(true))
            | "t8" -> Assert.AreEqual<string>("T8()->pred", t8.Type(true))
            | "t9" -> Assert.AreEqual<string>("T9()->tpl", t9.Type(true))
            | "t10" -> Assert.AreEqual<string>("T10()->tpl", t10.Type(true))
            | "t11" -> Assert.AreEqual<string>("T11()->Nat", t11.Type(true))
            | "t12" -> Assert.AreEqual<string>("T12()->Nat", t12.Type(true))
            | "t13" -> Assert.AreEqual<string>("T13()->func", t13.Type(true))
            | "t14" -> Assert.AreEqual<string>("T14()->func", t14.Type(true))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(true))
            | "theory" -> Assert.AreEqual<string>("", theory.Type(true))
            | "block" -> Assert.AreEqual<string>("TestPredicate()", block.Type(true)); 
            | "x" -> Assert.AreEqual<string>("pred(func(objobjobj)->objfunc(objobjobj)->objfunc(objobjobj)->obj)", x.Type(true))
            | "y" -> Assert.AreEqual<string>("pred(func(objobjobj)->objfunc(objobjobj)->objfunc(objobjobj)->obj)", y.Type(true))
            | "s" -> Assert.AreEqual<string>("Set", s.Type(true))
            | "xu" -> Assert.AreEqual<string>("func(objobjobj)->obj", xu.Type(true))
            | "xv" -> Assert.AreEqual<string>("func(objobjobj)->obj", xv.Type(true))
            | "xw" -> Assert.AreEqual<string>("func(objobjobj)->obj", xw.Type(true))
            | "yu" -> Assert.AreEqual<string>("func(objobjobj)->obj", yu.Type(true))
            | "yv" -> Assert.AreEqual<string>("func(objobjobj)->obj", yv.Type(true))
            | "yw" -> Assert.AreEqual<string>("func(objobjobj)->obj", yw.Type(true))
            | "xua" -> Assert.AreEqual<string>("obj", xua.Type(true))
            | "xub" -> Assert.AreEqual<string>("obj", xub.Type(true))
            | "xuc" -> Assert.AreEqual<string>("obj", xuc.Type(true))
            | "xva" -> Assert.AreEqual<string>("obj", xva.Type(true))
            | "xvb" -> Assert.AreEqual<string>("obj", xvb.Type(true))
            | "xvc" -> Assert.AreEqual<string>("obj", xvc.Type(true))
            | "xwa" -> Assert.AreEqual<string>("obj", xwa.Type(true))
            | "xwb" -> Assert.AreEqual<string>("obj", xwb.Type(true))
            | "xwc" -> Assert.AreEqual<string>("obj", xwc.Type(true))
            | "yua" -> Assert.AreEqual<string>("obj", yua.Type(true))
            | "yub" -> Assert.AreEqual<string>("obj", yub.Type(true))
            | "yuc" -> Assert.AreEqual<string>("obj", yuc.Type(true))
            | "yva" -> Assert.AreEqual<string>("obj", yva.Type(true))
            | "yvb" -> Assert.AreEqual<string>("obj", yvb.Type(true))
            | "yvc" -> Assert.AreEqual<string>("obj", yvc.Type(true))
            | "ywa" -> Assert.AreEqual<string>("obj", ywa.Type(true))
            | "ywb" -> Assert.AreEqual<string>("obj", ywb.Type(true))
            | "ywc" -> Assert.AreEqual<string>("obj", ywc.Type(true))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(true))
            | "theory" -> Assert.AreEqual<string>("", theory.Type(true))
            | "block" -> Assert.AreEqual<string>("TestPredicate()", block.Type(true)); 
            | "x" -> Assert.AreEqual<string>("+pred(func(*obj*obj*obj)->objfunc(*obj*obj*obj)->objfunc(*obj*obj*obj)->obj)", x.Type(true))
            | "y" -> Assert.AreEqual<string>("+pred(func(*obj*obj*obj)->objfunc(*obj*obj*obj)->objfunc(*obj*obj*obj)->obj)", y.Type(true))
            | "xu" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", xu.Type(true))
            | "xv" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", xv.Type(true))
            | "xw" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", xw.Type(true))
            | "yu" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", yu.Type(true))
            | "yv" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", yv.Type(true))
            | "yw" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", yw.Type(true))
            | "xua" -> Assert.AreEqual<string>("*obj", xua.Type(true))
            | "xub" -> Assert.AreEqual<string>("*obj", xub.Type(true))
            | "xuc" -> Assert.AreEqual<string>("*obj", xuc.Type(true))
            | "xva" -> Assert.AreEqual<string>("*obj", xva.Type(true))
            | "xvb" -> Assert.AreEqual<string>("*obj", xvb.Type(true))
            | "xvc" -> Assert.AreEqual<string>("*obj", xvc.Type(true))
            | "xwa" -> Assert.AreEqual<string>("*obj", xwa.Type(true))
            | "xwb" -> Assert.AreEqual<string>("*obj", xwb.Type(true))
            | "xwc" -> Assert.AreEqual<string>("*obj", xwc.Type(true))
            | "yua" -> Assert.AreEqual<string>("*obj", yua.Type(true))
            | "yub" -> Assert.AreEqual<string>("*obj", yub.Type(true))
            | "yuc" -> Assert.AreEqual<string>("*obj", yuc.Type(true))
            | "yva" -> Assert.AreEqual<string>("*obj", yva.Type(true))
            | "yvb" -> Assert.AreEqual<string>("*obj", yvb.Type(true))
            | "yvc" -> Assert.AreEqual<string>("*obj", yvc.Type(true))
            | "ywa" -> Assert.AreEqual<string>("*obj", ywa.Type(true))
            | "ywb" -> Assert.AreEqual<string>("*obj", ywb.Type(true))
            | "ywc" -> Assert.AreEqual<string>("*obj", ywc.Type(true))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(true))
            | "theory" -> Assert.AreEqual<string>("", theory.Type(true))
            | "block" -> Assert.AreEqual<string>("TestPredicate(pred(func(objobjobj)->objfunc(objobjobj)->objfunc(objobjobj)->obj)pred(func(objobjobj)->objfunc(objobjobj)->objfunc(objobjobj)->obj))", block.Type(true))
            | "x" -> Assert.AreEqual<string>("pred(func(objobjobj)->objfunc(objobjobj)->objfunc(objobjobj)->obj)", x.Type(true))
            | "y" -> Assert.AreEqual<string>("pred(func(objobjobj)->objfunc(objobjobj)->objfunc(objobjobj)->obj)", y.Type(true))
            | "xu" -> Assert.AreEqual<string>("func(objobjobj)->obj", xu.Type(true))
            | "xv" -> Assert.AreEqual<string>("func(objobjobj)->obj", xv.Type(true))
            | "xw" -> Assert.AreEqual<string>("func(objobjobj)->obj", xw.Type(true))
            | "yu" -> Assert.AreEqual<string>("func(objobjobj)->obj", yu.Type(true))
            | "yv" -> Assert.AreEqual<string>("func(objobjobj)->obj", yv.Type(true))
            | "yw" -> Assert.AreEqual<string>("func(objobjobj)->obj", yw.Type(true))
            | "xua" -> Assert.AreEqual<string>("obj", xua.Type(true))
            | "xub" -> Assert.AreEqual<string>("obj", xub.Type(true))
            | "xuc" -> Assert.AreEqual<string>("obj", xuc.Type(true))
            | "xva" -> Assert.AreEqual<string>("obj", xva.Type(true))
            | "xvb" -> Assert.AreEqual<string>("obj", xvb.Type(true))
            | "xvc" -> Assert.AreEqual<string>("obj", xvc.Type(true))
            | "xwa" -> Assert.AreEqual<string>("obj", xwa.Type(true))
            | "xwb" -> Assert.AreEqual<string>("obj", xwb.Type(true))
            | "xwc" -> Assert.AreEqual<string>("obj", xwc.Type(true))
            | "yua" -> Assert.AreEqual<string>("obj", yua.Type(true))
            | "yub" -> Assert.AreEqual<string>("obj", yub.Type(true))
            | "yuc" -> Assert.AreEqual<string>("obj", yuc.Type(true))
            | "yva" -> Assert.AreEqual<string>("obj", yva.Type(true))
            | "yvb" -> Assert.AreEqual<string>("obj", yvb.Type(true))
            | "yvc" -> Assert.AreEqual<string>("obj", yvc.Type(true))
            | "ywa" -> Assert.AreEqual<string>("obj", ywa.Type(true))
            | "ywb" -> Assert.AreEqual<string>("obj", ywb.Type(true))
            | "ywc" -> Assert.AreEqual<string>("obj", ywc.Type(true))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(true))
            | "theory" -> Assert.AreEqual<string>("", theory.Type(true))
            | "block" -> Assert.AreEqual<string>("TestPredicate(+pred(func(*obj*obj*obj)->objfunc(*obj*obj*obj)->objfunc(*obj*obj*obj)->obj)+pred(func(*obj*obj*obj)->objfunc(*obj*obj*obj)->objfunc(*obj*obj*obj)->obj))", block.Type(true)); 
            | "x" -> Assert.AreEqual<string>("+pred(func(*obj*obj*obj)->objfunc(*obj*obj*obj)->objfunc(*obj*obj*obj)->obj)", x.Type(true))
            | "y" -> Assert.AreEqual<string>("+pred(func(*obj*obj*obj)->objfunc(*obj*obj*obj)->objfunc(*obj*obj*obj)->obj)", y.Type(true))
            | "xu" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", xu.Type(true))
            | "xv" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", xv.Type(true))
            | "xw" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", xw.Type(true))
            | "yu" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", yu.Type(true))
            | "yv" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", yv.Type(true))
            | "yw" -> Assert.AreEqual<string>("func(*obj*obj*obj)->obj", yw.Type(true))
            | "xua" -> Assert.AreEqual<string>("*obj", xua.Type(true))
            | "xub" -> Assert.AreEqual<string>("*obj", xub.Type(true))
            | "xuc" -> Assert.AreEqual<string>("*obj", xuc.Type(true))
            | "xva" -> Assert.AreEqual<string>("*obj", xva.Type(true))
            | "xvb" -> Assert.AreEqual<string>("*obj", xvb.Type(true))
            | "xvc" -> Assert.AreEqual<string>("*obj", xvc.Type(true))
            | "xwa" -> Assert.AreEqual<string>("*obj", xwa.Type(true))
            | "xwb" -> Assert.AreEqual<string>("*obj", xwb.Type(true))
            | "xwc" -> Assert.AreEqual<string>("*obj", xwc.Type(true))
            | "yua" -> Assert.AreEqual<string>("*obj", yua.Type(true))
            | "yub" -> Assert.AreEqual<string>("*obj", yub.Type(true))
            | "yuc" -> Assert.AreEqual<string>("*obj", yuc.Type(true))
            | "yva" -> Assert.AreEqual<string>("*obj", yva.Type(true))
            | "yvb" -> Assert.AreEqual<string>("*obj", yvb.Type(true))
            | "yvc" -> Assert.AreEqual<string>("*obj", yvc.Type(true))
            | "ywa" -> Assert.AreEqual<string>("*obj", ywa.Type(true))
            | "ywb" -> Assert.AreEqual<string>("*obj", ywb.Type(true))
            | "ywc" -> Assert.AreEqual<string>("*obj", ywc.Type(true))
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
    [<DataRow("base18", "ex x is Range(a:T), y is C, z {and (a,b,c)}")>]
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
            | "base1" -> Assert.AreEqual<string>("pred", base1.Type(true))
            | "base2" -> Assert.AreEqual<string>("pred", base1.Type(true))
            | "base3" -> Assert.AreEqual<string>("undef", base1.Type(true))
            | "base4" -> Assert.AreEqual<string>("1.", base1.Type(true))
            | "base5" -> Assert.AreEqual<string>("del.Test()", base1.Type(true))
            | "base6" -> Assert.AreEqual<string>("ind", base1.Type(true))
            | "base7" -> Assert.AreEqual<string>("bydef.Test()", base1.Type(true))
            | "base8" -> Assert.AreEqual<string>("Test$1", base1.Type(true))
            | "base9" -> Assert.AreEqual<string>("Test$1()", base1.Type(true))
            | "base10" -> Assert.AreEqual<string>("Test", base1.Type(true))
            | "base11" -> Assert.AreEqual<string>("undef", base1.Type(true))
            | "base12" -> Assert.AreEqual<string>("self", base1.Type(true))
            | "base13" -> Assert.AreEqual<string>("1", base1.Type(true))
            | "base11a" -> Assert.AreEqual<string>("undef.undef", base1.Type(true))
            | "base12a" -> Assert.AreEqual<string>("self.undef", base1.Type(true))
            | "base10b" -> Assert.AreEqual<string>("Test()", base1.Type(true))
            | "base11b" -> Assert.AreEqual<string>("undef()", base1.Type(true))
            | "base12b" -> Assert.AreEqual<string>("self()", base1.Type(true))
            | "base13b" -> Assert.AreEqual<string>("1()", base1.Type(true))
            | "base10c" -> Assert.AreEqual<string>("Test(undefundef)", base1.Type(true))
            | "base11c" -> Assert.AreEqual<string>("undef(undefundef)", base1.Type(true))
            | "base12c" -> Assert.AreEqual<string>("self(undefundef)", base1.Type(true))
            | "base13c" -> Assert.AreEqual<string>("1(undefundef)", base1.Type(true))
            | "base10d" -> Assert.AreEqual<string>("Test[undefundef]", base1.Type(true))
            | "base11d" -> Assert.AreEqual<string>("undef[undefundef]", base1.Type(true))
            | "base12d" -> Assert.AreEqual<string>("self[undefundef]", base1.Type(true))
            | "base13d" -> Assert.AreEqual<string>("1[undef.undef]", base1.Type(true))
            | "base10e" -> Assert.AreEqual<string>("Test(undefundef).@self[undefundef]", base1.Type(true))
            | "base11e" -> Assert.AreEqual<string>("undef(undefundef).undef[undefundef]", base1.Type(true))
            | "base12e" -> Assert.AreEqual<string>("self(undefundef).3[undefundef]", base1.Type(true))
            | "base13e" -> Assert.AreEqual<string>("1(undefundef).T[undefundef]", base1.Type(true))
            | "base10f" -> Assert.AreEqual<string>("Test[undefundef].undef(undefundef)", base1.Type(true))
            | "base11f" -> Assert.AreEqual<string>("undef[undefundef].undef(undefundef)", base1.Type(true))
            | "base12f" -> Assert.AreEqual<string>("self[undefundef].self(undefundef)", base1.Type(true))
            | "base13f" -> Assert.AreEqual<string>("1[undef.undef].T(undefundef)", base1.Type(true))
            | "base14" -> Assert.AreEqual<string>("∅", base1.Type(true))
            | "base15" -> Assert.AreEqual<string>("-(undef)", base1.Type(true))
            | "base15a" -> Assert.AreEqual<string>("'(undef)", base1.Type(true))
            | "base15b" -> Assert.AreEqual<string>("'(-(undef))", base1.Type(true))
            | "base16" -> Assert.AreEqual<string>("-(+(undef=(undef*(2undef))))", base1.Type(true))
            | "base17" -> Assert.AreEqual<string>("'(+(undef=('(undef)*(2undef))))", base1.Type(true))
            | "base18" -> Assert.AreEqual<string>("ex", base1.Type(true))
            | "base19" -> Assert.AreEqual<string>("exn", base1.Type(true))
            | "base20" -> Assert.AreEqual<string>("all", base1.Type(true))
            | "base21" -> Assert.AreEqual<string>("and(undefundefundef)", base1.Type(true))
            | "base21a" -> Assert.AreEqual<string>("not(undef)", base1.Type(true))
            | "base21b" -> Assert.AreEqual<string>("not(undef)", base1.Type(true))
            | "base22" -> Assert.AreEqual<string>("xor(undefundefundef)", base1.Type(true))
            | "base23" -> Assert.AreEqual<string>("or(undefundefundef)", base1.Type(true))
            | "base24" -> Assert.AreEqual<string>("iif(undefundef)", base1.Type(true))
            | "base25" -> Assert.AreEqual<string>("impl(undefundef)", base1.Type(true))
            | "base26" -> Assert.AreEqual<string>("is(undefNat)", base1.Type(true))
            | "base27" -> Assert.AreEqual<string>("B()", base1.Type(true))
            | "base28" -> Assert.AreEqual<string>("C(undefundefundefundef)", base1.Type(true))
            | "base29" -> Assert.AreEqual<string>("D(selfundefundef)", base1.Type(true))
            | "base30" -> Assert.AreEqual<string>("B(In(undef))", base1.Type(true))
            | "base31" -> Assert.AreEqual<string>("C(Test1(undef)Test2(undefundefundef))", base1.Type(true))
            | "base32" -> Assert.AreEqual<string>("E(predundefpred)", base1.Type(true))
            | "base33" -> Assert.AreEqual<string>("pred(obj)", base1.Type(true))
            | "base34" -> Assert.AreEqual<string>("is(undefSet)", base1.Type(true))
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
            | "base1" -> Assert.AreEqual<string>("B()", base1.Type(true))
            | "base2" -> Assert.AreEqual<string>("C(T1funcindpred)", base1.Type(true))
            | "base3" -> Assert.AreEqual<string>("D(selfT1func)", base1.Type(true))
            | "base4" -> Assert.AreEqual<string>("B(In(undef))", base1.Type(true))
            | "base5" -> Assert.AreEqual<string>("C(Test1(T1)Test2(funcindpred))", base1.Type(true))
            | "base6" -> Assert.AreEqual<string>("E(predundefpred)", base1.Type(true))
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
            | "base1" -> Assert.AreEqual<string>("del.B()", base1.Type(true))
            | "base2" -> Assert.AreEqual<string>("del.C(undefundefundefundef)", base1.Type(true))
            | "base3" -> Assert.AreEqual<string>("del.D(selfundefundef)", base1.Type(true))
            | "base4" -> Assert.AreEqual<string>("del.B(In(undef))", base1.Type(true))
            | "base5" -> Assert.AreEqual<string>("del.Test()", base1.Type(true))
            | "base6" -> Assert.AreEqual<string>("del.C(Test1(undef)Test2(undefundefundef))", base1.Type(true))
            | "base7" -> Assert.AreEqual<string>("del.E(predundefpred)", base1.Type(true))
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
            | "base1" -> Assert.AreEqual<string>("T1()", base1.Type(true))
            | "base2" -> Assert.AreEqual<string>("T1()", base1.Type(true))
            | "base3" -> Assert.AreEqual<string>("T1()", base1.Type(true))
            | "base4" -> Assert.AreEqual<string>("T1()", base1.Type(true))
            | "base5" -> Assert.AreEqual<string>("T1", base1.Type(true))
            | "base5a" -> Assert.AreEqual<string>("T1", base1.Type(true))
            | "base6" -> Assert.AreEqual<string>("T1()->obj", base1.Type(true))
            | "base7" -> Assert.AreEqual<string>("T1()->obj", base1.Type(true))
            | "base8" -> Assert.AreEqual<string>("T1()->obj", base1.Type(true))
            | "base9" -> Assert.AreEqual<string>("T1()->obj", base1.Type(true))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
