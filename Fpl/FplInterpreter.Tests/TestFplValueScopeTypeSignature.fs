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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Type))
            | "theory" -> Assert.AreEqual<string>("TestScopeBlocksTypeSignature", theory.Type(SignatureType.Type))
            | "inf1" -> Assert.AreEqual<string>("pred()", inf1.Type(SignatureType.Type))
            | "inf2" -> Assert.AreEqual<string>("pred()", inf2.Type(SignatureType.Type))
            | "axi1" -> Assert.AreEqual<string>("pred()", axi1.Type(SignatureType.Type))
            | "axi2" -> Assert.AreEqual<string>("pred()", axi2.Type(SignatureType.Type))
            | "pst1" -> Assert.AreEqual<string>("pred()", pst1.Type(SignatureType.Type))
            | "pst2" -> Assert.AreEqual<string>("pred()", pst2.Type(SignatureType.Type))
            | "thm1" -> Assert.AreEqual<string>("pred()", thm1.Type(SignatureType.Type))
            | "thm2" -> Assert.AreEqual<string>("pred()", thm2.Type(SignatureType.Type))
            | "pro1" -> Assert.AreEqual<string>("pred()", pro1.Type(SignatureType.Type))
            | "pro2" -> Assert.AreEqual<string>("pred()", pro2.Type(SignatureType.Type))
            | "lem1" -> Assert.AreEqual<string>("pred()", lem1.Type(SignatureType.Type))
            | "lem2" -> Assert.AreEqual<string>("pred()", lem2.Type(SignatureType.Type))
            | "cor1" -> Assert.AreEqual<string>("pred$1()", cor1.Type(SignatureType.Type))
            | "cor2" -> Assert.AreEqual<string>("pred$1()", cor2.Type(SignatureType.Type))
            | "con1" -> Assert.AreEqual<string>("pred()", con1.Type(SignatureType.Type))
            | "con2" -> Assert.AreEqual<string>("pred()", con2.Type(SignatureType.Type))
            | "cla1" -> Assert.AreEqual<string>("SomeClass1", cla1.Type(SignatureType.Type))
            | "cla2" -> Assert.AreEqual<string>("SomeClass2", cla2.Type(SignatureType.Type))
            | "pre1" -> Assert.AreEqual<string>("pred()", pre1.Type(SignatureType.Type))
            | "pre2" -> Assert.AreEqual<string>("pred()", pre2.Type(SignatureType.Type))
            | "fun1" -> Assert.AreEqual<string>("func() -> obj", fun1.Type(SignatureType.Type))
            | "fun2" -> Assert.AreEqual<string>("func() -> obj", fun2.Type(SignatureType.Type))
            | "prf1" -> Assert.AreEqual<string>("pred$1", prf1.Type(SignatureType.Type))
            | "prf2" -> Assert.AreEqual<string>("pred$1", prf2.Type(SignatureType.Type))
            | "loc1" -> Assert.AreEqual<string>("pred(undef)", loc1.Type(SignatureType.Type))
            | "loc2" -> Assert.AreEqual<string>("Equal(undef, undef)", loc2.Type(SignatureType.Type))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Type))
            | "theory" -> Assert.AreEqual<string>("TestScopeConstructorsTypeSignature", theory.Type(SignatureType.Type))
            | "block" -> Assert.AreEqual<string>("TestId", block.Type(SignatureType.Type))
            | "t1" -> Assert.AreEqual<string>("TestId()", t1.Type(SignatureType.Type))
            | "t2" -> Assert.AreEqual<string>("TestId(obj)", t2.Type(SignatureType.Type))
            | "t3" -> Assert.AreEqual<string>("TestId(pred)", t3.Type(SignatureType.Type))
            | "t4" -> Assert.AreEqual<string>("TestId(ind)", t4.Type(SignatureType.Type))
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
                | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Type))
                | "theory" -> Assert.AreEqual<string>("TestScopeProofsAndCorollariesTypeSignature", theory.Type(SignatureType.Type))
                | "thm1" -> Assert.AreEqual<string>("pred()", thm1.Type(SignatureType.Type))
                | "proofThm1" -> Assert.AreEqual<string>("pred$1", proofThm1.Type(SignatureType.Type))
                | "lem1" -> Assert.AreEqual<string>("pred()", lem1.Type(SignatureType.Type))
                | "proofLem1" -> Assert.AreEqual<string>("pred$1", proofLem1.Type(SignatureType.Type))
                | "prp1" -> Assert.AreEqual<string>("pred()", prp1.Type(SignatureType.Type))
                | "proofPrp1" -> Assert.AreEqual<string>("pred$1", proofPrp1.Type(SignatureType.Type))
                | "cor1" -> Assert.AreEqual<string>("pred$2()", cor1.Type(SignatureType.Type))
                | "proofCor1" -> Assert.AreEqual<string>("pred$2$1", proofCor1.Type(SignatureType.Type))
                | "thm2" -> Assert.AreEqual<string>("pred()", thm2.Type(SignatureType.Type))
                | "corThm2" -> Assert.AreEqual<string>("pred$1()", corThm2.Type(SignatureType.Type))
                | "lem2" -> Assert.AreEqual<string>("pred()", lem2.Type(SignatureType.Type))
                | "corLem2" -> Assert.AreEqual<string>("pred$1()", corLem2.Type(SignatureType.Type))
                | "prp2" -> Assert.AreEqual<string>("pred()", prp2.Type(SignatureType.Type))
                | "corPrp2" -> Assert.AreEqual<string>("pred$1()", corPrp2.Type(SignatureType.Type))
                | "cor2" -> Assert.AreEqual<string>("pred$2()", cor2.Type(SignatureType.Type))
                | "corCor2" -> Assert.AreEqual<string>("pred$2$1()", corCor2.Type(SignatureType.Type))
                | "con1" -> Assert.AreEqual<string>("pred()", con1.Type(SignatureType.Type))
                | "corCon1" -> Assert.AreEqual<string>("pred$1()", corCon1.Type(SignatureType.Type))
                | "axi1" -> Assert.AreEqual<string>("pred()", axi1.Type(SignatureType.Type))
                | "corAxi1"  -> Assert.AreEqual<string>("pred$1()", corAxi1.Type(SignatureType.Type)) 
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Type))
            | "theory" -> Assert.AreEqual<string>("TestScopePropertiesTypeSignature", theory.Type(SignatureType.Type))
            | "block" -> Assert.AreEqual<string>("pred()", block.Type(SignatureType.Type))
            | "t1" -> Assert.AreEqual<string>("pred()", t1.Type(SignatureType.Type))
            | "t2" -> Assert.AreEqual<string>("pred()", t2.Type(SignatureType.Type))
            | "t3" -> Assert.AreEqual<string>("func() -> obj", t3.Type(SignatureType.Type))
            | "t4" -> Assert.AreEqual<string>("func() -> obj", t4.Type(SignatureType.Type))
            | "t5" -> Assert.AreEqual<string>("func() -> ind", t5.Type(SignatureType.Type))
            | "t6" -> Assert.AreEqual<string>("func() -> ind", t6.Type(SignatureType.Type))
            | "t7" -> Assert.AreEqual<string>("func() -> pred", t7.Type(SignatureType.Type))
            | "t8" -> Assert.AreEqual<string>("func() -> pred", t8.Type(SignatureType.Type))
            | "t9" -> Assert.AreEqual<string>("func() -> tpl", t9.Type(SignatureType.Type))
            | "t10" -> Assert.AreEqual<string>("func() -> tpl", t10.Type(SignatureType.Type))
            | "t11" -> Assert.AreEqual<string>("func() -> Nat", t11.Type(SignatureType.Type))
            | "t12" -> Assert.AreEqual<string>("func() -> Nat", t12.Type(SignatureType.Type))
            | "t13" -> Assert.AreEqual<string>("func() -> func", t13.Type(SignatureType.Type))
            | "t14" -> Assert.AreEqual<string>("func() -> func", t14.Type(SignatureType.Type))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Type))
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInBlockTypeSignature", theory.Type(SignatureType.Type))
            | "block" -> Assert.AreEqual<string>("pred()", block.Type(SignatureType.Type)); 
            | "x" -> Assert.AreEqual<string>("pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", x.Type(SignatureType.Type))
            | "y" -> Assert.AreEqual<string>("pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", y.Type(SignatureType.Type))
            | "s" -> Assert.AreEqual<string>("Set", s.Type(SignatureType.Type))
            | "xu" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", xu.Type(SignatureType.Type))
            | "xv" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", xv.Type(SignatureType.Type))
            | "xw" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", xw.Type(SignatureType.Type))
            | "yu" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", yu.Type(SignatureType.Type))
            | "yv" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", yv.Type(SignatureType.Type))
            | "yw" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", yw.Type(SignatureType.Type))
            | "xua" -> Assert.AreEqual<string>("obj", xua.Type(SignatureType.Type))
            | "xub" -> Assert.AreEqual<string>("obj", xub.Type(SignatureType.Type))
            | "xuc" -> Assert.AreEqual<string>("obj", xuc.Type(SignatureType.Type))
            | "xva" -> Assert.AreEqual<string>("obj", xva.Type(SignatureType.Type))
            | "xvb" -> Assert.AreEqual<string>("obj", xvb.Type(SignatureType.Type))
            | "xvc" -> Assert.AreEqual<string>("obj", xvc.Type(SignatureType.Type))
            | "xwa" -> Assert.AreEqual<string>("obj", xwa.Type(SignatureType.Type))
            | "xwb" -> Assert.AreEqual<string>("obj", xwb.Type(SignatureType.Type))
            | "xwc" -> Assert.AreEqual<string>("obj", xwc.Type(SignatureType.Type))
            | "yua" -> Assert.AreEqual<string>("obj", yua.Type(SignatureType.Type))
            | "yub" -> Assert.AreEqual<string>("obj", yub.Type(SignatureType.Type))
            | "yuc" -> Assert.AreEqual<string>("obj", yuc.Type(SignatureType.Type))
            | "yva" -> Assert.AreEqual<string>("obj", yva.Type(SignatureType.Type))
            | "yvb" -> Assert.AreEqual<string>("obj", yvb.Type(SignatureType.Type))
            | "yvc" -> Assert.AreEqual<string>("obj", yvc.Type(SignatureType.Type))
            | "ywa" -> Assert.AreEqual<string>("obj", ywa.Type(SignatureType.Type))
            | "ywb" -> Assert.AreEqual<string>("obj", ywb.Type(SignatureType.Type))
            | "ywc" -> Assert.AreEqual<string>("obj", ywc.Type(SignatureType.Type))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Type))
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInBlockVariadicTypeSignature", theory.Type(SignatureType.Type))
            | "block" -> Assert.AreEqual<string>("pred()", block.Type(SignatureType.Type)); 
            | "x" -> Assert.AreEqual<string>("+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", x.Type(SignatureType.Type))
            | "y" -> Assert.AreEqual<string>("+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", y.Type(SignatureType.Type))
            | "xu" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", xu.Type(SignatureType.Type))
            | "xv" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", xv.Type(SignatureType.Type))
            | "xw" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", xw.Type(SignatureType.Type))
            | "yu" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", yu.Type(SignatureType.Type))
            | "yv" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", yv.Type(SignatureType.Type))
            | "yw" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", yw.Type(SignatureType.Type))
            | "xua" -> Assert.AreEqual<string>("*obj", xua.Type(SignatureType.Type))
            | "xub" -> Assert.AreEqual<string>("*obj", xub.Type(SignatureType.Type))
            | "xuc" -> Assert.AreEqual<string>("*obj", xuc.Type(SignatureType.Type))
            | "xva" -> Assert.AreEqual<string>("*obj", xva.Type(SignatureType.Type))
            | "xvb" -> Assert.AreEqual<string>("*obj", xvb.Type(SignatureType.Type))
            | "xvc" -> Assert.AreEqual<string>("*obj", xvc.Type(SignatureType.Type))
            | "xwa" -> Assert.AreEqual<string>("*obj", xwa.Type(SignatureType.Type))
            | "xwb" -> Assert.AreEqual<string>("*obj", xwb.Type(SignatureType.Type))
            | "xwc" -> Assert.AreEqual<string>("*obj", xwc.Type(SignatureType.Type))
            | "yua" -> Assert.AreEqual<string>("*obj", yua.Type(SignatureType.Type))
            | "yub" -> Assert.AreEqual<string>("*obj", yub.Type(SignatureType.Type))
            | "yuc" -> Assert.AreEqual<string>("*obj", yuc.Type(SignatureType.Type))
            | "yva" -> Assert.AreEqual<string>("*obj", yva.Type(SignatureType.Type))
            | "yvb" -> Assert.AreEqual<string>("*obj", yvb.Type(SignatureType.Type))
            | "yvc" -> Assert.AreEqual<string>("*obj", yvc.Type(SignatureType.Type))
            | "ywa" -> Assert.AreEqual<string>("*obj", ywa.Type(SignatureType.Type))
            | "ywb" -> Assert.AreEqual<string>("*obj", ywb.Type(SignatureType.Type))
            | "ywc" -> Assert.AreEqual<string>("*obj", ywc.Type(SignatureType.Type))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Type))
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureTypeSignature", theory.Type(SignatureType.Type))
            | "block" -> Assert.AreEqual<string>("pred(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", block.Type(SignatureType.Type))
            | "x" -> Assert.AreEqual<string>("pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", x.Type(SignatureType.Type))
            | "y" -> Assert.AreEqual<string>("pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", y.Type(SignatureType.Type))
            | "xu" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", xu.Type(SignatureType.Type))
            | "xv" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", xv.Type(SignatureType.Type))
            | "xw" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", xw.Type(SignatureType.Type))
            | "yu" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", yu.Type(SignatureType.Type))
            | "yv" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", yv.Type(SignatureType.Type))
            | "yw" -> Assert.AreEqual<string>("func(obj, obj, obj) -> obj", yw.Type(SignatureType.Type))
            | "xua" -> Assert.AreEqual<string>("obj", xua.Type(SignatureType.Type))
            | "xub" -> Assert.AreEqual<string>("obj", xub.Type(SignatureType.Type))
            | "xuc" -> Assert.AreEqual<string>("obj", xuc.Type(SignatureType.Type))
            | "xva" -> Assert.AreEqual<string>("obj", xva.Type(SignatureType.Type))
            | "xvb" -> Assert.AreEqual<string>("obj", xvb.Type(SignatureType.Type))
            | "xvc" -> Assert.AreEqual<string>("obj", xvc.Type(SignatureType.Type))
            | "xwa" -> Assert.AreEqual<string>("obj", xwa.Type(SignatureType.Type))
            | "xwb" -> Assert.AreEqual<string>("obj", xwb.Type(SignatureType.Type))
            | "xwc" -> Assert.AreEqual<string>("obj", xwc.Type(SignatureType.Type))
            | "yua" -> Assert.AreEqual<string>("obj", yua.Type(SignatureType.Type))
            | "yub" -> Assert.AreEqual<string>("obj", yub.Type(SignatureType.Type))
            | "yuc" -> Assert.AreEqual<string>("obj", yuc.Type(SignatureType.Type))
            | "yva" -> Assert.AreEqual<string>("obj", yva.Type(SignatureType.Type))
            | "yvb" -> Assert.AreEqual<string>("obj", yvb.Type(SignatureType.Type))
            | "yvc" -> Assert.AreEqual<string>("obj", yvc.Type(SignatureType.Type))
            | "ywa" -> Assert.AreEqual<string>("obj", ywa.Type(SignatureType.Type))
            | "ywb" -> Assert.AreEqual<string>("obj", ywb.Type(SignatureType.Type))
            | "ywc" -> Assert.AreEqual<string>("obj", ywc.Type(SignatureType.Type))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Type))
            | "theory" -> Assert.AreEqual<string>("TestScopeVariablesInSignatureVariadicTypeSignature", theory.Type(SignatureType.Type))
            | "block" -> Assert.AreEqual<string>("pred(+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj), +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj))", block.Type(SignatureType.Type)); 
            | "x" -> Assert.AreEqual<string>("+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", x.Type(SignatureType.Type))
            | "y" -> Assert.AreEqual<string>("+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)", y.Type(SignatureType.Type))
            | "xu" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", xu.Type(SignatureType.Type))
            | "xv" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", xv.Type(SignatureType.Type))
            | "xw" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", xw.Type(SignatureType.Type))
            | "yu" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", yu.Type(SignatureType.Type))
            | "yv" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", yv.Type(SignatureType.Type))
            | "yw" -> Assert.AreEqual<string>("func(*obj, *obj, *obj) -> obj", yw.Type(SignatureType.Type))
            | "xua" -> Assert.AreEqual<string>("*obj", xua.Type(SignatureType.Type))
            | "xub" -> Assert.AreEqual<string>("*obj", xub.Type(SignatureType.Type))
            | "xuc" -> Assert.AreEqual<string>("*obj", xuc.Type(SignatureType.Type))
            | "xva" -> Assert.AreEqual<string>("*obj", xva.Type(SignatureType.Type))
            | "xvb" -> Assert.AreEqual<string>("*obj", xvb.Type(SignatureType.Type))
            | "xvc" -> Assert.AreEqual<string>("*obj", xvc.Type(SignatureType.Type))
            | "xwa" -> Assert.AreEqual<string>("*obj", xwa.Type(SignatureType.Type))
            | "xwb" -> Assert.AreEqual<string>("*obj", xwb.Type(SignatureType.Type))
            | "xwc" -> Assert.AreEqual<string>("*obj", xwc.Type(SignatureType.Type))
            | "yua" -> Assert.AreEqual<string>("*obj", yua.Type(SignatureType.Type))
            | "yub" -> Assert.AreEqual<string>("*obj", yub.Type(SignatureType.Type))
            | "yuc" -> Assert.AreEqual<string>("*obj", yuc.Type(SignatureType.Type))
            | "yva" -> Assert.AreEqual<string>("*obj", yva.Type(SignatureType.Type))
            | "yvb" -> Assert.AreEqual<string>("*obj", yvb.Type(SignatureType.Type))
            | "yvc" -> Assert.AreEqual<string>("*obj", yvc.Type(SignatureType.Type))
            | "ywa" -> Assert.AreEqual<string>("*obj", ywa.Type(SignatureType.Type))
            | "ywb" -> Assert.AreEqual<string>("*obj", ywb.Type(SignatureType.Type))
            | "ywc" -> Assert.AreEqual<string>("*obj", ywc.Type(SignatureType.Type))
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
    [<DataRow("base12", "parent")>]
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
    [<DataRow("base29", "D(parent,b,c)")>]
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
            let base1 = pr1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<string>("pred", base1.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("pred", base1.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("pred", base1.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("del.Test()", base1.Type(SignatureType.Type))
            | "base6" -> Assert.AreEqual<string>("ind", base1.Type(SignatureType.Type))
            | "base7" -> Assert.AreEqual<string>("bydef.Test()", base1.Type(SignatureType.Type))
            | "base8" -> Assert.AreEqual<string>("Test$1", base1.Type(SignatureType.Type))
            | "base9" -> Assert.AreEqual<string>("Test$1()", base1.Type(SignatureType.Type))
            | "base10" -> Assert.AreEqual<string>("Test", base1.Type(SignatureType.Type))
            | "base11" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Type))
            | "base12" -> Assert.AreEqual<string>("parent", base1.Type(SignatureType.Type))
            | "base13" -> Assert.AreEqual<string>("1", base1.Type(SignatureType.Type))
            | "base11a" -> Assert.AreEqual<string>("undef.undef", base1.Type(SignatureType.Type))
            | "base12a" -> Assert.AreEqual<string>("parent.undef", base1.Type(SignatureType.Type))
            | "base10b" -> Assert.AreEqual<string>("Test()", base1.Type(SignatureType.Type))
            | "base11b" -> Assert.AreEqual<string>("undef()", base1.Type(SignatureType.Type))
            | "base12b" -> Assert.AreEqual<string>("parent()", base1.Type(SignatureType.Type))
            | "base13b" -> Assert.AreEqual<string>("1()", base1.Type(SignatureType.Type))
            | "base10c" -> Assert.AreEqual<string>("Test(undef, undef)", base1.Type(SignatureType.Type))
            | "base11c" -> Assert.AreEqual<string>("undef(undef, undef)", base1.Type(SignatureType.Type))
            | "base12c" -> Assert.AreEqual<string>("parent(undef, undef)", base1.Type(SignatureType.Type))
            | "base13c" -> Assert.AreEqual<string>("1(undef, undef)", base1.Type(SignatureType.Type))
            | "base10d" -> Assert.AreEqual<string>("Test[undef, undef]", base1.Type(SignatureType.Type))
            | "base11d" -> Assert.AreEqual<string>("undef[undef, undef]", base1.Type(SignatureType.Type))
            | "base12d" -> Assert.AreEqual<string>("parent[undef, undef]", base1.Type(SignatureType.Type))
            | "base13d" -> Assert.AreEqual<string>("1[undef.undef]", base1.Type(SignatureType.Type))
            | "base10e" -> Assert.AreEqual<string>("Test(undef, undef).parent[undef, undef]", base1.Type(SignatureType.Type))
            | "base11e" -> Assert.AreEqual<string>("undef(undef, undef).undef[undef, undef]", base1.Type(SignatureType.Type))
            | "base12e" -> Assert.AreEqual<string>("parent(undef, undef).3[undef, undef]", base1.Type(SignatureType.Type))
            | "base13e" -> Assert.AreEqual<string>("1(undef, undef).T[undef, undef]", base1.Type(SignatureType.Type))
            | "base10f" -> Assert.AreEqual<string>("Test[undef, undef].undef(undef, undef)", base1.Type(SignatureType.Type))
            | "base11f" -> Assert.AreEqual<string>("undef[undef, undef].undef(undef, undef)", base1.Type(SignatureType.Type))
            | "base12f" -> Assert.AreEqual<string>("parent[undef, undef].parent(undef, undef)", base1.Type(SignatureType.Type))
            | "base13f" -> Assert.AreEqual<string>("1[undef.undef].T(undef, undef)", base1.Type(SignatureType.Type))
            | "base14" -> Assert.AreEqual<string>("∅", base1.Type(SignatureType.Type))
            | "base15" -> Assert.AreEqual<string>("-(undef)", base1.Type(SignatureType.Type))
            | "base15a" -> Assert.AreEqual<string>("'(undef)", base1.Type(SignatureType.Type))
            | "base15b" -> Assert.AreEqual<string>("'(-(undef))", base1.Type(SignatureType.Type))
            | "base16" -> Assert.AreEqual<string>("-(*(=(+(undef, undef), 2), undef))", base1.Type(SignatureType.Type))
            | "base17" -> Assert.AreEqual<string>("'(*(=(+(undef, '(undef)), 2), undef))", base1.Type(SignatureType.Type))
            | "base18" -> Assert.AreEqual<string>("pred(Range(T), C, obj)", base1.Type(SignatureType.Type))
            | "base19" -> Assert.AreEqual<string>("pred$1(obj)", base1.Type(SignatureType.Type))
            | "base20" -> Assert.AreEqual<string>("pred(obj)", base1.Type(SignatureType.Type))
            | "base21" -> Assert.AreEqual<string>("pred(undef, pred(undef, undef))", base1.Type(SignatureType.Type))
            | "base21a" -> Assert.AreEqual<string>("pred(undef)", base1.Type(SignatureType.Type))
            | "base21b" -> Assert.AreEqual<string>("pred(undef)", base1.Type(SignatureType.Type))
            | "base22" -> Assert.AreEqual<string>("pred(undef, pred(undef, undef))", base1.Type(SignatureType.Type))
            | "base23" -> Assert.AreEqual<string>("pred(undef, pred(undef, undef))", base1.Type(SignatureType.Type))
            | "base24" -> Assert.AreEqual<string>("pred(undef, undef)", base1.Type(SignatureType.Type))
            | "base25" -> Assert.AreEqual<string>("pred(undef, undef)", base1.Type(SignatureType.Type))
            | "base26" -> Assert.AreEqual<string>("pred(undef, Nat)", base1.Type(SignatureType.Type))
            | "base27" -> Assert.AreEqual<string>("B()", base1.Type(SignatureType.Type))
            | "base28" -> Assert.AreEqual<string>("C(undef, undef, undef, undef)", base1.Type(SignatureType.Type))
            | "base29" -> Assert.AreEqual<string>("D(parent, undef, undef)", base1.Type(SignatureType.Type))
            | "base30" -> Assert.AreEqual<string>("B(In(undef))", base1.Type(SignatureType.Type))
            | "base31" -> Assert.AreEqual<string>("C(Test1(undef), Test2(undef, undef, undef))", base1.Type(SignatureType.Type))
            | "base32" -> Assert.AreEqual<string>("E(pred, undef, pred)", base1.Type(SignatureType.Type))
            | "base33" -> Assert.AreEqual<string>("pred(obj)", base1.Type(SignatureType.Type))
            | "base34" -> Assert.AreEqual<string>("pred(undef, Set)", base1.Type(SignatureType.Type))
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
            let stmt = ctor.ArgList[0]
            let base1 = stmt.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("B()", base1.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("C(T1, func, ind, pred)", base1.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("D(A, T1, func)", base1.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("B(In(undef))", base1.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("C(Test1(T1), Test2(func, ind, pred))", base1.Type(SignatureType.Type))
            | "base6" -> Assert.AreEqual<string>("E(pred, undef, pred)", base1.Type(SignatureType.Type))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "del.B()")>]
    [<DataRow("base2", "del.C(a,b,c,d)")>]
    [<DataRow("base3", "del.D(parent,b,c)")>]
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
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("del.B()", base1.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("del.C(undef, undef, undef, undef)", base1.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("del.D(parent, undef, undef)", base1.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("del.B(In(undef))", base1.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("del.Test()", base1.Type(SignatureType.Type))
            | "base6" -> Assert.AreEqual<string>("del.C(Test1(undef), Test2(undef, undef, undef))", base1.Type(SignatureType.Type))
            | "base7" -> Assert.AreEqual<string>("del.E(pred, undef, pred)", base1.Type(SignatureType.Type))
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
            | "base1" -> Assert.AreEqual<string>("pred()", base1.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("pred()", base1.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("pred()", base1.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("pred()", base1.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("T1", base1.Type(SignatureType.Type))
            | "base5a" -> Assert.AreEqual<string>("T1", base1.Type(SignatureType.Type))
            | "base6" -> Assert.AreEqual<string>("func() -> obj", base1.Type(SignatureType.Type))
            | "base7" -> Assert.AreEqual<string>("func() -> obj", base1.Type(SignatureType.Type))
            | "base8" -> Assert.AreEqual<string>("func() -> obj", base1.Type(SignatureType.Type))
            | "base9" -> Assert.AreEqual<string>("func() -> obj", base1.Type(SignatureType.Type))
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
        let filename = "TestMappingTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<string>("obj", mapping.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("ind", mapping.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("func", mapping.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("pred", mapping.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("A", mapping.Type(SignatureType.Type))
            | "base6" -> Assert.AreEqual<string>("obj(ind)", mapping.Type(SignatureType.Type))
            | "base7" -> Assert.AreEqual<string>("pred(*obj)", mapping.Type(SignatureType.Type))
            | "base8" -> Assert.AreEqual<string>("func(*pred(obj)) -> pred(ind)", mapping.Type(SignatureType.Type))
            | "base9" -> Assert.AreEqual<string>("pred(+func(A) -> A)", mapping.Type(SignatureType.Type))
            | "base10" -> Assert.AreEqual<string>("A(func(A) -> A)", mapping.Type(SignatureType.Type))
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
        let filename = "TestArgumentTypeSignature"
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
            | "base1" -> Assert.AreEqual<string>("pred", arg.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("pred", arg.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("pred", arg.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("pred", arg.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("pred", arg.Type(SignatureType.Type))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", "true", """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestLanguage(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestLanguageTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]
            let lang = pred.Scope["tex"]

            match var with
            | "base0" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Type))
            | "base1" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("tex", lang.Type(SignatureType.Type))
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
        let filename = "TestLocalizationTypeSignature"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope[predName]

            match var with
            | "base1" -> Assert.AreEqual<string>("pred(undef, undef)", pred.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>("pred(undef)", pred.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>("pred(undef, undef)", pred.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>("Equal(undef, undef)", pred.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>("NotEqual(undef, undef)", pred.Type(SignatureType.Type))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", "true", """!tex: "1" !eng: "true" !ger: "wahr";""")>]
    [<DataRow("base1", "iif(x, y)", """!tex: x " \Leftrightarrow " y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""")>]
    [<DataRow("base2", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x;""")>]
    [<DataRow("base3", "and(p, q)", """!tex: p " \wedge " q !eng: p " and " q !ger: p " und " q;""")>]
    [<DataRow("base4", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y;""")>]
    [<DataRow("base5", "NotEqual(x, y)", """!tex: x "\neq " y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y;""")>]
    [<TestMethod>]
    member this.TestTranslation(var, predName, trslCode) =
        ad.Clear()
        let fplCode = sprintf """loc %s := %s;""" predName trslCode
        let filename = "TestTranslationTypeSignature"
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
            | "base0" -> Assert.AreEqual<string>(@"1", trsl.Type(SignatureType.Type))
            | "base1" -> Assert.AreEqual<string>(@"x \Leftrightarrow y", trsl.Type(SignatureType.Type))
            | "base2" -> Assert.AreEqual<string>(@"\neg(x)", trsl.Type(SignatureType.Type))
            | "base3" -> Assert.AreEqual<string>(@"p \wedge q", trsl.Type(SignatureType.Type))
            | "base4" -> Assert.AreEqual<string>(@"x=y", trsl.Type(SignatureType.Type))
            | "base5" -> Assert.AreEqual<string>(@"x\neq y", trsl.Type(SignatureType.Type))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)
