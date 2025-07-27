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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Repr))
            | "theory" -> Assert.AreEqual<string>("undef", theory.Type(SignatureType.Repr))
            | "inf1" -> Assert.AreEqual<string>("undetermined", inf1.Type(SignatureType.Repr))
            | "inf2" -> Assert.AreEqual<string>("undetermined", inf2.Type(SignatureType.Repr))
            | "axi1" -> Assert.AreEqual<string>("true", axi1.Type(SignatureType.Repr))
            | "axi2" -> Assert.AreEqual<string>("true", axi2.Type(SignatureType.Repr))
            | "pst1" -> Assert.AreEqual<string>("true", pst1.Type(SignatureType.Repr))
            | "pst2" -> Assert.AreEqual<string>("true", pst2.Type(SignatureType.Repr))
            | "thm1" -> Assert.AreEqual<string>("true", thm1.Type(SignatureType.Repr))
            | "thm2" -> Assert.AreEqual<string>("true", thm2.Type(SignatureType.Repr))
            | "pro1" -> Assert.AreEqual<string>("true", pro1.Type(SignatureType.Repr))
            | "pro2" -> Assert.AreEqual<string>("true", pro2.Type(SignatureType.Repr))
            | "lem1" -> Assert.AreEqual<string>("true", lem1.Type(SignatureType.Repr))
            | "lem2" -> Assert.AreEqual<string>("true", lem2.Type(SignatureType.Repr))
            | "cor1" -> Assert.AreEqual<string>("true", cor1.Type(SignatureType.Repr))
            | "cor2" -> Assert.AreEqual<string>("true", cor2.Type(SignatureType.Repr))
            | "con1" -> Assert.AreEqual<string>("true", con1.Type(SignatureType.Repr))
            | "con2" -> Assert.AreEqual<string>("true", con2.Type(SignatureType.Repr))
            | "cla1" -> Assert.AreEqual<string>("class SomeClass1", cla1.Type(SignatureType.Repr))
            | "cla2" -> Assert.AreEqual<string>("class SomeClass2", cla2.Type(SignatureType.Repr))
            | "pre1" -> Assert.AreEqual<string>("true", pre1.Type(SignatureType.Repr))
            | "pre2" -> Assert.AreEqual<string>("true", pre2.Type(SignatureType.Repr))
            | "fun1" -> Assert.AreEqual<string>("dec obj", fun1.Type(SignatureType.Repr))
            | "fun2" -> Assert.AreEqual<string>("dec obj", fun2.Type(SignatureType.Repr))
            | "prf1" -> Assert.AreEqual<string>("undetermined", prf1.Type(SignatureType.Repr))
            | "prf2" -> Assert.AreEqual<string>("undetermined", prf2.Type(SignatureType.Repr))
            | "loc1" -> Assert.AreEqual<string>("undef", loc1.Type(SignatureType.Repr))
            | "loc2" -> Assert.AreEqual<string>("undef(undef, undef)", loc2.Type(SignatureType.Repr))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Repr))
            | "theory" -> Assert.AreEqual<string>("undef", theory.Type(SignatureType.Repr))
            | "block" -> Assert.AreEqual<string>("class TestId", block.Type(SignatureType.Repr))
            | "t1" -> Assert.AreEqual<string>("obj()", t1.Type(SignatureType.Repr))
            | "t2" -> Assert.AreEqual<string>("obj(dec obj)", t2.Type(SignatureType.Repr))
            | "t3" -> Assert.AreEqual<string>("obj(undetermined)", t3.Type(SignatureType.Repr))
            | "t4" -> Assert.AreEqual<string>("obj(ind)", t4.Type(SignatureType.Repr))
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
                | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Repr))
                | "theory" -> Assert.AreEqual<string>("undef", theory.Type(SignatureType.Repr))
                | "thm1" -> Assert.AreEqual<string>("true", thm1.Type(SignatureType.Repr))
                | "proofThm1" -> Assert.AreEqual<string>("undetermined", proofThm1.Type(SignatureType.Repr))
                | "lem1" -> Assert.AreEqual<string>("true", lem1.Type(SignatureType.Repr))
                | "proofLem1" -> Assert.AreEqual<string>("undetermined", proofLem1.Type(SignatureType.Repr))
                | "prp1" -> Assert.AreEqual<string>("true", prp1.Type(SignatureType.Repr))
                | "proofPrp1" -> Assert.AreEqual<string>("undetermined", proofPrp1.Type(SignatureType.Repr))
                | "cor1" -> Assert.AreEqual<string>("true", cor1.Type(SignatureType.Repr))
                | "proofCor1" -> Assert.AreEqual<string>("undetermined", proofCor1.Type(SignatureType.Repr))
                | "thm2" -> Assert.AreEqual<string>("true", thm2.Type(SignatureType.Repr))
                | "corThm2" -> Assert.AreEqual<string>("true", corThm2.Type(SignatureType.Repr))
                | "lem2" -> Assert.AreEqual<string>("true", lem2.Type(SignatureType.Repr))
                | "corLem2" -> Assert.AreEqual<string>("true", corLem2.Type(SignatureType.Repr))
                | "prp2" -> Assert.AreEqual<string>("true", prp2.Type(SignatureType.Repr))
                | "corPrp2" -> Assert.AreEqual<string>("true", corPrp2.Type(SignatureType.Repr))
                | "cor2" -> Assert.AreEqual<string>("true", cor2.Type(SignatureType.Repr))
                | "corCor2" -> Assert.AreEqual<string>("true", corCor2.Type(SignatureType.Repr))
                | "con1" -> Assert.AreEqual<string>("true", con1.Type(SignatureType.Repr))
                | "corCon1" -> Assert.AreEqual<string>("true", corCon1.Type(SignatureType.Repr))
                | "axi1" -> Assert.AreEqual<string>("true", axi1.Type(SignatureType.Repr))
                | "corAxi1"  -> Assert.AreEqual<string>("true", corAxi1.Type(SignatureType.Repr)) 
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Repr))
            | "theory" -> Assert.AreEqual<string>("undef", theory.Type(SignatureType.Repr))
            | "block" -> Assert.AreEqual<string>("undetermined", block.Type(SignatureType.Repr))
            | "t1" -> Assert.AreEqual<string>("true", t1.Type(SignatureType.Repr))
            | "t2" -> Assert.AreEqual<string>("true", t2.Type(SignatureType.Repr))
            | "t3" -> Assert.AreEqual<string>("dec obj", t3.Type(SignatureType.Repr))
            | "t4" -> Assert.AreEqual<string>("dec obj", t4.Type(SignatureType.Repr))
            | "t5" -> Assert.AreEqual<string>("ind", t5.Type(SignatureType.Repr))
            | "t6" -> Assert.AreEqual<string>("ind", t6.Type(SignatureType.Repr))
            | "t7" -> Assert.AreEqual<string>("undetermined", t7.Type(SignatureType.Repr))
            | "t8" -> Assert.AreEqual<string>("undetermined", t8.Type(SignatureType.Repr))
            | "t9" -> Assert.AreEqual<string>("tpl", t9.Type(SignatureType.Repr))
            | "t10" -> Assert.AreEqual<string>("tpl", t10.Type(SignatureType.Repr))
            | "t11" -> Assert.AreEqual<string>("", t11.Type(SignatureType.Repr))
            | "t12" -> Assert.AreEqual<string>("", t12.Type(SignatureType.Repr))
            | "t13" -> Assert.AreEqual<string>("func", t13.Type(SignatureType.Repr))
            | "t14" -> Assert.AreEqual<string>("func", t14.Type(SignatureType.Repr))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Repr))
            | "theory" -> Assert.AreEqual<string>("undef", theory.Type(SignatureType.Repr))
            | "block" -> Assert.AreEqual<string>("true", block.Type(SignatureType.Repr)); 
            | "x" -> Assert.AreEqual<string>("undetermined", x.Type(SignatureType.Repr))
            | "y" -> Assert.AreEqual<string>("undetermined", y.Type(SignatureType.Repr))
            | "s" -> Assert.AreEqual<string>("undef", s.Type(SignatureType.Repr))
            | "xu" -> Assert.AreEqual<string>("obj", xu.Type(SignatureType.Repr))
            | "xv" -> Assert.AreEqual<string>("obj", xv.Type(SignatureType.Repr))
            | "xw" -> Assert.AreEqual<string>("obj", xw.Type(SignatureType.Repr))
            | "yu" -> Assert.AreEqual<string>("obj", yu.Type(SignatureType.Repr))
            | "yv" -> Assert.AreEqual<string>("obj", yv.Type(SignatureType.Repr))
            | "yw" -> Assert.AreEqual<string>("obj", yw.Type(SignatureType.Repr))
            | "xua" -> Assert.AreEqual<string>("dec obj", xua.Type(SignatureType.Repr))
            | "xub" -> Assert.AreEqual<string>("dec obj", xub.Type(SignatureType.Repr))
            | "xuc" -> Assert.AreEqual<string>("dec obj", xuc.Type(SignatureType.Repr))
            | "xva" -> Assert.AreEqual<string>("dec obj", xva.Type(SignatureType.Repr))
            | "xvb" -> Assert.AreEqual<string>("dec obj", xvb.Type(SignatureType.Repr))
            | "xvc" -> Assert.AreEqual<string>("dec obj", xvc.Type(SignatureType.Repr))
            | "xwa" -> Assert.AreEqual<string>("dec obj", xwa.Type(SignatureType.Repr))
            | "xwb" -> Assert.AreEqual<string>("dec obj", xwb.Type(SignatureType.Repr))
            | "xwc" -> Assert.AreEqual<string>("dec obj", xwc.Type(SignatureType.Repr))
            | "yua" -> Assert.AreEqual<string>("dec obj", yua.Type(SignatureType.Repr))
            | "yub" -> Assert.AreEqual<string>("dec obj", yub.Type(SignatureType.Repr))
            | "yuc" -> Assert.AreEqual<string>("dec obj", yuc.Type(SignatureType.Repr))
            | "yva" -> Assert.AreEqual<string>("dec obj", yva.Type(SignatureType.Repr))
            | "yvb" -> Assert.AreEqual<string>("dec obj", yvb.Type(SignatureType.Repr))
            | "yvc" -> Assert.AreEqual<string>("dec obj", yvc.Type(SignatureType.Repr))
            | "ywa" -> Assert.AreEqual<string>("dec obj", ywa.Type(SignatureType.Repr))
            | "ywb" -> Assert.AreEqual<string>("dec obj", ywb.Type(SignatureType.Repr))
            | "ywc" -> Assert.AreEqual<string>("dec obj", ywc.Type(SignatureType.Repr))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Repr))
            | "theory" -> Assert.AreEqual<string>("undef", theory.Type(SignatureType.Repr))
            | "block" -> Assert.AreEqual<string>("true", block.Type(SignatureType.Repr)); 
            | "x" -> Assert.AreEqual<string>("+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj){}", x.Type(SignatureType.Repr))
            | "y" -> Assert.AreEqual<string>("+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj){}", y.Type(SignatureType.Repr))
            | "xu" -> Assert.AreEqual<string>("obj", xu.Type(SignatureType.Repr))
            | "xv" -> Assert.AreEqual<string>("obj", xv.Type(SignatureType.Repr))
            | "xw" -> Assert.AreEqual<string>("obj", xw.Type(SignatureType.Repr))
            | "yu" -> Assert.AreEqual<string>("obj", yu.Type(SignatureType.Repr))
            | "yv" -> Assert.AreEqual<string>("obj", yv.Type(SignatureType.Repr))
            | "yw" -> Assert.AreEqual<string>("obj", yw.Type(SignatureType.Repr))
            | "xua" -> Assert.AreEqual<string>("*obj{}", xua.Type(SignatureType.Repr))
            | "xub" -> Assert.AreEqual<string>("*obj{}", xub.Type(SignatureType.Repr))
            | "xuc" -> Assert.AreEqual<string>("*obj{}", xuc.Type(SignatureType.Repr))
            | "xva" -> Assert.AreEqual<string>("*obj{}", xva.Type(SignatureType.Repr))
            | "xvb" -> Assert.AreEqual<string>("*obj{}", xvb.Type(SignatureType.Repr))
            | "xvc" -> Assert.AreEqual<string>("*obj{}", xvc.Type(SignatureType.Repr))
            | "xwa" -> Assert.AreEqual<string>("*obj{}", xwa.Type(SignatureType.Repr))
            | "xwb" -> Assert.AreEqual<string>("*obj{}", xwb.Type(SignatureType.Repr))
            | "xwc" -> Assert.AreEqual<string>("*obj{}", xwc.Type(SignatureType.Repr))
            | "yua" -> Assert.AreEqual<string>("*obj{}", yua.Type(SignatureType.Repr))
            | "yub" -> Assert.AreEqual<string>("*obj{}", yub.Type(SignatureType.Repr))
            | "yuc" -> Assert.AreEqual<string>("*obj{}", yuc.Type(SignatureType.Repr))
            | "yva" -> Assert.AreEqual<string>("*obj{}", yva.Type(SignatureType.Repr))
            | "yvb" -> Assert.AreEqual<string>("*obj{}", yvb.Type(SignatureType.Repr))
            | "yvc" -> Assert.AreEqual<string>("*obj{}", yvc.Type(SignatureType.Repr))
            | "ywa" -> Assert.AreEqual<string>("*obj{}", ywa.Type(SignatureType.Repr))
            | "ywb" -> Assert.AreEqual<string>("*obj{}", ywb.Type(SignatureType.Repr))
            | "ywc" -> Assert.AreEqual<string>("*obj{}", ywc.Type(SignatureType.Repr))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Repr))
            | "theory" -> Assert.AreEqual<string>("undef", theory.Type(SignatureType.Repr))
            | "block" -> Assert.AreEqual<string>("true", block.Type(SignatureType.Repr)); 
            | "x" -> Assert.AreEqual<string>("undetermined", x.Type(SignatureType.Repr))
            | "y" -> Assert.AreEqual<string>("undetermined", y.Type(SignatureType.Repr))
            | "xu" -> Assert.AreEqual<string>("obj", xu.Type(SignatureType.Repr))
            | "xv" -> Assert.AreEqual<string>("obj", xv.Type(SignatureType.Repr))
            | "xw" -> Assert.AreEqual<string>("obj", xw.Type(SignatureType.Repr))
            | "yu" -> Assert.AreEqual<string>("obj", yu.Type(SignatureType.Repr))
            | "yv" -> Assert.AreEqual<string>("obj", yv.Type(SignatureType.Repr))
            | "yw" -> Assert.AreEqual<string>("obj", yw.Type(SignatureType.Repr))
            | "xua" -> Assert.AreEqual<string>("dec obj", xua.Type(SignatureType.Repr))
            | "xub" -> Assert.AreEqual<string>("dec obj", xub.Type(SignatureType.Repr))
            | "xuc" -> Assert.AreEqual<string>("dec obj", xuc.Type(SignatureType.Repr))
            | "xva" -> Assert.AreEqual<string>("dec obj", xva.Type(SignatureType.Repr))
            | "xvb" -> Assert.AreEqual<string>("dec obj", xvb.Type(SignatureType.Repr))
            | "xvc" -> Assert.AreEqual<string>("dec obj", xvc.Type(SignatureType.Repr))
            | "xwa" -> Assert.AreEqual<string>("dec obj", xwa.Type(SignatureType.Repr))
            | "xwb" -> Assert.AreEqual<string>("dec obj", xwb.Type(SignatureType.Repr))
            | "xwc" -> Assert.AreEqual<string>("dec obj", xwc.Type(SignatureType.Repr))
            | "yua" -> Assert.AreEqual<string>("dec obj", yua.Type(SignatureType.Repr))
            | "yub" -> Assert.AreEqual<string>("dec obj", yub.Type(SignatureType.Repr))
            | "yuc" -> Assert.AreEqual<string>("dec obj", yuc.Type(SignatureType.Repr))
            | "yva" -> Assert.AreEqual<string>("dec obj", yva.Type(SignatureType.Repr))
            | "yvb" -> Assert.AreEqual<string>("dec obj", yvb.Type(SignatureType.Repr))
            | "yvc" -> Assert.AreEqual<string>("dec obj", yvc.Type(SignatureType.Repr))
            | "ywa" -> Assert.AreEqual<string>("dec obj", ywa.Type(SignatureType.Repr))
            | "ywb" -> Assert.AreEqual<string>("dec obj", ywb.Type(SignatureType.Repr))
            | "ywc" -> Assert.AreEqual<string>("dec obj", ywc.Type(SignatureType.Repr))
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
            | "r" -> Assert.AreEqual<string>("", r.Type(SignatureType.Repr))
            | "theory" -> Assert.AreEqual<string>("undef", theory.Type(SignatureType.Repr))
            | "block" -> Assert.AreEqual<string>("true", block.Type(SignatureType.Repr)); 
            | "x" -> Assert.AreEqual<string>("+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj){}", x.Type(SignatureType.Repr))
            | "y" -> Assert.AreEqual<string>("+pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj){}", y.Type(SignatureType.Repr))
            | "xu" -> Assert.AreEqual<string>("obj", xu.Type(SignatureType.Repr))
            | "xv" -> Assert.AreEqual<string>("obj", xv.Type(SignatureType.Repr))
            | "xw" -> Assert.AreEqual<string>("obj", xw.Type(SignatureType.Repr))
            | "yu" -> Assert.AreEqual<string>("obj", yu.Type(SignatureType.Repr))
            | "yv" -> Assert.AreEqual<string>("obj", yv.Type(SignatureType.Repr))
            | "yw" -> Assert.AreEqual<string>("obj", yw.Type(SignatureType.Repr))
            | "xua" -> Assert.AreEqual<string>("*obj{}", xua.Type(SignatureType.Repr))
            | "xub" -> Assert.AreEqual<string>("*obj{}", xub.Type(SignatureType.Repr))
            | "xuc" -> Assert.AreEqual<string>("*obj{}", xuc.Type(SignatureType.Repr))
            | "xva" -> Assert.AreEqual<string>("*obj{}", xva.Type(SignatureType.Repr))
            | "xvb" -> Assert.AreEqual<string>("*obj{}", xvb.Type(SignatureType.Repr))
            | "xvc" -> Assert.AreEqual<string>("*obj{}", xvc.Type(SignatureType.Repr))
            | "xwa" -> Assert.AreEqual<string>("*obj{}", xwa.Type(SignatureType.Repr))
            | "xwb" -> Assert.AreEqual<string>("*obj{}", xwb.Type(SignatureType.Repr))
            | "xwc" -> Assert.AreEqual<string>("*obj{}", xwc.Type(SignatureType.Repr))
            | "yua" -> Assert.AreEqual<string>("*obj{}", yua.Type(SignatureType.Repr))
            | "yub" -> Assert.AreEqual<string>("*obj{}", yub.Type(SignatureType.Repr))
            | "yuc" -> Assert.AreEqual<string>("*obj{}", yuc.Type(SignatureType.Repr))
            | "yva" -> Assert.AreEqual<string>("*obj{}", yva.Type(SignatureType.Repr))
            | "yvb" -> Assert.AreEqual<string>("*obj{}", yvb.Type(SignatureType.Repr))
            | "yvc" -> Assert.AreEqual<string>("*obj{}", yvc.Type(SignatureType.Repr))
            | "ywa" -> Assert.AreEqual<string>("*obj{}", ywa.Type(SignatureType.Repr))
            | "ywb" -> Assert.AreEqual<string>("*obj{}", ywb.Type(SignatureType.Repr))
            | "ywc" -> Assert.AreEqual<string>("*obj{}", ywc.Type(SignatureType.Repr))
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
            | "base1" -> Assert.AreEqual<string>("true", base1.Type(SignatureType.Repr))
            | "base2" -> Assert.AreEqual<string>("false", base1.Type(SignatureType.Repr))
            | "base3" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base4" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base5" -> Assert.AreEqual<string>("undef(undef)", base1.Type(SignatureType.Repr))
            | "base6" -> Assert.AreEqual<string>("$1", base1.Type(SignatureType.Repr))
            | "base7" -> Assert.AreEqual<string>("undetermined(undef(undef))", base1.Type(SignatureType.Repr))
            | "base8" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base9" -> Assert.AreEqual<string>("undetermined(undef)", base1.Type(SignatureType.Repr))
            | "base10" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base11" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base12" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base13" -> Assert.AreEqual<string>("1", base1.Type(SignatureType.Repr))
            | "base11a" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base12a" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base10b" -> Assert.AreEqual<string>("undef(undef)", base1.Type(SignatureType.Repr))
            | "base11b" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base12b" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base13b" -> Assert.AreEqual<string>("1(undef)", base1.Type(SignatureType.Repr))
            | "base10c" -> Assert.AreEqual<string>("undef(undef, undef)", base1.Type(SignatureType.Repr))
            | "base11c" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base12c" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base13c" -> Assert.AreEqual<string>("1(undef, undef)", base1.Type(SignatureType.Repr))
            | "base10d" -> Assert.AreEqual<string>("undef[undef, undef]", base1.Type(SignatureType.Repr))
            | "base11d" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base12d" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base13d" -> Assert.AreEqual<string>("1[undef]", base1.Type(SignatureType.Repr))
            | "base10e" -> Assert.AreEqual<string>("undef(undef, undef).undef[undef, undef]", base1.Type(SignatureType.Repr))
            | "base11e" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base12e" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base13e" -> Assert.AreEqual<string>("1(undef, undef).undef[undef, undef]", base1.Type(SignatureType.Repr))
            | "base10f" -> Assert.AreEqual<string>("undef[undef, undef].undef", base1.Type(SignatureType.Repr))
            | "base11f" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base12f" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base13f" -> Assert.AreEqual<string>("1[undef].undef(undef, undef)", base1.Type(SignatureType.Repr))
            | "base14" -> Assert.AreEqual<string>("undef", base1.Type(SignatureType.Repr))
            | "base15" -> Assert.AreEqual<string>("-(undef)", base1.Type(SignatureType.Repr))
            | "base15a" -> Assert.AreEqual<string>("'(undef)", base1.Type(SignatureType.Repr))
            | "base15b" -> Assert.AreEqual<string>("'(-(undef))", base1.Type(SignatureType.Repr))
            | "base16" -> Assert.AreEqual<string>("-(*(=(+(undef, undef), 2), undef))", base1.Type(SignatureType.Repr))
            | "base17" -> Assert.AreEqual<string>("'(*(=(+(undef, undef('(undef))), 2), undef))", base1.Type(SignatureType.Repr))
            | "base18" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base19" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base20" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base21" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base21a" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base21b" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base22" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base23" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base24" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base25" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base26" -> Assert.AreEqual<string>("false", base1.Type(SignatureType.Repr))
            | "base27" -> Assert.AreEqual<string>("undef(undef)", base1.Type(SignatureType.Repr))
            | "base28" -> Assert.AreEqual<string>("undef(undef, undef, undef, undef)", base1.Type(SignatureType.Repr))
            | "base29" -> Assert.AreEqual<string>("undef(undef, undef, undef)", base1.Type(SignatureType.Repr))
            | "base30" -> Assert.AreEqual<string>("undef(undef(undef))", base1.Type(SignatureType.Repr))
            | "base31" -> Assert.AreEqual<string>("undef(undef(undef), undef(undef, undef, undef))", base1.Type(SignatureType.Repr))
            | "base32" -> Assert.AreEqual<string>("undef(true, undef, false)", base1.Type(SignatureType.Repr))
            | "base33" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base34" -> Assert.AreEqual<string>("false", base1.Type(SignatureType.Repr))
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
            | "base1" -> Assert.AreEqual<string>("bas", base1.Type(SignatureType.Repr))
            | "base2" -> Assert.AreEqual<string>("bas", base1.Type(SignatureType.Repr))
            | "base3" -> Assert.AreEqual<string>("bas", base1.Type(SignatureType.Repr))
            | "base4" -> Assert.AreEqual<string>("bas", base1.Type(SignatureType.Repr))
            | "base5" -> Assert.AreEqual<string>("bas", base1.Type(SignatureType.Repr))
            | "base6" -> Assert.AreEqual<string>("bas", base1.Type(SignatureType.Repr))
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
            | "base1" -> Assert.AreEqual<string>("undef(undef, undef)", base1.Type(SignatureType.Repr))
            | "base2" -> Assert.AreEqual<string>("undef(undef, undef, undef, undef)", base1.Type(SignatureType.Repr))
            | "base3" -> Assert.AreEqual<string>("undef(undef, undef, undef)", base1.Type(SignatureType.Repr))
            | "base4" -> Assert.AreEqual<string>("undef(undef(undef))", base1.Type(SignatureType.Repr))
            | "base5" -> Assert.AreEqual<string>("undef(undef)", base1.Type(SignatureType.Repr))
            | "base6" -> Assert.AreEqual<string>("undef(undef(undef), undef(undef, undef, undef))", base1.Type(SignatureType.Repr))
            | "base7" -> Assert.AreEqual<string>("undef(true, undef, false)", base1.Type(SignatureType.Repr))
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
            | "base1" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base2" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base3" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base4" -> Assert.AreEqual<string>("undetermined", base1.Type(SignatureType.Repr))
            | "base5" -> Assert.AreEqual<string>("class T1", base1.Type(SignatureType.Repr))
            | "base5a" -> Assert.AreEqual<string>("class T1", base1.Type(SignatureType.Repr))
            | "base6" -> Assert.AreEqual<string>("dec obj", base1.Type(SignatureType.Repr))
            | "base7" -> Assert.AreEqual<string>("dec obj", base1.Type(SignatureType.Repr))
            | "base8" -> Assert.AreEqual<string>("dec obj", base1.Type(SignatureType.Repr))
            | "base9" -> Assert.AreEqual<string>("dec obj", base1.Type(SignatureType.Repr))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("varIndUnset", """def pred T1() { dec ~x:ind; true};""")>]
    [<DataRow("varIndSet0", """def pred T1() { dec ~x:ind x:=$0; true};""")>]
    [<DataRow("varIndSet1", """def pred T1() { dec ~x:ind x:=$1; true};""")>]
    [<DataRow("varIndSet42", """def pred T1() { dec ~x:ind x:=$42; true};""")>]
    [<DataRow("varIndSet100", """def pred T1() { dec ~x:ind x:=$100; true};""")>]
    [<DataRow("varFuncUnset", """def pred T1() { dec ~x:func; true};""")>]
    [<DataRow("varPredUnset", """def pred T1() { dec ~x:pred; true};""")>]
    [<TestMethod>]
    member this.TestVariableRepresentationOtherThanObjects(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "%s;" varVal
        let filename = "TestVariableRepresentationOtherThanObjects"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope["T1()"]
            let varObj = base1.Scope["x"]


            match var with
            | "varIndUnset" -> Assert.AreEqual<string>("ind", varObj.Type(SignatureType.Repr))
            | "varIndSet0" -> Assert.AreEqual<string>("$0", varObj.Type(SignatureType.Repr))
            | "varIndSet1" -> Assert.AreEqual<string>("$1", varObj.Type(SignatureType.Repr))
            | "varIndSet42" -> Assert.AreEqual<string>("$42", varObj.Type(SignatureType.Repr))
            | "varIndSet100" -> Assert.AreEqual<string>("$100", varObj.Type(SignatureType.Repr))
            | "varFuncUnset" -> Assert.AreEqual<string>("func", varObj.Type(SignatureType.Repr))
            | "varPredUnset" -> Assert.AreEqual<string>("undetermined", varObj.Type(SignatureType.Repr))
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("o", "dec obj")>]    // without anything
    [<DataRow("a", "dec class A")>]    // without constructor, without inheritance, without instantiation
    [<DataRow("aI1", "intr A:intr obj")>]  // without constructor, without inheritance, with instantiation (without ())
    [<DataRow("aI2", "intr A:intr obj")>]  // without constructor, without inheritance, with instantiation (with ()) -> should also trigger another error
    [<DataRow("b", "dec class B")>]    // without constructor, with inheritance, without instantiation
    [<DataRow("bI1", "intr B:intr A:intr obj")>]  // without constructor, with inheritance, with instantiation (without ())
    [<DataRow("bI2", "intr B:intr A:intr obj")>]  // without constructor, with inheritance, with instantiation (with ()) -> should also trigger another error
    [<DataRow("c", "dec class C")>]    // with constructor, without inheritance, without instantiation
    [<DataRow("cI1", "C:intr obj")>]  // with constructor, without inheritance, with instantiation (without ()) -> should also trigger another error
    [<DataRow("cI2", "C:obj")>]  // with constructor, without inheritance, with instantiation (with ())
    [<DataRow("d", "dec class D")>]    // with constructor, with inheritance, without instantiation
    [<DataRow("dI1", "D:intr B:intr A:intr obj")>]  // with constructor, with inheritance, with instantiation (without ())
    [<DataRow("dI2", "D:intr B:intr A")>]  
    [<TestMethod>]
    member this.TestVariableRepresentationObjects(var, expected:string) =
        ad.Clear()
        let fplCode = sprintf """uses Fpl.Commons

                def cl A: obj {intr}
                def cl B: A {intr}
                def cl C: obj {ctor C() {dec base.obj (); self }}
                def cl D: B {ctor D() {dec base.B(); self }}

                def pred T() 
                { 
                    dec ~o:object 
                    ~a:A 
                    ~aI1:A aI1:=A 
                    ~aI2:A aI2:=A() 
                    ~b:B 
                    ~bI1:B bI1:=B 
                    ~bI2:B bI2:=B() 
                    ~c:C 
                    ~cI1:C cI1:=C 
                    ~cI2:C cI2:=C() 
                    ~d:D 
                    ~dI1:D dI1:=D 
                    ~dI2:D dI2:=D() 
                    ;

                    (a = b = c = d = o)
                }
        ;""" 
        let filename = "TestVariableRepresentationObjects"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope["T()"]
            let varObj = base1.Scope[var]
            Assert.AreEqual<string>(expected, varObj.Type(SignatureType.Repr))

        | None -> 
            Assert.IsTrue(false)
