namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplGrammarCommons
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
        let res = CommonFplValueTestCases.ScopeBlocks("FplRepresentation") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>(literalUndef, getRepresentation r)
            | "theory" -> Assert.AreEqual<string>(literalUndef, getRepresentation theory)
            | "inf1" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation inf1)
            | "inf2" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation inf2)
            | "axi1" -> Assert.AreEqual<string>(literalTrue, getRepresentation axi1)
            | "axi2" -> Assert.AreEqual<string>(literalTrue, getRepresentation axi2)
            | "pst1" -> Assert.AreEqual<string>(literalTrue, getRepresentation pst1)
            | "pst2" -> Assert.AreEqual<string>(literalTrue, getRepresentation pst2)
            | "thm1" -> Assert.AreEqual<string>(literalTrue, getRepresentation thm1)
            | "thm2" -> Assert.AreEqual<string>(literalTrue, getRepresentation thm2)
            | "pro1" -> Assert.AreEqual<string>(literalTrue, getRepresentation pro1)
            | "pro2" -> Assert.AreEqual<string>(literalTrue, getRepresentation pro2)
            | "lem1" -> Assert.AreEqual<string>(literalTrue, getRepresentation lem1)
            | "lem2" -> Assert.AreEqual<string>(literalTrue, getRepresentation lem2)
            | "cor1" -> Assert.AreEqual<string>(literalTrue, getRepresentation cor1)
            | "cor2" -> Assert.AreEqual<string>(literalTrue, getRepresentation cor2)
            | "con1" -> Assert.AreEqual<string>(literalTrue, getRepresentation con1)
            | "con2" -> Assert.AreEqual<string>(literalTrue, getRepresentation con2)
            | "cla1" -> Assert.AreEqual<string>("dec cl SomeClass1", getRepresentation cla1)
            | "cla2" -> Assert.AreEqual<string>("dec cl SomeClass2", getRepresentation cla2)
            | "pre1" -> Assert.AreEqual<string>(literalTrue, getRepresentation pre1)
            | "pre2" -> Assert.AreEqual<string>(literalTrue, getRepresentation pre2)
            | "fun1" -> Assert.AreEqual<string>("dec obj", getRepresentation fun1)
            | "fun2" -> Assert.AreEqual<string>("dec obj", getRepresentation fun2)
            | "fun3" -> Assert.AreEqual<string>("dec obj", getRepresentation fun3)
            | "fun4" -> Assert.AreEqual<string>("dec obj(pred)", getRepresentation fun4)
            | "fun5" -> Assert.AreEqual<string>("dec SomeClass1", getRepresentation fun5)
            | "fun6" -> Assert.AreEqual<string>("dec SomeClass1", getRepresentation fun6)
            | "fun7" -> Assert.AreEqual<string>("dec SomeClass1", getRepresentation fun7)
            | "fun8" -> Assert.AreEqual<string>("dec ind", getRepresentation fun8)
            | "fun9" -> Assert.AreEqual<string>("dec ind", getRepresentation fun9)
            | "prf1" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation prf1)
            | "prf2" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation prf2)
            | "loc1" -> Assert.AreEqual<string>(literalUndef, getRepresentation loc1)
            | "loc2" -> Assert.AreEqual<string>("undef(undef, undef)", getRepresentation loc2)
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
            | "r" -> Assert.AreEqual<string>(literalUndef, getRepresentation r)
            | "theory" -> Assert.AreEqual<string>(literalUndef, getRepresentation theory)
            | "block" -> Assert.AreEqual<string>("class TestId", getRepresentation block)
            | "t1" -> Assert.AreEqual<string>("obj()", getRepresentation t1)
            | "t2" -> Assert.AreEqual<string>("obj(dec obj)", getRepresentation t2)
            | "t3" -> Assert.AreEqual<string>("obj(undetermined)", getRepresentation t3)
            | "t4" -> Assert.AreEqual<string>("obj(ind)", getRepresentation t4)
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
                | "r" -> Assert.AreEqual<string>(literalUndef, getRepresentation r)
                | "theory" -> Assert.AreEqual<string>(literalUndef, getRepresentation theory)
                | "thm1" -> Assert.AreEqual<string>(literalTrue, getRepresentation thm1)
                | "proofThm1" -> Assert.AreEqual<string>(literalTrue, getRepresentation proofThm1)
                | "lem1" -> Assert.AreEqual<string>(literalTrue, getRepresentation lem1)
                | "proofLem1" -> Assert.AreEqual<string>(literalTrue, getRepresentation proofLem1)
                | "prp1" -> Assert.AreEqual<string>(literalTrue, getRepresentation prp1)
                | "proofPrp1" -> Assert.AreEqual<string>(literalTrue, getRepresentation proofPrp1)
                | "cor1" -> Assert.AreEqual<string>(literalTrue, getRepresentation cor1)
                | "proofCor1" -> Assert.AreEqual<string>(literalTrue, getRepresentation proofCor1)
                | "thm2" -> Assert.AreEqual<string>(literalTrue, getRepresentation thm2)
                | "corThm2" -> Assert.AreEqual<string>(literalTrue, getRepresentation corThm2)
                | "lem2" -> Assert.AreEqual<string>(literalTrue, getRepresentation lem2)
                | "corLem2" -> Assert.AreEqual<string>(literalTrue, getRepresentation corLem2)
                | "prp2" -> Assert.AreEqual<string>(literalTrue, getRepresentation prp2)
                | "corPrp2" -> Assert.AreEqual<string>(literalTrue, getRepresentation corPrp2)
                | "cor2" -> Assert.AreEqual<string>(literalTrue, getRepresentation cor2)
                | "corCor2" -> Assert.AreEqual<string>(literalTrue, getRepresentation corCor2)
                | "con1" -> Assert.AreEqual<string>(literalTrue, getRepresentation con1)
                | "corCon1" -> Assert.AreEqual<string>(literalTrue, getRepresentation corCon1)
                | "axi1" -> Assert.AreEqual<string>(literalTrue, getRepresentation axi1)
                | "corAxi1"  -> Assert.AreEqual<string>(literalTrue, getRepresentation corAxi1) 
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
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>(literalUndef, getRepresentation r)
            | "theory" -> Assert.AreEqual<string>(literalUndef, getRepresentation theory)
            | "block" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation block)
            | "t1" -> Assert.AreEqual<string>(literalTrue, getRepresentation t1)
            | "t2" -> Assert.AreEqual<string>(literalTrue, getRepresentation t2)
            | "t3" -> Assert.AreEqual<string>("dec obj", getRepresentation t3)
            | "t4" -> Assert.AreEqual<string>("dec obj", getRepresentation t4)
            | "t5" -> Assert.AreEqual<string>(literalInd, getRepresentation t5)
            | "t6" -> Assert.AreEqual<string>(literalInd, getRepresentation t6)
            | "t7" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation t7)
            | "t8" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation t8)
            | "t9" -> Assert.AreEqual<string>(literalTpl, getRepresentation t9)
            | "t10" -> Assert.AreEqual<string>(literalTpl, getRepresentation t10)
            | "t11" -> Assert.AreEqual<string>("", getRepresentation t11)
            | "t12" -> Assert.AreEqual<string>("", getRepresentation t12)
            | "t13" -> Assert.AreEqual<string>(literalFunc, getRepresentation t13)
            | "t14" -> Assert.AreEqual<string>(literalFunc, getRepresentation t14)
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
            | "r" -> Assert.AreEqual<string>(literalUndef, getRepresentation r)
            | "theory" -> Assert.AreEqual<string>(literalUndef, getRepresentation theory)
            | "block" -> Assert.AreEqual<string>(literalTrue, getRepresentation block); 
            | "x" -> Assert.AreEqual<string>("dec pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", getRepresentation x)
            | "y" -> Assert.AreEqual<string>("dec pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", getRepresentation y)
            | "s" -> Assert.AreEqual<string>("dec Set", getRepresentation s)
            | "xu" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation xu)
            | "xv" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation xv)
            | "xw" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation xw)
            | "yu" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation yu)
            | "yv" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation yv)
            | "yw" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation yw)
            | "xua" -> Assert.AreEqual<string>("dec obj", getRepresentation xua)
            | "xub" -> Assert.AreEqual<string>("dec obj", getRepresentation xub)
            | "xuc" -> Assert.AreEqual<string>("dec obj", getRepresentation xuc)
            | "xva" -> Assert.AreEqual<string>("dec obj", getRepresentation xva)
            | "xvb" -> Assert.AreEqual<string>("dec obj", getRepresentation xvb)
            | "xvc" -> Assert.AreEqual<string>("dec obj", getRepresentation xvc)
            | "xwa" -> Assert.AreEqual<string>("dec obj", getRepresentation xwa)
            | "xwb" -> Assert.AreEqual<string>("dec obj", getRepresentation xwb)
            | "xwc" -> Assert.AreEqual<string>("dec obj", getRepresentation xwc)
            | "yua" -> Assert.AreEqual<string>("dec obj", getRepresentation yua)
            | "yub" -> Assert.AreEqual<string>("dec obj", getRepresentation yub)
            | "yuc" -> Assert.AreEqual<string>("dec obj", getRepresentation yuc)
            | "yva" -> Assert.AreEqual<string>("dec obj", getRepresentation yva)
            | "yvb" -> Assert.AreEqual<string>("dec obj", getRepresentation yvb)
            | "yvc" -> Assert.AreEqual<string>("dec obj", getRepresentation yvc)
            | "ywa" -> Assert.AreEqual<string>("dec obj", getRepresentation ywa)
            | "ywb" -> Assert.AreEqual<string>("dec obj", getRepresentation ywb)
            | "ywc" -> Assert.AreEqual<string>("dec obj", getRepresentation ywc)
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
            | "r" -> Assert.AreEqual<string>(literalUndef, getRepresentation r)
            | "theory" -> Assert.AreEqual<string>(literalUndef, getRepresentation theory)
            | "block" -> Assert.AreEqual<string>(literalTrue, getRepresentation block); 
            | "x" -> Assert.AreEqual<string>("dec +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)[]", getRepresentation x)
            | "y" -> Assert.AreEqual<string>("dec +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)[]", getRepresentation y)
            | "xu" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation xu)
            | "xv" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation xv)
            | "xw" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation xw)
            | "yu" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation yu)
            | "yv" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation yv)
            | "yw" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation yw)
            | "xua" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xua)
            | "xub" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xub)
            | "xuc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xuc)
            | "xva" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xva)
            | "xvb" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xvb)
            | "xvc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xvc)
            | "xwa" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xwa)
            | "xwb" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xwb)
            | "xwc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xwc)
            | "yua" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yua)
            | "yub" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yub)
            | "yuc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yuc)
            | "yva" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yva)
            | "yvb" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yvb)
            | "yvc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yvc)
            | "ywa" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation ywa)
            | "ywb" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation ywb)
            | "ywc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation ywc)
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
            | "r" -> Assert.AreEqual<string>(literalUndef, getRepresentation r)
            | "theory" -> Assert.AreEqual<string>(literalUndef, getRepresentation theory)
            | "block" -> Assert.AreEqual<string>(literalTrue, getRepresentation block); 
            | "x" -> Assert.AreEqual<string>("dec pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", getRepresentation x)
            | "y" -> Assert.AreEqual<string>("dec pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", getRepresentation y)
            | "xu" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation xu)
            | "xv" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation xv)
            | "xw" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation xw)
            | "yu" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation yu)
            | "yv" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation yv)
            | "yw" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", getRepresentation yw)
            | "xua" -> Assert.AreEqual<string>("dec obj", getRepresentation xua)
            | "xub" -> Assert.AreEqual<string>("dec obj", getRepresentation xub)
            | "xuc" -> Assert.AreEqual<string>("dec obj", getRepresentation xuc)
            | "xva" -> Assert.AreEqual<string>("dec obj", getRepresentation xva)
            | "xvb" -> Assert.AreEqual<string>("dec obj", getRepresentation xvb)
            | "xvc" -> Assert.AreEqual<string>("dec obj", getRepresentation xvc)
            | "xwa" -> Assert.AreEqual<string>("dec obj", getRepresentation xwa)
            | "xwb" -> Assert.AreEqual<string>("dec obj", getRepresentation xwb)
            | "xwc" -> Assert.AreEqual<string>("dec obj", getRepresentation xwc)
            | "yua" -> Assert.AreEqual<string>("dec obj", getRepresentation yua)
            | "yub" -> Assert.AreEqual<string>("dec obj", getRepresentation yub)
            | "yuc" -> Assert.AreEqual<string>("dec obj", getRepresentation yuc)
            | "yva" -> Assert.AreEqual<string>("dec obj", getRepresentation yva)
            | "yvb" -> Assert.AreEqual<string>("dec obj", getRepresentation yvb)
            | "yvc" -> Assert.AreEqual<string>("dec obj", getRepresentation yvc)
            | "ywa" -> Assert.AreEqual<string>("dec obj", getRepresentation ywa)
            | "ywb" -> Assert.AreEqual<string>("dec obj", getRepresentation ywb)
            | "ywc" -> Assert.AreEqual<string>("dec obj", getRepresentation ywc)
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
            | "r" -> Assert.AreEqual<string>(literalUndef, getRepresentation r)
            | "theory" -> Assert.AreEqual<string>(literalUndef, getRepresentation theory)
            | "block" -> Assert.AreEqual<string>(literalTrue, getRepresentation block); 
            | "x" -> Assert.AreEqual<string>("dec +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)[]", getRepresentation x)
            | "y" -> Assert.AreEqual<string>("dec +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)[]", getRepresentation y)
            | "xu" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation xu)
            | "xv" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation xv)
            | "xw" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation xw)
            | "yu" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation yu)
            | "yv" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation yv)
            | "yw" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", getRepresentation yw)
            | "xua" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xua)
            | "xub" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xub)
            | "xuc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xuc)
            | "xva" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xva)
            | "xvb" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xvb)
            | "xvc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xvc)
            | "xwa" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xwa)
            | "xwb" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xwb)
            | "xwc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation xwc)
            | "yua" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yua)
            | "yub" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yub)
            | "yuc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yuc)
            | "yva" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yva)
            | "yvb" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yvb)
            | "yvc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation yvc)
            | "ywa" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation ywa)
            | "ywb" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation ywb)
            | "ywc" -> Assert.AreEqual<string>("dec *obj[]", getRepresentation ywc)
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
    [<DataRow("base12", literalParent)>]
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
    member this.TestExpression(var, varVal) =
        ad.Clear()
        let fplCode = sprintf "def pred T1() { %s };" varVal
        let filename = "TestExpression.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]

            let pr1 = theory.Scope["T1()"] 
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>(literalTrue, getRepresentation pr1)
            | "base2" -> Assert.AreEqual<string>(literalFalse, getRepresentation base1)
            | "base3" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base4" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base5" -> Assert.AreEqual<string>("undef(undef)", getRepresentation base1)
            | "base6" -> Assert.AreEqual<string>("$1", getRepresentation base1)
            | "base7" -> Assert.AreEqual<string>("undetermined(undef(undef))", getRepresentation base1)
            | "base8" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base9" -> Assert.AreEqual<string>("undetermined(undef)", getRepresentation base1)
            | "base10" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base11" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base12" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base13" -> Assert.AreEqual<string>("1", getRepresentation base1)
            | "base11a" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base12a" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base10b" -> Assert.AreEqual<string>("undef(undef)", getRepresentation base1)
            | "base11b" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base12b" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base13b" -> Assert.AreEqual<string>("1(undef)", getRepresentation base1)
            | "base10c" -> Assert.AreEqual<string>("undef(undef, undef)", getRepresentation base1)
            | "base11c" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base12c" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base13c" -> Assert.AreEqual<string>("1(undef, undef)", getRepresentation base1)
            | "base10d" -> Assert.AreEqual<string>("undef[undef, undef]", getRepresentation base1)
            | "base11d" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base12d" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base13d" -> Assert.AreEqual<string>("1[undef]", getRepresentation base1)
            | "base10e" -> Assert.AreEqual<string>("undef(undef, undef).undef[undef, undef]", getRepresentation base1)
            | "base11e" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base12e" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base13e" -> Assert.AreEqual<string>("1(undef, undef).undef[undef, undef]", getRepresentation base1)
            | "base10f" -> Assert.AreEqual<string>("undef[undef, undef].undef", getRepresentation base1)
            | "base11f" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base12f" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base13f" -> Assert.AreEqual<string>("1[undef].undef(undef, undef)", getRepresentation base1)
            | "base14" -> Assert.AreEqual<string>(literalUndef, getRepresentation base1)
            | "base15" -> Assert.AreEqual<string>("-(undef)", getRepresentation base1)
            | "base15a" -> Assert.AreEqual<string>("'(undef)", getRepresentation base1)
            | "base15b" -> Assert.AreEqual<string>("'(-(undef))", getRepresentation base1)
            | "base16" -> Assert.AreEqual<string>("-(*(=(+(undef, undef), 2), undef))", getRepresentation base1)
            | "base17" -> Assert.AreEqual<string>("'(*(=(+(undef, undef('(undef))), 2), undef))", getRepresentation base1)
            | "base18" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base19" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base20" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base21" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base21a" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base21b" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base22" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base23" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base24" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base25" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base26" -> Assert.AreEqual<string>(literalFalse, getRepresentation base1)
            | "base27" -> Assert.AreEqual<string>("undef(undef)", getRepresentation base1)
            | "base28" -> Assert.AreEqual<string>("undef(undef, undef, undef, undef)", getRepresentation base1)
            | "base29" -> Assert.AreEqual<string>("undef(undef, undef, undef)", getRepresentation base1)
            | "base30" -> Assert.AreEqual<string>("undef(undef(undef))", getRepresentation base1)
            | "base31" -> Assert.AreEqual<string>("undef(undef(undef), undef(undef, undef, undef))", getRepresentation base1)
            | "base32" -> Assert.AreEqual<string>("undef(true, undef, false)", getRepresentation base1)
            | "base33" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base34" -> Assert.AreEqual<string>(literalFalse, getRepresentation base1)
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
            let base1 = ctor.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("bas", getRepresentation base1)
            | "base2" -> Assert.AreEqual<string>("bas", getRepresentation base1)
            | "base3" -> Assert.AreEqual<string>("bas", getRepresentation base1)
            | "base4" -> Assert.AreEqual<string>("bas", getRepresentation base1)
            | "base5" -> Assert.AreEqual<string>("bas", getRepresentation base1)
            | "base6" -> Assert.AreEqual<string>("bas", getRepresentation base1)
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
            let base1 = pr1.ArgList[0]

            match var with
            | "base1" -> Assert.AreEqual<string>("undef(undef, undef)", getRepresentation base1)
            | "base2" -> Assert.AreEqual<string>("undef(undef, undef, undef, undef)", getRepresentation base1)
            | "base3" -> Assert.AreEqual<string>("undef(undef, undef, undef)", getRepresentation base1)
            | "base4" -> Assert.AreEqual<string>("undef(undef(undef))", getRepresentation base1)
            | "base5" -> Assert.AreEqual<string>("undef(undef)", getRepresentation base1)
            | "base6" -> Assert.AreEqual<string>("undef(undef(undef), undef(undef, undef, undef))", getRepresentation base1)
            | "base7" -> Assert.AreEqual<string>("undef(true, undef, false)", getRepresentation base1)
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
                if varVal.Contains literalCl then 
                    theory.Scope["T1"]
                elif varVal.Contains literalFunc then 
                    theory.Scope["T1() -> obj"]
                else 
                    theory.Scope["T1()"]

            match var with
            | "base1" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base2" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base3" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base4" -> Assert.AreEqual<string>(literalUndetermined, getRepresentation base1)
            | "base5" -> Assert.AreEqual<string>("class T1", getRepresentation base1)
            | "base5a" -> Assert.AreEqual<string>("class T1", getRepresentation base1)
            | "base6" -> Assert.AreEqual<string>("dec obj", getRepresentation base1)
            | "base7" -> Assert.AreEqual<string>("dec obj", getRepresentation base1)
            | "base8" -> Assert.AreEqual<string>("dec obj", getRepresentation base1)
            | "base9" -> Assert.AreEqual<string>("dec obj", getRepresentation base1)
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
            | "varIndUnset" -> Assert.AreEqual<string>("dec ind", getRepresentation varObj)
            | "varIndSet0" -> Assert.AreEqual<string>("$0", getRepresentation varObj)
            | "varIndSet1" -> Assert.AreEqual<string>("$1", getRepresentation varObj)
            | "varIndSet42" -> Assert.AreEqual<string>("$42", getRepresentation varObj)
            | "varIndSet100" -> Assert.AreEqual<string>("$100", getRepresentation varObj)
            | "varFuncUnset" -> Assert.AreEqual<string>("dec func", getRepresentation varObj)
            | "varPredUnset" -> Assert.AreEqual<string>("dec pred", getRepresentation varObj)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("o", """def pred T() {dec ~o:obj; true};""", "dec obj")>]    // without anything
    [<DataRow("a", """def cl A: obj {intr} def pred T() {dec ~a:A; true};""", "dec A")>]    // without constructor, without inheritance, without instantiation
    [<DataRow("aI1", """def cl A: obj {intr} def pred T() {dec ~aI1:A aI1:=A; true};""", "intr A:intr obj")>]  // without constructor, without inheritance, with instantiation (without ())
    [<DataRow("aI2", """def cl A: obj {intr} def pred T() {dec ~aI2:A aI2:=A(); true};""", "intr A:intr obj")>]  // without constructor, without inheritance, with instantiation (with ()) -> should also trigger another error
    [<DataRow("b", """def cl A: obj {intr} def cl B: A {intr} def pred T() {dec ~b:B; true};""", "dec class B")>]    // without constructor, with inheritance, without instantiation
    [<DataRow("bI1", """def cl A: obj {intr} def cl B: A {intr} def pred T() {dec ~bI1:B bI1:=B; true};""", "intr B:intr A:intr obj")>]  // without constructor, with inheritance, with instantiation (without ())
    [<DataRow("bI2", """def cl A: obj {intr} def cl B: A {intr} def pred T() {dec ~bI2:B bI2:=B(); true};""", "intr B:intr A:intr obj")>]  // without constructor, with inheritance, with instantiation (with ()) -> should also trigger another error
    [<DataRow("c", """def cl A: obj {intr} def cl B: A {intr} def cl C: obj {ctor C() {dec base.obj (); self }} def pred T() {dec ~c:C; true};""", "dec C")>]    // with constructor, without inheritance, without instantiation
    [<DataRow("cI1", """def cl A: obj {intr} def cl B: A {intr} def cl C: obj {ctor C() {dec base.obj (); self }} def pred T() {dec ~cI1:C cI1:=C; true};""", "C:intr obj")>]  // with constructor, without inheritance, with instantiation (without ()) -> should also trigger another error
    [<DataRow("cI2", """def cl A: obj {intr} def cl B: A {intr} def cl C: obj {ctor C() {dec base.obj (); self }} def pred T() {dec ~cI2:C cI2:=C(); true};""", "C:obj")>]  // with constructor, without inheritance, with instantiation (with ())
    [<DataRow("d", """def cl A: obj {intr} def cl B: A {intr} def cl D: B {ctor D() {dec base.B(); self }} def pred T() {dec ~d:D; true};""", "dec D")>]    // with constructor, with inheritance, without instantiation
    [<DataRow("dI1", """def cl A: obj {intr} def cl B: A {intr} def cl D: B {ctor D() {dec base.B(); self }} def pred T() {dec ~dI1:D dI1:=D; true};""", "D:intr B:intr A:intr obj")>]  // with constructor, with inheritance, with instantiation (without ())
    [<DataRow("dI2", """def cl A: obj {intr} def cl B: A {intr} def cl D: B {ctor D() {dec base.B(); self }} def pred T() {dec ~dI2:D dI2:=D(); true};""", "D:intr B:intr A")>]  
    [<TestMethod>]
    member this.TestVariableRepresentationObjects(var, fplCode:string, expected:string) =
        ad.Clear()
        let filename = "TestVariableRepresentationObjects"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope["T()"]
            let varObj = base1.Scope[var]
            Assert.AreEqual<string>(expected, getRepresentation varObj)

        | None -> 
            Assert.IsTrue(false)
