namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplPrimitives
open ErrDiagnostics
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type TestFplValueScopeFplRepresentation() =

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
        let res = CommonFplValueTestCases.ScopeBlocks("FplRepresentation") 
        match res with
        | Some (r:FplRoot,theory:FplValue,inf1:FplValue,inf2:FplValue,axi1:FplValue,axi2:FplValue,pst1:FplValue,pst2:FplValue,thm1:FplValue,thm2:FplValue,pro1:FplValue,pro2:FplValue,lem1:FplValue,lem2:FplValue,cor1:FplValue,cor2:FplValue,con1:FplValue,con2:FplValue,cla1:FplValue,cla2:FplValue,pre1:FplValue,pre2:FplValue,fun1:FplValue,fun2:FplValue,fun3:FplValue,fun4:FplValue,fun5:FplValue,fun6:FplValue,fun7:FplValue,fun8:FplValue,fun9:FplValue,prf1:FplValue,prf2:FplValue,loc1:FplValue,loc2:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>(literalUndef, r.Represent())
            | PrimTheoryL -> Assert.AreEqual<string>(literalUndef, theory.Represent())
            | "inf1" -> Assert.AreEqual<string>(literalUndetermined, inf1.Represent())
            | "inf2" -> Assert.AreEqual<string>(literalUndetermined, inf2.Represent())
            | "axi1" -> Assert.AreEqual<string>(literalTrue, axi1.Represent())
            | "axi2" -> Assert.AreEqual<string>(literalTrue, axi2.Represent())
            | "pst1" -> Assert.AreEqual<string>(literalTrue, pst1.Represent())
            | "pst2" -> Assert.AreEqual<string>(literalTrue, pst2.Represent())
            | "thm1" -> Assert.AreEqual<string>(literalTrue, thm1.Represent())
            | "thm2" -> Assert.AreEqual<string>(literalTrue, thm2.Represent())
            | "pro1" -> Assert.AreEqual<string>(literalTrue, pro1.Represent())
            | "pro2" -> Assert.AreEqual<string>(literalTrue, pro2.Represent())
            | "lem1" -> Assert.AreEqual<string>(literalTrue, lem1.Represent())
            | "lem2" -> Assert.AreEqual<string>(literalTrue, lem2.Represent())
            | "cor1" -> Assert.AreEqual<string>(literalTrue, cor1.Represent())
            | "cor2" -> Assert.AreEqual<string>(literalTrue, cor2.Represent())
            | "con1" -> Assert.AreEqual<string>(literalTrue, con1.Represent())
            | "con2" -> Assert.AreEqual<string>(literalTrue, con2.Represent())
            | "cla1" -> Assert.AreEqual<string>("dec cl SomeClass1", cla1.Represent())
            | "cla2" -> Assert.AreEqual<string>("dec cl SomeClass2", cla2.Represent())
            | "pre1" -> Assert.AreEqual<string>(literalTrue, pre1.Represent())
            | "pre2" -> Assert.AreEqual<string>(literalTrue, pre2.Represent())
            | "fun1" -> Assert.AreEqual<string>("dec obj", fun1.Represent())
            | "fun2" -> Assert.AreEqual<string>("dec obj", fun2.Represent())
            | "fun3" -> Assert.AreEqual<string>("dec obj", fun3.Represent())
            | "fun4" -> Assert.AreEqual<string>("dec obj(pred)", fun4.Represent())
            | "fun5" -> Assert.AreEqual<string>("dec SomeClass1", fun5.Represent())
            | "fun6" -> Assert.AreEqual<string>("dec SomeClass1", fun6.Represent())
            | "fun7" -> Assert.AreEqual<string>("dec SomeClass1", fun7.Represent())
            | "fun8" -> Assert.AreEqual<string>("dec ind", fun8.Represent())
            | "fun9" -> Assert.AreEqual<string>("dec ind", fun9.Represent())
            | "prf1" -> Assert.AreEqual<string>(literalUndetermined, prf1.Represent())
            | "prf2" -> Assert.AreEqual<string>(literalUndetermined, prf2.Represent())
            | "loc1" -> Assert.AreEqual<string>("not(x)", loc1.Represent())
            | "loc2" -> Assert.AreEqual<string>("Equal(x, y)", loc2.Represent())
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
        let res = CommonFplValueTestCases.ScopeConstructors("FplRepresentation") 
        match res with
        | Some (r,theory,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>(literalUndef, r.Represent())
            | PrimTheoryL -> Assert.AreEqual<string>(literalUndef, theory.Represent())
            | "block" -> Assert.AreEqual<string>("dec cl TestId", block.Represent())
            | "t1" -> Assert.AreEqual<string>("TestId()", t1.Represent())
            | "t2" -> Assert.AreEqual<string>("TestId(obj)", t2.Represent())
            | "t3" -> Assert.AreEqual<string>("TestId(pred)", t3.Represent())
            | "t4" -> Assert.AreEqual<string>("TestId(ind)", t4.Represent())
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
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries("FplRepresentation") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.AreEqual<string>(literalUndef, r.Represent())
                | PrimTheoryL -> Assert.AreEqual<string>(literalUndef, theory.Represent())
                | "thm1" -> Assert.AreEqual<string>(literalTrue, thm1.Represent())
                | "proofThm1" -> Assert.AreEqual<string>(literalTrue, proofThm1.Represent())
                | "lem1" -> Assert.AreEqual<string>(literalTrue, lem1.Represent())
                | "proofLem1" -> Assert.AreEqual<string>(literalTrue, proofLem1.Represent())
                | "prp1" -> Assert.AreEqual<string>(literalTrue, prp1.Represent())
                | "proofPrp1" -> Assert.AreEqual<string>(literalTrue, proofPrp1.Represent())
                | "cor1" -> Assert.AreEqual<string>(literalTrue, cor1.Represent())
                | "proofCor1" -> Assert.AreEqual<string>(literalTrue, proofCor1.Represent())
                | "thm2" -> Assert.AreEqual<string>(literalTrue, thm2.Represent())
                | "corThm2" -> Assert.AreEqual<string>(literalTrue, corThm2.Represent())
                | "lem2" -> Assert.AreEqual<string>(literalTrue, lem2.Represent())
                | "corLem2" -> Assert.AreEqual<string>(literalTrue, corLem2.Represent())
                | "prp2" -> Assert.AreEqual<string>(literalTrue, prp2.Represent())
                | "corPrp2" -> Assert.AreEqual<string>(literalTrue, corPrp2.Represent())
                | "cor2" -> Assert.AreEqual<string>(literalTrue, cor2.Represent())
                | "corCor2" -> Assert.AreEqual<string>(literalTrue, corCor2.Represent())
                | "con1" -> Assert.AreEqual<string>(literalTrue, con1.Represent())
                | "corCon1" -> Assert.AreEqual<string>(literalTrue, corCon1.Represent())
                | "axi1" -> Assert.AreEqual<string>(literalTrue, axi1.Represent())
                | "corAxi1"  -> Assert.AreEqual<string>(literalTrue, corAxi1.Represent()) 
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
        let res = CommonFplValueTestCases.ScopeProperties("FplRepresentation") 
        match res with
        | Some (r:FplRoot,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue,t5:FplValue,t6:FplValue,t7:FplValue,t8:FplValue,t9:FplValue,t10:FplValue,t11:FplValue,t12:FplValue,
            t13:FplValue,t14:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual<string>(literalUndef, r.Represent())
            | PrimTheoryL -> Assert.AreEqual<string>(literalUndef, theory.Represent())
            | "block" -> Assert.AreEqual<string>(literalUndetermined, block.Represent())
            | "t1" -> Assert.AreEqual<string>(literalTrue, t1.Represent())
            | "t2" -> Assert.AreEqual<string>(literalTrue, t2.Represent())
            | "t3" -> Assert.AreEqual<string>("dec obj", t3.Represent())
            | "t4" -> Assert.AreEqual<string>("dec obj", t4.Represent())
            | "t5" -> Assert.AreEqual<string>($"dec {literalInd}", t5.Represent())
            | "t6" -> Assert.AreEqual<string>($"dec {literalInd}", t6.Represent())
            | "t7" -> Assert.AreEqual<string>($"dec {literalPred}", t7.Represent())
            | "t8" -> Assert.AreEqual<string>($"dec {literalPred}", t8.Represent())
            | "t9" -> Assert.AreEqual<string>($"dec {literalTpl}", t9.Represent())
            | "t10" -> Assert.AreEqual<string>($"dec {literalTpl}", t10.Represent())
            | "t11" -> Assert.AreEqual<string>($"dec Nat", t11.Represent())
            | "t12" -> Assert.AreEqual<string>($"dec Nat", t12.Represent())
            | "t13" -> Assert.AreEqual<string>($"dec {literalFunc}", t13.Represent())
            | "t14" -> Assert.AreEqual<string>($"dec {literalFunc}", t14.Represent())
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlock("FplRepresentation")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>(literalUndef, r.Represent())
            | PrimTheoryL -> Assert.AreEqual<string>(literalUndef, theory.Represent())
            | "block" -> Assert.AreEqual<string>(literalTrue, block.Represent()); 
            | "x" -> Assert.AreEqual<string>("dec pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", x.Represent())
            | "y" -> Assert.AreEqual<string>("dec pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", y.Represent())
            | "s" -> Assert.AreEqual<string>("dec Set", s.Represent())
            | "xu" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", xu.Represent())
            | "xv" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", xv.Represent())
            | "xw" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", xw.Represent())
            | "yu" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", yu.Represent())
            | "yv" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", yv.Represent())
            | "yw" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", yw.Represent())
            | "xua" -> Assert.AreEqual<string>("dec obj", xua.Represent())
            | "xub" -> Assert.AreEqual<string>("dec obj", xub.Represent())
            | "xuc" -> Assert.AreEqual<string>("dec obj", xuc.Represent())
            | "xva" -> Assert.AreEqual<string>("dec obj", xva.Represent())
            | "xvb" -> Assert.AreEqual<string>("dec obj", xvb.Represent())
            | "xvc" -> Assert.AreEqual<string>("dec obj", xvc.Represent())
            | "xwa" -> Assert.AreEqual<string>("dec obj", xwa.Represent())
            | "xwb" -> Assert.AreEqual<string>("dec obj", xwb.Represent())
            | "xwc" -> Assert.AreEqual<string>("dec obj", xwc.Represent())
            | "yua" -> Assert.AreEqual<string>("dec obj", yua.Represent())
            | "yub" -> Assert.AreEqual<string>("dec obj", yub.Represent())
            | "yuc" -> Assert.AreEqual<string>("dec obj", yuc.Represent())
            | "yva" -> Assert.AreEqual<string>("dec obj", yva.Represent())
            | "yvb" -> Assert.AreEqual<string>("dec obj", yvb.Represent())
            | "yvc" -> Assert.AreEqual<string>("dec obj", yvc.Represent())
            | "ywa" -> Assert.AreEqual<string>("dec obj", ywa.Represent())
            | "ywb" -> Assert.AreEqual<string>("dec obj", ywb.Represent())
            | "ywc" -> Assert.AreEqual<string>("dec obj", ywc.Represent())
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
        let result = CommonFplValueTestCases.ScopeVariablesInBlockVariadic("FplRepresentation")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>(literalUndef, r.Represent())
            | PrimTheoryL -> Assert.AreEqual<string>(literalUndef, theory.Represent())
            | "block" -> Assert.AreEqual<string>(literalTrue, block.Represent()); 
            | "x" -> Assert.AreEqual<string>("dec +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)[]", x.Represent())
            | "y" -> Assert.AreEqual<string>("dec +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)[]", y.Represent())
            | "xu" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", xu.Represent())
            | "xv" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", xv.Represent())
            | "xw" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", xw.Represent())
            | "yu" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", yu.Represent())
            | "yv" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", yv.Represent())
            | "yw" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", yw.Represent())
            | "xua" -> Assert.AreEqual<string>("dec *obj[]", xua.Represent())
            | "xub" -> Assert.AreEqual<string>("dec *obj[]", xub.Represent())
            | "xuc" -> Assert.AreEqual<string>("dec *obj[]", xuc.Represent())
            | "xva" -> Assert.AreEqual<string>("dec *obj[]", xva.Represent())
            | "xvb" -> Assert.AreEqual<string>("dec *obj[]", xvb.Represent())
            | "xvc" -> Assert.AreEqual<string>("dec *obj[]", xvc.Represent())
            | "xwa" -> Assert.AreEqual<string>("dec *obj[]", xwa.Represent())
            | "xwb" -> Assert.AreEqual<string>("dec *obj[]", xwb.Represent())
            | "xwc" -> Assert.AreEqual<string>("dec *obj[]", xwc.Represent())
            | "yua" -> Assert.AreEqual<string>("dec *obj[]", yua.Represent())
            | "yub" -> Assert.AreEqual<string>("dec *obj[]", yub.Represent())
            | "yuc" -> Assert.AreEqual<string>("dec *obj[]", yuc.Represent())
            | "yva" -> Assert.AreEqual<string>("dec *obj[]", yva.Represent())
            | "yvb" -> Assert.AreEqual<string>("dec *obj[]", yvb.Represent())
            | "yvc" -> Assert.AreEqual<string>("dec *obj[]", yvc.Represent())
            | "ywa" -> Assert.AreEqual<string>("dec *obj[]", ywa.Represent())
            | "ywb" -> Assert.AreEqual<string>("dec *obj[]", ywb.Represent())
            | "ywc" -> Assert.AreEqual<string>("dec *obj[]", ywc.Represent())
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignature("FplRepresentation")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>(literalUndef, r.Represent())
            | PrimTheoryL -> Assert.AreEqual<string>(literalUndef, theory.Represent())
            | "block" -> Assert.AreEqual<string>(literalTrue, block.Represent()); 
            | "x" -> Assert.AreEqual<string>("dec pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", x.Represent())
            | "y" -> Assert.AreEqual<string>("dec pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj)", y.Represent())
            | "xu" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", xu.Represent())
            | "xv" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", xv.Represent())
            | "xw" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", xw.Represent())
            | "yu" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", yu.Represent())
            | "yv" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", yv.Represent())
            | "yw" -> Assert.AreEqual<string>("dec func(obj, obj, obj) -> obj", yw.Represent())
            | "xua" -> Assert.AreEqual<string>("dec obj", xua.Represent())
            | "xub" -> Assert.AreEqual<string>("dec obj", xub.Represent())
            | "xuc" -> Assert.AreEqual<string>("dec obj", xuc.Represent())
            | "xva" -> Assert.AreEqual<string>("dec obj", xva.Represent())
            | "xvb" -> Assert.AreEqual<string>("dec obj", xvb.Represent())
            | "xvc" -> Assert.AreEqual<string>("dec obj", xvc.Represent())
            | "xwa" -> Assert.AreEqual<string>("dec obj", xwa.Represent())
            | "xwb" -> Assert.AreEqual<string>("dec obj", xwb.Represent())
            | "xwc" -> Assert.AreEqual<string>("dec obj", xwc.Represent())
            | "yua" -> Assert.AreEqual<string>("dec obj", yua.Represent())
            | "yub" -> Assert.AreEqual<string>("dec obj", yub.Represent())
            | "yuc" -> Assert.AreEqual<string>("dec obj", yuc.Represent())
            | "yva" -> Assert.AreEqual<string>("dec obj", yva.Represent())
            | "yvb" -> Assert.AreEqual<string>("dec obj", yvb.Represent())
            | "yvc" -> Assert.AreEqual<string>("dec obj", yvc.Represent())
            | "ywa" -> Assert.AreEqual<string>("dec obj", ywa.Represent())
            | "ywb" -> Assert.AreEqual<string>("dec obj", ywb.Represent())
            | "ywc" -> Assert.AreEqual<string>("dec obj", ywc.Represent())
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
        let result = CommonFplValueTestCases.ScopeVariablesInSignatureVariadic("FplRepresentation")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.AreEqual<string>(literalUndef, r.Represent())
            | PrimTheoryL -> Assert.AreEqual<string>(literalUndef, theory.Represent())
            | "block" -> Assert.AreEqual<string>(literalTrue, block.Represent()); 
            | "x" -> Assert.AreEqual<string>("dec +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)[]", x.Represent())
            | "y" -> Assert.AreEqual<string>("dec +pred(func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj, func(*obj, *obj, *obj) -> obj)[]", y.Represent())
            | "xu" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", xu.Represent())
            | "xv" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", xv.Represent())
            | "xw" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", xw.Represent())
            | "yu" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", yu.Represent())
            | "yv" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", yv.Represent())
            | "yw" -> Assert.AreEqual<string>("dec func(*obj, *obj, *obj) -> obj", yw.Represent())
            | "xua" -> Assert.AreEqual<string>("dec *obj[]", xua.Represent())
            | "xub" -> Assert.AreEqual<string>("dec *obj[]", xub.Represent())
            | "xuc" -> Assert.AreEqual<string>("dec *obj[]", xuc.Represent())
            | "xva" -> Assert.AreEqual<string>("dec *obj[]", xva.Represent())
            | "xvb" -> Assert.AreEqual<string>("dec *obj[]", xvb.Represent())
            | "xvc" -> Assert.AreEqual<string>("dec *obj[]", xvc.Represent())
            | "xwa" -> Assert.AreEqual<string>("dec *obj[]", xwa.Represent())
            | "xwb" -> Assert.AreEqual<string>("dec *obj[]", xwb.Represent())
            | "xwc" -> Assert.AreEqual<string>("dec *obj[]", xwc.Represent())
            | "yua" -> Assert.AreEqual<string>("dec *obj[]", yua.Represent())
            | "yub" -> Assert.AreEqual<string>("dec *obj[]", yub.Represent())
            | "yuc" -> Assert.AreEqual<string>("dec *obj[]", yuc.Represent())
            | "yva" -> Assert.AreEqual<string>("dec *obj[]", yva.Represent())
            | "yvb" -> Assert.AreEqual<string>("dec *obj[]", yvb.Represent())
            | "yvc" -> Assert.AreEqual<string>("dec *obj[]", yvc.Represent())
            | "ywa" -> Assert.AreEqual<string>("dec *obj[]", ywa.Represent())
            | "ywb" -> Assert.AreEqual<string>("dec *obj[]", ywb.Represent())
            | "ywc" -> Assert.AreEqual<string>("dec *obj[]", ywc.Represent())
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", literalTrue)>]
    [<DataRow("base2", literalFalse)>]
    [<DataRow("base3", literalUndef)>]
    [<DataRow("base4", "-1")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "$1")>]
    [<DataRow("base7", "Test$1(x)")>] 
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
            | "base1" -> Assert.AreEqual<string>(literalTrue, base1.Represent())
            | "base2" -> Assert.AreEqual<string>(literalFalse, base1.Represent())
            | "base3" -> Assert.AreEqual<string>(literalUndef, base1.Represent())
            | "base4" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef})", base1.Represent())
            | "base5" -> Assert.AreEqual<string>($"{literalUndef}()", base1.Represent())
            | "base6" -> Assert.AreEqual<string>($"$1", base1.Represent())
            | "base7" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef})", base1.Represent())
            | "base8" -> Assert.AreEqual<string>(literalUndef, base1.Represent())
            | "base9" -> Assert.AreEqual<string>($"{literalUndef}()", base1.Represent())
            | "base10" -> Assert.AreEqual<string>(literalUndef, base1.Represent())
            | "base11" -> Assert.AreEqual<string>(literalUndef, base1.Represent())
            | "base12" -> Assert.AreEqual<string>(literalUndef, base1.Represent())
            | "base13" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base11a" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base12a" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base10b" -> Assert.AreEqual<string>($"{literalUndef}()", base1.Represent())
            | "base11b" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base12b" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base13b" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base10c" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}, {literalUndef})", base1.Represent())
            | "base11c" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base12c" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base13c" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base10d" -> Assert.AreEqual<string>($"{literalUndef}[{literalUndef}, {literalUndef}]", base1.Represent())
            | "base11d" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base12d" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base13d" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base10e" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}, {literalUndef}).{literalUndef}[{literalUndef}, {literalUndef}]", base1.Represent())
            | "base11e" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base12e" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base13e" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base10f" -> Assert.AreEqual<string>($"{literalUndef}[{literalUndef}, {literalUndef}].{literalUndef}", base1.Represent())
            | "base11f" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base12f" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base13f" -> Assert.AreEqual<string>($"{literalUndef}", base1.Represent())
            | "base14" -> Assert.AreEqual<string>(literalUndef, base1.Represent())
            | "base15" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef})", base1.Represent())
            | "base15a" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef})", base1.Represent())
            | "base15b" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}({literalUndef}))", base1.Represent())
            | "base16" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}({literalUndef}({literalUndef}({literalUndef}, {literalUndef}), {literalUndef}), {literalUndef}))", base1.Represent())
            | "base17" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}({literalUndef}({literalUndef}({literalUndef}, {literalUndef}({literalUndef})), {literalUndef}), {literalUndef}))", base1.Represent())
            | "base18" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base19" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base20" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base21" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base21a" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base21b" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base22" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base23" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base24" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base25" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base26" -> Assert.AreEqual<string>(literalFalse, base1.Represent())
            | "base27" -> Assert.AreEqual<string>($"{literalUndef}()", base1.Represent())
            | "base28" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}, {literalUndef}, {literalUndef}, {literalUndef})", base1.Represent())
            | "base29" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}, {literalUndef}, {literalUndef})", base1.Represent())
            | "base30" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}({literalUndef}))", base1.Represent())
            | "base31" -> Assert.AreEqual<string>($"{literalUndef}({literalUndef}({literalUndef}), {literalUndef}({literalUndef}, {literalUndef}, {literalUndef}))", base1.Represent())
            | "base32" -> Assert.AreEqual<string>($"{literalUndef}({literalTrue}, {literalUndef}, {literalFalse})", base1.Represent())
            | "base33" -> Assert.AreEqual<string>("dec pred(obj)", base1.Represent())
            | "base34" -> Assert.AreEqual<string>(literalFalse, base1.Represent())
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
            | "base1" -> Assert.AreEqual<string>("bas", base1.Represent())
            | "base2" -> Assert.AreEqual<string>("bas", base1.Represent())
            | "base3" -> Assert.AreEqual<string>("bas", base1.Represent())
            | "base4" -> Assert.AreEqual<string>("bas", base1.Represent())
            | "base5" -> Assert.AreEqual<string>("bas", base1.Represent())
            | "base6" -> Assert.AreEqual<string>("bas", base1.Represent())
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
        let filename = "TestMappingRepresentation"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
            let mapping = base1.ArgList[0]
            match var with
            | "base1" -> Assert.AreEqual<string>($"dec {literalObj}", mapping.Represent())
            | "base2" -> Assert.AreEqual<string>($"dec {literalInd}", mapping.Represent())
            | "base3" -> Assert.AreEqual<string>($"dec {literalFunc}", mapping.Represent())
            | "base4" -> Assert.AreEqual<string>($"dec {literalPred}", mapping.Represent())
            | "base5" -> Assert.AreEqual<string>("dec A", mapping.Represent())
            | "base6" -> Assert.AreEqual<string>($"dec {literalObj}({literalInd})", mapping.Represent())
            | "base7" -> Assert.AreEqual<string>($"dec {literalPred}(*{literalObj})", mapping.Represent())
            | "base8" -> Assert.AreEqual<string>($"dec {literalFunc}(*{literalPred}({literalObj})) -> {literalPred}({literalInd})", mapping.Represent())
            | "base9" -> Assert.AreEqual<string>($"dec {literalPred}(+{literalFunc}(A) -> A)", mapping.Represent())
            | "base10" -> Assert.AreEqual<string>($"dec A({literalFunc}(A) -> A)", mapping.Represent())
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base0", "dec ~x,y:pred; del.Equal(x,y)")>]
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
            let variableStack = new FplVariableStack()
            pr1.Run variableStack
            let base1 = pr1.ArgList[0]

            match var with
            | "base0" -> Assert.AreEqual<string>("undetermined", base1.Represent())
            | "base1" -> Assert.AreEqual<string>("undef", base1.Represent())
            | "base2" -> Assert.AreEqual<string>("undef(undef, undef, undef, undef)", base1.Represent())
            | "base3" -> Assert.AreEqual<string>("undef(undef, undef, undef)", base1.Represent())
            | "base4" -> Assert.AreEqual<string>("undef(undef(undef))", base1.Represent())
            | "base5" -> Assert.AreEqual<string>("undef()", base1.Represent())
            | "base6" -> Assert.AreEqual<string>("undef(undef(undef), undef(undef, undef, undef))", base1.Represent())
            | "base7" -> Assert.AreEqual<string>("undef(true, undef, false)", base1.Represent())
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", """def pred T1() {intr};""")>]
    [<DataRow("base2", """def pred T1 infix ">" -1 () {intr};""")>]
    [<DataRow("base3", """def pred T1 postfix "'" () {intr};""")>]
    [<DataRow("base4", """def pred T1 prefix "-" () {intr};""")>]
    [<DataRow("base5", """def cl T1 symbol "∅" :obj {intr};""")>]
    [<DataRow("base5a", """def cl T1:obj {intr};""")>]
    [<DataRow("base6", """def func T1()->obj {intr};""")>]
    [<DataRow("base7", """def func T1 infix ">" -1 ()->obj {intr};""")>]
    [<DataRow("base8", """def func T1 postfix "'" ()->obj {intr};""")>]
    [<DataRow("base9", """def func T1 prefix "-" ()->obj {intr};""")>]
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
            | "base1" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base2" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base3" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base4" -> Assert.AreEqual<string>(literalUndetermined, base1.Represent())
            | "base5" -> Assert.AreEqual<string>("class T1", base1.Represent())
            | "base5a" -> Assert.AreEqual<string>("class T1", base1.Represent())
            | "base6" -> Assert.AreEqual<string>("dec obj", base1.Represent())
            | "base7" -> Assert.AreEqual<string>("dec obj", base1.Represent())
            | "base8" -> Assert.AreEqual<string>("dec obj", base1.Represent())
            | "base9" -> Assert.AreEqual<string>("dec obj", base1.Represent())
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
            | "varIndUnset" -> Assert.AreEqual<string>("dec ind", varObj.Represent())
            | "varIndSet0" -> Assert.AreEqual<string>("$0", varObj.Represent())
            | "varIndSet1" -> Assert.AreEqual<string>("$1", varObj.Represent())
            | "varIndSet42" -> Assert.AreEqual<string>("$42", varObj.Represent())
            | "varIndSet100" -> Assert.AreEqual<string>("$100", varObj.Represent())
            | "varFuncUnset" -> Assert.AreEqual<string>("dec func", varObj.Represent())
            | "varPredUnset" -> Assert.AreEqual<string>("dec pred", varObj.Represent())
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("o", """def pred T() {dec ~o:obj; true};""", "dec obj")>]    // without anything
    [<DataRow("a", """def cl A: obj {intr} def pred T() {dec ~a:A; true};""", "dec A")>]    // without constructor, without inheritance, without instantiation
    [<DataRow("aI1", """def cl A: obj {intr} def pred T() {dec ~aI1:A aI1:=A; true};""", "intr A:intr obj")>]  // without constructor, without inheritance, with instantiation (without ())
    [<DataRow("aI2", """def cl A: obj {intr} def pred T() {dec ~aI2:A aI2:=A(); true};""", "intr A:intr obj")>]  // without constructor, without inheritance, with instantiation (with ()) -> should also trigger another error
    [<DataRow("b", """def cl A: obj {intr} def cl B: A {intr} def pred T() {dec ~b:B; true};""", "dec B")>]    // without constructor, with inheritance, without instantiation
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
            Assert.AreEqual<string>(expected, varObj.Represent())

        | None -> 
            Assert.IsTrue(false)


    [<DataRow("base1", "$1",  literalFalse)>]
    [<DataRow("base2", "$2",  literalTrue)>]
    [<DataRow("base3", "$3",  literalFalse)>]
    [<DataRow("base4", "$0",  literalUndef)>]
    [<DataRow("base5", "$4", literalUndef)>]
    [<TestMethod>]
    member this.TestMCaseStatement(var, input, (output:string)) =
        ad.Clear()
        let fplCode = sprintf """
                def pred Equal infix "=" 50 (x,y: tpl)
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
        let filename = "TestMCaseStatementFplRepresentation"
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
            | "base1" -> Assert.AreEqual<string>(output, res.Represent())
            | "base2" -> Assert.AreEqual<string>(output, res.Represent())
            | "base3" -> Assert.AreEqual<string>(output, res.Represent())
            | "base4" -> Assert.AreEqual<string>(output, res.Represent())
            | "base5" -> Assert.AreEqual<string>(output, res.Represent())
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", "$1",  literalFalse)>]
    [<DataRow("base2", "$2",  literalTrue)>]
    [<DataRow("base3", "$3",  literalFalse)>]
    [<DataRow("base4", "$0",  "")>]
    [<DataRow("base5", "$4", "")>]
    [<TestMethod>]
    member this.TestConditionResultStatement(var, input, (output:string)) =
        ad.Clear()
        let fplCode = sprintf """
                def pred Equal infix "=" 50 (x,y: tpl)
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
        let filename = "TestConditionResultStatementFplRepresentation"
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
            | "base1" -> 
                let cr = res.ArgList[0] 
                Assert.AreEqual<string>(output, cr.Represent())
            | "base2" -> 
                let cr = res.ArgList[1] 
                Assert.AreEqual<string>(output, cr.Represent())
            | "base3" -> 
                let cr = res.ArgList[2] 
                Assert.AreEqual<string>(output, cr.Represent())
            | "base4" -> () 
            | "base5" -> ()
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", "dec ~x:pred x:=false;",  literalFalse)>]
    [<TestMethod>]
    member this.TestAssignmentVariableReferenceTheSame(no:string, input, (expected:string)) =
        ad.Clear()
        let fplCode = sprintf "def pred T() {%s true};" input
        let filename = "TestAssignment"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope["T()"]
            let assignPre = pred.ArgList[0]
            let assign = assignPre :?> FplAssignment
            let assigneeVariableOpt = assign.Assignee
            match assigneeVariableOpt with
            | Some var -> 
                let xVar = pred.Scope["x"]
                Assert.AreEqual<bool>(true, LanguagePrimitives.PhysicalEquality xVar var)
            | None -> 
                Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", "dec ~x:pred x:=false;",  literalFalse)>]
    [<TestMethod>]
    member this.TestAssignmentValue(no:string, input, (expected:string)) =
        ad.Clear()
        let fplCode = sprintf "def pred T() {%s true};" input
        let filename = "TestAssignment"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope["T()"]
            let variableStack = new FplVariableStack()
            let assignPre = pred.ArgList[0]
            let assign = assignPre :?> FplAssignment
            assign.Run variableStack
            let assigneeVariableOpt = assign.Assignee
            match assigneeVariableOpt with
            | Some var -> 
                let actual = var.Represent()
                Assert.AreEqual<string>(expected, actual)
            | None -> 
                Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("00", "dec ~x:pred x:=false;",  true)>]
    [<TestMethod>]
    member this.TestAssignmentVariableInitialized(no:string, input, (expected:bool)) =
        ad.Clear()
        let fplCode = sprintf "def pred T() {%s true};" input
        let filename = "TestAssignment"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let r = st.Root
            let theory = r.Scope[filename]
            let pred = theory.Scope["T()"]
            let variableStack = new FplVariableStack()
            let assignPre = pred.ArgList[0]
            let assign = assignPre :?> FplAssignment
            assign.Run variableStack
            let assigneeVariableOpt = assign.Assignee
            match assigneeVariableOpt with
            | Some var -> 
                let actual = var.IsInitializedVariable
                Assert.AreEqual<bool>(expected, actual)
            | None -> 
                Assert.IsTrue(false)

        | None -> 
            Assert.IsTrue(false)
