namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeNameIsFinal() =

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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "inf1" -> Assert.AreEqual(true, inf1.NameIsFinal)
            | "inf2" -> Assert.AreEqual(true, inf2.NameIsFinal)
            | "axi1" -> Assert.AreEqual(true, axi1.NameIsFinal)
            | "axi2" -> Assert.AreEqual(true, axi2.NameIsFinal)
            | "pst1" -> Assert.AreEqual(true, pst1.NameIsFinal)
            | "pst2" -> Assert.AreEqual(true, pst2.NameIsFinal)
            | "thm1" -> Assert.AreEqual(true, thm1.NameIsFinal)
            | "thm2" -> Assert.AreEqual(true, thm2.NameIsFinal)
            | "pro1" -> Assert.AreEqual(true, pro1.NameIsFinal)
            | "pro2" -> Assert.AreEqual(true, pro2.NameIsFinal)
            | "lem1" -> Assert.AreEqual(true, lem1.NameIsFinal)
            | "lem2" -> Assert.AreEqual(true, lem2.NameIsFinal)
            | "cor1" -> Assert.AreEqual(true, cor1.NameIsFinal)
            | "cor2" -> Assert.AreEqual(true, cor2.NameIsFinal)
            | "con1" -> Assert.AreEqual(true, con1.NameIsFinal)
            | "con2" -> Assert.AreEqual(true, con2.NameIsFinal)
            | "cla1" -> Assert.AreEqual(true, cla1.NameIsFinal)
            | "cla2" -> Assert.AreEqual(true, cla2.NameIsFinal)
            | "pre1" -> Assert.AreEqual(true, pre1.NameIsFinal)
            | "pre2" -> Assert.AreEqual(true, pre2.NameIsFinal)
            | "fun1" -> Assert.AreEqual(true, fun1.NameIsFinal)
            | "fun2" -> Assert.AreEqual(true, fun2.NameIsFinal)
            | "prf1" -> Assert.AreEqual(true, prf1.NameIsFinal)
            | "prf2" -> Assert.AreEqual(true, prf2.NameIsFinal)
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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "t1" -> Assert.AreEqual(true, t1.NameIsFinal)
            | "t2" -> Assert.AreEqual(true, t2.NameIsFinal)
            | "t3" -> Assert.AreEqual(true, t3.NameIsFinal)
            | "t4" -> Assert.AreEqual(true, t4.NameIsFinal)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestProofsAndCorollaries() =
        CommonFplValueTestCases.ScopeVariablesInSignatureWithVariadic() |> ignore
        Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("t1")>]
    [<DataRow("t2")>]
    [<DataRow("t3")>]
    [<DataRow("t4")>]
    [<TestMethod>]
    member this.TestProperties(var) =
        let res = CommonFplValueTestCases.ScopeProperties() 
        match res with
        | Some (r:FplValue,theory:FplValue,block:FplValue,t1:FplValue,t2:FplValue,t3:FplValue,t4:FplValue) -> 
            match var with 
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "t1" -> Assert.AreEqual(true, t1.NameIsFinal)
            | "t2" -> Assert.AreEqual(true, t2.NameIsFinal)
            | "t3" -> Assert.AreEqual(true, t3.NameIsFinal)
            | "t4" -> Assert.AreEqual(true, t4.NameIsFinal)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlock() =
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlockWithVariadic() =
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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "x" -> Assert.AreEqual(true, x.NameIsFinal)
            | "y" -> Assert.AreEqual(true, y.NameIsFinal)
            | "xu" -> Assert.AreEqual(true, xu.NameIsFinal)
            | "xv" -> Assert.AreEqual(true, xv.NameIsFinal)
            | "xw" -> Assert.AreEqual(true, xw.NameIsFinal)
            | "yu" -> Assert.AreEqual(true, yu.NameIsFinal)
            | "yv" -> Assert.AreEqual(true, yv.NameIsFinal)
            | "yw" -> Assert.AreEqual(true, yw.NameIsFinal)
            | "xua" -> Assert.AreEqual(true, xua.NameIsFinal)
            | "xub" -> Assert.AreEqual(true, xub.NameIsFinal)
            | "xuc" -> Assert.AreEqual(true, xuc.NameIsFinal)
            | "xva" -> Assert.AreEqual(true, xva.NameIsFinal)
            | "xvb" -> Assert.AreEqual(true, xvb.NameIsFinal)
            | "xvc" -> Assert.AreEqual(true, xvc.NameIsFinal)
            | "xwa" -> Assert.AreEqual(true, xwa.NameIsFinal)
            | "xwb" -> Assert.AreEqual(true, xwb.NameIsFinal)
            | "xwc" -> Assert.AreEqual(true, xwc.NameIsFinal)
            | "yua" -> Assert.AreEqual(true, yua.NameIsFinal)
            | "yub" -> Assert.AreEqual(true, yub.NameIsFinal)
            | "yuc" -> Assert.AreEqual(true, yuc.NameIsFinal)
            | "yva" -> Assert.AreEqual(true, yva.NameIsFinal)
            | "yvb" -> Assert.AreEqual(true, yvb.NameIsFinal)
            | "yvc" -> Assert.AreEqual(true, yvc.NameIsFinal)
            | "ywa" -> Assert.AreEqual(true, ywa.NameIsFinal)
            | "ywb" -> Assert.AreEqual(true, ywb.NameIsFinal)
            | "ywc" -> Assert.AreEqual(true, ywc.NameIsFinal)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)