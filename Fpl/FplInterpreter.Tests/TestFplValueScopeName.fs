namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeName() =

    [<TestMethod>]
    member this.TestBlocks() =
        CommonFplValueTestCases.ScopeBlocks() |> ignore
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestId", block.Name)
            | "t1" -> Assert.AreEqual("TestId()", t1.Name)
            | "t2" -> Assert.AreEqual("TestId(obj)", t2.Name)
            | "t3" -> Assert.AreEqual("TestId(pred)", t3.Name)
            | "t4" -> Assert.AreEqual("TestId(ind)", t4.Name)
            | _ -> ()
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestId()", block.Name)
            | "t1" -> Assert.AreEqual("T1()", t1.Name)
            | "t2" -> Assert.AreEqual("T2()", t2.Name)
            | "t3" -> Assert.AreEqual("T3() -> obj", t3.Name)
            | "t4" -> Assert.AreEqual("T4() -> obj", t4.Name)
            | _ -> ()
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
            | "r" -> Assert.AreEqual("", r.Name)
            | "theory" -> Assert.AreEqual("Test", theory.Name)
            | "block" -> Assert.AreEqual("TestPredicate(pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj), pred(func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj, func(obj, obj, obj) -> obj))", block.Name); 
            | "x" -> Assert.AreEqual("x", x.Name)
            | "y" -> Assert.AreEqual("y", y.Name)
            | "xu" -> Assert.AreEqual("u", xu.Name)
            | "xv" -> Assert.AreEqual("v", xv.Name)
            | "xw" -> Assert.AreEqual("w", xw.Name)
            | "yu" -> Assert.AreEqual("u", yu.Name)
            | "yv" -> Assert.AreEqual("v", yv.Name)
            | "yw" -> Assert.AreEqual("w", yw.Name)
            | "xua" -> Assert.AreEqual("a", xua.Name)
            | "xub" -> Assert.AreEqual("b", xub.Name)
            | "xuc" -> Assert.AreEqual("c", xuc.Name)
            | "xva" -> Assert.AreEqual("a", xva.Name)
            | "xvb" -> Assert.AreEqual("b", xvb.Name)
            | "xvc" -> Assert.AreEqual("c", xvc.Name)
            | "xwa" -> Assert.AreEqual("a", xwa.Name)
            | "xwb" -> Assert.AreEqual("b", xwb.Name)
            | "xwc" -> Assert.AreEqual("c", xwc.Name)
            | "yua" -> Assert.AreEqual("a", yua.Name)
            | "yub" -> Assert.AreEqual("b", yub.Name)
            | "yuc" -> Assert.AreEqual("c", yuc.Name)
            | "yva" -> Assert.AreEqual("a", yva.Name)
            | "yvb" -> Assert.AreEqual("b", yvb.Name)
            | "yvc" -> Assert.AreEqual("c", yvc.Name)
            | "ywa" -> Assert.AreEqual("a", ywa.Name)
            | "ywb" -> Assert.AreEqual("b", ywb.Name)
            | "ywc" -> Assert.AreEqual("c", ywc.Name)
            | _ -> ()
        | None -> 
            Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)