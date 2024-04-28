namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeNameIsFinal() =

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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "t1" -> Assert.AreEqual(true, t1.NameIsFinal)
            | "t2" -> Assert.AreEqual(true, t2.NameIsFinal)
            | "t3" -> Assert.AreEqual(true, t3.NameIsFinal)
            | "t4" -> Assert.AreEqual(true, t4.NameIsFinal)
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
            | "r" -> Assert.AreEqual(true, r.NameIsFinal)
            | "theory" -> Assert.AreEqual(true, theory.NameIsFinal)
            | "block" -> Assert.AreEqual(true, block.NameIsFinal)
            | "t1" -> Assert.AreEqual(true, t1.NameIsFinal)
            | "t2" -> Assert.AreEqual(true, t2.NameIsFinal)
            | "t3" -> Assert.AreEqual(true, t3.NameIsFinal)
            | "t4" -> Assert.AreEqual(true, t4.NameIsFinal)
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
            | _ -> ()
        | None -> 
            Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)