namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeBlockType() =

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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Class, block.BlockType)
            | "t1" -> Assert.AreEqual(FplBlockType.Constructor, t1.BlockType)
            | "t2" -> Assert.AreEqual(FplBlockType.Constructor, t2.BlockType)
            | "t3" -> Assert.AreEqual(FplBlockType.Constructor, t3.BlockType)
            | "t4" -> Assert.AreEqual(FplBlockType.Constructor, t4.BlockType)
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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Predicate, block.BlockType)
            | "t1" -> Assert.AreEqual(FplBlockType.MandatoryProperty, t1.BlockType)
            | "t2" -> Assert.AreEqual(FplBlockType.OptionalProperty, t2.BlockType)
            | "t3" -> Assert.AreEqual(FplBlockType.MandatoryProperty, t3.BlockType)
            | "t4" -> Assert.AreEqual(FplBlockType.OptionalProperty, t4.BlockType)
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
            | "r" -> Assert.AreEqual(FplBlockType.Root, r.BlockType)
            | "theory" -> Assert.AreEqual(FplBlockType.Theory, theory.BlockType)
            | "block" -> Assert.AreEqual(FplBlockType.Predicate, block.BlockType)
            | "x" -> Assert.AreEqual(FplBlockType.Variable, x.BlockType)
            | "y" -> Assert.AreEqual(FplBlockType.Variable, y.BlockType)
            | "xu" -> Assert.AreEqual(FplBlockType.Variable, xu.BlockType)
            | "xv" -> Assert.AreEqual(FplBlockType.Variable, xv.BlockType)
            | "xw" -> Assert.AreEqual(FplBlockType.Variable, xw.BlockType)
            | "yu" -> Assert.AreEqual(FplBlockType.Variable, yu.BlockType)
            | "yv" -> Assert.AreEqual(FplBlockType.Variable, yv.BlockType)
            | "yw" -> Assert.AreEqual(FplBlockType.Variable, yw.BlockType)
            | "xua" -> Assert.AreEqual(FplBlockType.Variable, xua.BlockType)
            | "xub" -> Assert.AreEqual(FplBlockType.Variable, xub.BlockType)
            | "xuc" -> Assert.AreEqual(FplBlockType.Variable, xuc.BlockType)
            | "xva" -> Assert.AreEqual(FplBlockType.Variable, xva.BlockType)
            | "xvb" -> Assert.AreEqual(FplBlockType.Variable, xvb.BlockType)
            | "xvc" -> Assert.AreEqual(FplBlockType.Variable, xvc.BlockType)
            | "xwa" -> Assert.AreEqual(FplBlockType.Variable, xwa.BlockType)
            | "xwb" -> Assert.AreEqual(FplBlockType.Variable, xwb.BlockType)
            | "xwc" -> Assert.AreEqual(FplBlockType.Variable, xwc.BlockType)
            | "yua" -> Assert.AreEqual(FplBlockType.Variable, yua.BlockType)
            | "yub" -> Assert.AreEqual(FplBlockType.Variable, yub.BlockType)
            | "yuc" -> Assert.AreEqual(FplBlockType.Variable, yuc.BlockType)
            | "yva" -> Assert.AreEqual(FplBlockType.Variable, yva.BlockType)
            | "yvb" -> Assert.AreEqual(FplBlockType.Variable, yvb.BlockType)
            | "yvc" -> Assert.AreEqual(FplBlockType.Variable, yvc.BlockType)
            | "ywa" -> Assert.AreEqual(FplBlockType.Variable, ywa.BlockType)
            | "ywb" -> Assert.AreEqual(FplBlockType.Variable, ywb.BlockType)
            | "ywc" -> Assert.AreEqual(FplBlockType.Variable, ywc.BlockType)
            | _ -> ()
        | None -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)