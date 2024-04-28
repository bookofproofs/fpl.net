namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeBlockTypeName() =

    [<TestMethod>]
    member this.TestBlocks() =
        CommonFplValueTestCases.ScopeBlocks() |> ignore
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestConstructors() =
        CommonFplValueTestCases.ScopeConstructors() |> ignore
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestProofsAndCorollaries() =
        CommonFplValueTestCases.ScopeVariablesInSignatureWithVariadic() |> ignore
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestProperties() =
        CommonFplValueTestCases.ScopeProperties() |> ignore
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
            | "r" -> Assert.AreEqual("a root", r.BlockType.Name)
            | "theory" -> Assert.AreEqual("a theory", theory.BlockType.Name)
            | "block" -> Assert.AreEqual("a predicate definition", block.BlockType.Name)
            | "x" -> Assert.AreEqual("a variable", x.BlockType.Name)
            | "y" -> Assert.AreEqual("a variable", y.BlockType.Name)
            | "xu" -> Assert.AreEqual("a variable", xu.BlockType.Name)
            | "xv" -> Assert.AreEqual("a variable", xv.BlockType.Name)
            | "xw" -> Assert.AreEqual("a variable", xw.BlockType.Name)
            | "yu" -> Assert.AreEqual("a variable", yu.BlockType.Name)
            | "yv" -> Assert.AreEqual("a variable", yv.BlockType.Name)
            | "yw" -> Assert.AreEqual("a variable", yw.BlockType.Name)
            | "xua" -> Assert.AreEqual("a variable", xua.BlockType.Name)
            | "xub" -> Assert.AreEqual("a variable", xub.BlockType.Name)
            | "xuc" -> Assert.AreEqual("a variable", xuc.BlockType.Name)
            | "xva" -> Assert.AreEqual("a variable", xva.BlockType.Name)
            | "xvb" -> Assert.AreEqual("a variable", xvb.BlockType.Name)
            | "xvc" -> Assert.AreEqual("a variable", xvc.BlockType.Name)
            | "xwa" -> Assert.AreEqual("a variable", xwa.BlockType.Name)
            | "xwb" -> Assert.AreEqual("a variable", xwb.BlockType.Name)
            | "xwc" -> Assert.AreEqual("a variable", xwc.BlockType.Name)
            | "yua" -> Assert.AreEqual("a variable", yua.BlockType.Name)
            | "yub" -> Assert.AreEqual("a variable", yub.BlockType.Name)
            | "yuc" -> Assert.AreEqual("a variable", yuc.BlockType.Name)
            | "yva" -> Assert.AreEqual("a variable", yva.BlockType.Name)
            | "yvb" -> Assert.AreEqual("a variable", yvb.BlockType.Name)
            | "yvc" -> Assert.AreEqual("a variable", yvc.BlockType.Name)
            | "ywa" -> Assert.AreEqual("a variable", ywa.BlockType.Name)
            | "ywb" -> Assert.AreEqual("a variable", ywb.BlockType.Name)
            | "ywc" -> Assert.AreEqual("a variable", ywc.BlockType.Name)
            | _ -> ()
        | None -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)