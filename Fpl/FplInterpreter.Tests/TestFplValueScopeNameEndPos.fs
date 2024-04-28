namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeNameEndPos() =

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
            | "r" -> Assert.AreEqual(Position("", 0, 1, 1), r.NameEndPos)
            | "theory" -> Assert.AreEqual(Position("", 0, 1, 1), theory.NameEndPos)
            | "block" -> Assert.AreEqual(Position("", 70, 2, 69), block.NameEndPos); 
            | "x" -> Assert.AreEqual(Position("", 69, 2, 68), x.NameEndPos)
            | "y" -> Assert.AreEqual(Position("", 69, 2, 68), y.NameEndPos)
            | "xu" -> Assert.AreEqual(Position("", 68, 2, 67), xu.NameEndPos)
            | "xv" -> Assert.AreEqual(Position("", 68, 2, 67), xv.NameEndPos)
            | "xw" -> Assert.AreEqual(Position("", 68, 2, 67), xw.NameEndPos)
            | "yu" -> Assert.AreEqual(Position("", 68, 2, 67), yu.NameEndPos)
            | "yv" -> Assert.AreEqual(Position("", 68, 2, 67), yv.NameEndPos)
            | "yw" -> Assert.AreEqual(Position("", 68, 2, 67), yw.NameEndPos)
            | "xua" -> Assert.AreEqual(Position("", 54, 2, 53), xua.NameEndPos)
            | "xub" -> Assert.AreEqual(Position("", 56, 2, 55), xub.NameEndPos)
            | "xuc" -> Assert.AreEqual(Position("", 58, 2, 57), xuc.NameEndPos)
            | "xva" -> Assert.AreEqual(Position("", 54, 2, 53), xva.NameEndPos)
            | "xvb" -> Assert.AreEqual(Position("", 56, 2, 55), xvb.NameEndPos)
            | "xvc" -> Assert.AreEqual(Position("", 58, 2, 57), xvc.NameEndPos)
            | "xwa" -> Assert.AreEqual(Position("", 54, 2, 53), xwa.NameEndPos)
            | "xwb" -> Assert.AreEqual(Position("", 56, 2, 55), xwb.NameEndPos)
            | "xwc" -> Assert.AreEqual(Position("", 58, 2, 57), xwc.NameEndPos)
            | "yua" -> Assert.AreEqual(Position("", 54, 2, 53), yua.NameEndPos)
            | "yub" -> Assert.AreEqual(Position("", 56, 2, 55), yub.NameEndPos)
            | "yuc" -> Assert.AreEqual(Position("", 58, 2, 57), yuc.NameEndPos)
            | "yva" -> Assert.AreEqual(Position("", 54, 2, 53), yva.NameEndPos)
            | "yvb" -> Assert.AreEqual(Position("", 56, 2, 55), yvb.NameEndPos)
            | "yvc" -> Assert.AreEqual(Position("", 58, 2, 57), yvc.NameEndPos)
            | "ywa" -> Assert.AreEqual(Position("", 54, 2, 53), ywa.NameEndPos)
            | "ywb" -> Assert.AreEqual(Position("", 56, 2, 55), ywb.NameEndPos)
            | "ywc" -> Assert.AreEqual(Position("", 58, 2, 57), ywc.NameEndPos)
            | _ -> ()
        | None -> 
            Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)