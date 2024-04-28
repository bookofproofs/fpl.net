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
            | "r" -> Assert.AreEqual(Position("", 0, 1, 1), r.NameEndPos)
            | "theory" -> Assert.AreEqual(Position("", 0, 1, 1), theory.NameEndPos)
            | "block" -> Assert.AreEqual(Position("", 27, 2, 26), block.NameEndPos)
            | "t1" -> Assert.AreEqual(Position("", 0, 5, 27), t1.NameEndPos)
            | "t2" -> Assert.AreEqual(Position("", 0, 6, 31), t2.NameEndPos)
            | "t3" -> Assert.AreEqual(Position("", 0, 7, 32), t3.NameEndPos)
            | "t4" -> Assert.AreEqual(Position("", 0, 8, 41), t4.NameEndPos)
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