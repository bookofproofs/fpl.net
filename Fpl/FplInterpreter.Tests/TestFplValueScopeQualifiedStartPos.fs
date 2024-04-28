namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeQualifiedStartPos() =

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
            | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual("Test(Ln: 1, Col: 1)", theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual("Test(Ln: 2, Col: 13)", block.QualifiedStartPos)
            | "t1" -> Assert.AreEqual("Test(Ln: 5, Col: 13)", t1.QualifiedStartPos)
            | "t2" -> Assert.AreEqual("Test(Ln: 6, Col: 13)", t2.QualifiedStartPos)
            | "t3" -> Assert.AreEqual("Test(Ln: 7, Col: 13)", t3.QualifiedStartPos)
            | "t4" -> Assert.AreEqual("Test(Ln: 8, Col: 13)", t4.QualifiedStartPos)
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
            | "r" -> Assert.AreEqual("", r.QualifiedStartPos)
            | "theory" -> Assert.AreEqual("Test(Ln: 1, Col: 1)", theory.QualifiedStartPos)
            | "block" -> Assert.AreEqual("Test(Ln: 2, Col: 13)", block.QualifiedStartPos)
            | "x" -> Assert.AreEqual("Test(Ln: 2, Col: 32)", x.QualifiedStartPos)
            | "y" -> Assert.AreEqual("Test(Ln: 2, Col: 34)", y.QualifiedStartPos)
            | "xu" -> Assert.AreEqual("Test(Ln: 2, Col: 41)", xu.QualifiedStartPos)
            | "xv" -> Assert.AreEqual("Test(Ln: 2, Col: 43)", xv.QualifiedStartPos)
            | "xw" -> Assert.AreEqual("Test(Ln: 2, Col: 45)", xw.QualifiedStartPos)
            | "yu" -> Assert.AreEqual("Test(Ln: 2, Col: 41)", yu.QualifiedStartPos)
            | "yv" -> Assert.AreEqual("Test(Ln: 2, Col: 43)", yv.QualifiedStartPos)
            | "yw" -> Assert.AreEqual("Test(Ln: 2, Col: 45)", yw.QualifiedStartPos)
            | "xua" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", xua.QualifiedStartPos)
            | "xub" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", xub.QualifiedStartPos)
            | "xuc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", xuc.QualifiedStartPos)
            | "xva" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", xva.QualifiedStartPos)
            | "xvb" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", xvb.QualifiedStartPos)
            | "xvc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", xvc.QualifiedStartPos)
            | "xwa" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", xwa.QualifiedStartPos)
            | "xwb" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", xwb.QualifiedStartPos)
            | "xwc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", xwc.QualifiedStartPos)
            | "yua" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", yua.QualifiedStartPos)
            | "yub" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", yub.QualifiedStartPos)
            | "yuc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", yuc.QualifiedStartPos)
            | "yva" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", yva.QualifiedStartPos)
            | "yvb" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", yvb.QualifiedStartPos)
            | "yvc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", yvc.QualifiedStartPos)
            | "ywa" -> Assert.AreEqual("Test(Ln: 2, Col: 52)", ywa.QualifiedStartPos)
            | "ywb" -> Assert.AreEqual("Test(Ln: 2, Col: 54)", ywb.QualifiedStartPos)
            | "ywc" -> Assert.AreEqual("Test(Ln: 2, Col: 56)", ywc.QualifiedStartPos)
            | _ -> ()
        | None -> ()

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)