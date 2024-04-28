namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeQualifiedName() =

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
            | "r" -> Assert.AreEqual("", r.QualifiedName)
            | "theory" -> Assert.AreEqual("Test", theory.QualifiedName)
            | "block" -> Assert.AreEqual("Test.TestPredicate()", block.QualifiedName)
            | "x" -> Assert.AreEqual("x", x.QualifiedName)
            | "y" -> Assert.AreEqual("y", y.QualifiedName)
            | "xu" -> Assert.AreEqual("x.u", xu.QualifiedName)
            | "xv" -> Assert.AreEqual("x.v", xv.QualifiedName)
            | "xw" -> Assert.AreEqual("x.w", xw.QualifiedName)
            | "yu" -> Assert.AreEqual("y.u", yu.QualifiedName)
            | "yv" -> Assert.AreEqual("y.v", yv.QualifiedName)
            | "yw" -> Assert.AreEqual("y.w", yw.QualifiedName)
            | "xua" -> Assert.AreEqual("x.u.a", xua.QualifiedName)
            | "xub" -> Assert.AreEqual("x.u.b", xub.QualifiedName)
            | "xuc" -> Assert.AreEqual("x.u.c", xuc.QualifiedName)
            | "xva" -> Assert.AreEqual("x.v.a", xva.QualifiedName)
            | "xvb" -> Assert.AreEqual("x.v.b", xvb.QualifiedName)
            | "xvc" -> Assert.AreEqual("x.v.c", xvc.QualifiedName)
            | "xwa" -> Assert.AreEqual("x.w.a", xwa.QualifiedName)
            | "xwb" -> Assert.AreEqual("x.w.b", xwb.QualifiedName)
            | "xwc" -> Assert.AreEqual("x.w.c", xwc.QualifiedName)
            | "yua" -> Assert.AreEqual("y.u.a", yua.QualifiedName)
            | "yub" -> Assert.AreEqual("y.u.b", yub.QualifiedName)
            | "yuc" -> Assert.AreEqual("y.u.c", yuc.QualifiedName)
            | "yva" -> Assert.AreEqual("y.v.a", yva.QualifiedName)
            | "yvb" -> Assert.AreEqual("y.v.b", yvb.QualifiedName)
            | "yvc" -> Assert.AreEqual("y.v.c", yvc.QualifiedName)
            | "ywa" -> Assert.AreEqual("y.w.a", ywa.QualifiedName)
            | "ywb" -> Assert.AreEqual("y.w.b", ywb.QualifiedName)
            | "ywc" -> Assert.AreEqual("y.w.c", ywc.QualifiedName)
            | _ -> ()
        | None -> ()

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)