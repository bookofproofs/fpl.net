namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeTypeSignature() =

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
            | "r" -> Assert.AreEqual([], r.TypeSignature)
            | "theory" -> Assert.AreEqual([], theory.TypeSignature)
            | "block" -> Assert.AreEqual(["TestPredicate"; "("; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"; "pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"; ")"], block.TypeSignature)
            | "x" -> Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], x.TypeSignature)
            | "y" -> Assert.AreEqual(["pred"; "("; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; "func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"; ")"], y.TypeSignature)
            | "xu" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xu.TypeSignature)
            | "xv" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xv.TypeSignature)
            | "xw" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], xw.TypeSignature)
            | "yu" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yu.TypeSignature)
            | "yv" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yv.TypeSignature)
            | "yw" -> Assert.AreEqual(["func"; "("; "obj"; "obj"; "obj"; ")"; "->"; "obj"], yw.TypeSignature)
            | "xua" -> Assert.AreEqual(["obj"], xua.TypeSignature)
            | "xub" -> Assert.AreEqual(["obj"], xub.TypeSignature)
            | "xuc" -> Assert.AreEqual(["obj"], xuc.TypeSignature)
            | "xva" -> Assert.AreEqual(["obj"], xva.TypeSignature)
            | "xvb" -> Assert.AreEqual(["obj"], xvb.TypeSignature)
            | "xvc" -> Assert.AreEqual(["obj"], xvc.TypeSignature)
            | "xwa" -> Assert.AreEqual(["obj"], xwa.TypeSignature)
            | "xwb" -> Assert.AreEqual(["obj"], xwb.TypeSignature)
            | "xwc" -> Assert.AreEqual(["obj"], xwc.TypeSignature)
            | "yua" -> Assert.AreEqual(["obj"], yua.TypeSignature)
            | "yub" -> Assert.AreEqual(["obj"], yub.TypeSignature)
            | "yuc" -> Assert.AreEqual(["obj"], yuc.TypeSignature)
            | "yva" -> Assert.AreEqual(["obj"], yva.TypeSignature)
            | "yvb" -> Assert.AreEqual(["obj"], yvb.TypeSignature)
            | "yvc" -> Assert.AreEqual(["obj"], yvc.TypeSignature)
            | "ywa" -> Assert.AreEqual(["obj"], ywa.TypeSignature)
            | "ywb" -> Assert.AreEqual(["obj"], ywb.TypeSignature)
            | "ywc" -> Assert.AreEqual(["obj"], ywc.TypeSignature)
            | _ -> Assert.IsTrue(false)
        | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)