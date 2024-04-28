namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeEvaluationType() =

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
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "block" -> Assert.AreEqual(FplType.Object, block.EvaluationType)
            | "t1" -> Assert.AreEqual(FplType.Object, t1.EvaluationType)
            | "t2" -> Assert.AreEqual(FplType.Object, t2.EvaluationType)
            | "t3" -> Assert.AreEqual(FplType.Object, t3.EvaluationType)
            | "t4" -> Assert.AreEqual(FplType.Object, t4.EvaluationType)
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
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "block" -> Assert.AreEqual(FplType.Predicate, block.EvaluationType)
            | "t1" -> Assert.AreEqual(FplType.Predicate, t1.EvaluationType)
            | "t2" -> Assert.AreEqual(FplType.Predicate, t2.EvaluationType)
            | "t3" -> Assert.AreEqual(FplType.Object, t3.EvaluationType)
            | "t4" -> Assert.AreEqual(FplType.Object, t4.EvaluationType)
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
            | "r" -> Assert.AreEqual(FplType.Object, r.EvaluationType)
            | "theory" -> Assert.AreEqual(FplType.Predicate, theory.EvaluationType)
            | "block" -> Assert.AreEqual(FplType.Predicate, block.EvaluationType)
            | "x" -> Assert.AreEqual(FplType.Object, x.EvaluationType)
            | "y" -> Assert.AreEqual(FplType.Object, y.EvaluationType)
            | "xu" -> Assert.AreEqual(FplType.Object, xu.EvaluationType)
            | "xv" -> Assert.AreEqual(FplType.Object, xv.EvaluationType)
            | "xw" -> Assert.AreEqual(FplType.Object, xw.EvaluationType)
            | "yu" -> Assert.AreEqual(FplType.Object, yu.EvaluationType)
            | "yv" -> Assert.AreEqual(FplType.Object, yv.EvaluationType)
            | "yw" -> Assert.AreEqual(FplType.Object, yw.EvaluationType)
            | "xua" -> Assert.AreEqual(FplType.Object, xua.EvaluationType)
            | "xub" -> Assert.AreEqual(FplType.Object, xub.EvaluationType)
            | "xuc" -> Assert.AreEqual(FplType.Object, xuc.EvaluationType)
            | "xva" -> Assert.AreEqual(FplType.Object, xva.EvaluationType)
            | "xvb" -> Assert.AreEqual(FplType.Object, xvb.EvaluationType)
            | "xvc" -> Assert.AreEqual(FplType.Object, xvc.EvaluationType)
            | "xwa" -> Assert.AreEqual(FplType.Object, xwa.EvaluationType)
            | "xwb" -> Assert.AreEqual(FplType.Object, xwb.EvaluationType)
            | "xwc" -> Assert.AreEqual(FplType.Object, xwc.EvaluationType)
            | "yua" -> Assert.AreEqual(FplType.Object, yua.EvaluationType)
            | "yub" -> Assert.AreEqual(FplType.Object, yub.EvaluationType)
            | "yuc" -> Assert.AreEqual(FplType.Object, yuc.EvaluationType)
            | "yva" -> Assert.AreEqual(FplType.Object, yva.EvaluationType)
            | "yvb" -> Assert.AreEqual(FplType.Object, yvb.EvaluationType)
            | "yvc" -> Assert.AreEqual(FplType.Object, yvc.EvaluationType)
            | "ywa" -> Assert.AreEqual(FplType.Object, ywa.EvaluationType)
            | "ywb" -> Assert.AreEqual(FplType.Object, ywb.EvaluationType)
            | "ywc" -> Assert.AreEqual(FplType.Object, ywc.EvaluationType)
            | _ -> ()
        | None -> 
            Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)