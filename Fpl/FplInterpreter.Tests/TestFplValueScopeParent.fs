namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplValueScopeParent() =

    [<TestMethod>]
    member this.TestBlocks() =
        Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow("theory")>]
    [<DataRow("thm1")>]
    [<DataRow("proofThm1")>]
    [<DataRow("lem1")>]
    [<DataRow("proofLem1")>]
    [<DataRow("prp1")>]
    [<DataRow("proofPrp1")>]
    [<DataRow("cor1")>]
    [<DataRow("proofCor1")>]
    [<DataRow("thm2")>]
    [<DataRow("corThm2")>]
    [<DataRow("lem2")>]
    [<DataRow("corLem2")>]
    [<DataRow("prp2")>]
    [<DataRow("corPrp2")>]
    [<DataRow("cor2")>]
    [<DataRow("corCor2")>]
    [<DataRow("con1")>]
    [<DataRow("corCon1")>]
    [<DataRow("axi1")>]
    [<DataRow("corAxi1")>]
    [<TestMethod>]
    member this.TestScopeProofsAndCorollariesCompleteQualifiedStartPos(var) =
        let res = CommonFplValueTestCases.ScopeProofsAndCorollaries() 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "theory" -> Assert.AreEqual(r, theory.Parent.Value)
                | "thm1" -> Assert.AreEqual(theory, thm1.Parent.Value)
                | "proofThm1" -> Assert.AreEqual(thm1, proofThm1.Parent.Value)
                | "lem1" -> Assert.AreEqual(theory, lem1.Parent.Value)
                | "proofLem1" -> Assert.AreEqual(lem1, proofLem1.Parent.Value)
                | "prp1" -> Assert.AreEqual(theory, prp1.Parent.Value)
                | "proofPrp1" -> Assert.AreEqual(prp1, proofPrp1.Parent.Value)
                | "cor1" -> Assert.AreEqual(theory, cor1.Parent.Value)
                | "proofCor1" -> Assert.AreEqual(cor1, proofCor1.Parent.Value)
                | "thm2" -> Assert.AreEqual(theory, thm2.Parent.Value)
                | "corThm2" -> Assert.AreEqual(thm2, corThm2.Parent.Value)
                | "lem2" -> Assert.AreEqual(theory, lem2.Parent.Value)
                | "corLem2" -> Assert.AreEqual(lem2, corLem2.Parent.Value)
                | "prp2" -> Assert.AreEqual(theory, prp2.Parent.Value)
                | "corPrp2" -> Assert.AreEqual(prp2, corPrp2.Parent.Value)
                | "cor2" -> Assert.AreEqual(theory, cor2.Parent.Value)
                | "corCor2" -> Assert.AreEqual(cor2, corCor2.Parent.Value)
                | "con1" -> Assert.AreEqual(theory, con1.Parent.Value)
                | "corCon1" -> Assert.AreEqual(con1, corCon1.Parent.Value)
                | "axi1" -> Assert.AreEqual(theory, axi1.Parent.Value)
                | "corAxi1"  -> Assert.AreEqual(axi1, corAxi1.Parent.Value) 
                | _ -> ()
        | _ -> 
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
            | "r" -> Assert.AreEqual(None, r.Parent)
            | "theory" -> Assert.AreEqual(r, theory.Parent.Value)
            | "block" -> Assert.AreEqual(theory, block.Parent.Value)
            | "t1" -> Assert.AreEqual(block, t1.Parent.Value)
            | "t2" -> Assert.AreEqual(block, t2.Parent.Value)
            | "t3" -> Assert.AreEqual(block, t3.Parent.Value)
            | "t4" -> Assert.AreEqual(block, t4.Parent.Value)
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
            | "r" -> Assert.AreEqual(None, r.Parent)
            | "theory" -> Assert.AreEqual(r, theory.Parent.Value)
            | "block" -> Assert.AreEqual(theory, block.Parent.Value)
            | "t1" -> Assert.AreEqual(block, t1.Parent.Value)
            | "t2" -> Assert.AreEqual(block, t2.Parent.Value)
            | "t3" -> Assert.AreEqual(block, t3.Parent.Value)
            | "t4" -> Assert.AreEqual(block, t4.Parent.Value)
            | _ -> ()
        | _ -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlock() =
        Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInBlockWithVariadic() =
        Assert.IsTrue(false)

    [<DataRow("theory")>]
    [<DataRow("block")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("xw")>]
    [<DataRow("xu")>]
    [<DataRow("xv")>]
    [<DataRow("yw")>]
    [<DataRow("yu")>]
    [<DataRow("yv")>]
    [<DataRow("xwa")>]
    [<DataRow("xwb")>]
    [<DataRow("xwc")>]
    [<DataRow("xua")>]
    [<DataRow("xub")>]
    [<DataRow("xuc")>]
    [<DataRow("xva")>]
    [<DataRow("xvb")>]
    [<DataRow("xvc")>]
    [<DataRow("ywa")>]
    [<DataRow("ywb")>]
    [<DataRow("ywc")>]
    [<DataRow("yua")>]
    [<DataRow("yub")>]
    [<DataRow("yuc")>]
    [<DataRow("yva")>]
    [<DataRow("yvb")>]
    [<DataRow("yvc")>]
    [<TestMethod>]
    member this.TestVariablesInSignature(var) =
        let result = CommonFplValueTestCases.ScopeVariablesInSignature()
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with 
            | "ywc" -> Assert.AreEqual(yw, ywc.Parent.Value)
            | "ywb" -> Assert.AreEqual(yw, ywb.Parent.Value)
            | "ywa" -> Assert.AreEqual(yw, ywa.Parent.Value)
            | "yvc" -> Assert.AreEqual(yv, yvc.Parent.Value)
            | "yvb" -> Assert.AreEqual(yv, yvb.Parent.Value)
            | "yva" -> Assert.AreEqual(yv, yva.Parent.Value)
            | "yuc" -> Assert.AreEqual(yu, yuc.Parent.Value)
            | "yub" -> Assert.AreEqual(yu, yub.Parent.Value)
            | "yua" -> Assert.AreEqual(yu, yua.Parent.Value)
            | "xwc" -> Assert.AreEqual(xw, xwc.Parent.Value)
            | "xwb" -> Assert.AreEqual(xw, xwb.Parent.Value)
            | "xwa" -> Assert.AreEqual(xw, xwa.Parent.Value)
            | "xvc" -> Assert.AreEqual(xv, xvc.Parent.Value)
            | "xvb" -> Assert.AreEqual(xv, xvb.Parent.Value)
            | "xva" -> Assert.AreEqual(xv, xva.Parent.Value)
            | "xuc" -> Assert.AreEqual(xu, xuc.Parent.Value)
            | "xub" -> Assert.AreEqual(xu, xub.Parent.Value)
            | "xua" -> Assert.AreEqual(xu, xua.Parent.Value)

            | "yw" -> Assert.AreEqual(y, yw.Parent.Value)
            | "yv" -> Assert.AreEqual(y, yv.Parent.Value)
            | "yu" -> Assert.AreEqual(y, yu.Parent.Value)
            | "xw" -> Assert.AreEqual(x, xw.Parent.Value)
            | "xv" -> Assert.AreEqual(x, xv.Parent.Value)
            | "xu" -> Assert.AreEqual(x,  xu.Parent.Value)
            | "y" -> Assert.AreEqual(block, y.Parent.Value)
            | "x" -> Assert.AreEqual(block, x.Parent.Value)
            | "block" -> Assert.AreEqual(theory, block.Parent.Value)
            | "theory" -> Assert.AreEqual(r, theory.Parent.Value)
            | _ -> 
                Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestVariablesInSignatureWithVariadic() =
        Assert.IsTrue(false)
