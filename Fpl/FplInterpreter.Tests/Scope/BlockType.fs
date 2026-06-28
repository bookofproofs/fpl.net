namespace Scope

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Primitives
open Fpl.Interpreter.BasicTypes
open Fpl.Interpreter.SymbolTable.Types1.TopLevel
open Fpl.Interpreter.SymbolTable.Storage.Heap
open Fpl.Interpreter.SymbolTable.Types2.Intrinsic
open Fpl.Interpreter.SymbolTable.Types2.Variables
open Fpl.Interpreter.SymbolTable.Types2.CompoundPredicates
open Fpl.Interpreter.SymbolTable.Types2.References
open Fpl.Interpreter.SymbolTable.Types2.Definitions
open Fpl.Interpreter.SymbolTable.Types3.SelfParent
open Fpl.Interpreter.SymbolTable.Types3.PredicativeBlocks
open Fpl.Interpreter.SymbolTable.Types3.DefinitionProperties
open Fpl.Interpreter.SymbolTable.Types3.Quantors
open Fpl.Interpreter.SymbolTable.Types3.RulesOfInferences
open Fpl.Interpreter.SymbolTable.Types3.Extensions
open Fpl.Interpreter.SymbolTable.Types3.Delegates
open Fpl.Interpreter.SymbolTable.Types3.MapCases
open Fpl.Interpreter.SymbolTable.Types3.Localization
open Fpl.Interpreter.SymbolTable.Types3.IsOperator
open Fpl.Interpreter.SymbolTable.Types4.Proofs
open TestFplInterpreter.Helpers.Common

[<TestClass>]
type BlockType() =

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
    [<DataRow("inf1")>]
    [<DataRow("inf2")>]
    [<DataRow("axi1")>]
    [<DataRow("axi2")>]
    [<DataRow("pst1")>]
    [<DataRow("pst2")>]
    [<DataRow("thm1")>]
    [<DataRow("thm2")>]
    [<DataRow("pro1")>]
    [<DataRow("pro2")>]
    [<DataRow("lem1")>]
    [<DataRow("lem2")>]
    [<DataRow("cor1")>]
    [<DataRow("cor2")>]
    [<DataRow("con1")>]
    [<DataRow("con2")>]
    [<DataRow("cla1")>]
    [<DataRow("cla2")>]
    [<DataRow("pre1")>]
    [<DataRow("pre2")>]
    [<DataRow("fun1")>]
    [<DataRow("fun2")>]
    [<DataRow("fun3")>]
    [<DataRow("fun4")>]
    [<DataRow("fun5")>]
    [<DataRow("fun6")>]
    [<DataRow("fun7")>]
    [<DataRow("fun8")>]
    [<DataRow("fun9")>]
    [<DataRow("prf1")>]
    [<DataRow("prf2")>]
    [<DataRow("loc1")>]
    [<DataRow("loc2")>]
    [<TestMethod>]
    member this.TestBlocks(var) =
        let res = TestCases.ScopeBlocks("BlockType") 
        match res with
        | Some (r:FplRoot,theory:FplGenericNode,inf1:FplGenericNode,inf2:FplGenericNode,axi1:FplGenericNode,axi2:FplGenericNode,pst1:FplGenericNode,pst2:FplGenericNode,thm1:FplGenericNode,thm2:FplGenericNode,pro1:FplGenericNode,pro2:FplGenericNode,lem1:FplGenericNode,lem2:FplGenericNode,cor1:FplGenericNode,cor2:FplGenericNode,con1:FplGenericNode,con2:FplGenericNode,cla1:FplGenericNode,cla2:FplGenericNode,pre1:FplGenericNode,pre2:FplGenericNode,fun1:FplGenericNode,fun2:FplGenericNode,fun3:FplGenericNode,fun4:FplGenericNode,fun5:FplGenericNode,fun6:FplGenericNode,fun7:FplGenericNode,fun8:FplGenericNode,fun9:FplGenericNode,prf1:FplGenericNode,prf2:FplGenericNode,loc1:FplGenericNode,loc2:FplGenericNode) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType(r, typeof<FplRoot>)
            | PrimTheoryL -> Assert.IsInstanceOfType(theory, typeof<FplTheory>)
            | "inf1" -> Assert.IsInstanceOfType(inf1, typeof<FplRuleOfInference>)
            | "inf2" -> Assert.IsInstanceOfType(inf2, typeof<FplRuleOfInference>)
            | "axi1" -> Assert.IsInstanceOfType(axi1, typeof<FplAxiom>)
            | "axi2" -> Assert.IsInstanceOfType(axi2, typeof<FplAxiom>)
            | "pst1" -> Assert.IsInstanceOfType(pst1, typeof<FplAxiom>)
            | "pst2" -> Assert.IsInstanceOfType(pst2, typeof<FplAxiom>)
            | "thm1" -> Assert.IsInstanceOfType(thm1, typeof<FplTheorem>)
            | "thm2" -> Assert.IsInstanceOfType(thm2, typeof<FplTheorem>)
            | "lem1" -> Assert.IsInstanceOfType(lem1, typeof<FplLemma>)
            | "lem2" -> Assert.IsInstanceOfType(lem2, typeof<FplLemma>)
            | "pro1" -> Assert.IsInstanceOfType(pro1, typeof<FplProposition>)
            | "pro2" -> Assert.IsInstanceOfType(pro2, typeof<FplProposition>)
            | "cor1" -> Assert.IsInstanceOfType(cor1, typeof<FplCorollary>)
            | "cor2" -> Assert.IsInstanceOfType(cor2, typeof<FplCorollary>)
            | "con1" -> Assert.IsInstanceOfType(con1, typeof<FplConjecture>)
            | "con2" -> Assert.IsInstanceOfType(con2, typeof<FplConjecture>)
            | "pre1" -> Assert.IsInstanceOfType(pre1, typeof<FplPredicate>)
            | "pre2" -> Assert.IsInstanceOfType(pre2, typeof<FplPredicate>)
            | "cla1" -> Assert.IsInstanceOfType(cla1, typeof<FplClass>)
            | "cla2" -> Assert.IsInstanceOfType(cla2, typeof<FplClass>)
            | "fun1" -> Assert.IsInstanceOfType(fun1, typeof<FplFunctionalTerm>)
            | "fun2" -> Assert.IsInstanceOfType(fun2, typeof<FplFunctionalTerm>)
            | "fun3" -> Assert.IsInstanceOfType(fun3, typeof<FplFunctionalTerm>)
            | "fun4" -> Assert.IsInstanceOfType(fun4, typeof<FplFunctionalTerm>)
            | "fun5" -> Assert.IsInstanceOfType(fun5, typeof<FplFunctionalTerm>)
            | "fun6" -> Assert.IsInstanceOfType(fun6, typeof<FplFunctionalTerm>)
            | "fun7" -> Assert.IsInstanceOfType(fun7, typeof<FplFunctionalTerm>)
            | "fun8" -> Assert.IsInstanceOfType(fun8, typeof<FplFunctionalTerm>)
            | "fun9" -> Assert.IsInstanceOfType(fun9, typeof<FplFunctionalTerm>)
            | "prf1" -> Assert.IsInstanceOfType(prf1, typeof<FplProof>)
            | "prf2" -> Assert.IsInstanceOfType(prf2, typeof<FplProof>)
            | "loc1" -> Assert.IsInstanceOfType(loc1, typeof<FplLocalization>)
            | "loc2" -> Assert.IsInstanceOfType(loc2, typeof<FplLocalization>)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
    [<DataRow("block")>]
    [<DataRow("t1")>]
    [<DataRow("t2")>]
    [<DataRow("t3")>]
    [<DataRow("t4")>]
    [<TestMethod>]
    member this.TestConstructors(var) =
        let res = TestCases.ScopeConstructors("BlockType") 
        match res with
        | Some (r,theory,block:FplGenericNode,t1:FplGenericNode,t2:FplGenericNode,t3:FplGenericNode,t4:FplGenericNode) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType(r, typeof<FplRoot>)
            | PrimTheoryL -> Assert.IsInstanceOfType(theory, typeof<FplTheory>)
            | "block" -> Assert.IsInstanceOfType(block, typeof<FplClass>)
            | "t1" -> Assert.IsInstanceOfType(t1, typeof<FplConstructor>)
            | "t2" -> Assert.IsInstanceOfType(t2, typeof<FplConstructor>)
            | "t3" -> Assert.IsInstanceOfType(t3, typeof<FplConstructor>)
            | "t4" -> Assert.IsInstanceOfType(t4, typeof<FplConstructor>)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
    member this.TestProofsAndCorollaries(var) =
        let res = TestCases.ScopeProofsAndCorollaries("BlockType") 
        match res with
        | Some (r,theory,thm1,proofThm1,lem1,proofLem1,prp1,proofPrp1,cor1,proofCor1,thm2,
                                corThm2,lem2,corLem2,prp2,corPrp2,cor2,corCor2,con1,corCon1,
                                axi1,corAxi1) -> 
                match var with
                | "r" -> Assert.IsInstanceOfType(r, typeof<FplRoot>)
                | PrimTheoryL -> Assert.IsInstanceOfType(theory, typeof<FplTheory>)
                | "thm1" -> Assert.IsInstanceOfType(thm1, typeof<FplTheorem>)
                | "proofThm1" -> Assert.IsInstanceOfType(proofThm1, typeof<FplProof>)
                | "thm2" -> Assert.IsInstanceOfType(thm2, typeof<FplTheorem>)
                | "lem1" -> Assert.IsInstanceOfType(lem1, typeof<FplLemma>)
                | "proofLem1" -> Assert.IsInstanceOfType(proofLem1, typeof<FplProof>)
                | "lem2" -> Assert.IsInstanceOfType(lem2, typeof<FplLemma>)
                | "cor1" -> Assert.IsInstanceOfType(cor1, typeof<FplCorollary>)
                | "proofCor1" -> Assert.IsInstanceOfType(proofCor1, typeof<FplProof>)
                | "cor2" -> Assert.IsInstanceOfType(cor2, typeof<FplCorollary>)
                | "con1" -> Assert.IsInstanceOfType(con1, typeof<FplConjecture>)
                | "corCon1" -> Assert.IsInstanceOfType(corCon1, typeof<FplCorollary>)
                | "prp1" -> Assert.IsInstanceOfType(prp1, typeof<FplProposition>)
                | "proofPrp1" -> Assert.IsInstanceOfType(proofPrp1, typeof<FplProof>)
                | "prp2" -> Assert.IsInstanceOfType(prp2, typeof<FplProposition>)
                | "corPrp2" -> Assert.IsInstanceOfType(corPrp2, typeof<FplCorollary>)
                | "corThm2" -> Assert.IsInstanceOfType(corThm2, typeof<FplCorollary>)
                | "corLem2" -> Assert.IsInstanceOfType(corLem2, typeof<FplCorollary>)
                | "corCor2" -> Assert.IsInstanceOfType(corCor2, typeof<FplCorollary>)
                | "axi1" -> Assert.IsInstanceOfType(axi1, typeof<FplAxiom>)
                | "corAxi1"  -> Assert.IsInstanceOfType(corAxi1, typeof<FplCorollary>) 
                | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
    [<DataRow("block")>]
    [<DataRow("t1")>]
    [<DataRow("t3")>]
    [<DataRow("t5")>]
    [<DataRow("t7")>]
    [<DataRow("t9")>]
    [<DataRow("t11")>]
    [<DataRow("t13")>]
    [<TestMethod>]
    member this.TestProperties(var) =
        let res = TestCases.ScopeProperties("BlockType") 
        match res with
        | Some (r:FplRoot,theory:FplGenericNode,block:FplGenericNode,t1:FplGenericNode,t3:FplGenericNode,t5:FplGenericNode,t7:FplGenericNode,t9:FplGenericNode,t11:FplGenericNode,t13:FplGenericNode) -> 
            match var with 
            | "r" -> Assert.IsInstanceOfType(r, typeof<FplRoot>)
            | PrimTheoryL -> Assert.IsInstanceOfType(theory, typeof<FplTheory>)
            | "block" -> Assert.IsInstanceOfType(block, typeof<FplPredicate>)
            | "t1" -> Assert.IsInstanceOfType(t1, typeof<FplMandatoryPredicate>)
            | "t3" -> Assert.IsInstanceOfType(t3, typeof<FplMandatoryFunctionalTerm>)
            | "t5" -> Assert.IsInstanceOfType(t5, typeof<FplMandatoryFunctionalTerm>)
            | "t7" -> Assert.IsInstanceOfType(t7, typeof<FplMandatoryFunctionalTerm>)
            | "t9" -> Assert.IsInstanceOfType(t9, typeof<FplMandatoryFunctionalTerm>)
            | "t11" -> Assert.IsInstanceOfType(t11, typeof<FplMandatoryFunctionalTerm>)
            | "t13" -> Assert.IsInstanceOfType(t13, typeof<FplMandatoryFunctionalTerm>)
            | _ -> Assert.IsTrue(false)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
    [<DataRow("block")>]
    [<DataRow("s")>]
    [<DataRow("x")>]
    [<DataRow("y")>]
    [<DataRow("s")>]
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
    member this.TestVariablesInBlock(var) =
        let result = TestCases.ScopeVariablesInBlock("BlockType")
        match result with
        | Some (r,theory,block,x,y,s,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType(r, typeof<FplRoot>)
            | PrimTheoryL -> Assert.IsInstanceOfType(theory, typeof<FplTheory>)
            | "block" -> Assert.IsInstanceOfType(block, typeof<FplPredicate>)
            | "s" -> Assert.IsInstanceOfType(s, typeof<FplVariable>)
            | "x" -> Assert.IsInstanceOfType(x, typeof<FplVariable>)
            | "y" -> Assert.IsInstanceOfType(y, typeof<FplVariable>)
            | "xu" -> Assert.IsInstanceOfType(xu, typeof<FplVariable>) 
            | "xv" -> Assert.IsInstanceOfType(xv, typeof<FplVariable>)
            | "xw" -> Assert.IsInstanceOfType(xw, typeof<FplVariable>) 
            | "yu" -> Assert.IsInstanceOfType(yu, typeof<FplVariable>) 
            | "yv" -> Assert.IsInstanceOfType(yv, typeof<FplVariable>) 
            | "yw" -> Assert.IsInstanceOfType(yw, typeof<FplVariable>) 
            | "xua" -> Assert.IsInstanceOfType(xua, typeof<FplVariable>)
            | "xub" -> Assert.IsInstanceOfType(xub, typeof<FplVariable>)
            | "xuc" -> Assert.IsInstanceOfType(xuc, typeof<FplVariable>)
            | "xva" -> Assert.IsInstanceOfType(xva, typeof<FplVariable>)
            | "xvb" -> Assert.IsInstanceOfType(xvb, typeof<FplVariable>)
            | "xvc" -> Assert.IsInstanceOfType(xvc, typeof<FplVariable>)
            | "xwa" -> Assert.IsInstanceOfType(xwa, typeof<FplVariable>)
            | "xwb" -> Assert.IsInstanceOfType(xwb, typeof<FplVariable>)
            | "xwc" -> Assert.IsInstanceOfType(xwc, typeof<FplVariable>)
            | "yua" -> Assert.IsInstanceOfType(yua, typeof<FplVariable>)
            | "yub" -> Assert.IsInstanceOfType(yub, typeof<FplVariable>)
            | "yuc" -> Assert.IsInstanceOfType(yuc, typeof<FplVariable>)
            | "yva" -> Assert.IsInstanceOfType(yva, typeof<FplVariable>)
            | "yvb" -> Assert.IsInstanceOfType(yvb, typeof<FplVariable>)
            | "yvc" -> Assert.IsInstanceOfType(yvc, typeof<FplVariable>)
            | "ywa" -> Assert.IsInstanceOfType(ywa, typeof<FplVariable>)
            | "ywb" -> Assert.IsInstanceOfType(ywb, typeof<FplVariable>)
            | "ywc" -> Assert.IsInstanceOfType(ywc, typeof<FplVariable>)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
    member this.TestVariablesInBlockVariadic(var) =
        let result = TestCases.ScopeVariablesInBlockVariadic("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType(r, typeof<FplRoot>)
            | PrimTheoryL -> Assert.IsInstanceOfType(theory, typeof<FplTheory>)
            | "block" -> Assert.IsInstanceOfType(block, typeof<FplPredicate>)
            | "x" -> Assert.IsInstanceOfType(x, typeof<FplVariableArray>)
            | "y" -> Assert.IsInstanceOfType(y, typeof<FplVariableArray>)
            | "xu" -> Assert.IsInstanceOfType(xu, typeof<FplVariable>) 
            | "xv" -> Assert.IsInstanceOfType(xv, typeof<FplVariable>)
            | "xw" -> Assert.IsInstanceOfType(xw, typeof<FplVariable>) 
            | "yu" -> Assert.IsInstanceOfType(yu, typeof<FplVariable>) 
            | "yv" -> Assert.IsInstanceOfType(yv, typeof<FplVariable>) 
            | "yw" -> Assert.IsInstanceOfType(yw, typeof<FplVariable>) 
            | "xua" -> Assert.IsInstanceOfType(xua, typeof<FplVariableArray>) 
            | "xub" -> Assert.IsInstanceOfType(xub, typeof<FplVariableArray>)
            | "xuc" -> Assert.IsInstanceOfType(xuc, typeof<FplVariableArray>)
            | "xva" -> Assert.IsInstanceOfType(xva, typeof<FplVariableArray>)
            | "xvb" -> Assert.IsInstanceOfType(xvb, typeof<FplVariableArray>)
            | "xvc" -> Assert.IsInstanceOfType(xvc, typeof<FplVariableArray>)
            | "xwa" -> Assert.IsInstanceOfType(xwa, typeof<FplVariableArray>)
            | "xwb" -> Assert.IsInstanceOfType(xwb, typeof<FplVariableArray>)
            | "xwc" -> Assert.IsInstanceOfType(xwc, typeof<FplVariableArray>)
            | "yua" -> Assert.IsInstanceOfType(yua, typeof<FplVariableArray>)
            | "yub" -> Assert.IsInstanceOfType(yub, typeof<FplVariableArray>)
            | "yuc" -> Assert.IsInstanceOfType(yuc, typeof<FplVariableArray>)
            | "yva" -> Assert.IsInstanceOfType(yva, typeof<FplVariableArray>)
            | "yvb" -> Assert.IsInstanceOfType(yvb, typeof<FplVariableArray>)
            | "yvc" -> Assert.IsInstanceOfType(yvc, typeof<FplVariableArray>)
            | "ywa" -> Assert.IsInstanceOfType(ywa, typeof<FplVariableArray>)
            | "ywb" -> Assert.IsInstanceOfType(ywb, typeof<FplVariableArray>)
            | "ywc" -> Assert.IsInstanceOfType(ywc, typeof<FplVariableArray>)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)


    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
        let result = TestCases.ScopeVariablesInSignature("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType(r, typeof<FplRoot>)
            | PrimTheoryL -> Assert.IsInstanceOfType(theory, typeof<FplTheory>)
            | "block" -> Assert.IsInstanceOfType(block, typeof<FplPredicate>)
            | "x" -> Assert.IsInstanceOfType(x, typeof<FplVariable>)
            | "y" -> Assert.IsInstanceOfType(y, typeof<FplVariable>)
            | "xu" -> Assert.IsInstanceOfType(xu, typeof<FplVariable>) 
            | "xv" -> Assert.IsInstanceOfType(xv, typeof<FplVariable>) 
            | "xw" -> Assert.IsInstanceOfType(xw, typeof<FplVariable>) 
            | "yu" -> Assert.IsInstanceOfType(yu, typeof<FplVariable>) 
            | "yv" -> Assert.IsInstanceOfType(yv, typeof<FplVariable>) 
            | "yw" -> Assert.IsInstanceOfType(yw, typeof<FplVariable>) 
            | "xua" -> Assert.IsInstanceOfType(xua, typeof<FplVariable>)
            | "xub" -> Assert.IsInstanceOfType(xub, typeof<FplVariable>)
            | "xuc" -> Assert.IsInstanceOfType(xuc, typeof<FplVariable>)
            | "xva" -> Assert.IsInstanceOfType(xva, typeof<FplVariable>)
            | "xvb" -> Assert.IsInstanceOfType(xvb, typeof<FplVariable>)
            | "xvc" -> Assert.IsInstanceOfType(xvc, typeof<FplVariable>)
            | "xwa" -> Assert.IsInstanceOfType(xwa, typeof<FplVariable>)
            | "xwb" -> Assert.IsInstanceOfType(xwb, typeof<FplVariable>)
            | "xwc" -> Assert.IsInstanceOfType(xwc, typeof<FplVariable>)
            | "yua" -> Assert.IsInstanceOfType(yua, typeof<FplVariable>)
            | "yub" -> Assert.IsInstanceOfType(yub, typeof<FplVariable>)
            | "yuc" -> Assert.IsInstanceOfType(yuc, typeof<FplVariable>)
            | "yva" -> Assert.IsInstanceOfType(yva, typeof<FplVariable>)
            | "yvb" -> Assert.IsInstanceOfType(yvb, typeof<FplVariable>)
            | "yvc" -> Assert.IsInstanceOfType(yvc, typeof<FplVariable>)
            | "ywa" -> Assert.IsInstanceOfType(ywa, typeof<FplVariable>)
            | "ywb" -> Assert.IsInstanceOfType(ywb, typeof<FplVariable>)
            | "ywc" -> Assert.IsInstanceOfType(ywc, typeof<FplVariable>)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("r")>]
    [<DataRow(PrimTheoryL)>]
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
    member this.TestVariablesInSignatureVariadic(var) =
        let result = TestCases.ScopeVariablesInSignatureVariadic("BlockType")
        match result with
        | Some (r,theory,block,x,y,xw,xu,xv,yw,yu,yv,xwa,xwb,xwc,xua,xub,xuc,xva,xvb,xvc,ywa,ywb,ywc,yua,yub,yuc,yva,yvb,yvc) ->
            match var with
            | "r" -> Assert.IsInstanceOfType(r, typeof<FplRoot>)
            | PrimTheoryL -> Assert.IsInstanceOfType(theory, typeof<FplTheory>)
            | "block" -> Assert.IsInstanceOfType(block, typeof<FplPredicate>)
            | "x" -> Assert.IsInstanceOfType(x, typeof<FplVariableArray>)
            | "y" -> Assert.IsInstanceOfType(y, typeof<FplVariableArray>)
            | "xu" -> Assert.IsInstanceOfType(xu, typeof<FplVariable>) 
            | "xv" -> Assert.IsInstanceOfType(xv, typeof<FplVariable>)
            | "xw" -> Assert.IsInstanceOfType(xw, typeof<FplVariable>) 
            | "yu" -> Assert.IsInstanceOfType(yu, typeof<FplVariable>) 
            | "yv" -> Assert.IsInstanceOfType(yv, typeof<FplVariable>) 
            | "yw" -> Assert.IsInstanceOfType(yw, typeof<FplVariable>) 
            | "xua" -> Assert.IsInstanceOfType(xua, typeof<FplVariableArray>) 
            | "xub" -> Assert.IsInstanceOfType(xub, typeof<FplVariableArray>)
            | "xuc" -> Assert.IsInstanceOfType(xuc, typeof<FplVariableArray>)
            | "xva" -> Assert.IsInstanceOfType(xva, typeof<FplVariableArray>)
            | "xvb" -> Assert.IsInstanceOfType(xvb, typeof<FplVariableArray>)
            | "xvc" -> Assert.IsInstanceOfType(xvc, typeof<FplVariableArray>)
            | "xwa" -> Assert.IsInstanceOfType(xwa, typeof<FplVariableArray>)
            | "xwb" -> Assert.IsInstanceOfType(xwb, typeof<FplVariableArray>)
            | "xwc" -> Assert.IsInstanceOfType(xwc, typeof<FplVariableArray>)
            | "yua" -> Assert.IsInstanceOfType(yua, typeof<FplVariableArray>)
            | "yub" -> Assert.IsInstanceOfType(yub, typeof<FplVariableArray>)
            | "yuc" -> Assert.IsInstanceOfType(yuc, typeof<FplVariableArray>)
            | "yva" -> Assert.IsInstanceOfType(yva, typeof<FplVariableArray>)
            | "yvb" -> Assert.IsInstanceOfType(yvb, typeof<FplVariableArray>)
            | "yvc" -> Assert.IsInstanceOfType(yvc, typeof<FplVariableArray>)
            | "ywa" -> Assert.IsInstanceOfType(ywa, typeof<FplVariableArray>)
            | "ywb" -> Assert.IsInstanceOfType(ywb, typeof<FplVariableArray>)
            | "ywc" -> Assert.IsInstanceOfType(ywc, typeof<FplVariableArray>)
            | _ -> Assert.IsTrue(false)
        | None -> 
            Assert.IsTrue(false)

    [<DataRow("base1", LiteralTrue)>]
    [<DataRow("base2", LiteralFalse)>]
    [<DataRow("base3", LiteralUndef)>]
    [<DataRow("base4", "-1")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "$1")>]
    [<DataRow("base7", "Test1(x)")>] 
    [<DataRow("base8", "Test$1")>]
    [<DataRow("base9", "Test1()")>]
    [<DataRow("base10", "Test")>]
    [<DataRow("base11", "v")>]
    [<DataRow("base12", LiteralSelf)>]
    [<DataRow("base13", "@1")>]
    [<DataRow("base11a", "v.x")>]
    [<DataRow("base12a", "self.x")>]
    [<DataRow("base10b", "Test()")>]
    [<DataRow("base11b", "v()")>]
    [<DataRow("base12b", "self()")>]
    [<DataRow("base13b", "@1()")>]
    [<DataRow("base10c", "Test(x, y)")>]
    [<DataRow("base11c", "v(x, y)")>]
    [<DataRow("base12c", "self(x, y)")>]
    [<DataRow("base13c", "@1(x, y)")>]
    [<DataRow("base10d", "Test[x, y]")>]
    [<DataRow("base11d", "v[x, y]")>]
    [<DataRow("base12d", "self[x, y]")>]
    [<DataRow("base13d", "@1[x.y]")>]
    [<DataRow("base10e", "Test(x, y).parent[a, b]")>]
    [<DataRow("base11e", "v(x, y).x[a, b]")>]
    [<DataRow("base12e", "self(x, y).@3[a, b]")>]
    [<DataRow("base13e", "@1(x, y).T[a, b]")>]
    [<DataRow("base10f", "Test[x, y].x(a, b)")>]
    [<DataRow("base11f", "v[x, y].x(a, b)")>]
    [<DataRow("base12f", "self[x, y].self(a, b)")>]
    [<DataRow("base13f", "@1[x.y].T(a, b)")>]
    [<DataRow("base14", "∅")>]
    [<DataRow("base15", "-x")>]
    [<DataRow("base15a", "x'")>]
    [<DataRow("base15b", "-x'")>]
    [<DataRow("base16", "-(y + x = @2 * x)")>]
    [<DataRow("base17", "(y + x' = @2 * x)'")>]
    [<DataRow("base18", "ex x:pred(a:obj,b:T), y:C, z:obj {and (a,and(b,c))}")>]
    [<DataRow("base19", "exn$1 x:obj {all y:N {true}}")>]
    [<DataRow("base20", "all x:obj {not x}")>]
    [<DataRow("base21", "and (x, and(y, z))")>]
    [<DataRow("base21a", "not x")>]
    [<DataRow("base21b", "not (x)")>]
    [<DataRow("base22", "xor (x, xor(y, z))")>]
    [<DataRow("base23", "or (x, or(y, z))")>]
    [<DataRow("base24", "iif (x, y)")>]
    [<DataRow("base25", "impl (x, y)")>]
    [<DataRow("base26", "is (x, Nat)")>]
    [<DataRow("base27", "B()")>]
    [<DataRow("base28", "C(a,b,c,d)")>]
    [<DataRow("base29", "D(self,b,c)")>]
    [<DataRow("base30", "B(In(x))")>]
    [<DataRow("base31", "C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base32", "E(true, undef, false)")>]
    [<DataRow("base33", "dec p: pred(c: obj); p(c)")>]
    [<DataRow("base34", "is(x, Set)")>]
    [<TestMethod>]
    member this.TestPredicate(var, varVal) =
        
        let fplCode = sprintf "def pred T1() { %s }" varVal
        let filename = "TestPredicateBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]

        let pr1 = theory.Scope["T1()"] 
        let base1 = pr1.ArgList[0]

        match var with
        | "base1" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base2" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base3" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base4" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base5" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base6" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base7" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base8" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base9" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base10" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base11" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base12" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base13" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base11a" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base12a" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base10b" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base11b" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base12b" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base13b" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base10c" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base11c" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base12c" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base13c" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base10d" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base11d" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base12d" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base13d" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base10e" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base11e" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base12e" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base13e" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base10f" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base11f" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base12f" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base13f" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base14" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base15" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base15a" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base15b" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base16" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base17" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base18" -> Assert.IsInstanceOfType(base1, typeof<FplGenericQuantor>) 
        | "base19" -> Assert.IsInstanceOfType(base1, typeof<FplGenericQuantor>)  
        | "base20" -> Assert.IsInstanceOfType(base1, typeof<FplGenericQuantor>) 
        | "base21" -> Assert.IsInstanceOfType(base1, typeof<FplConjunction>)
        | "base21a" -> Assert.IsInstanceOfType(base1, typeof<FplNegation>)
        | "base21b" -> Assert.IsInstanceOfType(base1, typeof<FplNegation>)
        | "base22" -> Assert.IsInstanceOfType(base1, typeof<FplExclusiveOr>)
        | "base23" -> Assert.IsInstanceOfType(base1, typeof<FplDisjunction>)
        | "base24" -> Assert.IsInstanceOfType(base1, typeof<FplEquivalence>)
        | "base25" -> Assert.IsInstanceOfType(base1, typeof<FplImplication>)
        | "base26" -> Assert.IsInstanceOfType(base1, typeof<FplIsOperator>)
        | "base27" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base28" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base29" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base30" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base31" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base32" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base33" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base34" -> Assert.IsInstanceOfType(base1, typeof<FplIsOperator>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", "base.B()")>]
    [<DataRow("base2", "base.C(a, b, c, d)")>]
    [<DataRow("base3", "base.D(self, a, b)")>]
    [<DataRow("base4", "base.B(In(x))")>]
    [<DataRow("base5", "base.C(Test1(a), Test2(b, c, d))")>]
    [<DataRow("base6", "base.E(true, undef, false)")>]
    [<TestMethod>]
    member this.TestBaseConstructorCallBlockType(var, varVal) =
        
        let fplCode = sprintf """
                        def cl B {intr}
                        def cl C {intr}
                        def cl D {intr}

                        def cl A:B,C,D,E
                        {
                            ctor A(a:T1, b:func, c:ind, d:pred) 
                            {
                                dec
                                    %s
                                ;
                                
                            }
                        }
                        """ varVal
        let filename = "TestBaseConstructorCallBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let cl = theory.Scope["A"]
        let ctor = cl.Scope["A(T1, func, ind, pred)"]
        let base1 = ctor.ArgList[0]

        match var with
        | "base1" -> Assert.IsInstanceOfType(base1, typeof<FplBaseConstructorCall>)
        | "base2" -> Assert.IsInstanceOfType(base1, typeof<FplBaseConstructorCall>)
        | "base3" -> Assert.IsInstanceOfType(base1, typeof<FplBaseConstructorCall>)
        | "base4" -> Assert.IsInstanceOfType(base1, typeof<FplBaseConstructorCall>)
        | "base5" -> Assert.IsInstanceOfType(base1, typeof<FplBaseConstructorCall>)
        | "base6" -> Assert.IsInstanceOfType(base1, typeof<FplBaseConstructorCall>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", "del.B()")>]
    [<DataRow("base2", "del.C(a,b,c,d)")>]
    [<DataRow("base3", "del.D(self,b,c)")>]
    [<DataRow("base4", "del.B(In(x))")>]
    [<DataRow("base5", "del.Test()")>]
    [<DataRow("base6", "del.C(Test1(a),Test2(b,c,d))")>]
    [<DataRow("base7", "del.E(true, undef, false)")>] 
    [<TestMethod>]
    member this.TestDelegate(var, varVal) =
        
        let fplCode = sprintf "def pred T1() { %s }" varVal
        let filename = "TestDelegateBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]

        let pr1 = theory.Scope["T1()"] 
        let base1 = pr1.ArgList[0]

        match var with
        | "base1" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base2" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base3" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base4" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base5" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base6" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | "base7" -> Assert.IsInstanceOfType(base1, typeof<FplReference>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", """def pred T1() {intr}""")>]
    [<DataRow("base2", """def pred T1() infix ">" -1 {intr}""")>]
    [<DataRow("base3", """def pred T1 () postfix "'" {intr}""")>]
    [<DataRow("base4", """def pred T1 () prefix "-" {intr}""")>]
    [<DataRow("base5", """def cl T1 symbol "∅" {intr}""")>]
    [<DataRow("base5a", """def cl T1 {intr}""")>]
    [<DataRow("base6", """def func T1()->obj {intr}""")>]
    [<DataRow("base7", """def func T1 ()->obj infix ">" -1 {intr}""")>]
    [<DataRow("base8", """def func T1 ()->obj postfix "'" {intr}""")>]
    [<DataRow("base9", """def func T1 ()->obj prefix "-" {intr}""")>]
    [<TestMethod>]
    member this.TestFixNotationBlockType(var, varVal) =
        
        let fplCode = sprintf "%s" varVal
        let filename = "TestFixNotationBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let base1 = 
            if varVal.Contains LiteralCl then 
                theory.Scope["T1"]
            elif varVal.Contains LiteralFunc then 
                theory.Scope["T1() -> obj"]
            else 
                theory.Scope["T1()"]

        match var with
        | "base1" -> Assert.IsInstanceOfType(base1, typeof<FplPredicate>)
        | "base2" -> Assert.IsInstanceOfType(base1, typeof<FplPredicate>)
        | "base3" -> Assert.IsInstanceOfType(base1, typeof<FplPredicate>)
        | "base4" -> Assert.IsInstanceOfType(base1, typeof<FplPredicate>)
        | "base5" -> Assert.IsInstanceOfType(base1, typeof<FplClass>)
        | "base5a" -> Assert.IsInstanceOfType(base1, typeof<FplClass>)
        | "base6" -> Assert.IsInstanceOfType(base1, typeof<FplFunctionalTerm>) 
        | "base7" -> Assert.IsInstanceOfType(base1, typeof<FplFunctionalTerm>) 
        | "base8" -> Assert.IsInstanceOfType(base1, typeof<FplFunctionalTerm>) 
        | "base9" -> Assert.IsInstanceOfType(base1, typeof<FplFunctionalTerm>) 
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", """def func T()->obj {intr}""")>]
    [<DataRow("base2", """def func T()->ind {intr}""")>]
    [<DataRow("base3", """def func T()->func {intr}""")>]
    [<DataRow("base4", """def func T()->pred {intr}""")>]
    [<DataRow("base5", """def cl A {intr} def func T()->A {intr}""")>]
    [<DataRow("base6", """def func T()->tpl {intr}""")>]
    [<DataRow("base7", """def func T()->pred(z:*obj[ind]) {intr}""")>]
    [<DataRow("base8", """def func T()->func(p:*pred(x:obj)[ind])->pred(x:ind) {intr}""")>]
    [<DataRow("base9", """def func T()->pred(f:*func(x:A)->A[ind]) {intr}""")>]
    [<DataRow("base10", """def cl A {intr} def func T()->pred(f:func(x:A)->A) {intr}""")>]
    [<TestMethod>]
    member this.TestMapping(var, varVal) =
        
        let fplCode = sprintf "%s" varVal
        let filename = "TestMappingBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let base1 = theory.Scope |> Seq.filter (fun kvp -> kvp.Key.StartsWith("T(")) |> Seq.map (fun kvp -> kvp.Value) |> Seq.toList |> List.head
        let mapping = base1.ArgList[0]
        match var with
        | "base1" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base2" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base3" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base4" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base5" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base6" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base7" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base8" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base9" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | "base10" -> Assert.IsInstanceOfType(mapping, typeof<FplMapping>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", """100: trivial""", 0)>]
    [<DataRow("base2", """100. ExistsByExample, 1 |- false""", 2)>]
    [<DataRow("base3", """100. T1 |- assume not somePremise """, 1)>]
    [<DataRow("base4", """100. 2, 3, 5 |- iif (a,b)""", 3)>]
    [<DataRow("base5", """100: revoke 3""", 0)>]
    [<TestMethod>]
    member this.TestArgumentNumberOfJustifications(var, argExpression, expNumber:int) =
        
        let fplCode = sprintf """proof T$1 { %s }""" argExpression
        let filename = "TestArgumentNumberOfJustifications"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let proof = theory.Scope["T$1"]
        let arg = proof.Scope["100"]
        let just = arg.ArgList[0]
        let numbOfJustifications = just.ArgList.Count
        Assert.AreEqual<int>(expNumber, numbOfJustifications)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", """100: trivial""")>]
    [<DataRow("base2", """100. ExistsByExample, 1 |- false""")>]
    [<DataRow("base3", """100. T1 |- assume not somePremise """)>]
    [<DataRow("base4", """100. 2, 3, 5 |- iif (a,b)""")>]
    [<DataRow("base5", """100: revoke 3""")>]
    [<TestMethod>]
    member this.TestArgumentBlockType(var, argExpression) =
        
        let fplCode = sprintf """proof T$1 { %s }""" argExpression
        let filename = "TestArgumentBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let proof = theory.Scope["T$1"]
        let arg = proof.Scope["100"]
        match var with
        | "base1" -> Assert.IsInstanceOfType(arg, typeof<FplArgument>)
        | "base2" -> Assert.IsInstanceOfType(arg, typeof<FplArgument>)
        | "base3" -> Assert.IsInstanceOfType(arg, typeof<FplArgument>)
        | "base4" -> Assert.IsInstanceOfType(arg, typeof<FplArgument>)
        | "base5" -> Assert.IsInstanceOfType(arg, typeof<FplArgument>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base0", LiteralTrue, LiteralTrue, """!tex: "1" !eng: "true" !ger: "wahr" """)>]
    [<DataRow("base1", "iif(undef, undef)", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y""")>]
    [<DataRow("base2", "not(undef)", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x""")>]
    [<DataRow("base3", "and(undef, undef)", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q""")>]
    [<DataRow("base4", "Equal(undef, undef)", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y""")>]
    [<DataRow("base5", "NotEqual(undef, undef)", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y""")>]
    [<TestMethod>]
    member this.TestLanguageBlockType(var, predName, predDecl, trslCode) =
        
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLanguageBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope[predName]
        let lang = pred.Scope["tex"]

        match var with
        | "base0" -> Assert.IsInstanceOfType(lang, typeof<FplLanguage>)
        | "base1" -> Assert.IsInstanceOfType(lang, typeof<FplLanguage>)
        | "base2" -> Assert.IsInstanceOfType(lang, typeof<FplLanguage>)
        | "base3" -> Assert.IsInstanceOfType(lang, typeof<FplLanguage>)
        | "base4" -> Assert.IsInstanceOfType(lang, typeof<FplLanguage>)
        | "base5" -> Assert.IsInstanceOfType(lang, typeof<FplLanguage>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base0", LiteralTrue, LiteralTrue, """!tex: "1" !eng: "true" !ger: "wahr" """)>]
    [<DataRow("base1", "iif(undef, undef)", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y""")>]
    [<DataRow("base2", "not(undef)", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x""")>]
    [<DataRow("base3", "and(undef, undef)", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q""")>]
    [<DataRow("base4", "Equal(undef, undef)", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y""")>]
    [<DataRow("base5", "NotEqual(undef, undef)", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y""")>]
    [<TestMethod>]
    member this.TestLocalizationBlockType(var, predName, predDecl, trslCode) =
        
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestLocalizationBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope[predName]

        match var with
        | "base0" -> Assert.IsInstanceOfType(pred, typeof<FplLocalization>)
        | "base1" -> Assert.IsInstanceOfType(pred, typeof<FplLocalization>)
        | "base2" -> Assert.IsInstanceOfType(pred, typeof<FplLocalization>)
        | "base3" -> Assert.IsInstanceOfType(pred, typeof<FplLocalization>)
        | "base4" -> Assert.IsInstanceOfType(pred, typeof<FplLocalization>)
        | "base5" -> Assert.IsInstanceOfType(pred, typeof<FplLocalization>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base0", LiteralTrue, LiteralTrue, """!tex: "1" !eng: "true" !ger: "wahr" """)>]
    [<DataRow("base1", "iif(undef, undef)", "iif(x, y)", """!tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y""")>]
    [<DataRow("base2", "not(undef)", "not(x)", """!tex: "\neg(" x ")" !eng: "not " x !ger: "nicht " x""")>]
    [<DataRow("base3", "and(undef, undef)", "and(p, q)", """!tex: p "\wedge" q !eng: p " and " q !ger: p " und " q""")>]
    [<DataRow("base4", "Equal(undef, undef)", "Equal(x, y)", """!tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y !ita: x " è uguale a " y !pol: x " równa się " y""")>]
    [<DataRow("base5", "NotEqual(undef, undef)", "NotEqual(x, y)", """!tex: x "\neq" y !eng: x "is unequal" y !ger: x "ist ungleich" y !pol: x ( "nie równa się" | "nie równe" ) y""")>]
    [<TestMethod>]
    member this.TestTranslationBlockType(var, predName, predDecl, trslCode) =
        
        let fplCode = sprintf """loc %s := %s;""" predDecl trslCode
        let filename = "TestTranslationBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope[predName]
        let lang = pred.Scope["tex"]
        let trsl = lang.ArgList[0]

        match var with
        | "base0" -> Assert.IsInstanceOfType(trsl, typeof<FplTranslation>)
        | "base1" -> Assert.IsInstanceOfType(trsl, typeof<FplTranslation>)
        | "base2" -> Assert.IsInstanceOfType(trsl, typeof<FplTranslation>)
        | "base3" -> Assert.IsInstanceOfType(trsl, typeof<FplTranslation>)
        | "base4" -> Assert.IsInstanceOfType(trsl, typeof<FplTranslation>)
        | "base5" -> Assert.IsInstanceOfType(trsl, typeof<FplTranslation>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("@0", "")>]
    [<DataRow("@1", "")>]
    [<DataRow("@2", "")>]
    [<DataRow("@3", "")>]
    [<DataRow("@4", "")>]
    [<DataRow("@100", "")>]
    [<DataRow("@42", "")>]
    [<TestMethod>]
    member this.TestDecrement(varVal, expected:string) =
        
        let fplCode = sprintf """def pred T() { del.Decrement(%s) }""" varVal
        let filename = "TestDecrementBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pr = theory.Scope["T()"] 
        let pre = pr.ArgList |> Seq.toList |> List.rev |> List.head
        let dec = pre.RefersTo.Value
        Assert.IsInstanceOfType(dec, typeof<FplDecrement>)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("@0", "")>]
    [<DataRow("@1", "")>]
    [<DataRow("@2", "")>]
    [<DataRow("@3", "")>]
    [<DataRow("@4", "")>]
    [<DataRow("@100", "")>]
    [<DataRow("@42", "")>]
    [<TestMethod>]
    member this.TestExtensionObj(varVal, expected:string) =
        
        let fplCode = sprintf """def pred T() { %s }""" varVal
        let filename = "TestExtensionObjBlockType.fpl"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pr = theory.Scope["T()"] 
        let basePre = pr.ArgList |> Seq.head
        let base1 = basePre.RefersTo.Value 
        Assert.IsInstanceOfType(base1, typeof<FplExtensionObj>)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", "$1")>]
    [<DataRow("base2", "$2")>]
    [<DataRow("base3", "$3")>]
    [<DataRow("base4", "$0")>]
    [<DataRow("base5", "$4")>]
    [<TestMethod>]
    member this.TestMCaseStatement(var, input) =
        
        let fplCode = sprintf """
                def pred Equal (x,y: tpl) infix "=" 50 
                {
                    del.Equal(x,y)
                }
                
                def pred Test(x:ind) { dec 
                n:pred
                n:= mcases
                (
                    | (x = $1): false 
                    | (x = $2): true 
                    | (x = $3): false 
                    ? undef  
                )
                ;n } def pred T() {Test(%s)}""" input 
        let filename = "TestMCaseStatement"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope["Test(ind)"]
        let assignment = pred.ArgList[0]
        let res = assignment.ArgList[1]
 
        match var with
        | "base1" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | "base2" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | "base3" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | "base4" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | "base5" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", "$1")>]
    [<DataRow("base2", "$2")>]
    [<DataRow("base3", "$3")>]
    [<DataRow("base4", "$0")>]
    [<DataRow("base5", "$4")>]
    [<TestMethod>]
    member this.TestMapCasesBlockType(var, input) =
        
        let fplCode = sprintf """
                def pred Equal (x,y: tpl) infix "=" 50 
                {
                    del.Equal(x,y)
                }              
                
                def pred Test(x:ind) { dec 
                n:pred
                n:= mcases
                (
                    | (x = $1): false 
                    | (x = $2): true 
                    | (x = $3): false 
                    ? undef  
                )
                ;n } def pred T() {Test(%s)}""" input 
        let filename = "TestMapCasesBlockType"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope["Test(ind)"]
        let assignment = pred.ArgList[0]
        let res = assignment.ArgList[1]

 
        match var with
        | "base1" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | "base2" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | "base3" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | "base4" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | "base5" -> Assert.IsInstanceOfType(res, typeof<FplMapCases>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore

    [<DataRow("base1", "$1",  0)>]
    [<DataRow("base2", "$2",  2)>]
    [<DataRow("base3", "$3",  1)>]
    [<DataRow("base4", "$0",  3)>]
    [<DataRow("base5", "$4", 1)>]
    [<TestMethod>]
    member this.TestCaseStatement(var, input, (output:int)) =
        
        let fplCode = sprintf """
                def pred Equal (x,y: tpl) infix "=" 50 
                {
                    del.Equal(x,y)
                }              
        
                def pred Test(x:ind) { dec 
                n:pred
                cases
                (
                    | (x = $1): n:=false 
                    | (x = $2): n:=true 
                    | (x = $3): n:=false 
                    ? n:=undef  
                )
                ;n } def pred T() {Test(%s)}""" input 
        let filename = "TestMCaseStatement"
        prepareFplCode(filename + ".fpl", fplCode, false) 
        let r = heap.Root
        let theory = r.Scope[filename]
        let pred = theory.Scope["Test(ind)"]
        let cases = pred.ArgList[0]
 
        match var with
        | "base1" -> Assert.IsInstanceOfType(cases, typeof<FplGenericStmt>)
        | "base2" -> Assert.IsInstanceOfType(cases, typeof<FplGenericStmt>)
        | "base3" -> Assert.IsInstanceOfType(cases, typeof<FplGenericStmt>)
        | "base4" -> Assert.IsInstanceOfType(cases, typeof<FplGenericStmt>)
        | "base5" -> Assert.IsInstanceOfType(cases, typeof<FplGenericStmt>)
        | _ -> Assert.IsTrue(false)
        prepareFplCode(filename, "", false) |> ignore
