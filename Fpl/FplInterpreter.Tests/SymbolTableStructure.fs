namespace FplInterpreter.Tests
open System
open System.Collections.Generic
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplPrimitives
open FplInterpreterTypes
open CommonTestHelpers

[<TestClass>]
type SymbolTableStructure() =
    let positions = (Position("",0,0,0), Position("",0,0,0))
    let parent = new FplRoot()

    let getName nodeType = 
        match nodeType with
        | "FplArgInferenceAssume" ->
            let x = new FplArgInferenceAssume(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplArgInferenceDerived" ->
            let x = new FplArgInferenceDerived(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplArgInferenceRevoke" ->
            let x = new FplArgInferenceRevoke(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplArgInferenceTrivial" ->
            let x = new FplArgInferenceTrivial(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplCaseElse" ->
            let x = new FplCaseElse(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplCaseSingle" ->
            let x = new FplCaseSingle(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplBaseConstructorCall" ->
            let x = new FplBaseConstructorCall(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplForInStmt" ->
            let x = new FplForInStmt(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplForInStmtDomain" ->
            let x = new FplForInStmtDomain(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplForInStmtEntity" ->
            let x = new FplForInStmtEntity(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplIntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplIntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplIntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplIntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplIntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplIntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplIsOperator" ->
            let x = new FplIsOperator(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustification" ->
            let x = new FplJustification(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustificationItemByAx" ->
            let x = new FplJustificationItemByAx(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustificationItemByCor" ->
            let x = new FplJustificationItemByCor(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustificationItemByDef" ->
            let x = new FplJustificationItemByDef(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustificationItemByDefVar" ->
            let x = new FplJustificationItemByDefVar(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustificationItemByInf" ->
            let x = new FplJustificationItemByInf(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustificationItemByProofArgument" ->
            let x = new FplJustificationItemByProofArgument(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustificationItemByRefArgument" ->
            let x = new FplJustificationItemByRefArgument(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplJustificationItemByTheoremLikeStmt" ->
            let x = new FplJustificationItemByTheoremLikeStmt(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplMapCaseElse" ->
            let x = new FplMapCaseElse(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplMapCaseSingle" ->
            let x = new FplMapCaseSingle(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplPredicateList" ->
            let x = new FplPredicateList(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplQuantorAll" ->
            let x = new FplQuantorAll(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplQuantorExists" ->
            let x = new FplQuantorExists(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplQuantorExistsN" ->
            let x = new FplQuantorExistsN(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplRoot" ->
            let x = new FplRoot()
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplTheory" ->
            let x = new FplTheory("", parent, "", 0)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplVariable" -> 
            let x = new FplVariable(positions, parent) 
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            [x.Name; x.ShortName; x.FplId; x.TypeId; $"""{match x.RunOrder with Some _ -> "Some" | None -> "None"}"""]
        | _ -> 
            failwith $"Unknown node type {nodeType}"


    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicFunc")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicObj")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariadicVariableMany")>]
    [<DataRow("FplVariadicVariableMany1")>]
    [<TestMethod>]
    member this.TestBlockTypeName(var) =
        let index = 0 // name 
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(PrimArgInfAssume, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(PrimArgInfDerive, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(PrimArgInfRevoke, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(PrimArgInfTrivial, (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>(PrimArgL, (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>(PrimAssertion, (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>(PrimAssignment, (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>(LiteralAxL, (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>(PrimBaseConstructorCall, (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>(PrimCases, (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>(PrimCaseElse, (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>(PrimCaseSingle, (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>(PrimClassL, (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>(LiteralConjL, (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>(PrimConjunction, (getName var).[index])        
        | "FplConstructor" ->
            Assert.AreEqual<string>(LiteralCtorL, (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>(LiteralCorL, (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>(PrimDecrementL, (getName var).[index])        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(PrimDisjunction, (getName var).[index])        
        | "FplEquality" ->
            Assert.AreEqual<string>(PrimEqualityL, (getName var).[index])        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(PrimEquivalence, (getName var).[index])        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(PrimExclusiveOr, (getName var).[index])        
        | "FplExtension" ->
            Assert.AreEqual<string>(PrimExtensionL, (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>(PrimExtensionObj, (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>(PrimForInStmt, (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>(PrimForInStmtDomain, (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>(PrimForInStmtEntity, (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>(PrimFuncionalTermL, (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>(PrimImplication, (getName var).[index])        
        | "FplInstance" ->
            Assert.AreEqual<string>(PrimInstanceL, (getName var).[index])
        | "FplIntrinsicFunc" ->
            Assert.AreEqual<string>(PrimIntrinsicFunc, (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(PrimIntrinsicInd, (getName var).[index])
        | "FplIntrinsicObj" ->
            Assert.AreEqual<string>(PrimIntrinsicObj, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(PrimIntrinsicPred, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(PrimIntrinsicTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(PrimIntrinsicUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>(PrimIsOperator, (getName var).[index])        
        | "FplJustification" ->
            Assert.AreEqual<string>(PrimJustificationL, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>(PrimJIByAx, (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>(PrimJIByCor, (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>(PrimJIByDef, (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>(PrimJIByDefVar, (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>(PrimJIByInf, (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>(PrimJIByProofArgument, (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>(PrimJIByRefArgument, (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>(PrimJIByTheoremLikeStmt, (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>(PrimLanguageL, (getName var).[index])        
        | "FplLemma" ->
            Assert.AreEqual<string>(LiteralLemL, (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>(LiteralLocL, (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>(PrimMandatoryFunctionalTermL, (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(PrimMandatoryPredicateL, (getName var).[index])        
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>(PrimMapCaseElse, (getName var).[index])        
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>(PrimMapCaseSingle, (getName var).[index])
        | "FplMapCases" ->
            Assert.AreEqual<string>(PrimMapCases, (getName var).[index])        
        | "FplMapping" ->
            Assert.AreEqual<string>(PrimMappingL, (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>(PrimNegation, (getName var).[index])        
        | "FplOptionalFunctionalTerm" ->
            Assert.AreEqual<string>(PrimOptionalFunctionalTermL, (getName var).[index])
        | "FplOptionalPredicate" ->
            Assert.AreEqual<string>(PrimOptionalPredicateL, (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>(PrimPredicateL, (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>(LiteralPreL, (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>(LiteralPrfL, (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>(LiteralPropL, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(PrimQuantorAll, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(PrimQuantorExists, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(PrimQuantorExistsN, (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>(PrimRefL, (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>(PrimReturn, (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>(PrimRoot, (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(PrimRuleOfInference, (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>(LiteralThmL, (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>(PrimTheoryL, (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>(PrimTranslationL, (getName var).[index])
        | "FplVariable" -> 
            Assert.AreEqual<string>(PrimVariableL, (getName var).[index])
        | "FplVariadicVariableMany" ->
            Assert.AreEqual<string>(PrimVariableManyL, (getName var).[index])
        | "FplVariadicVariableMany1" ->
            Assert.AreEqual<string>(PrimVariableMany1L, (getName var).[index])
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicFunc")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicObj")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariadicVariableMany")>]
    [<DataRow("FplVariadicVariableMany1")>]
    [<TestMethod>]
    member this.TestFplBlockTypeShortName(var) =
        let index = 1 // short name 
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(PrimArgInf, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(PrimArgInf, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(PrimArgInf, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(PrimArgInf, (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>(PrimArg, (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>(LiteralAss, (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>(LiteralAx, (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>(PrimClass, (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>(LiteralConj, (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>(LiteralAnd, (getName var).[index])        
        | "FplConstructor" ->
            Assert.AreEqual<string>(LiteralCtor, (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>(LiteralCor, (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>(PrimDecrement, (getName var).[index])        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(LiteralOr, (getName var).[index])        
        | "FplEquality" ->
            Assert.AreEqual<string>(PrimEquality, (getName var).[index])        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(LiteralIif, (getName var).[index])        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(LiteralXor, (getName var).[index])        
        | "FplExtension" ->
            Assert.AreEqual<string>(PrimExtension, (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>(PrimFuncionalTerm, (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>(LiteralImpl, (getName var).[index])        
        | "FplInstance" ->
            Assert.AreEqual<string>(PrimInstance, (getName var).[index])
        | "FplIntrinsicFunc" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(LiteralInd, (getName var).[index])
        | "FplIntrinsicObj" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(LiteralTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>(LiteralIs, (getName var).[index])        
        | "FplJustification" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>(PrimJustification, (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>(PrimLanguage, (getName var).[index])
        | "FplLemma" ->
            Assert.AreEqual<string>(LiteralLem, (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>(LiteralLoc, (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>(PrimMandatoryFunctionalTerm, (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(PrimMandatoryPredicate, (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])        
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])        
        | "FplMapCases" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])        
        | "FplMapping" ->
            Assert.AreEqual<string>(PrimMapping, (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>(LiteralNot, (getName var).[index])
        | "FplOptionalFunctionalTerm" ->
            Assert.AreEqual<string>(PrimOptionalFunctionalTerm, (getName var).[index])
        | "FplOptionalPredicate" ->
            Assert.AreEqual<string>(PrimOptionalPredicate, (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>(PrimPredicate, (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>(LiteralInf, (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>(LiteralPrf, (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>(LiteralProp, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(PrimQuantor, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(PrimQuantor, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(PrimQuantor, (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>(PrimRef, (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>(PrimStmt, (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>(PrimRoot, (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(LiteralInf, (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>(LiteralThm, (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>(PrimTheory, (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>(PrimTranslation, (getName var).[index])
        | "FplVariable" ->
            Assert.AreEqual<string>(PrimVariable, (getName var).[index])
        | "FplVariadicVariableMany" ->
            Assert.AreEqual<string>(PrimVariableMany, (getName var).[index])
        | "FplVariadicVariableMany1" ->
            Assert.AreEqual<string>(PrimVariableMany1, (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, var)

    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicFunc")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicObj")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariadicVariableMany")>]
    [<DataRow("FplVariadicVariableMany1")>]
    [<TestMethod>]
    member this.TestFplBlockTypeFplId(var) =
        let index = 2 // FplId
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>("assign (ln 0)", (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>(LiteralAnd, (getName var).[index])        
        | "FplConstructor" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>("del.", (getName var).[index])        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(LiteralOr, (getName var).[index])        
        | "FplEquality" ->
            Assert.AreEqual<string>("del.", (getName var).[index])        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(LiteralIif, (getName var).[index])        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(LiteralXor, (getName var).[index])        
        | "FplExtension" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>(LiteralImpl, (getName var).[index])        
        | "FplInstance" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplIntrinsicFunc" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(LiteralInd, (getName var).[index])
        | "FplIntrinsicObj" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(LiteralTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>(LiteralIs, (getName var).[index])        
        | "FplJustification" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLemma" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCases" ->
            Assert.AreEqual<string>("", (getName var).[index])        
        | "FplMapping" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>(LiteralNot, (getName var).[index])
        | "FplOptionalFunctionalTerm" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplOptionalPredicate" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(LiteralAll, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(LiteralEx, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(LiteralExN, (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>(LiteralRet, (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>(LiteralUndetermined, (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariable" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariadicVariableMany" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariadicVariableMany1" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, var)


    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicFunc")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicObj")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariadicVariableMany")>]
    [<DataRow("FplVariadicVariableMany1")>]
    [<TestMethod>]
    member this.TestFplBlockTypeId(var) =
        let index = 3 // TypeId
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplConstructor" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplEquality" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplExtension" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplInstance" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplIntrinsicFunc" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(LiteralInd, (getName var).[index])
        | "FplIntrinsicObj" ->
            Assert.AreEqual<string>(LiteralObj, (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(LiteralTpl, (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])        
        | "FplJustification" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplLemma" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplMapCases" ->
            Assert.AreEqual<string>("", (getName var).[index])        
        | "FplMapping" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplOptionalFunctionalTerm" ->
            Assert.AreEqual<string>(LiteralFunc, (getName var).[index])
        | "FplOptionalPredicate" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>(LiteralUndef, (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>(LiteralPred, (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariable" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariadicVariableMany" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | "FplVariadicVariableMany1" ->
            Assert.AreEqual<string>("", (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, var)

    [<DataRow("FplArgInferenceAssume")>]
    [<DataRow("FplArgInferenceDerived")>]
    [<DataRow("FplArgInferenceRevoke")>]
    [<DataRow("FplArgInferenceTrivial")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplBaseConstructorCall")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplCaseElse")>]
    [<DataRow("FplCaseSingle")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConjecture")>]
    [<DataRow("FplConjunction")>]
    [<DataRow("FplConstructor")>]
    [<DataRow("FplCorollary")>]
    [<DataRow("FplDecrement")>]
    [<DataRow("FplDisjunction")>]
    [<DataRow("FplEquality")>]
    [<DataRow("FplEquivalence")>]
    [<DataRow("FplExclusiveOr")>]
    [<DataRow("FplExtension")>]
    [<DataRow("FplExtensionObj")>]
    [<DataRow("FplForInStmt")>]
    [<DataRow("FplForInStmtDomain")>]
    [<DataRow("FplForInStmtEntity")>]
    [<DataRow("FplFunctionalTerm")>]
    [<DataRow("FplImplication")>]
    [<DataRow("FplInstance")>]
    [<DataRow("FplIntrinsicFunc")>]
    [<DataRow("FplIntrinsicInd")>]
    [<DataRow("FplIntrinsicObj")>]
    [<DataRow("FplIntrinsicPred")>]
    [<DataRow("FplIntrinsicTpl")>]
    [<DataRow("FplIntrinsicUndef")>]
    [<DataRow("FplIsOperator")>]
    [<DataRow("FplJustification")>]
    [<DataRow("FplJustificationItemByAx")>]
    [<DataRow("FplJustificationItemByCor")>]
    [<DataRow("FplJustificationItemByDef")>]
    [<DataRow("FplJustificationItemByDefVar")>]
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
    [<DataRow("FplJustificationItemByRefArgument")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapCaseElse")>]
    [<DataRow("FplMapCaseSingle")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplPredicateList")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantorAll")>]
    [<DataRow("FplQuantorExists")>]
    [<DataRow("FplQuantorExistsN")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariadicVariableMany")>]
    [<DataRow("FplVariadicVariableMany1")>]
    [<TestMethod>]
    member this.TestFplBlockTypeRunOrder(var) =
        let index = 4 // RunOrder
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplArgument" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplAssertion" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplAssignment" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplAxiom" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplBaseConstructorCall" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplCases" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplCaseElse" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplCaseSingle" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplClass" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplConjecture" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplConjunction" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplConstructor" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplCorollary" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplDecrement" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplDisjunction" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplEquality" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplEquivalence" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplExtension" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplExtensionObj" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplForInStmt" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplImplication" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplInstance" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicFunc" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicObj" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplIsOperator" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustification" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByDefVar" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByRefArgument" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplLanguage" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplLemma" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplLocalization" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMapCases" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplMapping" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplNegation" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplOptionalFunctionalTerm" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplOptionalPredicate" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplPredicate" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplPredicateList" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplProof" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplProposition" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplQuantorAll" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplQuantorExists" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplReference" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplReturn" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplRoot" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplTheorem" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplTheory" ->
            Assert.AreEqual<string>("Some", (getName var).[index])
        | "FplTranslation" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplVariable" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplVariadicVariableMany" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | "FplVariadicVariableMany1" ->
            Assert.AreEqual<string>("None", (getName var).[index])
        | _ -> 
            Assert.IsTrue(false, var)

    // todo: issue diagnostics if assuming referenced arguments
    // todo: issue diagnostics restricting assumtions only to non-compound predicates and to references to definitions of predicates
    // todo: issue diagnostics if proving an implication impl(a,b) and the first argument is not the assumption of a, and the last derived argument is not b.
    // todo: issue diagnostics if proving an equivalence iif(a,b) and the proof does not consist of two blocks, each starting with the assumption of a (resp. b) and ending with the derivation of b (resp. a)
    [<DataRow("FplArgInferenceAssume", "00", """proof T$1 {1. |- assume and(x,y) };""", "")>]
    [<DataRow("FplArgInferenceAssume", "01", """proof T$1 {1. |- assume is(x,Nat) };""", "")>]

    // todo: issue diagnostics if the first argument of the proof starts with a derived argument without providing a justification
    // todo: issue diagnostics if the derived predicate is neither one of the prime predicates 'true' and 'false', nor a compound predicate, nor a reference to a definition of a predicate
    [<DataRow("FplArgInferenceDerived", "00", """proof T$1 {1. byax A |- and(x,y)};""", "")>]
    [<DataRow("FplArgInferenceDerived", "01", """proof T$1 {1. |- and(x,y)};""", "")>]

    [<DataRow("FplArgInferenceRevoke", "00", """proof T$1 {1. |- revoke 1};""", "")>]

    [<DataRow("FplArgInferenceTrivial", "00", """;""", "")>]
    
    [<DataRow("FplArgument", "00", """;""", "")>]
    [<DataRow("FplAssertion", "00", """;""", "")>]

    [<DataRow("FplAssignment", "00", """def pred T() {dec ~x:pred x:=false; true};""", "")>]

    [<DataRow("FplAxiom", "00", """ax T {true};""", "")>]

    [<DataRow("FplBaseConstructorCall", "00", """def cl A:obj { ctor A() {dec base.obj(); } };""", "")>]

    [<DataRow("FplCases", "00", """;""", "")>]
    [<DataRow("FplCaseElse", "00", """;""", "")>]
    [<DataRow("FplCaseSingle", "00", """;""", "")>]
    
    [<DataRow("FplClass", "00", """def cl A:obj {intr};""", "")>]
    // base classed not declared
    [<DataRow("FplClass", "00a", """def cl A:B,C {intr};""", "")>] 
    // base classed declared
    [<DataRow("FplClass", "01", """def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {intr};""", "D")>]
    // one constructor
    [<DataRow("FplClass", "02", """def cl A:obj { ctor A() {} };""", "")>]
    // two constructors
    [<DataRow("FplClass", "02a", """def cl A:obj { ctor A() {} ctor A(x,y,z:obj) {} };""", "")>]
    // intrinsic (without constructor), but with properties
    [<DataRow("FplClass", "03", """def cl A:obj { intr prty func MandF()->obj {intr} opt prty func OptF()->obj {intr} prty pred MandP() {true} opt prty pred OptP() {true} };""", "")>]
    // with constructor and properties
    [<DataRow("FplClass", "04", """def cl A:obj { ctor A() {} prty func MandF()->obj {intr} opt prty func OptF()->obj {intr} prty pred MandP() {true} opt prty pred OptP() {true} };""", "")>]
    // with variables constructor and properties
    [<DataRow("FplClass", "05", """def cl A:obj { dec ~x,y:obj; ctor A() {} prty func MandF()->obj {intr} opt prty func OptF()->obj {intr} prty pred MandP() {true} opt prty pred OptP() {true} };""", "")>]
    
    // conjecture
    [<DataRow("FplConjecture", "00", """conj T {true};""", "")>]

    [<DataRow("FplConjunction", "00", """;""", "")>]

    // one constructor
    [<DataRow("FplConstructor", "00", """def cl A:obj { ctor A() {} };""", "")>]
    // two constructors
    [<DataRow("FplConstructor", "01", """def cl A:obj { ctor A() {} ctor A(x,y,z:obj) {} };""", "A(x, y, z)")>]
    // one constructor with variables 
    [<DataRow("FplConstructor", "02", """def cl A:obj { ctor A(x:obj) {dec ~y:obj; } };""", "")>]
    // with with shared variables and stmts
    [<DataRow("FplConstructor", "03", """def cl A:obj { dec ~x,y:obj; ctor A(z:obj) {dec z:=x; } };""", "")>]

    [<DataRow("FplCorollary", "00", """;""", "")>]
    [<DataRow("FplDecrement", "00", """;""", "")>]
    [<DataRow("FplDisjunction", "00", """;""", "")>]
    [<DataRow("FplEquality", "00", """;""", "")>]
    [<DataRow("FplEquivalence", "00", """;""", "")>]
    [<DataRow("FplExclusiveOr", "00", """;""", "")>]
    [<DataRow("FplExtension", "00", """;""", "")>]
    [<DataRow("FplExtensionObj", "00", """;""", "")>]
    [<DataRow("FplForInStmt", "00", """;""", "")>]
    [<DataRow("FplForInStmtDomain", "00", """;""", "")>]
    [<DataRow("FplForInStmtEntity", "00", """;""", "")>]
    
    // intrinsic functional term
    [<DataRow("FplFunctionalTerm", "00", """def func T()->obj {intr};""", "")>]
    // intrinsic functional term with variables
    [<DataRow("FplFunctionalTerm", "01", """def func T(x,y:obj)->obj {intr};""", "")>]
    // intrinsic functional term with variables and properties
    [<DataRow("FplFunctionalTerm", "02", """def func T(x,y:obj)->obj {intr prty func MandF()->obj {intr} opt prty func OptF()->obj {intr} prty pred MandP() {true} opt prty pred OptP() {true} };""", "")>]
    // non-intrinsic functional term with variables and properties
    [<DataRow("FplFunctionalTerm", "03", """def func T(x,y:obj)->obj {dec ~z:obj; return z prty func MandF()->obj {intr} opt prty func OptF()->obj {intr} prty pred MandP() {true} opt prty pred OptP() {true} };""", "")>]
    // non-intrinsic functional term with some statements 
    [<DataRow("FplFunctionalTerm", "04", """def func T(x,y:obj)->obj {dec ~z:obj z:=x y:=z; return z };""", "")>]
    
    [<DataRow("FplImplication", "00", """;""", "")>]
    [<DataRow("FplInstance", "00", """;""", "")>]
    [<DataRow("FplIntrinsicFunc", "00", """;""", "")>]
    [<DataRow("FplIntrinsicInd", "00", """;""", "")>]
    [<DataRow("FplIntrinsicObj", "00", """;""", "")>]
    [<DataRow("FplIntrinsicPred", "00", """;""", "")>]
    [<DataRow("FplIntrinsicTpl", "00", """;""", "")>]
    [<DataRow("FplIntrinsicUndef", "00", """;""", "")>]
    [<DataRow("FplIsOperator", "00", """;""", "")>]

    // no justification    
    [<DataRow("FplJustification", "00", """proof T$1 {1. |- trivial};""", "")>]
    // justification by theorem-like stmt  
    [<DataRow("FplJustification", "01", """proof T$1 {1. A |- trivial};""", "")>]
    // justification byax   
    [<DataRow("FplJustification", "02", """proof T$1 {1. byax A |- trivial};""", "")>]
    // justification bycor   
    [<DataRow("FplJustification", "03", """proof T$1 {1. bycor A$1 |- trivial};""", "")>]
    // justification bydef   
    [<DataRow("FplJustification", "04", """proof T$1 {1. bydef A |- trivial};""", "")>]
    // justificattion by proof argument (other proof)
    [<DataRow("FplJustification", "05", """proof T$1 {1. A$1:2 |- trivial};""", "")>]
    // justificattion byinf
    [<DataRow("FplJustification", "06", """proof T$1 {1. byinf A |- trivial};""", "")>]
    // justificattion by proof argument (this proof)
    [<DataRow("FplJustification", "07", """proof T$1 {1. 2 |- trivial};""", "")>]
    // justification bydef var  
    [<DataRow("FplJustification", "08", """proof T$1 {1. bydef x |- trivial};""", "")>]
    // all kinds
    [<DataRow("FplJustification", "09", """proof T$1 {1.  A, byax B, bycor C$1, bydef D, E$1:2, byinf F, 2, bydef x |- trivial};""", "")>]

    // byax without reference
    [<DataRow("FplJustificationItemByAx", "00", """proof T$1 {1. byax A |- trivial};""", "")>]
    // byax with reference to an axiom
    [<DataRow("FplJustificationItemByAx", "00a", """ax A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    // byax with reference to a non-axiom
    [<DataRow("FplJustificationItemByAx", "00b", """inf A {pre:true con:true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00b", """thm A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00c", """prop A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00d", """lem A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00e", """conj A {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00f", """def pred A() {true} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00g", """def func A()->pred {intr} proof T$1 {1. byax A |- trivial};""", "")>]
    [<DataRow("FplJustificationItemByAx", "00h", """def cl A:obj {intr} proof T$1 {1. byax A |- trivial};""", "")>]

    [<DataRow("FplJustificationItemByCor", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByDef", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByDefVar", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByInf", "00", """inf ExistsByExample {pre:true con:true} proof T$1 { 100. ExistsByExample, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByProofArgument", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByRefArgument", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt", "00", """;""", "")>]


    [<DataRow("FplLanguage", "00", """;""", "")>]

    // lemma 
    [<DataRow("FplLemma", "00", """lem T {true};""", "")>]
    // lemma with two variables
    [<DataRow("FplLemma", "01", """lem T {dec ~x,y:pred; true};""", "")>]

    [<DataRow("FplLocalization", "00", """;""", "")>]

    // intrinsic mandatory functional term with predicate parent
    [<DataRow("FplMandatoryFunctionalTerm", "00",  """def pred T() {intr prty func MandF(x:obj)->obj {intr} };""", "")>]
    // intrinsic mandatory functional term with functional term parent
    [<DataRow("FplMandatoryFunctionalTerm", "01",  """def func T()->obj {intr prty func MandF(x:obj)->obj {intr} };""", "")>]
    // intrinsic mandatory functional term with class parent
    [<DataRow("FplMandatoryFunctionalTerm", "02",  """def cl T:obj {intr prty func MandF(x:obj)->obj {intr} };""", "")>]
    // non-intrinsic mandatory functional term with predicate parent
    [<DataRow("FplMandatoryFunctionalTerm", "03",  """def pred T() {intr prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic mandatory functional term with functional term parent
    [<DataRow("FplMandatoryFunctionalTerm", "04",  """def func T()->obj {intr prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic mandatory functional term with class parent
    [<DataRow("FplMandatoryFunctionalTerm", "05",  """def cl T:obj {intr prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic mandatory functional term with predicate parent, shared variables and one statement
    [<DataRow("FplMandatoryFunctionalTerm", "06",  """def pred T() {dec ~y:obj; true prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]
    // non-intrinsic mandatory functional term with functional term parent, shared variables and one statement
    [<DataRow("FplMandatoryFunctionalTerm", "07",  """def func T()->obj {dec ~y:obj; return y prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]
    // non-intrinsic mandatory functional term with class parent, shared variables and one statement
    [<DataRow("FplMandatoryFunctionalTerm", "08",  """def cl T:obj {dec ~y:obj; ctor T() {} prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]

    // intrinsic optional predicate with predicate parent
    [<DataRow("FplMandatoryPredicate", "00",  """def pred T() {intr prty pred MandF(x:obj) {intr} };""", "")>]
    // intrinsic optional predicate with functional term parent
    [<DataRow("FplMandatoryPredicate", "01",  """def func T()->obj {intr prty pred MandF(x:obj) {intr} };""", "")>]
    // intrinsic optional predicate with class parent
    [<DataRow("FplMandatoryPredicate", "02",  """def cl T:obj {intr prty pred MandF(x:obj) {intr} };""", "")>]
    // non-intrinsic optional predicate with predicate parent
    [<DataRow("FplMandatoryPredicate", "03",  """def pred T() {intr prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic optional predicate with functional term parent
    [<DataRow("FplMandatoryPredicate", "04",  """def func T()->obj {intr prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic optional predicate with class parent
    [<DataRow("FplMandatoryPredicate", "05",  """def cl T:obj {intr prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic optional predicate with predicate parent, shared variables and one statement
    [<DataRow("FplMandatoryPredicate", "06",  """def pred T() {dec ~y:obj; true prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]
    // non-intrinsic optional predicate with functional term parent, shared variables and one statement
    [<DataRow("FplMandatoryPredicate", "07",  """def func T()->obj {dec ~y:obj; return y prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]
    // non-intrinsic optional predicate with class parent, shared variables and one statement
    [<DataRow("FplMandatoryPredicate", "08",  """def cl T:obj {dec ~y:obj; ctor T() {} prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]

    [<DataRow("FplMapCases", "00", """;""", "")>]
    [<DataRow("FplMapCaseElse", "00", """;""", "")>]
    [<DataRow("FplMapCaseSingle", "00", """;""", "")>]
    [<DataRow("FplMapping", "00", """;""", "")>]
    [<DataRow("FplNegation", "00", """;""", "")>]

    // intrinsic optional functional term with predicate parent
    [<DataRow("FplOptionalFunctionalTerm", "00",  """def pred T() {intr opt prty func MandF(x:obj)->obj {intr} };""", "")>]
    // intrinsic optional functional term with functional term parent
    [<DataRow("FplOptionalFunctionalTerm", "01",  """def func T()->obj {intr opt prty func MandF(x:obj)->obj {intr} };""", "")>]
    // intrinsic optional functional term with class parent
    [<DataRow("FplOptionalFunctionalTerm", "02",  """def cl T:obj {intr opt prty func MandF(x:obj)->obj {intr} };""", "")>]
    // non-intrinsic optional functional term with predicate parent
    [<DataRow("FplOptionalFunctionalTerm", "03",  """def pred T() {intr opt prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic optional functional term with functional term parent
    [<DataRow("FplOptionalFunctionalTerm", "04",  """def func T()->obj {intr opt prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic optional functional term with class parent
    [<DataRow("FplOptionalFunctionalTerm", "05",  """def cl T:obj {intr opt prty func MandF(x:obj)->obj {return x} };""", "")>]
    // non-intrinsic optional functional term with predicate parent, shared variables and one statement
    [<DataRow("FplOptionalFunctionalTerm", "06",  """def pred T() {dec ~y:obj; true opt prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]
    // non-intrinsic optional functional term with functional term parent, shared variables and one statement
    [<DataRow("FplOptionalFunctionalTerm", "07",  """def func T()->obj {dec ~y:obj; return y opt prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]
    // non-intrinsic optional functional term with class parent, shared variables and one statement
    [<DataRow("FplOptionalFunctionalTerm", "08",  """def cl T:obj {dec ~y:obj; ctor T() {} opt prty func MandF(x:obj)->obj {dec x:=y; return x} };""", "")>]

    // intrinsic optional predicate with predicate parent
    [<DataRow("FplOptionalPredicate", "00",  """def pred T() {intr opt prty pred MandF(x:obj) {intr} };""", "")>]
    // intrinsic optional predicate with functional term parent
    [<DataRow("FplOptionalPredicate", "01",  """def func T()->obj {intr opt prty pred MandF(x:obj) {intr} };""", "")>]
    // intrinsic optional predicate with class parent
    [<DataRow("FplOptionalPredicate", "02",  """def cl T:obj {intr opt prty pred MandF(x:obj) {intr} };""", "")>]
    // non-intrinsic optional predicate with predicate parent
    [<DataRow("FplOptionalPredicate", "03",  """def pred T() {intr opt prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic optional predicate with functional term parent
    [<DataRow("FplOptionalPredicate", "04",  """def func T()->obj {intr opt prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic optional predicate with class parent
    [<DataRow("FplOptionalPredicate", "05",  """def cl T:obj {intr opt prty pred MandF(x:obj) {true} };""", "")>]
    // non-intrinsic optional predicate with predicate parent, shared variables and one statement
    [<DataRow("FplOptionalPredicate", "06",  """def pred T() {dec ~y:obj; true opt prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]
    // non-intrinsic optional predicate with functional term parent, shared variables and one statement
    [<DataRow("FplOptionalPredicate", "07",  """def func T()->obj {dec ~y:obj; return y opt prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]
    // non-intrinsic optional predicate with class parent, shared variables and one statement
    [<DataRow("FplOptionalPredicate", "08",  """def cl T:obj {dec ~y:obj; ctor T() {} opt prty pred MandF(x:obj) {dec x:=y; true} };""", "")>]

    // intrinsic predicate
    [<DataRow("FplPredicate", "00", """def pred T() {intr};""", "")>]
    // intrinsic predicate with variables
    [<DataRow("FplPredicate", "01", """def pred T(x,y:obj) {intr};""", "")>]
    // intrinsic predicate with variables and properties
    [<DataRow("FplPredicate", "02", """def pred T(x,y:obj) {intr prty func MandF()->obj {intr} opt prty func OptF()->obj {intr} prty pred MandP() {true} opt prty pred OptP() {true} };""", "")>]
    // non-intrinsic predicate with variables and properties
    [<DataRow("FplPredicate", "03", """def pred T(x,y:obj) {dec ~z:obj; true prty func MandF()->obj {intr} opt prty func OptF()->obj {intr} prty pred MandP() {true} opt prty pred OptP() {true} };""", "")>]
    // non-intrinsic predicate with some statements 
    [<DataRow("FplPredicate", "04", """def pred T(x,y:obj) {dec ~z:obj z:=x y:=z; false };""", "")>]

    [<DataRow("FplPredicateList", "00", """inf T {pre: true con: true};""", "")>]
    [<DataRow("FplPredicateList", "01", """inf T {pre: true, true, true con: true};""", "")>]

    // proof 
    [<DataRow("FplProof", "00", """proof T$1 {1. |- trivial};""", "")>]
    // proof with qed
    [<DataRow("FplProof", "00x", """proof T$1 {1. |- trivial qed};""", "")>]
    // proof with axiom
    [<DataRow("FplProof", "00a", """ax T {true} proof T$1 {1. |- trivial};""", "")>]
    // proof with conjecture
    [<DataRow("FplProof", "00b", """conj T {true} proof T$1 {1. |- trivial};""", "")>]
    // proof with theorem
    [<DataRow("FplProof", "01a", """thm T {true} proof T$1 {1. |- trivial};""", "")>]
    // proof with conjecture
    [<DataRow("FplProof", "01b", """lem T {true} proof T$1 {1. |- trivial};""", "")>]
    // proof with conjecture
    [<DataRow("FplProof", "01c", """prop T {true} proof T$1 {1. |- trivial};""", "")>]
    // proof with corollary
    [<DataRow("FplProof", "02", """thm T {true} cor T$1 {true} proof T$1$1 {1. |- trivial};""", "")>]
    // proof with two arguments
    [<DataRow("FplProof", "03", """proof T$1$1 {1. byax A |- trivial 2. |- trivial};""", "")>]

    // proposition
    [<DataRow("FplProposition", "00", """prop T {true};""", "")>]

    [<DataRow("FplQuantorAll", "00", """inf T {pre: true con: all x:pred {and (x,x)}};""", "")>]
    [<DataRow("FplQuantorAll", "01", """inf T {dec ~p: pred(c: obj); pre: p(c) con: all x:obj {p(x)}};""", "")>]

    [<DataRow("FplQuantorExists", "00", """inf T {pre: ex x:tpl {or(p, q(x))} con: true};""", "")>]
    [<DataRow("FplQuantorExists", "01", """inf T {dec ~p: pred ~q:pred(z:tpl); pre: ex x:pred { p(z) } con: true};""", "")>]
    [<DataRow("FplQuantorExists", "02", """inf T {pre: ex x,y:pred { and(x,y) }, true con: true};""", "")>]

    [<DataRow("FplQuantorExistsN", "00", """ax T {exn$1 x:tpl {and(p, q(x))} };""", "")>]
    [<DataRow("FplQuantorExistsN", "01", """conj T {dec ~p: pred ~q:pred(z:tpl); exn$1 x:pred { p(z) } };""", "")>]
    [<DataRow("FplQuantorExistsN", "02", """lem T {exn$1 x:pred { x } };""", "")>]

    [<DataRow("FplReference", "00", """;""", "")>]
    [<DataRow("FplReturn", "00", """;""", "")>]
    [<DataRow("FplRoot", "00", """;""", "")>]

    // rule of inference with one premise
    [<DataRow("FplRuleOfInference", "00", """inf T {pre:true con:false};""", "")>]
    // rule of inference with three premises
    [<DataRow("FplRuleOfInference", "01", """inf T {pre:true, true, true con:false};""", "")>]

    // theorem
    [<DataRow("FplTheorem", "00", """thm T {true};""", "")>]

    [<DataRow("FplTheory", "00", """;""", "")>]
    [<DataRow("FplTranslation", "00", """;""", "")>]
    [<DataRow("FplVariable", "00", """;""", "")>]
    [<DataRow("FplVariable", "00", """;""", "")>]
    [<DataRow("FplVariadicVariableMany", "00", """;""", "")>]
    [<DataRow("FplVariadicVariableMany1", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructure(nodeType, varVal, fplCode, identifier) =
        let rec findNamedItem firstTypeNode identifier (infiniteLoop:HashSet<obj>) (root:FplValue) = 
            if infiniteLoop.Contains(root) then
                None
            else
                infiniteLoop.Add(root) |> ignore
                if identifier = "" then 
                    if root.Name = firstTypeNode then 
                        Some root
                    else
                        match root.Scope.Values |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop) with 
                        | Some found -> Some found
                        | _ -> root.ArgList |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop)
                else
                    let searchItem = root.Type(SignatureType.Name)
                    if root.Name = firstTypeNode && searchItem = identifier then 
                        Some root
                    else
                        match root.Scope.Values |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop) with 
                        | Some found -> Some found
                        | _ -> root.ArgList |> Seq.tryPick (findNamedItem firstTypeNode identifier infiniteLoop)
        ad.Clear()
        let filename = "TestStructure.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let nodeName = (getName nodeType).[0]
            let infiniteLoop = new HashSet<obj>()
            let testNodeOpt = findNamedItem nodeName identifier infiniteLoop st.Root
            match testNodeOpt with 
            | Some (node:FplValue) when node.Parent.IsSome ->
                let parent = node.Parent.Value 
                match nodeType, varVal with

                | "FplArgInferenceAssume", "00" -> 
                    Assert.IsInstanceOfType<FplArgument>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplArgInferenceAssume>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplArgInferenceAssume", "01" -> 
                    Assert.IsInstanceOfType<FplArgument>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplArgInferenceAssume>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)

                | "FplArgInferenceDerived", "00" -> 
                    Assert.IsInstanceOfType<FplArgument>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplArgInferenceDerived>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplArgInferenceDerived", "01" -> 
                    Assert.IsInstanceOfType<FplArgument>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplArgInferenceDerived>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)


                | "FplArgInferenceRevoke", "00" -> 
                    Assert.IsInstanceOfType<FplArgument>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplArgInferenceRevoke>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)

                | "FplAssignment", "00" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplAssignment>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)

                | "FplAxiom", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplAxiom>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)

                | "FplBaseConstructorCall", "00" -> 
                    Assert.IsInstanceOfType<FplConstructor>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // constructor
                    Assert.AreEqual<int>(0, parent.Scope.Count) 
                    Assert.IsInstanceOfType<FplBaseConstructorCall>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // call to base.obj()
                    Assert.AreEqual<int>(0, node.Scope.Count) 

                | "FplClass", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplClass>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplClass", "00a" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplClass>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // base classes are added only if they were previously declared
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplClass", "01" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(4, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplClass>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // three base classes 
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplClass", "02" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplClass>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) 
                    Assert.AreEqual<int>(1, node.Scope.Count) // constructor
                | "FplClass", "02a" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplClass>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) 
                    Assert.AreEqual<int>(2, node.Scope.Count) // 2 constructors
                | "FplClass", "03" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplClass>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) 
                    Assert.AreEqual<int>(4, node.Scope.Count) // 4 properties
                | "FplClass", "04" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplClass>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) 
                    Assert.AreEqual<int>(5, node.Scope.Count) // 1 constructor + 4 properties
                | "FplClass", "05" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplClass>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) 
                    Assert.AreEqual<int>(7, node.Scope.Count) // 2 variables + 1 constructor + 4 properties

                // todo: issue diagnostics if the constructor does nothing
                | "FplConstructor", "00" -> 
                    Assert.IsInstanceOfType<FplClass>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplConstructor>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // nothing in constructor
                    Assert.AreEqual<int>(0, node.Scope.Count) 
                | "FplConstructor", "01" -> 
                    Assert.IsInstanceOfType<FplClass>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base
                    Assert.AreEqual<int>(2, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplConstructor>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // nothing in constructor
                    Assert.AreEqual<int>(3, node.Scope.Count) // three variables
                | "FplConstructor", "02" -> 
                    Assert.IsInstanceOfType<FplClass>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplConstructor>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // nothing in constructor
                    Assert.AreEqual<int>(2, node.Scope.Count) // two variables
                | "FplConstructor", "03" -> 
                    Assert.IsInstanceOfType<FplClass>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base
                    Assert.AreEqual<int>(3, parent.Scope.Count) // two variables
                    Assert.IsInstanceOfType<FplConstructor>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // assignment
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable

                | "FplConjecture", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplConjecture>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplFunctionalTerm", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplFunctionalTerm", "01" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(2, node.Scope.Count) // two variables
                | "FplFunctionalTerm", "02" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(6, node.Scope.Count) // 2 variables, 4 properties
                | "FplFunctionalTerm", "03" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplFunctionalTerm>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return statemenet
                    Assert.AreEqual<int>(7, node.Scope.Count) // 3 variables, 4 properties
                | "FplFunctionalTerm", "04" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplFunctionalTerm>(node)
                    Assert.AreEqual<int>(4, node.ArgList.Count) // non-intrinsic with mapping, 2 statements, and return statement
                    Assert.AreEqual<int>(3, node.Scope.Count) // 3 variables
                
                | "FplJustification", "00" ->
                    Assert.IsInstanceOfType<FplArgument>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count) 
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplJustification>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // no justification 
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplJustification", "01" 
                | "FplJustification", "02" 
                | "FplJustification", "03" 
                | "FplJustification", "04" 
                | "FplJustification", "05" 
                | "FplJustification", "06" 
                | "FplJustification", "07" 
                | "FplJustification", "08" ->
                    Assert.IsInstanceOfType<FplArgument>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count) 
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplJustification>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count)  
                    Assert.AreEqual<int>(1, node.Scope.Count) // contains only a single type of FplJustificationItem
                | "FplJustification", "09" ->
                    Assert.IsInstanceOfType<FplArgument>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count) 
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplJustification>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count)  
                    Assert.AreEqual<int>(8, node.Scope.Count) // contains all possible types of FplJustificationItem
                

                | "FplJustificationItemByAx", "00" ->
                    Assert.IsInstanceOfType<FplJustification>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) 
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplJustificationItemByAx>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count)  
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplJustificationItemByAx", "00a" ->
                    Assert.IsInstanceOfType<FplJustification>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) 
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplJustificationItemByAx>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count)  
                    Assert.AreEqual<int>(1, node.Scope.Count) // referenced axiom
                | "FplJustificationItemByAx", "00b" 
                | "FplJustificationItemByAx", "00c" 
                | "FplJustificationItemByAx", "00d" 
                | "FplJustificationItemByAx", "00e" 
                | "FplJustificationItemByAx", "00f" 
                | "FplJustificationItemByAx", "00g" 
                | "FplJustificationItemByAx", "00h" ->
                    Assert.IsInstanceOfType<FplJustification>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) 
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplJustificationItemByAx>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) 
                    Assert.AreEqual<int>(0, node.Scope.Count) // referenced to a wrong block with matching name
                    
                
                | "FplLemma", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplLemma>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplLemma", "01" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplLemma>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(2, node.Scope.Count)
                | "FplMandatoryFunctionalTerm", "00" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryFunctionalTerm", "01" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryFunctionalTerm", "02" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryFunctionalTerm", "03" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryFunctionalTerm", "04" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryFunctionalTerm", "05" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryFunctionalTerm", "06" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // predicate's value
                    Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryFunctionalTerm", "07" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(2, parent.ArgList.Count) // parent's mapping, return stmt
                    Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryFunctionalTerm", "08" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(3, parent.Scope.Count) // variable, property and constructor
                    Assert.IsInstanceOfType<FplMandatoryFunctionalTerm>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                
                | "FplMandatoryPredicate", "00" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic  
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryPredicate", "01" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryPredicate", "02" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryPredicate", "03" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic  
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryPredicate", "04" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryPredicate", "05" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryPredicate", "06" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // predicate's value
                    Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with assignment 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryPredicate", "07" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(2, parent.ArgList.Count) // parent's mapping, return stmt
                    Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic assignment
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplMandatoryPredicate", "08" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(3, parent.Scope.Count) // variable, property and constructor
                    Assert.IsInstanceOfType<FplMandatoryPredicate>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic withassignment
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable

                | "FplOptionalFunctionalTerm", "00" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalFunctionalTerm", "01" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalFunctionalTerm", "02" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // intrinsic with mapping 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalFunctionalTerm", "03" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalFunctionalTerm", "04" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalFunctionalTerm", "05" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with mapping and return stmt
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalFunctionalTerm", "06" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // predicate's value
                    Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalFunctionalTerm", "07" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(2, parent.ArgList.Count) // parent's mapping, return stmt
                    Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalFunctionalTerm", "08" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(3, parent.Scope.Count) // variable, property and constructor
                    Assert.IsInstanceOfType<FplOptionalFunctionalTerm>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with mapping, assignment, and return stmt
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                
                | "FplOptionalPredicate", "00" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic  
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalPredicate", "01" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalPredicate", "02" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalPredicate", "03" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic  
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalPredicate", "04" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // parent's mapping
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalPredicate", "05" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalPredicate", "06" -> 
                    Assert.IsInstanceOfType<FplPredicate>(parent) // predicate parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // predicate's value
                    Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic with assignment 
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalPredicate", "07" -> 
                    Assert.IsInstanceOfType<FplFunctionalTerm>(parent) // functional term parent
                    Assert.AreEqual<int>(2, parent.ArgList.Count) // parent's mapping, return stmt
                    Assert.AreEqual<int>(2, parent.Scope.Count) // variable and property
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic assignment
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable
                | "FplOptionalPredicate", "08" -> 
                    Assert.IsInstanceOfType<FplClass>(parent) // class parent
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // class's base class
                    Assert.AreEqual<int>(3, parent.Scope.Count) // variable, property and constructor
                    Assert.IsInstanceOfType<FplOptionalPredicate>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // non-intrinsic withassignment
                    Assert.AreEqual<int>(1, node.Scope.Count) // one variable

                | "FplPredicate", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplPredicate", "01" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic
                    Assert.AreEqual<int>(2, node.Scope.Count) // two variables
                | "FplPredicate", "02" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplPredicate>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) // intrinsic
                    Assert.AreEqual<int>(6, node.Scope.Count) // 2 variables, 4 properties
                | "FplPredicate", "03" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplPredicate>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // non-intrinsic
                    Assert.AreEqual<int>(7, node.Scope.Count) // 3 variables, 4 properties
                | "FplPredicate", "04" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplPredicate>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // non-intrinsic with 2 statements
                    Assert.AreEqual<int>(3, node.Scope.Count) // 3 variables

                | "FplPredicateList", "00" -> 
                    Assert.IsInstanceOfType<FplRuleOfInference>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplPredicateList>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // one predicate
                    Assert.AreEqual<int>(0, node.Scope.Count) 
                | "FplPredicateList", "01" -> 
                    Assert.IsInstanceOfType<FplRuleOfInference>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplPredicateList>(node)
                    Assert.AreEqual<int>(3, node.ArgList.Count) // three predicates
                    Assert.AreEqual<int>(0, node.Scope.Count) 

                | "FplProof", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count) // single proof
                    Assert.IsInstanceOfType<FplProof>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) 
                    Assert.AreEqual<int>(1, node.Scope.Count)
                | "FplProof", "00x" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count) // single proof with qed
                    Assert.IsInstanceOfType<FplProof>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count) 
                    Assert.AreEqual<int>(1, node.Scope.Count)
                | "FplProof", "00a" 
                | "FplProof", "00b" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(2, parent.Scope.Count) // proof with wrong parent (axiom or conjecture)
                    Assert.IsInstanceOfType<FplProof>(node) 
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(1, node.Scope.Count)
                | "FplProof", "01a" ->
                    Assert.IsInstanceOfType<FplTheorem>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count) // proof with theorem
                    Assert.IsInstanceOfType<FplProof>(node)  
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(1, node.Scope.Count)
                | "FplProof", "01b" ->
                    Assert.IsInstanceOfType<FplLemma>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count) // proof with lemma
                    Assert.IsInstanceOfType<FplProof>(node)  
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(1, node.Scope.Count)
                | "FplProof", "01c" -> 
                    Assert.IsInstanceOfType<FplProposition>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count) // proof with proposition
                    Assert.IsInstanceOfType<FplProof>(node)  
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(1, node.Scope.Count)
                | "FplProof", "02" -> 
                    Assert.IsInstanceOfType<FplCorollary>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count) // proof with proposition
                    Assert.IsInstanceOfType<FplProof>(node)  
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(1, node.Scope.Count)
                | "FplProof", "03" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count) // proof with proposition
                    Assert.IsInstanceOfType<FplProof>(node)  
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(2, node.Scope.Count)

                | "FplProposition", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplProposition>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)

                | "FplQuantorAll", "00" -> 
                    Assert.IsInstanceOfType<FplRuleOfInference>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplQuantorAll>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // conjunction
                    Assert.AreEqual<int>(1, node.Scope.Count) // x variable
                | "FplQuantorAll", "01" -> 
                    Assert.IsInstanceOfType<FplRuleOfInference>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count) // variable p
                    Assert.IsInstanceOfType<FplQuantorAll>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // reference to p
                    Assert.AreEqual<int>(1, node.Scope.Count) // x variable

                | "FplQuantorExists", "00" -> 
                    Assert.IsInstanceOfType<FplPredicateList>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplQuantorExists>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // disjunction
                    Assert.AreEqual<int>(1, node.Scope.Count) // x variable
                | "FplQuantorExists", "01" -> 
                    Assert.IsInstanceOfType<FplPredicateList>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count) // variable p
                    Assert.IsInstanceOfType<FplQuantorExists>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // reference to p
                    Assert.AreEqual<int>(1, node.Scope.Count) // x variable
                | "FplQuantorExists", "02" -> 
                    Assert.IsInstanceOfType<FplPredicateList>(parent)
                    Assert.AreEqual<int>(2, parent.ArgList.Count) // two predicates in list
                    Assert.AreEqual<int>(0, parent.Scope.Count) 
                    Assert.IsInstanceOfType<FplQuantorExists>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // conjunction
                    Assert.AreEqual<int>(2, node.Scope.Count) // 2 variables

                | "FplQuantorExistsN", "00" -> 
                    Assert.IsInstanceOfType<FplAxiom>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count)
                    Assert.AreEqual<int>(0, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplQuantorExistsN>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // conjunction
                    Assert.AreEqual<int>(1, node.Scope.Count) // 1 variable
                | "FplQuantorExistsN", "01" -> 
                    Assert.IsInstanceOfType<FplConjecture>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count)
                    Assert.AreEqual<int>(2, parent.Scope.Count) // 2 variables
                    Assert.IsInstanceOfType<FplQuantorExistsN>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // reference to p
                    Assert.AreEqual<int>(1, node.Scope.Count) // 1 variable
                | "FplQuantorExistsN", "02" -> 
                    Assert.IsInstanceOfType<FplLemma>(parent)
                    Assert.AreEqual<int>(1, parent.ArgList.Count) // inner predicate
                    Assert.AreEqual<int>(0, parent.Scope.Count) 
                    Assert.IsInstanceOfType<FplQuantorExistsN>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count) // reference to p
                    Assert.AreEqual<int>(1, node.Scope.Count) // z variable

                | "FplRuleOfInference", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplRuleOfInference>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | "FplRuleOfInference", "01" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplRuleOfInference>(node)
                    Assert.AreEqual<int>(2, node.ArgList.Count) // still 2, because we have FplPremiseList object 
                    Assert.AreEqual<int>(0, node.Scope.Count)


                | "FplTheorem", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplTheorem>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)


                | "FplTheory", "00" ->
                    Assert.IsInstanceOfType<FplRoot>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplTheory>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
                | _ -> failwith($"unmatched test {nodeType} {varVal}")
            | Some (node:FplValue) ->
                match nodeType, varVal with
                | "FplRoot", "00" ->
                    Assert.IsInstanceOfType<FplRoot>(node)
                    Assert.AreEqual<int>(0, node.ArgList.Count)
                    Assert.AreEqual<int>(1, node.Scope.Count)
                | _ ->
                    failwith($"Nodetype {nodeType} has unexpectedly no parent.")
            | None ->
                failwith($"Nodetype {nodeType} not found in symbol table. Test is not implemented correctly.")
        | None -> 
            failwith($"FPL code could not be interpreted due to errors {Environment.NewLine}{ad.DiagnosticsToString}")
