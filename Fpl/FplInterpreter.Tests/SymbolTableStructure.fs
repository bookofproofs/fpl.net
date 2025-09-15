namespace FplInterpreter.Tests
open System
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
            x.Name
        | "FplArgInferenceDerived" ->
            let x = new FplArgInferenceDerived(positions, parent)
            x.Name
        | "FplArgInferenceRevoke" ->
            let x = new FplArgInferenceRevoke(positions, parent)
            x.Name
        | "FplArgInferenceTrivial" ->
            let x = new FplArgInferenceTrivial(positions, parent)
            x.Name
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            x.Name
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            x.Name
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            x.Name
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            x.Name
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            x.Name
        | "FplCaseElse" ->
            let x = new FplCaseElse(positions, parent)
            x.Name
        | "FplCaseSingle" ->
            let x = new FplCaseSingle(positions, parent)
            x.Name
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            x.Name
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            x.Name
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            x.Name
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            x.Name
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            x.Name
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            x.Name
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            x.Name
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            x.Name
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            x.Name
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            x.Name
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            x.Name
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            x.Name
        | "FplForInStmt" ->
            let x = new FplForInStmt(positions, parent)
            x.Name
        | "FplForInStmtDomain" ->
            let x = new FplForInStmtDomain(positions, parent)
            x.Name
        | "FplForInStmtEntity" ->
            let x = new FplForInStmtEntity(positions, parent)
            x.Name
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            x.Name
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            x.Name
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            x.Name
        | "FplIntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            x.Name
        | "FplIntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            x.Name
        | "FplIntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            x.Name
        | "FplIntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            x.Name
        | "FplIntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            x.Name
        | "FplIntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            x.Name
        | "FplIsOperator" ->
            let x = new FplIsOperator(positions, parent)
            x.Name
        | "FplJustification" ->
            let x = new FplJustification(positions, parent)
            x.Name
        | "FplJustificationItemByAx" ->
            let x = new FplJustificationItemByAx(positions, parent, 0)
            x.Name
        | "FplJustificationItemByCor" ->
            let x = new FplJustificationItemByCor(positions, parent, 0)
            x.Name
        | "FplJustificationItemByDef" ->
            let x = new FplJustificationItemByDef(positions, parent, 0)
            x.Name
        | "FplJustificationItemByInf" ->
            let x = new FplJustificationItemByInf(positions, parent, 0)
            x.Name
        | "FplJustificationItemByProofArgument" ->
            let x = new FplJustificationItemByProofArgument(positions, parent, 0)
            x.Name
        | "FplJustificationItemByTheoremLikeStmt" ->
            let x = new FplJustificationItemByTheoremLikeStmt(positions, parent, 0)
            x.Name
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            x.Name
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            x.Name
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            x.Name
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            x.Name
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            x.Name
        | "FplMapCaseElse" ->
            let x = new FplMapCaseElse(positions, parent)
            x.Name
        | "FplMapCaseSingle" ->
            let x = new FplMapCaseSingle(positions, parent)
            x.Name
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            x.Name
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            x.Name
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            x.Name
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            x.Name
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            x.Name
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            x.Name
        | "FplPremiseList" ->
            let x = new FplPremiseList(positions, parent, 0)
            x.Name
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            x.Name
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            x.Name
        | "FplQuantorAll" ->
            let x = new FplQuantorAll(positions, parent)
            x.Name
        | "FplQuantorExists" ->
            let x = new FplQuantorExists(positions, parent)
            x.Name
        | "FplQuantorExistsN" ->
            let x = new FplQuantorExistsN(positions, parent)
            x.Name
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            x.Name
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            x.Name
        | "FplRoot" ->
            let x = new FplRoot()
            x.Name
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent, 0)
            x.Name
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            x.Name
        | "FplTheory" ->
            let x = new FplTheory(positions, parent, "", 0)
            x.Name
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            x.Name
        | "FplVariable" -> 
            let x = new FplVariable(positions, parent) 
            x.Name
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            x.Name
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            x.Name
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
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
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
    [<DataRow("FplPremiseList")>]
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
        match var with
        | "FplArgInferenceAssume" ->
            Assert.AreEqual<string>(PrimArgInfAssume, getName var)
        | "FplArgInferenceDerived" ->
            Assert.AreEqual<string>(PrimArgInfDerive, getName var)
        | "FplArgInferenceRevoke" ->
            Assert.AreEqual<string>(PrimArgInfRevoke, getName var)
        | "FplArgInferenceTrivial" ->
            Assert.AreEqual<string>(PrimArgInfTrivial, getName var)
        | "FplArgument" ->
            Assert.AreEqual<string>(PrimArgL, getName var)
        | "FplAssertion" ->
            Assert.AreEqual<string>(PrimAssertion, getName var)
        | "FplAssignment" ->
            Assert.AreEqual<string>(PrimAssignment, getName var)
        | "FplAxiom" ->
            Assert.AreEqual<string>(literalAxL, getName var)
        | "FplCases" ->
            Assert.AreEqual<string>(PrimCases, getName var)
        | "FplCaseElse" ->
            Assert.AreEqual<string>(PrimCaseElse, getName var)
        | "FplCaseSingle" ->
            Assert.AreEqual<string>(PrimCaseSingle, getName var)
        | "FplClass" ->
            Assert.AreEqual<string>(PrimClassL, getName var)
        | "FplConjecture" ->
            Assert.AreEqual<string>(literalConjL, getName var)
        | "FplConjunction" ->
            Assert.AreEqual<string>(PrimConjunction, getName var)        
        | "FplConstructor" ->
            Assert.AreEqual<string>(literalCtorL, getName var)
        | "FplCorollary" ->
            Assert.AreEqual<string>(literalCorL, getName var)
        | "FplDecrement" ->
            Assert.AreEqual<string>(PrimDecrementL, getName var)        
        | "FplDisjunction" ->
            Assert.AreEqual<string>(PrimDisjunction, getName var)        
        | "FplEquality" ->
            Assert.AreEqual<string>(PrimEqualityL, getName var)        
        | "FplEquivalence" ->
            Assert.AreEqual<string>(PrimEquivalence, getName var)        
        | "FplExclusiveOr" ->
            Assert.AreEqual<string>(PrimExclusiveOr, getName var)        
        | "FplExtension" ->
            Assert.AreEqual<string>(PrimExtensionL, getName var)
        | "FplExtensionObj" ->
            Assert.AreEqual<string>(PrimExtensionObj, getName var)
        | "FplForInStmt" ->
            Assert.AreEqual<string>(PrimForInStmt, getName var)
        | "FplForInStmtDomain" ->
            Assert.AreEqual<string>(PrimForInStmtDomain, getName var)
        | "FplForInStmtEntity" ->
            Assert.AreEqual<string>(PrimForInStmtEntity, getName var)
        | "FplFunctionalTerm" ->
            Assert.AreEqual<string>(PrimFuncionalTermL, getName var)
        | "FplImplication" ->
            Assert.AreEqual<string>(PrimImplication, getName var)        
        | "FplInstance" ->
            Assert.AreEqual<string>(PrimInstanceL, getName var)
        | "FplIntrinsicFunc" ->
            Assert.AreEqual<string>(PrimIntrinsicFunc, getName var)
        | "FplIntrinsicInd" ->
            Assert.AreEqual<string>(PrimIntrinsicInd, getName var)
        | "FplIntrinsicObj" ->
            Assert.AreEqual<string>(PrimIntrinsicObj, getName var)
        | "FplIntrinsicPred" ->
            Assert.AreEqual<string>(PrimIntrinsicPred, getName var)
        | "FplIntrinsicTpl" ->
            Assert.AreEqual<string>(PrimIntrinsicTpl, getName var)
        | "FplIntrinsicUndef" ->
            Assert.AreEqual<string>(PrimIntrinsicUndef, getName var)
        | "FplIsOperator" ->
            Assert.AreEqual<string>(PrimIsOperator, getName var)        
        | "FplJustification" ->
            Assert.AreEqual<string>(PrimJustificationL, getName var)
        | "FplJustificationItemByAx" ->
            Assert.AreEqual<string>(PrimJIByAx, getName var)
        | "FplJustificationItemByCor" ->
            Assert.AreEqual<string>(PrimJIByCor, getName var)
        | "FplJustificationItemByDef" ->
            Assert.AreEqual<string>(PrimJIByDef, getName var)
        | "FplJustificationItemByInf" ->
            Assert.AreEqual<string>(PrimJIByInf, getName var)
        | "FplJustificationItemByProofArgument" ->
            Assert.AreEqual<string>(PrimJIByProofArgument, getName var)
        | "FplJustificationItemByTheoremLikeStmt" ->
            Assert.AreEqual<string>(PrimJIByTheoremLikeStmt, getName var)
        | "FplLanguage" ->
            Assert.AreEqual<string>(PrimLanguageL, getName var)        
        | "FplLemma" ->
            Assert.AreEqual<string>(literalLemL, getName var)
        | "FplLocalization" ->
            Assert.AreEqual<string>(literalLocL, getName var)
        | "FplMandatoryFunctionalTerm" ->
            Assert.AreEqual<string>(PrimMandatoryFunctionalTermL, getName var)
        | "FplMandatoryPredicate" ->
            Assert.AreEqual<string>(PrimMandatoryPredicateL, getName var)        
        | "FplMapCaseElse" ->
            Assert.AreEqual<string>(PrimMapCaseElse, getName var)        
        | "FplMapCaseSingle" ->
            Assert.AreEqual<string>(PrimMapCaseSingle, getName var)
        | "FplMapCases" ->
            Assert.AreEqual<string>(PrimMapCases, getName var)        
        | "FplMapping" ->
            Assert.AreEqual<string>(PrimMappingL, getName var)
        | "FplNegation" ->
            Assert.AreEqual<string>(PrimNegation, getName var)        
        | "FplOptionalFunctionalTerm" ->
            Assert.AreEqual<string>(PrimOptionalFunctionalTermL, getName var)
        | "FplOptionalPredicate" ->
            Assert.AreEqual<string>(PrimOptionalPredicateL, getName var)
        | "FplPredicate" ->
            Assert.AreEqual<string>(PrimPredicateL, getName var)
        | "FplPremiseList" ->
            Assert.AreEqual<string>(literalPreL, getName var)
        | "FplProof" ->
            Assert.AreEqual<string>(literalPrfL, getName var)
        | "FplProposition" ->
            Assert.AreEqual<string>(literalPropL, getName var)
        | "FplQuantorAll" ->
            Assert.AreEqual<string>(PrimQuantorAll, getName var)
        | "FplQuantorExists" ->
            Assert.AreEqual<string>(PrimQuantorExists, getName var)
        | "FplQuantorExistsN" ->
            Assert.AreEqual<string>(PrimQuantorExistsN, getName var)
        | "FplReference" ->
            Assert.AreEqual<string>(PrimRefL, getName var)
        | "FplReturn" ->
            Assert.AreEqual<string>(PrimReturn, getName var)
        | "FplRoot" ->
            Assert.AreEqual<string>(PrimRoot, getName var)
        | "FplRuleOfInference" ->
            Assert.AreEqual<string>(PrimRuleOfInference, getName var)
        | "FplTheorem" ->
            Assert.AreEqual<string>(literalThmL, getName var)
        | "FplTheory" ->
            Assert.AreEqual<string>(PrimTheoryL, getName var)
        | "FplTranslation" ->
            Assert.AreEqual<string>(PrimTranslationL, getName var)
        | "FplVariable" -> 
            Assert.AreEqual<string>(PrimVariableL, getName var)
        | "FplVariadicVariableMany" ->
            Assert.AreEqual<string>(PrimVariableManyL, getName var)
        | "FplVariadicVariableMany1" ->
            Assert.AreEqual<string>(PrimVariableMany1L, getName var)
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
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
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
    [<DataRow("FplPremiseList")>]
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
        match var with
        | "FplArgInferenceAssume" ->
            let x = new FplArgInferenceAssume(positions, parent)
            Assert.AreEqual<string>(PrimArgInf, x.ShortName)
        | "FplArgInferenceDerived" ->
            let x = new FplArgInferenceDerived(positions, parent)
            Assert.AreEqual<string>(PrimArgInf, x.ShortName)
        | "FplArgInferenceRevoke" ->
            let x = new FplArgInferenceRevoke(positions, parent)
            Assert.AreEqual<string>(PrimArgInf, x.ShortName)
        | "FplArgInferenceTrivial" ->
            let x = new FplArgInferenceTrivial(positions, parent)
            Assert.AreEqual<string>(PrimArgInf, x.ShortName)
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            Assert.AreEqual<string>(PrimArg, x.ShortName)
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.AreEqual<string>(literalAss, x.ShortName)
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            Assert.AreEqual<string>(literalAx, x.ShortName)
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)
        | "FplCaseElse" ->
            let x = new FplCaseElse(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)
        | "FplCaseSingle" ->
            let x = new FplCaseSingle(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            Assert.AreEqual<string>(PrimClass, x.ShortName)
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            Assert.AreEqual<string>(literalConj, x.ShortName)
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            Assert.AreEqual<string>(literalAnd, x.ShortName)        
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.AreEqual<string>(literalCtor, x.ShortName)
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            Assert.AreEqual<string>(literalCor, x.ShortName)
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            Assert.AreEqual<string>(PrimDecrement, x.ShortName)        
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            Assert.AreEqual<string>(literalOr, x.ShortName)        
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            Assert.AreEqual<string>(PrimEquality, x.ShortName)        
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            Assert.AreEqual<string>(literalIif, x.ShortName)        
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            Assert.AreEqual<string>(literalXor, x.ShortName)        
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            Assert.AreEqual<string>(PrimExtension, x.ShortName)
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            Assert.AreEqual<string>(literalObj, x.ShortName)
        | "FplForInStmt" ->
            let x = new FplForInStmt(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)
        | "FplForInStmtDomain" ->
            let x = new FplForInStmtDomain(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)
        | "FplForInStmtEntity" ->
            let x = new FplForInStmtEntity(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            Assert.AreEqual<string>(PrimFuncionalTerm, x.ShortName)
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            Assert.AreEqual<string>(literalImpl, x.ShortName)        
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            Assert.AreEqual<string>(PrimInstance, x.ShortName)
        | "FplIntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            Assert.AreEqual<string>(literalFunc, x.ShortName)
        | "FplIntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            Assert.AreEqual<string>(literalInd, x.ShortName)
        | "FplIntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            Assert.AreEqual<string>(literalObj, x.ShortName)
        | "FplIntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            Assert.AreEqual<string>(literalPred, x.ShortName)
        | "FplIntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            Assert.AreEqual<string>(literalTpl, x.ShortName)
        | "FplIntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            Assert.AreEqual<string>(literalUndef, x.ShortName)
        | "FplIsOperator" ->
            let x = new FplIsOperator(positions, parent)
            Assert.AreEqual<string>(literalIs, x.ShortName)        
        | "FplJustification" ->
            let x = new FplJustification(positions, parent)
            Assert.AreEqual<string>(PrimJustification, x.ShortName)
        | "FplJustificationItemByAx" ->
            let x = new FplJustificationItemByAx(positions, parent, 0)
            Assert.AreEqual<string>(PrimJustification, x.ShortName)
        | "FplJustificationItemByCor" ->
            let x = new FplJustificationItemByCor(positions, parent, 0)
            Assert.AreEqual<string>(PrimJustification, x.ShortName)
        | "FplJustificationItemByDef" ->
            let x = new FplJustificationItemByDef(positions, parent, 0)
            Assert.AreEqual<string>(PrimJustification, x.ShortName)
        | "FplJustificationItemByInf" ->
            let x = new FplJustificationItemByInf(positions, parent, 0)
            Assert.AreEqual<string>(PrimJustification, x.ShortName)
        | "FplJustificationItemByProofArgument" ->
            let x = new FplJustificationItemByProofArgument(positions, parent, 0)
            Assert.AreEqual<string>(PrimJustification, x.ShortName)
        | "FplJustificationItemByTheoremLikeStmt" ->
            let x = new FplJustificationItemByTheoremLikeStmt(positions, parent, 0)
            Assert.AreEqual<string>(PrimJustification, x.ShortName)
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            Assert.AreEqual<string>(PrimLanguage, x.ShortName)
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            Assert.AreEqual<string>(literalLem, x.ShortName)
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            Assert.AreEqual<string>(literalLoc, x.ShortName)
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.AreEqual<string>(PrimMandatoryFunctionalTerm, x.ShortName)
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.AreEqual<string>(PrimMandatoryPredicate, x.ShortName)
        | "FplMapCaseElse" ->
            let x = new FplMapCaseElse(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)        
        | "FplMapCaseSingle" ->
            let x = new FplMapCaseSingle(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)        
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)        
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            Assert.AreEqual<string>(PrimMapping, x.ShortName)
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            Assert.AreEqual<string>(literalNot, x.ShortName)
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.AreEqual<string>(PrimOptionalFunctionalTerm, x.ShortName)
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.AreEqual<string>(PrimOptionalPredicate, x.ShortName)
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            Assert.AreEqual<string>(PrimPredicate, x.ShortName)
        | "FplPremiseList" ->
            let x = new FplPremiseList(positions, parent, 0)
            Assert.AreEqual<string>(literalInf, x.ShortName)
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            Assert.AreEqual<string>(literalPrf, x.ShortName)
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            Assert.AreEqual<string>(literalProp, x.ShortName)
        | "FplQuantorAll" ->
            let x = new FplQuantorAll(positions, parent)
            Assert.AreEqual<string>(PrimQuantor, x.ShortName)
        | "FplQuantorExists" ->
            let x = new FplQuantorExists(positions, parent)
            Assert.AreEqual<string>(PrimQuantor, x.ShortName)
        | "FplQuantorExistsN" ->
            let x = new FplQuantorExistsN(positions, parent)
            Assert.AreEqual<string>(PrimQuantor, x.ShortName)
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            Assert.AreEqual<string>(PrimRef, x.ShortName)
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            Assert.AreEqual<string>(PrimStmt, x.ShortName)
        | "FplRoot" ->
            let x = new FplRoot()
            Assert.AreEqual<string>(PrimRoot, x.ShortName)
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent, 0)
            Assert.AreEqual<string>(literalInf, x.ShortName)
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            Assert.AreEqual<string>(literalThm, x.ShortName)
        | "FplTheory" ->
            let x = new FplTheory(positions, parent, "", 0)
            Assert.AreEqual<string>(PrimTheory, x.ShortName)
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            Assert.AreEqual<string>(PrimTranslation, x.ShortName)
        | "FplVariable" ->
            let x = new FplVariable(positions, parent)
            Assert.AreEqual<string>(PrimVariable, x.ShortName)
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            Assert.AreEqual<string>(PrimVariableMany, x.ShortName)
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            Assert.AreEqual<string>(PrimVariableMany1, x.ShortName)
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
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
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
    [<DataRow("FplPremiseList")>]
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
        match var with
        | "FplArgInferenceAssume" ->
            let x = new FplArgInferenceAssume(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplArgInferenceDerived" ->
            let x = new FplArgInferenceDerived(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplArgInferenceRevoke" ->
            let x = new FplArgInferenceRevoke(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplArgInferenceTrivial" ->
            let x = new FplArgInferenceTrivial(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            Assert.AreEqual<string>("assign (ln 0)", x.FplId)
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplCaseElse" ->
            let x = new FplCaseElse(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplCaseSingle" ->
            let x = new FplCaseSingle(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            Assert.AreEqual<string>(literalObj, x.FplId)
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            Assert.AreEqual<string>("and", x.FplId)        
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.AreEqual<string>(literalObj, x.FplId)
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            Assert.AreEqual<string>("del.", x.FplId)        
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            Assert.AreEqual<string>(literalOr, x.FplId)        
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            Assert.AreEqual<string>("del.", x.FplId)        
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            Assert.AreEqual<string>("iif", x.FplId)        
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            Assert.AreEqual<string>(literalXor, x.FplId)        
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplForInStmt" ->
            let x = new FplForInStmt(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplForInStmtDomain" ->
            let x = new FplForInStmtDomain(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplForInStmtEntity" ->
            let x = new FplForInStmtEntity(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            Assert.AreEqual<string>(literalImpl, x.FplId)        
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            Assert.AreEqual<string>(literalObj, x.FplId)
        | "FplIntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            Assert.AreEqual<string>(literalFunc, x.FplId)
        | "FplIntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            Assert.AreEqual<string>(literalInd, x.FplId)
        | "FplIntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            Assert.AreEqual<string>(literalObj, x.FplId)
        | "FplIntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplIntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            Assert.AreEqual<string>(literalTpl, x.FplId)
        | "FplIntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            Assert.AreEqual<string>(literalUndef, x.FplId)
        | "FplIsOperator" ->
            let x = new FplIsOperator(positions, parent)
            Assert.AreEqual<string>(literalIs, x.FplId)        
        | "FplJustification" ->
            let x = new FplJustification(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplJustificationItemByAx" ->
            let x = new FplJustificationItemByAx(positions, parent, 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplJustificationItemByCor" ->
            let x = new FplJustificationItemByCor(positions, parent, 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplJustificationItemByDef" ->
            let x = new FplJustificationItemByDef(positions, parent, 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplJustificationItemByInf" ->
            let x = new FplJustificationItemByInf(positions, parent, 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplJustificationItemByProofArgument" ->
            let x = new FplJustificationItemByProofArgument(positions, parent, 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplJustificationItemByTheoremLikeStmt" ->
            let x = new FplJustificationItemByTheoremLikeStmt(positions, parent, 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplMapCaseElse" ->
            let x = new FplMapCaseElse(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplMapCaseSingle" ->
            let x = new FplMapCaseSingle(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            Assert.AreEqual<string>("", x.FplId)        
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            Assert.AreEqual<string>(literalNot, x.FplId)
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplPremiseList" ->
            let x = new FplPremiseList(positions, parent, 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplQuantorAll" ->
            let x = new FplQuantorAll(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplQuantorExists" ->
            let x = new FplQuantorExists(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplQuantorExistsN" ->
            let x = new FplQuantorExistsN(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            Assert.AreEqual<string>(literalRet, x.FplId)
        | "FplRoot" ->
            let x = new FplRoot()
            Assert.AreEqual<string>("", x.FplId)
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplTheory" ->
            let x = new FplTheory(positions, parent, "", 0)
            Assert.AreEqual<string>("", x.FplId)
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplVariable" ->
            let x = new FplVariable(positions, parent)
            Assert.AreEqual<string>("", x.FplId)
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            Assert.AreEqual<string>("", x.FplId)
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            Assert.AreEqual<string>("", x.FplId)
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
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
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
    [<DataRow("FplPremiseList")>]
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
        match var with
        | "FplArgInferenceAssume" ->
            let x = new FplArgInferenceAssume(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplArgInferenceDerived" ->
            let x = new FplArgInferenceDerived(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplArgInferenceRevoke" ->
            let x = new FplArgInferenceRevoke(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplArgInferenceTrivial" ->
            let x = new FplArgInferenceTrivial(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplCaseElse" ->
            let x = new FplCaseElse(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplCaseSingle" ->
            let x = new FplCaseSingle(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplForInStmt" ->
            let x = new FplForInStmt(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplForInStmtDomain" ->
            let x = new FplForInStmtDomain(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplForInStmtEntity" ->
            let x = new FplForInStmtEntity(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplIntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplIntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplIntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplIntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplIntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplIntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplIsOperator" ->
            let x = new FplIsOperator(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplJustification" ->
            let x = new FplJustification(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplJustificationItemByAx" ->
            let x = new FplJustificationItemByAx(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplJustificationItemByCor" ->
            let x = new FplJustificationItemByCor(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplJustificationItemByDef" ->
            let x = new FplJustificationItemByDef(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplJustificationItemByInf" ->
            let x = new FplJustificationItemByInf(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplJustificationItemByProofArgument" ->
            let x = new FplJustificationItemByProofArgument(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplJustificationItemByTheoremLikeStmt" ->
            let x = new FplJustificationItemByTheoremLikeStmt(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplMapCaseElse" ->
            let x = new FplMapCaseElse(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplMapCaseSingle" ->
            let x = new FplMapCaseSingle(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplPremiseList" ->
            let x = new FplPremiseList(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplQuantorAll" ->
            let x = new FplQuantorAll(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplQuantorExists" ->
            let x = new FplQuantorExists(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplQuantorExistsN" ->
            let x = new FplQuantorExistsN(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplRoot" ->
            let x = new FplRoot()
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplTheory" ->
            let x = new FplTheory(positions, parent, "", 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplVariable" ->
            let x = new FplVariable(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            Assert.IsTrue(x.RunOrder.IsNone)
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
    [<DataRow("FplJustificationItemByInf")>]
    [<DataRow("FplJustificationItemByProofArgument")>]
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
    [<DataRow("FplPremiseList")>]
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
    member this.TestFplBlockTypeRunIsImplemented(var) =
        try 
            let variableStack = new FplVariableStack()
            match var with
            | "FplArgInferenceAssume" ->
                let x = new FplArgInferenceAssume(positions, parent)
                x.Run variableStack 
            | "FplArgInferenceDerived" ->
                let x = new FplArgInferenceDerived(positions, parent)
                x.Run variableStack 
            | "FplArgInferenceRevoke" ->
                let x = new FplArgInferenceRevoke(positions, parent)
                x.Run variableStack 
            | "FplArgInferenceTrivial" ->
                let x = new FplArgInferenceTrivial(positions, parent)
                x.Run variableStack 
            | "FplArgument" ->
                let x = new FplArgument(positions, parent, 0)
                x.Run variableStack 
            | "FplAssertion" ->
                let x = new FplAssertion(positions, parent)
                x.Run variableStack 
            | "FplAssignment" ->
                let x = new FplAssignment(positions, parent)
                x.Run variableStack 
            | "FplAxiom" ->
                let x = new FplAxiom(positions, parent, 0)
                x.Run variableStack 
            | "FplCases" ->
                let x = new FplCases(positions, parent)
                x.Run variableStack 
            | "FplCaseElse" ->
                let x = new FplCaseElse(positions, parent)
                x.Run variableStack 
            | "FplCaseSingle" ->
                let x = new FplCaseSingle(positions, parent)
                x.Run variableStack 
            | "FplClass" ->
                let x = new FplClass(positions, parent)
                x.Run variableStack 
            | "FplConjecture" ->
                let x = new FplConjecture(positions, parent, 0)
                x.Run variableStack 
            | "FplConjunction" ->
                let x = new FplConjunction(positions, parent)
                x.Run variableStack 
            | "FplConstructor" ->
                let x = new FplConstructor(positions, parent)
                x.Run variableStack 
            | "FplCorollary" ->
                let x = new FplCorollary(positions, parent, 0)
                x.Run variableStack 
            | "FplDecrement" ->
                let x = new FplDecrement(positions, parent)
                x.Run variableStack 
            | "FplDisjunction" ->
                let x = new FplDisjunction(positions, parent)
                x.Run variableStack 
            | "FplEquality" ->
                let x = new FplEquality(positions, parent)
                x.Run variableStack 
            | "FplEquivalence" ->
                let x = new FplEquivalence(positions, parent)
                x.Run variableStack 
            | "FplExclusiveOr" ->
                let x = new FplExclusiveOr(positions, parent)
                x.Run variableStack 
            | "FplExtension" ->
                let x = new FplExtension(positions, parent)
                x.Run variableStack 
            | "FplExtensionObj" ->
                let x = new FplExtensionObj(positions, parent)
                x.Run variableStack 
            | "FplForInStmt" ->
                let x = new FplForInStmt(positions, parent)
                x.Run variableStack 
            | "FplForInStmtDomain" ->
                let x = new FplForInStmtDomain(positions, parent)
                x.Run variableStack 
            | "FplForInStmtEntity" ->
                let x = new FplForInStmtEntity(positions, parent)
                x.Run variableStack 
            | "FplFunctionalTerm" ->
                let x = new FplFunctionalTerm(positions, parent, 0)
                x.Run variableStack 
            | "FplImplication" ->
                let x = new FplImplication(positions, parent)
                x.Run variableStack 
            | "FplInstance" ->
                let x = new FplInstance(positions, parent)
                x.Run variableStack 
            | "FplIntrinsicFunc" ->
                let x = new FplIntrinsicFunc(positions, parent)
                x.Run variableStack 
            | "FplIntrinsicInd" ->
                let x = new FplIntrinsicInd(positions, parent)
                x.Run variableStack 
            | "FplIntrinsicObj" ->
                let x = new FplIntrinsicObj(positions, parent)
                x.Run variableStack 
            | "FplIntrinsicPred" ->
                let x = new FplIntrinsicPred(positions, parent)
                x.Run variableStack 
            | "FplIntrinsicTpl" ->
                let x = new FplIntrinsicTpl(positions, parent)
                x.Run variableStack 
            | "FplIntrinsicUndef" ->
                let x = new FplIntrinsicUndef(positions, parent)
                x.Run variableStack 
            | "FplIsOperator" ->
                let x = new FplIsOperator(positions, parent)
                x.Run variableStack 
            | "FplJustification" ->
                let x = new FplJustification(positions, parent)
                x.Run variableStack 
            | "FplJustificationItemByAx" ->
                let x = new FplJustificationItemByAx(positions, parent, 0)
                x.Run variableStack 
            | "FplJustificationItemByCor" ->
                let x = new FplJustificationItemByCor(positions, parent, 0)
                x.Run variableStack 
            | "FplJustificationItemByDef" ->
                let x = new FplJustificationItemByDef(positions, parent, 0)
                x.Run variableStack 
            | "FplJustificationItemByInf" ->
                let x = new FplJustificationItemByInf(positions, parent, 0)
                x.Run variableStack 
            | "FplJustificationItemByProofArgument" ->
                let x = new FplJustificationItemByProofArgument(positions, parent, 0)
                x.Run variableStack 
            | "FplJustificationItemByTheoremLikeStmt" ->
                let x = new FplJustificationItemByTheoremLikeStmt(positions, parent, 0)
                x.Run variableStack 
            | "FplLanguage" ->
                let x = new FplLanguage(positions, parent)
                x.Run variableStack 
            | "FplLemma" ->
                let x = new FplLemma(positions, parent, 0)
                x.Run variableStack 
            | "FplLocalization" ->
                let x = new FplLocalization(positions, parent)
                x.Run variableStack 
            | "FplMandatoryFunctionalTerm" ->
                let x = new FplMandatoryFunctionalTerm(positions, parent)
                x.Run variableStack 
            | "FplMandatoryPredicate" ->
                let x = new FplMandatoryPredicate(positions, parent)
                x.Run variableStack 
            | "FplMapCaseElse" ->
                let x = new FplMapCaseElse(positions, parent)
                x.Run variableStack 
            | "FplMapCaseSingle" ->
                let x = new FplMapCaseSingle(positions, parent)
                x.Run variableStack 
            | "FplMapCases" ->
                let x = new FplMapCases(positions, parent)
                x.Run variableStack 
            | "FplMapping" ->
                let x = new FplMapping(positions, parent)
                x.Run variableStack 
            | "FplNegation" ->
                let x = new FplNegation(positions, parent)
                x.Run variableStack 
            | "FplOptionalFunctionalTerm" ->
                let x = new FplOptionalFunctionalTerm(positions, parent)
                x.Run variableStack 
            | "FplOptionalPredicate" ->
                let x = new FplOptionalPredicate(positions, parent)
                x.Run variableStack 
            | "FplPredicate" ->
                let x = new FplPredicate(positions, parent, 0)
                x.Run variableStack
            | "FplPremiseList" ->
                let x = new FplPremiseList(positions, parent, 0)
                x.Run variableStack
            | "FplProof" ->
                let x = new FplProof(positions, parent, 0)
                x.Run variableStack 
            | "FplProposition" ->
                let x = new FplProposition(positions, parent, 0)
                x.Run variableStack 
            | "FplQuantorAll" ->
                let x = new FplQuantorAll(positions, parent)
                x.Run variableStack 
            | "FplQuantorExists" ->
                let x = new FplQuantorExists(positions, parent)
                x.Run variableStack 
            | "FplQuantorExistsN" ->
                let x = new FplQuantorExistsN(positions, parent)
                x.Run variableStack 
            | "FplReference" ->
                let x = new FplReference(positions, parent)
                x.Run variableStack 
            | "FplReturn" ->
                let x = new FplReturn(positions, parent)
                x.Run variableStack 
            | "FplRoot" ->
                let x = new FplRoot()
                x.Run variableStack 
            | "FplRuleOfInference" ->
                let x = new FplRuleOfInference(positions, parent, 0)
                x.Run variableStack 
            | "FplTheorem" ->
                let x = new FplTheorem(positions, parent, 0)
                x.Run variableStack 
            | "FplTheory" ->
                let x = new FplTheory(positions, parent, "", 0)
                x.Run variableStack 
            | "FplTranslation" ->
                let x = new FplTranslation(positions, parent)
                x.Run variableStack 
            | "FplVariable" ->
                let x = new FplVariable(positions, parent)
                x.Run variableStack 
            | "FplVariadicVariableMany" ->
                let x = new FplVariable(positions, parent)
                x.SetToMany() |> ignore
                x.Run variableStack 
            | "FplVariadicVariableMany1" ->
                let x = new FplVariable(positions, parent)
                x.SetToMany1() |> ignore
                x.Run variableStack 
            | _ -> 
                Assert.IsTrue(false, var)
        with
        | :? System.NotImplementedException -> Assert.Fail("Run method not implmented")
        | _ -> ()


    [<DataRow("FplArgInferenceAssume", "00", """proof T$1 {1. |- assume 1};""", "")>]
    [<DataRow("FplArgInferenceDerived", "00", """proof T$1 {1. |- and(x,y)};""", "")>]
    [<DataRow("FplArgInferenceRevoke", "00", """proof T$1 {1. |- revoke 1};""", "")>]
    [<DataRow("FplArgInferenceTrivial", "00", """;""", "")>]
    [<DataRow("FplArgument", "00", """;""", "")>]
    [<DataRow("FplAssertion", "00", """;""", "")>]
    [<DataRow("FplAssignment", "00", """def pred T() {dec ~x:pred x:=false; true};""", "")>]
    [<DataRow("FplAxiom", "00", """ax T() {true};""", "")>]
    [<DataRow("FplCases", "00", """;""", "")>]
    [<DataRow("FplCaseElse", "00", """;""", "")>]
    [<DataRow("FplCaseSingle", "00", """;""", "")>]
    [<DataRow("FplClass", "00", """def cl A:obj {intr};""", "")>]
    // base classed not declared
    [<DataRow("FplClass", "00a", """def cl A:B,C {intr};""", "")>] 
    // base classed declared
    [<DataRow("FplClass", "01", """def cl A:obj {intr} def cl B:obj {intr} def cl C:obj {intr} def cl D:A,B,C {intr};""", "D")>]
    // one constructor
    [<DataRow("FplClass", "02", """def cl A:obj { ctor A() {self} };""", "")>]
    // two constructors
    [<DataRow("FplClass", "02a", """def cl A:obj { ctor A() {self} ctor A(x,y,z:obj) {self} };""", "")>]
    // intrinsic (without constructor), but with properties
    [<DataRow("FplClass", "03", """def cl A:obj { intr prty func MandF()->obj {intr} prty func opt OptF()->obj {intr} prty pred MandP() {true} prty pred opt OptP() {true} };""", "")>]
    // with constructor and properties
    [<DataRow("FplClass", "04", """def cl A:obj { ctor A() {self} prty func MandF()->obj {intr} prty func opt OptF()->obj {intr} prty pred MandP() {true} prty pred opt OptP() {true} };""", "")>]
    [<DataRow("FplConjecture", "00", """conj T() {true};""", "")>]
    [<DataRow("FplConjunction", "00", """;""", "")>]
    [<DataRow("FplConstructor", "00", """;""", "")>]
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
    [<DataRow("FplFunctionalTerm", "00", """;""", "")>]
    [<DataRow("FplImplication", "00", """;""", "")>]
    [<DataRow("FplInstance", "00", """;""", "")>]
    [<DataRow("FplIntrinsicFunc", "00", """;""", "")>]
    [<DataRow("FplIntrinsicInd", "00", """;""", "")>]
    [<DataRow("FplIntrinsicObj", "00", """;""", "")>]
    [<DataRow("FplIntrinsicPred", "00", """;""", "")>]
    [<DataRow("FplIntrinsicTpl", "00", """;""", "")>]
    [<DataRow("FplIntrinsicUndef", "00", """;""", "")>]
    [<DataRow("FplIsOperator", "00", """;""", "")>]
    [<DataRow("FplJustification", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByAx", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByCor", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByDef", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByInf", "00", """inf ExistsByExample() {pre:true con:true} T$1 { 100. ExistsByExample, 1 |- false };""", "")>]
    [<DataRow("FplJustificationItemByProofArgument", "00", """;""", "")>]
    [<DataRow("FplJustificationItemByTheoremLikeStmt", "00", """;""", "")>]
    [<DataRow("FplLanguage", "00", """;""", "")>]
    [<DataRow("FplLemma", "00", """lem T() {true};""", "")>]
    [<DataRow("FplLemma", "01", """lem T() {dec ~x,y:pred; true};""", "")>]
    [<DataRow("FplLocalization", "00", """;""", "")>]
    [<DataRow("FplMandatoryFunctionalTerm", "00", """;""", "")>]
    [<DataRow("FplMandatoryPredicate", "00", """;""", "")>]
    [<DataRow("FplMapCases", "00", """;""", "")>]
    [<DataRow("FplMapCaseElse", "00", """;""", "")>]
    [<DataRow("FplMapCaseSingle", "00", """;""", "")>]
    [<DataRow("FplMapping", "00", """;""", "")>]
    [<DataRow("FplNegation", "00", """;""", "")>]
    [<DataRow("FplOptionalFunctionalTerm", "00", """;""", "")>]
    [<DataRow("FplOptionalPredicate", "00", """;""", "")>]
    [<DataRow("FplPredicate", "00", """;""", "")>]
    [<DataRow("FplPremiseList", "00", """;""", "")>]
    [<DataRow("FplProof", "00", """;""", "")>]
    [<DataRow("FplProposition", "00", """prop T() {true};""", "")>]
    [<DataRow("FplQuantorAll", "00", """;""", "")>]
    [<DataRow("FplQuantorExists", "00", """;""", "")>]
    [<DataRow("FplQuantorExistsN", "00", """;""", "")>]
    [<DataRow("FplReference", "00", """;""", "")>]
    [<DataRow("FplReturn", "00", """;""", "")>]
    [<DataRow("FplRoot", "00", """;""", "")>]
    [<DataRow("FplRuleOfInference", "00", """;""", "")>]
    [<DataRow("FplTheorem", "00", """thm T() {true};""", "")>]
    [<DataRow("FplTheory", "00", """;""", "")>]
    [<DataRow("FplTranslation", "00", """;""", "")>]
    [<DataRow("FplVariable", "00", """;""", "")>]
    [<DataRow("FplVariable", "00", """;""", "")>]
    [<DataRow("FplVariadicVariableMany", "00", """;""", "")>]
    [<DataRow("FplVariadicVariableMany1", "00", """;""", "")>]
    [<TestMethod>]
    member this.TestStructure(nodeType, varVal, fplCode, identifier) =
        let rec findNamedItem firstTypeNode identifier (root:FplValue) = 
            if identifier = "" then 
                if root.Name = firstTypeNode then 
                    Some root
                else
                    match root.Scope.Values |> Seq.tryPick (findNamedItem firstTypeNode identifier) with 
                    | Some found -> Some found
                    | _ -> root.ArgList |> Seq.tryPick (findNamedItem firstTypeNode identifier)
            else
                if root.Name = firstTypeNode && root.FplId = identifier then 
                    Some root
                else
                    match root.Scope.Values |> Seq.tryPick (findNamedItem firstTypeNode identifier) with 
                    | Some found -> Some found
                    | _ -> root.ArgList |> Seq.tryPick (findNamedItem firstTypeNode identifier)
        ad.Clear()
        let filename = "TestStructure.fpl"
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        prepareFplCode(filename, "", false) |> ignore
        match stOption with
        | Some st -> 
            let nodeName = (getName nodeType)
            let testNodeOpt = findNamedItem nodeName identifier st.Root
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
                | "FplArgInferenceDerived", "00" -> 
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
                | "FplConjecture", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplConjecture>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
                    Assert.AreEqual<int>(0, node.Scope.Count)
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
                | "FplProposition", "00" -> 
                    Assert.IsInstanceOfType<FplTheory>(parent)
                    Assert.AreEqual<int>(0, parent.ArgList.Count)
                    Assert.AreEqual<int>(1, parent.Scope.Count)
                    Assert.IsInstanceOfType<FplProposition>(node)
                    Assert.AreEqual<int>(1, node.ArgList.Count)
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
