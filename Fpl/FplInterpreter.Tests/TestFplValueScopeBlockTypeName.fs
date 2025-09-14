namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FplPrimitives
open FplInterpreterTypes

[<TestClass>]
type TestFplBlockType() =
    let positions = (Position("",0,0,0), Position("",0,0,0))
    let parent = new FplRoot()

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
            let x = new FplArgInferenceAssume(positions, parent)
            Assert.AreEqual<string>(PrimArgInfAssume, x.Name)
        | "FplArgInferenceDerived" ->
            let x = new FplArgInferenceDerived(positions, parent)
            Assert.AreEqual<string>(PrimArgInfDerive, x.Name)
        | "FplArgInferenceRevoke" ->
            let x = new FplArgInferenceRevoke(positions, parent)
            Assert.AreEqual<string>(PrimArgInfRevoke, x.Name)
        | "FplArgInferenceTrivial" ->
            let x = new FplArgInferenceTrivial(positions, parent)
            Assert.AreEqual<string>(PrimArgInfTrivial, x.Name)
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            Assert.AreEqual<string>(PrimArgL, x.Name)
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.AreEqual<string>(PrimAssertion, x.Name)
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            Assert.AreEqual<string>(PrimAssignment, x.Name)
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            Assert.AreEqual<string>(literalAxL, x.Name)
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            Assert.AreEqual<string>(PrimCases, x.Name)
        | "FplCaseElse" ->
            let x = new FplCaseElse(positions, parent)
            Assert.AreEqual<string>(PrimCaseElse, x.Name)
        | "FplCaseSingle" ->
            let x = new FplCaseSingle(positions, parent)
            Assert.AreEqual<string>(PrimCaseSingle, x.Name)
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            Assert.AreEqual<string>(PrimClassL, x.Name)
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            Assert.AreEqual<string>(literalConjL, x.Name)
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            Assert.AreEqual<string>(PrimConjunction, x.Name)        
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.AreEqual<string>(literalCtorL, x.Name)
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            Assert.AreEqual<string>(literalCorL, x.Name)
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            Assert.AreEqual<string>(PrimDecrementL, x.Name)        
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            Assert.AreEqual<string>(PrimDisjunction, x.Name)        
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            Assert.AreEqual<string>(PrimEqualityL, x.Name)        
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            Assert.AreEqual<string>(PrimEquivalence, x.Name)        
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            Assert.AreEqual<string>(PrimExclusiveOr, x.Name)        
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            Assert.AreEqual<string>(PrimExtensionL, x.Name)
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            Assert.AreEqual<string>(PrimExtensionObj, x.Name)
        | "FplForInStmt" ->
            let x = new FplForInStmt(positions, parent)
            Assert.AreEqual<string>(PrimForInStmt, x.Name)
        | "FplForInStmtDomain" ->
            let x = new FplForInStmtDomain(positions, parent)
            Assert.AreEqual<string>(PrimForInStmtDomain, x.Name)
        | "FplForInStmtEntity" ->
            let x = new FplForInStmtEntity(positions, parent)
            Assert.AreEqual<string>(PrimForInStmtEntity, x.Name)
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            Assert.AreEqual<string>(PrimFuncionalTermL, x.Name)
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            Assert.AreEqual<string>(PrimImplication, x.Name)        
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            Assert.AreEqual<string>(PrimInstanceL, x.Name)
        | "FplIntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            Assert.AreEqual<string>(PrimIntrinsicFunc, x.Name)
        | "FplIntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            Assert.AreEqual<string>(PrimIntrinsicInd, x.Name)
        | "FplIntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            Assert.AreEqual<string>(PrimIntrinsicObj, x.Name)
        | "FplIntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            Assert.AreEqual<string>(PrimIntrinsicPred, x.Name)
        | "FplIntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            Assert.AreEqual<string>(PrimIntrinsicTpl, x.Name)
        | "FplIntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            Assert.AreEqual<string>(PrimIntrinsicUndef, x.Name)
        | "FplIsOperator" ->
            let x = new FplIsOperator(positions, parent)
            Assert.AreEqual<string>(PrimIsOperator, x.Name)        
        | "FplJustification" ->
            let x = new FplJustification(positions, parent)
            Assert.AreEqual<string>(PrimJustificationL, x.Name)
        | "FplJustificationItemByAx" ->
            let x = new FplJustificationItemByAx(positions, parent, 0)
            Assert.AreEqual<string>(PrimJIByAx, x.Name)
        | "FplJustificationItemByCor" ->
            let x = new FplJustificationItemByCor(positions, parent, 0)
            Assert.AreEqual<string>(PrimJIByCor, x.Name)
        | "FplJustificationItemByDef" ->
            let x = new FplJustificationItemByDef(positions, parent, 0)
            Assert.AreEqual<string>(PrimJIByDef, x.Name)
        | "FplJustificationItemByInf" ->
            let x = new FplJustificationItemByInf(positions, parent, 0)
            Assert.AreEqual<string>(PrimJIByInf, x.Name)
        | "FplJustificationItemByProofArgument" ->
            let x = new FplJustificationItemByProofArgument(positions, parent, 0)
            Assert.AreEqual<string>(PrimJIByProofArgument, x.Name)
        | "FplJustificationItemByTheoremLikeStmt" ->
            let x = new FplJustificationItemByTheoremLikeStmt(positions, parent, 0)
            Assert.AreEqual<string>(PrimJIByTheoremLikeStmt, x.Name)
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            Assert.AreEqual<string>(PrimLanguageL, x.Name)        
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            Assert.AreEqual<string>(literalLemL, x.Name)
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            Assert.AreEqual<string>(literalLocL, x.Name)
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.AreEqual<string>(PrimMandatoryFunctionalTermL, x.Name)
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.AreEqual<string>(PrimMandatoryPredicateL, x.Name)        
        | "FplMapCaseElse" ->
            let x = new FplMapCaseElse(positions, parent)
            Assert.AreEqual<string>(PrimMapCaseElse, x.Name)        
        | "FplMapCaseSingle" ->
            let x = new FplMapCaseSingle(positions, parent)
            Assert.AreEqual<string>(PrimMapCaseSingle, x.Name)
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            Assert.AreEqual<string>(PrimMapCases, x.Name)        
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            Assert.AreEqual<string>(PrimMappingL, x.Name)
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            Assert.AreEqual<string>(PrimNegation, x.Name)        
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.AreEqual<string>(PrimOptionalFunctionalTermL, x.Name)
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.AreEqual<string>(PrimOptionalPredicateL, x.Name)
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            Assert.AreEqual<string>(PrimPredicateL, x.Name)
        | "FplPremiseList" ->
            let x = new FplPremiseList(positions, parent, 0)
            Assert.AreEqual<string>(literalPreL, x.Name)
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            Assert.AreEqual<string>(literalPrfL, x.Name)
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            Assert.AreEqual<string>(literalPropL, x.Name)
        | "FplQuantorAll" ->
            let x = new FplQuantorAll(positions, parent)
            Assert.AreEqual<string>(PrimQuantorAll, x.Name)
        | "FplQuantorExists" ->
            let x = new FplQuantorExists(positions, parent)
            Assert.AreEqual<string>(PrimQuantorExists, x.Name)
        | "FplQuantorExistsN" ->
            let x = new FplQuantorExistsN(positions, parent)
            Assert.AreEqual<string>(PrimQuantorExistsN, x.Name)
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            Assert.AreEqual<string>(PrimRefL, x.Name)
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            Assert.AreEqual<string>(PrimReturn, x.Name)
        | "FplRoot" ->
            let x = new FplRoot()
            Assert.AreEqual<string>(PrimRoot, x.Name)
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent, 0)
            Assert.AreEqual<string>(PrimRuleOfInference, x.Name)
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            Assert.AreEqual<string>(literalThmL, x.Name)
        | "FplTheory" ->
            let x = new FplTheory(positions, parent, "", 0)
            Assert.AreEqual<string>(PrimTheoryL, x.Name)
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            Assert.AreEqual<string>(PrimTranslationL, x.Name)
        | "FplVariable" -> 
            let x = new FplVariable(positions, parent) 
            Assert.AreEqual<string>(PrimVariableL, x.Name)
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            Assert.AreEqual<string>(PrimVariableManyL, x.Name)
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            Assert.AreEqual<string>(PrimVariableMany1L, x.Name)
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
