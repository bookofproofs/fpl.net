namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FplGrammarCommons
open FplInterpreterTypes

[<TestClass>]
type TestFplBlockType() =
    let positions = (Position("",0,0,0), Position("",0,0,0))
    let parent = new FplRoot()

    [<DataRow("FplArgInference")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConditionResult")>]
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
    [<DataRow("FplJustificationItem")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantor")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplStmt")>]
    [<DataRow("FplTheorem")>]
    [<DataRow("FplTheory")>]
    [<DataRow("FplTranslation")>]
    [<DataRow("FplVariable")>]
    [<DataRow("FplVariadicVariableMany")>]
    [<DataRow("FplVariadicVariableMany1")>]
    [<TestMethod>]
    member this.TestBlockTypeName(var) =
        match var with
        | "FplArgInference" ->
            let x = new FplArgInference(positions, parent)
            Assert.AreEqual<string>("argument inference", x.Name)
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            Assert.AreEqual<string>("argument", x.Name)
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.AreEqual<string>("assertion", x.Name)
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            Assert.AreEqual<string>("assignment statement", x.Name)
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            Assert.AreEqual<string>("axiom", x.Name)
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            Assert.AreEqual<string>("cases statement", x.Name)
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            Assert.AreEqual<string>("class definition", x.Name)
        | "FplConditionResult" ->
            let x = new FplConditionResult(positions, parent)
            Assert.AreEqual<string>("condition result statement", x.Name)
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            Assert.AreEqual<string>("conjecture", x.Name)
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            Assert.AreEqual<string>("conjunction", x.Name)        
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.AreEqual<string>("constructor", x.Name)
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            Assert.AreEqual<string>("corollary", x.Name)
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            Assert.AreEqual<string>("decrement statement", x.Name)        
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            Assert.AreEqual<string>("disjunction", x.Name)        
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            Assert.AreEqual<string>("equality", x.Name)        
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            Assert.AreEqual<string>("equivalence", x.Name)        
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            Assert.AreEqual<string>("exclusive disjunction", x.Name)        
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            Assert.AreEqual<string>("extension definition", x.Name)
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            Assert.AreEqual<string>($"{literalExtL} {literalObjL}", x.Name)
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            Assert.AreEqual<string>("functional term definition", x.Name)
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            Assert.AreEqual<string>("implication", x.Name)        
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            Assert.AreEqual<string>("instance", x.Name)
        | "FplIntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            Assert.AreEqual<string>("intrinsic functional term", x.Name)
        | "FplIntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            Assert.AreEqual<string>("intrinsic index", x.Name)
        | "FplIntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            Assert.AreEqual<string>("intrinsic object", x.Name)
        | "FplIntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            Assert.AreEqual<string>("intrinsic predicate", x.Name)
        | "FplIntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            Assert.AreEqual<string>("intrinsic template", x.Name)
        | "FplIntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            Assert.AreEqual<string>("intrinsic undefined", x.Name)
        | "FplIsOperator" ->
            let x = new FplIsOperator(positions, parent)
            Assert.AreEqual<string>("is operator", x.Name)        
        | "FplJustification" ->
            let x = new FplJustification(positions, parent)
            Assert.AreEqual<string>("justification", x.Name)
        | "FplJustificationItem" ->
            let x = new FplJustificationItem(positions, parent, 0)
            Assert.AreEqual<string>("justification item", x.Name)
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            Assert.AreEqual<string>("language", x.Name)        
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            Assert.AreEqual<string>("lemma", x.Name)
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            Assert.AreEqual<string>("localization", x.Name)
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("functional term property", x.Name)
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.AreEqual<string>("predicate property", x.Name)        
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            Assert.AreEqual<string>($"{literalMapCases} statement", x.Name)        
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            Assert.AreEqual<string>("mapping", x.Name)
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            Assert.AreEqual<string>("negation", x.Name)        
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("optional functional term property", x.Name)
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.AreEqual<string>("optional predicate property", x.Name)
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            Assert.AreEqual<string>("predicate definition", x.Name)
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            Assert.AreEqual<string>("proof", x.Name)
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            Assert.AreEqual<string>("proposition", x.Name)
        | "FplQuantor" ->
            let x = new FplQuantor(positions, parent)
            Assert.AreEqual<string>("quantor", x.Name)
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            Assert.AreEqual<string>("reference", x.Name)
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            Assert.AreEqual<string>($"{literalRetL} statement", x.Name)
        | "FplRoot" ->
            let x = new FplRoot()
            Assert.AreEqual<string>("root", x.Name)
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent)
            Assert.AreEqual<string>("rule of inference", x.Name)
        | "FplStmt" ->
            let x = new FplStmt(positions, parent)
            Assert.AreEqual<string>("statement", x.Name)
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            Assert.AreEqual<string>("theorem", x.Name)
        | "FplTheory" ->
            let x = new FplTheory(positions, parent, "", 0)
            Assert.AreEqual<string>("theory", x.Name)
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            Assert.AreEqual<string>("translation", x.Name)
        | "FplVariable" -> 
            let x = new FplVariable(positions, parent) 
            Assert.AreEqual<string>("variable", x.Name)
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            Assert.AreEqual<string>("zero-or-more variable", x.Name)
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            Assert.AreEqual<string>("one-or-more variable", x.Name)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("FplArgInference")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConditionResult")>]
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
    [<DataRow("FplJustificationItem")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantor")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplStmt")>]
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
        | "FplArgInference" ->
            let x = new FplArgInference(positions, parent)
            Assert.AreEqual<string>("ainf", x.ShortName)
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            Assert.AreEqual<string>("arg", x.ShortName)
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.AreEqual<string>(literalAss, x.ShortName)
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            Assert.AreEqual<string>("stmt", x.ShortName)
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            Assert.AreEqual<string>(literalAx, x.ShortName)
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            Assert.AreEqual<string>("stmt", x.ShortName)
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            Assert.AreEqual<string>("def cl", x.ShortName)
        | "FplConditionResult" ->
            let x = new FplConditionResult(positions, parent)
            Assert.AreEqual<string>("stmt", x.ShortName)
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            Assert.AreEqual<string>(literalConj, x.ShortName)
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            Assert.AreEqual<string>("and", x.ShortName)        
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.AreEqual<string>(literalCtor, x.ShortName)
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            Assert.AreEqual<string>(literalCor, x.ShortName)
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            Assert.AreEqual<string>("decr", x.ShortName)        
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            Assert.AreEqual<string>(literalOr, x.ShortName)        
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            Assert.AreEqual<string>("=", x.ShortName)        
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            Assert.AreEqual<string>("iif", x.ShortName)        
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            Assert.AreEqual<string>(literalXor, x.ShortName)        
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            Assert.AreEqual<string>("def ext", x.ShortName)
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            Assert.AreEqual<string>(literalObj, x.ShortName)
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            Assert.AreEqual<string>("def func", x.ShortName)
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            Assert.AreEqual<string>(literalImpl, x.ShortName)        
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            Assert.AreEqual<string>("inst", x.ShortName)
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
            Assert.AreEqual<string>("just", x.ShortName)
        | "FplJustificationItem" ->
            let x = new FplJustificationItem(positions, parent, 0)
            Assert.AreEqual<string>("just", x.ShortName)
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            Assert.AreEqual<string>("lang", x.ShortName)
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            Assert.AreEqual<string>(literalLem, x.ShortName)
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            Assert.AreEqual<string>(literalLoc, x.ShortName)
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("mfunc", x.ShortName)
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.AreEqual<string>("mpred", x.ShortName)
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            Assert.AreEqual<string>("stmt", x.ShortName)        
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            Assert.AreEqual<string>("map", x.ShortName)
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            Assert.AreEqual<string>(literalNot, x.ShortName)
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("ofunc", x.ShortName)
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.AreEqual<string>("opred", x.ShortName)
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            Assert.AreEqual<string>("def pred", x.ShortName)
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            Assert.AreEqual<string>(literalPrf, x.ShortName)
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            Assert.AreEqual<string>(literalProp, x.ShortName)
        | "FplQuantor" ->
            let x = new FplQuantor(positions, parent)
            Assert.AreEqual<string>("qtr", x.ShortName)
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            Assert.AreEqual<string>("ref", x.ShortName)
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            Assert.AreEqual<string>("stmt", x.ShortName)
        | "FplRoot" ->
            let x = new FplRoot()
            Assert.AreEqual<string>("root", x.ShortName)
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent)
            Assert.AreEqual<string>(literalInf, x.ShortName)
        | "FplStmt" ->
            let x = new FplStmt(positions, parent)
            Assert.AreEqual<string>("stmt", x.ShortName)
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            Assert.AreEqual<string>(literalThm, x.ShortName)
        | "FplTheory" ->
            let x = new FplTheory(positions, parent, "", 0)
            Assert.AreEqual<string>("th", x.ShortName)
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            Assert.AreEqual<string>("trsl", x.ShortName)
        | "FplVariable" ->
            let x = new FplVariable(positions, parent)
            Assert.AreEqual<string>("var", x.ShortName)
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            Assert.AreEqual<string>("*var", x.ShortName)
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            Assert.AreEqual<string>("+var", x.ShortName)
        | _ -> 
            Assert.IsTrue(false, var)

    [<DataRow("FplArgInference")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConditionResult")>]
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
    [<DataRow("FplJustificationItem")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantor")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplStmt")>]
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
        | "FplArgInference" ->
            let x = new FplArgInference(positions, parent)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplArgument" ->
            let x = new FplArgument(positions, parent, 0)
            Assert.AreEqual<string>(literalUndetermined, x.FplId)
        | "FplAssertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.AreEqual<string>(literalAss, x.FplId)
        | "FplAssignment" ->
            let x = new FplAssignment(positions, parent)
            Assert.AreEqual<string>("stmt", x.FplId)
        | "FplAxiom" ->
            let x = new FplAxiom(positions, parent, 0)
            Assert.AreEqual<string>(literalAx, x.FplId)
        | "FplCases" ->
            let x = new FplCases(positions, parent)
            Assert.AreEqual<string>("stmt", x.FplId)
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            Assert.AreEqual<string>("def cl", x.FplId)
        | "FplConditionResult" ->
            let x = new FplConditionResult(positions, parent)
            Assert.AreEqual<string>("stmt", x.FplId)
        | "FplConjecture" ->
            let x = new FplConjecture(positions, parent, 0)
            Assert.AreEqual<string>(literalConj, x.FplId)
        | "FplConjunction" ->
            let x = new FplConjunction(positions, parent)
            Assert.AreEqual<string>("and", x.FplId)        
        | "FplConstructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.AreEqual<string>(literalCtor, x.FplId)
        | "FplCorollary" ->
            let x = new FplCorollary(positions, parent, 0)
            Assert.AreEqual<string>(literalCor, x.FplId)
        | "FplDecrement" ->
            let x = new FplDecrement(positions, parent)
            Assert.AreEqual<string>("decr", x.FplId)        
        | "FplDisjunction" ->
            let x = new FplDisjunction(positions, parent)
            Assert.AreEqual<string>(literalOr, x.FplId)        
        | "FplEquality" ->
            let x = new FplEquality(positions, parent)
            Assert.AreEqual<string>("=", x.FplId)        
        | "FplEquivalence" ->
            let x = new FplEquivalence(positions, parent)
            Assert.AreEqual<string>("iif", x.FplId)        
        | "FplExclusiveOr" ->
            let x = new FplExclusiveOr(positions, parent)
            Assert.AreEqual<string>(literalXor, x.FplId)        
        | "FplExtension" ->
            let x = new FplExtension(positions, parent)
            Assert.AreEqual<string>("def ext", x.FplId)
        | "FplExtensionObj" ->
            let x = new FplExtensionObj(positions, parent)
            Assert.AreEqual<string>(literalObj, x.FplId)
        | "FplFunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent, 0)
            Assert.AreEqual<string>("def func", x.FplId)
        | "FplImplication" ->
            let x = new FplImplication(positions, parent)
            Assert.AreEqual<string>(literalImpl, x.FplId)        
        | "FplInstance" ->
            let x = new FplInstance(positions, parent)
            Assert.AreEqual<string>("inst", x.FplId)
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
            Assert.AreEqual<string>(literalPred, x.FplId)
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
            Assert.AreEqual<string>("just", x.FplId)
        | "FplJustificationItem" ->
            let x = new FplJustificationItem(positions, parent, 0)
            Assert.AreEqual<string>("just", x.FplId)
        | "FplLanguage" ->
            let x = new FplLanguage(positions, parent)
            Assert.AreEqual<string>("lang", x.FplId)
        | "FplLemma" ->
            let x = new FplLemma(positions, parent, 0)
            Assert.AreEqual<string>(literalLem, x.FplId)
        | "FplLocalization" ->
            let x = new FplLocalization(positions, parent)
            Assert.AreEqual<string>(literalLoc, x.FplId)
        | "FplMandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("mfunc", x.FplId)
        | "FplMandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.AreEqual<string>("mpred", x.FplId)
        | "FplMapCases" ->
            let x = new FplMapCases(positions, parent)
            Assert.AreEqual<string>("stmt", x.FplId)        
        | "FplMapping" ->
            let x = new FplMapping(positions, parent)
            Assert.AreEqual<string>("map", x.FplId)
        | "FplNegation" ->
            let x = new FplNegation(positions, parent)
            Assert.AreEqual<string>(literalNot, x.FplId)
        | "FplOptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("ofunc", x.FplId)
        | "FplOptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.AreEqual<string>("opred", x.FplId)
        | "FplPredicate" ->
            let x = new FplPredicate(positions, parent, 0)
            Assert.AreEqual<string>("def pred", x.FplId)
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            Assert.AreEqual<string>(literalPrf, x.FplId)
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            Assert.AreEqual<string>(literalProp, x.FplId)
        | "FplQuantor" ->
            let x = new FplQuantor(positions, parent)
            Assert.AreEqual<string>("qtr", x.FplId)
        | "FplReference" ->
            let x = new FplReference(positions, parent)
            Assert.AreEqual<string>("ref", x.FplId)
        | "FplReturn" ->
            let x = new FplReturn(positions, parent)
            Assert.AreEqual<string>("stmt", x.FplId)
        | "FplRoot" ->
            let x = new FplRoot()
            Assert.AreEqual<string>("root", x.FplId)
        | "FplRuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent)
            Assert.AreEqual<string>(literalInf, x.FplId)
        | "FplStmt" ->
            let x = new FplStmt(positions, parent)
            Assert.AreEqual<string>("stmt", x.FplId)
        | "FplTheorem" ->
            let x = new FplTheorem(positions, parent, 0)
            Assert.AreEqual<string>(literalThm, x.FplId)
        | "FplTheory" ->
            let x = new FplTheory(positions, parent, "", 0)
            Assert.AreEqual<string>("th", x.FplId)
        | "FplTranslation" ->
            let x = new FplTranslation(positions, parent)
            Assert.AreEqual<string>("trsl", x.FplId)
        | "FplVariable" ->
            let x = new FplVariable(positions, parent)
            Assert.AreEqual<string>("var", x.FplId)
        | "FplVariadicVariableMany" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany() |> ignore
            Assert.AreEqual<string>("*var", x.FplId)
        | "FplVariadicVariableMany1" ->
            let x = new FplVariable(positions, parent)
            x.SetToMany1() |> ignore
            Assert.AreEqual<string>("+var", x.FplId)
        | _ -> 
            Assert.IsTrue(false, var)

    [<DataRow("FplArgInference")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConditionResult")>]
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
    [<DataRow("FplJustificationItem")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantor")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplStmt")>]
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
        | "FplArgInference" ->
            let x = new FplArgInference(positions, parent)
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
        | "FplClass" ->
            let x = new FplClass(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplConditionResult" ->
            let x = new FplConditionResult(positions, parent)
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
        | "FplJustificationItem" ->
            let x = new FplJustificationItem(positions, parent, 0)
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
        | "FplProof" ->
            let x = new FplProof(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplProposition" ->
            let x = new FplProposition(positions, parent, 0)
            Assert.IsTrue(x.RunOrder.IsSome)
        | "FplQuantor" ->
            let x = new FplQuantor(positions, parent)
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
            let x = new FplRuleOfInference(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
        | "FplStmt" ->
            let x = new FplStmt(positions, parent)
            Assert.IsTrue(x.RunOrder.IsNone)
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


    [<DataRow("FplArgInference")>]
    [<DataRow("FplArgument")>]
    [<DataRow("FplAssertion")>]
    [<DataRow("FplAssignment")>]
    [<DataRow("FplAxiom")>]
    [<DataRow("FplCases")>]
    [<DataRow("FplClass")>]
    [<DataRow("FplConditionResult")>]
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
    [<DataRow("FplJustificationItem")>]
    [<DataRow("FplLanguage")>]
    [<DataRow("FplLemma")>]
    [<DataRow("FplLocalization")>]
    [<DataRow("FplMandatoryFunctionalTerm")>]
    [<DataRow("FplMandatoryPredicate")>]
    [<DataRow("FplMapCases")>]
    [<DataRow("FplMapping")>]
    [<DataRow("FplNegation")>]
    [<DataRow("FplOptionalFunctionalTerm")>]
    [<DataRow("FplOptionalPredicate")>]
    [<DataRow("FplPredicate")>]
    [<DataRow("FplProof")>]
    [<DataRow("FplProposition")>]
    [<DataRow("FplQuantor")>]
    [<DataRow("FplReference")>]
    [<DataRow("FplReturn")>]
    [<DataRow("FplRoot")>]
    [<DataRow("FplRuleOfInference")>]
    [<DataRow("FplStmt")>]
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
            | "FplArgInference" ->
                let x = new FplArgInference(positions, parent)
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
            | "FplClass" ->
                let x = new FplClass(positions, parent)
                x.Run variableStack 
            | "FplConditionResult" ->
                let x = new FplConditionResult(positions, parent)
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
            | "FplJustificationItem" ->
                let x = new FplJustificationItem(positions, parent, 0)
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
            | "FplProof" ->
                let x = new FplProof(positions, parent, 0)
                x.Run variableStack 
            | "FplProposition" ->
                let x = new FplProposition(positions, parent, 0)
                x.Run variableStack 
            | "FplQuantor" ->
                let x = new FplQuantor(positions, parent)
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
                let x = new FplRuleOfInference(positions, parent)
                x.Run variableStack 
            | "FplStmt" ->
                let x = new FplStmt(positions, parent)
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
