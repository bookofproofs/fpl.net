namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open FplGrammarCommons
open FplInterpreterTypes

[<TestClass>]
type TestFplBlockType() =
    let positions = (Position("",0,0,0), Position("",0,0,0))
    let parent = new FplRoot()

    [<DataRow("Variable")>]
    [<DataRow("VariadicVariableMany")>]
    [<DataRow("VariadicVariableMany1")>]
    [<DataRow("MandatoryPredicate")>]
    [<DataRow("OptionalPredicate")>]
    [<DataRow("MandatoryFunctionalTerm")>]
    [<DataRow("OptionalFunctionalTerm")>]
    [<DataRow("Constructor")>]
    [<DataRow("Class")>]
    [<DataRow("Localization")>]
    [<DataRow("Theorem")>]
    [<DataRow("Lemma")>]
    [<DataRow("Proposition")>]
    [<DataRow("Corollary")>]
    [<DataRow("Proof")>]
    [<DataRow("Conjecture")>]
    [<DataRow("Axiom")>]
    [<DataRow("RuleOfInference")>]
    [<DataRow("Quantor")>]
    [<DataRow("Predicate")>]
    [<DataRow("FunctionalTerm")>]
    [<DataRow("Reference")>]
    [<DataRow("Theory")>]
    [<DataRow("Argument")>]
    [<DataRow("Justification")>]
    [<DataRow("ArgInference")>]
    [<DataRow("Language")>]
    [<DataRow("Translation")>]
    [<DataRow("Mapping")>]
    [<DataRow("Stmt")>]
    [<DataRow("Assertion")>]
    [<DataRow("Extension")>]
    [<DataRow("Instance")>]
    [<DataRow("IntrinsicInd")>]
    [<DataRow("IntrinsicObj")>]
    [<DataRow("IntrinsicPred")>]
    [<DataRow("IntrinsicFunc")>]
    [<DataRow("IntrinsicTpl")>]
    [<DataRow("IntrinsicUndef")>]
    [<DataRow("Root")>]
    [<TestMethod>]
    member this.TestBlockTypeName(var) =
        match var with
        | "Variable" -> 
            let x = new FplVariable(positions, parent) 
            Assert.AreEqual<string>("a variable", x.Name)
        | "VariadicVariableMany" ->
            let x = new FplVariadicVariableMany(positions, parent)
            Assert.AreEqual<string>("a zero-or-more variable", x.Name)
        | "VariadicVariableMany1" ->
            let x = new FplVariadicVariableMany1(positions, parent)
            Assert.AreEqual<string>("a one-or-more variable", x.Name)
        | "MandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("a functional term property", x.Name)
        | "OptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("an optional functional term property", x.Name)
        | "MandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.AreEqual<string>("a predicate property", x.Name)        
        | "OptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.AreEqual<string>("an optional predicate property", x.Name)
        | "Constructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.AreEqual<string>("a constructor", x.Name)
        | "Class" ->
            let x = new FplClass(positions, parent)
            Assert.AreEqual<string>("a class definition", x.Name)
        | "Theorem" ->
            let x = new FplTheorem(positions, parent)
            Assert.AreEqual<string>("a theorem", x.Name)
        | "Lemma" ->
            let x = new FplLemma(positions, parent)
            Assert.AreEqual<string>("a lemma", x.Name)
        | "Proposition" ->
            let x = new FplProposition(positions, parent)
            Assert.AreEqual<string>("a proposition", x.Name)
        | "Corollary" ->
            let x = new FplCorollary(positions, parent)
            Assert.AreEqual<string>("a corollary", x.Name)
        | "Proof" ->
            let x = new FplProof(positions, parent)
            Assert.AreEqual<string>("a proof", x.Name)
        | "Conjecture" ->
            let x = new FplConjecture(positions, parent)
            Assert.AreEqual<string>("a conjecture", x.Name)
        | "Axiom" ->
            let x = new FplAxiom(positions, parent)
            Assert.AreEqual<string>("an axiom", x.Name)
        | "RuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent)
            Assert.AreEqual<string>("a rule of inference", x.Name)
        | "Quantor" ->
            let x = new FplQuantor(positions, parent)
            Assert.AreEqual<string>("a quantor", x.Name)
        | "Predicate" ->
            let x = new FplPredicate(positions, parent)
            Assert.AreEqual<string>("a predicate definition", x.Name)
        | "FunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("a functional term definition", x.Name)
        | "Theory" ->
            let x = new FplTheory(positions, parent, "")
            Assert.AreEqual<string>("a theory", x.Name)
        | "Root" ->
            let x = new FplRoot()
            Assert.AreEqual<string>("a root", x.Name)
        | "ArgInference" ->
            let x = new FplArgInference(positions, parent)
            Assert.AreEqual<string>("an argument inference", x.Name)
        | "Argument" ->
            let x = new FplArgument(positions, parent)
            Assert.AreEqual<string>("an argument", x.Name)
        | "Assertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.AreEqual<string>("an assertion", x.Name)
        | "Extension" ->
            let x = new FplExtension(positions, parent)
            Assert.AreEqual<string>("an extension", x.Name)
        | "Instance" ->
            let x = new FplInstance(positions, parent)
            Assert.AreEqual<string>("an instance", x.Name)
        | "IntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            Assert.AreEqual<string>("an intrinsic functional term", x.Name)
        | "IntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            Assert.AreEqual<string>("an intrinsic index", x.Name)
        | "IntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            Assert.AreEqual<string>("an intrinsic object", x.Name)
        | "IntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            Assert.AreEqual<string>("an intrinsic predicate", x.Name)
        | "IntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            Assert.AreEqual<string>("an intrinsic template", x.Name)
        | "IntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            Assert.AreEqual<string>("an intrinsic undefined", x.Name)
        | "Justification" ->
            let x = new FplJustification(positions, parent)
            Assert.AreEqual<string>("a justification", x.Name)
        | "Localization" ->
            let x = new FplLocalization(positions, parent)
            Assert.AreEqual<string>("a localization", x.Name)
        | "Mapping" ->
            let x = new FplMapping(positions, parent)
            Assert.AreEqual<string>("a mapping", x.Name)
        | "Reference" ->
            let x = new FplReference(positions, parent)
            Assert.AreEqual<string>("a reference", x.Name)
        | "Stmt" ->
            let x = new FplStmt(positions, parent)
            Assert.AreEqual<string>("a statement", x.Name)
        | "Translation" ->
            let x = new FplTranslation(positions, parent)
            Assert.AreEqual<string>("a translation", x.Name)
        | "Language" ->
            let x = new FplLanguage(positions, parent)
            Assert.AreEqual<string>("a language", x.Name)        
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("Variable")>]
    [<DataRow("VariadicVariableMany")>]
    [<DataRow("VariadicVariableMany1")>]
    [<DataRow("MandatoryPredicate")>]
    [<DataRow("OptionalPredicate")>]
    [<DataRow("MandatoryFunctionalTerm")>]
    [<DataRow("OptionalFunctionalTerm")>]
    [<DataRow("Constructor")>]
    [<DataRow("Class")>]
    [<DataRow("Localization")>]
    [<DataRow("Theorem")>]
    [<DataRow("Lemma")>]
    [<DataRow("Proposition")>]
    [<DataRow("Corollary")>]
    [<DataRow("Proof")>]
    [<DataRow("Conjecture")>]
    [<DataRow("Axiom")>]
    [<DataRow("RuleOfInference")>]
    [<DataRow("Quantor")>]
    [<DataRow("Predicate")>]
    [<DataRow("FunctionalTerm")>]
    [<DataRow("Reference")>]
    [<DataRow("Theory")>]
    [<DataRow("Argument")>]
    [<DataRow("Justification")>]
    [<DataRow("ArgInference")>]
    [<DataRow("Language")>]
    [<DataRow("Translation")>]
    [<DataRow("Mapping")>]
    [<DataRow("Stmt")>]
    [<DataRow("Assertion")>]
    [<DataRow("Extension")>]
    [<DataRow("Instance")>]
    [<DataRow("IntrinsicInd")>]
    [<DataRow("IntrinsicObj")>]
    [<DataRow("IntrinsicPred")>]
    [<DataRow("IntrinsicFunc")>]
    [<DataRow("IntrinsicTpl")>]
    [<DataRow("IntrinsicUndef")>]
    [<DataRow("Root")>]
    [<TestMethod>]
    member this.TestFplBlockTypeShortName(var) =
        match var with
        | "Variable" ->
            let x = new FplVariable(positions, parent)
            Assert.AreEqual<string>("var", x.ShortName)
        | "VariadicVariableMany" ->
            let x = new FplVariadicVariableMany(positions, parent)
            Assert.AreEqual<string>("*var", x.ShortName)
        | "VariadicVariableMany1" ->
            let x = new FplVariadicVariableMany1(positions, parent)
            Assert.AreEqual<string>("+var", x.ShortName)
        | "MandatoryPredicate" ->
            let x = new FplMandatoryPredicate(positions, parent)
            Assert.AreEqual<string>("mpred", x.ShortName)
        | "OptionalPredicate" ->
            let x = new FplOptionalPredicate(positions, parent)
            Assert.AreEqual<string>("opred", x.ShortName)
        | "MandatoryFunctionalTerm" ->
            let x = new FplMandatoryFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("mfunc", x.ShortName)
        | "OptionalFunctionalTerm" ->
            let x = new FplOptionalFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("ofunc", x.ShortName)
        | "Constructor" ->
            let x = new FplConstructor(positions, parent)
            Assert.AreEqual<string>(literalCtor, x.ShortName)
        | "Class" ->
            let x = new FplClass(positions, parent)
            Assert.AreEqual<string>("def cl", x.ShortName)
        | "Localization" ->
            let x = new FplLocalization(positions, parent)
            Assert.AreEqual<string>("loc", x.ShortName)
        | "Theorem" ->
            let x = new FplTheorem(positions, parent)
            Assert.AreEqual<string>("thm", x.ShortName)
        | "Lemma" ->
            let x = new FplLemma(positions, parent)
            Assert.AreEqual<string>("lem", x.ShortName)
        | "Proposition" ->
            let x = new FplProposition(positions, parent)
            Assert.AreEqual<string>("prop", x.ShortName)
        | "Corollary" ->
            let x = new FplCorollary(positions, parent)
            Assert.AreEqual<string>(literalCor, x.ShortName)
        | "Proof" ->
            let x = new FplProof(positions, parent)
            Assert.AreEqual<string>("prf", x.ShortName)
        | "Conjecture" ->
            let x = new FplConjecture(positions, parent)
            Assert.AreEqual<string>(literalConj, x.ShortName)
        | "Axiom" ->
            let x = new FplAxiom(positions, parent)
            Assert.AreEqual<string>(literalAx, x.ShortName)
        | "RuleOfInference" ->
            let x = new FplRuleOfInference(positions, parent)
            Assert.AreEqual<string>(literalInf, x.ShortName)
        | "Quantor" ->
            let x = new FplQuantor(positions, parent)
            Assert.AreEqual<string>("qtr", x.ShortName)
        | "Predicate" ->
            let x = new FplPredicate(positions, parent)
            Assert.AreEqual<string>("def pred", x.ShortName)
        | "FunctionalTerm" ->
            let x = new FplFunctionalTerm(positions, parent)
            Assert.AreEqual<string>("def func", x.ShortName)
        | "Reference" ->
            let x = new FplReference(positions, parent)
            Assert.AreEqual<string>("ref", x.ShortName)
        | "Theory" ->
            let x = new FplTheory(positions, parent, "")
            Assert.AreEqual<string>("th", x.ShortName)
        | "Argument" ->
            let x = new FplArgument(positions, parent)
            Assert.AreEqual<string>("arg", x.ShortName)
        | "Justification" ->
            let x = new FplJustification(positions, parent)
            Assert.AreEqual<string>("just", x.ShortName)
        | "ArgInference" ->
            let x = new FplArgInference(positions, parent)
            Assert.AreEqual<string>("ainf", x.ShortName)
        | "Language" ->
            let x = new FplLanguage(positions, parent)
            Assert.AreEqual<string>("lang", x.ShortName)
        | "Translation" ->
            let x = new FplTranslation(positions, parent)
            Assert.AreEqual<string>("trsl", x.ShortName)
        | "Mapping" ->
            let x = new FplMapping(positions, parent)
            Assert.AreEqual<string>("map", x.ShortName)
        | "Stmt" ->
            let x = new FplStmt(positions, parent)
            Assert.AreEqual<string>("stmt", x.ShortName)
        | "Assertion" ->
            let x = new FplAssertion(positions, parent)
            Assert.AreEqual<string>(literalAss, x.ShortName)
        | "Extension" ->
            let x = new FplExtension(positions, parent)
            Assert.AreEqual<string>("def ext", x.ShortName)
        | "Instance" ->
            let x = new FplInstance(positions, parent)
            Assert.AreEqual<string>("inst", x.ShortName)
        | "IntrinsicInd" ->
            let x = new FplIntrinsicInd(positions, parent)
            Assert.AreEqual<string>(literalInd, x.ShortName)
        | "IntrinsicObj" ->
            let x = new FplIntrinsicObj(positions, parent)
            Assert.AreEqual<string>("obj", x.ShortName)
        | "IntrinsicPred" ->
            let x = new FplIntrinsicPred(positions, parent)
            Assert.AreEqual<string>(literalPred, x.ShortName)
        | "IntrinsicFunc" ->
            let x = new FplIntrinsicFunc(positions, parent)
            Assert.AreEqual<string>(literalFunc, x.ShortName)
        | "IntrinsicTpl" ->
            let x = new FplIntrinsicTpl(positions, parent)
            Assert.AreEqual<string>("tpl", x.ShortName)
        | "IntrinsicUndef" ->
            let x = new FplIntrinsicUndef(positions, parent)
            Assert.AreEqual<string>("undef", x.ShortName)
        | "Root" ->
            let x = new FplRoot()
            Assert.AreEqual<string>("root", x.ShortName)
        | _ -> 
            Assert.IsTrue(false, var)

