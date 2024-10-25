namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplBlockType() =

    [<DataRow("Variable")>]
    [<DataRow("VariadicVariableMany")>]
    [<DataRow("VariadicVariableMany1")>]
    [<DataRow("MandatoryFunctionalTerm")>]
    [<DataRow("OptionalFunctionalTerm")>]
    [<DataRow("MandatoryPredicate")>]
    [<DataRow("OptionalPredicate")>]
    [<DataRow("Constructor")>]
    [<DataRow("Class")>]
    [<DataRow("Theorem")>]
    [<DataRow("Lemma")>]
    [<DataRow("Proposition")>]
    [<DataRow("Corollary")>]
    [<DataRow("Proof")>]
    [<DataRow("Conjecture")>]
    [<DataRow("Axiom")>]
    [<DataRow("RuleOfInference")>]
    [<DataRow("Quantor")>]
    [<DataRow("Conclusion")>]
    [<DataRow("Predicate")>]
    [<DataRow("FunctionalTerm")>]
    [<DataRow("Theory")>]
    [<DataRow("Root")>]
    [<TestMethod>]
    member this.TestBlockTypeName(var) =
        match var with
        | "Variable" -> Assert.AreEqual<string>("a variable",FplValueType.Variable.Name)
        | "VariadicVariableMany" -> Assert.AreEqual<string>("a zero-or-more variable",FplValueType.VariadicVariableMany.Name)
        | "VariadicVariableMany1" -> Assert.AreEqual<string>("a one-or-more variable",FplValueType.VariadicVariableMany1.Name)
        | "MandatoryFunctionalTerm" -> Assert.AreEqual<string>("a functional term property",FplValueType.MandatoryFunctionalTerm.Name)
        | "OptionalFunctionalTerm" -> Assert.AreEqual<string>("an optional functional term property",FplValueType.OptionalFunctionalTerm.Name)
        | "MandatoryPredicate" -> Assert.AreEqual<string>("a predicate property",FplValueType.MandatoryPredicate.Name)
        | "OptionalPredicate" -> Assert.AreEqual<string>("an optional predicate property",FplValueType.OptionalPredicate.Name)
        | "Constructor" -> Assert.AreEqual<string>("a constructor",FplValueType.Constructor.Name)
        | "Class" -> Assert.AreEqual<string>("a class definition",FplValueType.Class.Name)
        | "Theorem" -> Assert.AreEqual<string>("a theorem",FplValueType.Theorem.Name)
        | "Lemma" -> Assert.AreEqual<string>("a lemma",FplValueType.Lemma.Name)
        | "Proposition" -> Assert.AreEqual<string>("a proposition",FplValueType.Proposition.Name)
        | "Corollary" -> Assert.AreEqual<string>("a corollary",FplValueType.Corollary.Name)
        | "Proof" -> Assert.AreEqual<string>("a proof",FplValueType.Proof.Name)
        | "Conjecture" -> Assert.AreEqual<string>("a conjecture",FplValueType.Conjecture.Name)
        | "Axiom" -> Assert.AreEqual<string>("an axiom",FplValueType.Axiom.Name)
        | "RuleOfInference" -> Assert.AreEqual<string>("a rule of inference",FplValueType.RuleOfInference.Name)
        | "Quantor" -> Assert.AreEqual<string>("a quantor",FplValueType.Quantor.Name)
        | "Conclusion" -> Assert.AreEqual<string>("a conclusion",FplValueType.Conclusion.Name)
        | "Predicate" -> Assert.AreEqual<string>("a predicate definition",FplValueType.Predicate.Name)
        | "FunctionalTerm" -> Assert.AreEqual<string>("a functional term definition",FplValueType.FunctionalTerm.Name)
        | "Theory" -> Assert.AreEqual<string>("a theory",FplValueType.Theory.Name)
        | "Root" -> Assert.AreEqual<string>("a root",FplValueType.Root.Name)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("Variable")>]
    [<DataRow("VariadicVariableMany")>]
    [<DataRow("VariadicVariableMany1")>]
    [<DataRow("MandatoryFunctionalTerm")>]
    [<DataRow("OptionalFunctionalTerm")>]
    [<DataRow("MandatoryPredicate")>]
    [<DataRow("OptionalPredicate")>]
    [<DataRow("Constructor")>]
    [<DataRow("Class")>]
    [<DataRow("Theorem")>]
    [<DataRow("Lemma")>]
    [<DataRow("Proposition")>]
    [<DataRow("Corollary")>]
    [<DataRow("Proof")>]
    [<DataRow("Conjecture")>]
    [<DataRow("Axiom")>]
    [<DataRow("RuleOfInference")>]
    [<DataRow("Quantor")>]
    [<DataRow("Conclusion")>]
    [<DataRow("Predicate")>]
    [<DataRow("FunctionalTerm")>]
    [<DataRow("Theory")>]
    [<DataRow("Root")>]
    [<TestMethod>]
    member this.TestBlockTypeShortName(var) =
        match var with
        | "Variable" -> Assert.AreEqual<string>("var",FplValueType.Variable.ShortName)
        | "VariadicVariableMany" -> Assert.AreEqual<string>("*var",FplValueType.VariadicVariableMany.ShortName)
        | "VariadicVariableMany1" -> Assert.AreEqual<string>("+var",FplValueType.VariadicVariableMany1.ShortName)
        | "MandatoryFunctionalTerm" -> Assert.AreEqual<string>("prop",FplValueType.MandatoryFunctionalTerm.ShortName)
        | "OptionalFunctionalTerm" -> Assert.AreEqual<string>("prop",FplValueType.OptionalFunctionalTerm.ShortName)
        | "MandatoryPredicate" -> Assert.AreEqual<string>("prop",FplValueType.MandatoryPredicate.ShortName)
        | "OptionalPredicate" -> Assert.AreEqual<string>("prop",FplValueType.OptionalPredicate.ShortName)
        | "Constructor" -> Assert.AreEqual<string>("ctor",FplValueType.Constructor.ShortName)
        | "Class" -> Assert.AreEqual<string>("cl",FplValueType.Class.ShortName)
        | "Theorem" -> Assert.AreEqual<string>("thm",FplValueType.Theorem.ShortName)
        | "Lemma" -> Assert.AreEqual<string>("lem",FplValueType.Lemma.ShortName)
        | "Proposition" -> Assert.AreEqual<string>("prop",FplValueType.Proposition.ShortName)
        | "Corollary" -> Assert.AreEqual<string>("cor",FplValueType.Corollary.ShortName)
        | "Proof" -> Assert.AreEqual<string>("prf",FplValueType.Proof.ShortName)
        | "Conjecture" -> Assert.AreEqual<string>("conj",FplValueType.Conjecture.ShortName)
        | "Axiom" -> Assert.AreEqual<string>("ax",FplValueType.Axiom.ShortName)
        | "RuleOfInference" -> Assert.AreEqual<string>("inf",FplValueType.RuleOfInference.ShortName)
        | "Quantor" -> Assert.AreEqual<string>("qtr",FplValueType.Quantor.ShortName)
        | "Conclusion" -> Assert.AreEqual<string>("con",FplValueType.Conclusion.ShortName)
        | "Predicate" -> Assert.AreEqual<string>("pred",FplValueType.Predicate.ShortName)
        | "FunctionalTerm" -> Assert.AreEqual<string>("func",FplValueType.FunctionalTerm.ShortName)
        | "Theory" -> Assert.AreEqual<string>("th",FplValueType.Theory.ShortName)
        | "Root" -> Assert.AreEqual<string>("root",FplValueType.Root.ShortName)
        | _ -> 
            Assert.IsTrue(false)

