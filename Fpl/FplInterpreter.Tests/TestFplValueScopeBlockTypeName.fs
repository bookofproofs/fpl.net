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
    [<DataRow("Predicate")>]
    [<DataRow("FunctionalTerm")>]
    [<DataRow("Theory")>]
    [<DataRow("Root")>]
    [<TestMethod>]
    member this.TestBlockTypeName(var) =
        match var with
        | "Variable" -> Assert.AreEqual<string>("a variable",FplBlockType.Variable.Name)
        | "VariadicVariableMany" -> Assert.AreEqual<string>("a zero-or-more variable",FplBlockType.VariadicVariableMany.Name)
        | "VariadicVariableMany1" -> Assert.AreEqual<string>("a one-or-more variable",FplBlockType.VariadicVariableMany1.Name)
        | "MandatoryFunctionalTerm" -> Assert.AreEqual<string>("a functional term property",FplBlockType.MandatoryFunctionalTerm.Name)
        | "OptionalFunctionalTerm" -> Assert.AreEqual<string>("an optional functional term property",FplBlockType.OptionalFunctionalTerm.Name)
        | "MandatoryPredicate" -> Assert.AreEqual<string>("a predicate property",FplBlockType.MandatoryPredicate.Name)
        | "OptionalPredicate" -> Assert.AreEqual<string>("an optional predicate property",FplBlockType.OptionalPredicate.Name)
        | "Constructor" -> Assert.AreEqual<string>("a constructor",FplBlockType.Constructor.Name)
        | "Class" -> Assert.AreEqual<string>("a class definition",FplBlockType.Class.Name)
        | "Theorem" -> Assert.AreEqual<string>("a theorem",FplBlockType.Theorem.Name)
        | "Lemma" -> Assert.AreEqual<string>("a lemma",FplBlockType.Lemma.Name)
        | "Proposition" -> Assert.AreEqual<string>("a proposition",FplBlockType.Proposition.Name)
        | "Corollary" -> Assert.AreEqual<string>("a corollary",FplBlockType.Corollary.Name)
        | "Proof" -> Assert.AreEqual<string>("a proof",FplBlockType.Proof.Name)
        | "Conjecture" -> Assert.AreEqual<string>("a conjecture",FplBlockType.Conjecture.Name)
        | "Axiom" -> Assert.AreEqual<string>("an axiom",FplBlockType.Axiom.Name)
        | "RuleOfInference" -> Assert.AreEqual<string>("a rule of inference",FplBlockType.RuleOfInference.Name)
        | "Quantor" -> Assert.AreEqual<string>("a quantor",FplBlockType.Quantor.Name)
        | "Predicate" -> Assert.AreEqual<string>("a predicate definition",FplBlockType.Predicate.Name)
        | "FunctionalTerm" -> Assert.AreEqual<string>("a functional term definition",FplBlockType.FunctionalTerm.Name)
        | "Theory" -> Assert.AreEqual<string>("a theory",FplBlockType.Theory.Name)
        | "Root" -> Assert.AreEqual<string>("a root",FplBlockType.Root.Name)
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
    [<DataRow("Predicate")>]
    [<DataRow("FunctionalTerm")>]
    [<DataRow("Theory")>]
    [<DataRow("Root")>]
    [<TestMethod>]
    member this.TestBlockTypeShortName(var) =
        match var with
        | "Variable" -> Assert.AreEqual<string>("var",FplBlockType.Variable.ShortName)
        | "VariadicVariableMany" -> Assert.AreEqual<string>("*var",FplBlockType.VariadicVariableMany.ShortName)
        | "VariadicVariableMany1" -> Assert.AreEqual<string>("+var",FplBlockType.VariadicVariableMany1.ShortName)
        | "MandatoryFunctionalTerm" -> Assert.AreEqual<string>("mfunc",FplBlockType.MandatoryFunctionalTerm.ShortName)
        | "OptionalFunctionalTerm" -> Assert.AreEqual<string>("ofunc",FplBlockType.OptionalFunctionalTerm.ShortName)
        | "MandatoryPredicate" -> Assert.AreEqual<string>("mpred",FplBlockType.MandatoryPredicate.ShortName)
        | "OptionalPredicate" -> Assert.AreEqual<string>("opred",FplBlockType.OptionalPredicate.ShortName)
        | "Constructor" -> Assert.AreEqual<string>("ctor",FplBlockType.Constructor.ShortName)
        | "Class" -> Assert.AreEqual<string>("cl",FplBlockType.Class.ShortName)
        | "Theorem" -> Assert.AreEqual<string>("thm",FplBlockType.Theorem.ShortName)
        | "Lemma" -> Assert.AreEqual<string>("lem",FplBlockType.Lemma.ShortName)
        | "Proposition" -> Assert.AreEqual<string>("prop",FplBlockType.Proposition.ShortName)
        | "Corollary" -> Assert.AreEqual<string>("cor",FplBlockType.Corollary.ShortName)
        | "Proof" -> Assert.AreEqual<string>("prf",FplBlockType.Proof.ShortName)
        | "Conjecture" -> Assert.AreEqual<string>("conj",FplBlockType.Conjecture.ShortName)
        | "Axiom" -> Assert.AreEqual<string>("ax",FplBlockType.Axiom.ShortName)
        | "RuleOfInference" -> Assert.AreEqual<string>("inf",FplBlockType.RuleOfInference.ShortName)
        | "Quantor" -> Assert.AreEqual<string>("qtr",FplBlockType.Quantor.ShortName)
        | "Predicate" -> Assert.AreEqual<string>("pred",FplBlockType.Predicate.ShortName)
        | "FunctionalTerm" -> Assert.AreEqual<string>("func",FplBlockType.FunctionalTerm.ShortName)
        | "Theory" -> Assert.AreEqual<string>("th",FplBlockType.Theory.ShortName)
        | "Root" -> Assert.AreEqual<string>("root",FplBlockType.Root.ShortName)
        | _ -> 
            Assert.IsTrue(false)

