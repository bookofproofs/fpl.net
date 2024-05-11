namespace FplInterpreter.Tests
open Microsoft.VisualStudio.TestTools.UnitTesting
open FplInterpreterTypes

[<TestClass>]
type TestFplBlockType() =

    [<DataRow("Variable")>]
    [<DataRow("VariadicVariableMany")>]
    [<DataRow("VariadicVariableMany1")>]
    [<DataRow("Expression")>]
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
    [<DataRow("Premise")>]
    [<DataRow("Conclusion")>]
    [<DataRow("Predicate")>]
    [<DataRow("FunctionalTerm")>]
    [<DataRow("Theory")>]
    [<DataRow("Root")>]
    [<TestMethod>]
    member this.TestBlockTypeName(var) =
        match var with
        | "Variable" -> Assert.AreEqual("a variable",FplValueType.Variable.Name)
        | "VariadicVariableMany" -> Assert.AreEqual("a zero-or-more variable",FplValueType.VariadicVariableMany.Name)
        | "VariadicVariableMany1" -> Assert.AreEqual("a one-or-more variable",FplValueType.VariadicVariableMany1.Name)
        | "Expression" -> Assert.AreEqual("an expression",FplValueType.Expression.Name)
        | "MandatoryFunctionalTerm" -> Assert.AreEqual("a functional term property",FplValueType.MandatoryFunctionalTerm.Name)
        | "OptionalFunctionalTerm" -> Assert.AreEqual("an optional functional term property",FplValueType.OptionalFunctionalTerm.Name)
        | "MandatoryPredicate" -> Assert.AreEqual("a predicate property",FplValueType.MandatoryPredicate.Name)
        | "OptionalPredicate" -> Assert.AreEqual("an optional predicate property",FplValueType.OptionalPredicate.Name)
        | "Constructor" -> Assert.AreEqual("a constructor",FplValueType.Constructor.Name)
        | "Class" -> Assert.AreEqual("a class definition",FplValueType.Class.Name)
        | "Theorem" -> Assert.AreEqual("a theorem",FplValueType.Theorem.Name)
        | "Lemma" -> Assert.AreEqual("a lemma",FplValueType.Lemma.Name)
        | "Proposition" -> Assert.AreEqual("a proposition",FplValueType.Proposition.Name)
        | "Corollary" -> Assert.AreEqual("a corollary",FplValueType.Corollary.Name)
        | "Proof" -> Assert.AreEqual("a proof",FplValueType.Proof.Name)
        | "Conjecture" -> Assert.AreEqual("a conjecture",FplValueType.Conjecture.Name)
        | "Axiom" -> Assert.AreEqual("an axiom",FplValueType.Axiom.Name)
        | "RuleOfInference" -> Assert.AreEqual("a rule of inference",FplValueType.RuleOfInference.Name)
        | "Premise" -> Assert.AreEqual("a premise",FplValueType.Premise.Name)
        | "Conclusion" -> Assert.AreEqual("a conclusion",FplValueType.Conclusion.Name)
        | "Predicate" -> Assert.AreEqual("a predicate definition",FplValueType.Predicate.Name)
        | "FunctionalTerm" -> Assert.AreEqual("a functional term definition",FplValueType.FunctionalTerm.Name)
        | "Theory" -> Assert.AreEqual("a theory",FplValueType.Theory.Name)
        | "Root" -> Assert.AreEqual("a root",FplValueType.Root.Name)
        | _ -> 
            Assert.IsTrue(false)

    [<DataRow("Variable")>]
    [<DataRow("VariadicVariableMany")>]
    [<DataRow("VariadicVariableMany1")>]
    [<DataRow("Expression")>]
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
    [<DataRow("Premise")>]
    [<DataRow("Conclusion")>]
    [<DataRow("Predicate")>]
    [<DataRow("FunctionalTerm")>]
    [<DataRow("Theory")>]
    [<DataRow("Root")>]
    [<TestMethod>]
    member this.TestBlockTypeShortName(var) =
        match var with
        | "Variable" -> Assert.AreEqual("var",FplValueType.Variable.ShortName)
        | "VariadicVariableMany" -> Assert.AreEqual("*var",FplValueType.VariadicVariableMany.ShortName)
        | "VariadicVariableMany1" -> Assert.AreEqual("+var",FplValueType.VariadicVariableMany1.ShortName)
        | "Expression" -> Assert.AreEqual("expr",FplValueType.Expression.ShortName)
        | "MandatoryFunctionalTerm" -> Assert.AreEqual("prop",FplValueType.MandatoryFunctionalTerm.ShortName)
        | "OptionalFunctionalTerm" -> Assert.AreEqual("prop",FplValueType.OptionalFunctionalTerm.ShortName)
        | "MandatoryPredicate" -> Assert.AreEqual("prop",FplValueType.MandatoryPredicate.ShortName)
        | "OptionalPredicate" -> Assert.AreEqual("prop",FplValueType.OptionalPredicate.ShortName)
        | "Constructor" -> Assert.AreEqual("ctor",FplValueType.Constructor.ShortName)
        | "Class" -> Assert.AreEqual("cl",FplValueType.Class.ShortName)
        | "Theorem" -> Assert.AreEqual("thm",FplValueType.Theorem.ShortName)
        | "Lemma" -> Assert.AreEqual("lem",FplValueType.Lemma.ShortName)
        | "Proposition" -> Assert.AreEqual("prop",FplValueType.Proposition.ShortName)
        | "Corollary" -> Assert.AreEqual("cor",FplValueType.Corollary.ShortName)
        | "Proof" -> Assert.AreEqual("prf",FplValueType.Proof.ShortName)
        | "Conjecture" -> Assert.AreEqual("conj",FplValueType.Conjecture.ShortName)
        | "Axiom" -> Assert.AreEqual("ax",FplValueType.Axiom.ShortName)
        | "RuleOfInference" -> Assert.AreEqual("inf",FplValueType.RuleOfInference.ShortName)
        | "Premise" -> Assert.AreEqual("pre",FplValueType.Premise.ShortName)
        | "Conclusion" -> Assert.AreEqual("con",FplValueType.Conclusion.ShortName)
        | "Predicate" -> Assert.AreEqual("pred",FplValueType.Predicate.ShortName)
        | "FunctionalTerm" -> Assert.AreEqual("func",FplValueType.FunctionalTerm.ShortName)
        | "Theory" -> Assert.AreEqual("th",FplValueType.Theory.ShortName)
        | "Root" -> Assert.AreEqual("root",FplValueType.Root.ShortName)
        | _ -> 
            Assert.IsTrue(false)

