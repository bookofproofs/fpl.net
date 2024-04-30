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
        | "Variable" -> Assert.AreEqual("a variable",FplBlockType.Variable.Name)
        | "VariadicVariableMany" -> Assert.AreEqual("a zero-or-more variable",FplBlockType.VariadicVariableMany.Name)
        | "VariadicVariableMany1" -> Assert.AreEqual("a one-or-more variable",FplBlockType.VariadicVariableMany1.Name)
        | "Expression" -> Assert.AreEqual("an expression",FplBlockType.Expression.Name)
        | "MandatoryFunctionalTerm" -> Assert.AreEqual("a functional term property",FplBlockType.MandatoryFunctionalTerm.Name)
        | "OptionalFunctionalTerm" -> Assert.AreEqual("an optional functional term property",FplBlockType.OptionalFunctionalTerm.Name)
        | "MandatoryPredicate" -> Assert.AreEqual("a predicate property",FplBlockType.MandatoryPredicate.Name)
        | "OptionalPredicate" -> Assert.AreEqual("an optional predicate property",FplBlockType.OptionalPredicate.Name)
        | "Constructor" -> Assert.AreEqual("a constructor",FplBlockType.Constructor.Name)
        | "Class" -> Assert.AreEqual("a class definition",FplBlockType.Class({Inheritance.From = []}).Name)
        | "Theorem" -> Assert.AreEqual("a theorem",FplBlockType.Theorem.Name)
        | "Lemma" -> Assert.AreEqual("a lemma",FplBlockType.Lemma.Name)
        | "Proposition" -> Assert.AreEqual("a proposition",FplBlockType.Proposition.Name)
        | "Corollary" -> Assert.AreEqual("a corollary",FplBlockType.Corollary.Name)
        | "Proof" -> Assert.AreEqual("a proof",FplBlockType.Proof.Name)
        | "Conjecture" -> Assert.AreEqual("a conjecture",FplBlockType.Conjecture.Name)
        | "Axiom" -> Assert.AreEqual("an axiom",FplBlockType.Axiom.Name)
        | "RuleOfInference" -> Assert.AreEqual("a rule of inference",FplBlockType.RuleOfInference.Name)
        | "Premise" -> Assert.AreEqual("a premise",FplBlockType.Premise.Name)
        | "Conclusion" -> Assert.AreEqual("a conclusion",FplBlockType.Conclusion.Name)
        | "Predicate" -> Assert.AreEqual("a predicate definition",FplBlockType.Predicate.Name)
        | "FunctionalTerm" -> Assert.AreEqual("a functional term definition",FplBlockType.FunctionalTerm.Name)
        | "Theory" -> Assert.AreEqual("a theory",FplBlockType.Theory.Name)
        | "Root" -> Assert.AreEqual("a root",FplBlockType.Root.Name)
        | _ -> 
            Assert.IsTrue(false)

