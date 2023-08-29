namespace FplParser.Tests

open FParsec
open FplGrammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open Newtonsoft.Json

[<TestClass>]
type TestReferenceRules() =

    [<TestMethod>]
    member this.TestReferenceRule01 () =
        let result = run ruleOfInference """ModusPonens()
        {
            p,q: pred

            premise:
                and (p, impl (p,q) )
            conclusion:
                q
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: RuleOfInference
  (Signature (AliasedId ["ModusPonens"], []),
   (([BlockVariableDeclaration
        ([Var "p"; Var "q"], VariableTypeWithModifier (None, PredicateType))],
     And
       [PredicateWithoutArgs (Var "p");
        Impl (PredicateWithoutArgs (Var "p"), PredicateWithoutArgs (Var "q"))]),
    PredicateWithoutArgs (Var "q")))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestReferenceRule02 () =
        let result = run ruleOfInference """ModusTollens()
        {
            p,q: pred

            premise:
                and (not(q), impl(p,q) )
            conclusion:
                not (p)
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: RuleOfInference
  (Signature (AliasedId ["ModusTollens"], []),
   (([BlockVariableDeclaration
        ([Var "p"; Var "q"], VariableTypeWithModifier (None, PredicateType))],
     And
       [Not (PredicateWithoutArgs (Var "q"));
        Impl (PredicateWithoutArgs (Var "p"), PredicateWithoutArgs (Var "q"))]),
    Not (PredicateWithoutArgs (Var "p"))))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestReferenceRule03 () =
        let result = run ruleOfInference """HypotheticalSyllogism()
        {
            p,q,r: pred
            premise:
                and (impl(p,q), impl(q,r))
            conclusion:
                impl(p,r)
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: RuleOfInference
  (Signature (AliasedId ["HypotheticalSyllogism"], []),
   (([BlockVariableDeclaration
        ([Var "p"; Var "q"; Var "r"],
         VariableTypeWithModifier (None, PredicateType))],
     And
       [Impl (PredicateWithoutArgs (Var "p"), PredicateWithoutArgs (Var "q"));
        Impl (PredicateWithoutArgs (Var "q"), PredicateWithoutArgs (Var "r"))]),
    Impl (PredicateWithoutArgs (Var "p"), PredicateWithoutArgs (Var "r"))))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestReferenceRule04 () =
        let result = run ruleOfInference """DisjunctiveSyllogism()
        {
            p,q: pred
            premise:
                and (not(p), or(p,q))
            conclusion:
                q
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: RuleOfInference
  (Signature (AliasedId ["DisjunctiveSyllogism"], []),
   (([BlockVariableDeclaration
        ([Var "p"; Var "q"], VariableTypeWithModifier (None, PredicateType))],
     And
       [Not (PredicateWithoutArgs (Var "p"));
        Or [PredicateWithoutArgs (Var "p"); PredicateWithoutArgs (Var "q")]]),
    PredicateWithoutArgs (Var "q")))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestReferenceRule05 () =
        let result = run ruleOfInference """ProceedingResults(p: +pred)
        {
            proceedingResult: pred
            
            range proceedingResult p$
            (
                assert proceedingResult
            )

            premise: undefined
            conclusion: and (p)
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: RuleOfInference
  (Signature
     (AliasedId ["ProceedingResults"],
      [([Var "p"], VariableTypeWithModifier (Some Many1, PredicateType))]),
   (([BlockVariableDeclaration
        ([Var "proceedingResult"],
         VariableTypeWithModifier (None, PredicateType));
      BlockStatement
        (Range
           ((Var "proceedingResult", Var "p"),
            [Assertion (PredicateWithoutArgs (Var "proceedingResult"))]))],
     Undefined), And [PredicateWithoutArgs (Var "p")]))""".Replace("\r","")
        Assert.AreEqual(expected, actual);

    [<TestMethod>]
    member this.TestReferenceRule06 () =
        let result = run ruleOfInference """ExistsByExample(p: pred(c: obj))
        {
            x: obj
            premise:
                p(c)
            conclusion:
                ex x(p(x))
        }"""
        let actual = sprintf "%O" result
        let expected = """Success: RuleOfInference
  (Signature
     (AliasedId ["ExistsByExample"],
      [([Var "p"],
        VariableType [([Var "c"], VariableTypeWithModifier (None, ObjectType))])]),
   (([BlockVariableDeclaration
        ([Var "x"], VariableTypeWithModifier (None, ObjectType))],
     PredicateWithArgs (Var "p", [PredicateWithoutArgs (Var "c")])),
    Exists
      ([Var "x"], PredicateWithArgs (Var "p", [PredicateWithoutArgs (Var "x")]))))""".Replace("\r","")
        Assert.AreEqual(expected, actual);
