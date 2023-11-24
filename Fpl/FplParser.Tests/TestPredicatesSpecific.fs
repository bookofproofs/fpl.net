namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestPredicatesSpecific () =

    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""


    [<TestMethod>]
    member this.TestPrimePredicate1 () =
        let result = run primePredicate """true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate2 () =
        let result = run primePredicate """false"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate3 () =
        let result = run primePredicate """undef"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate4 () =
        let result = run primePredicate """undefined"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate5 () =
        let result = run primePredicate """list$i"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate6 () =
        let result = run primePredicate """arr<i>"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate7 () =
        let result = run primePredicate """@@self"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjunction1 () =
        let result = run conjunction """and(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjunction2 () =
        let result = run conjunction """and ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjunction3 () =
        let result = run conjunction """and ( true, and( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjunction4 () =
        let result = run conjunction """and ( and ( true, and( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<TestMethod>]
    member this.TestDisjunctrion1 () =
        let result = run disjunction """or(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDisjunctrion2 () =
        let result = run disjunction """or ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDisjunctrion3 () =
        let result = run disjunction """or ( true, or( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDisjunctrion4 () =
        let result = run disjunction """or ( or ( true, or( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDisjunctrion5 () =
        let result = run disjunction """or(1.,2.)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestImplication1 () =
        let result = run implication """impl(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestImplication2 () =
        let result = run implication """impl ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestImplication3 () =
        let result = run implication """impl ( true, impl( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestImplication4 () =
        let result = run implication """impl ( impl ( true, impl( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEquivalence1 () =
        let result = run equivalence """iif(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEquivalence2 () =
        let result = run equivalence """iif ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEquivalence3 () =
        let result = run equivalence """iif ( true, iif( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEquivalence4 () =
        let result = run equivalence """iif ( iif ( true, iif( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestXor1 () =
        let result = run exclusiveOr """xor(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestXor2 () =
        let result = run exclusiveOr """xor ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestXor3 () =
        let result = run exclusiveOr """xor ( true, xor( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestXor4 () =
        let result = run exclusiveOr """xor ( xor ( true, xor( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs1 () =
        let result = run predicateWithQualification """Zero()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs2 () =
        let result = run predicateWithQualification """self(i)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs3 () =
        let result = run predicateWithQualification """ProceedingResults(1.,2.)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs4 () =
        let result = run predicateWithQualification """Add(result,list!i)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs5 () =
        let result = run predicateWithQualification """Add(result,arr<i>)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs1a () =
        let result = run predicateWithQualification """x!Zero()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs2a () =
        let result = run predicateWithQualification """x!self(i)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs3a () =
        let result = run predicateWithQualification """x!ProceedingResults(1.,2.)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs4a () =
        let result = run predicateWithQualification """x!Add(result,list!i)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs5a () =
        let result = run predicateWithQualification """x!Add(result,arr<i>)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs6a () =
        let result = run predicateWithQualification """x!A1()!A2()!A3() """
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs7 () =
        let result = run predicateWithQualification """x!$3!$2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs71 () =
        let result = run (predicateWithQualification .>> eof) """x!$3()!$2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
        
    [<TestMethod>]
    member this.TestPredicateWithArgs7a () =
        let result = run (predicateWithQualification .>> eof) """x!3!2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs71a () =
        let result = run (predicateWithQualification .>> eof) """x!3()!2"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))



    [<TestMethod>]
    member this.TestQualifiedIdentifier1 () =
        let result = run (predicateWithQualification .>> eof) """myOp.NeutralElement()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQualifiedIdentifier2 () =
        let result = run (predicateWithQualification .>> eof) """myOp.NeutralElement().SomeProperty()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestNot1 () =
        let result = run negation """not(true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestNot2 () =
        let result = run negation """not (iif ( true, not(false)))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestNot3 () =
        let result = run negation """not (iif ( iif( true, false), true))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestNot4 () =
        let result = run negation """not(iif ( iif ( true, iif( true, false)), not(true) ))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestIsOperator1 () =
        let result = run isOperator """is(x, Nat)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestIsOperator2 () =
        let result = run isOperator """is(1, Set)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestIsOperator3 () =
        let result = run isOperator """is(One, Set)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestIsOperator4 () =
        let result = run isOperator """is(T.X.Y, Set)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestIsOperator5 () =
        let result = run isOperator """is(self, Set)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestIsOperator6 () =
        let result = run isOperator """is(@self, Set)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll1 () =
        let result = run all """all x,y,z(true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll2 () =
        let result = run all """all x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll3 () =
        let result = run all """all x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll4 () =
        let result = run all """all x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll5 () =
        let result = run all """all x in [a,b], y in c, z (and (a,b,c))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll6 () =
        let result = run all """all x in Real, y in pred, z in func (and (a,b,c))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx1 () =
        let result = run exists """ex x,y,z(true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx2 () =
        let result = run exists """ex x,y,z (not (iif ( true, not(false))))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx3 () =
        let result = run exists """ex x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx4 () =
        let result = run exists """ex x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx5 () =
        let result = run exists """ex x in [a,b], y in c, z (and (a,b,c))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx6 () =
        let result = run exists """ex x in Real, y in pred, z in func (and (a,b,c))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExN1 () =
        let result = run existsTimesN """exn$0 x (true)"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExN2 () =
        let result = run existsTimesN """exn$1 x in Nat (not (iif ( true, not(false))))"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExN3 () =
        let result = run existsTimesN """exn$2 x,y,z (not (iif ( iif( true, false), true)))"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestExN4 () =
        let result = run existsTimesN """exn$3 x (not(iif ( iif ( true, iif( true, false)), not(true) )))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestByDef01 () =
        let result = run byDefinition """bydef x"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestByDef02 () =
        let result = run byDefinition """bydef x!y"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
 