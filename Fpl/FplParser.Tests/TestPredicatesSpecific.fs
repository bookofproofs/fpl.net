namespace FplParser.Tests

open FParsec
open FplParser
open FplPrimitives
open Microsoft.VisualStudio.TestTools.UnitTesting



[<TestClass>]
type TestPredicatesSpecific () =

    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""


    [<TestMethod>]
    member this.TestPrimePredicate1 () =
        let result = run (primePredicate .>> eof) LiteralTrue
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate2 () =
        let result = run (primePredicate .>> eof) LiteralFalse
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate3 () =
        let result = run (primePredicate .>> eof) LiteralUndef
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate4 () =
        let result = run (primePredicate .>> eof) LiteralUndefL
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate5 () =
        let result = run (primePredicate .>> eof) """list[i]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate6 () =
        let result = run (primePredicate .>> eof) """arr[i]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPrimePredicate7 () =
        let result = run (primePredicate .>> eof) LiteralParent
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjunction1 () =
        let result = run (conjunction .>> eof) """and(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjunction2 () =
        let result = run (conjunction .>> eof) """and ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjunction3 () =
        let result = run (conjunction .>> eof) """and ( true, and( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConjunction4 () =
        let result = run (conjunction .>> eof) """and ( and ( true, and( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<TestMethod>]
    member this.TestDisjunctrion1 () =
        let result = run (disjunction .>> eof) """or(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDisjunctrion2 () =
        let result = run (disjunction .>> eof) """or ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDisjunctrion3 () =
        let result = run (disjunction .>> eof) """or ( true, or( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDisjunctrion4 () =
        let result = run (disjunction .>> eof) """or ( or ( true, or( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestImplication1 () =
        let result = run (implication .>> eof) """impl(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestImplication2 () =
        let result = run (implication .>> eof) """impl ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestImplication3 () =
        let result = run (implication .>> eof) """impl ( true, impl( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestImplication4 () =
        let result = run (implication .>> eof) """impl ( impl ( true, impl( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEquivalence1 () =
        let result = run (equivalence .>> eof) """iif(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEquivalence2 () =
        let result = run (equivalence .>> eof) """iif ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEquivalence3 () =
        let result = run (equivalence .>> eof) """iif ( true, iif( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEquivalence4 () =
        let result = run (equivalence .>> eof) """iif ( iif ( true, iif( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestXor1 () =
        let result = run (exclusiveOr .>> eof) """xor(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestXor2 () =
        let result = run (exclusiveOr .>> eof) """xor ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:")) 

    [<TestMethod>]
    member this.TestXor2a () =
        let result = run (exclusiveOr .>> eof) """xor ( true, xor(true, false) )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestXor3 () =
        let result = run (exclusiveOr .>> eof) """xor ( true, xor( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestXor4 () =
        let result = run (exclusiveOr .>> eof) """xor ( xor ( true, xor( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs1 () =
        let result = run (predicateWithQualification .>> eof) """Zero()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs2 () =
        let result = run (predicateWithQualification .>> eof) """self(i)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs4 () =
        let result = run (predicateWithQualification .>> eof) """Add(result,list[i])"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs5 () =
        let result = run (predicateWithQualification .>> eof) """Add(result,arr[i])"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs1a () =
        let result = run (predicateWithQualification .>> eof) """x[Zero()]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs2a () =
        let result = run (predicateWithQualification .>> eof) """x[self(i)]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs3a () =
        let result = run (predicateWithQualification .>> eof) """x[ProceedingResults(x,y)]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs4a () =
        let result = run (predicateWithQualification .>> eof) """x[Add(result,list[i])]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs5a () =
        let result = run (predicateWithQualification .>> eof) """x[Add(result,arr[i])]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs6a () =
        let result = run (predicateWithQualification .>> eof) """x[A1[A2().A3()]]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs7 () =
        let result = run (predicateWithQualification .>> eof) """x[$3,$2]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs71 () =
        let result = run (predicateWithQualification .>> eof) """x[$3(),$2]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
        
    [<TestMethod>]
    member this.TestPredicateWithArgs7a () =
        let result = run (predicateWithQualification .>> eof) """x[@3,@3]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateWithArgs71a () =
        let result = run (predicateWithQualification .>> eof) """x[@3()]"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))



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
        let result = run (negation .>> eof) """not (true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestNot2 () =
        let result = run (negation .>> eof) """not (iif ( true, not (false)))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestNot3 () =
        let result = run (negation .>> eof) """not (iif ( iif( true, false), true))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestNot4 () =
        let result = run (negation .>> eof) """not (iif ( iif ( true, iif( true, false)), not (true) ))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestNot5 () =
        let result = run (negation .>> eof) """not all x,y:N { (x >< y) }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("is(x, Nat)")>]
    [<DataRow("is(1, Set)")>]
    [<DataRow("is(One, Set)")>]
    [<DataRow("is(T.X.Y, Set)")>]
    [<DataRow("is(self, Set)")>]
    [<DataRow("is(parent, Set)")>]
    [<DataRow("is(A$1, Set)")>]
    [<DataRow("is($1, ind)")>]
    [<DataRow("is(undef, ind)")>]
    [<DataRow("is(true, ind)")>]
    [<DataRow("is(false, ind)")>]
    [<DataRow("is(x, A$1)")>]

    [<TestMethod>]
    member this.TestIsOperator (fplCode:string) =
        let result = run (isOperator .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll1 () =
        let result = run (all .>> eof) """all x,y,z:obj {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll2 () =
        let result = run (all .>> eof) """all x,y,z:obj {not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll3 () =
        let result = run (all .>> eof) """all x,y,z:obj {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll4 () =
        let result = run (all .>> eof) """all x:obj {not (iif ( iif ( true, iif( true, false)), not (true) ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll5 () =
        let result = run (all .>> eof) """all x:Range, y:C, z:obj {and (and (a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestAll6 () =
        let result = run (all .>> eof) """all x:Real, y:pred, z:func {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx1 () =
        let result = run (exists .>> eof) """ex x,y,z:func {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx2 () =
        let result = run (exists .>> eof) """ex x,y,z:ind {not (iif ( true, not (false)))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx3 () =
        let result = run (exists .>> eof) """ex x,y,z:pred {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx4 () =
        let result = run (exists .>> eof) """ex x:obj {not (iif ( iif ( true, iif( true, false)), not (true) ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx5 () =
        let result = run (exists .>> eof) """ex x:Range, y:C, z:obj {and (a,and(b,c))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestEx6 () =
        let result = run (exists .>> eof) """ex x:Real, y:pred, z:func {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExN1 () =
        let result = run (existsTimesN .>> eof) """exn$0 x:obj { true}"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExN2 () =
        let result = run (existsTimesN .>> eof) """exn$1 x:Nat {not (iif ( true, not (false)))}"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExN3 () =
        let result = run (existsTimesN .>> eof) """exn$2 x,y,z:obj {not (iif ( iif( true, false), true))}"""
        let actual = replaceWhiteSpace (sprintf "%O" result)
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestExN4 () =
        let result = run (existsTimesN .>> eof) """exn$3 x:obj { not (iif ( iif ( true, iif( true, false)), not (true) )) }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
 
    [<TestMethod>]
    member this.TestOperator01 () =
        let result = run (infixOperation .>> eof) """( x = 1 )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestOperator02 () =
        let result = run (infixOperation .>> eof) """( x = y = z )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestOperator03 () =
        let result = run (infixOperation .>> eof) """( x b y c z )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestOperator04 () =
        let result = run (infixOperation .>> eof) """( x + y / z = abc )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestOperator05 () =
        let result = run (infixOperation .>> eof) """( ((x) + y) / z = abc )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
 