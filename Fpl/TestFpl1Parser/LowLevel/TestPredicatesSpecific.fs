namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestPredicatesSpecific () =

    [<DataRow("01", LiteralTrue)>]
    [<DataRow("02", LiteralFalse)>]
    [<DataRow("03", LiteralUndef)>]
    [<DataRow("04", LiteralUndefL)>]
    [<DataRow("05", """list[i]""")>]
    [<DataRow("06", """arr[i]""")>]
    [<DataRow("07", LiteralParent)>]
    [<TestMethod>]
    member this.TestPrimePredicateSuccess (no:string, fplCode:string) =
        let result = run (primePredicate .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """and(true,false)""")>]
    [<DataRow("02", """and ( true, true )""")>]
    [<DataRow("03", """and ( true, and( true, false))""")>]
    [<DataRow("04", """and ( and ( true, and( true, false)), true )""")>]
    [<TestMethod>]
    member this.TestConjunctionSuccess (no:string, fplCode:string) =
        let result = run (conjunction .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """or(true,false)""")>]
    [<DataRow("02", """or ( true, true )""")>]
    [<DataRow("03", """or ( true, or( true, false))""")>]
    [<DataRow("04", """or ( or ( true, or( true, false)), true )""")>]
    [<TestMethod>]
    member this.TestDisjunctionSuccess (no:string, fplCode:string) =
        let result = run (disjunction .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """impl(true,false)""")>]
    [<DataRow("02", """impl ( true, true )""")>]
    [<DataRow("03", """impl ( true, impl( true, false))""")>]
    [<DataRow("04", """impl ( impl ( true, impl( true, false)), true )""")>]
    [<TestMethod>]
    member this.TestImplicationSuccess (no:string, fplCode:string) =
        let result = run (implication .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """iif(true,false)""")>]
    [<DataRow("02", """iif ( true, true )""")>]
    [<DataRow("03", """iif ( true, iif( true, false))""")>]
    [<DataRow("04", """iif ( iif ( true, iif( true, false)), true )""")>]
    [<TestMethod>]
    member this.TestEquivalenceSuccess (no:string, fplCode:string) =
        let result = run (equivalence .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """xor(true,false)""")>]
    [<DataRow("02", """xor ( true, true )""")>]
    [<DataRow("02a", """xor ( true, xor(true, false) )""")>]
    [<DataRow("03", """xor ( true, xor( true, false))""")>]
    [<DataRow("04", """xor ( xor ( true, xor( true, false)), true )""")>]
    [<TestMethod>]
    member this.TestExclusiveOrSuccess (no:string, fplCode:string) =
        let result = run (exclusiveOr .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """Zero()""")>]
    [<DataRow("02", """self(i)""")>]
    [<DataRow("04", """Add(result,list[i])""")>]
    [<DataRow("05", """Add(result,arr[i])""")>]
    [<DataRow("01a", """x[Zero()]""")>]
    [<DataRow("02a", """x[self(i)]""")>]
    [<DataRow("03a", """x[PrecedingResults(x,y)]""")>]
    [<DataRow("04a", """x[Add(result,list[i])]""")>]
    [<DataRow("05a", """x[Add(result,arr[i])]""")>]
    [<DataRow("06a", """x[A1[A2().A3()]]""")>]
    [<DataRow("07", """x[$3,$2]""")>]
    [<DataRow("07a", """x[@3,@3]""")>]
    [<DataRow("71a", """x[@3()]""")>]
    [<DataRow("08", """myOp.NeutralElement()""")>]
    [<DataRow("09", """myOp.NeutralElement().SomeProperty()""")>]
    [<TestMethod>]
    member this.TestPredicateWithQualificationSuccess (no:string, fplCode:string) =
        let result = run (predicateWithQualification .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """x[$3(),$2]""")>]
    [<TestMethod>]
    member this.TestPredicateWithQualificationFailure (no:string, fplCode:string) =
        let result = run (predicateWithQualification .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<DataRow("01", """not (true)""")>]
    [<DataRow("02", """not (iif ( true, not (false)))""")>]
    [<DataRow("03", """not (iif ( iif( true, false), true))""")>]
    [<DataRow("04", """not (iif ( iif ( true, iif( true, false)), not (true) ))""")>]
    [<DataRow("05", """not all x,y:N { (x >< y) }""")>]
    [<TestMethod>]
    member this.TestNegationSuccess (no:string, fplCode:string) =
        let result = run (negation .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """is(x, Nat)""")>]
    [<DataRow("02", """is(1, Set)""")>]
    [<DataRow("03", """is(One, Set)""")>]
    [<DataRow("04", """is(T.X.Y, Set)""")>]
    [<DataRow("05", """is(self, Set)""")>]
    [<DataRow("06", """is(parent, Set)""")>]
    [<DataRow("07", """is(A$1, Set)""")>]
    [<DataRow("08", """is($1, ind)""")>]
    [<DataRow("09", """is(undef, ind)""")>]
    [<DataRow("10", """is(true, ind)""")>]
    [<DataRow("11", """is(false, ind)""")>]
    [<TestMethod>]
    member this.TestIsOperatorSuccess (no:string, fplCode:string) =
        let result = run (isOperator .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """all x,y,z:obj {true}""")>]
    [<DataRow("02", """all x,y,z:obj {not (iif ( true, not false))}""")>]
    [<DataRow("03", """all x,y,z:obj {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("04", """all x:obj {not (iif ( iif ( true, iif( true, false)), not (true) ))}""")>]
    [<DataRow("05", """all x:Range, y:C, z:obj {and (and (a,b),c)}""")>]
    [<DataRow("06", """all x:Real, y:pred, z:func {and (and(a,b),c)}""")>]
    [<TestMethod>]
    member this.TestAllSuccess (no:string, fplCode:string) =
        let result = run (all .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """ex x,y,z:func {true}""")>]
    [<DataRow("02", """ex x,y,z:ind {not (iif ( true, not (false)))}""")>]
    [<DataRow("03", """ex x,y,z:pred {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("04", """ex x:obj {not (iif ( iif ( true, iif( true, false)), not (true) ))}""")>]
    [<DataRow("05", """ex x:Range, y:C, z:obj {and (a,and(b,c))}""")>]
    [<DataRow("06", """ex x:Real, y:pred, z:func {and (and(a,b),c)}""")>]
    [<TestMethod>]
    member this.TestExistsSuccess (no:string, fplCode:string) =
        let result = run (exists .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """exn$0 x:obj { true}""")>]
    [<DataRow("02", """exn$1 x:Nat {not (iif ( true, not (false)))}""")>]
    [<DataRow("03", """exn$2 x,y,z:obj {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("04", """exn$3 x:obj { not (iif ( iif ( true, iif( true, false)), not (true) )) }""")>]
    [<TestMethod>]
    member this.TestExistsTimesNSuccess (no:string, fplCode:string) =
        let result = run (existsTimesN .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """( x = 1 )""")>]
    [<DataRow("02", """( x = y = z )""")>]
    [<DataRow("03", """( x ∈ y ∈ z )""")>]
    [<DataRow("04", """( x + y / z = abc )""")>]
    [<DataRow("05", """( ((x) + y) / z = abc )""")>]
    [<TestMethod>]
    member this.TestInfixExprSuccess (no:string, fplCode:string) =
        let result = run (pInfixExpr .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
