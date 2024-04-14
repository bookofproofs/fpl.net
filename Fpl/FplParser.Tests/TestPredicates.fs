namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestPredicates () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""


    [<TestMethod>]
    member this.TestPredicate01 () =
        let result = run (predicate .>> eof) """true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate01a () =
        let result = run (primePredicate .>> eof) """true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate02 () =
        let result = run (predicate .>> eof) """false"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate03 () =
        let result = run (predicate .>> eof) """undef"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate04 () =
        let result = run (predicate .>> eof) """undefined"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate05 () =
        let result = run (predicate .>> eof) """and(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate06 () =
        let result = run (predicate .>> eof) """and ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate07 () =
        let result = run (predicate .>> eof) """and ( true, and( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate08 () =
        let result = run (predicate .>> eof) """and ( and ( true, and( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        
    [<TestMethod>]
    member this.TestPredicate09 () =
        let result = run (predicate .>> eof) """or(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate10 () =
        let result = run (predicate .>> eof) """or ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate11 () =
        let result = run (predicate .>> eof) """or ( true, or( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate12 () =
        let result = run (predicate .>> eof) """or ( or ( true, or( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate13 () =
        let result = run (predicate .>> eof) """or(1.,2.)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate14 () =
        let result = run (predicate .>> eof) """impl(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate15 () =
        let result = run (predicate .>> eof) """impl ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate16 () =
        let result = run (predicate .>> eof) """impl ( true, impl( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate17 () =
        let result = run (predicate .>> eof) """impl ( impl ( true, impl( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate18 () =
        let result = run (predicate .>> eof) """iif(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate19 () =
        let result = run (predicate .>> eof) """iif ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate20 () =
        let result = run (predicate .>> eof) """iif ( true, iif( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate21 () =
        let result = run (predicate .>> eof) """iif ( iif ( true, iif( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate22 () =
        let result = run (predicate .>> eof) """xor(true,false)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate23 () =
        let result = run (predicate .>> eof) """xor ( true, true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate23a () =
        let result = run (predicate .>> eof) """xor ( true, xor( true, false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate24 () =
        let result = run (predicate .>> eof) """xor ( xor ( true, xor( true, false)), true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate25 () =
        let result = run (predicate .>> eof) """Zero()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"));

    [<TestMethod>]
    member this.TestPredicate26 () =
        let result = run (predicate .>> eof) """self(i)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate27 () =
        let result = run (predicate .>> eof) """ProceedingResults(1.,2.)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate28 () =
        let result = run (predicate .>> eof) """myOp.NeutralElement()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate29 () =
        let result = run (predicate .>> eof) """myOp.NeutralElement().SomeProperty()"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate30 () =
        let result = run (predicate .>> eof) """not true"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate31 () =
        let result = run (predicate .>> eof) """not (iif ( true, not false))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestPredicate32 () =
        let result = run (predicate .>> eof) """not (iif ( iif( true, false), true))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate33 () =
        let result = run (predicate .>> eof) """not iif ( iif ( true, iif( true, false)), not true )"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate34 () =
        let result = run (predicate .>> eof) """is(x, Nat)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate35 () =
        let result = run (predicate .>> eof) """all x,y,z{true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate36 () =
        let result = run (predicate .>> eof) """all x,y,z {not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate37 () =
        let result = run (predicate .>> eof) """all x,y,z {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate38 () =
        let result = run (predicate .>> eof) """all x {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate39 () =
        let result = run (predicate .>> eof) """ex x,y,z {true }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate40 () =
        let result = run (predicate .>> eof) """ex x,y,z { not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate41 () =
        let result = run (predicate .>> eof) """ex x,y,z {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate42 () =
        let result = run (predicate .>> eof) """ex x {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate43 () =
        let result = run (predicate .>> eof) """exn$0 x,y,z(true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicate44 () =
        let result = run (predicate .>> eof) """exn$1 x in Nat {not (iif ( true, not (false)))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate45 () =
        let result = run (predicate .>> eof) """exn$2 x in Nat,y {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicate46 () =
        let result = run (predicate .>> eof) """exn$3 x {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate47 () =
        let result = run (predicate .>> eof) """or(1.,2.)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate48 () =
        let result = run (predicate .>> eof) """all arg in args 
				{
					is(arg,Set)
				}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate49 () =
        let result = run (predicate .>> eof) """delegate.abc(x,y,z)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestPredicate50 () =
        let result = run (predicate .>> eof) """(z = y)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate51 () =
        let result = run (predicate .>> eof) """(z @= true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate52 () =
        let result = run (predicate .>> eof) """(z @ true @= and(x,y))"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate53 () =
        let result = run (predicate .>> eof) """all x in Range(a,b), y in c, z {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate54 () =
        let result = run (predicate .>> eof) """all x is Real, y is pred, z is func {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate55 () =
        let result = run (predicate .>> eof) """ex x in Range(a,b), y in c, z {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicate56 () =
        let result = run (predicate .>> eof) """ex x is Real, y is pred, z is func {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
