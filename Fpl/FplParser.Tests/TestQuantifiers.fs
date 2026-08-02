namespace FplParser.Tests

open FParsec
open Fpl.Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestQuantifiers () =

    [<TestMethod>]
    member this.TestQuantifiers01 () =
        let result = run (predicate .>> eof) """all x:obj {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers02 () =
        let result = run (predicate .>> eof) """all x:obj {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers03 () =
        let result = run (predicate .>> eof) """all x:func {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers04 () =
        let result = run (predicate .>> eof) """all x:ind {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers05 () =
        let result = run (predicate .>> eof) """all x:pred {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers06 () =
        let result = run (predicate .>> eof) """all x:TestClass {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers07 () =
        let result = run (predicate .>> eof) """all x:template {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers08 () =
        let result = run (predicate .>> eof) """all x:Nat {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers09 () =
        let result = run (predicate .>> eof) """all x:func()->obj {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers10 () =
        let result = run (predicate .>> eof) """all x:SomeVar {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers11 () =
        let result = run (predicate .>> eof) """all x:self {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantifiers12 () =
        let result = run (predicate .>> eof) """all x:ClosedRange(from,to) {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantifiers13 () =
        let result = run (predicate .>> eof) """all x in T[x] {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantifiers14 () =
        let result = run (predicate .>> eof) """all x:T {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers15 () =
        let result = run (predicate .>> eof) """all x:Range, y:C, z:obj {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers16 () =
        let result = run (predicate .>> eof) """all x:Real, y:pred, z:func {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers17 () =
        let result = run (predicate .>> eof) """ex x:Range, y:C, z:obj {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers18 () =
        let result = run (predicate .>> eof) """ex x:Real, y:pred, z:func {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestQuantifiers19 () =
        let result = run (predicate .>> eof) """all x,y,z:pred {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers20 () =
        let result = run (predicate .>> eof) """all x,y,z:obj {not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers21 () =
        let result = run (predicate .>> eof) """all x,y,z:obj {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers22 () =
        let result = run (predicate .>> eof) """all  x:ind {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers23 () =
        let result = run (predicate .>> eof) """ex x,y,z:obj {true }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers24 () =
        let result = run (predicate .>> eof) """ex x,y,z:obj { not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers25 () =
        let result = run (predicate .>> eof) """ex x,y,z:obj {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers26 () =
        let result = run (predicate .>> eof) """ex  x:ind {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers27 () =
        let result = run (predicate .>> eof) """exn$0 x,y,z(true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantifiers28 () =
        let result = run (predicate .>> eof) """exn$1 x:Nat {not (iif ( true, not (false)))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers29 () =
        let result = run (predicate .>> eof) """exn$2 x in Nat,y {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantifiers30 () =
        let result = run (predicate .>> eof) """exn$3  x:ind {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers31 () =
        let result = run (predicate .>> eof) """all x:Range, y:C, z:obj {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers32 () =
        let result = run (predicate .>> eof) """all x:Real, y:pred, z:func {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers33 () =
        let result = run (predicate .>> eof) """ex x:Range, y:C, z:obj {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers34 () =
        let result = run (predicate .>> eof) """ex x:Real, y:pred, z:func {and (and(a,b),c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers35 () =
        let result = run (predicate .>> eof) """ex x:Real {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantifiers36 () =
        let result = run (predicate .>> eof) """ex x:Real {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
