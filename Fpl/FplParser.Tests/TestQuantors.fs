namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestQuantors () =

    [<TestMethod>]
    member this.TestQuantors01 () =
        let result = run (predicate .>> eof) """all x is obj {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors02 () =
        let result = run (predicate .>> eof) """all x is obj[x:TestClass] {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors03 () =
        let result = run (predicate .>> eof) """all x is func {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors04 () =
        let result = run (predicate .>> eof) """all x is ind {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors05 () =
        let result = run (predicate .>> eof) """all x is pred {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors06 () =
        let result = run (predicate .>> eof) """all x is TestClass {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors07 () =
        let result = run (predicate .>> eof) """all x is template {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors08 () =
        let result = run (predicate .>> eof) """all x is @Nat {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors09 () =
        let result = run (predicate .>> eof) """all x is func()->obj {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors10 () =
        let result = run (predicate .>> eof) """all x is SomeVar {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors11 () =
        let result = run (predicate .>> eof) """all x is self {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantors12 () =
        let result = run (predicate .>> eof) """all x is ClosedRange(from,to) {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantors13 () =
        let result = run (predicate .>> eof) """all x in T[x] {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantors14 () =
        let result = run (predicate .>> eof) """all x is T[x:func] {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors15 () =
        let result = run (predicate .>> eof) """all x is Range(a:B), y is C, z {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors16 () =
        let result = run (predicate .>> eof) """all x is Real, y is pred, z is func {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors17 () =
        let result = run (predicate .>> eof) """ex x is Range(a:B), y is C, z {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors18 () =
        let result = run (predicate .>> eof) """ex x is Real, y is pred, z is func {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestQuantors19 () =
        let result = run (predicate .>> eof) """all x,y,z{true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors20 () =
        let result = run (predicate .>> eof) """all x,y,z {not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors21 () =
        let result = run (predicate .>> eof) """all x,y,z {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors22 () =
        let result = run (predicate .>> eof) """all x {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors23 () =
        let result = run (predicate .>> eof) """ex x,y,z {true }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors24 () =
        let result = run (predicate .>> eof) """ex x,y,z { not (iif ( true, not false))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors25 () =
        let result = run (predicate .>> eof) """ex x,y,z {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors26 () =
        let result = run (predicate .>> eof) """ex x {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors27 () =
        let result = run (predicate .>> eof) """exn$0 x,y,z(true)"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantors28 () =
        let result = run (predicate .>> eof) """exn$1 x is Nat {not (iif ( true, not (false)))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors29 () =
        let result = run (predicate .>> eof) """exn$2 x in Nat,y {not (iif ( iif( true, false), true))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestQuantors30 () =
        let result = run (predicate .>> eof) """exn$3 x {not (iif ( iif ( true, iif( true, false)), not true ))}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors31 () =
        let result = run (predicate .>> eof) """all x is Range(a:B), y is C, z {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors32 () =
        let result = run (predicate .>> eof) """all x is Real, y is pred, z is func {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors33 () =
        let result = run (predicate .>> eof) """ex x is Range(a:B), y is C, z {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors34 () =
        let result = run (predicate .>> eof) """ex x is Real, y is pred, z is func {and (a,b,c)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors35 () =
        let result = run (predicate .>> eof) """ex x is Real {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestQuantors36 () =
        let result = run (predicate .>> eof) """ex x is Real {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
