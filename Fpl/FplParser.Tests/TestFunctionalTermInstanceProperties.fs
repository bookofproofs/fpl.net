namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestFunctionalTermProperties () =

    [<TestMethod>]
    member this.TestClassInstance01 () =
        let result = run property """func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01a () =
        let result = run property """func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01b () =
        let result = run property """func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01c () =
        let result = run property """func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec:;
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01d () =
        let result = run property """func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec:;
                spec:;
                x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01e () =
        let result = run property """func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec:;
                x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01f () =
        let result = run property """func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance2a () =
        let result = run property """optional func X() -> Y
	        {
                // a function term instance with a return statement 
                dec:;
                spec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2b () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance with a return statement 
                dec:;
                spec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2c () =
        let result = run property """optional func X() -> Y
	        {
                // a function term instance with a return statement 
                spec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2d () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance with a return statement 
                spec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2e () =
        let result = run property """optional func X() -> Y
	        {
                // a function term instance with a return statement 
                dec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2f () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance with a return statement 
                dec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance3 () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance with a return statement and 
                // some other content following it is not allowed
                return x
                // except comments
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance3a () =
        let result = run property """func X() -> Y
	        {
                // a function term instance with a return statement and 
                // some other content following it is not allowed
                return x
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance3b () =
        let result = run property """func X() -> Y
	        {
                // a function term instance with a return statement and 
                // some other content following it is not allowed
                return x
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
