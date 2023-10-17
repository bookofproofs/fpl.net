namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClassInstanceProperties () =

    [<TestMethod>]
    member this.TestClassInstance01 () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01a () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01b () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01c () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
                dec:;
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01d () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
                dec:;
                spec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01e () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
                dec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01f () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance2a () =
        let result = run property """optional T X() 
	        {
                // a class instance with a self at the end
                dec:;
                spec:;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2b () =
        let result = run property """mand T X() 
	        {
                // a class instance with a self at the end
                dec:;
                spec:;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2c () =
        let result = run property """optional T X() 
	        {
                // a class instance with a self at the end 
                spec:;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2d () =
        let result = run property """mand T X() 
	        {
                // a class instance with a self at the end 
                spec:;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2e () =
        let result = run property """optional T X() 
	        {
                // a class instance with a self at the end
                dec:;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2f () =
        let result = run property """mand T X() 
	        {
                // a class instance with a self at the end 
                dec:;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance3 () =
        let result = run property """mand T X() 
	        {
                // a class instance with a a self at the end and 
                // some other content following it is not allowed
                self
                // except comments
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance3a () =
        let result = run property """mand T X() 
	        {
                // a class instance with a self at the end and 
                // some other content following it is not allowed
                self
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance3b () =
        let result = run property """mand T X() 
	        {
                // a class instance with a self at the end and 
                // some other content following it is not allowed
                self
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
