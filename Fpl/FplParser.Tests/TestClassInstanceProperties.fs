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
                dec ~a:obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01c () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
                dec ~a:obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance01d () =
        let result = run property """mand T X() 
	        {
                // a class instance without a predicate is not allowed
                dec ~a:obj ;
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
        // a class instance with a self at the end
        let result = run property """property optional T X() 
	        {
                dec ~a:obj;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2b () =
        // a class instance with a self at the end
        let result = run property """property T X() 
	        {
                dec ~a:obj;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2c () =
        // a class instance with a self at the end 
        let result = run property """property optional T X() 
	        {
                dec ~a:obj;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2d () =
        // a class instance with a self at the end 
        let result = run property """property T X() 
	        {
                dec ~a:obj;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2e () =
        // a class instance with a self at the end
        let result = run property """property opt T X() 
	        {
                dec ~a:obj;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance2f () =
        // a class instance with a self at the end 
        let result = run property """property T X() 
	        {
                dec ~a:obj;
                self
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClassInstance3 () =
            // a class instance with a a self at the end and 
            // some other content following it is not allowed
        let result = run property """property T X() 
	        {
                self
                dec ~a:obj;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClassInstance3a () =
        // a class instance with a self at the end and 
        // some other content following it is not allowed
        let result = run property """property T X() 
	        {
                self
                dec ~a:obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    