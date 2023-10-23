namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestPredicateInstanceProperties () =

    [<TestMethod>]
    member this.TestPredicateInstance01 () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance without a predicate is not allowed
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01a () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01b () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01c () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                dec:;
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01d () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                dec:;
                spec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01e () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                dec:;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01f () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance2a () =
        let result = run property """optional pred X() 
	        {
                // a predicate instance with a return statement 
                dec ~a:obj;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2b () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance with a return statement 
                dec ~a:obj;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2c () =
        let result = run property """optional pred X() 
	        {
                // a predicate instance with a return statement 
                dec ~a:obj;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2d () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance with a return statement 
                dec ~a:obj;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2e () =
        let result = run property """optional pred X() 
	        {
                // a predicate instance with a return statement 
                dec ~a:obj;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2f () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance with a return statement 
                dec ~a:obj;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance3 () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance with a return statement and 
                // some other content following it is not allowed
                true
                // except comments
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance3a () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance with a return statement and 
                // some other content following it is not allowed
                true
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance3b () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance with a return statement and 
                // some other content following it is not allowed
                true
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance4 () =
        let result = run property """mand pred X() 
	        {
                // a predicate instance can be intrinsic
                intr

	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))