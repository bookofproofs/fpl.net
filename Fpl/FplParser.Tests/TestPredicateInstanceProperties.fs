namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestPredicateInstanceProperties () =

    [<TestMethod>]
    member this.TestPredicateInstance01 () =
        let result = run (definitionProperty .>> eof) """prty pred X() 
	        {
                // a predicate instance without a predicate is not allowed
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01a () =
        let result = run (definitionProperty .>> eof) """prty pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01b () =
        let result = run (definitionProperty .>> eof) """prty pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                dec ~a:Obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01c () =
        let result = run (definitionProperty .>> eof) """prty pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                dec ~a:Obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01d () =
        let result = run (definitionProperty .>> eof) """prty pred X() 
	        {
                // a predicate instance without a predicate is not allowed
                dec ~a:Obj ;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance01e () =
        let result = run (definitionProperty .>> eof) """prty pred X() 
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
        // a predicate instance without a predicate is not allowed
        let result = run (definitionProperty .>> eof) """prty pred X() 
	        {
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestPredicateInstance2a () =
        // a predicate instance with a return statement 
        let result = run (definitionProperty .>> eof) """property pred X() 
	        {
                dec ~a:Obj ;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2b () =
        // a predicate instance with a return statement 
        let result = run (definitionProperty .>> eof) """property pred X() 
	        {
                dec ~a:Obj ;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2c () =
        // a predicate instance with a return statement 
        let result = run (definitionProperty .>> eof) """property  pred X() 
	        {
                dec ~a:Obj ;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2d () =
        // a predicate instance with a return statement 
        let result = run (definitionProperty .>> eof) """property pred X() 
	        {
                dec ~a:Obj ;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2e () =
        // a predicate instance with a return statement 
        let result = run (definitionProperty .>> eof) """property pred X() 
	        {
                dec ~a:Obj ;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance2f () =
        // a predicate instance with a return statement 
        let result = run (definitionProperty .>> eof) """property pred X() 
	        {
                dec ~a:Obj ;
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance3 () =
        // a predicate instance with a return statement and 
        // some other content following it is not allowed
        let result = run (definitionProperty .>> eof) """property pred X() 
	        {
                true
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance3a () =
        // a predicate instance with a return statement and 
        // some other content following it is not allowed
        let result = run (definitionProperty .>> eof) """property pred X() 
	        {
                true
                dec ~a:Obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))


    [<TestMethod>]
    member this.TestPredicateInstance4 () =
        // a predicate instance can be intrinsic
        let result = run (definitionProperty .>> eof) """property pred X() 
	        {
                intr

	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestPredicateInstance5 () =
        let result = run (definitionProperty .>> eof) """prty pred T() {true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))