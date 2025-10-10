namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestFunctionalTermProperties () =

    [<TestMethod>]
    member this.TestFunctionalTermInstance01 () =
        let result = run (definitionProperty .>> eof) """prty func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01a () =
        let result = run (definitionProperty .>> eof) """prty func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01b () =
        let result = run (definitionProperty .>> eof) """prty func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec ~a:obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01c () =
        let result = run (definitionProperty .>> eof) """prty func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec:;
                spec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01d () =
        let result = run (definitionProperty .>> eof) """prty func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec ~a:obj ;
                x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01e () =
        let result = run (definitionProperty .>> eof) """prty func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec:;
                x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01f () =
        // a function term instance without a return statement is not allowed
        let result = run (definitionProperty .>> eof) """prty func X() -> Y
	        {
                x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2a () =
        // a function term instance with a return statement 
        let result = run (definitionProperty .>> eof) """property func X() -> Y
	        {
                dec ~a:obj ;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2b () =
        // a function term instance with a return statement 
        let result = run (definitionProperty .>> eof) """property func X() -> Y
	        {
                dec ~a:obj ;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2c () =
        // a function term instance with a return statement 
        let result = run (definitionProperty .>> eof) """property func X() -> Y
	        {
                dec ~a:obj ;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2d () =
        // a function term instance with a return statement 
        let result = run (definitionProperty .>> eof) """property func X() -> Y
	        {
                dec ~a:obj ;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2e () =
        // a function term instance with a return statement 
        let result = run (definitionProperty .>> eof) """property func X() -> Y
	        {
                dec ~a:obj ;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2f () =
        // a function term instance with a return statement 
        let result = run (definitionProperty .>> eof) """property func X() -> Y
	        {
                dec ~a:obj ;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance3 () =
        // a function term instance with a return statement and 
        // some other content following it is not allowed
        let result = run (definitionProperty .>> eof) """property func X() -> Y
	        {
                return x
                dec ~a:obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance3a () =
        // a function term instance with a return statement and 
        // some other content following it is not allowed
        let result = run (definitionProperty .>> eof) """func X() -> Y
	        {
                return x
                dec ~a:obj ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance3b () =
        // a function term instance with a return statement and 
        // some other content following it is not allowed
        let result = run (definitionProperty .>> eof) """prty func X() -> Y
	        {
                return x
                dec a:ind;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance4 () =
        let result = run (definitionProperty .>> eof) """prty func VecAdd(from,to: Nat, v,w: tplFieldElem[from ~ to]) -> tplFieldElem[from ~ to]
	        {
			    dec ~a:obj
    	            self[from ~ to]:=addInField(v[from ~ to],w[from ~ to])
                ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))