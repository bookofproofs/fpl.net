namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestFunctionalTermProperties () =

    [<TestMethod>]
    member this.TestFunctionalTermInstance01 () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01a () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec:;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01b () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec ~a:obj;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01c () =
        let result = run property """mand func X() -> Y
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
        let result = run property """mand func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                dec ~a:obj;
                x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance01e () =
        let result = run property """mand func X() -> Y
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
        let result = run property """mand func X() -> Y
	        {
                // a function term instance without a return statement is not allowed
                x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2a () =
        let result = run property """optional func X() -> Y
	        {
                // a function term instance with a return statement 
                dec ~a:obj;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2b () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance with a return statement 
                dec ~a:obj;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2c () =
        let result = run property """optional func X() -> Y
	        {
                // a function term instance with a return statement 
                dec ~a:obj;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2d () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance with a return statement 
                dec ~a:obj;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2e () =
        let result = run property """optional func X() -> Y
	        {
                // a function term instance with a return statement 
                dec ~a:obj;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance2f () =
        let result = run property """mand func X() -> Y
	        {
                // a function term instance with a return statement 
                dec ~a:obj;
                return x
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance3 () =
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
    member this.TestFunctionalTermInstance3a () =
        let result = run property """func X() -> Y
	        {
                // a function term instance with a return statement and 
                // some other content following it is not allowed
                return x
                dec ~a:obj;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestFunctionalTermInstance3b () =
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

    [<TestMethod>]
    member this.TestFunctionalTermInstance4 () =
        let result = run property """mand func VecAdd(from,to: Nat, v,w: tplFieldElem[from ~ to]) -> tplFieldElem[from ~ to]
	        {
			    dec ~a:obj
    	            self[from ~ to]:=addInField(v[from ~ to],w[from ~ to])
                ;
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))