namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestConstructors () =

    [<TestMethod>]
    member this.TestConstructor00 () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors are not allowed to be empty
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor00a () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors are not allowed to be empty and only contain declarations
                dec: x: obj ;
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor00b () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors are not allowed to be empty and only contain specifications
                spec: x:= 1 ;
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor01 () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors are not allowed to be intrinsic
                intr
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor01a () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors are not allowed to be intrinsic
                dec:;
                intr
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor01b () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors are not allowed to be intrinsic
                spec:;
                intr
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor01c () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors are not allowed to be intrinsic
                dec:;
                spec:;
                intr
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor02 () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors must contain the call(s) of the parent constructors
                // of all parent classes of the class; the call's syntax starts
                // with self followed by a dotted name of the classes constructor
                self.AlgebraicStructure(x,op)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConstructor03 () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // constructors must begin with the call of the parent constructor
                self.AlgebraicStructure(x,op)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
