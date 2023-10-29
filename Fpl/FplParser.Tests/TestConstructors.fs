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
                // empty constructors not allowed
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
                dec x:= 1 ;
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
                dec ~a:obj ;
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
                dec ~a:obj ;
                intr
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor02 () =
        // constructors must contain the call(s) of the parent constructors
        // of all parent classes of the class; the call's syntax starts
        // with self followed by an exclamation mark and name of the classes constructor
        let result = run constructor """ctor Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj  self!AlgebraicStructure(x,op);
                self
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConstructor03 () =
        // constructors must contain the call(s) of the parent constructors
        // of all parent classes of the class; the call's syntax starts
        // with self followed by an exclamation mark and name of the classes constructor
        let result = run constructor """constructor Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj
                    self!AlgebraicStructure(x,op)
                ;
                self
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConstructor03a () =
        let result = run constructor """ctor Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj;
                self 
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConstructor03b () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj 
                    self! // incomplete (syntax error)
                ;
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor04 () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj
                self!obj()
                ;
                // incomplete ( self missing)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor05 () =
        let result = run constructor """Magma(x: tplSet, op: BinOp)
            {
                // calls to multiple parental constructors possible (multi-inheritance)
                dec 
                    ~a:obj
                    self!obj() 
                    self!T1(x) 
                    self!T2(op) 
                ;
                // incomplete ( self missing)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor06 () =
        let result = run constructor """ctor FieldPowerN
            (
                field : Field,
                n: Nat
            )
            {
    			dec ~a:obj
                    myField := field
                    addInField := myField.AddOp()
                    mulInField := myField.MulOp()
                    assert NotEqual(n, Zero())
                    self:=SetBuilder( myField[1 ~ n], true)
    				self!obj()
                ;
                self
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

