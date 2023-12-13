namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestConstructors () =

    [<TestMethod>]
    member this.TestConstructor00 () =
        // empty constructors not allowed
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor00a () =
        // constructors are not allowed to be empty and only contain declarations
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                
                dec: x: obj ;
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor00b () =
        // constructors are not allowed to be empty and only contain specifications
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                
                dec x:= 1 ;
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor01 () =
        // constructors are not allowed to be intrinsic
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                
                intr
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor01a () =
        // constructors are not allowed to be intrinsic
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                
                dec:;
                intr
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor01b () =
        // constructors are not allowed to be intrinsic
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                
                dec ~a:obj ;
                intr
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor01c () =
        // constructors are not allowed to be intrinsic
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                
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
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj  base.AlgebraicStructure(x,op);
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
        let result = run (constructor .>> eof) """constructor Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj
                    base.AlgebraicStructure(x,op)
                ;
                self
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConstructor03a () =
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj ;
                self 
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestConstructor03b () =
        // incomplete (syntax error)
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj 
                    base. 
                ;
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor04 () =
        // incomplete ( self missing)
        let result = run (constructor .>> eof) """ctor Magma(x: tplSet, op: BinOp)
            {
                dec ~a:obj
                base.obj()
                ;
                
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor05 () =
        // calls to multiple parental constructors possible (multi-inheritance)
        // incomplete ( self missing)
        let result = run (constructor .>> eof) """Magma(x: tplSet, op: BinOp)
            {
                
                dec 
                    ~a:obj
                    base.obj () 
                    base.T1(x) 
                    base.T2(op) 
                ;
                
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestConstructor06 () =
        let result = run (constructor .>> eof) """ctor FieldPowerN
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
    				base.obj ()
                    self:=SetBuilder( myField[1 , n], true)
                ;
                self
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

