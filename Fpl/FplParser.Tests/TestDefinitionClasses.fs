namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClasses () =

    [<TestMethod>]
    member this.TestClass00 () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Obj
        {
        
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass01 () =
        // if empty then intrinsic
        let result = run (definitionClass .>> eof) """object FieldPowerN: Obj
        {
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestClass01a () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // intrinsic classes with declarations or specifications not allowed
            dec:;
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass01b () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // intrinsic classes with declarations or specifications not allowed
            dec ~a:Obj ;
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass01c () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // intrinsic classes with declarations or specifications not allowed
            dec ~d:Nat 
            d:=1
            ;
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass01d () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // intrinsic classes with declarations or specifications not allowed
            decs := x

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass02 () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            ctor FieldPowerN()
            {
                dec base.obj() ;
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02a () =
        // A object with a constructor and a self reference
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            dec ~x: Obj ;
            constructor FieldPowerN() 
            {
                dec base.obj() ;
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02a0 () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // A object with a constructor and a self reference but without self
            dec:;
            FieldPowerN() 
            {
                base.obj()
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass02b () =
        // A object with a constructor
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            dec ~a:Obj ;
            ctor FieldPowerN() 
            {
                dec base.obj() ;
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02b0 () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // A object with a constructor but without self
            dec ~a:Obj ;
            FieldPowerN() 
            {
                dec base.obj() ;
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass02c () =
        // A object with a constructor 
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            ctor FieldPowerN() 
            {
                dec base.obj() ;
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02c0 () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // A object with a constructor but without self
            FieldPowerN() 
            {
                base.obj()
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass03 () =
        // A object with more than one constructor
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            ctor FieldPowerN() 
            {
                dec base.obj() ;
            }
            constructor FieldPowerN() 
            {
                dec base.T1() ;
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass03a () =
        // A object with more than one constructor and some properties
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            ctor FieldPowerN() 
            {
                dec base.obj() ;
            }

            ctor FieldPowerN() 
            {
                dec base.T1() ;
            }

            property func T() -> obj
	        {
	            dec ~a:Obj ;
                return x
	        } 

            property pred T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass03b () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // A object with more than one constructor and some properties
            FieldPowerN() 
            {
                dec 
                    ~a:Obj
                    base.obj()
                    ;
                self
            }

            optional pred T() 
	        {
                true
	        } 

            FieldPowerN() 
            {
                dec
                    base.T1()
                ;
                self
            }

            mand func T() -> obj
	        {
	            dec ~a:Obj ;
                return x
	        } 


        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass04a () =
        // A object with a constructor and a property 
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {

            ctor FieldPowerN() 
            {
                dec base.T1() ;
            }

            property pred T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass04b () =
        let result = run (definitionClass .>> eof) """object FieldPowerN: Set
        {
            // A object with a constructor and a property but constructors have to proceed properties

            optional pred T() 
	        {
                true
	        } 

            FieldPowerN() 
            {
                dec 
                    ~a:Obj
                    self.T1()
                ;
                self
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass05 () =
        // A object with multiple inheritance
        let result = run (definitionClass .>> eof) """object FieldPowerN: Typ1, Typ2, Typ3 
        {
            intrinsic

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass06 () =
        // An intrinsic object 
        let result = run (definitionClass .>> eof) """object FieldPowerN: Typ1
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass07 () =
        // An intrinsic object some following properties
        let result = run (definitionClass .>> eof) """object FieldPowerN: Typ1
        {
            intrinsic

            property func T() -> obj
	        {
	            dec ~a:Obj ;
                return x
	        } 

            property pred  T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestClass08 () =
        // An intrinsic object some following properties
        let result = run (definitionClass .>> eof) """object SomeClass:Nat1 ,Nat2Nat3,Nat3 
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestClass09 () =
        // An intrinsic object some following properties
        let result = run (definitionClass .>> eof) """object SomeClass :Nat1,Nat2, Nat3,Nat3 
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestClass10 () =
        let result = run (definitionClass .>> eof) """cl TestId:Obj 
        {
            ctor TestId() {} 
            ctor TestId(x:Obj) {} 
            ctor TestId(x:pred) {} 
            ctor TestId(x:ind) {} 
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

        