namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClasses () =

    [<TestMethod>]
    member this.TestClass00 () =
        let result = run definitionClass """class FieldPowerN: Set
        {
        
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass01 () =
        // if empty then intrinsic
        let result = run definitionClass """class FieldPowerN: Set
        {
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestClass01a () =
        let result = run definitionClass """class FieldPowerN: Set
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
        let result = run definitionClass """class FieldPowerN: Set
        {
            // intrinsic classes with declarations or specifications not allowed
            dec ~a:obj;
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass01c () =
        let result = run definitionClass """class FieldPowerN: Set
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
        let result = run definitionClass """class FieldPowerN: Set
        {
            // intrinsic classes with declarations or specifications not allowed
            decs := x

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass02 () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            ctor FieldPowerN()
            {
                dec self!obj() ;
                self
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02a () =
        // A class with a constructor and a self reference
        let result = run definitionClass """class FieldPowerN: Set
        {
            dec ~x: obj;
            constructor FieldPowerN() 
            {
                dec self!obj() ;
                self
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02a0 () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor and a self reference but without self
            dec:;
            FieldPowerN() 
            {
                self!obj()
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass02b () =
        // A class with a constructor
        let result = run definitionClass """class FieldPowerN: Set
        {
            dec ~a:obj;
            ctor FieldPowerN() 
            {
                dec self!obj() ;
                self
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02b0 () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor but without self
            dec ~a:obj;
            FieldPowerN() 
            {
                dec self!obj() ;
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass02c () =
        // A class with a constructor 
        let result = run definitionClass """class FieldPowerN: Set
        {
            ctor FieldPowerN() 
            {
                dec self!obj() ;
                self
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02c0 () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor but without self
            FieldPowerN() 
            {
                self!obj()
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass03 () =
        // A class with more than one constructor
        let result = run definitionClass """class FieldPowerN: Set
        {
            ctor FieldPowerN() 
            {
                dec self!obj() ;
                self
            }
            constructor FieldPowerN() 
            {
                dec self!T1() ;
                self
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass03a () =
        // A class with more than one constructor and some properties
        let result = run definitionClass """class FieldPowerN: Set
        {
            ctor FieldPowerN() 
            {
                dec self!obj() ;
                self
            }

            ctor FieldPowerN() 
            {
                dec self!T1() ;
                self
            }

            property func T() -> obj
	        {
	            dec ~a:obj;
                return x
	        } 

            property optional pred T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass03b () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with more than one constructor and some properties
            FieldPowerN() 
            {
                dec 
                    ~a:obj
                    self.obj()
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
                    self.T1()
                ;
                self
            }

            mand func T() -> obj
	        {
	            dec ~a:obj;
                return x
	        } 


        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass04a () =
        // A class with a constructor and a property 
        let result = run definitionClass """class FieldPowerN: Set
        {

            ctor FieldPowerN() 
            {
                dec self!T1() ;
                self
            }

            property optional pred T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass04b () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor and a property but constructors have to proceed properties

            optional pred T() 
	        {
                true
	        } 

            FieldPowerN() 
            {
                dec 
                    ~a:obj
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
        // A class with multiple inheritance
        let result = run definitionClass """class FieldPowerN: Typ1, :* Typ2, :+ Typ3 
        {
            intrinsic

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass06 () =
        // An intrinsic class 
        let result = run definitionClass """class FieldPowerN: Typ1
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass07 () =
        // An intrinsic class some following properties
        let result = run definitionClass """class FieldPowerN: Typ1
        {
            intrinsic

            property func T() -> obj
	        {
	            dec ~a:obj;
                return x
	        } 

            property optional pred T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       