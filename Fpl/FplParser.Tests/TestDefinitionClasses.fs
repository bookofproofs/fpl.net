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
        let result = run definitionClass """class FieldPowerN: Set
        {
            // if empty then intrinsic
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
            spec:;
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
            dec:;
            spec:;
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass02 () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor
            dec:;
            spec:;
            FieldPowerN() 
            {
                self!obj()
                self
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass02a () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor and a self reference
            dec:;
            FieldPowerN() 
            {
                self!obj()
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
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor
            spec:;
            FieldPowerN() 
            {
                self!obj()
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
            spec:;
            FieldPowerN() 
            {
                self!obj()
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass02c () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor 
            FieldPowerN() 
            {
                self!obj()
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
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with more than one constructor
            FieldPowerN() 
            {
                self!obj()
                self
            }
            FieldPowerN() 
            {
                self!T1()
                self
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass03a () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with more than one constructor and some properties
            FieldPowerN() 
            {
                self!obj()
                self
            }

            FieldPowerN() 
            {
                self!T1()
                self
            }

            mand func T() -> obj
	        {
	            dec:;
                spec:;
                return x
	        } 

            optional pred T() 
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
                self.obj()
            }

            optional pred T() 
	        {
                true
	        } 

            FieldPowerN() 
            {
                self.T1()
            }

            mand func T() -> obj
	        {
	            dec:;
                spec:;
                return x
	        } 


        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass04a () =
        let result = run definitionClass """class FieldPowerN: Set
        {
            // A class with a constructor and a property 

            FieldPowerN() 
            {
                self!T1()
                self
            }

            optional pred T() 
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
                self.T1()
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestClass05 () =
        let result = run definitionClass """class FieldPowerN: Typ1, :* Typ2, :+ Typ3 
        {
            // A class with multiple inheritance
            intrinsic

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass06 () =
        let result = run definitionClass """class FieldPowerN: Typ1
        {
            // An intrinsic class with a following comment
            intrinsic
            // comment
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestClass07 () =
        let result = run definitionClass """class FieldPowerN: Typ1
        {
            // An intrinsic class some following properties
            intrinsic

            mand func T() -> obj
	        {
	            dec:;
                spec:;
                return x
	        } 

            optional pred T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       