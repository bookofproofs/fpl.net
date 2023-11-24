namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestDefinitionFunctionalTerms01 () =

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm01 () =
        let result = run (definitionFunctionalTerm .>> eof) """func LeftNeutralElement() -> tplSetElem
        {
            dec ~a:obj ~ e: tplSetElem

            assert 
                ex e
                (
                    IsLeftNeutralElement(e)
                );
            return e
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm02 () =
        let result = run (definitionFunctionalTerm .>> eof) """func Succ(n: Nat) -> Nat
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm03 () =
        let result = run (definitionFunctionalTerm .>> eof) """func Add(n,m: @Digits)->Nat
        {
            return delegate.add(n,m)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm04 () =
        let result = run (definitionFunctionalTerm .>> eof) """func Sum(list:* Nat)->Nat
        {
            dec ~a:obj ~  
                result, addend: Nat
                result:=Zero()
                for addend in list
                (
                    result:=Add(result,addend)
                )
            ;
            return result
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm05 () =
        let result = run (definitionFunctionalTerm .>> eof) """func Sum(list:* Nat)->Nat
        {
            dec ~a:obj ~  
                i: index
                result:=Zero()
                for i in list
                (
                    result:=Add(result,list!i)
                )
            ;
            return result
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm06 () =
        let result = run (definitionFunctionalTerm .>> eof) """func Sum(from,to:Nat, arr: Nat[from,to]) -> Nat
        {
            dec ~a:obj ~ 
                i, result: Nat
            
                result:=Zero()
                for  i in [from,to]
                (
                    result:=Add(result,arr<i>)
                )
            ;
            return result
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm07 () =
        let result = run (definitionFunctionalTerm .>> eof) """func Sum(arr: Nat[,]) -> Nat
        {
            dec ~a:obj ~ 
                addend, result: Nat
            
                result:=Zero()
                for addend in arr
                (
                    result:=Add(result,addend)
                )
            ;
            return result
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm08 () =
        let result = run (definitionFunctionalTerm .>> eof) """func Addend(a: Nat)->Nat
        {
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm09 () =
        let result = run (definitionFunctionalTerm .>> eof) """func PowerSet(x: Set) -> Set
        {
            dec ~a:obj ~ 
                y: Set
            
                assert IsPowerSet(x,y)
                ;
            return y
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm10 () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be empty
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm10a () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be empty with dec
            dec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm10b () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be empty with spec
            dec ~a:obj;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm10c () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be empty with some spec or dec
            dec ~a:obj;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm11 () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be intrinsic with some proceeding spec or dec
            dec ~a:obj;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm11a () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be intrinsic with some proceeding spec or dec
            dec:;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm11b () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be intrinsic with some proceeding spec or dec
            dec ~a:obj;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))   
        
    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12 () =
        // a functional term can be intrinsic 
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12a () =
        // a functional term can be intrinsic 
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12b () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12c () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec ~a:obj;
            spec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12d () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            // a functional term cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec ~a:obj;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm13 () =
        // a functional term can be intrinsic with some following properties 
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
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

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm14 () =
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            mand func T() -> obj
	        {
	            dec ~a:obj;
                return x
	        } 

            // a functional term cannot be intrinsic with some proceeding properties 
            intrinsic


            optional pred T()
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       


    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm15 () =
        // a functional term with some proceeding declarations or specifications
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            dec ~a:obj;
            return x
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm16 () =
        // a functional term with some proceeding declarations or specifications
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            dec ~a:obj;
            return x
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm17 () =
        // a functional term with some proceeding declarations or specifications
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            dec ~a:obj;
            return x
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm18 () =
        // a functional term 
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            return x
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm19 () =
        // a functional term with some succeeding properties
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            return x 


            property func T() -> obj
	        {
	            dec ~a:obj ;
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
    member this.TestDefinitionFunctionalTerm20 () =
        // properties cannot succeed a return statement within a functional term definition
        let result = run (definitionFunctionalTerm .>> eof) """func T() -> obj
        {
            optional pred T() 
	        {
                true
	        } 


            return x 
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

