namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestDefinitionFunctionalTerms01 () =

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm01 () =
        let result = run definitionFunctionalTerm """func LeftNeutralElement() -> tplSetElem
        {
            dec:e: tplSetElem;
            spec:
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
        let result = run definitionFunctionalTerm """func Succ(n: Nat) -> Nat
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm03 () =
        let result = run definitionFunctionalTerm """func Add(n,m: @Digits)->Nat
        {
            return delegate.add(n,m)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm04 () =
        let result = run definitionFunctionalTerm """func Sum(list:* Nat)->Nat
        {
            dec: 
                result, addend: Nat
            ;
            spec:
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
        let result = run definitionFunctionalTerm """func Sum(list:* Nat)->Nat
        {
            dec: 
                i: index
            ;
            spec:
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
        let result = run definitionFunctionalTerm """func Sum(from,to:Nat, arr: Nat[from~to]) -> Nat
        {
            dec:
                i, result: Nat
            ;
            spec:
                result:=Zero()
                for  i in[from~to]
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
        let result = run definitionFunctionalTerm """func Sum(arr: Nat[~]) -> Nat
        {
            dec:
                addend, result: Nat
            ;
            spec:
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
        let result = run definitionFunctionalTerm """func Addend(a: Nat)->Nat
        {
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm09 () =
        let result = run definitionFunctionalTerm """func PowerSet(x: Set) -> Set
        {
            dec:
                y: Set
                ;
            spec:
                assert IsPowerSet(x,y)
                ;
            return y
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm10 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term cannot be empty
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm10a () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term cannot be empty with dec
            dec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm10b () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term cannot be empty with spec
            spec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm10c () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term cannot be empty with some spec or dec
            spec:;
            dec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm11 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term cannot be intrinsic with some proceeding spec or dec
            spec:;
            dec:;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm11a () =
        let result = run definitionFunctionalTerm """func T() -> obj
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
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term cannot be intrinsic with some proceeding spec or dec
            spec:;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))   
        
    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term can be intrinsic 
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12a () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term can be intrinsic with some following comments
            intrinsic
            // a comment
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12b () =
        let result = run definitionFunctionalTerm """func T() -> obj
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
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec:;
            spec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm12d () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term cannot be intrinsic with some following declarations or specifications
            intrinsic
            spec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm13 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term can be intrinsic with some following properties 
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

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm14 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            mand func T() -> obj
	        {
	            dec:;
                spec:;
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
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term with some proceeding declarations or specifications
            dec:;
            spec:;
            return x
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm16 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term with some proceeding declarations or specifications
            dec:;
            return x
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm17 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term with some proceeding declarations or specifications
            spec:;
            return x
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm18 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term 
            return x
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestDefinitionFunctionalTerm19 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // a functional term with some succeeding properties
            return x 


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
    member this.TestDefinitionFunctionalTerm20 () =
        let result = run definitionFunctionalTerm """func T() -> obj
        {
            // properties cannot succeed a return statement within a functional term definition
            optional pred T() 
	        {
                true
	        } 


            return x 
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

