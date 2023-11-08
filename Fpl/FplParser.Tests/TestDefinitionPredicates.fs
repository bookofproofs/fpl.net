namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestDefinitionPredicates () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestDefinitionPredicate01 () =
        let result = run definitionPredicate """pred IsGreaterOrEqual(n,m: Nat)
        {
            dec ~a:obj ~  k: Nat;
            ex k ( Equal(n,Add(m,k)) )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate02 () =
        let result = run definitionPredicate """pred IsBounded(x: Real)
        {
            dec ~a:obj ~  upperBound, lowerBound: Real;
            ex upperBound, lowerBound
            (
                and (LowerEqual(x,upperBound), LowerEqual(lowerBound,x))
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate03 () =
        let result = run definitionPredicate """pred IsBounded(f: RealValuedFunction)
        {
            dec ~a:obj ~  x: Real;
            all x
            (
                IsBounded(f(x))
            )
        }
"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate04 () =
        let result = run definitionPredicate """pred Equal(a,b: tpl)
        {
            dec ~a:obj ~  p: pred;

			all p
			(
				iif
				(
					p(a),
					p(b)
				)
			)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestDefinitionPredicate05 () =
        let result = run definitionPredicate """pred NotEqual(x,y: tpl)
        {
            not
            (
                Equal(x,y)
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate06 () =
        let result = run definitionPredicate """pred AreRelated(u,v: Set, r: BinaryRelation)
        {
            dec ~a:obj ~  
                one, two:Nat
                one := Nat(1)
                two := Nat(2)
                ~tuple: Tuple[one,two]
                tuple:=Tuple(u,v)
                ;
            
            and
            (
                In(tuple,r),
                In(u,r.Domain()),
                In(v,r.Codomain())
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate07 () =
        let result = run definitionPredicate """pred Greater(x,y: obj)
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate08 () =
        let result = run definitionPredicate """pred IsPowerSet(ofSet, potentialPowerSet: Set)
        {
            dec ~a:obj ~  z: Set;
            all z
            (
                impl (Subset(z,ofSet), In(z, potentialPowerSet))
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate09 () =
        let result = run definitionPredicate """pred Union(x,superSet: Set)
        {
            dec ~a:obj ~  u: Set;
            all u
            (
                impl (In(u, x), In(u, superSet))
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate10 () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be empty
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate10a () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be empty with dec
            dec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate10b () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be empty with spec
            dec ~a:obj;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate10c () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be empty with some spec or dec
            dec ~a:obj;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate11 () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be intrinsic with some proceeding spec or dec
            dec ~a:obj;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate11a () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be intrinsic with some proceeding spec or dec
            dec:;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate11b () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be intrinsic with some proceeding spec or dec
            dec ~a:obj;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))   
        
    [<TestMethod>]
    member this.TestDefinitionPredicate12 () =
        // a predicate can be intrinsic 
        let result = run definitionPredicate """pred T()
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate12a () =
        // a predicate can be intrinsic 
        let result = run definitionPredicate """pred T()
        {
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate12b () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate12c () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec ~a:obj;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate12d () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec ~a:obj;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate13 () =
        // a predicate can be intrinsic with some following properties 
        let result = run definitionPredicate """pred T()
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
    member this.TestDefinitionPredicate14 () =
        let result = run definitionPredicate """pred T()
        {
            mand func T() -> obj
	        {
	            dec ~a:obj;
                return x
	        } 

            // a predicate cannot be intrinsic with some proceeding properties 
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
    member this.TestDefinitionPredicate15 () =
        // a predicate with some proceeding declarations or specifications
        let result = run definitionPredicate """pred T()
        {
            dec ~a:obj;
            true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate16 () =
        let result = run definitionPredicate """pred T()
        {
            // a predicate with some proceeding declarations or specifications
            dec ~a:obj ~ ;
            true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate17 () =
        // a predicate with some proceeding declarations or specifications
        let result = run definitionPredicate """pred T()
        {
            dec ~a:obj;
            true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate18 () =
        // a predicate 
        let result = run definitionPredicate """pred T()
        {
            true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestDefinitionPredicate19 () =
        // a predicate with some succeeding properties
        let result = run definitionPredicate """pred T()
        {
            true 


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
    member this.TestDefinitionPredicate20 () =
        let result = run definitionPredicate """pred T()
        {
            // properties cannot succeed a predicate within a predicate definition
            optional pred T() 
	        {
                true
	        } 


            true 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

