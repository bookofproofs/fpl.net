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
        let result = run (definitionPredicate .>> eof) """pred IsGreaterOrEqual(n,m: Nat)
        {
            ex k:Nat { Equal(n,Add(m,k)) }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate02 () =
        let result = run (definitionPredicate .>> eof) """pred IsBounded(x: Real)
        {
            ex upperBound, lowerBound:Real
            {
                and (LowerEqual(x,upperBound), LowerEqual(lowerBound,x))
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate03 () =
        let result = run (definitionPredicate .>> eof) """pred IsBounded(f: RealValuedFunction)
        {
            all x:Real
            {
                IsBounded(f(x))
            }
        }
"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate04 () =
        let result = run (definitionPredicate .>> eof) """pred Equal(a,b: tpl)
        {

			all p:pred
			{
				iif
				(
					p(a),
					p(b)
				)
			}
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestDefinitionPredicate05 () =
        let result = run (definitionPredicate .>> eof) """pred NotEqual(x,y: tpl)
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
        let result = run (definitionPredicate .>> eof) """pred AreRelated(u,v: Set, r: BinaryRelation)
        {
            dec ~a:obj
                ~tuple: Tuple[one:Nat,two:Nat]
                tuple:=Tuple(Nat(1),Nat(2))
                ;
            
            and
            (
                and (In(tuple,r), In(u,r.Domain())),
                In(v,r.Codomain())
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate07 () =
        let result = run (definitionPredicate .>> eof) """pred Greater(x,y: obj)
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate08 () =
        let result = run (definitionPredicate .>> eof) """pred IsPowerSet(ofSet, potentialPowerSet: Set)
        {
            all z:Set
            {
                impl (Subset(z,ofSet), In(z, potentialPowerSet))
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate09 () =
        let result = run (definitionPredicate .>> eof) """pred Union(x,superSet: Set)
        {
            all u:Set
            {
                impl (In(u, x), In(u, superSet))
            }
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate10 () =
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            // a predicate cannot be empty
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate10a () =
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            // a predicate cannot be empty with dec
            dec:;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate10b () =
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            // a predicate cannot be empty with spec
            dec ~a:obj ;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate10c () =
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            // a predicate cannot be empty with some spec or dec
            dec ~a:obj ;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate11 () =
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            // a predicate cannot be intrinsic with some proceeding spec or dec
            dec ~a:obj ;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))        

    [<TestMethod>]
    member this.TestDefinitionPredicate11a () =
        let result = run (definitionPredicate .>> eof) """pred T()
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
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            // a predicate cannot be intrinsic with some proceeding spec or dec
            dec ~a:obj ;
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))   
        
    [<TestMethod>]
    member this.TestDefinitionPredicate12 () =
        // a predicate can be intrinsic 
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            intrinsic
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate12a () =
        // a predicate can be intrinsic 
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            intr
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate12b () =
        let result = run (definitionPredicate .>> eof) """pred T()
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
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            // a predicate cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec ~a:obj ;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate12d () =
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            // a predicate cannot be intrinsic with some following declarations or specifications
            intrinsic
            dec ~a:obj ;
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate13 () =
        // a predicate can be intrinsic with some following properties 
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            intrinsic

            property func T() -> obj
	        {
	            dec ~a:obj ;
                return x
	        } 

            property pred opt T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))       

    [<TestMethod>]
    member this.TestDefinitionPredicate14 () =
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            mand func T() -> obj
	        {
	            dec ~a:obj ;
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
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            dec ~a:obj ;
            true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate16 () =
        let result = run (definitionPredicate .>> eof) """pred T()
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
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            dec ~a:obj ;
            true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate18 () =
        // a predicate 
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


    [<TestMethod>]
    member this.TestDefinitionPredicate19 () =
        // a predicate with some succeeding properties
        let result = run (definitionPredicate .>> eof) """pred T()
        {
            true 


            property func T() -> obj
	        {
	            dec ~a:obj ;
                return x
	        } 

            property  pred opt T() 
	        {
                true
	        } 

        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate20 () =
        let result = run (definitionPredicate .>> eof) """pred T()
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

    [<TestMethod>]
    member this.TestDefinitionPredicate21 () =
        let result = run (definitionPredicate .>> eof) """pred TestPredicate(a:T1, b:func, c:ind, d:pred) 
            {
                delegate.C(Test1(a),Test2(b,c,d))
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate22 () =
        let result = run (definitionPredicate .>> eof) """pred TestPredicate(a:T1, b:func, c:ind, d:pred) 
            {
                D(self,b,c)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
 
    [<TestMethod>]
    member this.TestDefinitionPredicate23 () =
        let result = run (definitionPredicate .>> eof) """pred TestPredicate(a:T1, b:func, c:ind, d:pred) 
            {
                true

                property pred T1() 
                {
                    delegate.B()
                }
                property pred T2() 
                {
                    delegate.C(a,b,c,d)
                }
                property pred T3() 
                {
                    delegate.D(self,b,c)
                }
                property pred T4() 
                {
                    delegate.B(In(x))
                }
                property pred T5() 
                {
                    delegate.C(Test1(a),Test2(b,c,d))
                }
                property pred T6() 
                {
                    delegate.E(true, undef, false)
                }

            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate24 () =
        let result = run (definitionPredicate .>> eof) """pred T() {dec ~cI2:C1 cI2:=C1($2); true }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate25 () =
        let result = run (definitionPredicate .>> eof) """pred T() { $1 }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate26 () =
        let result = run (definitionPredicate .>> eof) """pred T() {dec ~dI1:D dI1:=D; true }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    