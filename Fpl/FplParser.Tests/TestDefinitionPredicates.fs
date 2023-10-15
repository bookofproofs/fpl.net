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
            dec: k: Nat;
            ex k ( Equal(n,Add(m,k)) )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestDefinitionPredicate02 () =
        let result = run definitionPredicate """pred IsBounded(x: Real)
        {
            dec: upperBound, lowerBound: Real;
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
            dec: x: Real;
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
            dec: p: pred;

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
            dec: 
                one, two:Nat
                ;
            spec:
                one := Nat(1)
                two := Nat(2)
                ;
            dec: 
                tuple: Tuple[one~two]
                ;
            spec:
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
            dec: z: Set;
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
            dec: u: Set;
            all u
            (
                impl (In(u, x), In(u, superSet))
            )
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

        