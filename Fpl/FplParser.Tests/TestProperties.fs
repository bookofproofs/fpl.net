namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestProperties () =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestProperty01 () =
        let result = run property """mand func VecAdd(from,to: Nat, v,w: tplFieldElem[from ~ to]) -> tplFieldElem[from ~ to]
	        {
	            spec: self[from ~ to] :=addInField(v[from ~ to],w[from ~ to])
                ;
                return self[from ~ to] 
	        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty02 () =
        let result = run property """mandatory Nat Length()
            {
                myLength
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty03 () =
        let result = run property """mandatory tpl Coord(i: Nat)
            {
                self(i)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty04 () =
        let result = run property """mandatory tplSetElem NeutralElem()
            {
                spec:
                self:=myOp.NeutralElement()
                ;
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty05 () =
        let result = run property """mandatory func InverseOf(x: tplSetElem) -> tplSetElem
            {
                dec:val: tplSetElem;
                spec:
                assert
                    and
                    (
                        Equal( myOp(x,val), self.NeutralElem()),
                        Equal( myOp(val,x), self.NeutralElem())
                    )
                    ;
                ret val
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty06 () =
        let result = run property """optional pred IsAssociative()
            {
                dec: a,b,c: tplSetElem;
                all a,b,c
                (
                    Equal
                    (
                        @self(a,@self(b,c)),
                        @self(@self(a,b),c)
                    )
                )
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty07 () =
        let result = run property """optional pred IsLeftNeutralElement(e: tplSetElem)
            {
                Equal(@self(e,x), x)
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty08 () =
        let result = run property """optional pred IsNeutralElement(e: tplSetElem)
            {
                and (IsLeftNeutralElement(e), IsRightNeutralElement(e))
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty09 () =
        let result = run property """optional pred HasLeftNeutralElement()
            {
                dec: e: tplSetElem;
                ex e
                (
                    IsLeftNeutralElement(e)
                )
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty10 () =
        let result = run property """optional pred HasNeutralElement()
            {
                dec: e: tplSetElem;
                ex e
                (
                    IsNeutralElement(e)
                )
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestProperty11 () =
        let result = run property """optional func LeftNeutralElement() -> tplSetElem
            {
                dec: e: tplSetElem;
                spec:
                assert 
                    ex e
                    (
                        IsLeftNeutralElement(e)
                    )
                ;
                return e
            }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
