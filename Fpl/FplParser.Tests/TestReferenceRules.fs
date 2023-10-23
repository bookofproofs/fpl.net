namespace FplParser.Tests

open FParsec
open FplParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestReferenceRules() =
    let replaceWhiteSpace (input: string) =
        let whiteSpaceChars = [|' '; '\t'; '\n'; '\r'|]
        input.Split(whiteSpaceChars)
            |> String.concat ""

    [<TestMethod>]
    member this.TestReferenceRule01 () =
        let result = run ruleOfInference """ModusPonens()
        {
            dec: p,q: pred;

            premise:
                and (p, impl (p,q) )
            conclusion:
                q
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule02 () =
        let result = run ruleOfInference """ModusTollens()
        {
            dec: p,q: pred;

            premise:
                and (not(q), impl(p,q) )
            conclusion:
                not (p)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule03 () =
        let result = run ruleOfInference """HypotheticalSyllogism()
        {
            dec: p,q,r: pred;
            premise:
                and (impl(p,q), impl(q,r))
            conclusion:
                impl(p,r)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule04 () =
        let result = run ruleOfInference """DisjunctiveSyllogism()
        {
            dec: p,q: pred;
            premise:
                and (not(p), or(p,q))
            conclusion:
                q
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule05 () =
        let result = run ruleOfInference """ProceedingResults(p:+ pred)
        {
            dec: proceedingResult: pred;
            spec:
                for proceedingResult in p
                (
                    assert proceedingResult
                )
            ;
            premise: undefined
            conclusion: and (p)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule05a () =
        let result = run ruleOfInference """ProceedingResults(p:+ pred)
        {
            dec: proceedingResult: pred;
            premise: all proceedingResult in p ( proceedingResult )
            conclusion: and (p)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule06 () =
        let result = run ruleOfInference """ExistsByExample(p: pred(c: obj))
        {
            dec: x: obj;
            premise:
                p(c)
            conclusion:
                ex x(p(x))
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
