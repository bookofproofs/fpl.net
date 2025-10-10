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
        let result = run (ruleOfInference .>> eof) """inf ModusPonens
        {
            dec ~a:Obj ~p,q: pred;

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
        let result = run (ruleOfInference .>> eof) """inference ModusTollens
        {
            dec ~a:Obj ~p,q: pred;

            premise:
                and (not (q), impl(p,q) )
            conclusion:
                not (p)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule03 () =
        let result = run (ruleOfInference .>> eof) """inf HypotheticalSyllogism
        {
            dec ~a:Obj ~ p,q,r: pred;
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
        let result = run (ruleOfInference .>> eof) """inference DisjunctiveSyllogism
        {
            dec ~a:Obj ~p,q: pred;
            premise:
                and (not (p), or(p,q))
            conclusion:
                q
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule05 () =
        let result = run (ruleOfInference .>> eof) """inf ProceedingResults2
        {
            dec ~a,b: pred;
            premise: a, b
            conclusion: and (a,b)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule05a () =
        let result = run (ruleOfInference .>> eof) """inference ProceedingResults3
        {
            dec ~a,b,c: pred;
            premise: a,b,c
            conclusion: and(and(a,b),c)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule06 () =
        let result = run (ruleOfInference .>> eof) """inf ExistsByExample
        {
            dec ~p:pred(c:Obj);
            premise:
                p(c)
            conclusion:
                ex x:Obj {p(x)}
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule07 () =
        let result = run (ruleOfInference .>> eof) """inf TestRuleOfInference
        {
            premise:true
            conclusion:true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVarsInReferenceRule() =
        let result = run (ruleOfInference .>> eof) """inf ExistsByExample {dec ~c: Obj; pre: true con: true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule08() =
        let result = run (ruleOfInference .>> eof) """inf ProceedingResults {dec ~a,b: pred; pre: a, b con: and(a,b)}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        