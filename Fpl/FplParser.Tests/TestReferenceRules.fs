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
        let result = run (ruleOfInference .>> eof) """inf ModusPonens()
        {
            dec ~a:obj ~p,q: pred;

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
        let result = run (ruleOfInference .>> eof) """inference ModusTollens()
        {
            dec ~a:obj ~p,q: pred;

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
        let result = run (ruleOfInference .>> eof) """inf HypotheticalSyllogism()
        {
            dec ~a:obj ~ p,q,r: pred;
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
        let result = run (ruleOfInference .>> eof) """inference DisjunctiveSyllogism()
        {
            dec ~a:obj ~p,q: pred;
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
        let result = run (ruleOfInference .>> eof) """inf ProceedingResults(p:+ pred)
        {
            dec ~a:obj ~ proceedingResult: pred

                for proceedingResult in p
                {
                    assert proceedingResult
                }
            ;
            premise: undefined
            conclusion: and (p)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule05a () =
        let result = run (ruleOfInference .>> eof) """inference ProceedingResults(p:+ pred)
        {
            dec ~a:obj ~res,proceedingResult: pred res:=true for proceedingResult in p { res:=and(res, proceedingResult) } ;
            premise: res
            conclusion: and (p)
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule06 () =
        let result = run (ruleOfInference .>> eof) """inf ExistsByExample(p: pred(c: obj))
        {
            dec ~a:obj ~ x: obj ;
            premise:
                p(c)
            conclusion:
                ex x{p(x)}
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestReferenceRule07 () =
        let result = run (ruleOfInference .>> eof) """inf TestRuleOfInference()
        {
            premise:true
            conclusion:true
        }"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<TestMethod>]
    member this.TestVarsInReferenceRule() =
        let result = run (ruleOfInference .>> eof) """inf ExistsByExample(p: pred(c: obj)) {dec ~c: obj; pre: true con: true}"""
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))
        