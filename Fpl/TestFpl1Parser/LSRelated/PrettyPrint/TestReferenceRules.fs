namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestReferenceRules () =

    [<DataRow("01", """inf ModusPonens { dec p,q: pred; premise: and (p, impl (p,q) ) conclusion: q }""")>]
    [<DataRow("02", """inference ModusTollens { dec a:obj p,q: pred; premise: and (not (q), impl(p,q) ) conclusion: not (p) }""")>]
    [<DataRow("03", """inf HypotheticalSyllogism { dec a:obj  p,q,r: pred; premise: and (impl(p,q), impl(q,r)) conclusion: impl(p,r) }""")>]
    [<DataRow("04", """inference DisjunctiveSyllogism { dec a:obj p,q: pred; premise: and (not (p), or(p,q)) conclusion: q }""")>]
    [<DataRow("05", """inf PrecedingResults2 { dec a,b: pred; premise: a, b conclusion: and (a,b) }""")>]
    [<DataRow("06", """inference PrecedingResults3 { dec a,b,c: pred; premise: a,b,c conclusion: and(and(a,b),c) }""")>]
    [<DataRow("07", """inf ExistsByExample { dec p:pred(c:obj); premise: p(c) conclusion: ex x:obj {p(x)} }""")>]
    [<DataRow("08", """inf TestRuleOfInference { premise:true conclusion:true }""")>]
    [<DataRow("09", """inf ExistsByExample {dec c: obj; pre: true con: true}""")>]
    [<DataRow("10", """inf PrecedingResults {dec a,b: pred; pre: a, b con: and(a,b)}""")>]
    [<TestMethod>]
    member _.TestRuleOfInferenceSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments ruleOfInference fplCode

