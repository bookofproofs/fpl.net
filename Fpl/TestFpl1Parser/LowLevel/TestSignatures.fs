namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestSignatures () =

    [<DataRow("01", """pred AreRelated(u,v: Set, r: BinaryRelation)""")>]
    [<DataRow("02", """pred IsSubset(subset,superset: Set) infix "∈" 2""")>]
    [<TestMethod>]
    member this.TestPredicateSignatureSuccess (no:string, fplCode:string) =
        let result = run (predicateSignature .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """inf ExistsByExample""")>]
    [<DataRow("02", """inference PrecedingResults""")>]
    [<TestMethod>]
    member this.TestRuleOfInferenceSignatureSuccess (no:string, fplCode:string) =
        let result = run (ruleOfInferenceSignature .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """ctor Zero()""")>]
    [<DataRow("02", """constructor SetRoster(listOfSets:* Set[obj])""")>]
    [<DataRow("03", """ctor Nat(x: Decimal)""")>]
    [<DataRow("04", """ctor AlgebraicStructure(x: tplSet, ops:* func(args:* tplSetElem[ind,obj])->tplSetElem[ind] )""")>]
    [<DataRow("05", """ctor AlgebraicStructure(x: tplSet, ops:* func(args:* tplSetElem[ind,obj])->*tplSetElem[ind][obj] )""")>]
    [<TestMethod>]
    member this.TestConstructorSignatureSuccess (no:string, fplCode:string) =
        let result = run (constructorSignature .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """pred Test(a,b: tpl)""")>]
    [<TestMethod>]
    member this.TestPredicateInstanceSignatureSuccess (no:string, fplCode:string) =
        let result = run (predicateInstanceSignature .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """func TestPredicate(a,b:obj)->obj""")>]
    [<DataRow("02", """func VecAdd(v,w: tplFieldElem) -> obj prefix "+" """)>]
    [<TestMethod>]
    member this.TestFunctionalTermSignatureSuccess (no:string, fplCode:string) =
        let result = run (functionalTermSignature .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """func BinOp(x,y: tplSetElem) -> tplSetElem""")>]
    [<DataRow("02", """func Add(n,m: Nat) -> pred""")>]
    [<TestMethod>]
    member this.TestFunctionalTermInstanceSignatureSuccess (no:string, fplCode:string) =
        let result = run (functionalTermInstanceSignature .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """cl ZeroVectorN""")>]
    [<TestMethod>]
    member this.TestClassSignatureSuccess (no:string, fplCode:string) =
        let result = run (classSignature .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))


