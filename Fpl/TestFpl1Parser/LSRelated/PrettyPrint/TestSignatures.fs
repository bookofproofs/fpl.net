namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestSignatures () =

    [<DataRow("01", """pred AreRelated(u,v: Set, r: BinaryRelation)""")>]
    [<DataRow("02", """pred IsSubset(subset,superset: Set) infix "∈" 2""")>]
    [<TestMethod>]
    member _.TestPredicateSignatureSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments predicateSignature fplCode

    [<DataRow("01", """inf ExistsByExample""")>]
    [<DataRow("02", """inference PrecedingResults""")>]
    [<TestMethod>]
    member _.TestRuleOfInferenceSignatureSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments ruleOfInferenceSignature fplCode

    [<DataRow("01", """ctor Zero()""")>]
    [<DataRow("02", """constructor SetRoster(listOfSets:* Set[obj])""")>]
    [<DataRow("03", """ctor Nat(x: Decimal)""")>]
    [<DataRow("04", """ctor AlgebraicStructure(x: tplSet, ops:* func(args:* tplSetElem[ind,obj])->tplSetElem[ind] )""")>]
    [<DataRow("05", """ctor AlgebraicStructure(x: tplSet, ops:* func(args:* tplSetElem[ind,obj])->*tplSetElem[ind][obj] )""")>]
    [<TestMethod>]
    member _.TestConstructorSignatureSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments constructorSignature fplCode

    [<DataRow("01", """pred Test(a,b: tpl)""")>]
    [<TestMethod>]
    member _.TestcPredicateInstanceSignatureSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments predicateInstanceSignature fplCode

    [<DataRow("01", """func TestPredicate(a,b:obj)->obj""")>]
    [<DataRow("02", """func VecAdd(v,w: tplFieldElem) -> obj prefix "+" """)>]
    [<TestMethod>]
    member _.TestFunctionalTermSignatureSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments functionalTermSignature fplCode

    [<DataRow("01", """func BinOp(x,y: tplSetElem) -> tplSetElem""")>]
    [<DataRow("02", """func Add(n,m: Nat) -> pred""")>]
    [<TestMethod>]
    member _.TestFunctionalTermInstanceSignatureSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments functionalTermInstanceSignature fplCode

    [<DataRow("01", """cl ZeroVectorN""")>]
    [<TestMethod>]
    member _.TestClassSignatureSignatureSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments classSignature fplCode


