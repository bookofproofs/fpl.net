namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Fpl0Base.Primitives
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestPredicatesSpecific () =

    [<DataRow("01", LiteralTrue)>]
    [<DataRow("02", LiteralFalse)>]
    [<DataRow("03", LiteralUndef)>]
    [<DataRow("04", LiteralUndefL)>]
    [<DataRow("05", """list[i]""")>]
    [<DataRow("06", """arr[i]""")>]
    [<DataRow("07", LiteralParent)>]
    [<TestMethod>]
    member _.TestPrimePredicateSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments primePredicate fplCode

    [<DataRow("01", """and(true,false)""")>]
    [<DataRow("02", """and ( true, true )""")>]
    [<DataRow("03", """and ( true, and( true, false))""")>]
    [<DataRow("04", """and ( and ( true, and( true, false)), true )""")>]
    [<TestMethod>]
    member _.TestConjunctionSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments conjunction fplCode

    [<DataRow("01", """or(true,false)""")>]
    [<DataRow("02", """or ( true, true )""")>]
    [<DataRow("03", """or ( true, or( true, false))""")>]
    [<DataRow("04", """or ( or ( true, or( true, false)), true )""")>]
    [<TestMethod>]
    member _.TestDisjunctionSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments disjunction fplCode

    [<DataRow("01", """impl(true,false)""")>]
    [<DataRow("02", """impl ( true, true )""")>]
    [<DataRow("03", """impl ( true, impl( true, false))""")>]
    [<DataRow("04", """impl ( impl ( true, impl( true, false)), true )""")>]
    [<TestMethod>]
    member _.TestImplicationSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments implication fplCode

    [<DataRow("01", """iif(true,false)""")>]
    [<DataRow("02", """iif ( true, true )""")>]
    [<DataRow("03", """iif ( true, iif( true, false))""")>]
    [<DataRow("04", """iif ( iif ( true, iif( true, false)), true )""")>]
    [<TestMethod>]
    member _.TestEquivalenceSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments equivalence fplCode

    [<DataRow("01", """xor(true,false)""")>]
    [<DataRow("02", """xor ( true, true )""")>]
    [<DataRow("02a", """xor ( true, xor(true, false) )""")>]
    [<DataRow("03", """xor ( true, xor( true, false))""")>]
    [<DataRow("04", """xor ( xor ( true, xor( true, false)), true )""")>]
    [<TestMethod>]
    member _.TestExclusiveOrSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments exclusiveOr fplCode

    [<DataRow("01", """Zero()""")>]
    [<DataRow("02", """self(i)""")>]
    [<DataRow("04", """Add(result,list[i])""")>]
    [<DataRow("05", """Add(result,arr[i])""")>]
    [<DataRow("01a", """x[Zero()]""")>]
    [<DataRow("02a", """x[self(i)]""")>]
    [<DataRow("03a", """x[PrecedingResults(x,y)]""")>]
    [<DataRow("04a", """x[Add(result,list[i])]""")>]
    [<DataRow("05a", """x[Add(result,arr[i])]""")>]
    [<DataRow("06a", """x[A1[A2().A3()]]""")>]
    [<DataRow("07", """x[$3,$2]""")>]
    [<DataRow("07a", """x[@3,@3]""")>]
    [<DataRow("71a", """x[@3()]""")>]
    [<DataRow("08", """myOp.NeutralElement()""")>]
    [<DataRow("09", """myOp.NeutralElement().SomeProperty()""")>]
    [<DataRow("a01", """Xx""")>]
    [<DataRow("a02", """Xx.Xx""")>]
    [<DataRow("a03", """Xx.Xx.Xx""")>]
    [<DataRow("a04", """Xx()""")>]
    [<DataRow("a05", """Xx.Xx()""")>]
    [<DataRow("a06", """Xx.Xx.Xx()""")>]
    [<DataRow("a07", """Xx().Yy""")>]
    [<DataRow("a08", """Xx.Xx().Yy""")>]
    [<DataRow("a09", """Xx.Xx.Xx().Yy""")>]
    [<DataRow("a10", """Xx().Yy.Zz""")>]
    [<DataRow("a11", """Xx.Xx().Yy.Zz""")>]
    [<DataRow("a12", """Xx.Xx.Xx().Yy.Zz""")>]
    [<DataRow("a13", """Xx().Yy().Zz()""")>]
    [<DataRow("a14", """Xx[Xx.Xx]""")>]
    [<DataRow("a15", """Xx[Xx.Xx()]""")>]
    [<DataRow("a16", """Xx[Xx()].Yy""")>]
    [<DataRow("a17", """Xx.Xx[Yy]""")>]
    [<DataRow("a18", """Xx[Xx[Xx().Yy]]""")>]
    [<DataRow("a19", """Xx[Yy().Zz]""")>]
    [<DataRow("a20", """Xx[Xx[Yy().Zz]]""")>]
    [<DataRow("a21", """Xx[Xx[Xx()].Yy[Zz]]""")>]
    [<DataRow("a22", """Xx[Yy()].Zz()""")>]
    [<DataRow("b01", """x[@123]""")>]
    [<DataRow("b02", """x[y]""")>]
    [<DataRow("b03", """myField[@1 , n]""")>]
    [<DataRow("b04", """self[from , to]""")>]
    [<DataRow("b05", """tpls[from , to]""")>]
    [<TestMethod>]
    member _.TestPredicateWithQualificationSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments predicateWithQualification fplCode

    [<DataRow("01", """not (true)""")>]
    [<DataRow("02", """not (iif ( true, not (false)))""")>]
    [<DataRow("03", """not (iif ( iif( true, false), true))""")>]
    [<DataRow("04", """not (iif ( iif ( true, iif( true, false)), not (true) ))""")>]
    [<DataRow("05", """not all x,y:N { (x >< y) }""")>]
    [<TestMethod>]
    member _.TestNegationSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments negation fplCode

    [<DataRow("01", """is(x, Nat)""")>]
    [<DataRow("01a", """x is Nat""")>]
    [<DataRow("02", """is(1, Set)""")>]
    [<DataRow("02a", """2 is Set""")>]
    [<DataRow("03", """is(One, Set)""")>]
    [<DataRow("03a", """One is Set""")>]
    [<DataRow("04", """is(T.X.Y, Set)""")>]
    [<DataRow("04a", """T.X.Y is Set""")>]
    [<DataRow("05", """is(self, Set)""")>]
    [<DataRow("05a", """self is Set""")>]
    [<DataRow("06", """is(parent, Set)""")>]
    [<DataRow("06a", """parent is Set""")>]
    [<DataRow("07", """is(A$1, Set)""")>]
    [<DataRow("07a", """A$1 is Set""")>]
    [<DataRow("08", """is($1, ind)""")>]
    [<DataRow("08a", """$1 is ind""")>]
    [<DataRow("09", """is(undef, ind)""")>]
    [<DataRow("09a", """undef is ind""")>]
    [<DataRow("10", """is(true, ind)""")>]
    [<DataRow("10a", """true is ind""")>]
    [<DataRow("11", """is(false, ind)""")>]
    [<DataRow("11a", """false is ind""")>]
    [<TestMethod>]
    member _.TestIsOperatorSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments isOperator fplCode

    [<DataRow("01", """ex x,y,z:func {true}""")>]
    [<DataRow("02", """ex x,y,z:ind {not (iif ( true, not (false)))}""")>]
    [<DataRow("03", """ex x,y,z:pred {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("04", """ex x:obj {not (iif ( iif ( true, iif( true, false)), not (true) ))}""")>]
    [<DataRow("05", """ex x:Range, y:C, z:obj {and (a,and(b,c))}""")>]
    [<DataRow("06", """ex x:Real, y:pred, z:func {and (and(a,b),c)}""")>]
    [<TestMethod>]
    member _.TestExistsSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments exists fplCode

    [<DataRow("01", """exn$0 x:obj { true}""")>]
    [<DataRow("02", """exn$1 x:Nat {not (iif ( true, not (false)))}""")>]
    [<DataRow("03", """exn$2 x,y,z:obj {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("04", """exn$3 x:obj { not (iif ( iif ( true, iif( true, false)), not (true) )) }""")>]
    [<TestMethod>]
    member _.TestExistsTimesNSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments existsTimesN fplCode

    [<DataRow("01", """( x = 1 )""")>]
    [<DataRow("02", """( x = y = z )""")>]
    [<DataRow("03", """( x ∈ y ∈ z )""")>]
    [<DataRow("04", """( x + y / z = abc )""")>]
    [<DataRow("05", """( ((x) + y) / z = abc )""")>]
    [<TestMethod>]
    member _.TestPInfixExprNSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments pInfixExpr fplCode

