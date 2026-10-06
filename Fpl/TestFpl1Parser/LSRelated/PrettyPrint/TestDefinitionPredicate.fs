namespace TestFpl1Parser.LSRelated.PrettyPrint

open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestDefinitionPredicate () =

    [<DataRow("01", """def pred IsGreaterOrEqual(n,m: Nat) { ex k:Nat { Equal(n,Add(m,k)) } }""")>]
    [<DataRow("02", """def pred IsBounded(x: Real) { ex upperBound, lowerBound:Real { and (LowerEqual(x,upperBound), LowerEqual(lowerBound,x)) } }""")>]
    [<DataRow("03", """def pred IsBounded(f: RealValuedFunction) { all x:Real { IsBounded(f(x)) } }""")>]
    [<DataRow("04", """def pred Equal(a,b: tpl) { all p:pred { iif ( p(a), p(b) ) } }""")>]
    [<DataRow("05", """def pred NotEqual(x,y: tpl) { not ( Equal(x,y) ) }""")>]
    [<DataRow("06", """def pred AreRelated(u,v: Set, r: BinaryRelation) { dec a:obj one, two:Nat tuple:*Tuple[Nat] tuple:=Tuple(@1,@2) ; and ( and (In(tuple,r), In(u,r.Domain())), In(v,r.Codomain()) ) }""")>]
    [<DataRow("07", """def pred Greater(x,y: obj) { intrinsic }""")>]
    [<DataRow("08", """def pred IsPowerSet(ofSet, potentialPowerSet: Set) { all z:Set { impl (Subset(z,ofSet), In(z, potentialPowerSet)) } }""")>]
    [<DataRow("09", """def pred Union(x,superSet: Set) { all u:Set { impl (In(u, x), In(u, superSet)) } }""")>]
    [<DataRow("10", """def pred T() { intrinsic }""")>]
    [<DataRow("11", """def pred T() { intr }""")>]
    [<DataRow("12", """def pred T() { intrinsic property func T() -> obj { dec a:obj ; return x } property pred T() { true } }""")>]
    [<DataRow("13", """def pred T() { dec a:obj ; true }""")>]
    [<DataRow("14", """def pred T() { dec a:obj ; true }""")>]
    [<DataRow("15", """def pred T() { dec a:obj ; true }""")>]
    [<DataRow("16", """def pred T() { true }""")>]
    [<DataRow("17", """def predicate T() { true property func T() -> obj { dec a:obj ; return x } property pred T() { true } }""")>]
    [<DataRow("18", """def pred TestPredicate(a:T1, b:func, c:ind, d:pred) { true property pred T1() { delegate.B() } property pred T2() { delegate.C(a,b,c,d) } property pred T3() { delegate.D(self,b,c) } property pred T4() { delegate.B(In(x)) } property pred T5() { delegate.C(Test1(a),Test2(b,c,d)) } property pred T6() { delegate.E(true, undef, false) } }""")>]
    [<DataRow("19", """definition pred T() {dec dI1:D dI1:=D; true }""")>]
    [<DataRow("20", """def pred  T(x,y:obj) { @self(a,@self(b,c)) }""")>]
    [<DataRow("21", """def pred A()""")>]
    [<DataRow("22", """def predicate T() { A$1 }""")>]
    [<DataRow("23", """def pred T() { $1 }""")>]
    [<DataRow("24", """def pred T() { dec cI2:C1 cI2:=C1($2); true }""")>]
    [<DataRow("25", """definition pred TestPredicate(a:T1, b:func, c:ind, d:pred) { D(self,b,c) }""")>]
    [<DataRow("26", """definition predicate TestPredicate(a:T1, b:func, c:ind, d:pred) { delegate.C(Test1(a),Test2(b,c,d)) }""")>]
    [<DataRow("27", """def pred Successor(x: Nat) postfix "'" { intr }""")>]
    [<DataRow("28", """def pred Smaller(x,y: Nat) infix "<" 0 { intr }""")>]
    [<TestMethod>]
    member _.TestDefinitionPredicateSyntaxErrorFreeInput (no: string, fplCode: string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments definition fplCode

    [<DataRow("01", """def pred T() {  }""")>] 
    [<DataRow("02", """def pred T() { dec; }""")>]
    [<DataRow("03", """def pred T() { dec a:obj ; }""")>] 
    [<DataRow("05", """def pred T() { dec a:obj ; intrinsic }""")>] 
    [<DataRow("06", """def pred T() { dec; intrinsic }""")>]
    [<DataRow("08", """def pred T() { intrinsic dec; }""")>]
    [<DataRow("09", """def pred T() { intrinsic dec a:obj ; }""")>] 
    [<DataRow("11", """def pred T() { property func T() -> obj { dec a:obj ; return x } intrinsic property pred T() { true } }""")>]
    [<DataRow("12", """def pred T() { property pred T() { true } true }""")>] 
    [<DataRow("13", """def pred T() postfix "" {intr}""")>]
    [<DataRow("14", """def pred T() infix "" 0 {intr}""")>]
    [<DataRow("15", """def pred T() prefix "" {intr}""")>]
    [<TestMethod>]
    member this.TestDefinitionPredicateSyntaxErrorInput (no:string, fplCode:string) =
        allAssertionsForSyntaxErrorInput fplCode
