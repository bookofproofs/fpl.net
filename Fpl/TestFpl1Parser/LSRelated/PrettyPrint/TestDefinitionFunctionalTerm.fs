namespace TestFpl1Parser.LSRelated.PrettyPrint

open System
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestDefinitionFunctionalTerm () =

    [<DataRow("01", """def func A()->undef""")>]
    [<DataRow("02", """def func A()->ind""")>]
    [<DataRow("03", """def function A()->obj""")>]
    [<DataRow("04", """def func A()->pred""")>]
    [<DataRow("05", """def func A()->func""")>]
    [<DataRow("06", """def function Sum(list:* Nat[ind,ind])->Nat""")>]
    [<DataRow("07", """def func A()->obj { return x }""")>]
    [<DataRow("08", """def func A()->obj { dec for addend in list {x:=x}; return x }""")>]
    [<DataRow("09", """def function A()->obj { dec for addend in list {y:=Add(result,addend)}; return y }""")>]
    [<DataRow("10", """def func A()->obj { dec a:obj result, addend: Nat for addend in list {y:=Add(result,addend)}; return y }""")>]
    [<DataRow("11", """def function LeftNeutralElement() -> tplSetElem { dec e1:obj assert ex e: tplSetElem { and ( IsLeftNeutralElement(e) ,(e = e1) ) }; return e1 }""")>]
    [<DataRow("12", """def func Succ(n: Nat) -> Nat { intrinsic }""")>]
    [<DataRow("13", """def func Add(n,m: Digits)->Nat { return delegate.Add(n,m) }""")>]
    [<DataRow("14", """def func Sum(list:* Nat[ind,ind])->Nat { dec a:obj result, addend: Nat result:=Zero() for addend in list { result:=Add(result,addend) } ; return result }""")>]
    [<DataRow("15", """def func Sum(list:* Nat[tpl])->Nat { dec a:obj i: index result:=Zero() for i in list { result:=Add(result,list[i]) } ; return result }""")>]
    [<DataRow("16", """def func Sum(from, to: Nat, arr:*Nat[Nat]) -> Nat { dec a:obj i, result: Nat result:=Zero() for i in ClosedRange(from,to) { result:=Add(result,arr[i]) } ; return result }""")>]
    [<DataRow("17", """def func Sum(arr: Nat) -> Nat { dec a:obj addend, result: Nat result:=Zero() for addend in arr { result:=Add(result,addend) } ; return result }""")>]
    [<DataRow("18", """def func Sum() -> Nat { dec a:obj addend, result: Nat result:=Zero() for addend in Nat { result:=Add(result,addend) } ; return result }""")>]
    [<DataRow("19", """def func Sum() -> Nat { dec a:obj addend, result: Nat result:=Zero() for addend in Nat() { result:=Add(result,addend) } ; return result }""")>]
    [<DataRow("20", """definition func Sum() -> Nat { dec a:obj addend, result: Nat result:=Zero() for addend in Nat() { result:=Add(result,addend) } ; return result }""")>]
    [<DataRow("21", """definition func Addend(a: Nat)->Nat { intr }""")>]
    [<DataRow("22", """definition function PowerSet(x: Set) -> Set { dec a:obj y: Set assert IsPowerSet(x,y) ; return y }""")>]
    [<DataRow("23", """definition func T() -> obj { intrinsic }""")>]
    [<DataRow("24", """definition func T() -> obj { intr }""")>]
    [<DataRow("25", """definition func T() -> obj { intrinsic property func T() -> obj { dec a:obj ; return x } property pred T() { true } }""")>]
    [<DataRow("26", """definition func T() -> obj { dec a:obj ; return x }""")>]
    [<DataRow("27", """definition function T() -> obj { dec a:obj ; return x }""")>]
    [<DataRow("28", """definition func T() -> obj { dec a:obj ; return x }""")>]
    [<DataRow("29", """definition func T() -> obj { return x }""")>]
    [<DataRow("30", """definition function T() -> obj { return x property func T() -> obj { dec a:obj ; return x } property pred T() { true } }""")>]
    [<DataRow("31", """definition func T()->obj { dec x:obj; return (S(x)) }""")>]
    [<DataRow("32", """definition func T() ->obj { dec x:obj; return x }""")>]
    [<DataRow("33", """definition func T() -> obj { dec x:obj; return x }""")>]
    [<DataRow("34", """definition function T()-> obj { dec x:obj; return x }""")>]
    [<DataRow("35", """definition func T()->obj { dec x:func(d:tpl)->tpl; return x }""")>]
    [<DataRow("36", """definition func T() ->obj { dec x:obj; return x }""")>]
    [<DataRow("37", """definition func T() -> obj { dec x:obj; return x }""")>]
    [<DataRow("38", """definition function T()-> obj { dec x:obj; return x }""")>]
    [<DataRow("39", """definition func T:A()->obj { dec x:obj; return x }""")>]
    [<DataRow("40", """definition func T: A()->obj { dec x:obj; return x }""")>]
    [<DataRow("41", """definition func T :A()->obj { dec x:obj; return x }""")>]
    [<DataRow("42", """definition func T:A,B,C()->obj { dec x:obj; return x }""")>]
    [<DataRow("43", """definition func T: A, B, C()->obj { dec x:obj; return x }""")>]
    [<DataRow("44", """definition func T : A , B , C ()->obj { dec x:obj; return x }""")>]
    [<DataRow("45", """def func Add (x,y: Nat) -> Nat infix "+" 2 { intr }""")>]
    [<DataRow("46", """def func Minus(x: Nat) -> Nat prefix "-" { intr }""")>]
    [<TestMethod>]
    member this.TestDefinitionFunctionalTermSyntaxErrorFreeInput (no:string, fplCode:string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments definition fplCode


    [<DataRow("01", """def func T() -> obj {  }""")>] 
    [<DataRow("02", """def func T() -> obj { dec; }""")>] 
    [<DataRow("03", """def func T() -> obj { dec a:obj ; }""")>] 
    [<DataRow("05", """def func T() -> obj { dec a:obj ; intrinsic }""")>] 
    [<DataRow("06", """def function T() -> obj { dec; intrinsic }""")>] 
    [<DataRow("08", """def func T() -> obj { intrinsic dec; }""")>] 
    [<DataRow("09", """def func T() -> obj { intrinsic dec a:obj ; }""")>] 
    [<DataRow("11", """def func T() -> obj { property func T() -> obj { dec a:obj ; return x } intrinsic property pred T() { true } }""")>] 
    [<DataRow("12", """def func T() -> obj { property pred T() { true } return x }""")>]
    [<DataRow("13", """def function DoubleSuccessor postfix "''" (x: N) -> N { returtttt }""")>]
    [<TestMethod>]
    member this.TestDefinitionFunctionalTermSyntaxErrorInput (no:string, fplCode:string) =
        allAssertionsForSyntaxErrorInput fplCode

