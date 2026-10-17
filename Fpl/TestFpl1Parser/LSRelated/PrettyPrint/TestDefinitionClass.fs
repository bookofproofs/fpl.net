namespace TestFpl1Parser.LSRelated.PrettyPrint

open System
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting
open TestFpl1Parser.LSRelated.PrettyPrint.Commons

[<TestClass>]
type TestDefinitionClass () =

    [<DataRow("01", """definition class FieldPowerN: Obj { intr }""")>]
    [<DataRow("02", """definition class FieldPowerN: Set { ctor FieldPowerN() { dec base.Obj() ; } }""")>]
    [<DataRow("03", """definition class FieldPowerN: Set { dec x: obj ; constructor FieldPowerN() { dec base.Obj() ; } }""")>]
    [<DataRow("04", """definition cl FieldPowerN: Set { dec a:obj ; ctor FieldPowerN() { dec base.Obj() ; } }""")>]
    [<DataRow("05", """definition cl FieldPowerN: Set { ctor FieldPowerN() { dec base.Obj() ; } }""")>]
    [<DataRow("06", """definition cl FieldPowerN: Set { ctor FieldPowerN() { dec base.Obj() ; } constructor FieldPowerN() { dec base.T1() ; } }""")>]
    [<DataRow("07", """definition cl FieldPowerN: Set { ctor FieldPowerN() { dec base.Obj() ; } ctor FieldPowerN() { dec base.T1() ; } property func T() -> obj { dec a:obj ; return x } property pred T() { true } }""")>]
    [<DataRow("08", """definition cl FieldPowerN: Set { ctor FieldPowerN() { dec base.T1() ; } property pred T() { true } }""")>]
    [<DataRow("09", """def class FieldPowerN: Typ1, Typ2, Typ3 { intrinsic }""")>]
    [<DataRow("10", """def class FieldPowerN: Typ1 { intrinsic }""")>]
    [<DataRow("11", """def class FieldPowerN: Typ1 { intrinsic property func T() -> obj { dec a:obj ; return x } property pred T() { true } }""")>]
    [<DataRow("12", """def class SomeClass:Nat1 ,Nat2Nat3,Nat3 { intrinsic }""")>]
    [<DataRow("13", """def class SomeClass :Nat1,Nat2, Nat3,Nat3 { intrinsic }""")>]
    [<DataRow("14", """def cl TestId { ctor TestId() {} ctor TestId(x:obj) {} ctor TestId(x:pred) {} ctor TestId(x:ind) {} }""")>]
    [<DataRow("15", """def cl T { intr prty func T1() -> obj { ret y } }""")>]
    [<TestMethod>]
    member this.TestDefinitionClassSyntaxErrorFreeInput (no:string, fplCode:string) =
        allAssertionsSyntaxErrorFreeInputWithoutComments definition fplCode


    [<DataRow("01", """def class FieldPowerN: Obj { }""")>]
    [<DataRow("02", """def class FieldPowerN: Set { dec:; intr }""")>]
    [<DataRow("03", """def class FieldPowerN: Set { dec a:obj ; intr }""")>]
    [<DataRow("04", """def class FieldPowerN: Set { dec d:Nat d:=1 ; intr }""")>]
    [<DataRow("05", """def class FieldPowerN: Set { decs := x }""")>]
    [<DataRow("06", """def class FieldPowerN: Set { dec:; FieldPowerN() { base.Obj() } }""")>]
    [<DataRow("07", """def class FieldPowerN: Set { dec a:obj ; FieldPowerN() { dec base.Obj() ; } }""")>]
    [<DataRow("08", """def class FieldPowerN: Set { FieldPowerN() { base.Obj() } }""")>]
    [<DataRow("09", """def class FieldPowerN: Set { FieldPowerN() { dec a:obj base.Obj() ; self } optional pred T() { true } FieldPowerN() { dec base.T1() ; self } mand func T() -> obj { dec a:obj ; return x } }""")>]
    [<DataRow("10", """def class FieldPowerN: Set { optional pred T() { true } FieldPowerN() { dec a:obj self.T1() ; self } }""")>]
    [<TestMethod>]
    member this.TestDefinitionClassSyntaxErrorInput (no:string, fplCode:string) =
        allAssertionsForSyntaxErrorInput fplCode

