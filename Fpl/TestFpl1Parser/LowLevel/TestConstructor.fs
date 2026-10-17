namespace TestFplParser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestConstructor () =


    [<DataRow("01", """ctor Magma(x: tplSet, op: BinOp) { }""")>]
    [<DataRow("02", """ctor Magma(x: tplSet, op: BinOp) { dec x:= 1 ; }""")>]
    [<DataRow("03", """ctor Magma(x: tplSet, op: BinOp) { dec a:obj base.AlgebraicStructure(x,op); }""")>]
    [<DataRow("04", """constructor Magma(x: tplSet, op: BinOp) { dec a:obj base.AlgebraicStructure(x,op) ; }""")>]
    [<DataRow("05", """ctor Magma(x: tplSet, op: BinOp) { dec a:obj ; }""")>]
    [<DataRow("06", """ctor Magma(x: tplSet, op: BinOp) { dec a:obj base.Obj() ; }""")>]
    [<DataRow("07", """ctor FieldPowerN ( field: Field, n: Nat ) { dec a:obj myField := field addInField := myField.AddOp() mulInField := myField.MulOp() assert NotEqual(n, Zero()) base.SetBuilder( myField[@1 , n], true) ; }""")>]
    [<DataRow("08", """ctor A(a:T1, b:func, c:ind, d:pred) { dec base.B() base.C(a,b,c,d) base.D(self,b,c) base.B(In(x)) base.C(Test1(a),Test2(b,c,d)) base.D(self,b,c) base.E(true, undef) ; }""")>]
    [<TestMethod>]
    member this.TestConstructorSuccess (no:string, fplCode:string) =
        let result = run (constructor .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """ctor Magma(x: tplSet, op: BinOp) { dec: x: obj ; }""")>]
    [<DataRow("02", """ctor Magma(x: tplSet, op: BinOp) { intr }""")>]
    [<DataRow("03", """ctor Magma(x: tplSet, op: BinOp) { dec:; intr }""")>]
    [<DataRow("04", """ctor Magma(x: tplSet, op: BinOp) { dec a:obj ; intr }""")>]
    [<DataRow("05", """ctor Magma(x: tplSet, op: BinOp) { dec a:obj ; intr }""")>]
    [<DataRow("06", """ctor Magma(x: tplSet, op: BinOp) { dec a:obj base. ; }""")>]
    [<DataRow("07", """Magma(x: tplSet, op: BinOp) { dec a:obj base.obj () base.T1(x) base.T2(op) ; }""")>]
    [<TestMethod>]
    member this.TestConstructorFailure (no:string, fplCode:string) =
        let result = run (constructor .>> eof) fplCode
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))

