namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl0Base.Primitives
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type TestClassInheritanceTypes () =

    [<DataRow("02", """SomeClass""")>]
    [<TestMethod>]
    member this.TestInheritedTypeSuccess (no:string, input:string) =
        let result = run (inheritedType .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("01", """object """)>]
    [<DataRow("02", LiteralTpl)>]
    [<DataRow("03", """tplSetElem""")>]
    [<DataRow("04", """bla""")>]
    [<DataRow("05", """object[self,]""")>]
    [<DataRow("06", """object[x:SomeObject1, y:SomeObject2, a,b,c:SomeObject3]""")>]
    [<DataRow("07", """tpl[a:Nat,b:func]""")>]
    [<DataRow("08", """tpl[a:pred , b:index]""")>]
    [<DataRow("09", """tpl[a:tpl ,b:tplA]""")>]
    [<DataRow("10", """Set[x:ind , a,b:Nat]""")>]
    [<DataRow("11", """Set[x:index , y:func]""")>]
    [<DataRow("12", """Set[x:index , y:func()->obj]""")>]
    [<DataRow("13", """Set[a:func()]""")>]
    [<DataRow("14", """+Nat""")>]
    [<DataRow("15", """bla""")>]
    [<DataRow("16", """object[x:ind,y:index]""")>]
    [<DataRow("17", """object[x:obj,y:Nat]""")>]
    [<DataRow("18", """@extNat""")>]
    [<TestMethod>]
    member this.TestInheritedTypeFailure (no:string, input:string) =
        let result = run (inheritedType .>> eof) input
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
