namespace TestFpl1Parser.LowLevel

open FParsec
open Fpl1Parser.Grammar
open Microsoft.VisualStudio.TestTools.UnitTesting


[<TestClass>]
type TestQuantifiers () =

    [<DataRow("01", """all x:obj {true}""")>]
    [<DataRow("02", """all x:func {true}""")>]
    [<DataRow("03", """all x:ind {true}""")>]
    [<DataRow("04", """all x:pred {true}""")>]
    [<DataRow("05", """all x:TestClass {true}""")>]
    [<DataRow("06", """all x:template {true}""")>]
    [<DataRow("07", """all x:Nat {true}""")>]
    [<DataRow("08", """all x:func()->obj {true}""")>]
    [<DataRow("09", """all x:SomeVar {true}""")>]
    [<DataRow("13", """all x:T {true}""")>]
    [<DataRow("14", """all x:Range, y:C, z:obj {true}""")>]
    [<DataRow("15", """all x:Real, y:pred, z:func {and (and(a,b),c)}""")>]
    [<DataRow("16", """ex x:Range, y:C, z:obj {and (and(a,b),c)}""")>]
    [<DataRow("17", """ex x:Real, y:pred, z:func {and (and(a,b),c)}""")>]
    [<DataRow("18", """all x,y,z:pred {true}""")>]
    [<DataRow("19", """all x,y,z:obj {not (iif ( true, not false))}""")>]
    [<DataRow("20", """all x,y,z:obj {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("21", """all  x:ind {not (iif ( iif ( true, iif( true, false)), not true ))}""")>]
    [<DataRow("22", """ex x,y,z:obj {true }""")>]
    [<DataRow("23", """ex x,y,z:obj { not (iif ( true, not false))}""")>]
    [<DataRow("24", """ex x,y,z:obj {not (iif ( iif( true, false), true))}""")>]
    [<DataRow("25", """ex  x:ind {not (iif ( iif ( true, iif( true, false)), not true ))}""")>]
    [<DataRow("27", """exn$1 x:Nat {not (iif ( true, not (false)))}""")>]
    [<DataRow("29", """exn$3  x:ind {not (iif ( iif ( true, iif( true, false)), not true ))}""")>]
    [<DataRow("30", """ex x:Real {true}""")>]
    [<TestMethod>]
    member this.TestQuantifiers (no:string, code:string) =
        let result = run (predicate .>> eof) code
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Success:"))

    [<DataRow("10", """all x:self {true}""")>]
    [<DataRow("11", """all x:ClosedRange(from,to) {true}""")>]
    [<DataRow("12", """all x in T[x] {true}""")>]
    [<DataRow("26", """exn$0 x,y,z(true)""")>]
    [<DataRow("28", """exn$2 x in Nat,y {not (iif ( iif( true, false), true))}""")>]
    [<TestMethod>]
    member this.TestQuantifiersFail (no:string, code:string) =
        let result = run (predicate .>> eof) code
        let actual = sprintf "%O" result
        printf "%O" actual
        Assert.IsTrue(actual.StartsWith("Failure:"))
