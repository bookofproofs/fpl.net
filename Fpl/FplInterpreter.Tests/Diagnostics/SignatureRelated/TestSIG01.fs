namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG01() =

    [<DataRow("01", """def pred Equal(x,y:tpl) infix "=" 0 { delegate.Equal(x,y) } def pred NotEqual (x,y: tpl) { not (x = y) }""", 0)>]
    [<DataRow("02", """def pred NotEqual (x,y: tpl) { not (x = y) }""", 1)>]
    [<DataRow("03", """def pred T(x,y:obj) infix "+" 0  {true} def pred Test() {(x + y)}""", 0)>]
    [<DataRow("04", """def pred T(x,y:obj) infix "+" 0  {true} def pred Test() {+x}""", 0)>]
    [<DataRow("05", """def pred T(x,y:obj) infix "+" 0 {true} def pred Test() {x+}""", 0)>]
    [<DataRow("06", """def pred T(x,y:obj) infix "+" 0 {true} def pred Test() {(x - y)}""", 1)>]
    [<DataRow("07", """def pred T(x,y:obj) infix "+" 0 {true} def pred Test() {-x}""", 1)>]
    [<DataRow("08", """def pred T(x,y:obj) infix "+" 0 {true} def pred Test() {x-}""", 1)>]
    [<DataRow("09", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {(x + y)}""", 0)>]
    [<DataRow("10", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {+x}""", 0)>]
    [<DataRow("11", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {x+}""", 0)>]
    [<DataRow("12", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {(x - y)}""", 1)>]
    [<DataRow("13", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {-x}""", 1)>]
    [<DataRow("14", """def pred T(x,y:obj) prefix "+" {true} def pred Test() {x-}""", 1)>]
    [<DataRow("15", """def pred T(x,y:obj) postfix "+"  {true} def pred Test() {(x + y)}""", 0)>]
    [<DataRow("16", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {+x}""", 0)>]
    [<DataRow("17", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {x+}""", 0)>]
    [<DataRow("18", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {(x - y)}""", 1)>]
    [<DataRow("19", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {-x}""", 1)>]
    [<DataRow("20", """def pred T(x,y:obj) postfix "+" {true} def pred Test() {x-}""", 1)>]
    [<DataRow("21", """loc (x + y) := !tex: x "+" y; """, 1)>]
    [<DataRow("22", """def pred T(x,y:obj) infix "+" 0 loc (x + y) := !tex: x "+" y; """, 0)>]
    [<DataRow("23", """def cl A symbol "0" {intr} axiom T {0} """, 0)>]
    [<DataRow("24", """def cl A symbol "1" {intr} axiom T {0} """, 1)>]
    [<DataRow("25", """def cl A {intr} axiom T {0} """, 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG01(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG01 ""
            runTestHelper "TestSIG01.fpl" fplCode code expected
