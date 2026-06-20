namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestSIG12() =

    [<DataRow("00", "def pred T(x:tpl) {dec x:=$1 x:=$2; x}", 0)>]
    [<DataRow("00a", "def pred T(x:tpl) {dec x:=$1 x:=true; x}", 1)>] // A template not accepting assigning different types
    [<DataRow("01", "def pred Equal(x,y: tpl) { del.Equal(x,y) } def pred T() {dec x:ind y:ind x:=$1 y:=$2; Equal(x,y)}", 0)>]
    [<DataRow("01a", "def pred Equal(x,y: tpl) { del.Equal(x,y) } def pred T() {dec x:ind y:pred x:=$1 y:=true; Equal(x,y)}", 1)>] // same template in Equal do not accept different types
    [<DataRow("02", "def pred Equal(x: tpl, y:tpl1) { del.Equal(x,y) } def pred T() {dec x:ind y:ind x:=$1 y:=$2; Equal(x,y)}", 0)>] 
    [<DataRow("02a", "def pred Equal(x: tpl, y:tpl1) { del.Equal(x,y) } def pred T() {dec x:ind y:pred x:=$1 y:=true; Equal(x,y)}", 0)>] // two templates in Equal accepting different types
    [<DataRow("02", "def cl A def pred T() {dec x:*tpl[ind] x[$1]:=true x[$2]:=false; true}", 0)>] 
    [<DataRow("02a", "def cl A def pred T() {dec x:*tpl[ind] x[$1]:=true x[$2]:=A(); true}", 1)>] // a template not accepting assigning different types
    [<DataRow("03", """def pred Equal(x,y: tpl) infix "=" 50 { del.Equal(x,y)} ext Digits x@/\d+/ -> pred {ret mcases ( | (x = @0): true ? false ) }""", 0)>]
    [<DataRow("04", """def pred Equal(x,y: tpl) infix "=" 50 {del.Equal(x,y)} ext Digits x@/\d+/ -> pred {ret mcases (| (x = @0): true ? false)} def pred T(m:obj) { (m = @0) }""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG12(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG12 ("", "", "", "")
            
            runTestHelper "TestSIG12.fpl" fplCode code expected
