namespace Diagnostics.SignatureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SIG12
   Purpose: Detect inconsistent usages of a template where the same template is instantiated with different types within the same scope.
   What it indicates: A template was first used with one concrete type and later used with a different concrete type (or vice versa), so template instantiation is not uniform; the diagnostic includes the first usage position to help locate the original binding.
   Use: Locate and fix inconsistent template instantiations (for example, template parameters used with differing types across branches or declarations) so the template has a single, coherent usage in the given scope.
   Action / Treat: Make all template usages consistent (use the same concrete type everywhere in the affected scope), rename/refactor the template or restrict its scope, or adjust the earlier usage to match the intended type; treat SIG12 as a type-consistency error that should be resolved. *)

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
