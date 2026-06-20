namespace Diagnostics.StructureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestST005() =

    [<DataRow("01", """def cl Nat def func Sum(list:* Nat[ind])->Nat { dec a:obj result, addend: Nat result:=Zero() for addend in list { result:=Add(result,addend) }; return result }""", 0)>]
    [<DataRow("02", """def cl Nat def func Sum(from, to: Nat, arr:*Nat[Nat]) -> Nat { dec a:obj i, result: Nat result:=Zero() for i in ClosedRange(from,to) { result:=Add(result,arr[i]) }; return result }""", 1)>]
    [<DataRow("03", """def cl Nat def func Sum() -> Nat { dec addend, result: Nat result:=Zero() for addend in Nat { result:=Add(result,addend) }; return result }""", 1)>]
    [<DataRow("04", """def cl Nat def func Add(x,y:Nat)->Nat def func Sum()->Nat {dec addend, result: Nat for addend in Nat() { result:=Add(result,addend) }; ret result }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestST005(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST005 ("", "")
            runTestHelper "TestST005.fpl" fplCode code expected
