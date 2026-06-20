namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID027() =

    [<DataRow("00", "def func T(list:* Nat[ind])->pred { dec result:pred for list in list { result:=true }; return result }", 1)>]
    [<DataRow("01", "def func T(list:* Nat[ind])->pred { dec result:pred for list in a { result:=true }; return result }", 0)>]
    [<DataRow("02", "def cl Set def cl Nat:Set def pred T(list:*Set[Nat]) { dec s:Set for s in list { i:=Succ(i) }; true }", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID027(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID027 ""
            runTestHelper "TestID027.fpl" fplCode code expected
