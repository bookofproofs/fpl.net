namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID008() =

    [<DataRow("def cl Test {ctor TestTypo(x:Nat) {}}", 1)>]
    [<DataRow("def cl Test {ctor TestTypo1() {}}", 1)>]
    [<DataRow("def cl Test {ctor Test() {}}", 0)>]
    [<DataRow("def cl Test {dec x:obj x := 0; ctor Test() {dec base.Obj(); }}", 0)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID008(fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID008 ("", "") 
            runTestHelper "TestID008.fpl" fplCode code expected
