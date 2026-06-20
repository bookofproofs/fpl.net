namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID024() =

    [<DataRow("00a", """loc not(x) :=  !tex: "\neg(" x ")";""", 0)>]
    [<DataRow("00b", """loc not(x) :=  !tex: "\neg(" x ")"; loc not(x) :=  !tex: "\neg(" x ")"; """, 1)>]
    [<DataRow("00c", """loc not(y) :=  !tex: "\neg(" x ")"; loc not(x) :=  !tex: "\neg(" x ")"; """, 1)>]
    [<DataRow("01a", """loc Equal(x,y) := !tex: x "=" y !eng: x " equals " y !ger: x " ist gleich " y;""", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID024(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID024 ("", "")
            runTestHelper "TestID024.fpl" fplCode code expected
