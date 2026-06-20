namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID014() =

    [<DataRow("""loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !ger: x " dann und nur dann wenn " y;""", 0)>]
    [<DataRow("""loc iif(x, y) := !tex: x "\Leftrightarrow" y !eng: x " if and only if " y !tex: x " dann und nur dann wenn " y;""", 1)>]
    [<DataRow("uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID014(fplCode:string, expected:int) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID014 ("", "")
            runTestHelper "TestID014.fpl" fplCode code expected
