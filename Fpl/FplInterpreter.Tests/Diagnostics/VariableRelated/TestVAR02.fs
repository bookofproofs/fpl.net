namespace Diagnostics.VariableRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestVAR02() =

    [<DataRow("00", "axiom T {all n:pred { n } }", 0)>]
    [<DataRow("00a", "axiom T {all n, n:pred { n } }", 1)>]
    [<DataRow("00b", "axiom T {all n:pred, n:pred { n } }", 1)>]
    [<DataRow("01", "axiom T {ex n:pred { n } }", 0)>]
    [<DataRow("01a", "axiom T {ex n, n:pred { n } }", 1)>]
    [<DataRow("01b", "axiom T {ex n:pred, n:pred { n } }", 1)>]
    [<DataRow("02", "axiom T {exn$1 n:pred { n } }", 0)>]
    [<DataRow("02a", "axiom T {exn$1 n, n:pred { n } }", 1)>]
    [<DataRow("02a", "axiom T {exn$1 n:pred, n:pred { n } }", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR02(no: string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR02 ""
            runTestHelper "TestVAR02.fpl" fplCode code expected
