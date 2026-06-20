namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestID022() =

    [<DataRow("00", "def pred T() {intr}", 0)>]
    [<DataRow("00a", "def pred T() {intr property pred Surjective() {RightTotal()}}", 0)>]
    [<DataRow("01", "def cl T def cl D:T {ctor D() {dec x:ind base.T(x);}}", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID022(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID022 "" 
            runTestHelper "TestID022.fpl" fplCode code expected
