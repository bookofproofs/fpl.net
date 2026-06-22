namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

(* ID022
   Purpose: Report an invalid parameterized call to a base constructor from a derived class.
   What it indicates: A derived class constructor invoked the base constructor with parameters, but the base class does not expose a parameterized constructor. This typically happens when the base class is intrinsic and only provides the default (parameter‑free) constructor.
   Use: Pinpoints derived constructors that incorrectly forward parameters to a non‑parameterized base so the author can correct the call or the class hierarchy.
   Action / Treat: Remove the unsupported parameters from the base call, change the base to a type that provides a matching constructor, or refactor the derived class so it does not rely on a parameterized base constructor. Treat ID022 as an error that must be fixed for correct class construction. *)

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
