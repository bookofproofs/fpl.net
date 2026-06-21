namespace Diagnostics.StructureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

(* ST001
   Purpose: Warn that a block (class, function, or similar definition) is empty or marked as intrinsic without useful content.
   What it indicates: The defined construct has no executable content or meaningful members (e.g., an intrinsic class with no properties or constructors), so the block does nothing.
   Use: Help authors spot no-op definitions that might be unintended or indicate incomplete implementations.
   Action / Treat: Simplify the code by removing the empty definition block (making it intrinsic without even properties) or add meaningful content (properties, constructors, bodies) or remove the no-op definition; treat ST001 as a structural warning suggesting cleanup or implementation. *)

[<TestClass>]
type TestST001() =

    [<DataRow("00a", "def cl A {intr} ", 1)>]
    [<DataRow("00b", "def cl A:B {intr} ", 1)>]
    [<DataRow("00c", "def cl A:B {intr property pred T() {true} } ", 0)>]
    [<DataRow("00d", "def cl A:B {ctor A(){dec base.B();} } ", 0)>]
    [<DataRow("00e", "def cl A {ctor A(){} } ", 0)>]
    [<DataRow("00f", "def cl A", 0)>]
    [<DataRow("00g", "def cl A:B,C {intr} ", 1)>]
    [<DataRow("01a", "def func A()->obj {intr} ", 1)>]
    [<DataRow("01b", "def func A:B()->obj {intr} ", 1)>]
    [<DataRow("01c", "def func A(x:obj)->obj {intr} ", 1)>]
    [<DataRow("01d", "def func A:B(x:obj)->obj {intr} ", 1)>]
    [<DataRow("01c", "def func A:B()->obj {intr property pred T() {true} } ", 0)>]
    [<DataRow("01d", "def func A:B()->obj {dec x:obj; return x}", 0)>]
    [<DataRow("01e", "def func A()->obj", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestST001(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ST001 ""
            runTestHelper "TestST001.fpl" fplCode code expected
