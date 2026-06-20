namespace Diagnostics.LogicRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestLG002() =

    [<DataRow("00", """def pred T() { true }""", 0)>]
    [<DataRow("01a", """def pred T() { self }""", 1)>]
    [<DataRow("01b", """def pred T() { self() }""", 1)>]
    [<DataRow("02", """def pred T(x:obj) { self(x) }""", 0)>] // since x is an undetermined argument, self is not run and infinite loop LG002 diagnostics does not occur
    [<DataRow("02a", """def pred T(x:ind) { dec x:=$1; self(x) }""", 1)>] 
    [<DataRow("03", """def func T()->obj { intr }""", 0)>]
    [<DataRow("03a", """def func T()->obj { return self }""", 0)>] // since self typed `func() -> obj` doesn't match the expected returned value `obj`, self is not run and infinite loop LG002 diagnostics does not occur
    [<DataRow("03b", """def func T()->obj { return self() }""", 1)>]
    [<DataRow("04", """def func T(x:obj)->obj { return self(x) }""", 0)>] // since x is an undetermined argument, self is not run and infinite loop LG002 diagnostics does not occur
    [<DataRow("04a", """def func T(x:ind)->obj { dec x:=$1; return self(x) }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestLG002(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = LG002 ("",0)
            runTestHelper "TestLG002.fpl" fplCode code expected
