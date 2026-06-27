namespace Diagnostics.SignatureRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SIG11
   Purpose: Flag mappings that delegate to another mapping types (like definitions of functional terms or extensions that have an own mapping) instead of mapping directly to a concrete class/type.
   What it indicates: A mapping refers to a candidate that is not a valid direct mapping target, so the mapping is indirect or invalid.
   Use: Help authors find mapping declarations whose target is wrong or unintentionally delegated so they can correct the mapping target or the extension definition.
   Action / Treat: Change the mapping to point directly to the intended type, or adjust the extension so it maps to itself; treat SIG11 as an error that must be fixed to make the mapping explicit and valid. *)

[<TestClass>]
type TestSIG11() =

    // mapping
    [<DataRow("01", """def func T()->A""", 0)>] // undefined mapping -> no SIG11
    [<DataRow("01a", """def func T()->*A[ind]""", 0)>] // undefined mapping -> no SIG11
    [<DataRow("02", """def cl A def func T()->A""", 0)>] // mapping is defined class -> no SIG11
    [<DataRow("03", """def cl A def func T()->*A[ind]""", 0)>] // mapping is defined class -> no SIG11
    [<DataRow("04", """ext A x@/\d+/->A {ret x} def func T()->A""", 0)>] // mapping of functional term is an extension mapping to itself, no SIG11
    [<DataRow("04a", """def cl B ext A x@/\d+/->B {ret B()} def func T()->A""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("05", """def pred A() {intr} def func T()->A""", 1)>] // mapping is a defined predicate -> issue SIG11
    [<DataRow("06", """def func A()->obj {intr} def func T()->A""", 1)>] // mapping is a defined predicate -> issue SIG11
    [<DataRow("07", """inf A {pre:true con:true} def func T()->A""", 1)>] // mapping is a defined predicate -> issue SIG11
    [<DataRow("07", """ax A {true} def func T()->A""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("08", """thm A {true} def func T()->A""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("09", """prop A {true} def func T()->A""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("10", """lem A {true} def func T()->A""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("11", """conj A {true} def func T()->A""", 1)>] // mapping is not a class -> issue SIG11
    [<DataRow("12", """def func T()->pred""", 0)>] // mapping is not an identifier, no SIG11
    [<DataRow("13", """def func T()->func""", 0)>] // mapping is not an identifier, no SIG11
    [<DataRow("14", """def func T()->ind""", 0)>] // mapping is not an identifier, no SIG11
    [<DataRow("15", """def func T()->obj""", 0)>] // mapping is not an identifier, no SIG11
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG11(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG11 ""
            
            runTestHelper "TestSIG11.fpl" fplCode code expected
