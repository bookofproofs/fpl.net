namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

[<TestClass>]
type TestID025() =

    [<DataRow("00", """loc not(x) := !tex: "\not(x)" ; ax T { n }""", 0)>]
    [<DataRow("01e", """def cl A {intr} ax T { A }""", 1)>]
    [<DataRow("01f", """inf A {pre:true con:true} ax T { A }""", 1)>]
    [<DataRow("01g", """ax A {true} ax T { A }""", 1)>]
    [<DataRow("01h", """thm A {true} ax T { A }""", 1)>]
    [<DataRow("01i", """lem A {true} ax T { A }""", 1)>]
    [<DataRow("01j", """prop A {true} ax T { A }""", 1)>]
    [<DataRow("01k", """conj A {true} ax T { A }""", 1)>]
    [<DataRow("01l", """cor A$1 {true} ax T { A$1 }""", 1)>]
    [<DataRow("01m", """proof A$1 {1: trivial}ax T { A$1 }""", 1)>]
    [<DataRow("01n", """ext A x@/\d+/ -> obj {ret x} ax T { A }""", 1)>]
    [<DataRow("01o", """loc A := !tex: "\alpha" ; ax T { A }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID025(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID025 ("", "")
            runTestHelper "TestID025.fpl" fplCode code expected
