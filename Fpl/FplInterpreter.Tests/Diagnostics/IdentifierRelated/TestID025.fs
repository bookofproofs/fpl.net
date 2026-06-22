namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(* ID025
   Purpose: Report an illegal reference to an entity inside a given call site.
   What it indicates: A code site attempted to reference an entity from within a context where such references are disallowed or meaningless.
   Use: Emitted during symbol-table creation and expression embedding to point authors to references that violate scope/granularity rules.
   Action / Treat: Move or change the reference so it targets an allowed entity (or change the declaration context), or restructure the code so the referenced entity is accessible from the node's scope. ID025 is an error that must be corrected for correct symbol resolution. *)

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
