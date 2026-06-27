namespace Diagnostics.SignatureRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* SIG14
   Purpose: Warn about case branches that can never be reached because an earlier branch already matches the same condition signature.
   What it indicates: Two or more case conditions in the same `cases`/`mcases` construct produce identical condition signatures, so later branches are shadowed by earlier ones and will never execute.
   Use: Locate duplicated or shadowed case conditions (including equivalent predicates or identical signatures) so the author can correct ordering or make conditions more specific.
   Action / Treat: Remove or reorder the redundant branch, or refine the condition so each case is distinct; treat SIG14 as a correctness/readability warning that points to unreachable case branches. *)

[<TestClass>]
type TestSIG14() =

    [<DataRow("00", """def pred T() { mcases (| true: false | false: true ? undef) }""", 0)>] 
    [<DataRow("01", """def pred T() { mcases (| true: $1 | true: $2 ? undef) }""", 1)>] // second condition will never be reached
    [<DataRow("02", """def pred T() { mcases (| (x + 1 = 2): $1 | true: $2 ? undef) }""", 0)>] 
    [<DataRow("03", """def pred T() { mcases (| (x + 1 = 2): $1 | (x + 1 = 3): $2 ? undef) }""", 0)>] 
    [<DataRow("04", """def pred T() { mcases (| (x + 1 = 2): $1 | (x + 1 = 2): $2 ? undef) }""", 1)>] // second condition will never be reached
    [<DataRow("05", """def pred T() { mcases (| ex x:obj { (x = 2) }: $1 | (x + 1 = 2): $2 | ex x:obj { (x = 2) }: $3 | ex x:obj { (x = 2) }: $4 ? undef) }""", 2)>] // third and forth condition will never be reached
    [<DataRow("06", """def pred T() { dec res:pred cases (| true: res:=true | false: res:=true ? res:=undef); res }""", 0)>] 
    [<DataRow("07", """def pred T() { dec res:pred cases (| true: res:=$1 | true: res:=$2 ? res:=undef); res }""", 1)>] // second condition will never be reached
    [<DataRow("08", """def pred T() { dec res:pred cases (| (x + 1 = 2): res:=$1 | true: res:=$2 ? res:=undef); res }""", 0)>] 
    [<DataRow("09", """def pred T() { dec res:pred cases (| (x + 1 = 2): res:=$1 | (x + 1 = 3): res:=$2 ? res:=undef); res }""", 0)>] 
    [<DataRow("10", """def pred T() { dec res:pred cases (| (x + 1 = 2): res:=$1 | (x + 1 = 2): res:=$2 ? res:=undef); res }""", 1)>] // second condition will never be reached
    [<DataRow("11", """def pred T() { dec res:pred cases (| ex x:obj { (x = 2) }: res:=$1 | (x + 1 = 2): res:=$2 | ex x:obj { (x = 2) }: res:=$3 | ex x:obj { (x = 2) }: res:=$4 ? res:=undef); res }""", 2)>] // third and forth condition will never be reached
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSIG14(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SIG14
            
            runTestHelper "TestSIG14.fpl" fplCode code expected
