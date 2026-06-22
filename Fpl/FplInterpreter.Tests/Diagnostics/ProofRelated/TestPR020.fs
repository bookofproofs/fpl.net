namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(* PR020
   Purpose: Report that a `byinf` justification requires a specific number of proceeding justification items but received a different number.
   What it indicates: The rule-of-inference premise list expects N proceeding expressions, however the justification supplied M proceeding items (too few or too many), so argument inference cannot proceed.
   Use: Pinpoint failing `byinf` justification steps where the number of preceding justification items does not match the rule's premises.
   Action / Treat: Fix the proof by providing the required number of proceeding justification items, adjust the rule-of-inference premises, or restructure the justification sequence. PR020 is an error that prevents successful argument inference and must be resolved. *)

[<TestClass>]
type TestPR020() =

    [<DataRow("01", """inference ModusPonens { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } theorem T {dec x,z:pred; true } proof T$1 {1: and (x, impl(x,z)) 2. 1, byinf ModusPonens |- z }""", 0)>]
    [<DataRow("01a", """inference ModusPonens { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } theorem T {dec x,z:pred; true } proof T$1 {1: and (x, impl(x,z)) 2. byinf ModusPonens |- z }""", 1)>]
    [<DataRow("01b", """thm A {true} inference ModusPonens { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } theorem T {dec x,z:pred; true } proof T$1 {1: and (x, impl(x,z)) 2. 1, A, byinf ModusPonens |- z }""", 1)>]
    [<DataRow("01c", """inference ModusPonens { dec p,q: pred; pre: and (p, impl (p,q) ) con: q } theorem T {dec x,z:pred; true } proof T$1 {1: and (x, impl(x,z)) 2. 1, 1, byinf ModusPonens |- z }""", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR020(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR020 (0, 0) 
            runTestHelper "TestPR020.fpl" fplCode code expected
