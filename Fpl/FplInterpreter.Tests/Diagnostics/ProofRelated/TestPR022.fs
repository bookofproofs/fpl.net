namespace Diagnostics.ProofRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(* PR022
   Purpose: Report that argument inference for a justification item was prevented.
   What it indicates: A justification item attempted to collect inferred results to contribute to argument inference but failed. Typical reasons: the justification refers to a non-existing item, the referred item contains no predicative expressions to extract (e.g. an intrinsic definition or a class with no predicates), or the referenced item cannot provide the needed inferred formulas.
   Use: Point authors to failing justification steps so they can correct references or supply a source that actually yields inferable predicative expressions.
   Action / Treat: Fix the justification reference (correct the identifier or proof/corollary reference), ensure the referenced block exposes predicative expressions or assertions, or change the justification strategy. Treat PR022 as an error that prevents inference and must be resolved for the affected proof step. *)


[<TestClass>]
type TestPR022() =


    [<DataRow("00", """ax A {true} thm T {true} prf T$1 { 1. byax A |- true }""", 0)>]
    [<DataRow("00a", """prf T$1 { 1. byax A |- false }""", 1)>]
    [<DataRow("01", """conj A {true} thm T {true} prf T$1 { 1. byconj A |- true }""", 0)>]
    [<DataRow("01a", """prf T$1 { 1. byconj A |- false }""", 1)>]
    [<DataRow("02thm", """thm A {true} thm T {true} prf T$1 { 1. A |- true }""",  0)>]
    [<DataRow("02lem", """lem A {true} thm T {true} prf T$1 { 1. A |- true }""", 0)>]
    [<DataRow("02prop", """prop A {true} thm T {true} prf T$1 { 1. A |- true }""", 0)>]
    [<DataRow("02a", """prf T$1 { 1. A |- false }""",  1)>]
    [<DataRow("03", """cor A$1 {true} thm T {true} prf T$1 { 1. bycor A$1 |- true }""",  0)>]
    [<DataRow("03a", """thm T {true} prf T$1 { 1. bycor A$1 |- false }""",  1)>]
    [<DataRow("04cl", """def cl A thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // no predicates in definition
    [<DataRow("04pr", """def pred A() thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // no predicates in definition
    [<DataRow("04fu", """def func A()->obj thm T {true} prf T$1 { 1. bydef A |- false }""", 1)>] // no predicates in definition
    [<DataRow("04a", """prf T$1 { 1. bydef A |- false }""", 1)>]
    [<DataRow("05cl", """def cl A thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // no predicates in definition
    [<DataRow("05pr", """def pred A() thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // no predicates in definition
    [<DataRow("05fu", """def func A()->obj thm T {true} prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>] // no predicates in definition
    [<DataRow("05a", """prf T$1 {dec v:A; 1. bydef v |- false }""", 1)>]
    [<DataRow("06", """def cl N thm T {true} prf T$1 { 1: ex x:obj {is(x,N)} 2. 1 |- false}""", 0)>]
    [<DataRow("06a", """def cl N thm T {true} prf T$1 { 1: ex x:obj {is(x,N)} 2. 3 |- false}""", 1)>]

    [<DataRow("06", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- false ∧ true}", 0)>]
    [<DataRow("06a", "proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- false ∨ true}", 1)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestPR022(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR022 ""
            runTestHelper "TestPR022.fpl" fplCode code expected


