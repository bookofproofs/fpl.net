namespace Diagnostics.ProofRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


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
