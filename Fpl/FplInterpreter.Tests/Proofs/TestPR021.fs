namespace FplInterpreter.Tests.Proofs

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreter.Globals.Debug
open CommonTestHelpers


[<TestClass>]
type TestPR021() =



    // AndCummutative and(p,q) 
    [<DataRow("AndC_01", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- false ∧ true}", 0)>]
    [<DataRow("AndC_01a", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(true,false) 2. 1, byinf AndCummutative |- false ∨ true}", 1)>]
    [<DataRow("AndC_03", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, not(all x:obj {is(x,N)})) 2. 1, byinf AndCummutative |- ¬(∀ x:obj {x is N}) ∧ ∀ x:obj {x is N}}", 0)>]
    [<DataRow("AndC_03a", "inf AndCummutative{dec p,q:pred; pre:and(p,q) con:and(q,p)} thm T {true} proof T$1 {1: and(all x:obj {is(x,N)}, not(all x:obj {is(x,N)})) 2. 1, byinf AndCummutative |- ¬(∀ x:obj {x is N}) ∧ ∀ x:obj {x is M}}", 1)>]
    [<TestMethod>]
    member this.TestPR021Inference(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = PR021 ("", "")
            runTestHelper "TestPR021.fpl" fplCode code expected

    

