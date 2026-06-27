namespace Diagnostics.SyntaxRelated


open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common


(* SY002
   Purpose: Report parser syntax failures that include FParsec backtracked alternatives as a grouped error chain.
   What it indicates: The parser could not successfully parse the input and produced one or more backtracked errors; each backtracked failure is wrapped into the SY002 chain.
   Use: Inspect the numbered entries in the SY002 chain to identify the primary parse error and the contributing backtracked diagnostics that help pinpoint the faulty syntax.
   Action / Treat: Correct the offending syntax (or adjust the source to match the grammar); treat SY002 as a blocking parse error that must be resolved for successful interpretation. *)


[<TestClass>]
type TestSY002() =


    [<DataRow("00", "def cl T {ctor T() {dec base.T(); }}", 0)>] 
    [<DataRow("01", "def cl T {ctor T() {dec base. (); }}", 2)>] 
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestSY002(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY002 ("","")
            runTestHelperWithoutSyntaxChecking "TestSY002.fpl" fplCode code expected


    [<DataRow("01", "def cl T {ctor T() {dec base. (); }}", 2, "001.1~001.2")>] 
    [<TestMethod>]
    member this.TestSY002ErrorChain(no:string, fplCode:string, expectedErrors, chain:string) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = SY002 ("","")
            let result = runTestHelperWithoutSyntaxCheckingGetResult "TestSY002ErrorChain.fpl" fplCode code expectedErrors
            let firstChain = result.Head.Message.Substring(19, 5)
            let lastChain = (result |> List.rev |> List.head).Message.Substring(19, 5)
            let chainActual = $"{firstChain}~{lastChain}"
            Assert.AreEqual<string>(chain, chainActual)

