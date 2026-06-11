namespace FplInterpreter.Tests.Diagnostics.ErrRecovery

open Microsoft.VisualStudio.TestTools.UnitTesting
open ErrDiagnostics
open FplInterpreter.Globals.Debug
open CommonTestHelpers

(*
The SY002 diagnostic indicates a syntax error issued by FplParser that involve
the backtracking mechanism of the FParsec library, the FplParser is based on.
Each of the backtracked FParsec error will be wrapped into a separate SY002 diagnostic with
its own position in the faulty FPL code.

Because there is always more than one diagnostic (because there is at least a main FParsec syntax
error and a backtracked one), the diagnostics will be numbered.
The diagnostics numbered use the pattern XXX.Y where XXX is the global number of the SY002 main syntax error
and Y is the consecutive number 1,2,... of the backtracked error inside the main error.
*)

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

