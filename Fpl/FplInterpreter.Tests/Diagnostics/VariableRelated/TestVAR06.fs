namespace Diagnostics.VariableRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* VAR06
   Purpose: Flag variables in an inner/derived scope that overshadow inherited variables.
   What it indicates: A variable declared in a derived or inner scope has the same name as one inherited from a base scope, causing shadowing.
   Use: Make authors aware of possible unintended behavior changes introduced by overriding names from a base definition.
   Action / Treat: Verify whether shadowing is intentional; if not, rename or refactor to preserve the original binding. Treat VAR06 as a warning to avoid accidental name collisions across inheritance. *)

[<TestClass>]
type TestVAR06() =

    [<DataRow("00a", "def cl T { dec x:obj; ctor T() { dec base.Obj() ; }} def cl S:T { dec x:obj; ctor S() { dec base.T() ; }} ", 1)>]
    [<DataRow("00b", "def cl T { dec x:obj; ctor T() { dec base.Obj() ; }} def cl S:T { dec y:obj; ctor S() { dec base.T() ; }} ", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR06Classes(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR06 ("","","","")
            runTestHelper "TestVAR06Classes.fpl" fplCode code expected
            
    // base inner / derived inner 
    [<DataRow("IIa", "def func T()->obj { dec x:obj; return x} def func S:T()->obj { dec x:obj; return x } ", 1)>]
    [<DataRow("IIb", "def func T()->obj { dec x:obj; return x} def func S:T()->obj { dec y:obj; return y} ", 0)>]
    [<DataRow("IIc", "def func T()->*obj[ind] { dec x:*obj[ind]; return x} def func S:T()->*obj[ind] { dec x:*obj[ind]; return x } ", 1)>]
    [<DataRow("IId", "def func T()->*obj[ind] { dec x:*obj[ind]; return x} def func S:T()->*obj[ind] { dec y:*obj[ind]; return y} ", 0)>]
    [<DataRow("IIe", "def func T()->*obj[ind] { dec x:*obj[ind]; return x} def func S:T()->*obj[ind] { dec x:obj; return x } ", 1)>]
    [<DataRow("IIf", "def func T()->*obj[ind] { dec x:*obj[ind]; return x} def func S:T()->*obj[ind] { dec y:obj; return y} ", 0)>]
    [<DataRow("IIg", "def func T()->obj { dec x:obj; return x} def func S:T()->obj { dec x:*obj[ind]; return x } ", 1)>]
    [<DataRow("IIh", "def func T()->obj { dec x:obj; return x} def func S:T()->obj { dec y:*obj[ind]; return y} ", 0)>]
    // base inner / derived signature 
    [<DataRow("ISa", "def func T(a:obj)->obj { dec x:obj; return x} def func S:T(x:obj)->obj ", 1)>]
    [<DataRow("ISb", "def func T(a:obj)->obj { dec x:obj; return x} def func S:T(y:obj)->obj ", 0)>]
    [<DataRow("ISc", "def func T(a:obj)->*obj[ind] { dec x:*obj[ind]; return x} def func S:T(x:obj)->*obj[ind] ", 1)>]
    [<DataRow("ISd", "def func T(a:obj)->*obj[ind] { dec x:*obj[ind]; return x} def func S:T(y:obj)->*obj[ind] ", 0)>]
    // base inner / derived signature (will cause SIG07 instead, VAR06 is only possible if SIG07 is not issued)
    [<DataRow("ISe", "def func T(a:obj)->*obj[ind] { dec x:*obj[ind]; return x} def func S:T(x:*obj[ind])->*obj[ind] ", 0)>]
    [<DataRow("ISf", "def func T(a:obj)->*obj[ind] { dec x:*obj[ind]; return x} def func S:T(y:*obj[ind])->*obj[ind] ", 0)>]
    [<DataRow("ISg", "def func T(a:*obj[ind])->*obj[ind] { dec x:*obj[ind]; return x} def func S:T(x:obj)->*obj[ind] ", 0)>]
    [<DataRow("ISh", "def func T(a:*obj[ind])->*obj[ind] { dec x:*obj[ind]; return x} def func S:T(y:obj)->*obj[ind] ", 0)>]
    // base inner / derived pred mapping 
    [<DataRow("IPa", "def func T()->pred(z:obj) { dec x:obj; return x} def func S:T()->pred(x:obj) ", 1)>]
    [<DataRow("IPb", "def func T()->pred(z:obj) { dec x:obj; return x} def func S:T()->pred(y:obj) ", 0)>]
    [<DataRow("IPc", "def func T()->pred(z:*obj[ind]) { dec x:*obj[ind]; return x} def func S:T()->pred(x:*obj[ind]) ", 1)>]
    [<DataRow("IPd", "def func T()->pred(z:*obj[ind]) { dec x:*obj[ind]; return x} def func S:T()->pred(y:*obj[ind]) ", 0)>]
    // base inner / derived pred mapping (will cause SIG07 instead, VAR06 is only possible if SIG07 is not issued)
    [<DataRow("IPe", "def func T()->pred(z:*obj[ind]) { dec x:*obj[ind]; return x} def func S:T()->pred(x:obj) ", 0)>]
    [<DataRow("IPf", "def func T()->pred(z:*obj[ind]) { dec x:*obj[ind]; return x} def func S:T()->pred(y:obj) ", 0)>]
    [<DataRow("IPg", "def func T()->pred(z:obj) { dec x:*obj[ind]; return x} def func S:T()->pred(x:*obj[ind]) ", 0)>]
    [<DataRow("IPh", "def func T()->pred(z:obj) { dec x:*obj[ind]; return x} def func S:T()->pred(y:*obj[ind]) ", 0)>]
    // base inner / derived func mapping 
    [<DataRow("IFa", "def func T()->func(z:obj)->obj { dec x:obj; return x} def func S:T()->func(x:obj)->obj ", 1)>]
    [<DataRow("IFb", "def func T()->func(z:obj)->obj { dec x:obj; return x} def func S:T()->func(y:obj)->obj ", 0)>]
    [<DataRow("IFc", "def func T()->func(z:*obj[ind])->obj { dec x:*obj[ind]; return x} def func S:T()->func(x:*obj[ind])->obj ", 1)>]
    [<DataRow("IFd", "def func T()->func(z:*obj[ind])->obj { dec x:*obj[ind]; return x} def func S:T()->func(y:*obj[ind])->obj ", 0)>]
    // base signature / derived inner 
    [<DataRow("SIa", "def func T(x:obj)->obj def func S:T(z:obj)->obj { dec x:obj; return x } ", 1)>]
    [<DataRow("SIb", "def func T(x:obj)->obj def func S:T(z:obj)->obj { dec y:obj; return y } ", 0)>]
    [<DataRow("SIc", "def func T(x:*obj[ind])->obj def func S:T(z:*obj[ind])->obj { dec x:*obj[ind]; return x } ", 1)>]
    [<DataRow("SId", "def func T(x:*obj[ind])->obj def func S:T(z:*obj[ind])->obj { dec y:*obj[ind]; return y } ", 0)>]
    // base signature / derived signature 
    [<DataRow("SSa", "def func T(x:obj)->obj def func S:T(x:obj)->obj ", 1)>]
    [<DataRow("SSb", "def func T(x:obj)->obj def func S:T(y:obj)->obj ", 0)>]
    [<DataRow("SSc", "def func T(x:*obj[ind])->obj def func S:T(x:*obj[ind])->obj ", 1)>]
    [<DataRow("SSd", "def func T(x:*obj[ind])->obj def func S:T(y:*obj[ind])->obj ", 0)>]
    // base signature / derived pred mapping 
    [<DataRow("SPa", "def func T(x:obj)->pred(a:obj) def func S:T(z:obj)->pred(x:obj) ", 1)>]
    [<DataRow("SPb", "def func T(x:obj)->pred(a:obj) def func S:T(z:obj)->pred(y:obj) ", 0)>]
    [<DataRow("SPc", "def func T(x:*obj[ind])->pred(a:*obj[ind]) def func S:T(z:*obj[ind])->pred(x:*obj[ind]) ", 1)>]
    [<DataRow("SPd", "def func T(x:*obj[ind])->pred(a:*obj[ind]) def func S:T(z:*obj[ind])->pred(y:*obj[ind]) ", 0)>]
    // base signature / derived func mapping 
    [<DataRow("SFa", "def func T(x:obj)->func(a:obj)->obj def func S:T(z:obj)->func(x:obj)->obj ", 1)>]
    [<DataRow("SFb", "def func T(x:obj)->func(a:obj)->obj def func S:T(z:obj)->func(y:obj)->obj ", 0)>]
    [<DataRow("SFc", "def func T(x:*obj[ind])->func(a:*obj[ind])->obj def func S:T(z:*obj[ind])->func(x:*obj[ind])->obj ", 1)>]
    [<DataRow("SFd", "def func T(x:*obj[ind])->func(a:*obj[ind])->obj def func S:T(z:*obj[ind])->func(y:*obj[ind])->obj ", 0)>]
    // base pred mapping / derived inner 
    [<DataRow("PIa", "def func T()->pred(x:obj) def func S:T()->pred(z:obj) { dec x:obj; return true} ", 1)>]
    [<DataRow("PIb", "def func T()->pred(x:obj) def func S:T()->pred(z:obj) { dec y:obj; return true} ", 0)>]
    [<DataRow("PIc", "def func T()->pred(x:*obj[ind]) def func S:T()->pred(z:*obj[ind]) { dec x:*obj[ind]; return true} ", 1)>]
    [<DataRow("PId", "def func T()->pred(x:*obj[ind]) def func S:T()->pred(z:*obj[ind]) { dec y:*obj[ind]; return true} ", 0)>]
    // base pred mapping / derived signature 
    [<DataRow("PSa", "def func T(a:obj)->pred(x:obj) def func S:T(z:obj)->pred(x:obj) ", 1)>]
    [<DataRow("PSb", "def func T(a:obj)->pred(x:obj) def func S:T(z:obj)->pred(y:obj) ", 0)>]
    [<DataRow("PSc", "def func T(a:*obj[ind])->pred(x:*obj[ind]) def func S:T(z:*obj[ind])->pred(x:*obj[ind]) ", 1)>]
    [<DataRow("PSd", "def func T(a:*obj[ind])->pred(x:*obj[ind]) def func S:T(z:*obj[ind])->pred(y:*obj[ind]) ", 0)>]
    // base pred mapping / derived pred mapping 
    [<DataRow("PPa", "def func T()->pred(x:obj) def func S:T()->pred(x:obj) ", 1)>]
    [<DataRow("PPb", "def func T()->pred(x:obj) def func S:T()->pred(y:obj) ", 0)>]
    [<DataRow("PPc", "def func T()->pred(x:*obj[ind]) def func S:T()->pred(x:*obj[ind]) ", 1)>]
    [<DataRow("PPd", "def func T()->pred(x:*obj[ind]) def func S:T()->pred(y:*obj[ind]) ", 0)>]
    // base pred mapping / derived func mapping (will cause SIG07 instead, VAR06 is only possible if SIG07 is not issued)
    [<DataRow("PFa", "def func T()->pred(x:obj) def func S:T()->func(x:obj)->obj ", 0)>]
    [<DataRow("PFb", "def func T()->pred(x:obj) def func S:T()->func(y:obj)->obj ", 0)>]
    [<DataRow("PFc", "def func T()->pred(x:*obj[ind]) def func S:T()->func(x:*obj[ind])->obj ", 0)>]
    [<DataRow("PFd", "def func T()->pred(x:*obj[ind]) def func S:T()->func(y:*obj[ind])->obj ", 0)>]
    // base func mapping / derived inner
    [<DataRow("FIa", "def func T()->func(x:obj)->obj def func S:T()->func(z:obj)->obj { dec x:obj; return x } ", 1)>]
    [<DataRow("FIb", "def func T()->func(x:obj)->obj def func S:T()->func(z:obj)->obj { dec y:obj; return y } ", 0)>]
    [<DataRow("FIc", "def func T()->func(x:*obj[ind])->obj def func S:T()->func(z:*obj[ind])->obj { dec x:*obj[ind]; return x } ", 1)>]
    [<DataRow("FId", "def func T()->func(x:*obj[ind])->obj def func S:T()->func(z:*obj[ind])->obj { dec y:*obj[ind]; return y } ", 0)>]
    // base func mapping / derived signature 
    [<DataRow("FSa", "def func T(a:obj)->func(x:obj)->obj def func S:T(x:obj)->func(z:obj)->obj ", 1)>]
    [<DataRow("FSb", "def func T(a:obj)->func(x:obj)->obj def func S:T(y:obj)->func(z:obj)->obj ", 0)>]
    [<DataRow("FSc", "def func T(a:*obj[ind])->func(x:*obj[ind])->obj def func S:T(x:*obj[ind])->func(z:*obj[ind])->obj ", 1)>]
    [<DataRow("FSd", "def func T(a:*obj[ind])->func(x:*obj[ind])->obj def func S:T(y:*obj[ind])->func(z:*obj[ind])->obj ", 0)>]
    // base func mapping / derived pred mapping (will cause SIG07 instead, VAR06 is only possible if SIG07 is not issued)
    [<DataRow("FPa", "def func T()->func(x:obj)->obj def func S:T()->pred(x:obj) ", 0)>]
    [<DataRow("FPb", "def func T()->func(x:obj)->obj def func S:T()->pred(y:obj) ", 0)>]
    [<DataRow("FPc", "def func T()->func(x:*obj[ind])->obj def func S:T()->pred(x:*obj[ind]) ", 0)>]
    [<DataRow("FPd", "def func T()->func(x:*obj[ind])->obj def func S:T()->pred(y:*obj[ind]) ", 0)>]
    // base func mapping / derived func mapping 
    [<DataRow("FFa", "def func T()->func(x:obj)->obj def func S:T()->func(x:obj)->obj ", 1)>]
    [<DataRow("FFb", "def func T()->func(x:obj)->obj def func S:T()->func(y:obj)->obj ", 0)>]
    [<DataRow("FFc", "def func T()->func(x:*obj[ind])->obj def func S:T()->func(x:*obj[ind])->obj ", 1)>]
    [<DataRow("FFd", "def func T()->func(x:*obj[ind])->obj def func S:T()->func(y:*obj[ind])->obj ", 0)>]
    // general
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestVAR06FunctionalTerms(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = VAR06 ("","","","")
            runTestHelper "TestVAR06FunctionalTerms.fpl" fplCode code expected
