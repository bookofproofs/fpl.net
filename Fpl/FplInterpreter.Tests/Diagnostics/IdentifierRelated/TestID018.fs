namespace Diagnostics.IdentifierRelated

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open TestFplInterpreter.Helpers.Common

(* ID018
   Purpose: Report when an extension pattern cannot be matched to any declared extension.
   What it indicates: Code attempted to use or resolve an extension defined by a pattern, but no declared extension fits that pattern in the current scope.
   Use: Helps locate unresolved extension usages so the author can add the appropriate extension declaration or correct the pattern/reference.
   Action / Treat: Declare an extension that matches the required pattern or adjust the reference/pattern so it matches an existing extension; treat ID018 as an error that must be resolved for extension resolution to succeed. *)


[<TestClass>]
type TestID018() =

    [<DataRow("00", @"ext Digits x @ /\d+/ -> N {return x} def pred T() {@1}", 0)>]
    [<DataRow("01", @"ext Alpha x @ /[a-z]+/ -> N {return x} ext Digits x @ /\d+/ -> P {ret x} def pred T() {@123}", 0)>]
    [<DataRow("02", @"ext Alpha x @ /[a-z]+/ -> N {return x} ext Digits x @ /\d+/ -> P {ret x} def pred T() {@abc}", 0)>]
    [<DataRow("03", @"ext Alpha x @ /[a-z]+/ -> N {return x} def pred T() {@123}", 1)>]
    [<DataRow("04", @"ext Digits x @ /\d+/ -> N {return x} def pred T() {@abc}", 1)>]
    [<DataRow("05", @"ext Alpha x @ /[a-z]+/ -> N {return x} def pred T() {@abc}", 0)>]
    [<DataRow("06", @"ext Digits x @ /\d+/ -> N {return (x = @0)}", 0)>]
    [<DataRow("99", "uses Fpl.Commons.Structures ", 0)>]
    [<TestMethod>]
    member this.TestID018(no:string, fplCode:string, expected) =
        if offlineWatcher.OfflineMode && fplCode.StartsWith("uses Fpl.") then 
            ()
        else
            let code = ID018 ""
            runTestHelper "TestID018.fpl" fplCode code expected
