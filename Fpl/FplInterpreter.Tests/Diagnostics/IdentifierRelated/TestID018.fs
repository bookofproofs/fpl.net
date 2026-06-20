namespace Diagnostics.IdentifierRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


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
