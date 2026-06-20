namespace Diagnostics.NamespaceRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestNSP03() =

    [<TestMethod>]
    member this.TestNSP03() =
        let code = NSP03 "T1"
        printf "Trying %s" code.Message
        let input = """
        uses Fpl1 alias T1
        uses Fpl2 alias T1
        """
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = PathEquivalentUri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        fplInterpreter input uri fplLibUrl |> ignore 
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)
