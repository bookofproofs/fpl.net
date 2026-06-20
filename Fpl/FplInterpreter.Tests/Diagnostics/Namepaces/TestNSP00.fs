namespace Diagnostics.Namespaces

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open Fpl.Interpreter.Helpers.Debug
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig


[<TestClass>]
type TestNSP00() =

    [<TestMethod>]
    member this.TestNSP00() =
        let code = NSP00 "Bla.fpl"
        printf "Trying %s" code.Message
        let input = """
        uses Bla 
        """
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let uri = PathEquivalentUri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        fplInterpreter input uri fplLibUrl |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)

