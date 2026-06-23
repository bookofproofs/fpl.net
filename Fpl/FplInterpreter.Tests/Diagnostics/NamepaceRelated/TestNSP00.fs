namespace Diagnostics.NamespaceRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common

(* NSP00
   Purpose: Report that no FPL source matched a `uses`-clause filename pattern.
   What it indicates: Resolution for the requested theory returned no candidate files (the search pattern did not match any local or remote sources).
   Use: Emitted during `uses`-clause processing to pinpoint missing dependency files so authors can correct the file pattern or provide the required sources.
   Action / Treat: Verify the `uses` pattern and available source locations (current directory, lib/repo folders, or remote registry); add or rename the missing files, adjust the pattern, or fix repository configuration. Treat NSP00 as an error that must be resolved to load the referenced theory. *)

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

