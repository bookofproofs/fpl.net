namespace Diagnostics.NamespaceRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common

(* NSP03
   Purpose: Report a repeated alias declaration inside a namespace.
   What it indicates: A `uses` alias was declared more than once in the same namespace scope, producing a naming conflict for that alias.
   Use: Emitted while processing `uses` clauses to point authors to duplicate alias declarations so they can disambiguate imports.
   Action / Treat: Remove or rename the conflicting alias (or consolidate the `uses` clauses) so each alias is unique within the namespace; NSP03 is an error that must be resolved so imports are unambiguous. *)

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
