namespace Diagnostics.NamespaceRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common
open TestSharedConfig

(* NSP05
   Purpose: Report ambiguous sources when the same theory is found in multiple locations.
   What it indicates: A `uses` resolution discovered more than one possible source for the requested theory; the interpreter selected one source but the choice may be unintended.
   Use: Draws attention to duplicate or conflicting theory providers so the author can disambiguate `uses` clauses or remove duplicate sources.
   Action / Treat: Make the desired source unambiguous (remove or rename duplicate theories, qualify the `uses` clause, or adjust repository/namespace configuration). NSP05 should be addressed to avoid inadvertently depending on the wrong theory implementation. *)

[<TestClass>]
type TestNSP05() =

    member this.PrepareTestNSP05a (delete:bool) =
        
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        deleteFiles currDir "Fpl.Commons.fpl"
        deleteFiles (Path.Combine(currDir, "lib")) "Fpl.Commons.fpl"
        File.WriteAllText(Path.Combine(currDir, "Fpl.Commons.fpl"), input)
        File.WriteAllText(Path.Combine(currDir, "lib", "Fpl.Commons.fpl"), input)
        let uri = PathEquivalentUri(Path.Combine(currDir, "Fpl.Commons.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            deleteFilesWithExtension (Path.Combine(currDir, "lib")) "fpl"
        else
            fplInterpreter input uri fplLibUrl |> ignore

    member this.PrepareTestNSP05 (delete:bool) =
        
        let input = """;"""
        let currDir = Directory.GetCurrentDirectory()

        deleteFiles currDir "Fpl.Commons.fpl"
        File.WriteAllText(Path.Combine(currDir, "Fpl.Commons.fpl"), input)
        let uri = PathEquivalentUri(Path.Combine(currDir, "Fpl.Commons.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
        else
            fplInterpreter input uri fplLibUrl |> ignore

    member this.PrepareTestNSP05CrossCheck (delete:bool) =
        
        let input = """ """
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Test.fpl"), input)
        let uri = PathEquivalentUri(Path.Combine(currDir, "Test.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
            deleteFilesWithExtension (Path.Combine(currDir, "lib")) "fpl"
        else
            fplInterpreter input uri fplLibUrl |> ignore

    [<TestMethod>]
    member this.TestNSP05() =
        if not TestConfig.IsOffline then 
            let code = NSP05 ( ["./"; "https"], "Fpl.Commons", "./")
            printf "Trying %s" code.Message
            this.PrepareTestNSP05(false) |> ignore
            let result = filterByErrorCode ad code.Code
            Assert.AreEqual<int>(1, result.Length)
            this.PrepareTestNSP05(true) |> ignore

    [<TestMethod>]
    member this.TestNSP05a() =
        let code = NSP05 (["./"; "./lib"; "https"], "Fpl.Commons", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05a(false) |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)
        this.PrepareTestNSP05a(true) |> ignore


    [<TestMethod>]
    member this.TestNSP05CrossCheck() =
        let code = NSP05 (["./"], "Test", "./")
        printf "Trying %s" code.Message
        this.PrepareTestNSP05CrossCheck(false) |> ignore
        Assert.AreEqual<int>(0, ad.CountDiagnostics)
        this.PrepareTestNSP05CrossCheck(true) |> ignore
