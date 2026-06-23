namespace Diagnostics.NamespaceRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common

(* NSP04
   Purpose: Report a circular theory reference discovered while resolving `uses` clauses.
   What it indicates: The requested theory import chain contains a cycle (a theory directly or indirectly references itself), making resolution ambiguous or non‑terminating.
   Use: Emitted during `uses`-clause processing to point to cycles in the theory dependency graph so authors can identify and break the loop.
   Action / Treat: Remove or refactor `uses` relationships to eliminate the cycle (rearrange imports, merge modules, or introduce an explicit dependency direction). Treat NSP04 as an error that must be resolved to successfully load the affected theories. *)

[<TestClass>]
type TestNSP04() =

    member this.PrepareTestNSP04CircularABCA(delete:bool) =
        
        let A = """uses Test2_B;"""
        let B = """uses Test2_C;"""
        let C = """uses Test2_A;"""
        let currDir = Directory.GetCurrentDirectory()

        File.WriteAllText(Path.Combine(currDir, "Test2_A.fpl"), A)
        File.WriteAllText(Path.Combine(currDir, "Test2_B.fpl"), B)
        File.WriteAllText(Path.Combine(currDir, "Test2_C.fpl"), C)
        let uri = PathEquivalentUri(Path.Combine(currDir, "Test2_A.fpl"))
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            deleteFilesWithExtension currDir "fpl"
        else

            fplInterpreter A uri fplLibUrl |> ignore

    member this.PrepareTestNSP04CircularAA(delete:bool) =
        
        let A = """uses Test1_A;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test1_A.fpl")
        File.WriteAllText(pathToFile, A)
        let uri = PathEquivalentUri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            File.Delete(pathToFile)
        else
            fplInterpreter A uri fplLibUrl |> ignore

    member this.PrepareTestNSP04NonCircular(delete:bool) =
        
        let input = """
            uses Fpl.Commons
            uses Fpl.SetTheory
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test1_A.fpl")
        File.WriteAllText(pathToFile, input)
        let uri = PathEquivalentUri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        if delete then 
            File.Delete(pathToFile)
        else
            fplInterpreter input uri fplLibUrl |> ignore


    [<TestMethod>]
    member this.TestNSP04CircularAA() =
        let code = NSP04 "Test1_A -> Test1_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP04CircularAA(false) |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)
        this.PrepareTestNSP04CircularAA(true) |> ignore

    [<TestMethod>]
    member this.TestNSP04CircularABCA() =
        let code = NSP04 "Test2_A -> Test2_B -> Test2_C -> Test2_A"
        printf "Trying %s" code.Message
        this.PrepareTestNSP04CircularABCA(false) |> ignore
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(1, result.Length)
        this.PrepareTestNSP04CircularABCA(true) |> ignore

    [<TestMethod>]
    member this.TestNSP04NonCircular() =
        this.PrepareTestNSP04NonCircular(false) |> ignore
        let code = NSP04 ""
        let result = filterByErrorCode ad code.Code
        Assert.AreEqual<int>(0, result.Length)
        this.PrepareTestNSP04NonCircular(true) |> ignore
