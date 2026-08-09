namespace Diagnostics.NamespaceRelated

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Fpl.Errors.Diagnostics
open FplInterpreter.Main
open TestFplInterpreter.Helpers.Common

(* NSP04
   Purpose: Report a circular theory reference discovered while resolving `uses` clauses.
   What it indicates: The requested theory import chain contains a cycle (a theory directly or indirectly references itself),
   making resolution ambiguous or non‑terminating.
   Use: Emitted during `uses`-clause processing to point to cycles in the theory dependency graph so authors can identify
   and break the loop.
   Action / Treat: Remove or refactor `uses` relationships to eliminate the cycle (rearrange imports, merge modules, or
   introduce an explicit dependency direction). Treat NSP04 as an error that must be resolved to successfully load the
   affected theories. *)

[<TestClass>]
type TestNSP04() =

    [<TestInitialize>]
    member _.Initialize() =
        ad.Clear()

    member private _.RunNSP04Test
        (
            rootFileName: string,
            rootInput: string,
            otherFiles: (string * string) list,
            expected: int
        ) =
        let currDir = Directory.GetCurrentDirectory()
        let rootPath = Path.Combine(currDir, rootFileName)
        let otherPaths = otherFiles |> List.map (fun (fileName, _) -> Path.Combine(currDir, fileName))
        let allPaths = rootPath :: otherPaths
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

        try
            File.WriteAllText(rootPath, rootInput)

            otherFiles
            |> List.iter (fun (fileName, fileContent) ->
                File.WriteAllText(Path.Combine(currDir, fileName), fileContent))

            let uri = PathEquivalentUri(rootPath)
            fplInterpreter rootInput uri fplLibUrl |> ignore

            let result = filterByErrorCode ad "NSP04"
            Assert.AreEqual<int>(expected, result.Length)
        finally
            allPaths
            |> List.iter (fun path ->
                if File.Exists(path) then
                    File.Delete(path))

    [<TestMethod>]
    member this.TestNSP04CircularAA() =
        let input = """uses TestNSP04_A;"""
        this.RunNSP04Test("TestNSP04_A.fpl", input, [], 1)

    [<TestMethod>]
    member this.TestNSP04CircularAB() =
        let input = """uses TestNSP04_B;"""
        let otherFiles = [ "TestNSP04_B.fpl", """uses TestNSP04_A;""" ]
        this.RunNSP04Test("TestNSP04_A.fpl", input, otherFiles, 1)

    [<TestMethod>]
    member this.TestNSP04CircularABCA() =
        let input = """uses TestNSP04_B;"""
        let otherFiles =
            [ "TestNSP04_B.fpl", """uses TestNSP04_C;"""
              "TestNSP04_C.fpl", """uses TestNSP04_A;""" ]
        this.RunNSP04Test("TestNSP04_A.fpl", input, otherFiles, 1)

    [<TestMethod>]
    member this.TestNSP04NonCircular() =
        let input = """
        uses TestNSP04_B
        uses TestNSP04_C
        ;"""
        let otherFiles =
            [ "TestNSP04_B.fpl", """;"""
              "TestNSP04_C.fpl", """;""" ]
        this.RunNSP04Test("TestNSP04_A.fpl", input, otherFiles, 0)
