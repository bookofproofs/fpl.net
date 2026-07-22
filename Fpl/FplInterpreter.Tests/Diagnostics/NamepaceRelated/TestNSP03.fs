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

    [<TestInitialize>]
    member _.Initialize() =
        ad.Clear()

    member private _.RunNSP03Test(input: string) =
        let currDir = Directory.GetCurrentDirectory()
        let rootPath = Path.Combine(currDir, "NSP03Coverage_Root.fpl")
        let sourceA = Path.Combine(currDir, "NSP03Coverage_A.fpl")
        let sourceB = Path.Combine(currDir, "NSP03Coverage_B.fpl")
        let sourceC = Path.Combine(currDir, "NSP03Coverage_C.fpl")

        File.WriteAllText(sourceA, ";")
        File.WriteAllText(sourceB, ";")
        File.WriteAllText(sourceC, ";")
        File.WriteAllText(rootPath, input)

        let uri = PathEquivalentUri(rootPath)
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

        try
            fplInterpreter input uri fplLibUrl |> ignore
            filterByErrorCode ad "NSP03"
        finally
            deleteFiles currDir "NSP03Coverage_*.fpl"

    [<TestMethod>]
    member this.TestNSP03() =
        let input = """
        uses NSP03Coverage_A alias T1
        uses NSP03Coverage_B alias T1
        """
        let result = this.RunNSP03Test input
        Assert.AreEqual<int>(1, result.Length)

    [<TestMethod>]
    member this.TestNSP03UniqueAliasesDoNotEmit() =
        let input = """
        uses NSP03Coverage_A alias T1
        uses NSP03Coverage_B alias T2
        """
        let result = this.RunNSP03Test input
        Assert.AreEqual<int>(0, result.Length)

    [<TestMethod>]
    member this.TestNSP03IgnoresStarAndEmptyAliases() =
        let input = """
        uses NSP03Coverage_A
        uses NSP03Coverage_B *
        """
        let result = this.RunNSP03Test input
        Assert.AreEqual<int>(0, result.Length)
