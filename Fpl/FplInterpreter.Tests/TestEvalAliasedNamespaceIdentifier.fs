namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterUsesClause
open CommonTestHelpers

[<TestClass>]
type TestEvalAliasedNamespaceIdentifier() =

    [<DataRow("*", "Test1", "Test1*.fpl")>]
    [<DataRow("*", "Test1.Test2", "Test1.Test2*.fpl")>]
    [<DataRow("T1", "Test1", "Test1.fpl")>]
    [<DataRow("T2", "Test1.Test2", "Test1.Test2.fpl")>]
    [<DataRow("", "Test1", "Test1.fpl")>]
    [<DataRow("", "Test1.Test2", "Test1.Test2.fpl")>]
    [<DataRow("", "Fpl.Commons", "Fpl.Commons.fpl")>]
    [<TestMethod>]
    member this.TestFileNamePattern(aliasOrStar: string, pascelCaseId: string, expected: string) =
        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let evalAlias = {
                EvalAlias.StartPos = pos
                EvalAlias.EndPos = pos
                EvalAlias.AliasOrStar = aliasOrStar
            }

        let eval = { 
                EvalAliasedNamespaceIdentifier.StartPos = pos
                EvalAliasedNamespaceIdentifier.EndPos = pos
                EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                EvalAliasedNamespaceIdentifier.PascalCaseIdList = pascelCaseId.Split('.') |> Array.toList 
            }

        Assert.AreEqual(expected, eval.FileNamePattern)

    [<DataRow("*", "Test1", "Test1")>]
    [<DataRow("*", "Test1.Test2", "Test1.Test2")>]
    [<DataRow("T1", "Test1", "T1")>]
    [<DataRow("T2", "Test1.Test2", "T2")>]
    [<DataRow("", "Test1", "Test1")>]
    [<DataRow("", "Test1.Test2", "Test1.Test2")>]
    [<TestMethod>]
    member this.TestName(aliasOrStar: string, pascelCaseId: string, expected: string) =
        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let evalAlias = {
                EvalAlias.StartPos = pos
                EvalAlias.EndPos = pos
                EvalAlias.AliasOrStar = aliasOrStar
            }

        let eval =
            { 
                EvalAliasedNamespaceIdentifier.StartPos = pos
                EvalAliasedNamespaceIdentifier.EndPos = pos
                EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                EvalAliasedNamespaceIdentifier.PascalCaseIdList = [ pascelCaseId ] 
            }

        Assert.AreEqual(expected, eval.Name)

    [<TestMethod>]
    member this.TestCreateLibSubfolder01() =
        let uri = System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let expected = Directory.GetCurrentDirectory()
        let (directoryPath, libDirectoryPath) = createLibSubfolder uri
        Assert.AreEqual(expected, directoryPath)

        if Directory.Exists(libDirectoryPath) then
            Directory.Delete(libDirectoryPath, true)

    [<TestMethod>]
    member this.TestCreateLibSubfolder02() =
        let uri = System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let expected = Path.Combine(Directory.GetCurrentDirectory(), "lib")
        let (directoryPath, libDirectoryPath) = createLibSubfolder uri
        Assert.AreEqual(expected, libDirectoryPath)

        if Directory.Exists(libDirectoryPath) then
            Directory.Delete(libDirectoryPath, true)

    [<TestMethod>]
    member this.TestCreateLibSubfolder03() =
        let uri = System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let expected = Path.Combine(Directory.GetCurrentDirectory(), "lib")
        let (directoryPath, libDirectoryPath) = createLibSubfolder uri
        Assert.IsTrue(Directory.Exists(libDirectoryPath))

        if Directory.Exists(libDirectoryPath) then
            Directory.Delete(libDirectoryPath, true)

    [<TestMethod>]
    member this.TestDownloadLibMap01() =
        let url = "https://github.com/bookofproofs/fpl.net/blob/main/theories/lib"
        ad.Clear()
        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let evalAlias = {
                EvalAlias.StartPos = pos
                EvalAlias.EndPos = pos
                EvalAlias.AliasOrStar = ""
            }
        let e = EvalAliasedNamespaceIdentifier.CreateEani([],evalAlias,pos,pos)
        let libMap = downloadLibMap url
        Assert.IsTrue(libMap.Length > 0)

    [<TestMethod>]
    member this.TestDownloadLibMap02() =
        let url = "https://github.com/bookofproofs/fpl.net/blob/main/theories/lib"
        ad.Clear()
        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let evalAlias = {
                EvalAlias.StartPos = pos
                EvalAlias.EndPos = pos
                EvalAlias.AliasOrStar = ""
            }
        let e = EvalAliasedNamespaceIdentifier.CreateEani([],evalAlias,pos,pos)
        let libMap = downloadLibMap url
        Assert.AreEqual(0, ad.CountDiagnostics)

    [<DataRow("Fpl *", 4)>]
    [<DataRow("Fpl.Test *", 0)>]
    [<DataRow("Fpl.Commons *", 2)>]
    [<DataRow("Fpl.Commons", 1)>]
    [<DataRow("Fpl.SetTheory", 1)>]
    [<DataRow("FpX *", 0)>]
    [<TestMethod>]
    member this.TestFindFilesInLibMapWithWildcard(usesClause: string, expected) =
        let st = prepareFplCode (sprintf "uses %s;" usesClause, false) 
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        
        let uri =
            System.Uri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let sources = acquireSources uri fplLibUrl
        let eaniList = eval_uses_clause st.Value.ParsedAsts[0].Parsing.Ast

        let filtered = sources.FindWithPattern eaniList.Head.FileNamePattern
        Assert.AreEqual(expected, filtered.Length)
        prepareFplCode ("", true) |> ignore


    [<DataRow("Fpl *", 5)>]
    [<DataRow("Fpl.Commons *", 5)>]
    [<DataRow("Fpl alias T1", 1)>]
    [<DataRow("Fpl.Commons alias T2", 2)>]
    [<DataRow("Fpl", 1)>]
    [<DataRow("Fpl.Commons", 2)>]
    [<TestMethod>]
    member this.TestParsedAstsCount(usesClause: string, expected: int) =
        let st = prepareFplCode (sprintf "uses %s;" usesClause, false) 
        let result = st.Value.ParsedAsts
        Assert.AreEqual(expected, result.Count)
        prepareFplCode ("", true) |> ignore

    [<DataRow("Test1")>]
    [<DataRow("Test1.Test2")>]
    [<DataRow("Test1")>]
    [<DataRow("Test1.Test2")>]
    [<DataRow("Test1")>]
    [<DataRow("Test1.Test2")>]
    [<TestMethod>]
    member this.TestAcquireSourcesWebAndCurrDir(pascelCaseId: string) =
        // prepare test
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), pascelCaseId + ".fpl")

        deleteFilesWithExtension (Directory.GetCurrentDirectory()) "fpl"
        deleteFilesWithExtension (Path.Combine(Directory.GetCurrentDirectory(), "lib")) "fpl"

        File.WriteAllText(pathToFile, ";")

        let uri = System.Uri(pathToFile)

        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

        // perform test
        let sources = acquireSources uri fplLibUrl 
        Assert.AreEqual(5, sources.Length)

        // clean up test
        File.Delete(pathToFile)

    [<TestMethod>]
    member this.TestComputeMD5Checksum() =
        let input = "Hello world"
        let actual = computeMD5Checksum input
        let expected = "3e25960a79dbc69b674cd4ec67a72c62"
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.TestFplSourcesUrls() =
        let fplSources = FplSources(["c:\temp\Test1.fpl"; "c:\temp\Test2.fpl"; "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib/Test3.fpl"])
        Assert.AreEqual(1, fplSources.Urls.Length)

    [<TestMethod>]
    member this.TestFplSourcesFiles() =
        let fplSources = FplSources(["c:\temp\Test1.fpl"; "c:\temp\Test2.fpl"; "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib/Test3.fpl"])
        Assert.AreEqual(2, fplSources.FilePaths.Length)

    member this.PrepareTestLoadAllUsesClauses01() =
        let input = """
            uses Fpl.Commons
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl")
        let uri = System.Uri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let parsedAsts = System.Collections.Generic.List<ParsedAst>()
        loadAllUsesClauses input uri fplLibUrl parsedAsts
        parsedAsts


    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Number() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        Assert.AreEqual(2, result.Count)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        Assert.AreEqual("Test", result.Find(fun pa -> pa.Id = "Test").Id)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        Assert.AreEqual("Fpl.Commons", result.Find(fun pa -> pa.Id = "Fpl.Commons").Id)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Test" knows that it references to "Fpl.Commons"
        Assert.AreEqual(["Fpl.Commons"], result.Find(fun pa -> pa.Id = "Test").Sorting.ReferencedAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Test" knows that nothing is referencing to it
        Assert.AreEqual([], result.Find(fun pa -> pa.Id = "Test").Sorting.ReferencingAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Fpl.Commons" knows that it doesn't reference to anything
        Assert.AreEqual([], result.Find(fun pa -> pa.Id = "Fpl.Commons").Sorting.ReferencedAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Fpl.Commons" knows that "Test" is referencing to it
        Assert.AreEqual(["Test"], result.Find(fun pa -> pa.Id = "Fpl.Commons").Sorting.ReferencingAsts)

    member this.PrepareTestLoadAllUsesClauses02() =
        let input = """
            uses Fpl.Commons
            uses Fpl.SetTheory
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl")
        let uri = System.Uri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let parsedAsts = System.Collections.Generic.List<ParsedAst>()
        loadAllUsesClauses input uri fplLibUrl parsedAsts
        parsedAsts

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Number() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        Assert.AreEqual(3, result.Count)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        let actual = result.Find(fun pa -> pa.Id = "Test").Id
        Assert.AreEqual("Test", actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Test" knows that it references to "Fpl.Commons"
        let actual = result.Find(fun pa -> pa.Id = "Test").Sorting.ReferencedAsts
        Assert.AreEqual(["Fpl.Commons"; "Fpl.SetTheory"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Test" knows that nothing is referencing to it
        let actual = result.Find(fun pa -> pa.Id = "Test").Sorting.ReferencingAsts
        Assert.AreEqual([], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        let actual = result.Find(fun pa -> pa.Id = "Fpl.SetTheory").Id
        Assert.AreEqual("Fpl.SetTheory", actual)


    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.SetTheory" references to FplCommons
        let actual = result.Find(fun pa -> pa.Id = "Fpl.SetTheory").Sorting.ReferencedAsts
        Assert.AreEqual(["Fpl.Commons"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.Commons" knows that "Test" is referencing to it
        let actual = result.Find(fun pa -> pa.Id = "Fpl.SetTheory").Sorting.ReferencingAsts
        Assert.AreEqual(["Test"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        let actual = result.Find(fun pa -> pa.Id = "Fpl.Commons").Id
        Assert.AreEqual("Fpl.Commons", actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.Commons" knows that it doesn't reference to anything
        let actual = result.Find(fun pa -> pa.Id = "Fpl.Commons").Sorting.ReferencedAsts
        Assert.AreEqual([], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.Commons" knows that "Test" is referencing to it
        let actual = result.Find(fun pa -> pa.Id = "Fpl.Commons").Sorting.ReferencingAsts
        Assert.AreEqual(["Test"; "Fpl.SetTheory"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClausesTopologicalSorting02() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.Commons" knows that "Test" is referencing to it
        let a = result.Find(fun pa -> pa.Id = "Fpl.Commons").Sorting.TopologicalSorting
        let b = result.Find(fun pa -> pa.Id = "Fpl.SetTheory").Sorting.TopologicalSorting
        let c = result.Find(fun pa -> pa.Id = "Test").Sorting.TopologicalSorting
        Assert.AreEqual(2, a)
        Assert.AreEqual(1, b)
        Assert.AreEqual(0, c)

    [<TestMethod>]
    member this.TestLoadAllUsesClausesTopologicalSorting01() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Fpl.Commons" knows that "Test" is referencing to it
        let a = result.Find(fun pa -> pa.Id = "Fpl.Commons").Sorting.TopologicalSorting
        let c = result.Find(fun pa -> pa.Id = "Test").Sorting.TopologicalSorting
        Assert.AreEqual(1, a)
        Assert.AreEqual(0, c)
    

