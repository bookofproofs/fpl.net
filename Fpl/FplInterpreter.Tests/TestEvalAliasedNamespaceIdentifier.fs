namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting

open FParsec
open ErrDiagnostics
open FplInterpreterUsesClause

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
        let e = 
            { 
                EvalAliasedNamespaceIdentifier.StartPos = pos
                EvalAliasedNamespaceIdentifier.EndPos = pos 
                EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                EvalAliasedNamespaceIdentifier.PascalCaseIdList = [] }
        let libMap = downloadLibMap url ad e
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
        let e = 
            { 
                EvalAliasedNamespaceIdentifier.StartPos = pos
                EvalAliasedNamespaceIdentifier.EndPos = pos 
                EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                EvalAliasedNamespaceIdentifier.PascalCaseIdList = [] }
        let libMap = downloadLibMap url ad e
        Assert.AreEqual(ad.CountDiagnostics, 0)


    [<DataRow("Fpl.*.fpl", 4)>]
    [<DataRow("Fpl.Test.*.fpl", 2)>]
    [<DataRow("Fpl.Commons.fpl", 1)>]
    [<DataRow("Fpl.SetTheory.fpl", 1)>]
    [<DataRow("FpX.*.fpl", 0)>]
    [<TestMethod>]
    member this.TestFindFilesInLibMapWithWildcard(wildcards: string, expected) =
        let sitelib =
            "Fpl.Commons.fpl\nFpl.SetTheory.fpl\nFpl.Test.Test1.fpl\nFpl.Test.Test2.fpl"

        let filteredList = findFilesInLibMapWithWildcard sitelib wildcards
        Assert.AreEqual(expected, filteredList.Length)

    [<DataRow("*", "Fpl", 2)>]
    [<DataRow("*", "Fpl.Commons", 1)>]
    [<DataRow("T1", "Fpl", 0)>]
    [<DataRow("T2", "Fpl.Commons", 1)>]
    [<DataRow("", "Fpl", 0)>]
    [<DataRow("", "Fpl.Commons", 1)>]
    [<TestMethod>]
    member this.TestAcquireSourcesWebOnly(aliasOrStar: string, pascelCaseId: string, expected: int) =
        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let evalAlias = {
                EvalAlias.StartPos = pos
                EvalAlias.EndPos = pos
                EvalAlias.AliasOrStar = aliasOrStar
            }

        let eval =
                { EvalAliasedNamespaceIdentifier.StartPos = Position("", 1, 1, 1)
                  EvalAliasedNamespaceIdentifier.EndPos = Position("", 1, 1, 1)
                  EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                  EvalAliasedNamespaceIdentifier.PascalCaseIdList = [ pascelCaseId ] }

        let uri =
            System.Uri(Path.Combine(Directory.GetCurrentDirectory(), pascelCaseId + ".fpl"))

        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

        let sources = acquireSources eval uri fplLibUrl ad
        Assert.AreEqual(expected, sources.Urls.Length)

    [<DataRow("*", "Test1")>]
    [<DataRow("*", "Test1.Test2")>]
    [<DataRow("T1", "Test1")>]
    [<DataRow("T2", "Test1.Test2")>]
    [<DataRow("", "Test1")>]
    [<DataRow("", "Test1.Test2")>]
    [<TestMethod>]
    member this.TestAcquireSourcesWebAndCurrDir(aliasOrStar: string, pascelCaseId: string) =
        // prepare test
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), pascelCaseId + ".fpl")

        File.WriteAllText(pathToFile, ";")

        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let evalAlias = {
                EvalAlias.StartPos = pos
                EvalAlias.EndPos = pos
                EvalAlias.AliasOrStar = aliasOrStar
            }

        let eval =
                { EvalAliasedNamespaceIdentifier.StartPos = Position("", 1, 1, 1)
                  EvalAliasedNamespaceIdentifier.EndPos = Position("", 1, 1, 1)
                  EvalAliasedNamespaceIdentifier.EvalAlias = evalAlias
                  EvalAliasedNamespaceIdentifier.PascalCaseIdList = [ pascelCaseId ] }

        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let uri = System.Uri(pathToFile)

        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

        // perform test
        let sources = acquireSources eval uri fplLibUrl ad
        Assert.AreEqual(sources.Length, 1)

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
        loadAllUsesClauses input uri fplLibUrl 


    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Number() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        Assert.AreEqual(2, result.ParsedAsts.Count)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        Assert.AreEqual("Test", result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Test").Id)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        Assert.AreEqual("Fpl.Commons", result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.Commons").Id)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Test" knows that it references to "Fpl.Commons"
        Assert.AreEqual(["Fpl.Commons"], result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Test").ReferencedAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Test" knows that nothing is referencing to it
        Assert.AreEqual([], result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Test").ReferencingAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Fpl.Commons" knows that it doesn't reference to anything
        Assert.AreEqual([], result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.Commons").ReferencedAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        // "Fpl.Commons" knows that "Test" is referencing to it
        Assert.AreEqual(["Test"], result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.Commons").ReferencingAsts)

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
        loadAllUsesClauses input uri fplLibUrl 

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Number() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        Assert.AreEqual(3, result.ParsedAsts.Count)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Test").Id
        Assert.AreEqual("Test", actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Test" knows that it references to "Fpl.Commons"
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Test").ReferencedAsts
        Assert.AreEqual(["Fpl.Commons"; "Fpl.SetTheory"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Test" knows that nothing is referencing to it
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Test").ReferencingAsts
        Assert.AreEqual([], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.SetTheory").Id
        Assert.AreEqual("Fpl.SetTheory", actual)


    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.Commons" knows that it doesn't reference to anything
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.SetTheory").ReferencedAsts
        Assert.AreEqual(["Fpl.Commons"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.Commons" knows that "Test" is referencing to it
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.SetTheory").ReferencingAsts
        Assert.AreEqual(["Test"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.Commons").Id
        Assert.AreEqual("Fpl.Commons", actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.Commons" knows that it doesn't reference to anything
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.Commons").ReferencedAsts
        Assert.AreEqual([], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02()
        // "Fpl.Commons" knows that "Test" is referencing to it
        let actual = result.ParsedAsts.Find(fun pa -> pa.EANI.Name = "Fpl.Commons").ReferencingAsts
        Assert.AreEqual(["Test"; "Fpl.SetTheory"], actual)
