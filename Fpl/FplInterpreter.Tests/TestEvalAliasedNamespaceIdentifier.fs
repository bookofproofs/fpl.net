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
    [<TestMethod>]
    member this.TestFileNamePattern(aliasOrStar: string, pascelCaseId: string, expected: string) =
        let eval =
            if aliasOrStar = "" then
                { StartPos = Position("", 1, 1, 1)
                  EndPos = Position("", 1, 1, 1)
                  AliasOrStar = None
                  PascalCaseIdList = [ pascelCaseId ] }
            else
                { StartPos = Position("", 1, 1, 1)
                  EndPos = Position("", 1, 1, 1)
                  AliasOrStar = Some aliasOrStar
                  PascalCaseIdList = [ pascelCaseId ] }

        Assert.AreEqual(expected, eval.FileNamePattern)

    [<DataRow("*", "Test1", "Test1")>]
    [<DataRow("*", "Test1.Test2", "Test1.Test2")>]
    [<DataRow("T1", "Test1", "T1")>]
    [<DataRow("T2", "Test1.Test2", "T2")>]
    [<DataRow("", "Test1", "Test1")>]
    [<DataRow("", "Test1.Test2", "Test1.Test2")>]
    [<TestMethod>]
    member this.TestName(aliasOrStar: string, pascelCaseId: string, expected: string) =
        let eval =
            if aliasOrStar = "" then
                { StartPos = Position("", 1, 1, 1)
                  EndPos = Position("", 1, 1, 1)
                  AliasOrStar = None
                  PascalCaseIdList = [ pascelCaseId ] }
            else
                { StartPos = Position("", 1, 1, 1)
                  EndPos = Position("", 1, 1, 1)
                  AliasOrStar = Some aliasOrStar
                  PascalCaseIdList = [ pascelCaseId ] }

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
        let libMap = downloadLibMap url ad pos
        Assert.IsTrue(libMap.Length > 0)

    [<TestMethod>]
    member this.TestDownloadLibMap02() =
        let url = "https://github.com/bookofproofs/fpl.net/blob/main/theories/lib"
        ad.Clear()
        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let libMap = downloadLibMap url ad pos
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
        let eval =
            if aliasOrStar = "" then
                { StartPos = Position("", 1, 1, 1)
                  EndPos = Position("", 1, 1, 1)
                  AliasOrStar = None
                  PascalCaseIdList = [ pascelCaseId ] }
            else
                { StartPos = Position("", 1, 1, 1)
                  EndPos = Position("", 1, 1, 1)
                  AliasOrStar = Some aliasOrStar
                  PascalCaseIdList = [ pascelCaseId ] }

        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)

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

        let eval =
            if aliasOrStar = "" then
                { StartPos = Position("", 1, 1, 1)
                  EndPos = Position("", 1, 1, 1)
                  AliasOrStar = None
                  PascalCaseIdList = [ pascelCaseId ] }
            else
                { StartPos = Position("", 1, 1, 1)
                  EndPos = Position("", 1, 1, 1)
                  AliasOrStar = Some aliasOrStar
                  PascalCaseIdList = [ pascelCaseId ] }

        let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
        let uri = System.Uri(pathToFile)

        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

        // perform test
        let sources = acquireSources eval uri fplLibUrl ad
        Assert.AreEqual(sources.Length, 1)

        // clean up test
        File.Delete(pathToFile)
