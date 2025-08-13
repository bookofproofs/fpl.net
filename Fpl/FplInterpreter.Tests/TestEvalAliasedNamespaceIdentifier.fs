namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterUsesClause
open CommonTestHelpers
open TestSharedConfig

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
                EvalAliasedNamespaceIdentifier.DebugMode = false
            }

        Assert.AreEqual<string>(expected, eval.FileNamePattern)

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
                EvalAliasedNamespaceIdentifier.DebugMode = false
            }

        Assert.AreEqual<string>(expected, eval.Name)

    [<TestMethod>]
    member this.TestCreateLibSubfolder01() =
        let uri = PathEquivalentUri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let expected = Directory.GetCurrentDirectory()
        let (directoryPath, libDirectoryPath) = createSubfolder uri "lib"
        Assert.AreEqual<string>(expected, directoryPath)

        if Directory.Exists(libDirectoryPath) then
            Directory.Delete(libDirectoryPath, true)

    [<TestMethod>]
    member this.TestCreateLibSubfolder02() =
        let uri = PathEquivalentUri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let expected = Path.Combine(Directory.GetCurrentDirectory(), "lib")
        let (directoryPath, libDirectoryPath) = createSubfolder uri "lib"
        Assert.AreEqual<string>(expected, libDirectoryPath)

        if Directory.Exists(libDirectoryPath) then
            Directory.Delete(libDirectoryPath, true)

    [<TestMethod>]
    member this.TestCreateLibSubfolder03() =
        let uri = PathEquivalentUri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
        let expected = Path.Combine(Directory.GetCurrentDirectory(), "lib")
        let (directoryPath, libDirectoryPath) = createSubfolder uri "lib"
        Assert.IsTrue(Directory.Exists(libDirectoryPath))

        if Directory.Exists(libDirectoryPath) then
            Directory.Delete(libDirectoryPath, true)

    [<TestMethod>]
    member this.TestDownloadLibMap01() =
        if not TestConfig.OfflineMode then 
            let url = "https://github.com/bookofproofs/fpl.net/blob/main/theories/lib"
            ad.Clear()
            let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
            let evalAlias = {
                    EvalAlias.StartPos = pos
                    EvalAlias.EndPos = pos
                    EvalAlias.AliasOrStar = ""
                }
            let e = EvalAliasedNamespaceIdentifier.CreateEani([],evalAlias,pos,pos, false)
            let libMap = downloadLibMap (PathEquivalentUri(url)) url e.DebugMode
            Assert.IsTrue(libMap.Length > 0)

    [<TestMethod>]
    member this.TestDownloadLibMap02() =
        if not TestConfig.OfflineMode then 
            let url = "https://github.com/bookofproofs/fpl.net/blob/main/theories/lib"
            ad.Clear()
            let pos = Position("", (int64) 0, (int64) 1, (int64) 1)
            let evalAlias = {
                    EvalAlias.StartPos = pos
                    EvalAlias.EndPos = pos
                    EvalAlias.AliasOrStar = ""
                }
            let e = EvalAliasedNamespaceIdentifier.CreateEani([],evalAlias,pos,pos, false)
            let libMap = downloadLibMap (PathEquivalentUri(url)) url e.DebugMode
            Assert.AreEqual<int>(0, ad.CountDiagnostics)

    [<DataRow("Fpl *", 4)>]
    [<DataRow("Fpl.Test *", 0)>]
    [<DataRow("Fpl.Commons *", 2)>]
    [<DataRow("Fpl.Commons", 1)>]
    [<DataRow("Fpl.SetTheory", 1)>]
    [<DataRow("FpX *", 0)>]
    [<TestMethod>]
    member this.TestFindFilesInLibMapWithWildcard(usesClause: string, expected:int) =
        if not TestConfig.OfflineMode then 
            let filename = "TestFindFilesInLibMapWithWildcard.fpl"
            let stOpt = prepareFplCode (filename, sprintf "uses %s;" usesClause, false) 
            let fplLibUrl =
                "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        
            let uri =
                PathEquivalentUri(Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl"))
            let sources = acquireSources uri fplLibUrl stOpt.Value.OfflineMode
            let testAst = stOpt.Value.ParsedAsts.TryFindAstById("TestFindFilesInLibMapWithWildcard").Value
            let eaniList = eval_uses_clause stOpt.Value.OfflineMode testAst.Parsing.Ast 
            if eaniList.IsEmpty then 
                Assert.AreEqual<int>(expected, 0)
            else
                let filtered = sources.FindWithPattern eaniList.Head.FileNamePattern
                Assert.AreEqual<int>(expected, filtered.Length)
            prepareFplCode (filename, "", true) |> ignore


    [<DataRow("Fpl *", 5)>]
    [<DataRow("Fpl.Commons *", 5)>]
    [<DataRow("Fpl alias T1", 1)>]
    [<DataRow("Fpl.Commons alias T2", 2)>]
    [<DataRow("Fpl", 1)>]
    [<DataRow("Fpl.Commons", 2)>]
    [<TestMethod>]
    member this.TestParsedAstsCount(usesClause: string, expected: int) =
        if not TestConfig.OfflineMode then 
            let filename = "TestParsedAstsCount.fpl"
            prepareFplCode (filename, "", true) |> ignore
            let st = prepareFplCode (filename, sprintf "uses %s;" usesClause, false) 
            let result = st.Value.ParsedAsts
            Assert.AreEqual<int>(expected, result.Count)

    [<DataRow("Test1")>]
    [<DataRow("Test1.Test2")>]
    [<DataRow("Test1")>]
    [<DataRow("Test1.Test2")>]
    [<DataRow("Test1")>]
    [<DataRow("Test1.Test2")>]
    [<TestMethod>]
    member this.TestAcquireSourcesWebAndCurrDir(pascelCaseId: string) =
        if not TestConfig.OfflineMode then 
        
            // prepare test
            let pathToFile =
                Path.Combine(Directory.GetCurrentDirectory(), pascelCaseId + ".fpl")

            deleteFilesWithExtension (Directory.GetCurrentDirectory()) "fpl"
            deleteFilesWithExtension (Path.Combine(Directory.GetCurrentDirectory(), "lib")) "fpl"

            File.WriteAllText(pathToFile, ";")

            let uri = PathEquivalentUri(pathToFile)

            let fplLibUrl =
                "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"

            // perform test
            let sources = acquireSources uri fplLibUrl false
            Assert.AreEqual<int>(5, sources.Length)

            // clean up test
            File.Delete(pathToFile)

    [<TestMethod>]
    member this.TestComputeMD5Checksum() =
        let input = "Hello world"
        let actual = computeMD5Checksum input
        let expected = "3e25960a79dbc69b674cd4ec67a72c62"
        Assert.AreEqual<string>(expected, actual)

    [<TestMethod>]
    member this.TestFplSourcesUrls() =
        let fplSources = FplSources([PathEquivalentUri.EscapedUri(@"c:\temp\Test1.fpl"); PathEquivalentUri.EscapedUri(@"c:\temp\Test2.fpl"); PathEquivalentUri.EscapedUri("https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib/Test3.fpl")],"c:\temp\repo")
        Assert.AreEqual<int>(1, fplSources.Urls.Length)

    [<TestMethod>]
    member this.TestFplSourcesFiles() =
        let fplSources = FplSources([PathEquivalentUri.EscapedUri(@"c:\temp\Test1.fpl"); PathEquivalentUri.EscapedUri(@"c:\temp\Test2.fpl"); PathEquivalentUri.EscapedUri("https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib/Test3.fpl")], "c:\temp\repo")
        Assert.AreEqual<int>(2, fplSources.FilePaths.Length)

    member this.PrepareTestLoadAllUsesClauses01() =
        let input = """
            uses Fpl.Commons
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl")
        let uri = PathEquivalentUri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let parsedAsts = ParsedAstList()
        let st = SymbolTable(parsedAsts, true, TestConfig.OfflineMode)
        loadAllUsesClauses st input uri fplLibUrl 
        parsedAsts


    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Number() =
        let result = this.PrepareTestLoadAllUsesClauses01()
        Assert.AreEqual<int>(2, result.Count)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1() =
        let result = this.PrepareTestLoadAllUsesClauses01().TryFindAstById("Test")
        Assert.AreEqual<string>("Test", result.Value.Id)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses01().TryFindAstById("Fpl.Commons")
            Assert.AreEqual<string>("Fpl.Commons", result.Value.Id)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1ReferencedAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses01().TryFindAstById("Test")
            // "Test" knows that it references to "Fpl.Commons"
            Assert.AreEqual<string list>(["Fpl.Commons"], result.Value.Sorting.ReferencedAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id1ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01().TryFindAstById("Test")
        // "Test" knows that nothing is referencing to it
        Assert.AreEqual<string list>([], result.Value.Sorting.ReferencingAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2ReferencedAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses01().TryFindAstById("Fpl.Commons")
            // "Fpl.Commons" knows that it doesn't reference to anything
            Assert.AreEqual<string list>([], result.Value.Sorting.ReferencedAsts)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses01Id2ReferencingAsts() =
        let result = this.PrepareTestLoadAllUsesClauses01().TryFindAstById("Fpl.Commons")
        // "Fpl.Commons" knows that "Test" is referencing to it
        Assert.AreEqual<string list>(["Test"], result.Value.Sorting.ReferencingAsts)

    member this.PrepareTestLoadAllUsesClauses02() =
        let input = """
            uses Fpl.Commons
            uses Fpl.SetTheory
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl")
        let uri = PathEquivalentUri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let parsedAsts = ParsedAstList()
        let st = SymbolTable(parsedAsts, false, TestConfig.OfflineMode)
        loadAllUsesClauses st input uri fplLibUrl 
        parsedAsts

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Number() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02()
            Assert.AreEqual<int>(3, result.Count)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Test")
            let actual = result.Value.Id
            Assert.AreEqual<string>("Test", actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1ReferencedAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Test")
            // "Test" knows that it references to "Fpl.Commons"
            let actual = result.Value.Sorting.ReferencedAsts
            Assert.AreEqual<string list>(["Fpl.Commons"; "Fpl.SetTheory"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id1ReferencingAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Test")
            // "Test" knows that nothing is referencing to it
            let actual = result.Value.Sorting.ReferencingAsts
            Assert.AreEqual<string list>([], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Fpl.SetTheory")
            let actual = result.Value.Id
            Assert.AreEqual<string>("Fpl.SetTheory", actual)


    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2ReferencedAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Fpl.SetTheory")
            // "Fpl.SetTheory" references to FplCommons
            let actual = result.Value.Sorting.ReferencedAsts
            Assert.AreEqual<string list>(["Fpl.Commons"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id2ReferencingAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Fpl.SetTheory")
            // "Fpl.Commons" knows that "Test" is referencing to it
            let actual = result.Value.Sorting.ReferencingAsts
            Assert.AreEqual<string list>(["Test"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3() =
        let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Fpl.Commons")
        let actual = result.Value.Id
        Assert.AreEqual<string>("Fpl.Commons", actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3ReferencedAsts() =
        let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Fpl.Commons")
        // "Fpl.Commons" knows that it doesn't reference to anything
        let actual = result.Value.Sorting.ReferencedAsts
        Assert.AreEqual<string list>([], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses02Id3ReferencingAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02().TryFindAstById("Fpl.Commons")
            // "Fpl.Commons" knows that "Test" is referencing to it
            let actual = result.Value.Sorting.ReferencingAsts
            Assert.AreEqual<string list>(["Test"; "Fpl.SetTheory"], actual)

    member this.PrepareTestLoadAllUsesClauses03() =
        let input = """
            uses Fpl * 
            ;"""
        let pathToFile =
            Path.Combine(Directory.GetCurrentDirectory(), "Test.fpl")
        let uri = PathEquivalentUri(pathToFile)
        let fplLibUrl =
            "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let parsedAsts = ParsedAstList()
        let st = SymbolTable(parsedAsts, false, TestConfig.OfflineMode)
        loadAllUsesClauses st input uri fplLibUrl 
        parsedAsts

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Number() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03()
            Assert.AreEqual<int>(5, result.Count)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id1() =
        let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Test")
        let actual = result.Value.Id
        Assert.AreEqual<string>("Test", actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id1ReferencedAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Test")
            // "Test" knows that it references to "Fpl.Commons"
            let actual = result.Value.Sorting.ReferencedAsts
            Assert.AreEqual<string list>(["Fpl.Commons"; "Fpl.Commons.Structures"; "Fpl.SetTheory"; "Fpl.PeanoArithmetics"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id1ReferencingAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Test")
            // "Test" knows that nothing is referencing to it
            let actual = result.Value.Sorting.ReferencingAsts
            Assert.AreEqual<string list>([], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id2() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Fpl.SetTheory")
            let actual = result.Value.Id
            Assert.AreEqual<string>("Fpl.SetTheory", actual)


    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id2ReferencedAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Fpl.SetTheory")
            // "Fpl.SetTheory" references to FplCommons
            let actual = result.Value.Sorting.ReferencedAsts
            Assert.AreEqual<string list>(["Fpl.Commons"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id2ReferencingAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Fpl.SetTheory")
            // "Fpl.Commons" knows that "Test" is referencing to it
            let actual = result.Value.Sorting.ReferencingAsts
            Assert.AreEqual<string list>(["Test"; "Fpl.Commons.Structures"; "Fpl.PeanoArithmetics"], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id3() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Fpl.Commons")
            let actual = result.Value.Id
            Assert.AreEqual<string>("Fpl.Commons", actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id3ReferencedAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Fpl.Commons")
            // "Fpl.Commons" knows that it doesn't reference to anything
            let actual = result.Value.Sorting.ReferencedAsts
            Assert.AreEqual<string list>([], actual)

    [<TestMethod>]
    member this.TestLoadAllUsesClauses03Id3ReferencingAsts() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03().TryFindAstById("Fpl.Commons")
            // "Fpl.Commons" knows that "Test" is referencing to it
            let actual = result.Value.Sorting.ReferencingAsts
            Assert.AreEqual<string list>(["Test"; "Fpl.Commons.Structures"; "Fpl.SetTheory"; "Fpl.PeanoArithmetics"], actual)


    [<TestMethod>]
    member this.TestLoadAllUsesClausesTopologicalSorting01() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses01()
            let ra = result.TryFindAstById("Fpl.Commons")
            let rc = result.TryFindAstById("Test")
            let a = ra.Value.Sorting.TopologicalSorting
            let c = rc.Value.Sorting.TopologicalSorting
            Assert.AreEqual<int>(1, a)
            Assert.AreEqual<int>(0, c)

    [<TestMethod>]
    member this.TestLoadAllUsesClausesTopologicalSorting02() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses02()
            let ra = result.TryFindAstById("Fpl.Commons")
            let rb = result.TryFindAstById("Fpl.SetTheory")
            let rc = result.TryFindAstById("Test")
            let a = ra.Value.Sorting.TopologicalSorting
            let b = rb.Value.Sorting.TopologicalSorting
            let c = rc.Value.Sorting.TopologicalSorting
            Assert.AreEqual<int>(2, a)
            Assert.AreEqual<int>(1, b)
            Assert.AreEqual<int>(0, c)

    [<TestMethod>]
    member this.TestLoadAllUsesClausesTopologicalSorting03() =
        if not TestConfig.OfflineMode then 
            let result = this.PrepareTestLoadAllUsesClauses03()
            let ra = result.TryFindAstById("Fpl.Commons")
            let rb = result.TryFindAstById("Fpl.SetTheory")
            let rc = result.TryFindAstById("Fpl.PeanoArithmetics")
            let rd = result.TryFindAstById("Fpl.Commons.Structures")
            let re = result.TryFindAstById("Test")
            let a = ra.Value.Sorting.TopologicalSorting
            let b = rb.Value.Sorting.TopologicalSorting
            let c = rc.Value.Sorting.TopologicalSorting
            let d = rd.Value.Sorting.TopologicalSorting
            let e = re.Value.Sorting.TopologicalSorting
            Assert.AreEqual<int>(4, a)
            Assert.AreEqual<int>(3, b)
            Assert.AreEqual<int>(2, c)
            Assert.AreEqual<int>(1, d)
            Assert.AreEqual<int>(0, e)
    
    [<TestMethod>]
    member this.TestGarbageCollector() =
        if not TestConfig.OfflineMode then 
            let filename = "TestGarbageCollector.fpl"
            match prepareFplCode(filename, "uses Fpl.SetTheory;", false) with
            | Some (st:SymbolTable) -> 
                // initial counts of parsed ast and theories in root
                Assert.AreEqual<int>(3, st.ParsedAsts.Count)
                Assert.AreEqual<int>(3, st.Root.Scope.Count)
                let currDir = Directory.GetCurrentDirectory()
                let uri = PathEquivalentUri(Path.Combine(currDir,filename))
                let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
                // reparse the Test.fpl after removing the uses clause
                FplInterpreter.fplInterpreter st ";" uri fplLibUrl
                Assert.AreEqual<int>(1, st.ParsedAsts.Count)
                Assert.AreEqual<int>(1, st.Root.Scope.Count)
                prepareFplCode(filename, "", true) |> ignore

            | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestGarbageCollector01() =
        if not TestConfig.OfflineMode then 
            let filename = "TestGarbageCollector01.fpl"
            prepareFplCode(filename, "", true) |> ignore
            match prepareFplCode(filename, "uses Fpl.SetTheory;", false) with
            | Some (st:SymbolTable) -> 
                // initial counts of parsed ast and theories in root
                Assert.AreEqual<int>(3, st.ParsedAsts.Count)
                Assert.AreEqual<int>(3, st.Root.Scope.Count)
                let currDir = Directory.GetCurrentDirectory()
                let uri = PathEquivalentUri(Path.Combine(currDir, filename))
                let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
                // reparse the file after replacing the uses clause with uses Fpl.Commons
                FplInterpreter.fplInterpreter st "uses Fpl.Commons ;" uri fplLibUrl
                Assert.AreEqual<int>(2, st.ParsedAsts.Count)
                Assert.AreEqual<int>(2, st.Root.Scope.Count)
            | None -> Assert.IsTrue(false)
        
    [<TestMethod>]
    member this.TestGarbageCollector02() =
        if not TestConfig.OfflineMode then 
            let filename = "TestGarbageCollector02.fpl"
            prepareFplCode(filename, "", true) |> ignore
            match prepareFplCode(filename, "uses Fpl.SetTheory;", false) with
            | Some (st:SymbolTable) -> 
                // initial counts of parsed ast and theories in root
                Assert.AreEqual<int>(3, st.ParsedAsts.Count)
                Assert.AreEqual<int>(3, st.Root.Scope.Count)
                let currDir = Directory.GetCurrentDirectory()
                let uri = PathEquivalentUri(Path.Combine(currDir, filename))
                let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
                // reparse the Test.fpl after removing the uses clause
                FplInterpreter.fplInterpreter st "uses BlaTypo ;" uri fplLibUrl
                Assert.AreEqual<int>(1, st.ParsedAsts.Count)
                Assert.AreEqual<int>(1, st.Root.Scope.Count)
            | None -> Assert.IsTrue(false)


    [<TestMethod>]
    member this.TestGarbageCollector03() =
        if not TestConfig.OfflineMode then 
            let filename = "TestGarbageCollector03.fpl"
            let fplCode = "uses Fpl.SetTheory;"
            prepareFplCode(filename, "", true) |> ignore
            match prepareFplCode(filename, fplCode, false) with
            | Some (st:SymbolTable) -> 
                // initial counts of parsed ast and theories in root
                let parsedAstsFirstTime = st.ParsedAsts.Count
                let scopeCountFirstTime = st.Root.Scope.Count
                let errorCountfirstTime = ad.CountDiagnostics

                let currDir = Directory.GetCurrentDirectory()
                let uri = PathEquivalentUri(Path.Combine(currDir, filename))
                let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
                // reparse the Test.fpl after slightly modifying the uses clause
                FplInterpreter.fplInterpreter st (fplCode + " ") uri fplLibUrl
                Assert.AreEqual<int>(parsedAstsFirstTime, st.ParsedAsts.Count)
                Assert.AreEqual<int>(scopeCountFirstTime, st.Root.Scope.Count)
                Assert.AreEqual<int>(errorCountfirstTime, ad.CountDiagnostics)

            | None -> Assert.IsTrue(false)

    [<TestMethod>]
    member this.TestGarbageCollector04() =
        if not TestConfig.OfflineMode then 
            let filename = "TestGarbageCollector04.fpl"
            let fplCode = "uses Fpl.SetTheory;"
            prepareFplCode(filename, "", true) |> ignore
            match prepareFplCode(filename, fplCode, false) with
            | Some (st:SymbolTable) -> 
                // initial counts of parsed ast and theories in root
                let parsedAstsFirstTime = st.ParsedAsts.Count
                let scopeCountFirstTime = st.Root.Scope.Count
                let errorCountfirstTime = ad.CountDiagnostics

                let currDir = Directory.GetCurrentDirectory()
                // now, we change the uri and the source code to some referenced FPL theory
                let uri = PathEquivalentUri(Path.Combine(currDir,"repo", "Fpl.Commons.fpl"))
                let fplCode = File.ReadAllText(uri.AbsolutePath)
                let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
                // reparse the Test.fpl after slightly modifying the uses clause
                FplInterpreter.fplInterpreter st fplCode uri fplLibUrl
                Assert.AreEqual<int>(parsedAstsFirstTime, st.ParsedAsts.Count)
                Assert.AreEqual<int>(scopeCountFirstTime, st.Root.Scope.Count)
                Assert.AreEqual<int>(errorCountfirstTime, ad.CountDiagnostics)

            | None -> Assert.IsTrue(false)

module TestModule2 =
    let main () = printfn "This is a Testmodule"
    main ()