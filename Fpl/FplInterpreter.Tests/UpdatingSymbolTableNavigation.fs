namespace FplInterpreter.Tests

open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open FParsec
open ErrDiagnostics
open FplInterpreterTypes
open FplInterpreterUsesClause
open CommonTestHelpers
open System

[<TestClass>]
type UpdatingSymbolTableNavigation() =

    [<TestMethod>]
    member this.UsesClauseCausesDownloads() =
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "UsesClauseCausesDownloads"  
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        match stOption with
        | Some st -> 
            // initial counts of parsed ast and theories in root
            Assert.AreEqual<int>(3, st.ParsedAsts.Count)
        | None -> Assert.IsTrue(false)

        // remove the test file
        prepareFplCode(filename, "", true) |> ignore

    [<TestMethod>]
    member this.UsesClauseCreatesSubdirectoriesLibAndRepo() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "UsesClauseCreatesSubdirectoriesLibAndRepo"  
        // file processing creates the subdirectories
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 

        // do the test - check if the subdirectories exist and contain the files 
        Assert.IsTrue(Directory.Exists(currentPathLib))
        Assert.IsTrue(Directory.Exists(currentPathRepo))
        Assert.IsTrue(isDirectoryEmpty currentPathLib)
        Assert.IsTrue(File.Exists(Path.Combine(currentPathRepo,"Fpl.SetTheory.fpl")))
        Assert.IsTrue(File.Exists(Path.Combine(currentPathRepo,"Fpl.Commons.fpl")))

        // remove the test file
        prepareFplCode(filename + ".fpl", fplCode, true) |> ignore

    [<TestMethod>]
    member this.OpeningFileInRepoDoesNotCreateYetOtherSubdirsLibAndRepo() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "OpeningFileInRepoDoesNotCreateYetOtherSubdirsLibAndRepo"  
        // file processing creates the subdirectories
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore

        // do the test - now, open a file in the repo subdirectory
        loadFplFile (Path.Combine(currentPathRepo,"Fpl.Commons.fpl")) |> ignore
        let currentPathRepoLib = Path.Combine(currentPathRepo,"lib")
        let currentPathRepoRepo = Path.Combine(currentPathRepo,"repo")
        Assert.IsFalse(Directory.Exists(currentPathRepoLib))
        Assert.IsFalse(Directory.Exists(currentPathRepoRepo))

        // remove the test file
        prepareFplCode(filename, "", true) |> ignore

    [<TestMethod>]
    member this.OpeningFileInRepoDoesNotRaiseRuntimeErrors() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "OpeningFileInRepoDoesNotRaiseRuntimeErrors"  
        // file processing creates the subdirectories
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore

        // do the test - now, open a file in the repo subdirectory
        loadFplFile (Path.Combine(currentPathRepo,"Fpl.Commons.fpl")) |> ignore
        // and test if there was a runtime error (e.g. subpath not found due to messed-up folder logic)
        let result = filterByErrorCode ad (GEN00 "").Code
        Assert.AreEqual<int>(0, result.Length)

        // remove the test file
        prepareFplCode(filename, "", true) |> ignore

    [<TestMethod>]
    member this.OpeningParentFileTheoryEnhancesSymbolTableCorrectly() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "OpeningParentFileTheoryEnhancesSymbolTableCorrectly"  
        // file processing creates the subdirectories
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore

        // do the test - now, open a specific file in the repo subdirectory
        let stOption = loadFplFile (Path.Combine(currentPathRepo,"Fpl.Commons.fpl")) 
        // and test if the corrent number of asts in symbol table
        match stOption with
        | Some st -> 
            // initial counts of parsed ast and theories in root
            Assert.AreEqual<int>(1, st.ParsedAsts.Count)
        | None -> Assert.IsTrue(false)
        
        // now, conserve the symbol table for the test's next step and open the parent file
        let st = SymbolTable(stOption.Value.ParsedAsts, false)
        let uri = PathEquivalentUri(Path.Combine(currentPathRepo,"Fpl.SetTheory.fpl"))
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let fplCode = File.ReadAllText(uri.AbsolutePath)
        FplInterpreter.fplInterpreter st fplCode uri fplLibUrl

        // and test if the corrent number of asts in symbol table
        Assert.AreEqual<int>(2, st.ParsedAsts.Count)

        // now, open the grand parent file
        let st = SymbolTable(stOption.Value.ParsedAsts, false)
        let uri = PathEquivalentUri(Path.Combine(currentPath,filename + ".fpl"))
        let fplCode = File.ReadAllText(uri.AbsolutePath)
        FplInterpreter.fplInterpreter st fplCode uri fplLibUrl

        // and test if the corrent number of asts in symbol table
        Assert.AreEqual<int>(3, st.ParsedAsts.Count)
        
        // remove the test file
        prepareFplCode(filename, "", true) |> ignore

    [<TestMethod>]
    member this.OpeningGrandParentFileTheoryEnhancesSymbolTableCorrectly() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "OpeningGrandParentFileTheoryEnhancesSymbolTableCorrectly"  
        // file processing creates the subdirectories
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore

        // do the test - now, open a specific file in the repo subdirectory
        let stOption = loadFplFile (Path.Combine(currentPathRepo,"Fpl.Commons.fpl")) 
        // and test if the corrent number of asts in symbol table
        match stOption with
        | Some st -> 
            // initial counts of parsed ast and theories in root
            Assert.AreEqual<int>(1, st.ParsedAsts.Count)
        | None -> Assert.IsTrue(false)
        
        // now, conserve the symbol table for the test's next step and open the grand parent file
        let st = SymbolTable(stOption.Value.ParsedAsts, false)
        let uri = PathEquivalentUri(Path.Combine(currentPath,filename + ".fpl"))
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let fplCode = File.ReadAllText(uri.AbsolutePath)
        FplInterpreter.fplInterpreter st fplCode uri fplLibUrl

        // and test if the corrent number of asts in symbol table
        Assert.AreEqual<int>(3, st.ParsedAsts.Count)
        
        // remove the test file
        prepareFplCode(filename, "", true) |> ignore

    [<TestMethod>]
    member this.OpeningFileInLibAsCopyOfFileInRepoDoesRaiseNSP05Error() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo
        deleteFiles currentPath "*.fpl"

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "OpeningFileInLibAsCopyOfFileInRepoDoesRaiseNSP05Error"  
        // file processing creates the subdirectories lib and repo
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore
        // test if the test's preparation didn't mess up. There should be no NSP05 error at all
        let result = filterByErrorCode ad (NSP05 ([],"","")).Code
        Assert.AreEqual<int>(0, result.Length)

        // the repo files are supposed to be in the repository (https source)
        // we now copy the repo file to the lib file and pretend to have the same file 
        // in our lib subfolder
        File.Copy(Path.Combine(currentPathRepo,"Fpl.Commons.fpl"),Path.Combine(currentPathLib,"Fpl.Commons.fpl"))

        // do the test - now, open a file in the lib subdirectory
        loadFplFile (Path.Combine(currentPathLib,"Fpl.Commons.fpl")) |> ignore
        // now, we should have an NSP05 error
        let result = filterByErrorCode ad (NSP05 ([],"","")).Code
        Assert.AreEqual<int>(1, result.Length)

        // remove the test file
        prepareFplCode(filename, "", true) |> ignore
        // remove the test file from lib
        deleteFiles currentPathLib "Fpl.Commons.fpl"

    [<TestMethod>]
    member this.OpeningFileInMainAsCopyOfFileInRepoDoesRaiseNSP05Error() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo
        deleteFiles currentPath "Fpl.Commons.fpl"
        deleteFiles currentPath "Fpl.SetTheory.fpl"

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "OpeningFileInMainAsCopyOfFileInRepoDoesRaiseNSP05Error"  
        // file processing creates the subdirectories lib and repo
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore
        // test if the test's preparation didn't mess up. There should be no NSP05 error at all
        let result = filterByErrorCode ad (NSP05 ([],"","")).Code
        Assert.AreEqual<int>(0, result.Length)

        // the repo files are supposed to be in the repository (https source)
        // we now copy the repo file to the lib subfolder
        File.Copy(Path.Combine(currentPathRepo,"Fpl.Commons.fpl"),Path.Combine(currentPath,"Fpl.Commons.fpl"))

        // do the test - now, open a file in the lib subdirectory
        loadFplFile (Path.Combine(currentPath,"Fpl.Commons.fpl")) |> ignore
        // now, we should have an NSP05 error
        let result = filterByErrorCode ad (NSP05 ([],"","")).Code
        Assert.AreEqual<int>(1, result.Length)

        // remove the test file
        prepareFplCode(filename, "", true) |> ignore

        // remove the test file from Main
        deleteFiles currentPath "Fpl.Commons.fpl"

    [<TestMethod>]
    member this.HavingRepoFileInLibDoesPreventItToBeDownloadedInRepo() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo
        deleteFiles currentPath "Fpl.Commons.fpl"
        deleteFiles currentPath "Fpl.SetTheory.fpl"

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "HavingRepoFileInLibDoesNotPreventItToBeDownloadedInRepo"  
        // file processing creates the subdirectories lib and repo
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore

        // the repo files are supposed to be in the repository (https source)
        // we now copy the repo file to the lib subfolder
        File.Copy(Path.Combine(currentPathRepo,"Fpl.Commons.fpl"),Path.Combine(currentPathLib,"Fpl.Commons.fpl"))

        // and delete it from the repo subfolder
        deleteFiles currentPathRepo "Fpl.Commons.fpl"

        // now, process the main file again. This should complement the repo folder again
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore

        // do the test - check, if the repo file was downloaded again although it exists in lib subfolder
        Assert.IsFalse(File.Exists(Path.Combine(currentPathRepo,"Fpl.Commons.fpl")))
        Assert.IsTrue(File.Exists(Path.Combine(currentPathLib,"Fpl.Commons.fpl")))

        // remove the test file
        prepareFplCode(filename, "", true) |> ignore
        // remove the test file from lib
        deleteFiles currentPathLib "Fpl.Commons.fpl"

    [<TestMethod>]
    member this.HavingRepoFileInMainDoesPreventItToBeDownloadedInRepo() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo
        deleteFiles currentPath "Fpl.Commons.fpl"
        deleteFiles currentPath "Fpl.SetTheory.fpl"

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "HavingRepoFileInMainDoesPreventItToBeDownloadedInRepo"  
        // file processing creates the subdirectories lib and repo
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore

        // the repo files are supposed to be in the repository (https source)
        // we now copy the repo file to main folder 
        File.Copy(Path.Combine(currentPathRepo,"Fpl.Commons.fpl"),Path.Combine(currentPath,"Fpl.Commons.fpl"))

        // and delete it from the repo subfolder
        deleteFiles currentPathRepo "Fpl.Commons.fpl"

        // now, process the main file again. This should complement the repo folder again
        prepareFplCode(filename + ".fpl", fplCode, false) |> ignore

        // do the test - check, if the repo file was downloaded again although it exists in lib subfolder
        Assert.IsFalse(File.Exists(Path.Combine(currentPathRepo,"Fpl.Commons.fpl")))
        Assert.IsTrue(File.Exists(Path.Combine(currentPath,"Fpl.Commons.fpl")))

        // remove the test file
        prepareFplCode(filename, "", true) |> ignore
        // remove the test file from lib
        deleteFiles currentPath "Fpl.Commons.fpl"

    [<TestMethod>]
    member this.OpeningFileInRepoAndChangingItChangesAlsoTheDiagnosticsOfThisFile() =
        // prepare test, making sure there is an empty 
        // lib subfolder and a repo subfolder containing the files Fpl.Commons.fpl and Fpl.SetTheory.fpl.
        ad.Clear()
        // first delete lib and repo subdirectories (if any)
        let currentPath = Directory.GetCurrentDirectory()
        let currentPathLib = Path.Combine(currentPath,"lib")
        let currentPathRepo = Path.Combine(currentPath,"repo")
        deleteDirectory currentPathLib
        deleteDirectory currentPathRepo
        deleteFiles currentPath "Fpl.Commons.fpl"
        deleteFiles currentPath "Fpl.SetTheory.fpl"

        let fplCode = """
            uses Fpl.SetTheory;
        """
        let filename = "OpeningFileInRepoDoesNotCreateYetOtherSubdirsLibAndRepo"  
        // file processing creates the subdirectories
        let stOption = prepareFplCode(filename + ".fpl", fplCode, false) 
        match stOption with
        | Some st -> 
            // initial counts of parsed ast and theories in root
            Assert.AreEqual<int>(3, st.ParsedAsts.Count)
        | None -> Assert.IsTrue(false)

        let pathToTestFile = Path.Combine(currentPathRepo,"Fpl.Commons.fpl")
        let diagnosticsOfFile = ad.GetStreamDiagnostics(PathEquivalentUri(pathToTestFile))
        let rememberDiagnosticsOfOriginalFile = diagnosticsOfFile.Count
        // now manipulate the file and reprocess it
        
        // now, conserve the symbol table for the test's next step and reprocess the manipulated file
        let st = SymbolTable(stOption.Value.ParsedAsts, false)
        let uri = PathEquivalentUri(pathToTestFile)
        let fplLibUrl = "https://raw.githubusercontent.com/bookofproofs/fpl.net/main/theories/lib"
        let fplCodeOriginal = File.ReadAllText(pathToTestFile)
        let fplCodeManipulated = fplCodeOriginal.Substring(0,fplCodeOriginal.Length-1) + "def pred Bla() { Bla1() };"
        FplInterpreter.fplInterpreter st fplCodeManipulated uri fplLibUrl

        // do the test - check, if the diagnostics changed
        let diagnosticsOfManipulatedFile = ad.GetStreamDiagnostics(PathEquivalentUri(pathToTestFile))
        Assert.IsTrue(diagnosticsOfManipulatedFile.Count > rememberDiagnosticsOfOriginalFile)

        // remove the test file
        prepareFplCode(filename, "", true) |> ignore

