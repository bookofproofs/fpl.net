namespace TestFpl3LanguageServer.ServiceRename

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open OmniSharp.Extensions.LanguageServer.Protocol.Models
open Fpl2Interpreter.SymbolTable.Storage.Heap
open Fpl3LanguageServer.ServiceRename.RenameCore
open TestFpl3LanguageServer.Helpers.SampleFpl

[<TestClass>]
type TestRename() =

    /// <summary>
    /// Converts a 0-based LSP position into an offset inside the text.
    /// </summary>
    let offsetOf (text: string) (position: Position) : int =
        let mutable offset = 0
        for _ in 1 .. position.Line do
            offset <- text.IndexOf('\n', offset) + 1
        offset + position.Character

    /// <summary>
    /// Returns the text covered by an LSP range.
    /// </summary>
    let rangeText (text: string) (range: Range) : string =
        let start = offsetOf text range.Start
        let finish = offsetOf text range.End
        text.Substring(start, finish - start)

    // group 1 and 2 (variables and axioms): the first version of the rename service
    [<DataRow("01", "axiom A1", 0, "A1", 1)>]
    [<DataRow("01a", "byax A1", 5, "A1", 1)>]
    [<DataRow("V01", "x, y:", 0, "x", 2)>]
    [<DataRow("V01a", "(x ⇒ (y ⇒ x))", 1, "x", 2)>]
    [<DataRow("V01b", "y ⇒ x", 4, "x", 2)>]
    [<DataRow("V01c", "x ⇒ x", 0, "x", 2)>]
    [<DataRow("V01d", "x ⇒ x", 4, "x", 2)>]
    [<TestMethod>]
    member _.TestPrepareRename(no: string, cursorAnchor: string, cursorOffset: int, expectedName: string, where: int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")

        match prepareRenameAt File.ReadAllText cursorNode.Value with
        | Result.Error message ->
            Assert.Fail($"prepareRename rejected '{cursorAnchor}' (case {no}): {message}")
        | Result.Ok(path, range) ->
            let text = File.ReadAllText path
            Assert.AreEqual<string>(expectedName, rangeText text range, $"Identifier range differs for case {no}.")

    // rows mirror TestFindReferences.fs and TestGoToDefinition.fs
    // columns: no, cursorAnchor, cursorOffset, newName, expectedEditCount, where
    // expectedEditCount is the number of nodes that find-references returns for the same cursor
    [<DataRow("01", "axiom A1", 0, "Ax1", 3, 1)>]
    [<DataRow("01a", "byax A1", 5, "Ax1", 3, 1)>]
    [<DataRow("02", "theorem T1", 0, "Renamed", 3, 1)>]
    [<DataRow("02a", "cor T1$2", 0, "Renamed", 2, 1)>]
    [<DataRow("03", "inf ModusPonens", 0, "Renamed", 5, 1)>]
    [<DataRow("03a", "byinf ModusPonens", 0, "Renamed", 5, 1)>]
    // find references to arguments that have justifications
    [<DataRow("04", "6 |-", 0, "Renamed", 2, 1)>]
    [<DataRow("04a", "6.", 0, "Renamed", 2, 1)>]
    [<DataRow("05", "4. byax A1 |-", 0, "Renamed", 3, 1)>]
    [<DataRow("05a", "T1$1:4", 0, "Renamed", 3, 1)>]
    // find references to arguments that have no justifications
    [<DataRow("06", "1,2, byinf ModusPonens |-", 0, "Renamed", 3, 1)>]
    [<DataRow("06a", "1:", 0, "Renamed", 3, 1)>]
    // cross-file referencing
    [<DataRow("07", "lem Le", 0, "Renamed", 3, 1)>]
    [<DataRow("07a", "Le, Pr", 0, "Renamed", 2, 2)>]
    [<DataRow("08", "conj Co", 0, "Renamed", 2, 1)>]
    [<DataRow("08a", "byconj Co", 0, "Renamed", 2, 2)>]
    [<DataRow("09", "cor Le$1", 0, "Renamed", 5, 2)>]
    [<DataRow("09a", "bycor Le$1", 0, "Renamed", 3, 2)>]
    [<DataRow("10", "cor Le$1$2", 0, "Renamed", 5, 2)>]
    [<DataRow("10a", "bycor Le$1$2", 0, "Renamed", 3, 2)>]
    // group 1: variables
    [<DataRow("V01", "x, y:", 0, "z", 5, 2)>]
    [<DataRow("V01a", "(x ⇒ (y ⇒ x))", 1, "z", 5, 2)>]
    [<DataRow("V01b", "y ⇒ x", 4, "z", 5, 2)>]
    [<DataRow("V01c", "x ⇒ x", 0, "z", 5, 2)>]
    [<DataRow("V01d", "x ⇒ x", 4, "z", 5, 2)>]
    // group 2: predicate and byDef
    [<DataRow("P01", "pred Q", 5, "Renamed", 3, 2)>]
    [<DataRow("P01a", "Q(f)", 0, "Renamed", 3, 2)>]
    [<DataRow("P01b", "bydef Q", 6, "Renamed", 3, 2)>]
    // group 3: functional term
    [<DataRow("F01", "def func Id", 4, "Renamed", 2, 2)>]
    [<DataRow("F01a", "Id(true)", 1, "Renamed", 2, 2)>]
    // classes
    [<DataRow("C01", "class Cls", 0, "Renamed", 4, 2)>]
    [<DataRow("C01a", "Sub : Cls", 6, "Renamed", 4, 2)>]
    [<DataRow("C02", "cl Cl1", 0, "Renamed", 2, 2)>]
    [<DataRow("C02a", "z:Cl1", 3, "Renamed", 2, 2)>]
    // group 9: properties
    [<DataRow("Pp01", "prty pred PropP", 10, "Renamed", 2, 2)>]
    [<DataRow("Pp01a", "x.PropP()", 2, "Renamed", 2, 2)>]
    [<DataRow("Pp02", "prty func PropF", 10, "Renamed", 2, 2)>]
    [<DataRow("Pp02a", "x.PropF()", 2, "Renamed", 2, 2)>]
    // group 10: extensions
    [<DataRow("Ext01", "ext Digits", 4, "Renamed", 2, 2)>]
    [<DataRow("Ext01a", "@1}", 0, "Renamed", 2, 2)>]
    // group 11: array type occurrences
    [<DataRow("Arr01", "cl ArrCl", 3, "Renamed", 2, 2)>]
    // group 12: array type occurrences with index references
    [<DataRow("Arr02", "cl ArrIdx", 3, "Renamed", 2, 2)>]
    // group 13: array variable references
    [<DataRow("Arr03", "x:*pred[ind,ind]", 0, "z", 3, 2)>]
    // constructors
    [<DataRow("Co1", "ctor Cls()", 6, "Renamed", 3, 2)>]
    [<DataRow("Co1a", "x:=Cls()", 4, "Renamed", 3, 2)>]
    [<DataRow("Co1b", "base.Cls()", 6, "Renamed", 3, 2)>]
    // group 6: proposition
    [<DataRow("PR01", "prop Pr", 5, "Renamed", 2, 1)>]
    [<DataRow("PR01a", "Le, Pr |-", 4, "Renamed", 2, 2)>]
    // infix operation
    [<DataRow("I01", "pred Inf", 0, "Renamed", 3, 2)>]
    [<DataRow("I01a", "x => y", 2, "Renamed", 3, 2)>]
    [<DataRow("I01b", "y => z", 3, "Renamed", 3, 2)>]
    // postfix operation
    [<DataRow("Pos01", "func Post", 0, "Renamed", 2, 2)>]
    [<DataRow("Pos01a", "x'", 1, "Renamed", 2, 2)>]
    // prefix operation
    [<DataRow("Pre01", "func Pre", 0, "Renamed", 2, 2)>]
    [<DataRow("Pre01a", "-x", 0, "Renamed", 2, 2)>]
    // symbol operation
    [<DataRow("Sym01", "cl Cl2", 0, "Renamed", 3, 2)>]
    [<DataRow("Sym01a", "∅ = ∅", 0, "Renamed", 3, 2)>]
    [<DataRow("Sym02a", "∅ = ∅", 4, "Renamed", 3, 2)>]
    // group 14: is operator
    [<DataRow("IS01", "cl IsCl", 3, "Renamed", 2, 2)>]
    [<DataRow("IS01a", "is(x,IsCl)", 5, "Renamed", 2, 2)>]
    [<TestMethod>]
    member _.TestRename(no: string, cursorAnchor: string, cursorOffset: int, newName: string, expectedCount: int, where: int) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")

        // the identifier that the rename must replace, as reported by prepareRename, along with its
        // path and range so that we can identify the one edit that corresponds to the cursor itself.
        let cursorPath, cursorRange, oldName =
            match prepareRenameAt File.ReadAllText cursorNode.Value with
            | Result.Ok(path, range) -> path, range, rangeText (File.ReadAllText path) range
            | Result.Error message -> failwith $"prepareRename rejected '{cursorAnchor}' (case {no}): {message}"
        let myEdits = renameEdits File.ReadAllText cursorNode.Value heap.SymbolTable.Nodes newName
        match myEdits with
        | Result.Error message ->
            Assert.Fail($"Rename to '{newName}' rejected (case {no}): {message}")
        | Result.Ok edits ->
            Assert.AreEqual(expectedCount, edits.Length, $"Edit count differs for case {no}.")
            let cursorEdit =
                edits
                |> List.tryFind (fun (path, range, _) -> path = cursorPath && range = cursorRange)
            match cursorEdit with
            | None ->
                Assert.Fail($"No edit found matching the cursor position for case {no}.")
            | Some(path, range, newText) ->
                Assert.AreEqual<string>(newName, newText, $"New text differs for case {no}.")
                Assert.AreEqual<string>(oldName, rangeText (File.ReadAllText path) range, $"Edited range differs for case {no}.")


    // columns: no, cursorAnchor, cursorOffset, newName, where, isValid, substring of error 
    [<DataRow("01",  "axiom A1", 0, "Ren", 1, true, "")>]
    [<DataRow("01a", "axiom A1", 0, "1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("01b", "axiom A1", 0, "", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("01c", "axiom A1", 0, "x", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("01d", "axiom A1", 0, "T1", 1, false, "already exists for theorem")>]
    [<DataRow("01e", "axiom A1", 0, "Ren$1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("01f", "axiom A1", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("02",  "theorem T1", 0, "Ren", 1, true, "")>]
    [<DataRow("02a", "theorem T1", 0, "1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("02b", "theorem T1", 0, "", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("02c", "theorem T1", 0, "x", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("02d", "theorem T1", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("02e", "theorem T1", 0, "Ren$1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("02f", "theorem T1", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("03",  "cor T1$2", 0, "Ren$1", 1, true, "")>]
    [<DataRow("03a", "cor T1$2", 0, "1", 1, false, "must match pattern")>]
    [<DataRow("03b", "cor T1$2", 0, "", 1, false, "must match pattern")>]
    [<DataRow("03c", "cor T1$2", 0, "x", 1, false, "must match pattern")>]
    [<DataRow("03d", "cor T1$2", 0, "A1", 1, false, "must match pattern")>]
    [<DataRow("03e", "cor T1$2", 0, "Ren", 1, false, "must match pattern")>]
    [<DataRow("03f", "cor T1$2", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("04",  "lem Le", 0, "Ren", 1, true, "")>]
    [<DataRow("04a", "lem Le", 0, "1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("04b", "lem Le", 0, "", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("04c", "lem Le", 0, "x", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("04d", "lem Le", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("04e", "lem Le", 0, "Ren$1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("04f", "lem Le", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("05",  "prop Pr", 0, "Ren", 1, true, "")>]
    [<DataRow("05a", "prop Pr", 0, "1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("05b", "prop Pr", 0, "", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("05c", "prop Pr", 0, "x", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("05d", "prop Pr", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("05e", "prop Pr", 0, "Ren$1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("05f", "prop Pr", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("06",  "conj Co", 0, "Ren", 1, true, "")>]
    [<DataRow("06a", "conj Co", 0, "1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("06b", "conj Co", 0, "", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("06c", "conj Co", 0, "x", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("06d", "conj Co", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("06e", "conj Co", 0, "Ren$1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("06f", "conj Co", 0, "xor", 1, false, "is a keyword")>]
    [<DataRow("07",  "inf ModusPonens", 0, "Ren", 1, true, "")>]
    [<DataRow("07a", "inf ModusPonens", 0, "1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("07b", "inf ModusPonens", 0, "", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("07c", "inf ModusPonens", 0, "x", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("07d", "inf ModusPonens", 0, "A1", 1, false, "already exists for axiom")>]
    [<DataRow("07e", "inf ModusPonens", 0, "Ren$1", 1, false, "must be a pascal-case identifier")>]
    [<DataRow("07f", "inf ModusPonens", 0, "xor", 1, false, "is a keyword")>]
    
    [<TestMethod>]
    member _.TestValidateNewName(no: string, cursorAnchor: string, cursorOffset: int, newName: string, where: int, isValid:bool, sError:string) =

        let (cursorLine, cursorColumn, cursorNode) = assertPrepareWorked cursorAnchor cursorOffset where

        Assert.IsTrue(cursorNode.IsSome, $"No node found at {cursorLine}:{cursorColumn} at code #{where}.")
        let validated = validateNewName cursorNode.Value newName heap.SymbolTable.Nodes
   
        match isValid, validated with 
        | true, Result.Error error ->
            Assert.Fail($"Failed - produced error for a valid rename to '{newName}' at '{cursorAnchor}'{Environment.NewLine}{error}")
        | true, Result.Ok _ ->
            printfn "%s" $"Succeeded validation to '{newName}' at '{cursorAnchor}'"
        | false, Result.Error error when error.Contains(sError) ->
            printfn "%s" $"Succeeded check for invalid rename to '{newName}' at '{cursorAnchor}' with error {Environment.NewLine}{error}"
        | false, Result.Error error when not (error.Contains(sError)) ->
            Assert.Fail($"Failed - produced wrong validation error for an invalid rename to '{newName}' at '{cursorAnchor}'{Environment.NewLine}{error}{Environment.NewLine}does not contain '{sError}'")
        | _, _ ->
            Assert.Fail($"Failed - produced no error for an invalid rename to '{newName}' at '{cursorAnchor}'")
